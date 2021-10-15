open System.Collections.Generic
open System.Transactions
open Plough.ControlFlow
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine

let scopes = Dictionary()

let createTransactionScope owner =
    let scope = 
        new TransactionScope(TransactionScopeOption.Required,
                             TransactionOptions(
                                 IsolationLevel=IsolationLevel.ReadCommitted, 
                                 Timeout=TransactionManager.MaximumTimeout
                             ),
                             TransactionScopeAsyncFlowOption.Enabled)
    scopes.Add(owner, scope)
    scope

let openConnection () = 
    let conn = new Npgsql.NpgsqlConnection("Host=localhost;Username=postgres;Password=postgres;Database=lims;Pooling=true")
    do conn.Open()
    conn

// First bug - if onError compensation for FailureMessage from f is throwing exception, it get's compensated second time, hiding original problem
let inline onErrorInPlough (onError : FailureMessage -> TaskEither<'b>) (f : unit -> TaskEither<'b>) : TaskEither<'b> =
    task {
        try
            match! f () with
            | Success s -> return Ok { Data=s; Warnings=[] }
            | SuccessWithWarning (s, w) -> return Ok { Data=s; Warnings=w }
            | Failure s -> return! onError s
        with
        | exn -> return! exn |> FailureMessage.ExceptionFailure |> onError
    }

let inline onError (onError : FailureMessage -> TaskEither<'b>) (f : unit -> TaskEither<'b>) : TaskEither<'b> =
    task {
        let! result =    
            task {
                try return! f()
                with
                | exn -> return! exn |> FailureMessage.ExceptionFailure |> TaskEither.fail
            }
        
        match result with
        | Success _ | SuccessWithWarning _ -> return result
        | Failure s -> return! onError s
    }
let mutable countHit = 0

let doWithErrorHandling () : TaskEither<unit> =
    fun () -> taskEither {
        use scope = createTransactionScope "doWithErrorHandling"
        do! "error" |> Validation |> TaskEither.fail
        scope.Complete()
    }
    |> onError (fun error -> taskEither {
        countHit <- countHit + 1
        let scopesCheck = scopes
        do! task {
            let asdas = error
            use connection = openConnection ()
            ()
        }
        return! TaskEither.fail error
    })

let execute ()  =
    taskEither {
        use scope = createTransactionScope "execute"
        do! doWithErrorHandling ()
        scope.Complete()
        return 1
    }


[<EntryPoint>]
let main argv =
    try
        let result = execute() |> TaskEither.unwrap
        0 // return an integer exit code
    with
    | e ->
        let asdas = countHit
        reraise ()