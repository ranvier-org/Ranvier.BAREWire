module Ranvier.BAREWire.Smoke.Fable.Main

open Fable.Core
open Ranvier.BAREWire.Smoke

[<Emit("process.exitCode = $0")>]
let private setExitCode (code: int) : unit = jsNative

let private report () =
    task {
        try
            let! results = Checks.run ()
            let mutable failures = 0

            for name, ok in results do
                if ok then
                    printfn "ok   %s" name
                else
                    failures <- failures + 1
                    printfn "FAIL %s" name

            if failures = 0 then
                printfn "all checks passed"
            else
                printfn "%d checks failed" failures
                setExitCode 1
        with error ->
            printfn "the checks raised: %s" error.Message
            setExitCode 1
    }

report () |> ignore
