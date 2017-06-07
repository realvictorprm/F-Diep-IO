namespace Game
open Fable.Import.Browser

module Logger =
    let mutable counter = 0
    let logf msg =  Printf.kprintf (fun s -> window.console.log s; s) msg

    let log s = 
        window.console.log s
        counter <- counter + 1




