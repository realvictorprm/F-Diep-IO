module Main
open Fable.Import.Browser
open Fable.Core.JsInterop
open Game
open Game.Physics
open Game.Physics.GridSystem
open System.Collections
let canvas = 
    let canvases = document.getElementsByTagName_canvas()
    [for i in 0 ..  int canvases.length - 1 do if canvases.[i].id = "game_canvas" then yield canvases.[i]] |> Seq.head
let ctx = canvas.getContext_2d()
let ($) s n = s + n.ToString()
let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

/// Set of currently pres  sed keys
let mutable keysPressed = Set.empty

let mutable mouseKeysPressed = Set.empty

let mutable mouseClickHandled = false
/// Update the keys as requested
let reset () = keysPressed <- Set.empty
let isPressed keyCode = Set.contains keyCode keysPressed

let createDefaultEntity x y =
    [ Physics.Circle(20.), { x = float x * 10. + 20.; y = float y * 10.; rad = 0.}, { vx = 100.; vy = 100.; rad = 0.}, x, y]
//       a    
let mutable tank = 
    { shape = Physics.Circle(30.) 
      pos = { x = 0.; y = 0.; rad = 0.}
      vel = { vx = 100.; vy = 100.; rad = 0.}
      uid = "maintank"
      collisionHandler = fun () -> false
      friction = DefaultFrictionValue
    }

let test = 
    { shape = Physics.Circle(20.) 
      pos = { x = 100.; y = 100.; rad = 0.}
      vel = { vx = 200.; vy = 200.; rad = 0.}
      uid = "projectile"
      collisionHandler = fun () -> true
      friction = DefaultFrictionValue
    }

let toString a = a.ToString()

let mutable grid =  
    { width = 50 
      height = 50
      horizontalTiles = 40
      verticalTiles = 22
      tiles = Map [ for x in 0 .. 40 - 1 do for y in 0 .. 22 - 1 do yield ((x, y), Generic.List())] |> Generic.Dictionary }

let addToMainGrid entity = addToGrid entity grid

do addToMainGrid test
do addToMainGrid tank
do addToMainGrid {test with pos = {x = 50.; y = 100.; rad = 0. }; vel = { vx = 100.; vy = 50.; rad = 0.} }
let mutable identifiers =  Generic.Dictionary()

let mutable mousePosOnCanvas = { x = 0.; y = 0.; rad = 0.}
let mutable frameCount = 0
let incFrames () = frameCount <- frameCount + 1 
let render () = 
    ctx.save()
    ctx.fillStyle <- !^ (rgb 0xff 0xff 0xff)
    ctx.fillRect(0., 0., ctx.canvas.width, ctx.canvas.height)
    let processKeys () =
        if identifiers.ContainsKey "maintank" then
                let tank, x, y = identifiers.["maintank"]
                let t = tank
                let tt = grid.tiles.[(x, y)]
                let factor = 15.
                let maxSpeed = 500.
                if keysPressed.Contains 32 then
                    tank.vel <-  { vx = 0.; vy = 0.; rad = 0. }
                else
                    let vel = t.vel
                    let vx, vy = 
                        (if keysPressed.Contains 68 then factor
                         elif keysPressed.Contains 65 then -factor
                         else 0.),
                        (if keysPressed.Contains 87 then -factor
                         elif keysPressed.Contains 83 then factor
                         else 0.)
                    let resvx =
                        let sum = vx + vel.vx
                        if sum > maxSpeed then maxSpeed elif sum < -maxSpeed then -maxSpeed else sum
                    let resvy =
                        let sum = vy + vel.vy 
                        if sum > maxSpeed then maxSpeed elif sum < -maxSpeed then -maxSpeed else sum
                    tank.vel <-  { vx = resvx; vy = resvy; rad = 0. }

    let drawEntity (e:Entity) =
        let shape, pos, vel = e.shape, e.pos, e.vel
        match shape with
        | Circle(r) ->
            ctx.beginPath()
            ctx.strokeStyle <- !^ (rgb 100 10 10)
            ctx.arc(pos.x, pos.y, r, 0., 2. * System.Math.PI, true)
            ctx.stroke()
            ctx.fillStyle <- !^ (rgb 0xFF 0x50 0x50)
            ctx.arc(pos.x, pos.y, r, 0., 2. * System.Math.PI, true)
            ctx.fill()
            ctx.closePath()
            // Logger.logf "pos x: %A, pos y: %A, radius %A" pos.x pos.y r |> ignore
        | _ -> ()
        ()

    let drawTank () =
        if identifiers.ContainsKey "maintank" then
            let tank, _, _ = identifiers.["maintank"]
            let pos = tank.pos
            let vectorToMouse, dist =
                let mTranslated = { mousePosOnCanvas with x = mousePosOnCanvas.x; y = mousePosOnCanvas.y }
                Physics.distanceVector pos mTranslated, Physics.distance mTranslated pos
            ctx.save()
            ctx.lineWidth <- 10.
            drawEntity tank
            ctx.restore()
            ctx.save()
            // draw our bullet gauge
            ctx.translate(pos.x, pos.y)
            ctx.rotate(vectorToMouse.rad)
            ctx.fillStyle <- !^ (rgb 0x00 0x00 0x00)
            ctx.fillRect(0., -15., 80., 30.)
            ctx.restore()

    let drawUI() =
        if identifiers.ContainsKey "maintank" then
            let tank, _, _ = identifiers.["maintank"]
            ctx.save()
            let pos, vel = tank.pos, tank.vel
            let width, height = canvas.width, canvas.height
            ctx.fillStyle <- !^ (rgb 0xAA 0x50 0x50)
            ctx.fillRect(0., 0., canvas.width, 50.)
            ctx.fillStyle <- !^ (rgb 0x00 0x50 0x50)
            ctx.font <- "20px Verdana"
            let text = 
                sprintf "Tank position, x: %.2f, y: %.2f | Velocity x: %.2f, y: %.2f | Mouse position x: %.2f, y: %.2f " 
                    pos.x pos.y vel.vx vel.vy mousePosOnCanvas.x mousePosOnCanvas.y
            ctx.fillText(text, 5., 20., 1400.)
            ctx.restore()
    
    do processKeys()
    do drawTank ()
    do grid.tiles |> Seq.iter (fun x -> x.Value |> Seq.iter drawEntity)
    do drawUI ()
    let newGrid, ids = Physics.stepForGrid grid defaultFractionNormalized 0.01
    grid <- newGrid
    identifiers <- ids
    //do tank <- step tank defaultFractionNormalized 0.01 
    ctx.restore()
    incFrames()

let rec update () =
    render ()
    window.setTimeout(update, 16.)


let keyHelperOperation pressed =  if pressed then Set.add else Set.remove

/// Triggered when key is pressed/released
let keyUpdate (e : KeyboardEvent, pressed) =
  let keyCode = int e.keyCode
  keysPressed <- (keyHelperOperation pressed) keyCode keysPressed
  null

let windowResized () =
    let width = window.outerWidth
    let height = window.outerHeight
    Logger.logf "window resized, width %A, height %A " width height |> ignore
    canvas.width <- width
    canvas.height <- height
    null

let mouseMoved (event:MouseEvent) =
    mousePosOnCanvas <- { mousePosOnCanvas with x = event.x; y = event.y}
    null

let mouseClick (event:MouseEvent) =
    if identifiers.ContainsKey "maintank" then
        let tank, _, _= identifiers.["maintank"]
        let pos = tank.pos
        let vectorToMouse, dist =
            let mTranslated = { mousePosOnCanvas with x = mousePosOnCanvas.x; y = mousePosOnCanvas.y }
            Physics.distanceVector pos mTranslated, Physics.distance mTranslated pos
        let x, y = cos vectorToMouse.rad, sin vectorToMouse.rad
        let radius = 
            let r = tank.shape |> function | Circle(r) -> r | _ -> 0.
            r * 2.5
        let bulletVector = { vy = y * 1000.; vx = x * 1000.; rad = vectorToMouse.rad }
        let bulletPos = { x = radius * x + tank.pos.x; y = radius * y + tank.pos.y; rad = 0. }
        let bullet = { pos = bulletPos; shape = Circle(15.); uid = ""; vel = bulletVector; collisionHandler = (fun () -> true); friction = 0.0001 } 
        addToMainGrid bullet
        tank.vel <- { vx = tank.vel.vx - 500. * x; vy = tank.vel.vy - 500. * y; rad = tank.vel.rad }
    mouseClickHandled <- true

let mouseUpdate (event:MouseEvent, pressed) =
    let keyCode = int event.button
    if pressed && not mouseClickHandled then
        mouseClick event
    elif not pressed then
        mouseClickHandled <- false
    mouseKeysPressed <- (keyHelperOperation pressed) keyCode mouseKeysPressed
    null

/// Register DOM event handlers
let init () =
  window.addEventListener_keydown(fun e -> keyUpdate(e, true))
  window.addEventListener_keyup(fun e -> keyUpdate(e, false))
  window.addEventListener_resize(fun e -> windowResized())
  canvas.addEventListener_mouseup(fun e -> mouseUpdate(e, false))
  canvas.addEventListener_mousedown(fun e -> mouseUpdate(e, true))
  canvas.addEventListener_mousemove(fun e -> mouseMoved e)
  windowResized ()
do update()
do init()