namespace Game
open System.Collections
module Physics =
    type Shape =
    | Circle of float
    | Rect of float * float
      
    type Position =
        { x : float
          y : float
          rad : float}

    type Velocity = 
        { vx : float
          vy : float
          rad : float }
    
    [<ReferenceEquality>]
    type Entity = 
        { shape : Shape
          mutable pos : Position
          mutable vel : Velocity
          uid : string
          friction : float
          collisionHandler : unit -> bool
        }
        
    module GridSystem =

        type Grid =
            {   width : int
                height : int
                verticalTiles : int
                horizontalTiles : int
                tiles : Generic.Dictionary<int * int, Generic.List<Entity>>
            }
        
        let createEmptyGridFromOld (oldGrid:Grid) =
            let emptyMap = Map [ for x in 0 .. oldGrid.horizontalTiles - 1 do for y in 0 .. oldGrid.verticalTiles - 1 do yield ((x, y), Generic.List())]
            { oldGrid with tiles = emptyMap |> Generic.Dictionary }
        
        let getGridTupleForPosition (pos:Position) (grid:Grid) =
            let x, y = pos.x, pos.y
            let w, h = grid.width, grid.height
            let gx, gy = 
                (float pos.x / float w) |> int,
                (float pos.y / float h) |> int
            (gx, gy)
        
        let addToGrid (entity:Entity) (grid:Grid) =
            grid.tiles.[getGridTupleForPosition entity.pos grid].Add entity

    open GridSystem
    let distance (pos1:Position) (pos2:Position) =
        let x1, y1, x2, y2 = pos1.x, pos1.y, pos2.x, pos2.y
        let dx, dy = abs(x1 - x2), abs(y1 - y2)
        let sqrdx, sqrdy = pown dx 2, pown dy 2
        sqrt (sqrdx + sqrdy)

    let distanceVector (posFrom:Position) (posTo:Position) =
        let x1, y1, x2, y2 = posTo.x, posTo.y, posFrom.x, posFrom.y
        let dx, dy = abs(x1 - x2), abs(y1 - y2)
        let rad = 
            let r = atan ((y1 - y2)/(x1 - x2))
            if posTo.x < posFrom.x then r + System.Math.PI
            else r
        { x = dx; y = dy; rad = rad}
    

    
    let DefaultFrictionValue = 0.02 

    let defaultFriction (v:float) (dt:float) (frictionValue:float) m =
        v - v * frictionValue

    let defaultFractionNormalized v dt frictionValue = defaultFriction v dt frictionValue 1.

    let checkEntityCollision entity enityList =
        
        ()
    
    let doElementsCollide (eA:Entity) (eB:Entity) =
        let matchShape = function | Circle(r) -> r | _ -> 0.
        let rA = matchShape eA.shape
        let rB = matchShape eB.shape
        if distance eA.pos eB.pos < (rA + rB) then
            Logger.logf "element %A and element %A collide" eA eB
            true
        else
            false

    let processCollisionInTile (tile : Generic.List<Entity>) =
        let processedElements = Generic.List()
        let toRemove = Generic.List()
        let checkCollision entity = 
            match entity.shape with
            | Circle(radius) ->
                let pos = entity.pos
                let collides e = if doElementsCollide entity e then distance entity.pos e.pos else -1.
                // This part is critical. Currently multi object collision might be wrong.
                let collidingElementOption = 
                    tile |> Seq.except [ entity ] |> Seq.filter (processedElements.Contains >> not) |> Seq.map (fun e -> e, collides e)
                    |> Seq.filter(fun (_, c) -> c > 0.) |> Seq.sortByDescending (fun (_, c) -> c) |> Seq.map (fun (e,_) -> e) |> Seq.tryHead

                match collidingElementOption with
                | None ->  ()
                | Some e -> // we have a collision. Apply the necessary changes to the most near objects.
                    if entity.collisionHandler() then toRemove.Add entity |> ignore
                    if e.collisionHandler () then toRemove.Add e |> ignore
                    do
                        let temp = { e.vel with rad = e.vel.rad }
                        e.vel <- { entity.vel with rad = entity.vel.rad }
                        entity.vel <- temp
                    [ e; entity ] |> Seq.iter (processedElements.Add >> ignore)
            | _ -> ()
        tile |> Seq.iter checkCollision
        toRemove |> Seq.iter (tile.Remove >> ignore)

    let step (entity : Entity) (slowDown: float -> float -> float -> float) dt  =
        let position, velocity = entity.pos, entity.vel
        let pos = 
            {  x = position.x + velocity.vx * dt
               y = position.y + velocity.vy * dt 
               rad = position.rad + velocity.rad * dt }
        let vel = 
            {  vx = slowDown velocity.vx dt entity.friction
               vy = slowDown velocity.vy dt entity.friction
               rad = slowDown velocity.rad dt entity.friction }
        entity.vel <- vel
        entity.pos <- pos
        
    let stepForGrid (grid : Grid) (slowDown: float -> float -> float -> float) dt =
        let tiles = grid.tiles
        let resultDict = new Generic.Dictionary<_,_>()
        for pair in grid.tiles do
            let key = pair.Key
            let entities = pair.Value
            entities |> processCollisionInTile 
            entities |> Seq.iter(fun e -> step e slowDown dt)
            entities 
            |> Seq.iter(
                    fun e ->
                        let lx = 
                            let x = e.pos.x / float grid.width
                            if x < 0. || x > float(grid.horizontalTiles - 1) then -1
                            else int x
                        let ly = 
                            let y = e.pos.y / float grid.height
                            if y < 0. || y > float(grid.verticalTiles - 1) then -1
                            else int y
                        if lx >= 0 && ly >= 0 then
                            if lx <> fst key && ly <> snd key then 
                                grid.tiles.[key].Remove e |> ignore
                                grid.tiles.[(lx, ly)].Add e
                                //Fable.Import.JS.debugger()
                            if e.uid.Length > 0 then resultDict.[e.uid] <- (e, lx, ly)
                        else
                            grid.tiles.[key].Remove e |> ignore
                            Logger.logf "Discarding entity %A, grid horizontal tiles count %A" e |> ignore)
        grid, resultDict


