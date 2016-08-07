#r "node_modules/fable-core/Fable.Core.dll"

open System
open Fable.Core
open Fable.Import.Browser

let floorHeight = 100.
let parachuteWidhtHeight = 62., 74.
let width = 900.
let height = 668.

[<Emit("Math.random()")>]
let chaos (): float = failwith "JS only"

type Image =
    {
        width : double
        height : double
        image : String
    }
               
type Sprite =
    {
        x     : double
        y     : double
        vx    : double
        vy    : double
        image : Image
    }
    with
        member this.ApplyDelta() =
            { this with x = this.x + this.vx; y = this.y + this.vy }

type PlayerState =
    | InAir of parachuteOpened : double option
    | Landed of freeFallTime : double
    | Splatted

type Mikishida =
    | Player1 of PlayerState  *  Sprite
    | Player2 of PlayerState  *  Sprite
    | Plane1 of Sprite
    | Plane2 of Sprite
    | Platform1 of Sprite
    | Platform2 of Sprite
    with
        member this.ApplyDelta() =
            match this with
            | Player1(x,y) -> Player1(x,y.ApplyDelta())
            | Player2(x,y) -> Player2(x,y.ApplyDelta())
            | Plane1 x -> Plane1(x.ApplyDelta())
            | Plane2 x -> Plane2(x.ApplyDelta())
            | Platform1(x) -> Platform1(x.ApplyDelta())
            | Platform2(x) -> Platform2(x.ApplyDelta())

type Splatzz =
    {
        mikis : Mikishida list
        score : (int * int)
        round : int
        wind : double
        keysPressed : Set<string>
    }

let getImage miki =
    match miki with
    | Plane1 _ -> { width = 150.; height = 171.; image = "p1.png" }
    | Plane2 _ -> { width = 150.; height = 171.; image = "p2.png" }
    | Player1(InAir(Some _), _) -> { width = 150.; height = 171.; image = "p1para.png" } // todo: sizes
    | Player2(InAir(Some _), _) -> { width = 150.; height = 171.; image = "p2para.png" } // todo: sizes
    | Player1(_) -> { width = 150.; height = 171.; image = "drop.png" }
    | Player2(_) -> { width = 150.; height = 171.; image = "drop.png" }
    | Platform1(_)  -> { width = 62.; height = 74.; image = "para1.png" }
    | Platform2(_) -> { width = 62.; height = 74.; image = "para1.png" }


//let createInitialState =



///////////////////////////
// JUAN JUAN JUAN JUAN JUAN
///////////////////////////
