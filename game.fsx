module Game
open System

let floorHeight = 100.
let atmosHeight = 300.
let parachuteWidhtHeight = 62., 74.
let width = 900.
let height = 668.


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
    | InPlane
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








///////////////////////////
// JUAN JUAN JUAN JUAN JUAN
///////////////////////////
