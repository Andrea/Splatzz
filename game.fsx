module Game
open System

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

type Mikishida =
    | Player1 of hasJumped : bool * parachuteOpened : double option *  Sprite
    | Player2 of hasJumped : bool * parachuteOpened : double option *  Sprite
    | Plane1 of Sprite
    | Plane2 of Sprite
    | Platform1 of Sprite
    | Platform2 of Sprite
    with
        member this.ApplyDelta() =
            match this with
            | Player1(x,y,z) -> Player1(x,y,z.ApplyDelta())
            | Player2(x,y,z) -> Player2(x,y,z.ApplyDelta())
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
