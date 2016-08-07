module Game
open System

type Sprite =
    {
        x     : double
        y     : double
        vx    : double
        vy    : double
        image : string
    }


type Mikishida =
    | Player1 of hasJumped : bool * parachuteOpened : double option *  Sprite
    | Player2 of hasJumped : bool * parachuteOpened : double option *  Sprite
    | Plane1 of Sprite
    | Plane2 of Sprite
    | Platform1 of Sprite
    | Platform2 of Sprite
    
    
type Splatzz =
    {
        mikis : Mikishida list
        score : (int * int)
        round : int
        wind : double
    }












///////////////////////////
// JUAN JUAN JUAN JUAN JUAN
///////////////////////////
