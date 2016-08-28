(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
#load "game.fsx"

open Fable.Core
open Fable.Import.Browser
open Game

[<Emit("Math.random()")>]
let rand (): float = failwith "JS only"

module Win =
  let canvas = document.getElementsByTagName_canvas().[0]
  let context = canvas.getContext_2d()

  let ($) s n = s + n.ToString()
  let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

  let filled color rect =
      let ctx = context
      ctx.fillStyle <- U3.Case1 color
      ctx.fillRect rect

  let position (x,y) (img : HTMLImageElement) =
      System.Console.WriteLine("X, {0} y {1}", x,y )
      img.style.left <- x.ToString() + "px"
      img.style.top <- (canvas.offsetTop + y).ToString() + "px"

  let dimensions () =
    canvas.width, canvas.height

  let createImage (src: Image) =
    let img = document.createElement_img()
    img.height <- src.width
    img.width <- src.height
    img.src <- src.image
    img

module Keyboard =
  let mutable keysPressed = Set.empty
  let code x = if keysPressed.Contains(x) then 1 else 0
  let arrows () = (code 39 - code 37, code 38 - code 40)
  let update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    let op =  if pressed then Set.add else Set.remove
    keysPressed <- op keyCode keysPressed
    null
  let init () =
    window.addEventListener_keydown(fun e -> update(e, true))
    window.addEventListener_keyup(fun e -> update(e, false))


Keyboard.init()

let canvas = document.getElementsByTagName_canvas().[0]
let ctx = canvas.getContext_2d()
canvas.width <- width
canvas.height <- height

let drawGrd (ctx:CanvasRenderingContext2D)
    (canvas:HTMLCanvasElement) (y0,y1) (c0,c1) =
  let grd = ctx.createLinearGradient(0.,y0,0.,y1)
  grd.addColorStop(0.,c0)
  grd.addColorStop(1.,c1)
  ctx.fillStyle <- U3.Case2 grd
  ctx.fillRect(0.,y0, canvas.width, y1- y0)

let drawBg ctx canvas =
  let atmosHeight = 300.
  drawGrd ctx canvas
    (0.,atmosHeight) ("blue","purple")
  drawGrd ctx canvas
    (atmosHeight, canvas.height-floorHeight)
    ("purple","white")
  ctx.fillStyle <- U3.Case1 "black"
  ctx.fillRect
    ( 0.,canvas.height-floorHeight, canvas.width,floorHeight )

let drawText(text,x,y) =
  ctx.fillStyle <- U3.Case1 "white"
  ctx.font <- "bold 40pt";
  ctx.fillText(text, x, y)

type Blob =
  { X:float; Y:float;
    vx:float; vy:float;
    image: string;
    width: float;
    heigth: float;
    Radius:float; color:string }

let drawBlob (ctx:CanvasRenderingContext2D)
    (canvas:HTMLCanvasElement) (blob:Sprite) =
  if (blob.image.image = "") then
    ctx.beginPath()
   // ctx.arc
   //   ( blob.X, canvas.height - (blob.Y + floorHeight + blob.Radius),
  //      blob.Radius, 0., 2. * System.Math.PI, false )
//    ctx.fillStyle <- U3.Case1 blob.color
    ctx.fill()
    ctx.lineWidth <- 3.
//    ctx.strokeStyle <- U3.Case1 blob.color
    ctx.stroke()
  else
    if (blob.x < height - floorHeight - fst parachuteWidhtHeight ) then
      let i  =blob.image
              |> Win.createImage
      ctx.drawImage(U3.Case1 i, blob.x, blob.y)

let direct (dx,dy) (blob:Sprite) =
  { blob with vx = blob.vx + (float dx)/4.0 }

let gravity (blob : Sprite) =
  if blob.y > 0. then { blob with vy = blob.vy + 0.1 }
  else blob

let move (blob : Sprite) =
  { blob with
      x = blob.x + blob.vx
      y = max 0.0 (blob.y + blob.vy) }

let step dir blob =
  blob |> direct dir |> move

let collide (a:Sprite) (b:Sprite) =
  let dx = (a.x - b.x)*(a.x - b.x)
  let dy = (a.y - b.y)*(a.y - b.y)
  let dist = sqrt(dx + dy)
  dist < abs(a.image.width - b.image.width) //YEs this is whatever for now

let bounce (blob:Sprite) =
  let n = width
  if blob.x < 0. then
    { blob with x = -blob.x; vx = -blob.vx }
  elif (blob.x > n) then
    { blob with x = n - (blob.x - n); vx = -blob.vx }
  else blob

let newDrop x color playerNumber=
  { X = x
    Y=50.; Radius=10.; vx=0.; vy = 0.0;
    image= "images/para"+playerNumber+".png";
    width = 88.0;
    heigth = 105.0;
    color=color }

let initialSplat = {
        mikis = []
        score  = (0, 0)
        round = 0
        wind = 0.0
        keysPressed = Set.empty
        }

let bla (x: Mikishida) : Sprite =
    match x with
    | Player1(o,t) -> t
    | Player2 (o,t) -> t
    | Plane1(x) -> x
    | Plane2(x) -> x
    | Platform1(x) -> x
    | Platform2(x) -> x


let rec update (splatz: Splatzz) = async {
    // splatzz.mikis
    // |> List.map(bla >> gravity >> move >> step) 
    // splatzz.keysPressed |> Set.map(step) 
    if false then // set some WIN/ LOOSE conditions
      return! completed()
    else
      do! Async.Sleep(int (1000. / 60.))
      return! update splatz}

and game () = async {
  return! update initialSplat }
and completed () = async {
  drawText ("COMPLETED",320.,300.)
  do! Async.Sleep 10000
  return! game () }

                         
// and updateOld  blob drops countdown = async {
//   let drops =
//     drops
//     |> List.map (gravity >> move >> step (Keyboard.arrows()))

//   let drops = drops |> List.filter (fun blob -> blob.Y > 0.)
//   drawBg ctx canvas
//   for drop in drops do drawBlob ctx canvas drop
//   drawBlob ctx canvas p1LandingPad
//   drawBlob ctx canvas p2LandingPad

//   if false then // set some WIN/ LOOSE conditions
//     return! completed()
//   else
//     do! Async.Sleep(int (1000. / 60.))
//     return! update blob drops countdown }

game () |> Async.StartImmediate
