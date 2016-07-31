(**
 - title: Super amazing splat 
 - tagline: Splat clone using HTML5 canvas
 - app-style: height:668px; width:900px; margin:20px auto 20px auto;
 - intro: [classic Splat game] Based looely on the Ozmos game
   To play the game, use left and right keys!
*)
(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser

[<Emit("Math.random()")>]
let rand (): float = failwith "JS only"

module Win =
  // Get the canvas context for drawing
  let canvas = document.getElementsByTagName_canvas().[0]
  let context = canvas.getContext_2d()

  // Format RGB color as "rgb(r,g,b)"
  let ($) s n = s + n.ToString()
  let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

  /// Fill rectangle with given color
  let filled color rect =
      let ctx = context
      ctx.fillStyle <- U3.Case1 color
      ctx.fillRect rect

  /// Move element to a specified X Y position
  let position (x,y) (img : HTMLImageElement) =
      System.Console.WriteLine("X, {0} y {1}", x,y )
      img.style.left <- x.ToString() + "px"
      img.style.top <- (canvas.offsetTop + y).ToString() + "px"

  let dimensions () =
    canvas.width, canvas.height

  /// Get the first <img /> element and set `src` (do
  /// nothing if it is the right one to keep animation)
  // let image (src:string) =
  //     let image = document.getElementsByTagName_img().[0]
  //     if image.src.IndexOf(src) = -1 then image.src <- src
  //     image

  let createImage (src: string) =
    let img = document.createElement_img()
    img.height <- 150.
    img.width <- 150.
    img.src <- src
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

/// The width of the canvas
let width = 900.
/// The height of the canvas
let height = 668.
/// Height of the floor - the bottom black part
let floorHeight = 100.
/// Height of the atmosphere - the yellow gradient
let atmosHeight = 300.
let parachuteWidhtHeight = 62., 74.

Keyboard.init()

let canvas = document.getElementsByTagName_canvas().[0]
let ctx = canvas.getContext_2d()
canvas.width <- width
canvas.height <- height

(**
This demo shows a simple game written using Fable and HTML5 canvas. One interesting
aspect of the game is the [asynchronous game loop](#Asynchronous-game-loop), which
updates the game state 60 times per second in a loop. This is implemented using F#
asynchronous workflows, which make it possible to capture the logic as a recursive
function, rather than using mutable state.

The Ozmo game uses the [Keyboard helpers from the Mario sample](../mario/index.html#Keyboard-helpers),
so if you want to see those, check out the Mario sample first - it is also simpler,
so you can check it out for lighter introduction to Fable and F#.

## Drawing the world

The first few functions in the game deal with rendering. The world consists of two
gradients (with yellow orange gradient in the sky and gray gradient for the atmosphere)
and a filled black rectangle. The `drawGrd` function draws a gradient and `drawBg`
renders the world. We also need `drawText` for printing text when the game finishes:

*)
/// Draw gradient between two Y offsets and two colours
let drawGrd (ctx:CanvasRenderingContext2D)
    (canvas:HTMLCanvasElement) (y0,y1) (c0,c1) =
  let grd = ctx.createLinearGradient(0.,y0,0.,y1)
  grd.addColorStop(0.,c0)
  grd.addColorStop(1.,c1)
  ctx.fillStyle <- U3.Case2 grd
  ctx.fillRect(0.,y0, canvas.width, y1- y0)

/// Draw background of the Ozmo game
let drawBg ctx canvas =
  drawGrd ctx canvas
    (0.,atmosHeight) ("blue","purple")
  drawGrd ctx canvas
    (atmosHeight, canvas.height-floorHeight)
    ("purple","white")
  ctx.fillStyle <- U3.Case1 "black"
  ctx.fillRect
    ( 0.,canvas.height-floorHeight,
      canvas.width,floorHeight )

/// Draw the specified text (when game finishes)
let drawText(text,x,y) =
  ctx.fillStyle <- U3.Case1 "white"
  ctx.font <- "bold 40pt";
  ctx.fillText(text, x, y)
(**
## Representing and drawing blobs

Each of the balls in the game is represented by a `Blob` value that stores
the X and Y coordinates, size of the blob (radius), its colour and current speed.
The type is used for both falling blobs and for the player's blob:
*)
type Blob =
  { X:float; Y:float;
    vx:float; vy:float;
    image: string;
    width: float;
    heigth: float;
    Radius:float; color:string }
(**
Drawing blob on the canvas is quite easy - the following function does that using
the `arc` function of the 2D rendering context of the canvas:
*)
let drawBlob (ctx:CanvasRenderingContext2D)
    (canvas:HTMLCanvasElement) (blob:Blob) =
  if (blob.image = "") then
    ctx.beginPath()
    ctx.arc
      ( blob.X, canvas.height - (blob.Y + floorHeight + blob.Radius),
        blob.Radius, 0., 2. * System.Math.PI, false )
    ctx.fillStyle <- U3.Case1 blob.color
    ctx.fill()
    ctx.lineWidth <- 3.
    ctx.strokeStyle <- U3.Case1 blob.color
    ctx.stroke()
  else
    if (blob.Y < height - floorHeight - fst parachuteWidhtHeight ) then
      let i  =blob.image
              |> Win.createImage
      ctx.drawImage(U3.Case1 i, blob.X, blob.Y)
(**

## Falling blobs and collisions

The next step is to define the physics for the game. This consists of several
functions that update the `Blob` objects and are composed to apply all rules of
physics in the main game loop.
*)
/// Apply key effects on Player's blob - changes X speed
let direct (dx,dy) (blob:Blob) =
  { blob with vx = blob.vx + (float dx)/4.0 }

/// Apply gravity on falling blobs - gets faster every step
let gravity (blob:Blob) =
  if blob.Y > 0. then { blob with vy = blob.vy + 0.1 }
  else blob

/// Bounde Player's blob off the wall if it hits it
let bounce (blob:Blob) =
  let n = width
  if blob.X < 0. then
    { blob with X = -blob.X; vx = -blob.vx }
  elif (blob.X > n) then
    { blob with X = n - (blob.X - n); vx = -blob.vx }
  else blob

/// Move blob by one step - adds X and Y
/// velocities to the X and Y coordinates
let move (blob:Blob) =
  { blob with
      X = blob.X + blob.vx
      Y = max 0.0 (blob.Y + blob.vy) }
(**
The above functions capture the individual aspects of the movement. The
following put everything together and handle steps of Player's blob and
also collision detection.
*)
/// Apply step on Player's blob. Composes above functions.
let step dir blob =
  blob |> direct dir |> move |> bounce

/// Check whether two blobs collide
let collide (a:Blob) (b:Blob) =
  let dx = (a.X - b.X)*(a.X - b.X)
  let dy = (a.Y - b.Y)*(a.Y - b.Y)
  let dist = sqrt(dx + dy)
  dist < abs(a.Radius - b.Radius)

/// Remove all falling blobs that hit Player's blob
let absorb (blob:Blob) (drops:Blob list) =
  drops |> List.filter (fun drop ->
    collide blob drop |> not )
(**
## Game logic helpers

Next, we define a couple of helpers for generating and updating the falling blobs.
We have black growing blobs and white shrinking blobs. The `newGrow` and `newShrink`
functions are used to generate new blobs:
*)
let p1Color = "red"
let p2Color = "blue"

let newDrop x color playerNumber=
  { X = x
    Y=50.; Radius=10.; vx=0.; vy = 0.0;
    image= "images/para"+playerNumber+".png";
    width = 88.0;
    heigth = 105.0;
    color=color }
let p1 () = newDrop (0. + 5.)  p1Color "1"
let p2 () = newDrop (width - fst parachuteWidhtHeight) p2Color "2"


(**
## Asynchronous game loop

The asynchronous game loop is perhaps the most interesting part of the source code.
Fable supports F# asynchronous workflows, which give us a way to write non-blocking loop
that includes sleeping in the middle, so you can write long-running processes as a recursive
loop rather than using timers and callbacks.

The following diagram illustrates the game loop:

    [lang=text]
    (start)        +----(tick)---+
       \           |             |
         +------+  |  +--------+ |   +-----------+
      +->| game |--+->| update |-+-->| completed |<-+
      |  +------+     +--------+     +-----------+  |
      |                                             |
      +-----------(after 10 seconds)----------------+

There are three states in which the game can be:

 - After starting, the `game` state initializes the Player's blob and starts the game
 - The `update` loop is active when the game is running. It calls itself recursively
   until the game ends.
 - After finishing, the `completed` state displays a message and sleeps for 10 seconds
   before starting a new game.

Using asynchronous workflows, the state machine can be represented using 3 mutually
recursive functions, each representing one of the states. The `game` and `completed`
states are simple:
*)
/// Starts a new game

let p1LandingPad = { X = 300.; Y=0.; Radius=30.; vx=0.; vy=0.; image = ""; heigth= 8.; width = 9.; color=p1Color }
let p2LandingPad = {p1LandingPad with X=500. ; color = p2Color }

let rec game () = async {

  return! update [p1LandingPad; p2LandingPad] [p1 (); p2 ()] 0 }

/// Displays message and sleeps for 10 sec
and completed () = async {
  drawText ("COMPLETED",320.,300.)
  do! Async.Sleep 10000
  return! game () }
(**
Note that we are using `let rec .. and`, which lets us write multiple recursive functions
that can call each other. The `completed` function calls `game` after 10 seconds using
`return!` (representing an asynchronous tail-call) and the `game` function calls `update`
with the initial state. The `update` loop looks as follows:
*)
/// Keeps current state for Player's blob, falling
/// drops and the countdown since last drop was generated
and update blob drops countdown = async {
  // Update the drops & countdown
  // let drops, countdown = updateDrops drops countdown

  // Count drops, apply physics and count them again
  // let beforeGrow, beforeShrink = countDrops drops
  let drops =
    drops
    |> List.map (gravity >> move >> step (Keyboard.arrows())) 
    //|> absorb blob
  
  // let afterGrow, afterShrink = countDrops drops
  let drops = drops |> List.filter (fun blob -> blob.Y > 0.)

  //apply keyboard events
  // let blob = blob |> step (Keyboard.arrows())

  // Render the new game state
  drawBg ctx canvas
  for drop in drops do drawBlob ctx canvas drop
  drawBlob ctx canvas p1LandingPad
  drawBlob ctx canvas p2LandingPad

  // If the game completed, switch state
  // otherwise sleep and update recursively!
  if false then // set some WIN/ LOOSE conditions
    return! completed()
  else
    do! Async.Sleep(int (1000. / 60.))
    return! update blob drops countdown }
(**
The last thing that we need to do is to start the game in the initial `game`
state using `Async.StartImmediate`:
*)
game () |> Async.StartImmediate
