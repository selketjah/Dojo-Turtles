(*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'Of Turtles & Discriminated Unions'

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*)

let path = __SOURCE_DIRECTORY__ + "/turtles.html"

let inTemplate content =
    sprintf """
<html>
<body>
    <h1>Turtles & F#!</h1>
    <svg width="500" height="500">
%s
    </svg>
</body>
</html>""" content

let svgLine (x1,y1,x2,y2, strokeWidth) =
    sprintf
        """<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="black" stroke-width="%i" />"""
        x1 y1 x2 y2 strokeWidth

type INSTRUCTION =
    | FORWARD of float // move fwd by x pixels
    | TURNLEFT of float // turn left by x degrees
    | TURNRIGHT of float // turn right by x degrees
    | SETPENSIZE of int // sets the pen size
    | REPEAT of int * INSTRUCTION list // repeat n times instructions

type State = { X:float; Y:float; Angle:float; PenSize:int }

let PI = System.Math.PI
let toRadians angle = angle * 2.0 * PI / 360.0

let moveForward (state:State) length =
    { state with
        X = state.X + length * cos (state.Angle |> toRadians)
        Y = state.Y + length * sin (state.Angle |> toRadians) }

let turnLeft (state:State) angle =
    { state with 
        Angle = (state.Angle + angle) % 360.0 }

let turnRight (state:State) angle =
    { state with 
        Angle = (state.Angle - angle) % 360.0 }

let setPenSize (state:State) penSize =
  { state with
      PenSize = penSize }

let rec execute (states:State list) (program:INSTRUCTION list) =
    match states with
    | [] -> failwith "Starting state required"
    | currentState :: previousStates ->
        match program with
        | [] -> states
        | head :: tail ->
            match head with
            | FORWARD(length) ->
                let nextState = moveForward currentState length
                execute (nextState :: states) tail
            | TURNLEFT(angle) ->
                let nextState = turnLeft currentState angle
                execute (nextState :: states) tail
            | TURNRIGHT(angle) ->
                let nextState = turnRight currentState angle
                execute(nextState:: states) tail
            | SETPENSIZE(size) ->
                let nextState = setPenSize currentState size
                execute(nextState:: states) tail
            | REPEAT(repeat,sub) ->
                let rec runSub iter result =
                    match (iter < repeat) with
                    | false -> result
                    | true  ->
                        let result' = execute result sub
                        runSub (iter+1) result'
                let subResult = runSub 0 states
                execute subResult tail

let run (startState:State) (program:INSTRUCTION list) =
    let states = execute [ startState ] program
    states |> List.rev


let linesBetweenStates (output:State list) =
    output
    |> Seq.pairwise
    |> Seq.map (fun (state1,state2) ->
        state1.X, state1.Y, state2.X, state2.Y, state1.PenSize)

let save (content:string) = 
    System.IO.File.WriteAllText(path,content)

let createSvg (startState:State) (program:INSTRUCTION list) =
    run startState program
    |> linesBetweenStates
    |> Seq.map svgLine
    |> String.concat "\n"
    |> inTemplate
    |> save



// STAR PROGRAM
let starState = { X = 150.0; Y = 100.0; Angle = 45.0; PenSize = 1 }
let starProgram  = 
  [ 
    SETPENSIZE 3
    REPEAT (6, [FORWARD 50.0; TURNLEFT (120.0); FORWARD 50.0; TURNRIGHT (60.0)])
  ]
createSvg starState starProgram




(*
Extensions / going further
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Here we are - we have implemented a simple version of LOGO!

At that point, what you want to do next is entirely up to you.
* You could try to add a couple of instructions, 
like SETCOLOR, or PENUP / PENDOWN. 
* Or, more ambitious, you could support variables, or FOR loops, 
or crazier things.

For reference, here are some more instructions from Logo:

http://derrel.net/ep/logo/logo_com.htm
http://www.snee.com/logo/logo4kids.pdf

In a totally different direction, perhaps you could build an 
interactive logo session, animating the Turtle?

Or - could we use Units of Measure to have a cleaner representation
for angles and length?

Otherwise, you can also check this video with Tomas Petricek,
which demonstrates how similar ideas can be used in different 
contexts:
https://vimeo.com/97315970
*)