(*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'Of Turtles & Discriminated Unions'

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The goal of this exercise is to introduce F# discriminated
unions, and how they can be used to design a DSL.

We will write a simplfied version of the LOGO/Turtle 
language, using it to create SVG images.

For a quick example of Turtle/LOGO, here is an online 
version: http://www.transum.org/software/Logo/

Our strategy will be to create the instructions for our 
language, using F# discriminated unions, and transform a 
list of instructions into a list of SVG lines, which we will
render in an html document.
*)


(*
Creating an image with SVG
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

See the following for information on SVG:
http://www.w3schools.com/svg/
*)


// TODO: run the following code

let svgExample = """
<html>
<body>
  <h1>Turtles & F#!</h1>
    <svg width="100" height="100">
      <line x1="10" y1="20" x2="90" y2="80" stroke="red" stroke-width="2" />
    </svg>
  </body>
</html>"""

let path = __SOURCE_DIRECTORY__ + "/turtles.html"

System.IO.File.WriteAllText(path,svgExample)


// TODO: this should have created a document "turtle.html"
// Open it in your browser - this will be our output.



(*
Filling in a template
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Instead of having the content of the <svg> ... </svg> 
block being pre-filled, we want now to create a few lines,
and "inject" them as content into a template.
*)

// we will use sprintf "%s" to inject our content as a
// string inside the template:
let sprintfDemo = sprintf "<content start>%s</content end>" "some content"

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


// TODO
// run the following program, which should create a square
// and refresh / reopen the turtle.html file in the browser

let pointsForSquare = 
    [ 
        (20.0, 20.0, 20.0, 100.0, 1)
        (20.0, 100.0, 100.0, 100.0, 3)
        (100.0, 100.0, 100.0, 20.0, 1)
        (100.0, 20.0, 20.0, 20.0, 3)
    ]

let squareAsSvg =
    pointsForSquare
    // make a line for each point
    |> List.map svgLine
    // contatenate into one string
    |> String.concat "\n"
    // inject into the template
    |> inTemplate

System.IO.File.WriteAllText(path,squareAsSvg)


// TODO
// create a triangle... or whatever you want!

let pointsForTriangle =
  [
    (40.0, 40.0, 20.0, 100.0, 1)
    (40.0, 40.0, 60.0, 100.0, 1)
    (20.0, 100.0, 60.0, 100.0, 1)
  ]

let triangleAsSvg =
    pointsForTriangle
    // make a line for each point
    |> List.map svgLine
    // contatenate into one string
    |> String.concat "\n"
    // inject into the template
    |> inTemplate

System.IO.File.WriteAllText(path,triangleAsSvg)

(*
Defining our language
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Next, we are going to define our language, using
Discriminated Unions to represent the supported
instructions.
*)

// our initial language contains 3 instructions:
// (go) forward, (turn) left, repeat
type INSTRUCTION =
    | FORWARD of float // move fwd by x pixels
    | TURNLEFT of float // turn left by x degrees
    | TURNRIGHT of float // turn right by x degrees
    | SETPENSIZE of int // sets the pen size
    | REPEAT of int * INSTRUCTION list // repeat n times instructions

// we can now write a simple program, 
// as a list of INSTRUCTIONs
let simpleProgram = [ FORWARD 50.0; TURNLEFT (90.0); FORWARD 50.0 ]

// or a more complex one, with a 'nested program':
let complexProgram =
    [
        FORWARD 100.0
        TURNLEFT (90.0)
        REPEAT (5, 
            // this is a "nested program"
            [ 
                FORWARD 50.0
                TURNLEFT (90.0) 
            ])
    ]


// TODO
// simply run the code above



(*
Computing the position of the Turtle
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

What we want now is to take an initial position for the turtle,
and create the list of all its successive states, when we apply
a program (a list of instructions) to it.
*)

// the current state of the turtle: 
// position = where the turtle is on screen
// angle = what direction the turtle is pointed to
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

// we can now recursively process a program:
// we maintain a list of the states we generated so far,
// and process the instruction at the head of the
// instructions list, until there is nothing left.
let rec execute (states:State list) (program:INSTRUCTION list) =
    match states with
    // we need a starting state
    | [] -> failwith "Starting state required"
    // the current state is the head of the list of states
    | currentState :: previousStates ->
        match program with
        // no instruction left: return the list of states
        | [] -> states
        // otherwise, pick the head instruction to process
        | head :: tail ->
            // process each of the instruction types we support
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


// TODO: run the 2 initial programs, look at the output
// This should produce a list of the states the Turtle
// goes through, as the program executes

let initialState = { X = 250.0; Y = 250.0; Angle = 0.0; PenSize = 1 }
let simpleProgramOutput = run initialState simpleProgram


(*
Transforming the program output into an SVG file
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Running a program returns a list of states, the successive
positions of the 'turtle'. The only thing we need to do at
that point is take all these states, group them in pairs, 
and connect their positions with a line.
*)

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


// TODO 
// createSvg from one of the sample programs,
// and open the turtle.html file in your browser
let currentStateProgram = { X = 250.0; Y = 250.0; Angle = 90.0; PenSize = 1 } 
createSvg currentStateProgram simpleProgram

createSvg currentStateProgram complexProgram

// TODO
// create a program to draw and save a star?
let starState = { X = 150.0; Y = 100.0; Angle = 45.0; PenSize = 1 }
let starProgram  = 
  [ 
    SETPENSIZE 3
    REPEAT (6, [FORWARD 50.0; TURNLEFT (120.0); FORWARD 50.0; TURNRIGHT (60.0)])
  ]
createSvg starState starProgram

(*
Extending the language: RIGHT
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We currently have LEFT turns covered; technically, 
that's enough, but it would be nice to have a RIGHT
turn instruction... 
*)


// TODO
// modify the INSTRUCTIONS and add RIGHT
// modify the execute function accordingly




(*
Extending the language: pen size
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let's make our images a bit fancier, for instance, by 
allowing the user to change the size of our pen. 
We need now to add an instruction, SETPENSIZE of size, 
and we probably also need to add the current pen size 
to the state, so that we can modify the way we create 
and svg line...
*)


// TODO
// modify the INSTRUCTIONS and add SETPENSIZE
// modify the State record, to include a PenSize value
// modify the execute function accordingly
// modify the svgLine function to take in a size




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