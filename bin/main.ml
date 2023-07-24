open Final_project.Jack_ast
open Final_project.Jack_parser
open Final_project.Compilation_engine
open Final_project.Vm_ast
open Final_project.Io
 
let _test2 () =
    let program =
{|
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/ArrayTest/Main.jack

// (identical to projects/09/Average/Main.jack)

/** Computes the average of a sequence of integers. */
class Main {
    function void main() {
        var Array a;
        var int length;
        var int i, sum;
	
	let length = Keyboard.readInt("HOW MANY NUMBERS? ");
	let a = Array.new(length);
	let i = 0;
	
	while (i < length) {
	   let a[i] = Keyboard.readInt("ENTER THE NEXT NUMBER: ");
	   let i = i + 1;
	}
	
	let i = 0;
	let sum = 0;
	
	while (i < length) {
	   let sum = sum + a[i];
	   let i = i + 1;
	}
	
	do Output.printString("THE AVERAGE IS: ");
	do Output.printInt(sum / length);
	do Output.println();
	
	return;
    }
}
|} in
    let program_ast = evalJP program in
    let () = print_endline @@ show_jackClass program_ast in
    let labeled_program_vm = jack_compiler [program_ast] in
    let () = List.iter (fun vm -> print_endline @@ show_vm vm) (List.hd labeled_program_vm |> snd) in
    ()

let _test1 () = 
    let program = 
{|
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/Square/SquareGame.jack

// (same as projects/09/Square/SquareGame.jack)

/**
 * Implements the Square Dance game.
 * This simple game allows the user to move a black square around
 * the screen, and change the square's size during the movement.
 * When the game starts, a square of 30 by 30 pixels is shown at the
 * top-left corner of the screen. The user controls the square as follows.
 * The 4 arrow keys are used to move the square up, down, left, and right.
 * The 'z' and 'x' keys are used, respectively, to decrement and increment
 * the square's size. The 'q' key is used to quit the game.
 */

class SquareGame {
   field Square square; // the square of this game
   field int direction; // the square's current direction: 
                        // 0=none, 1=up, 2=down, 3=left, 4=right

   /** Constructs a new Square Game. */
   constructor SquareGame new() {
      // Creates a 30 by 30 pixels square and positions it at the top-left
      // of the screen.
      let square = Square.new(0, 0, 30);
      let direction = 0;  // initial state is no movement
      return this;
   }

   /** Disposes this game. */
   method void dispose() {
      do square.dispose();
      do Memory.deAlloc(this);
      return;
   }

   /** Moves the square in the current direction. */
   method void moveSquare() {
      if (direction = 1) { do square.moveUp(); }
      if (direction = 2) { do square.moveDown(); }
      if (direction = 3) { do square.moveLeft(); }
      if (direction = 4) { do square.moveRight(); }
      do Sys.wait(5);  // delays the next movement
      return;
   }

   /** Runs the game: handles the user's inputs and moves the square accordingly */
   method void run() {
      var char key;  // the key currently pressed by the user
      var boolean exit;
      let exit = false;
      
      while (~exit) {
         // waits for a key to be pressed
         while (key = 0) {
            let key = Keyboard.keyPressed();
            do moveSquare();
         }
         if (key = 81)  { let exit = true; }     // q key
         if (key = 90)  { do square.decSize(); } // z key
         if (key = 88)  { do square.incSize(); } // x key
         if (key = 131) { let direction = 1; }   // up arrow
         if (key = 133) { let direction = 2; }   // down arrow
         if (key = 130) { let direction = 3; }   // left arrow
         if (key = 132) { let direction = 4; }   // right arrow

         // waits for the key to be released
         while (~(key = 0)) {
            let key = Keyboard.keyPressed();
            do moveSquare();
         }
     } // while
     return;
   }
}
|} in
    let program_ast = evalJP program in
    let () = print_endline @@ show_jackClass program_ast in
    let labeled_program_vm = jack_compiler [program_ast] in
    let () = List.iter (fun vm -> print_endline @@ show_vm vm) (List.hd labeled_program_vm |> snd) in
    ()

let _to_ast () =
    let () = print_endline "Input file or directory name" in
    let file = read_line () in
    let is_dir = Sys.is_directory file in
    let files = List.filter (filter_extension ".jack")
                (if is_dir
                then (dir_contents file)
                else (file :: [])) in
    let file_contents = List.map read_file files in
    let asts = List.map evalJP file_contents in
    let () = List.iter2 (fun f cls -> print_endline ("File: "^f) ; print_endline (show_jackClass cls)) files asts
        in ()

let to_vm () = 
    let () = print_endline "Input file or directory name" in
    let file = read_line () in
    let is_dir = Sys.is_directory file in
    let print = if is_dir then false
        else (print_endline "Output to file? [y/N]"; not @@ String.equal (read_line () |> String.trim |> String.lowercase_ascii) "y") in
    let files = List.filter (filter_extension ".jack")
                (if is_dir
                then (dir_contents file)
                else (file :: [])) in
    let file_contents = List.map read_file files in
    let asts = List.map evalJP file_contents in
    let (names, vms) = List.split @@ jack_compiler asts in
    let dir = Filename.dirname @@ List.hd files in
    let vm_strings = List.map (fun vm -> List.map show_vm vm |> String.concat "\n") vms in
    let () = if print
            then List.iter2 (fun f vm -> print_endline ("File: "^f) ; print_endline vm) names vm_strings
            else List.iter2 output_to_file (List.map (fun name -> dir ^ Filename.dir_sep ^ name ^ ".vm") names) (List.map (fun s -> s ^ "\n") vm_strings)
        in ()
 
let () = to_vm ()
