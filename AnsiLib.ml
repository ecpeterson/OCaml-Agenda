(* This is a library of routines that dump ANSI display codes to stdout, useful
 * for positioning the cursor, clearing the screen, and displaying colored
 * text.  The motivation for making it a library is that the ANSI codes
 * themselves are pretty opaque, and giving the routines names makes code
 * considerably more readable. *)

let clear_screen () = "\027c"

type attribute = Reset | Bright | Dim | Underscore | Blink | Reverse | Hidden
type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let int_of_color color =
    match color with
        |Black   -> 0
        |Red     -> 1
        |Green   -> 2
        |Yellow  -> 3
        |Blue    -> 4
        |Magenta -> 5
        |Cyan    -> 6
        |White   -> 7

let int_of_attribute attribute =
    match attribute with
        |Reset -> 0
        |Bright -> 1
        |Dim -> 2
        |Underscore -> 4
        |Blink -> 5
        |Reverse -> 7
        |Hidden -> 8

let set_style attribute foreground background =
    let attribute = List.fold_left (fun x y -> x ^ (
        string_of_int (int_of_attribute y)) ^ ";") "" attribute in
    let foreground = string_of_int (30 + int_of_color foreground) in
    let background = string_of_int (40 + int_of_color background) in
    "\027[" ^ attribute ^ foreground ^ ";" ^ background ^ "m"

let color_text foreground =
    set_style [Reset] foreground Black

let reset_cursor =
    "\027[H"
