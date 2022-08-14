open Unix

type key =
    | Char of char
    | UP
    | DOWN
    | LEFT
    | RIGHT
    | BACKSPACE
    | TAB
    | ENTER
    | FORMFEED
    | ESCAPE
    | DELETE
    | Unknown of char
exception Timed_out
exception Invalid_escape
exception Invalid_key

let escape_timeout = 0.05
let refresh_timeout = 15.0

let cbreak fh echo =
    let fd = descr_of_in_channel fh in
    let termios = tcgetattr fd in
    termios.c_icanon <- false;
    termios.c_echo   <- echo;
    termios.c_echoe  <- echo;
    termios.c_echok  <- echo;
    termios.c_echonl <- echo;
    tcsetattr fd TCSANOW termios

let cooked fh echo =
    let fd = descr_of_in_channel fh in
    let termios = tcgetattr fd in
    termios.c_icanon <- true;
    termios.c_echo   <- echo;
    termios.c_echoe  <- echo;
    termios.c_echok  <- echo;
    termios.c_echonl <- echo;
    tcsetattr fd TCSANOW termios

let readchar_raw fd =
    let buf = Bytes.create 1 in
    flush_all ();
    let _ = read fd buf 0 1 in
    Bytes.get buf 0

let readchar fh timeout =
    let fd = descr_of_in_channel fh in
    match timeout with
    | Some t -> (
        flush Stdlib.stdout;
        let (rout, _, _) = select [fd] [] [] t in
        match rout with
        | [fd] -> readchar_raw fd
        | _    -> raise Timed_out
      )
    | None -> readchar_raw fd

let key_of_char c =
    match Char.code c with
    | n when n >= 32 && n < 127 -> Char c
    | 8                         -> BACKSPACE
    | 9                         -> TAB
    | 10                        -> ENTER
    | 12                        -> FORMFEED
    | 27                        -> ESCAPE
    | 127                       -> DELETE
    | n                         -> Unknown c

let char_of_key k =
    match k with
    | Char c    -> c
    | BACKSPACE -> Char.chr 8
    | TAB       -> Char.chr 9
    | ENTER     -> Char.chr 10
    | FORMFEED  -> Char.chr 12
    | ESCAPE    -> Char.chr 27
    | DELETE    -> Char.chr 127
    | Unknown c -> c
    | _         -> raise Invalid_key

let readkey fh =
    let k = key_of_char (readchar fh (Some refresh_timeout)) in
    if k == ESCAPE then
        try
            let c = readchar fh (Some escape_timeout) in
            if c == '[' then
                let c = readchar fh (Some escape_timeout) in
                match c with
                | 'A' -> UP
                | 'B' -> DOWN
                | 'C' -> RIGHT
                | 'D' -> LEFT
                | _   -> raise Invalid_escape
            else
                ESCAPE
        (* this just drops invalid escape sequences on the floor other than
         * the initial escape key, but that's probably fine for now *)
        with
        | Timed_out      -> ESCAPE
        | Invalid_escape -> ESCAPE
        | e              -> raise e
    else
        k

(*
 * Some useful wrapper functions around readkey.
 *)

let read_int_default tag default =
    Printf.printf "%s [%d]: " tag default;
    let response = read_line () in
    if response = "" then default else int_of_string response

let read_char_default tag allowed default =
    let choices =
        Buffer.contents (
            List.fold_left (fun a b ->
                let c = (if b == default then
                    Char.uppercase_ascii b
                else
                    Char.lowercase_ascii b)
                in
                Buffer.add_char a c;
                a) (Buffer.create 4) allowed
        )
    in
    Printf.printf "%s [%s]: " tag choices;
    cbreak Stdlib.stdin false;
    let ret =
        try
            let rec rcd_aux () =
                try
                    let key = readkey Stdlib.stdin in
                    (match key with
                    | Char c -> (if List.exists (fun cc -> c == cc) allowed then
                                    c
                                else
                                    rcd_aux())
                    | ENTER  -> default
                    | _      -> rcd_aux ())
                with Timed_out -> rcd_aux () | e -> raise e
            in rcd_aux ()
        with e ->
            (cooked Stdlib.stdin true; raise e)
    in
    cooked Stdlib.stdin true;
    Printf.printf "%c\n" ret;
    ret

let read_string_default tag default =
    if String.length default == 0 then
        Printf.printf "%s: " tag
    else
        Printf.printf "%s [\"%s\"]: " tag default;
    match read_line () with
    | "" -> default
    | s  -> s

let yesno tag default =
    Printf.printf "%s [%s]: " tag (if default then "Yn" else "yN");
    cbreak Stdlib.stdin false;
    let ret =
        try
            let rec yesno_aux () =
                try
                    let key = readkey Stdlib.stdin in
                    (match key with
                     | Char c -> (match c with
                                  | 'Y' | 'y' -> true
                                  | 'N' | 'n' -> false
                                  | _         -> yesno_aux ())
                     | ENTER  -> default
                     | _      -> yesno_aux ())
                with Timed_out -> yesno_aux () | e -> raise e
            in yesno_aux ()
        with e ->
            (cooked Stdlib.stdin true; raise e)
    in
    cooked Stdlib.stdin true;
    Printf.printf "%c\n" (if ret then 'y' else 'n');
    ret
