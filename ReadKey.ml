open Unix

type key = Char of char | UP | DOWN | LEFT | RIGHT
exception Timed_out
exception Invalid_escape

let escape_timeout = 0.05

let cbreak fh =
    let fd = descr_of_in_channel fh in
    let termios = tcgetattr fd in
    termios.c_icanon <- false;
    termios.c_echo   <- false;
    termios.c_echoe  <- false;
    termios.c_echok  <- false;
    termios.c_echonl <- false;
    tcsetattr fd TCSANOW termios

let cooked fh =
    let fd = descr_of_in_channel fh in
    let termios = tcgetattr fd in
    termios.c_icanon <- true;
    termios.c_echo   <- true;
    termios.c_echoe  <- true;
    termios.c_echok  <- true;
    termios.c_echonl <- true;
    tcsetattr fd TCSANOW termios

let readchar_raw fd =
    let buf = String.create 1 in
    let _ = read fd buf 0 1 in
    buf.[0]

let readchar fh timeout =
    let fd = descr_of_in_channel fh in
    match timeout with
    | Some t -> (
        let (rout, _, _) = select [fd] [] [] t in
        match rout with
        | [fd] -> readchar_raw fd
        | _    -> raise Timed_out
      )
    | None -> readchar_raw fd

let readkey fh =
    let c = readchar fh None in
    if c == '\027' then
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
                raise Invalid_escape
        (* this just drops invalid escape sequences on the floor other than
         * the initial escape key, but that's probably fine for now *)
        with
        | Timed_out -> Char c
        | Invalid_escape -> Char c
        | e -> raise e
    else
        Char c
