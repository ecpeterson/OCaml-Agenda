open AnsiLib
open Date
open ReadKey
open Schedule

type loop_msg = ErrMsg of string | Notice of string | No_msg
exception Refresh

(* generic error string *)
let invalid_string = "Invalid choice."

(* routines to print out the left-most column of the schedule display *)
let print_spacer () =
    print_string "        |-"

(* a helper routine for reading labeled integers with default values *)
let read_int_default tag default =
    Printf.printf "%s [%d]: " tag default;
    let response = read_line () in
    if response = "" then default else int_of_string response

let read_char_default tag allowed default =
    let choices =
        Buffer.contents (
            List.fold_left (fun a b ->
                let c = (if b == default then
                    Char.uppercase b
                else
                    Char.lowercase b)
                in
                Buffer.add_char a c;
                a) (Buffer.create 4) allowed
        )
    in
    Printf.printf "%s [%s]: " tag choices;
    cbreak stdin false;
    let ret =
        try
            let rec rcd_aux () =
                let key = readkey stdin in
                (match key with
                | Char c -> (if List.exists (fun cc -> c == cc) allowed then
                                c
                            else
                                rcd_aux())
                | ENTER  -> default
                | _      -> rcd_aux ())
            in rcd_aux ()
        with e ->
            (cooked stdin true; raise e)
    in
    cooked stdin true;
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
    cbreak stdin false;
    let ret =
        try
            let rec yesno_aux () =
                let key = readkey stdin in
                (match key with
                 | Char c -> (match c with
                              | 'Y' | 'y' -> true
                              | 'N' | 'n' -> false
                              | _         -> yesno_aux ())
                 | ENTER  -> default
                 | _      -> yesno_aux ())
            in yesno_aux ()
        with e ->
            (cooked stdin true; raise e)
    in
    cooked stdin true;
    Printf.printf "%c\n" (if ret then 'y' else 'n');
    ret

(* read in a whole item, allow user to cancel, return item option *)
let read_item maybe_item =
    let our_date = gen_date () in
    let item = match maybe_item with
    | Some i -> i
    | None   -> {text     = "";
                 complete = false;
                 repeat   = Never;
                 date     = Some our_date}
    in
    let (in_date, def_date) = match item.date with
    | Some d -> (d, true)
    | None   -> (our_date, false)
    in
    let (date, repeat) = if yesno "Date" def_date then
        let def_repeatq = match item.repeat with
        | Weekly  -> 'w'
        | Monthly -> 'm'
        | Yearly  -> 'y'
        | Never   -> 'n'
        in
        let repeatq = read_char_default "Repeat? (weekly, monthly, yearly, never)" ['w';'m';'y';'n'] def_repeatq in
        let year  = read_int_default "Year"  in_date.year in
        let month = read_int_default "Month" in_date.month in
        let day   = read_int_default "Day"   in_date.day in
        let record_date = Some {year = year; month = month; day = day} in
        match repeatq with
            |'w' -> (record_date, Weekly)
            |'m' -> (record_date, Monthly)
            |'y' -> (record_date, Yearly)
            |'n' -> (record_date, Never)
            |_   -> raise (Failure "should never happen")
    else
        (None, Never)
    in
    let text = read_string_default "Text" item.text in
    {text     = text;
     complete = false;
     repeat   = repeat;
     date     = date}

(* display the working schedule *)
let display_schedule () =
    let our_date = gen_date () in
    let rec ds_aux incoming_items old_date number =
        (* iterate through the sorted items *)
        match incoming_items with [] -> () | item :: items ->
        (* check to see if we need to display the current date *)
        let next_date = match item.date with None -> our_date | Some date -> date in
        if next_date >= our_date && our_date > old_date then begin
            print_string ((set_style [Reset;Bright] White Black) ^
                          (Printf.sprintf "%04d/%02d/%02d ====== Today's Date\n"
                              our_date.year our_date.month our_date.day) ^
                          (set_style [Reset] White Black)) ;
            ds_aux incoming_items our_date number
        end else begin
        (* print either the date, a dateless line, or a continuation thing *)
        (match item.date with
            |None -> print_string "----------"
            |Some date ->
                if date <> old_date then print_date date else print_spacer () );
        print_string " [";
        (* this is the part that deals with the checkboxes, ANSI color codes
         * are a bit ugly *)
        print_string (if item.complete then
                            (color_text Blue ^ "x")
                      else if within_days item.date 1 then
                            (set_style [Reset;Bright] Red Black    ^ "!")
                      else if within_days item.date 3 then
                            (set_style [Reset;Bright] Yellow Black ^ "!")
                      else if within_days item.date 7 then
                            (set_style [Reset;Bright] Green Black  ^ "!")
                      else " ");
        print_string (color_text White ^ "] ");
        (* dump text and loop *)
        Printf.printf "%02d %s\n" number item.text;
        match item.date with
            |None -> ds_aux items our_date (number + 1)
            |Some date -> ds_aux items date (number + 1)
        end in
    (* print the header *)
    print_string AnsiLib.reset_cursor;
    let header = (set_style [Reset;Bright] White Black) ^
        (Printf.sprintf "================= List: %s\n" !schedule_title) ^
        (set_style [Reset] White Black) in
    print_string header;
    ds_aux (Hashtbl.find !schedule !schedule_title) old_date 1

(* the main loop for the program *)
let rec loop msg =
    alter_schedule trim_schedule;
    print_string (clear_screen ());
    display_schedule ();
    (match msg with
    | ErrMsg s -> print_endline (
        (set_style [Reset;Bright] Red Black)
      ^ s
      ^ (set_style [Reset] White Black)
      )
    | Notice s -> print_endline (
        (set_style [Reset] Blue Black)
      ^ s
      ^ (set_style [Reset] White Black)
      )
    | No_msg -> ());
    do_menu menu
(* parses the 'menu' list given below, handles an abstract UI *)
and do_menu menu =
    let opt_count = ref 0 in
    let rec print_menu menu =
        match menu with [] -> () | (item, c, _) :: menu ->
        Printf.printf "%c) %-20s" c item;
        opt_count := !opt_count + 1;
        if !opt_count mod 3 == 0 then print_endline "";
        print_menu menu in
    (* print the menu *)
    print_menu menu;
    if !opt_count mod 3 != 0 then print_endline "";
    (* ask for a choice *)
    try
        print_string "Choice: ";
        cbreak stdin false;
        let choice =
            match readkey stdin with
            | Char c   -> (Printf.printf "%c\n" c; c)
            | FORMFEED -> raise Refresh
            | _        -> raise (Failure invalid_string)
        in
        cooked stdin true;
        let rec iterate menu choice =
            match menu with
                (_, c, f) :: menu -> if c = choice then f () else iterate menu choice
               |[] -> raise (Failure invalid_string)
        in
        try
            iterate menu choice
        with Sys.Break -> loop (Notice "Cancelled") | e -> raise e
    with e ->
        cooked stdin true;
        match e with
        | Refresh   -> loop No_msg
        | Sys.Break -> raise Sys.Break
        (* if the user fucked up, do it again *)
        | Failure s -> loop (ErrMsg s)
        | _         -> loop (ErrMsg "Unknown error")
(* and the meaty part of the menu, parsed by do_menu *)
and menu =
    ["Add item", 'a', (fun () ->
        let item = read_item None in
        alter_schedule (fun x -> List.sort compare_items (item :: x));
        loop No_msg);
     "Toggle completion", 't', (fun () ->
         print_string "Item: ";
         let sched = Hashtbl.find !schedule !schedule_title in
         begin try
             let i = List.nth sched (read_int () - 1) in
             i.complete <- not i.complete
         with _ -> print_endline invalid_string end;
         loop No_msg);
     "Delete item", 'd', (fun () ->
         print_string "Item: ";
         alter_schedule (fun x -> delete_item x (read_int ()));
         loop No_msg);
     "Write schedule", 'w', (fun () ->
         write_schedule ();
         loop (Notice ("Wrote schedule to " ^ Schedule.filename)));
     "Change schedule", 's', (fun () ->
        print_endline "Available lists are:";
        Hashtbl.iter (fun a b -> print_endline ("    " ^ a)) !schedule;
        print_string "Change list to: ";
        let response = read_line () in
        begin try let _ = Hashtbl.find !schedule response in
            schedule_title := response
        with Not_found ->
            if yesno "Schedule does not exist!  Do you want to create it?" false then
                schedule_title := response;
                Hashtbl.add !schedule response []
        end;
        loop No_msg);
     "Quit", 'q', (fun () -> ())]

(* entry point for the program *)
let _ =
    Sys.catch_break true;
    read_schedule ();
    (try
        loop No_msg
    with
    | Failure s -> Printf.printf "Error: %s\n" s
    | Sys.Break -> print_endline ""
    | _         -> print_endline "Unknown error");
    write_schedule ()
