open AnsiLib
open Date
open ReadKey
open Schedule

type loop_msg = ErrMsg of string | Notice of string | No_msg
exception Refresh

(* default item choice *)
let last_idx = ref 1

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
                    Char.uppercase_ascii b
                else
                    Char.lowercase_ascii b)
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
                try
                    let key = readkey stdin in
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
                try
                    let key = readkey stdin in
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
            (cooked stdin true; raise e)
    in
    cooked stdin true;
    Printf.printf "%c\n" (if ret then 'y' else 'n');
    ret

(* read in a whole item and return it *)
let read_item maybe_item =
    let our_date = gen_date () in
    let item = match maybe_item with
    | Some i -> i
    | None   -> {text     = "";
                 complete = false;
                 repeat   = Never;
                 date     = Some our_date;
                 priority = 2}
    in
    let (in_date, def_date) = match item.date with
    | Some d -> (d, true)
    | None   -> (our_date, false)
    in
    let (date, repeat, priority) = if yesno "Date" def_date then begin
        let def_repeatq, count = match item.repeat with
            | Weeks  n -> 'w', n
            | Months n -> 'm', n
            | Years  n -> 'y', n
            | Days   n -> 'd', n
            | Never    -> 'n', 0
            | _ -> raise (Failure "Should never happen") in
        let repeatq = read_char_default "Repeat? (Week, Month, Year, Day, Never)" ['w';'m';'y';'d';'n'] def_repeatq in
        let count = if repeatq != 'n' then read_int_default "Count" count else 0 in
        let year  = read_int_default "Year"  in_date.year in
        let month = read_int_default "Month" in_date.month in
        let day   = read_int_default "Day"   in_date.day in
        let priority = read_int_default "Days until urgent" item.priority in
        let record_date = Some {year = year; month = month; day = day} in
        match repeatq with
            |'w' -> (record_date, Weeks  count, priority)
            |'m' -> (record_date, Months count, priority)
            |'y' -> (record_date, Years  count, priority)
            |'d' -> (record_date, Days   count, priority)
            |'n' -> (record_date, Never,        priority)
            |_   -> raise (Failure "should never happen")
    end else
        (None, Never, 0)
    in
    let text = read_string_default "Text" item.text in
    if text = "" then raise (Failure "blank item") else
    {text     = text;
     complete = false;
     repeat   = repeat;
     date     = date;
     priority = priority}

(* display the working schedule *)
let display_schedule () =
    let our_date = gen_date () in
    let rec ds_aux incoming_items old_date number =
        (* iterate through the sorted items *)
        match incoming_items with [] -> () | item :: items ->
        (* check to see if we need to display the current date *)
        let next_date = match item.date with None -> our_date | Some date -> date in
        if  next_date >= our_date && our_date > old_date then begin
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
        let date_offset = relative_offset item.date (Some our_date) in
        print_string (match (date_offset, item.complete) with
            (* if the item is complete... *)
            | (Some n, true) when n <= 7 ->
                set_style [Reset;Bright] Blue Black ^ (string_of_int n)
            | (_, true) -> (set_style [Reset;Bright] Blue Black ^ "x")
            (* if the item is incomplete and far off... *)
            | (Some n, false) when (n > 7) && (n < item.priority) ->
                color_text Red ^ "+"
            | (Some n, false) when (n > 7) && (n < item.priority*2) ->
                color_text Yellow ^ "+"
            | (Some n, false) when (n > 7) && (n < item.priority*4) ->
                color_text Green ^ "+"
            | (Some n, false) when (n >= item.priority*4) ->
                " " (* TODO: is this what you want? *)
            (* if the item is incomplete and near... *)
            | (Some n, false) when (n < item.priority) && (n >= 0) ->
                (set_style [Reset;Bright] Red Black ^ (string_of_int n))
            | (Some n, false) when (n < item.priority * 2) && (n >= 0) ->
                (set_style [Reset;Bright] Yellow Black ^ (string_of_int n))
            | (Some n, false) when (n < item.priority * 4) && (n >= 0) ->
                (set_style [Reset;Bright] Green Black ^ (string_of_int n))
            (* if the item is past due... *)
            | (Some n, false) when n < -7 ->
                (set_style [Reset;Bright] Magenta Black ^ "+")
            | (Some n, false) when n < 0 && n >= -7 ->
                (set_style [Reset;Bright] Magenta Black ^ (string_of_int (-n)))
            (* if the item is untimed... *)
            | (None, false) -> " "
            (* if we fucked up... *)
            | _ -> "?");
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
        | Timed_out -> loop No_msg
        (* if the user fucked up, do it again *)
        | Failure s -> loop (ErrMsg s)
        | _         -> loop (ErrMsg "Unknown error")
(* and the meaty part of the menu, parsed by do_menu *)
and menu =
    ["Add item", 'a', (fun () ->
        let item = read_item None in
        alter_schedule (fun x ->
            last_idx := sorted_index_for_item x item;
            item :: x);
        loop No_msg);
     "Edit item", 'e', (fun () ->
         let idx = read_int_default "Item" !last_idx in
         let item = read_item (Some (lookup_item idx)) in
         alter_schedule (fun x ->
             last_idx := sorted_index_for_item (delete_item x idx) item;
             replace_item x idx item);
         loop No_msg);
     "Toggle completion", 't', (fun () ->
         let idx = read_int_default "Item" !last_idx in
         let item = lookup_item idx in
         item.complete <- not item.complete;
         begin match item.date with
         (* it'll get cleaned *)
         | Some date when item.complete && date < gen_date ()
             -> last_idx := 1
         | _ -> last_idx := idx
         end;
         loop No_msg);
     "Delete item", 'd', (fun () ->
         let idx = read_int_default "Item" !last_idx in
         alter_schedule (fun x -> delete_item x idx);
         last_idx := 1;
         loop No_msg);
     "Forward item", 'f', (fun () ->
         let idx = read_int_default "Item" !last_idx in
         let item = lookup_item idx in
         alter_schedule (fun x ->
            match new_item_by_forward item with
            |None ->
                last_idx := 1;
                delete_item x idx
            |Some fresh_item ->
                last_idx := sorted_index_for_item (delete_item x idx) fresh_item;
                replace_item x idx fresh_item);
         loop No_msg);
     "Write schedule", 'w', (fun () ->
         write_schedule ();
         loop (Notice ("Wrote schedule to " ^ Schedule.filename)));
     "Change schedule", 's', (fun () ->
        print_endline "Available lists are:";
        Hashtbl.iter (fun a b -> print_endline ("    " ^ a)) !schedule;
        let response = read_string_default "Change list to" "Agenda" in
        begin try let _ = Hashtbl.find !schedule response in
            schedule_title := response;
            last_idx := 1
        with Not_found ->
            if yesno "Schedule does not exist!  Do you want to create it?" false then
                schedule_title := response;
                Hashtbl.add !schedule response [];
                last_idx := 1
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
