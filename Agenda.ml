open AnsiLib
open Date
open ReadKey

(* generic error string *)
let invalid_string = "Invalid choice."

(* routines to print out the left-most column of the schedule display *)
let print_spacer () =
    print_string "        |-"

(* datum for a single entry, note that changing this will alter the file format
 * used to store the schedule via Marshal. *)
type repeatT = Weekly | Monthly | Yearly | Never
type item = {
    text: string;
    mutable complete: bool;
    repeat: repeatT;
    date: date option}

(* a helper routine for reading labeled integers with default values *)
let read_int_default tag default =
    Printf.printf "%s [%d]: " tag default;
    let response = read_line () in
    if response = "" then default else int_of_string response

(* read in a whole item, allow user to cancel, return item option *)
let read_item () =
    let our_date = gen_date () in
    let dateq = print_string "Date [Yn]: "; read_line () in
    let (date, repeat) = match dateq with
        |"n" | "N" ->
            (None, Never)
        |_ ->
            let repeatq = print_string "Repeat [w]eekly, repeat [m]onthly, repeat [y]early, [N]ever repeat: "; read_line () in
            let year  = read_int_default "Year"  our_date.year in
            let month = read_int_default "Month" our_date.month in
            let day   = read_int_default "Day"   our_date.day in
            let record_date = Some {year = year; month = month; day = day} in
            match (if repeatq = "" then 'n' else repeatq.[0]) with
                |'w' | 'W' -> (record_date, Weekly)
                |'m' | 'M' -> (record_date, Monthly)
                |'y' | 'Y' -> (record_date, Yearly)
                |_ -> (record_date, Never)
        in
    let text  = print_string "Text: "; read_line () in
    let response = print_string "Confirm [yN]: "; flush stdout;
        read_line () in
    match response.[0] with
    |'y' | 'Y' -> Some {text = text;
                        complete = false;
                        repeat = repeat;
                        date = date}
    |_ -> None

(* this is our working schedule *)
let schedule_title = ref "Agenda"
let schedule = ref (let h = Hashtbl.create 1 in
                    Hashtbl.add h !schedule_title ([]: item list); h)
let filename = (Sys.getenv "HOME") ^ "/.schedule.sch"

(* lexicographic compare on items isn't quite satisfying *)
let compare_items a b =
    match (a.date, b.date) with
        |None,   None   -> 0
        |Some _, None   -> -1
        |None,   Some _ -> 1
        |Some x, Some y -> compare x y

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

(* delete the num'th item of schedule *)
let rec delete_item schedule num =
    match schedule with [] -> [] | s :: ss ->
    if num = 1 then ss else s :: delete_item ss (num-1)

(* rip off the head of the schedule until we get to the current date *)
let trim_schedule schedule =
    let our_date = gen_date () in
    let rec ts_aux prefix schedule =
        match schedule with |[] -> List.rev prefix |item :: items ->
            match item.date with
                |None ->
                    ts_aux (item :: prefix) items
                |Some incoming_date ->
                    if (incoming_date < our_date) then begin
                        (* if it's a repeating item, spawn a new one *)
                        match (item.complete, item.repeat) with
                        |(false, _) ->
                            ts_aux (item :: prefix) items
                        |(_, Weekly) ->
                            let new_item = {item with
                                date     = Some (add_week incoming_date);
                                complete = false} in
                            ts_aux (new_item :: prefix) items
                        |(_, Monthly) ->
                            let new_item = {item with
                                date     = Some (add_month incoming_date);
                                complete = false} in
                            ts_aux (new_item :: prefix) items
                        |(_, Yearly) ->
                            let new_item = {item with
                                date     = Some (add_year incoming_date);
                                complete = false} in
                            ts_aux (new_item :: prefix) items
                        |(_, Never) -> ts_aux prefix items
                    end else schedule @ prefix in
    List.sort compare_items (ts_aux [] schedule)

(* file io routines *)
let read_schedule () =
    try
        let fh = open_in_bin filename in
        schedule := Marshal.from_channel fh;
        close_in fh
    with _ -> print_endline "Couldn't open preexisting schedule."

let write_schedule () =
    try
        let fh = open_out_bin filename in
        Marshal.to_channel fh !schedule [];
        close_out fh
    with _ -> print_endline "Couldn't write changes to schedule."

let alter_schedule f =
    Hashtbl.replace !schedule !schedule_title
        (f (Hashtbl.find !schedule !schedule_title))

(* the main loop for the program *)
let rec loop () =
    alter_schedule trim_schedule;
    print_string (clear_screen ());
    display_schedule ();
    do_menu menu
(* parses the 'menu' list given below, handles an abstract UI *)
and do_menu menu =
    let rec print_menu menu =
        match menu with [] -> () | (item, c, _) :: menu ->
        Printf.printf "%c) %s\n" c item;
        print_menu menu in
    (* print the menu *)
    print_menu menu;
    (* ask for a choice *)
    try
        print_string "Choice: ";
        cbreak stdin;
        let choice =
            match readkey stdin with
            | Char c -> (Printf.printf "%c\n" c; c)
            | _               -> raise (Failure invalid_string)
        in
        cooked stdin;
        let rec iterate menu choice =
            match menu with
                (_, c, f) :: menu -> if c = choice then f () else iterate menu choice
               |[] -> raise (Failure invalid_string) in
        iterate menu choice
    with _ ->
        (* if the user fucked up, do it again *)
        loop ()
(* and the meaty part of the menu, parsed by do_menu *)
and menu =
    ["Add item", 'a', (fun () ->
        begin match read_item () with None -> () | Some item ->
        alter_schedule (fun x -> List.sort compare_items (item :: x)) end;
        loop ());
     "Toggle completion", 't', (fun () ->
         print_string "Item: ";
         let sched = Hashtbl.find !schedule !schedule_title in
         begin try
             let i = List.nth sched (read_int () - 1) in
             i.complete <- not i.complete
         with _ -> print_endline invalid_string end;
         loop ());
     "Delete item", 'd', (fun () ->
         print_string "Item: ";
         alter_schedule (fun x -> delete_item x (read_int ()));
         loop ());
     "Refresh screen", 'r', loop;
     "Write schedule", 'w', (fun () -> write_schedule (); loop ());
     "Change schedule", 's', (fun () ->
        print_endline "Available lists are:";
        Hashtbl.iter (fun a b -> print_endline ("    " ^ a)) !schedule;
        print_string "Change list to: ";
        let response = read_line () in
        begin try let _ = Hashtbl.find !schedule response in
            schedule_title := response
        with Not_found ->
            print_string "Schedule does not exist!  Do you want to create it? [yN]: ";
            match read_line () with
                |"Y" | "y" ->
                    schedule_title := response;
                    Hashtbl.add !schedule response []
                |_ -> () end;
        loop ());
     "Quit", 'q', (fun () -> ())]

(* entry point for the program *)
let _ =
    read_schedule ();
    loop ();
    write_schedule ()
