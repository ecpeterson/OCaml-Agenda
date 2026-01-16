open AnsiLib
open Date
open ReadKey
open Schedule

type loop_msg = ErrMsg of string | Notice of string | No_msg
exception Refresh

(* default item choice *)
let last_idx = ref 1

(* active group filter *)
let group = ref (None: int option)

(* generic error string *)
let invalid_string = "Invalid choice."

(* routines to print out the left-most column of the schedule display *)
let print_spacer date next_date =
    if Some date = next_date then
        print_string "        ├─"
    else
        print_string "        └─"

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
        let next_date = match items with [] -> None | next_item :: _more_items -> next_item.date in
        (* print either the date, a dateless line, or a continuation thing *)
        (match item.date with
            |None -> print_string "----------"
            |Some date ->
                if date <> old_date then print_date date else print_spacer date next_date );
        print_string " [";
        (* this is the part that deals with the checkboxes, ANSI color codes
         * are a bit ugly *)
        let date_offset = relative_offset item.date (Some our_date) in
        print_string (match (date_offset, item.complete) with
            (* if the item is complete... *)
            (* | (Some n, true) when n <= 7 -> *)
            (*     set_style [Reset;Bright] Blue Black ^ (string_of_int n) *)
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
        let highlight_color = match item.group, !group with
            |Some n, Some m when n = m -> set_style [Reset;Bright] Cyan Black
            |_ -> color_text White in
        let reset_color = color_text White in
        Printf.printf "%02d %s%s%s\n" number highlight_color item.text reset_color;
        match item.date with
            |None -> ds_aux items our_date (number + 1)
            |Some date -> ds_aux items date (number + 1)
        end in
    (* print the header *)
    Printf.printf "%s%s================= Agenda\n%s"
        AnsiLib.reset_cursor
        (set_style [Reset;Bright] White Black)
        (set_style [Reset] White Black);
    ds_aux !schedule old_date 1

(*
 * menu items
 *)

let menu_add_item () =
    let item = read_item None in
    alter_schedule (fun x ->
        last_idx := sorted_index_for_item x item;
        item :: x)

let menu_edit_item () =
    let idx = read_int_default "Item" !last_idx in
    let item = read_item (Some (lookup_item idx)) in
    alter_schedule (fun x ->
        last_idx := sorted_index_for_item (delete_item x idx) item;
        replace_item x idx item)

let menu_toggle_completion () =
    let idx = read_int_default "Item" !last_idx in
    let item = lookup_item idx in
    item.complete <- not item.complete;
    begin match item.date with
        (* it'll get cleaned *)
        | Some date when item.complete && date < gen_date ()
            -> last_idx := 1
        | _ -> last_idx := idx
    end

let menu_delete_item () =
    let idx = read_int_default "Item" !last_idx in
    alter_schedule (fun x -> delete_item x idx);
    last_idx := 1

let menu_forward_item () =
    let idx = read_int_default "Item" !last_idx in
    let item = lookup_item idx in
    alter_schedule (fun x ->
        match new_item_by_forward item with
        |None ->
            last_idx := 1;
            delete_item x idx
        |Some fresh_item ->
            last_idx := sorted_index_for_item (delete_item x idx) fresh_item;
            replace_item x idx fresh_item)

let menu_inherit_group () =
    let write_idx = read_int_default "Item" !last_idx in
    let read_idx = read_int_default "Parent" !last_idx in
    let parent_group = (lookup_item read_idx).group in
    begin match parent_group with
    |None -> 
        let new_group = 1 + (List.fold_left
            (fun a b -> match b.group with None -> a | Some bb -> max a bb)
            0 !schedule) in
        (lookup_item read_idx).group <- Some new_group;
        (lookup_item write_idx).group <- Some new_group
    |Some _ ->
        (lookup_item write_idx).group <- parent_group
    end;
    last_idx := write_idx

let menu_unset_group () =
    last_idx := read_int_default "Item" !last_idx;
    (lookup_item !last_idx).group <- None

let menu_highlight_group () =
    last_idx := read_int_default "Filter" !last_idx;
    group := (lookup_item !last_idx).group

let menu_unhighlight_group () =
    group := None

let build_menu () = [
    "Add item",          'a', menu_add_item,          No_msg;
    "Edit item",         'e', menu_edit_item,         No_msg;
    "Toggle completion", 't', menu_toggle_completion, No_msg;
    "Delete item",       'd', menu_delete_item,       No_msg;
    "Forward item",      'f', menu_forward_item,      No_msg;
    "Inherit group",     'g', menu_inherit_group,     No_msg;
    "Unset group",       'u', menu_unset_group,       No_msg;
    (match !group with
    |None -> "Highlight group", 'h', menu_highlight_group,   No_msg
    |_    -> "Unset highlight", 'h', menu_unhighlight_group, No_msg);
    "Write schedule",    'w', write_schedule,         Notice ("Wrote schedule to " ^ Schedule.filename)
]

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
    let menu = (List.map (fun (a, b, c, d) -> (a, b, fun () -> c (); loop d))
                         (build_menu ())) in
    do_menu (menu @ ["Quit", 'q', (fun () -> ())])
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
