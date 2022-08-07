open Date

(* datum for a single entry, note that changing this will alter the file format
 * used to store the schedule via Marshal. *)
type repeatT = Weekly | Monthly | Yearly | Count of int | (* <-- deprecated *)
    Never | Days of int | Weeks of int | Months of int | Years of int
type item = {
    text: string;
    mutable complete: bool;
    repeat: repeatT;
    date: date option;
    priority: int}

(* this is our working schedule *)
let schedule_title = ref "Agenda"
let schedule = ref (let h = Hashtbl.create 1 in
                    Hashtbl.add h !schedule_title ([]: item list); h)
let current_schedule () =
    Hashtbl.find !schedule !schedule_title
let filename = (Sys.getenv "HOME") ^ "/.schedule.sch"

(* lexicographic compare on items isn't quite satisfying *)
let compare_items a b =
    match (a.date, b.date) with
        |None,   None   -> 0
        |Some _, None   -> -1
        |None,   Some _ -> 1
        |Some x, Some y -> compare x y

let lookup_item idx =
    let schedule = current_schedule () in
    let rec li_aux idx sched =
        match sched with
        | x::xs -> if idx == 1 then x else li_aux (idx - 1) xs
        | []    -> raise (Failure "lookup_item: out of bounds")
    in li_aux idx schedule

let alter_schedule f =
    Hashtbl.replace !schedule !schedule_title (f (current_schedule ()))

(* delete the num'th item of schedule *)
let rec delete_item schedule num =
    match schedule with [] -> [] | s :: ss ->
    if num = 1 then ss else s :: delete_item ss (num-1)

let rec replace_item schedule idx item =
    match schedule with
    | x::xs -> if idx == 1 then
                   item::xs
               else
                   x::(replace_item xs (idx - 1) item)
    | []    -> []

let sorted_index_for_item schedule item =
    let rec sifi_aux schedule item idx =
        match schedule with
        | x::xs -> if compare_items item x != 1 then
                       idx
                   else
                       sifi_aux xs item (idx + 1)
        | []    -> idx
    in sifi_aux schedule item 1

let new_item_by_forward item =
    match item.date with
    |None -> None
    |Some incoming_date ->
        match item.repeat with
        |Never -> None
        |Days n ->
            Some {item with
                date     = Some (add_days n incoming_date);
                complete = false}
        |Weeks n ->
            Some {item with
                date = Some (add_weeks n incoming_date);
                complete = false}
        |Months n ->
            Some {item with
                date = Some (add_months n incoming_date);
                complete = false}
        |Years n ->
            Some {item with
                date = Some (add_years n incoming_date);
                complete = false}
        |_ -> raise (Failure "Should never happen")

let rec forward_item schedule num =
    match schedule with [] -> [] | s :: ss ->
    if num = 1  then
        begin match new_item_by_forward s with
        |None -> ss
        |Some item -> item :: ss
    end else s :: forward_item ss (num - 1)

(* rip off the head of the schedule until we get to the current date *)
let trim_schedule schedule =
    let our_date = gen_date () in
    let rec ts_aux prefix schedule =
        match schedule with |[] -> prefix |item :: items ->
            match item.date with
                |None ->
                    ts_aux (item :: prefix) items
                |Some incoming_date ->
                    if (incoming_date < our_date) then begin
                        (* if it's a repeating item, spawn a new one *)
                        match (item.complete, item.repeat) with
                        |(false, _) ->
                            ts_aux (item :: prefix) items
                        |(_, Never) -> ts_aux prefix items
                        |(_, _) ->
                            match new_item_by_forward item with
                            |None ->
                                ts_aux (item :: prefix) items
                            |Some forwarded_item ->
                                ts_aux (forwarded_item :: prefix) items
                    end else ts_aux (item :: prefix) items in
    List.sort compare_items (List.rev (ts_aux [] schedule))

(* bring forward old schedule dumps to the modern format *)
let update_schedule () =
    let update_subschedule _ subschedule =
        let us_aux item = match item.repeat with
            |Weekly  -> {item with repeat = Weeks  1}
            |Monthly -> {item with repeat = Months 1}
            |Yearly  -> {item with repeat = Years  1}
            |Count n -> {item with repeat = Days   n}
            |_ -> item in
        Some (List.map us_aux subschedule) in
    Hashtbl.filter_map_inplace update_subschedule !schedule

(* file io routines *)
let read_schedule () =
    try
        let fh = open_in_bin filename in
        schedule := Marshal.from_channel fh;
        update_schedule ();
        close_in fh
    with _ -> print_endline "Couldn't open preexisting schedule."

let write_schedule () =
    try
        let fh = open_out_bin filename in
        Marshal.to_channel fh !schedule [];
        close_out fh
    with _ -> print_endline "Couldn't write changes to schedule."
