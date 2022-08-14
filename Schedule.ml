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
    priority: int;
    mutable group: int option
}

(* read in a whole item and return it *)
let read_item maybe_item =
    let our_date = gen_date () in
    let item = match maybe_item with
    | Some i -> i
    | None   -> {text     = "";
                 complete = false;
                 repeat   = Never;
                 date     = Some our_date;
                 priority = 2;
                 group    = None}
    in
    let (in_date, def_date) = match item.date with
    | Some d -> (d, true)
    | None   -> (our_date, false)
    in
    let (date, repeat, priority) = if ReadKey.yesno "Date" def_date then begin
        let def_repeatq, count = match item.repeat with
            | Weeks  n -> 'w', n
            | Months n -> 'm', n
            | Years  n -> 'y', n
            | Days   n -> 'd', n
            | Never    -> 'n', 1
            | _ -> raise (Failure "Should never happen") in
        let repeatq = ReadKey.read_char_default "Repeat? (Week, Month, Year, Day, Never)" ['w';'m';'y';'d';'n'] def_repeatq in
        let count = if repeatq != 'n' then ReadKey.read_int_default "Count" count else 0 in
        let year  = ReadKey.read_int_default "Year"  in_date.year in
        let month = ReadKey.read_int_default "Month" in_date.month in
        let day   = ReadKey.read_int_default "Day"   in_date.day in
        let priority = ReadKey.read_int_default "Days until urgent" item.priority in
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
    let text = ReadKey.read_string_default "Text" item.text in
    if text = "" then raise (Failure "blank item") else
    let group = item.group in
    {text     = text;
     complete = false;
     repeat   = repeat;
     date     = date;
     priority = priority;
     group    = group}

(* this is our working schedule *)
let schedule = ref ([]: item list)
let filename = (Sys.getenv "HOME") ^ "/.schedule.sch"

(* lexicographic compare on items isn't quite satisfying *)
let compare_items a b =
    match (a.date, b.date) with
        |None,   None   -> 0
        |Some _, None   -> -1
        |None,   Some _ -> 1
        |Some x, Some y -> compare x y

let lookup_item idx =
    List.nth !schedule (idx - 1)

let alter_schedule f =
    schedule := f !schedule

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
                        match (item.complete, item.repeat) with
                        (* if it's not done, keep it *)
                        |(false, _) ->
                            ts_aux (item :: prefix) items
                        (* if it isn't repeating, discard it *)
                        |(_, Never) -> ts_aux prefix items
                        (* if it's a repeating item, spawn a new one *)
                        |(_, _) ->
                            match new_item_by_forward item with
                            |None ->
                                ts_aux (item :: prefix) items
                            |Some forwarded_item ->
                                ts_aux (forwarded_item :: prefix) items
                    end else ts_aux (item :: prefix) items in
    List.sort compare_items (List.rev (ts_aux [] schedule))

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
