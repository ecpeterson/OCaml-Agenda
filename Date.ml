open Unix

(* our representation of what a date is, note that this gets Marshal'ed.
 * note further that these fields are ordered so that compare (or <, whatever)
 * work appropriately on them.  don't shuffle the fields!  *)
type date = {
    year: int;
    month: int;
    day: int }
let old_date = {year = 0; month = 0; day = 0}

let print_date date =
    Printf.printf "%04d/%02d/%02d" date.year date.month date.day

let date_of_tm tm =
    {year  = tm.tm_year + 1900;
     month = tm.tm_mon  + 1;
     day   = tm.tm_mday}

(* get the current date *)
let gen_date () =
    date_of_tm (localtime (time ()))

(* check to see if a date record is within num days of the current time *)
let within_days date num =
    let our_date = gen_date () in
    match date with None -> false | Some date ->
        let years = date.year - our_date.year in
        let months = date.month - our_date.month + years*12 in
        let days = date.day - our_date.day + months*30 in
        if days <= num then true else false

let add_week date =
    let tm : Unix.tm =
        {tm_sec   = 0;
         tm_min   = 0;
         tm_hour  = 12;
         tm_mday  = date.day + 7;
         tm_mon   = date.month - 1;
         tm_year  = date.year - 1900;
         tm_wday  = 0;
         tm_yday  = 0;
         tm_isdst = false} in
    let (_, tm) = mktime tm in
    date_of_tm tm

let add_month date =
    if date.month = 12 then
        {date with
         month = 1;
         year = date.year + 1}
    else
        {date with
         month = date.month + 1}

let add_year date =
    {date with year = date.year + 1}
