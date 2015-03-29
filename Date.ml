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

let relative_offset date1 date2 =
    match (date1, date2) with
    | Some date1, Some date2 ->
        let (date1, _) = mktime { tm_sec = 0; tm_min = 0; tm_hour = 0;
                                  tm_mday = date1.day;
                                  tm_mon = date1.month - 1;
                                  tm_year = date1.year - 1900;
                                  tm_wday = 0; tm_yday = 0; tm_isdst = false; } in
        let (date2, _) = mktime { tm_sec = 0; tm_min = 0; tm_hour = 0;
                                  tm_mday = date2.day;
                                  tm_mon = date2.month - 1;
                                  tm_year = date2.year - 1900;
                                  tm_wday = 0; tm_yday = 0; tm_isdst = false; } in
        Some (int_of_float (((date1 -. date2) /. (60. *. 60. *. 24.)) +. 0.5))
    | _ -> None

(* check to see if a date record is within num days of the current time *)
let within_days date num =
    let our_date = Some (gen_date ()) in
    match relative_offset date our_date with
        | None -> false
        | Some days -> days <= num

let add_days days date =
    let tm : Unix.tm =
        {tm_sec   = 0;
         tm_min   = 0;
         tm_hour  = 12;
         tm_mday  = date.day + days;
         tm_mon   = date.month - 1;
         tm_year  = date.year - 1900;
         tm_wday  = 0;
         tm_yday  = 0;
         tm_isdst = false} in
    let (_, tm) = mktime tm in
    date_of_tm tm

let add_week date = add_days 7 date

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
