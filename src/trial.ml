open Glical
module Cal = CalendarLib.Calendar
module Printer = CalendarLib.Printer

type component =
  | VEVENT 
  | VTODO
  | UNSUPPORTED_CMP

type entry = {
    title: string option;
    location: string option;
    starts: string option;
    ends: string option;
    notes: string option
  }

type eventprop = 
  | DESCRIPTION
  | DTEND
  | DTSTART
  | SUMMARY
  | UNSUPPORTED_EP

let unwrap = function
  | Some s -> s
  | None -> ""

let init = {title = None; location = None; starts = None; ends = None; notes = None}

let dt_to_string ({year; month; day; hours; minutes; seconds; timezone}: Datetime.t) opdt =
  let dt = Cal.lmake ~year:year ~month:month ~day:day ~hour:hours ~minute:minutes () in
  let bf = Buffer.create 70 in
  match opdt with
  | Some str ->
     let e = Datetime.parse (1, 1) str in
     let dte = Cal.lmake ~year:e.year ~month:e.month ~day:e.day ~hour:e.hours ~minute:e.minutes () in
              begin
                Printer.Calendar.fprint "%F %a %R" (Format.formatter_of_buffer bf) dt;
                Printer.Calendar.fprint "-%R" (Format.formatter_of_buffer bf) dte;
                Buffer.contents bf
              end
  | None -> begin
      Printer.Calendar.fprint "%F %a %R" (Format.formatter_of_buffer bf) dt ;
      Buffer.contents bf
    end

let d_to_string ({year; month; day}: Date.t) opdt =
  let d = Cal.lmake ~year:year ~month:month ~day:day () in
  let bf = Buffer.create 70 in
  begin
    Printer.Calendar.fprint "%F %a" (Format.formatter_of_buffer bf) d ;
    Buffer.contents bf
  end

let rgxp = Str.regexp "<.+>"
let crlf = Str.regexp "\\\\n"

let show_entry ({title; location; starts; ends; notes}: entry) =
  let date = try dt_to_string (Datetime.parse (1, 1)(unwrap starts)) ends with
               Glical_kernel.Syntax_error(_)-> begin
                 try d_to_string (Date.parse (1, 1)(unwrap starts)) ends with
                   Glical_kernel.Syntax_error(_)-> "" end
  in
  begin
    Printf.printf "* %s \n" (unwrap title);
    Printf.printf "  <%s>\n" date;
    Printf.printf "%s \n" (unwrap location);
    Printf.printf "%s \n" (Str.replace_first rgxp "" (Str.global_replace crlf " " (unwrap notes)))
  end

let ep_of_string s =
  let is_DESCRIPTION = String.equal "DESCRIPTION" in
  let is_DTSTART = String.equal "DTSTART" in
  let is_DTEND = String.equal "DTEND" in
  let is_SUMMARY = String.equal "SUMMARY" in
  if is_DESCRIPTION s then DESCRIPTION
  else if is_DTEND s then DTEND
  else if is_DTSTART s then DTSTART
  else if is_SUMMARY s then SUMMARY
  else UNSUPPORTED_EP

let cmp_of_string s =
  let is_VEVENT = String.equal "VEVENT" in
  let is_VTODO = String.equal "VTODO" in
  if is_VEVENT s then VEVENT
  else if is_VTODO s then VTODO
  else UNSUPPORTED_CMP

let rec show = function
  | Ical.Block(_, n, icss) -> begin
      Printf.printf "[ %s ]\n" n ;
      List.iter show icss
    end
  | Ical.Assoc(_, k, _, v) -> Printf.printf "( %s | %s )\n" k (v.to_string())

let rec is_date_time = function
  | [] -> true
  | ("VALUE", d)::ps ->
     begin match Ical.(d.to_string()) with
     | "DATE-TIME" -> true
     | "DATE" -> false
     | _ -> is_date_time ps
     end

let col_entry {title; location; starts; ends; notes} = function
  | Ical.Block(l, n, _) -> begin
      Printf.printf "Something wrong with %s in (%i | %i) \n" n (fst l) (snd l);
      {title; location; starts; ends; notes}
    end
  | Ical.Assoc(l, k, p, v) -> begin
      match ep_of_string k with
      | DESCRIPTION -> {title; location; starts; ends; notes=Some(v.to_string())}
      | DTEND -> if is_date_time p then {title; location; starts; ends=Some(v.to_string()); notes}
                 else {title; location; starts; ends; notes}
      | DTSTART -> if is_date_time p then {title; location; starts=Some(v.to_string()); ends; notes}
                 else {title; location; starts; ends; notes}
      | SUMMARY -> {title=Some(v.to_string()); location; starts; ends; notes}
      | UNSUPPORTED_EP -> {title; location; starts; ends; notes}
    end

let rec collect acc =
  function
  | [] -> acc
  | Ical.Block(_, n, icss)::is -> begin
      match cmp_of_string n with
      | VEVENT -> collect ((List.fold_left col_entry init icss)::acc) is
      | _ -> collect acc (icss@is)
    end
  | Ical.Assoc(l, k, _, _)::is -> begin
      match ep_of_string k with
      | UNSUPPORTED_EP -> collect acc is
      | _ -> begin
          Printf.printf "Something wrong with %s in (%i | %i) \n" k (fst l) (snd l);
          collect acc is
        end
    end

let rec clean = function
  | Ical.Block(l, n, icss) -> Ical.Block(l, n, (List.map clean icss))
  | Ical.Assoc(l, k, p, v) as a ->
     match ep_of_string k with
     | DESCRIPTION -> let cl = Str.replace_first rgxp "" (Str.global_replace crlf " " (v.to_string())) in
                      let newv = Ical.({location=v.location; to_string=(fun () -> cl); to_pretty=(fun () -> cl); value=`Raw cl}) in
                      Ical.Assoc(l, k, p, newv)
     | _ -> a

let _ =
  let f_in = Sys.argv.(1) in
  let f = file_contents f_in in
  let i = f |> Lexing.lex_ical |> parse_ical in
  let is_clean = try Sys.argv.(2) with Invalid_argument _ ->
                   begin
                     List.iter show_entry (collect [] i);
                     ""
                   end
  in if String.equal is_clean "-clean" then Printf.printf "%s" (to_string (List.map clean i))
     else failwith "Please provide ~arg1: <.ics file path>, ~arg2: -clean \n"
