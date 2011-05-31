
open Unix
open Str

type irc_message = 
    | Ping of string
    | Privmsg of string * string * string
    | Join of string * string
    | Part of string * string * string
    | Quit of string * string
    | Unhandled of string

type irc_response =
    | Msg of string * string
    | Noreply

let send_line oc line =
    output_string oc (line ^ "\r\n");
    flush oc;;

let trim_carriage_return s =
    if String.rindex s '\r' = (String.length s) - 1 then
        String.sub s 0 ((String.length s) - 1)
    else
        s

let starts_with s c =
    String.get s 0 = c

let strip_first_char s =
    String.sub s 1 ((String.length s) -1)

(* TODO: make tail recursive... *)
let rec parse_params ss =
    match ss with
    | [] -> []
    | h::tl ->
        if starts_with h ':' then 
            [List.fold_left (fun a v -> a ^ " " ^ v) (strip_first_char h) tl]
        else
            h :: parse_params tl

let parse_parts ss =
    let h,tl = (List.hd ss), (List.tl ss) in
        if starts_with h ':' then
            (strip_first_char h), (List.hd tl), (parse_params (List.tl tl))
        else
            "", h, parse_params tl

let parse_message raw_msg =
    let msg = trim_carriage_return raw_msg in
        let parts = Str.split (Str.regexp_string " ") msg in
            let msg' = parse_parts parts in
                match msg' with
                | (_,"PING",[m]) -> Ping m
                | (s,"PRIVMSG",[t;m]) -> Privmsg (s,t,m)
                | (s,"JOIN",[c]) -> Join (s,c)
                | (s,"PART",[c;m]) -> Part (s,c,m)
                | (s,"PART",[c]) -> Part (s,c,"")
                | (s,"QUIT",[m]) -> Quit (s,m)
                | (s,"QUIT",[]) -> Quit (s,"")
                | _ -> Unhandled msg;;


let connect_bot host port nick room botfun =
    let server_addr =
        try inet_addr_of_string host
        with Failure("inet_addr_of_string") ->
            (gethostbyname host).Unix.h_addr_list.(0)
    in
        let inchan,outchan = open_connection (ADDR_INET (server_addr, port)) in
            let send = send_line outchan in
                send ("USER " ^ nick ^ " 0 * :Bum Bot");
                send ("NICK " ^ nick);
                send ("JOIN #" ^ room);
                let rec loop u = 
                    let msg = parse_message (input_line inchan) in
                    (
                        match msg with 
                        | Ping(m) -> send ("PONG :" ^ m); print_endline "PONG";
                        | _ as m -> 
                            match (botfun m) with
                            | Msg(target,tosend) -> send ("PRIVMSG " ^ target ^ " :" ^ tosend);
                            | Noreply -> () 
                    );
                    loop ();
                in loop ();;

