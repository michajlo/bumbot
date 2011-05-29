
open Unix
open Str

type irc_message = 
    | Ping of string
    | Privmsg of string * string * string
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

let parse_message raw_msg =
    let msg = trim_carriage_return raw_msg in
        if String.sub msg 0 6 = "PING :" then
            Ping (String.sub msg 6 ((String.length msg) - 6))
        else
            let parts = Str.split (Str.regexp_string " ") msg in
            match parts with
                | prefix::"PRIVMSG"::target::rest ->
                    let sender = String.sub prefix 1 ((String.length prefix) - 1) in
                    let message = String.sub (List.hd rest) 1 ((String.length (List.hd rest)) - 1) ^
                                List.fold_left (fun a v -> a ^ " " ^ v) "" (List.tl rest)
                    in Privmsg (sender, target, message)
                | _ -> Unhandled msg;;


let connect_bot host port nick room botfun =
    let server_addr =
        try inet_addr_of_string host
        with Failure("inet_addr_of_string") ->
            (gethostbyname host).Unix.h_addr_list.(0)
    in
        let inchan,outchan = 
            open_connection (ADDR_INET (server_addr, port))
        in
            send_line outchan ("USER " ^ nick ^ " 0 * :Bum Bot");
            send_line outchan ("NICK " ^ nick);
            send_line outchan ("JOIN #" ^ room);
            let rec loop u = 
                let msg = parse_message (input_line inchan) in
                match msg with 
                    | Ping(m) -> send_line outchan ("PONG :" ^ m); print_endline "PONG"; loop ();
                    | _ as m ->
                        match (botfun m) with
                            | Msg(target,tosend) -> send_line outchan ("PRIVMSG " ^ target ^ " :" ^ tosend); loop ();
                            | Noreply -> loop ();
            in loop ();;

