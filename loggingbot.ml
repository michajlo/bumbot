
open Bumbot;;
open Unix;;
open Printf;;

exception AlreadyFileError of string
exception InvalidFileError

(* make a directory only if it doesnt exist, if file of the same name
   already exists then raise AlreadyFileError *)
let mkdir_if_not_exists d =
    try
        if Sys.is_directory d
        then ()
        else raise (AlreadyFileError ("Cannot create dir " ^ d ^ ", already exists as regular file"))
    with Sys_error (_) ->
        mkdir d 0o755

(* return an output for the file "base + l.(0) + "/" +  l.(1) + "/" + ... + l(last)" *)
let rec output_of_list base l = 
    match l with
    | [] -> raise InvalidFileError
    | [e] -> open_out_gen [Open_append] 0o655 (base ^ e)
    | h::tl -> let dirname = base ^ h ^ "/" in
        mkdir_if_not_exists dirname;
        output_of_list dirname tl;;

(* log str in base/yyyy/mm/dd.txt, probably not best practice, but im working on it *)
let log_message base str =
    let now = localtime (time ()) in
        let logdesc = [sprintf "%04d" (1900 + now.tm_year);
                       sprintf "%02d" (now.tm_mon + 1);
                       sprintf "%02d.txt" now.tm_mday] in 
            let handle = output_of_list base logdesc in
                output_string handle (str ^ "\n");
                close_out handle;;

let handle_privmsg f t m =
    print_endline (f ^ " -> " ^ t ^ ": " ^ m);
    log_message "" (f ^ ": " ^ m); 
    Msg (t,m)

let handle_unhandled m =
    print_endline m;
    Noreply;;

let handle_command cmd =
    match cmd with
    | Privmsg(f,t,m) -> handle_privmsg f t m;
    | Unhandled(m) -> handle_unhandled m;
    | _ -> Noreply;;

try 
    connect_bot Sys.argv.(1) (int_of_string Sys.argv.(2)) Sys.argv.(3) Sys.argv.(4) handle_command
with Invalid_argument("index out of bounds") ->
    printf "usage: %s <host> <port> <nick> <room>\n" Sys.argv.(0)