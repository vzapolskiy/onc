(*
Copyright (c) 2009, Vladimir Zapolskiy <vzapolskiy@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* The names of contributors may not be used to endorse or promote
  products derived from this software without specific prior written
  permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Unix
open Getopt

let onc_poster =
  "usage:\n  " ^ Sys.argv.(0) ^
    " [-u] [-i file] [-o file] -h hostname -p port\n" ^"  " ^ Sys.argv.(0) ^
    " -l [-u] [-i file] [-o file] [-h hostname] -p port\n" ^ "where\n" ^
    "  -l, --listen     enable server mode\n" ^
    "  -u, --udp        set udp communication (default is tcp)\n" ^
    "  -i, --input      input filename (default is stdin)\n" ^
    "  -o, --output     output filename (default is stdout)\n" ^
    "  -h, --hostname   set peer hostname\n" ^
    "  -p, --port       set peer port\n\n" ^
    "Please report bugs to Vladimir Zapolskiy <vzapolskiy@gmail.com>"

let onc_exit s exit_hook =
  begin
    print_endline (Sys.argv.(0) ^ ": " ^ s);
    Lazy.force exit_hook;
    flush Pervasives.stdout;
    exit 1
  end

let exit_with_poster s = onc_exit s (lazy (print_endline onc_poster))
let exit_without_poster s = onc_exit s (lazy ())
let unix_call f =
  try
    Lazy.force f
  with Unix.Unix_error (a, _, _) -> exit_without_poster (Unix.error_message a)

class onc () =
object (self)
  val server_mode    = ref false
  val udp_connection = ref false
  val input_file:string option ref  = ref None
  val output_file:string option ref = ref None
  val hostname:string option ref    = ref None
  val port_number:int option ref    = ref None

  (* all foolowing file descriptors will be set *)
  val mutable input_descr:file_descr = Unix.stdin
  val mutable output_descr:file_descr = Unix.stdout
  val mutable comm_descr:file_descr = Unix.stderr

  method private parse_cmdline =
    let onc_specs =
      let atmost_once f var exc =
        Some (fun x -> if !var = None then var := Some (f x) else raise exc) in
        [
          ('l', "listen",   (set server_mode true),    None);
          ('u', "udp",      (set udp_connection true), None);
          ('i', "input",    None,
           (atmost_once (fun x -> x) input_file
              (Getopt.Error "Specify only one input file")));
          ('o', "output",   None,
           (atmost_once (fun x -> x) output_file
              (Getopt.Error "Specify only one output file")));
          ('h', "hostname", None,
           (atmost_once (fun x -> x) hostname
              (Getopt.Error "Specify only one hostname")));
          ('p', "port",     None,
           (atmost_once (fun x -> int_of_string x) port_number
              (Getopt.Error "Specify only one port number")));
        ] in
      try
        Getopt.parse_cmdline onc_specs
          (fun x -> exit_with_poster ("Unknown option " ^ x))
      with Getopt.Error a -> exit_with_poster a
        | Failure ("int_of_string") -> exit_with_poster "Invalid port"

  method private get_port_number = match !port_number with
    | Some p when (p > 0 && p < 65536) -> p
    | Some p -> exit_without_poster "Port value is out of range"
    | None ->   exit_with_poster "Port number is not defined"

  method private get_peer_address = (fun he -> he.h_addr_list.(0))
    ( try Unix.gethostbyname
        ( match !hostname with
            | Some h -> h
            | None when !server_mode -> "localhost"
            | _ -> exit_with_poster "Hostname is not specified")
      with Not_found -> exit_without_poster "Hostname is not found")

  method private set_input_file () = match !input_file with
    | Some f -> input_descr <-
        unix_call (lazy (Unix.openfile f [Unix.O_RDONLY] 0o400))
    | _ -> ()

  method private set_output_file () = match !output_file with
    | Some f -> output_descr <-
        unix_call (lazy (Unix.openfile f
                           [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT] 0o644))
    | _ -> ()

  method private set_socket () =
    let buff_size = 8 * 1024 in
    let char_buff = String.create buff_size in
    let addr = self#get_peer_address in
    let pn = self#get_port_number in
    let s = (Unix.socket Unix.PF_INET
               (if !udp_connection then Unix.SOCK_DGRAM else Unix.SOCK_STREAM) 0) in
      match !server_mode, !udp_connection with
        | true, true ->
            begin
              unix_call (lazy (Unix.bind s (Unix.ADDR_INET (addr, pn))));
              ignore (Unix.select [s] [] [] (-1.0));
              (fun (p, sa) ->
                 ignore (Unix.write output_descr char_buff 0 p);
                 Unix.connect s sa) (Unix.recvfrom s char_buff 0 buff_size []);
              comm_descr <- s
            end
        | true, false ->
            begin
              unix_call (lazy (Unix.bind s (Unix.ADDR_INET (addr, pn))));
              unix_call (lazy (Unix.listen s 0));
              comm_descr <- fst (unix_call (lazy (Unix.accept s)));
              Unix.close s (* that's safely to do so *)
            end
        | false, _ ->
            begin
              unix_call (lazy (Unix.connect s (Unix.ADDR_INET (addr, pn))));
              comm_descr <- s
            end

  method get_descr () =
    input_descr, output_descr, comm_descr

  initializer
    begin
      self#parse_cmdline;
      self#set_input_file ();
      self#set_output_file ();
      self#set_socket ();
    end
end

(* need to write down an accurate state machine *)
let onc_communicate arg =
  let buff_size = 8 * 1024 in
  let char_buff = String.create buff_size in
  let i, o, s = arg#get_descr () in
  let id = ref true in (* !id means two input data descriptors are opened *)
  let ex () =
    Unix.close o; Unix.close s; if !id then Unix.close i else (); exit 0 in
  let rd s = try Unix.read s char_buff 0 buff_size with
    | Unix.Unix_error (Unix.ECONNRESET, _, _) -> (* tcp rst received *) ex ()
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> (* udp peer closed *) ex ()
    | Unix.Unix_error (a, _, _) ->
        print_endline ("Error: " ^ (Unix.error_message a)); ex () in
    (* for now write could be incomplete and no one cares of it *)
  let wr s c = try ignore (Unix.write s char_buff 0 c) with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> (* udp peer closed *) ex ()
    | Unix.Unix_error (a, _, _) ->
        print_endline ("Error: " ^ (Unix.error_message a)); ex () in
  let rec comm_sock () =
    ignore (Unix.select [s] [] [] (-1.0));
    match (rd s) with
      | c when c > 0 -> wr o c; comm_sock ()
      | _ -> ex () in
  let rec comm_in () =
    let r = (fun (a, _, _) -> a) (Unix.select [i; s] [] [] (-1.0)) in
      if (List.exists (fun d -> d == s) r) then
        match (rd s) with
          | c when c > 0 -> wr o c
          | _ -> ex ()
      else ();
      if (List.exists (fun d -> d == i) r) then
        match (rd i) with
          | c when c > 0 -> wr s c
          | _ -> Unix.close i; id := false; comm_sock () (* half closed *)
      else ();
      comm_in () in
    comm_in ()

let _ = onc_communicate (new onc ())
