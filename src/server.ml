open Unix
 
(** [send_data sock data] sends [data] to client socket [sock]. *)
let send_data sock data =
  let byte = Bytes.of_string data in
  let length = Bytes.length byte in
  send sock byte 0 length []

(** [receive_data s] is the data received from client socket [sock]. Can have a
    maximum length of [100]. *)
let receive_data sock =
  let container = Bytes.create 100 in
  let received_length = recv sock container 0 100 [] in
  (Bytes.sub container 0 received_length) |> Bytes.to_string

(** Starts the server. *)
let init addr port =
  (* create server *)
  let host_info = gethostbyname addr in
  let inet_addr = (host_info).h_addr_list.(0) in
  let sockaddr = ADDR_INET (inet_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in

  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 1;
  let clientsock, clientaddr = accept sock in
  print_endline "connected with client";

  while(true) do
    ignore(send_data clientsock (read_line()));
    print_endline (receive_data clientsock);
  done;

  (* convert the socket into high-level channels *)
  let outchan = out_channel_of_descr sock in
  let inchan = in_channel_of_descr sock in
  (inchan, outchan)

let () =
  ignore(init "127.0.0.1" 8888)
