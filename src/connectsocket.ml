open Unix

(** [send_data sock data] sends [data] to server socket [sock]. *)
let send_data sock data =
  let byte = Bytes.of_string data in
  let length = Bytes.length byte in
  send sock byte 0 length []

(** [receive_data s] is the data received from server socket [sock]. Can have a
    maximum length of [100]. *)
let receive_data sock =
  let container = Bytes.create 50 in
  let received_length = recv sock container 0 50 [] in
  (Bytes.sub container 0 received_length) |> Bytes.to_string  
  
let connect_to_server addr port =
  let host_info = gethostbyname addr in
  let inet_addr = (host_info).h_addr_list.(0) in
  let sockaddr = ADDR_INET (inet_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock sockaddr;
  print_endline "connected with server";

  while(true) do
    print_endline(receive_data sock);
    ignore(send_data sock (read_line()));
  done

let () =
  connect_to_server "127.0.0.1" 8888
