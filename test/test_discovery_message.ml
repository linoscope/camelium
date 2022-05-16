open Camelium
open Core
open Stdio

(*
		wantPacket: &Ping{
			Version:    4,
			From:       Endpoint{net.ParseIP("127.0.0.1").To4(), 3322, 5544},
			To:         Endpoint{net.ParseIP("::1"), 2222, 3333},
			Expiration: 1136239445,
		},
 *)
let%expect_test "test" =
  Hex.to_string (`Hex "71dbda3a79554728d4f94411e42ee1f8b0d561c10e1e5f5893367948c6a7d70bb87b235fa28a77070271b6c164a2dce8c7e13a5739b53b5e96f2e5acb0e458a02902f5965d55ecbeb2ebb6cabb8b2b232896a36b737666c55265ad0a68412f250001ea04cb847f000001820cfa8215a8d790000000000000000000000000000000018208ae820d058443b9a355")
  |> Discovery_message.decode
  |> [%sexp_of : (Discovery_message.packet_data, string) Result.t]
  |> print_s;
  [%expect {|
    (Ok
     (Ping (version 4) (from ((ip 127.0.0.1) (udp_port 12) (tcp_port 21)))
      (to_ ((ip 127.0.0.1) (udp_port 8) (tcp_port 13))) (expiration 67)
      (enr_seq ()))) |}]
