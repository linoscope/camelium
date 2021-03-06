open Camelium
open Core
open Stdio
open Unix

let decode_and_print s =
  Hex.to_string (`Hex s)
  |> Discovery_message.decode
  |> [%sexp_of : (Discovery_message.t, string) Result.t]
  |> print_s


let%expect_test "decode ping without enr_seq" =
  "71dbda3a79554728d4f94411e42ee1f8b0d561c10e1e5f5893367948c6a7d70bb87b235fa28a77070271b6c164a2dce8c7e13a5739b53b5e96f2e5acb0e458a02902f5965d55ecbeb2ebb6cabb8b2b232896a36b737666c55265ad0a68412f250001ea04cb847f000001820cfa8215a8d790000000000000000000000000000000018208ae820d058443b9a355"
  |> decode_and_print;

  [%expect {|
    (Ok
     ((packet_header
       ((hash
         "q\219\218:yUG(\212\249D\017\228.\225\248\176\213a\193\014\030_X\1476yH\198\167\215\011")
        (signature
          "\184{#_\162\138w\007\002q\182\193d\162\220\232\199\225:W9\181;^\150\242\229\172\176\228X\160)\002\245\150]U\236\190\178\235\182\202\187\139+#(\150\163ksvf\197Re\173\
         \nhA/%\000")
        (packet_type Ping_type)))
      (packet_data
       (Ping (version 4) (from ((ip 127.0.0.1) (udp_port 3322) (tcp_port 5544)))
        (to_ ((ip ::1) (udp_port 2222) (tcp_port 3333))) (expiration 1136239445)
        (enr_seq ()))))) |}]

let%expect_test "decode ping with enr_seq" =
  "c7c44041b9f7c7e41934417ebac9a8e1a4c6298f74553f2fcfdcae6ed6fe53163eb3d2b52e39fe91831b8a927bf4fc222c3902202027e5e9eb812195f95d20061ef5cd31d502e47ecb61183f74a504fe04c51e73df81f25c4d506b26db4517490103f84eb840ca634cae0d49acb401d8a4c6b6fe8c55b70d115bf400769cc1400f3258cd31387574077f301b421bc84df7266c44e9e6d569fc56be00812904767bf5ccd1fc7f8443b9a35582999983999999280dc62cc8255c73471e0a61da0c89acdc0e035e260add7fc0c04ad9ebf3919644c91cb247affc82b69bd2ca235c71eab8e49737c937a2c396"
  |> decode_and_print;

  [%expect {|
    (Ok
     ((packet_header
       ((hash
         "\199\196@A\185\247\199\228\0254A~\186\201\168\225\164\198)\143tU?/\207\220\174n\214\254S\022")
        (signature
         ">\179\210\181.9\254\145\131\027\138\146{\244\252\",9\002  '\229\233\235\129!\149\249] \006\030\245\2051\213\002\228~\203a\024?t\165\004\254\004\197\030s\223\129\242\\MPk&\219E\023I\001")
        (packet_type Find_node_type)))
      (packet_data
       (Find_node
        (target
         "\202cL\174\rI\172\180\001\216\164\198\182\254\140U\183\r\017[\244\000v\156\193@\0152X\20518ut\007\1270\027B\027\200M\247&lD\233\230\213i\252V\190\000\129)\004v{\245\204\209\252\127")
        (expiration 1136239445))))) |}]

let%expect_test "decode find node" =
  "c7c44041b9f7c7e41934417ebac9a8e1a4c6298f74553f2fcfdcae6ed6fe53163eb3d2b52e39fe91831b8a927bf4fc222c3902202027e5e9eb812195f95d20061ef5cd31d502e47ecb61183f74a504fe04c51e73df81f25c4d506b26db4517490103f84eb840ca634cae0d49acb401d8a4c6b6fe8c55b70d115bf400769cc1400f3258cd31387574077f301b421bc84df7266c44e9e6d569fc56be00812904767bf5ccd1fc7f8443b9a35582999983999999280dc62cc8255c73471e0a61da0c89acdc0e035e260add7fc0c04ad9ebf3919644c91cb247affc82b69bd2ca235c71eab8e49737c937a2c396"
  |> decode_and_print;
  [%expect {|
    (Ok
     ((packet_header
       ((hash
         "\199\196@A\185\247\199\228\0254A~\186\201\168\225\164\198)\143tU?/\207\220\174n\214\254S\022")
        (signature
         ">\179\210\181.9\254\145\131\027\138\146{\244\252\",9\002  '\229\233\235\129!\149\249] \006\030\245\2051\213\002\228~\203a\024?t\165\004\254\004\197\030s\223\129\242\\MPk&\219E\023I\001")
        (packet_type Find_node_type)))
      (packet_data
       (Find_node
        (target
         "\202cL\174\rI\172\180\001\216\164\198\182\254\140U\183\r\017[\244\000v\156\193@\0152X\20518ut\007\1270\027B\027\200M\247&lD\233\230\213i\252V\190\000\129)\004v{\245\204\209\252\127")
        (expiration 1136239445))))) |}]

let%expect_test "decode negibors" =
  "c679fc8fe0b8b12f06577f2e802d34f6fa257e6137a995f6f4cbfc9ee50ed3710faf6e66f932c4c8d81d64343f429651328758b47d3dbc02c4042f0fff6946a50f4a49037a72bb550f3a7872363a83e1b9ee6469856c24eb4ef80b7535bcf99c0004f9015bf90150f84d846321163782115c82115db8403155e1427f85f10a5c9a7755877748041af1bcd8d474ec065eb33df57a97babf54bfd2103575fa829115d224c523596b401065a97f74010610fce76382c0bf32f84984010203040101b840312c55512422cf9b8a4097e9a6ad79402e87a15ae909a4bfefa22398f03d20951933beea1e4dfa6f968212385e829f04c2d314fc2d4e255e0d3bc08792b069dbf8599020010db83c4d001500000000abcdef12820d05820d05b84038643200b172dcfef857492156971f0e6aa2c538d8b74010f8e140811d53b98c765dd2d96126051913f44582e8c199ad7c6d6819e9a56483f637feaac9448aacf8599020010db885a308d313198a2e037073488203e78203e8b8408dcab8618c3253b558d459da53bd8fa68935a719aff8b811197101a4b2b47dd2d47295286fc00cc081bb542d760717d1bdd6bec2c37cd72eca367d6dd3b9df738443b9a355010203b525a138aa34383fec3d2719a0"
  |> decode_and_print;
  [%expect {|
    (Ok
     ((packet_header
       ((hash
         "\198y\252\143\224\184\177/\006W\127.\128-4\246\250%~a7\169\149\246\244\203\252\158\229\014\211q")
        (signature
         "\015\175nf\2492\196\200\216\029d4?B\150Q2\135X\180}=\188\002\196\004/\015\255iF\165\015JI\003zr\187U\015:xr6:\131\225\185\238di\133l$\235N\248\011u5\188\249\156\000")
        (packet_type Neighbors_type)))
      (packet_data
       (Neighbors (expiration 1136239445)
        (nodes
         (((node_id
             "1U\225B\127\133\241\
            \n\\\154wU\135wH\004\026\241\188\216\212t\236\006^\179=\245z\151\186\191T\191\210\0165u\250\130\145\021\210$\197#Yk@\016e\169\127t\001\006\016\252\231c\130\192\1912")
           (endpoint ((ip 99.33.22.55) (udp_port 4444) (tcp_port 4445))))
          ((node_id
            "1,UQ$\"\207\155\138@\151\233\166\173y@.\135\161Z\233\t\164\191\239\162#\152\240= \149\0253\190\234\030M\250o\150\130\0188^\130\159\004\194\211\020\252-N%^\r;\192\135\146\176i\219")
           (endpoint ((ip 1.2.3.4) (udp_port 1) (tcp_port 1))))
          ((node_id
            "8d2\000\177r\220\254\248WI!V\151\031\014j\162\1978\216\183@\016\248\225@\129\029S\185\140v]\210\217a&\005\025\019\244E\130\232\193\153\173|mh\025\233\165d\131\2467\254\170\201D\138\172")
           (endpoint
            ((ip 2001:db8:3c4d:15::abcd:ef12) (udp_port 3333) (tcp_port 3333))))
          ((node_id
            "\141\202\184a\1402S\181X\212Y\218S\189\143\166\1375\167\025\175\248\184\017\025q\001\164\178\180}\210\212r\149(o\192\012\192\129\187T-v\007\023\209\189\214\190\194\195|\215.\2026}m\211\185\223s")
           (endpoint
            ((ip 2001:db8:85a3:8d3:1319:8a2e:370:7348) (udp_port 999)
             (tcp_port 1000)))))))))) |}]

let%expect_test "encode ping" =
  Discovery_message.Ping {
    version = 4;
    from = {
      ip = Inet_addr.of_string "127.0.0.1";
      udp_port = 3322;
      tcp_port = 5544;
    };
    to_ = {
      ip = Inet_addr.of_string "::1";
      udp_port = 2222;
      tcp_port = 3333;
    };
    expiration = 1136239445;
    enr_seq = None;
  }
  |> Discovery_message.encode
  |> Hex.of_string
  |> Hex.show
  |> print_endline;

  [%expect {| ea04cb847f000001820cfa8215a8d790000000000000000000000000000000018208ae820d058443b9a355 |}]
