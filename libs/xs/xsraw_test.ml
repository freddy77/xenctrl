(*
 * Copyright (C) 2024 Cloud Software Group UK Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Thread

let%test_module "valid_path tests" =
  (module struct
    let valid_path path = Xsraw.validate_path path ; true

    let invalid_path path =
      try Xsraw.validate_path path ; false with Xsraw.Invalid_path _ -> true

    let%test _ = valid_path "/"

    let%test _ = valid_path ""

    let%test _ = invalid_path "//"

    let%test _ = invalid_path "before//after"

    let%test _ = invalid_path "/a/"
  end
)

let%test_module "split_string tests" =
  (module struct
    let split ?limit c s expected =
      let output =
        match limit with
        | Some limit ->
            Xsraw.split_string ~limit c s
        | None ->
            Xsraw.split_string c s
      in
      List.equal ( = ) output expected

    let%test _ = split ~limit:1 ':' "" [""]

    let%test _ = split ~limit:1 ':' ":" [":"]

    let%test _ = split ~limit:2 ':' ":" [""; ""]

    let%test _ = split ~limit:0 ':' "a:b:c" ["a:b:c"]

    let%test _ = split ~limit:1 ':' "a:b:c" ["a:b:c"]

    let%test _ = split ~limit:2 ':' "a:b:c" ["a"; "b:c"]

    let%test _ = split ~limit:3 ':' "a:b:c" ["a"; "b"; "c"]

    let%test _ = split ~limit:3 ':' "a:b:c:" ["a"; "b"; "c:"]

    let%test _ = split ~limit:4 ':' "a:b:c:" ["a"; "b"; "c"; ""]

    let%test _ = split ':' "a:b:c" ["a"; "b"; "c"]

    let%test _ = split ':' ":a:" [""; "a"; ""]

    let%test _ = split ':' "" [""]
  end
)

let%test_module "xenstore operation tests" =
  (module struct
    let xs_test test_func =
      let fd0, fd1 =
        Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
      in
      let con = Xsraw.open_fd fd0 in
      let res = test_func con fd1 in
      Xsraw.close con ; Unix.close fd1 ; res

    let xb_packet op data =
      let pkt = Xenbus.Packet.create 123 0 op data in
      Xenbus.Packet.to_string pkt

    let reply_data (fd, data) =
      Unix.write_substring fd data 0 (String.length data) |> ignore ;
      Unix.shutdown fd Unix.SHUTDOWN_SEND

    let%test "directory with short reply" =
      xs_test @@ fun con fd ->
      let data = xb_packet Xenbus.Op.Directory "one\000two\000" in
      let thread = Thread.create reply_data (fd, data) in
      let files = Xsraw.directory 123 "/test" con in
      Thread.join thread ;
      List.equal ( = ) files ["one"; "two"]

    let%test "directory with long reply, one packet" =
      xs_test @@ fun con fd ->
      let data = xb_packet Xenbus.Op.Error "E2BIG\000" in
      let data = data ^ xb_packet Xenbus.Op.Watchevent "foo\000data" in
      let data =
        data ^ xb_packet Xenbus.Op.Directory_part "1\000one\000two\000\000"
      in
      let thread = Thread.create reply_data (fd, data) in
      let files = Xsraw.directory 123 "/test" con in
      Thread.join thread ;
      List.equal ( = ) files ["one"; "two"]

    let%test "directory with long reply, two packets" =
      xs_test @@ fun con fd ->
      let data = xb_packet Xenbus.Op.Error "E2BIG\000" in
      let data = data ^ xb_packet Xenbus.Op.Watchevent "foo\000data" in
      let data = data ^ xb_packet Xenbus.Op.Directory_part "1\000one\000" in
      let data = data ^ xb_packet Xenbus.Op.Directory_part "1\000two\000\000" in
      let thread = Thread.create reply_data (fd, data) in
      let files = Xsraw.directory 123 "/test" con in
      Thread.join thread ;
      List.equal ( = ) files ["one"; "two"]

    let%test "directory with long reply, wrong generation" =
      xs_test @@ fun con fd ->
      let data = xb_packet Xenbus.Op.Error "E2BIG\000" in
      let data = data ^ xb_packet Xenbus.Op.Watchevent "foo\000data" in
      let data = data ^ xb_packet Xenbus.Op.Directory_part "1\000wrong\000" in
      let data =
        data ^ xb_packet Xenbus.Op.Directory_part "2\000other\000\000"
      in
      let data =
        data ^ xb_packet Xenbus.Op.Directory_part "2\000this is correct\000\000"
      in
      let thread = Thread.create reply_data (fd, data) in
      let files = Xsraw.directory 123 "/test" con in
      Thread.join thread ;
      List.equal ( = ) files ["this is correct"]

    let%test "read" =
      xs_test @@ fun con fd ->
      let data = xb_packet Xenbus.Op.Read "value\000" in
      let thread = Thread.create reply_data (fd, data) in
      let value = Xsraw.read 123 "/test" con in
      Thread.join thread ; value = "value"
  end
)
