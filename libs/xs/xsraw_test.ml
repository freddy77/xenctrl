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
