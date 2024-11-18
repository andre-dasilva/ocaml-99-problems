open Alcotest

let tail_of_a_list () =
  let test_func = Ocaml_99_problems.last in

  check (option string) "last is some d" (Some "d") (test_func ["a" ; "b" ; "c" ; "d"]);
  check (option string) "none" (Some("a")) (test_func [])


let () =
  Alcotest.run "OCaml 99 Problems" [
    "1.", [Alcotest.test_case "Tail of a List" `Quick tail_of_a_list]
  ]
