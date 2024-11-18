open Alcotest

let test_tail_of_a_list () =
  let test_func = Ocaml_99_problems.last in
  check (option string) "last is some d" (Some "d") (test_func [ "a"; "b"; "c"; "d" ]);
  check (option string) "none" None (test_func [])
;;

let test_last_two_elements_of_a_list () =
  let test_func = Ocaml_99_problems.last_two in
  check
    (option (pair string string))
    "last is some d"
    (Some ("c", "d"))
    (test_func [ "a"; "b"; "c"; "d" ]);
  check (option (pair string string)) "none" None (test_func [ "a" ]);
  check (option (pair string string)) "none" None (test_func [])
;;

let test_nth_element_of_list () =
  let test_func = Ocaml_99_problems.nth_element_of_list in
  check (option string) "none" (Some "c") (test_func [ "a"; "b"; "c"; "d"; "e" ] 2);
  check (option string) "none" None (test_func [ "a" ] 2)
;;

let () =
  Alcotest.run
    "OCaml 99 Problems"
    [ "1.", [ Alcotest.test_case "Tail of a List (Beginner)" `Quick test_tail_of_a_list ]
    ; ( "2."
      , [ Alcotest.test_case
            "Last Two Elements of a List (Beginner)"
            `Quick
            test_last_two_elements_of_a_list
        ] )
    ; ( "3."
      , [ Alcotest.test_case "N'th Element of a List (Beginner)" `Quick test_nth_element_of_list ] )
    ]
;;
