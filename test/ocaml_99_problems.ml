open Alcotest

let test_tail_of_a_list () =
  let test_func = Ocaml_99_problems.last in
  check (option string) "Some d" (Some "d") (test_func [ "a"; "b"; "c"; "d" ]);
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
  check (option string) "Some c" (Some "c") (test_func [ "a"; "b"; "c"; "d"; "e" ] 2);
  check (option string) "none" None (test_func [ "a" ] 2)
;;

let test_length_of_a_list () =
  let test_func = Ocaml_99_problems.length_of_a_list in
  check int "3" 3 (test_func [ "a"; "b"; "c" ]);
  check int "0" 0 (test_func [])
;;

let test_reverse_a_list () =
  let test_func = Ocaml_99_problems.reverse_a_list in
  check (list string) "reversed list" [ "c"; "b"; "a" ] (test_func [ "a"; "b"; "c" ])
;;

let test_is_palindrome () =
  let test_func = Ocaml_99_problems.is_palindrome in
  check bool "palindrome" true (test_func [ "x"; "a"; "m"; "a"; "x" ]);
  check bool "not palindrome" false (test_func [ "a"; "b" ])
;;

let test_flatten () =
  let test_func = Ocaml_99_problems.flatten in
  check
    (list string)
    "flatten"
    [ "a"; "b"; "c"; "d"; "e" ]
    (test_func [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])
;;

let test_eliminate_duplicates () =
  let test_func = Ocaml_99_problems.compress in
  check
    (list string)
    "compress"
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
    (test_func [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
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
    ; "4.", [ Alcotest.test_case "Length of a List (Beginner)" `Quick test_length_of_a_list ]
    ; "5.", [ Alcotest.test_case "Reverse a list (Beginner)" `Quick test_reverse_a_list ]
    ; "6.", [ Alcotest.test_case "Palindrome (Beginner)" `Quick test_is_palindrome ]
    ; "7.", [ Alcotest.test_case "Flatten a List (Intermediate)" `Quick test_flatten ]
    ; ( "8."
      , [ Alcotest.test_case "Eliminate Duplicates (Intermediate)" `Quick test_eliminate_duplicates
        ] )
    ]
;;
