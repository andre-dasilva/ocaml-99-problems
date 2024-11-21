open Alcotest
open Ocaml_99_problems

let test_tail_of_a_list () =
  let test_func = last in
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

let test_pack_consecutive_duplicates () =
  let test_func = Ocaml_99_problems.pack in
  check
    (list (list string))
    "pack"
    [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
    (test_func [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ])
;;

let test_run_length_encoding () =
  let test_func = Ocaml_99_problems.run_length_encoding in
  check
    (list (pair int string))
    "run length encoding"
    [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
    (test_func [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

let test_modified_run_length_encoding () =
  let ppf_rle ppf (x : string rle) =
    match x with
    | Single a -> Fmt.pf ppf "Single(%s)" a
    | Multiple (n, a) -> Fmt.pf ppf "Multiple(%d, %s)" n a
  in
  let rle_testable = testable ppf_rle ( = ) in
  let test_func = Ocaml_99_problems.modified_run_length_encoding in
  let expected =
    [ Multiple (4, "a")
    ; Single "b"
    ; Multiple (2, "c")
    ; Multiple (2, "a")
    ; Single "d"
    ; Multiple (4, "e")
    ]
  in
  check
    (list rle_testable)
    "run length encoding"
    expected
    (test_func [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;

let test_decode_run_length_encoded_list () =
  let test_func = Ocaml_99_problems.decode_run_length_encoded_list in
  check
    (list string)
    "run length encoding"
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    (test_func
       [ Multiple (4, "a")
       ; Single "b"
       ; Multiple (2, "c")
       ; Multiple (2, "a")
       ; Single "d"
       ; Multiple (4, "e")
       ])
;;

let test_duplicate_the_elements_of_a_list () =
  let test_func = Ocaml_99_problems.duplicates in
  check
    (list string)
    "duplicate elements of list"
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
    (test_func [ "a"; "b"; "c"; "c"; "d" ])
;;

let test_replicate_the_elements_of_a_list_a_given_number_of_times () =
  let test_func = Ocaml_99_problems.replicate in
  check
    (list string)
    "replicate elements"
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
    (test_func [ "a"; "b"; "c" ] 3)
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
    ; ( "9."
      , [ Alcotest.test_case
            "Pack Consecutive Duplicates (Intermediate)"
            `Quick
            test_pack_consecutive_duplicates
        ] )
    ; "10.", [ Alcotest.test_case "Run length encoding (Beginner)" `Quick test_run_length_encoding ]
    ; ( "11."
      , [ Alcotest.test_case
            "Modified run length encoding (Beginner)"
            `Quick
            test_modified_run_length_encoding
        ] )
    ; ( "12."
      , [ Alcotest.test_case
            "Modified run length encoding (Intermediate)"
            `Quick
            test_decode_run_length_encoded_list
        ] )
    ; ( "13."
      , [ Alcotest.test_case
            "Duplicate the Elements of a List (Beginner)"
            `Quick
            test_duplicate_the_elements_of_a_list
        ] )
    ; ( "14."
      , [ Alcotest.test_case
            "Replicate the Elements of a List a Given Number of Times (Intermediate)"
            `Quick
            test_replicate_the_elements_of_a_list_a_given_number_of_times
        ] )
    ]
;;
