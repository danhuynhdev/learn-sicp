;;; <response> -> (b)
;;; x -> ['a | -]-> ['b |/]
;;; y -> ['c | -]-> ['d |/]
;;; z -> ['a | -]-> ['b | -]-> ['c | -]-> ['d |/]

;;; <response> -> (b c d)
;;; x - - - - - - -> ['a | -]-> ['b | ' ]
;;;                                   /
;;;                                  /
;;;                                 /
;;; y - - - - - - - - - - - - - - -+ - -> ['c | -]-> ['d |/]
