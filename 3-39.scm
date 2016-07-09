;;; 101: P 1 sets x to 100 and then P 2 increments x to 101.
;;; 121: P 2 increments x to 11 and then P 1 sets x to x * x .
;;; 11: P 2 accesses x , then P 1 sets x to 100, then P 2 sets x .
;;; 100: P 1 accesses x (twice), then P 2 sets x to 11, then P 1 sets x .
