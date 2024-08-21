(let ((a 3)(b 4)))
(let ((flag false)))
(print "Enter a")
(setq a (input))
(setq b (if flag 2 -2))

(function process-maths (a b k)
  (if (< a 0)
      (print (- (+ (* a k) (* -2 b)) (/ b a)))
      (print "a is negative")
    )
)

(function run-maths ()
  (print "Enter number of iterations")
  (let ((n (parse-int (input)))))
   (loop for i from 1 to n do
      (process-maths a b i)
    )
)
(run-maths)  ; Вызов функции run-maths