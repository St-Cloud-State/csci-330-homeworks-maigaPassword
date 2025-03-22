(defun terminal (term cur)
  (if (and cur (eql term (car cur)))
      (cdr cur)
      nil))

(defun G (cur)
  (let ((result (terminal 'x cur)))
    (or result
        (terminal 'y cur)
        (terminal 'z cur)
        (terminal 'w cur))))

(defun E-prime (cur)
  (let ((result (terminal 'o cur)))
    (if result
        (let ((g-result (G result)))
          (if g-result
              (E-prime g-result)
              nil))
        cur))) ;; ε transition

(defun E (cur)
  (let ((g-result (G cur)))
    (if g-result
        (E-prime g-result)
        nil)))

(defun L (cur)
  (labels ((l (cur)
             (let ((result (terminal 's cur)))
               (if result
                   (l-prime result)
                   nil)))
           (l-prime (cur)
             (or (l cur) cur))) ;; ε
    (l cur)))

(defun S-fun (cur)
  (let ((s-result (terminal 's cur)))
    (if s-result
        s-result
        (let ((d-result (terminal 'd cur)))
          (if d-result
              (let ((l-result (L d-result)))
                (if l-result
                    (terminal 'b l-result)
                    nil))
              nil)))))

(defun I-prime (cur)
  (let ((e-result (terminal 'e cur)))
    (if e-result
        (S-fun e-result)
        cur))) ;; ε transition

(defun I (cur)
  (let ((i-result (terminal 'i cur)))
    (if i-result
        (let ((e-result (E i-result)))
          (if e-result
              (let ((s-result (S-fun e-result)))
                (if s-result
                    (let ((i-prime-result (I-prime s-result)))
                      i-prime-result)
                    nil))
              nil))
        nil)))

(defun parse (cur)
  (let ((result (I cur)))
    (and result (null result))))

(defun run-tests ()
  (let ((tests
         '((:input (i x s e s)
            :expected T
            :description "Test 1: (i x s e s)")

           (:input (i x o y o z s e s)
            :expected T
            :description "Test 2: (i x o y o z s e s)")

           (:input (i z d s s b e s)
            :expected T
            :description "Test 3: (i z d s s b e s)")

           (:input (i x s e)
            :expected NIL
            :description "Test 4: (i x s e)")

           (:input (x o y s e s)
            :expected NIL
            :description "Test 5: (x o y s e s)")

           (:input (i s x o y e s)
            :expected NIL
            :description "Test 6: (i s x o y e s)"))))
    
    (format t "~%Parser Tests:~%~%")
    (loop for test in tests
          for input = (getf test :input)
          for expected = (getf test :expected)
          for description = (getf test :description)
          for result = (parse input)
          do (format t "~a~%Input: ~a~%Expected: ~a, Got: ~a~%~%"
                     description
                     input
                     expected
                     result))))
