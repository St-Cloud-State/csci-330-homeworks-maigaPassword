(defun my-merge (left right)
  ;; Merges two sorted lists
  (let ((result '()))
    (loop while (and left right)
          do (if (<= (car left) (car right))
                  (progn
                    (push (car left) result) ;; Add left element
                    (setf left (cdr left)))  ;; Move left forward
                  (progn
                    (push (car right) result) ;; Add right element
                    (setf right (cdr right))))) ;; Move right forward
    (append (nreverse result) left right))) ;; Append remaining elements

(defun bottom-up-merge-sort (list)
  ;; Bottom-up merge sort
  (let ((pairs '())
        (remaining list))
    ;; Pairwise merging
    (loop while remaining
          do (progn
               (if (cdr remaining)
                   (push (my-merge (list (car remaining)) (list (cadr remaining))) pairs)
                   (push (list (car remaining)) pairs))
               (setf remaining (cddr remaining))))
    ;; Merge until one sorted list remains
    (loop while (> (length pairs) 1)
          do (let ((merged '()))
               (loop for i from 0 to (- (length pairs) 2) by 2
                     do (push (my-merge (nth i pairs) (nth (1+ i) pairs)) merged))
               (if (oddp (length pairs))
                   (push (car (last pairs)) merged))
               (setf pairs (nreverse merged))))
    (car pairs))) ;; Return sorted list

(princ (bottom-up-merge-sort '(1 7 2 1 8 6 5 3 7 9 4))) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (progn

                    (push (car left) result) ;; Add left element

                    (setf left (cdr left)))  ;; Move left forward

                  (progn

                    (push (car right) result) ;; Add right element

                    (setf right (cdr right))))) ;; Move right forward

    (append (nreverse result) left right))) ;; Append remaining elements



(defun bottom-up-merge-sort (list)

  ;; Bottom-up merge sort

  (let ((pairs '())

        (remaining list))

    ;; Pairwise merging

    (loop while remaining

          do (progn

               (if (cdr remaining)

                   (push (my-merge (list (car remaining)) (list (cadr remaining))) pairs)

                   (push (list (car remaining)) pairs))

               (setf remaining (cddr remaining))))

    ;; Merge until one sorted list remains

    (loop while (> (length pairs) 1)

          do (let ((merged '()))

               (loop for i from 0 to (- (length pairs) 2) by 2

                     do (push (my-merge (nth i pairs) (nth (1+ i) pairs)) merged))

               (if (oddp (length pairs))

                   (push (car (last pairs)) merged))

               (setf pairs (nreverse merged))))

    (car pairs))) ;; Return sorted list



(princ (bottom-up-merge-sort '(1 7 2 1 8 6 5 3 7 9 4))) 

[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ exit
exit
Script done, file is Q3.txt
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ clear
[H[2J[3J[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ script Q.[K3.lisp [K[K[K[K[Ktxt
Script started, file is Q3.txt
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ exit[K[K[K[Kcat Q3.lisp 
(defun my-merge (left right)
  ;; Merges two sorted lists
  (let ((result '()))
    (loop while (and left right)
          do (if (<= (car left) (car right))
                  (progn
                    (push (car left) result) ;; Add left element
                    (setf left (cdr left)))  ;; Move left forward
                  (progn
                    (push (car right) result) ;; Add right element
                    (setf right (cdr right))))) ;; Move right forward
    (append (nreverse result) left right))) ;; Append remaining elements

(defun bottom-up-merge-sort (list)
  ;; Bottom-up merge sort
  (let ((pairs '())
        (remaining list))
    ;; Pairwise merging
    (loop while remaining
          do (progn
               (if (cdr remaining)
                   (push (my-merge (list (car remaining)) (list (cadr remaining))) pairs)
                   (push (list (car remaining)) pairs))
               (setf remaining (cddr remaining))))
    ;; Merge until one sorted list remains
    (loop while (> (length pairs) 1)
          do (let ((merged '()))
               (loop for i from 0 to (- (length pairs) 2) by 2
                     do (push (my-merge (nth i pairs) (nth (1+ i) pairs)) merged))
               (if (oddp (length pairs))
                   (push (car (last pairs)) merged))
               (setf pairs (nreverse merged))))
    (car pairs))) ;; Return sorted list

(princ (bottom-up-merge-sort '(1 7 2 1 8 6 5 3 7 9 4))) 
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ sbcl --load l[KQ3.lisp 
This is SBCL 1.4.3, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
(1 1 2 3 4 5 6 7 7 8 9)
* ^Z
[1]+  Stopped                 sbcl --load Q3.lisp
[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ exit
exit
There are stopped jobs.
[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q3 [0;36m([1;31mmain[0;36m) [0m$ cd ..
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3 [0;36m([1;31mmain[0;36m) [0m$ clear
[H[2J[3J[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3 [0;36m([1;31mmain[0;36m) [0m$ cleard ..[1Pexitsbcl --load Q3.lisp [8Pcat[C[C[C[C[C[C[C[C[C[8Pexitcat Q3.lisp [8@sbcl --load[C[C[C[C[C[C[C[C[Cexit[Kcd ..leard ..[1Pexitsbcl --load Q3.lisp [8Pcat[C[C[C[C[C[C[C[C[C[8Pexitcat Q3.lisp [8Pexit./a.out g++ QuickSort.cpp -std=gnu++17[12Pcat QuickSort.cpp exit[Kquitclear./a.out g++ QuickSort.cpp -std=gnu++17[Kexit[Kstopexit[K[K[K[Kscript Q4.[K[K[K[K[K[K[K[K[K[Kcd Home[K[K[K[KQ4/
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ scrit[Kpt Q4.txt
Script started, file is Q4.txt
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ cat Q4.lisp 
;;function to insert an element into a sorted list
(defun insert (item sorted)
  (cond ((null sorted) (list item))
        ((<= item (car sorted)) (cons item sorted))
        (t (cons (car sorted) (insert item (cdr sorted))))))

;; Insertion sort function
(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))

;;Different Testings
(print (insertion-sort '(4 2 7 4 5)))  
(print (insertion-sort '(3 4 8 7 1)))  
(print (insertion-sort '(-5 -2 -3 -1 -4)))  
(print (insertion-sort '()))  
[0;32m@maigaPassword [0m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ sbcl --load Q4.lisp 
This is SBCL 1.4.3, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

(2 4 4 5 7) 
(1 3 4 7 8) 
(-5 -4 -3 -2 -1) 
NIL 
* ^Z
[1]+  Stopped                 sbcl --load Q4.lisp
[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ exit
exit
There are stopped jobs.
[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-homeworks-maigaPassword/Homework3/Q4 [0;36m([1;31mmain[0;36m) [0m$ 
[K[0;32m@maigaPassword [1;31m➜ [1;34m/workspaces/csci-330-hom