;; Function to split the list into two halves
(defun split-list (lst)
  (if (or (null lst) (null (cdr lst)))
      (list lst nil)
      (let ((first nil) (second nil))
        (loop for x in lst
              for i from 0
              do (if (evenp i)
                     (push x first)
                     (push x second)))
        (list (reverse first) (reverse second)))))

;; Merge two sorted lists
(defun my-merge (lst1 lst2)
  (cond ((null lst1) lst2)
        ((null lst2) lst1)
        ((<= (car lst1) (car lst2)) (cons (car lst1) (my-merge (cdr lst1) lst2)))
        (t (cons (car lst2) (my-merge lst1 (cdr lst2))))))

;; Mergesort function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let* ((split (split-list lst))
             (left (first split))
             (right (second split)))
        (my-merge (mergesort left) (mergesort right)))))

;;Different Testing 
(print (mergesort '(1 2 3 4 5)))  
(print (mergesort '(5 4 3 2 1)))
(print (mergesort '(-5 -2 -3 -1 -4))) 
(print (mergesort '())) 
