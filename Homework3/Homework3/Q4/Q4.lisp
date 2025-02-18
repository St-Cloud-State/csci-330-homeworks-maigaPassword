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
