#!/usr/bin/sbcl --script
(defun read-file (file)
  (with-open-file (input file)
    (loop for line = (read-line input nil)
          while line
          collect line)))

(defun parse-line (line)
  (read-from-string (substitute #\Space #\, (substitute #\( #\[ (substitute #\) #\] line)))))

(defun list->pair (x)
    (cond ((not (listp x)) x)
          ((= (length x) 1) (car x))
          (t (cons (list->pair (car x))
                   (list->pair (cadr x))))))

(defun prepare-line (line)
  (list->pair (parse-line line)))

(defun magnitude (x)
  (if (listp x)
      (+ (* 3 (magnitude (car x)))
         (* 2 (magnitude (cdr x))))
      x))

(defun split (x)
  (if (not (listp x))
      (if (< x 10) (cons x nil) (cons (cons (floor (/ x 2)) (ceiling (/ x 2))) t))
      (let* ((left (split (car x)))
             (right (if (cdr left)
                        (cons (cdr x) t)
                        (split (cdr x)))))
        (cons (cons (car left) (car right)) (cdr right)))))

(defun add-right (n x)
  (if (not (listp x))
      (+ x n)
      (let ((added (add-right n (cdr x))))
        (cons
          (if (equal added (cdr x))
              (add-right n (car x))
              (car x))
          added))))

(defun add-left (n x)
  (if (not (listp x))
      (+ x n)
      (let ((added (add-left n (car x))))
        (cons
          added
          (if (equal added (car x))
              (add-left n (cdr x))
              (cdr x))))))

; return: (list exploded? left right)
(defun expl (x n)
  (if (not (listp x))
      (list x nil nil nil)
      (if (> n 4)
          (list 0 t (car x) (cdr x))
          (let ((res1 (expl (car x) (1+ n)))
                (res2 (expl (cdr x) (1+ n))))
            (if (cadr res1)
                (if (cadddr res1)
                    (list (cons (car res1) (add-left (cadddr res1) (cdr x))) (cadr res1) (caddr res1) nil)
                    (cons (cons (car res1) (cdr x)) (cdr res1)))
                (if (and (cadr res2) (caddr res2))
                    (list (cons (add-right (caddr res2) (car x)) (car res2)) (cadr res2) nil (cadddr res2))
                    (cons (cons (car x) (car res2)) (cdr res2))))))))

(defun explode (x)
  (expl x 1))

(defun red (x)
  (let ((ex (explode x)))
    (if (cadr ex)
        (red (car ex))
        (let ((spl (split x)))
          (if (cdr spl)
              (red (car spl))
              (car spl))))))

(defun add (a b)
  (red (cons a b)))

(defun all-combinations (lines)
  (apply #'max
    (mapcar (lambda (x)
              (apply #'max
                (mapcar (lambda (y) (if (equal x y) 0 (magnitude (add x y)))) lines)))
            lines)))

(defun argv () sb-ext:*posix-argv*)

(defun main ()
  (let ((lines (mapcar #'prepare-line (read-file (cadr (argv))))))
    (format t "Part 1: ~a~%" (magnitude (reduce #'add lines)))
    (format t "Part 2: ~a~%" (all-combinations lines))))

(main)
