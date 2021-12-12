#!/usr/bin/guile \
-e main -s
!#
(define (string->pair str)
  (let ((i (string-index str #\-)))
    (cons
      (substring str 0 i)
      (substring str (1+ i)))))

(define (read-line)
  (let loop ((char (read-char))
             (result '()))
    (if (or (eof-object? char) (eqv? #\newline char))
      (cons (reverse-list->string result) char)
      (loop (read-char) (cons char result)))))

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line))
                 (result '()))
        (if (eof-object? (cdr line))
          (reverse result)
          (loop (read-line) (cons (car line) result)))))))

(define (flip p) (cons (cdr p) (car p)))

(define (with-reverse x)
  (list x (flip x)))

(define (build-dict lines)
  (filter (lambda (x) (not (or (equal? (car x) "end") (equal? (cdr x) "start"))))
    (apply append
      (map (compose with-reverse string->pair) lines))))

(define (contains? l e)
  (and (not (null? l))
       (or (equal? (car l) e)
           (contains? (cdr l) e))))

(define (count l e)
  (apply + (map (lambda (x) (if (equal? x e) 1 0)) l)))

(define (partition f l)
  (let loop ((rest l)
             (x '())
             (y '()))
    (cond ((null? rest) (cons x y))
          ((f (car rest)) (loop (cdr rest) (cons (car rest) x) y))
          (else (loop (cdr rest) x (cons (car rest) y))))))

(define (next-elements edges element)
  (map cdr (filter (lambda (x) (equal? (car x) element)) edges)))

(define (big-cave? cave)
  (char-upper-case? (string-ref cave 0)))

(define (valid-path? path)
  (let ((last (car path)))
    (or (big-cave? last)
        (not (contains? (cdr path) last)))))

(define (valid-path2? path)
  (let loop ((caves (filter (compose not big-cave?) path))
             (visited-twice #f))
    (or (null? caves)
        (if (contains? (cdr caves) (car caves))
          (and (not visited-twice)
               (loop (cdr caves) #t))
          (loop (cdr caves) visited-twice)))))

(define (done? path)
  (equal? "end" (car path)))

(define (next edges path)
  (let ((last (car path)))
    (map (lambda (x) (cons x path)) (next-elements edges last))))

(define (find-paths f edges)
  (let loop ((paths '(("start")))
             (found '()))
    (let* ((next-paths (filter f (apply append (map (lambda (x) (next edges x)) paths))))
           (part (partition done? next-paths))
           (found2 (append (car part) found)))
      (if (null? (cdr part))
        found2
        (loop (cdr part) found2)))))

(define (part1 dict)
  (length (find-paths valid-path? dict)))

(define (part2 dict)
  (length (find-paths valid-path2? dict)))

(define (main args)
  (if (< (length args) 2)
      (begin
        (display "Usage: ")
        (display (car args))
        (display " <filename>")
        (newline))
      (let ((dict (build-dict (read-file (cadr args)))))
        (begin
          (display "Part 1: ")
          (display (part1 dict))
          (newline)
          (display "Part 2: ")
          (display (part2 dict))
          (newline)))))
