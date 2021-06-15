
(define (map f lst)
  (let loop ((l lst)
	     (acc '()))
    (cond
     ((null? l) (reverse acc))
     (else (loop (cdr l) (cons (f (car l)) acc))))))

(define (filter f lst)
  (let loop ((l lst)
	     (acc '()))
    (cond
     ((null? l) (reverse acc))
     (else (let ((result (f (car l))))
	     (if result
		 (loop (cdr l) (cons (car l) acc))
		 (loop (cdr l) acc)))))))

(define (reduce f init lst)
  (let loop ((l lst)
	     (i init))
    (cond
     ((null? l) i)
     (else (loop (cdr l) (f (car l) i))))))

(define (zip-with f list1 list2)
  (let loop ((a list1)
	     (b list2)
	     (acc '()))
    (cond
     ((or (null? a) (null? b)) (reverse acc))
     (else (loop (cdr a) (cdr b) (cons (f (car a) (car b)) acc))))))

(define (split-at-index index lst)
  (let loop ((l lst)
	     (i index)
	     (acc '()))
    (cond
     ((zero? i) (list (reverse acc) l))
     (else (loop (cdr l) (- i 1) (cons (car l) acc))))))

(define (split lst)
  (let loop ((l lst)
	     (xs '())
	     (ys '()))
    (cond
     ((null? l) (list xs ys))
     ((null? (cdr l)) (list (cons (car l) xs) ys))
     (else (loop (cddr l) (cons (car l) xs) (cons (cadr l) ys))))))

(define (merge f a b)
  (cond
   ((null? a) b)
   ((null? b) a)
   ((f (car a) (car b)) (cons (car a) (merge f (cdr a) b)))
   (else (cons (car b) (merge f a (cdr b))))))

(define (sort f lst)
  (cond
   ((null? lst) lst)
   ((null? (cdr lst)) lst)
   (else
    (let* ((index (quotient (length lst) 2))
	   (splt (split-at-index index lst))
	   (left (car splt))
	   (right (cadr splt)))
      (merge f (sort f left) (sort f right))))))

(define (sort-old f lst)
  (cond
   ((null? lst) lst)
   ((null? (cdr lst)) lst)
   (else
    (let* ((splt (split lst))
	   (left (car splt))
	   (right (cadr splt)))
      (merge f (sort f left) (sort f right))))))
    
  
