
(define-structure empty
  (empty-compare empty-compare))

(define-structure node
  (key key)
  (value value)
  (left left)
  (right right)
  (height height)
  (compare compare))

(define make-dict
  (case-lambda
   (() (make-empty <))
   ((lt) (make-empty lt))))

(define make-set
  (case-lambda
   (() (make-empty <))
   ((lt) (make-empty lt))))

(define (get-left d)
  (if (empty? d)
      d
      (left d)))

(define (get-right d)
  (if (empty? d)
      d
      (right d)))

(define (get-height d)
  (cond
   ((empty? d) -1)
   (else (height d))))

(define (get-balance d)
  (- (get-height (right d)) (get-height (left d))))

(define (new-height left right)
  (max (+ 1 (get-height right)) (+ 1 (get-height left))))

(define (rotate-right d)
  (let ((new-right (make-node (key d)
			      (value d)
			      (get-right (get-left d))
			      (get-right d)
			      (new-height (get-right (get-left d)) (get-right d))
			      (compare d))))
    (make-node (key (get-left d))
	       (value (get-left d))
	       (get-left (get-left d))
	       new-right
	       (new-height new-right (get-left (get-left d)))
	       (compare d))))

(define (rotate-left d)
  (let ((new-left (make-node (key d)
			     (value d)
			     (get-left d)
			     (get-left (get-right d))
			     (new-height (get-left d) (get-left (get-right d)))
			     (compare d))))
    (make-node (key (get-right d))
	       (value (get-right d))
	       new-left
	       (get-right (get-right d))
	       (new-height (get-right (get-right d)) new-left)
	       (compare d))))

(define (balance d)
  (cond
   ((< -2 (get-balance d) 2) d)
   ((<= (get-balance d) -2)
    (cond
     ((<= (get-balance (get-left d)) 0) (rotate-right d))
     (else (let ((new-sub (rotate-left (get-left d))))
	     (rotate-right (make-node (key d)
				      (value d)
				      new-sub
				      (right d)
				      (new-height new-sub (right d))
				      (compare d)))))))
   (else
    (cond
     ((>= (get-balance (get-right d)) 0) (rotate-left d))
     (else (let ((new-sub (rotate-right (get-right d))))
	     (rotate-left (make-node (key d)
				     (value d)
				     (left d)
				     new-sub
				     (new-height (left d) new-sub)
				     (compare d)))))))))
				      
(define (insert k v d)
  (cond
   ((empty? d)
    (make-node k v (empty-copy d) (empty-copy d) 0 (empty-compare d)))
   ((equal? k (key d)) d)
   (((compare d) k (key d))
    (let ((new-left (balance (insert k v (get-left d)))))
      (balance (make-node (key d)
			  (value d)
			  new-left
			  (get-right d)
			  (new-height (get-right d) new-left)
			  (compare d)))))
   (else
    (let ((new-right (balance (insert k v (get-right d)))))
      (balance (make-node (key d)
			  (value d)
			  (get-left d)
			  new-right
			  (new-height new-right (get-left d))
			  (compare d)))))))

(define (lookup k d)
  (cond
   ((empty? d) (make-empty (empty-compare d)))
   ((equal? k (key d)) (value d))
   (((compare d) k (key d)) (lookup k (left d)))
   (else (lookup k (right d)))))

(define (set-member? k s)
  (not (empty? (lookup k s))))

(define list->dict
  (case-lambda
   ((lst) (list->dict lst <))
   ((lst comp) 
    (let loop ((d (make-dict comp))
	       (l lst))
      (cond
       ((null? l) d)
       (else (loop (insert (car (car l)) (cdr (car l)) d) (cdr l))))))))

(define (set-add e s)
  (insert e #f s))

(define list->set
  (case-lambda
   ((lst) (list->set lst <))
   ((lst comp)
    (let loop ((s (make-set comp))
	       (l lst))
      (cond
       ((null? l) s)
       (else (loop (set-add (car l) s) (cdr l))))))))

