
(define (curry f)
  (lambda (a)
    (lambda (b)
      (f a b))))

(define (flip f)
  (lambda (a b) (f b a)))

(define ~ flip)

(define & curry)

(define (-> data . functions)
  (let loop ((fs functions)
	     (d data))
    (cond
     ((null? fs) d)
     (else (loop (cdr fs) ((car fs) d))))))
