
(define (parse-file filename)
  (let loop ((s (open-input-file filename))
	     (lines '()))
    (let ((line (read-line s)))
      (cond
       ((eof-object? line)
	(close-input-port s)
	(reverse lines))
       (else (loop s (cons line lines)))))))
