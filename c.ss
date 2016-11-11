(define (emit . args)
  (write-char #\tab)
  (apply printf args)
  (newline))

(define (emit-label label)
  (printf "L~a:\n" label))

(define (emit-symbol s)
  (printf "_~a:\n" s))

(define (compile-program x)
  (emit "movl	$~a, %eax" x)
  (emit "retq"))

(define (compile-all x)
  (emit ".text")
  (emit ".globl	_scheme_entry")
  ;; (emit ".type	_scheme_entry, @function")
  (emit-symbol "scheme_entry")
  (compile-program x))

(compile-all (read))
