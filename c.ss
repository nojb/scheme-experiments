(import (chezscheme))

(define (comp-expr-list exprl cont)
  (cond
    ((pair? exprl)
      (cond
        ((null? (cdr exprl)) (comp-expr (car exprl) cont))
        (else (comp-expr (car exprl) (cons* 'push (comp-expr-list (cdr exprl) cont))))))
    (else cont)))

(define (comp-args argl cont)
  (comp-expr-list (reverse argl) cont))

(define (comp-expr exp cont)
  (cond
    ((or (null? exp) (integer? exp)) (cons* 'const exp cont))
    ((pair? exp)
      (case (car exp)
        ((cons)
          (comp-args (cdr exp) (cons* 'c-call "scheme_cons" 2 cont)))
        ((car)
          (comp-args (cdr exp) (cons* 'c-call "scheme_car" 1 cont)))
        ((cdr)
          (comp-args (cdr exp) (cons* 'c-call "scheme_cdr" 1 cont)))
        (else
          (error 'comp-expr "Primitive not handled" exp))))
    (else (error 'comp-expr "Not handled" exp))))

(define (compile-phrase expr)
  (let ((res (comp-expr expr '(stop))))
    (display res)
    (newline)
    res))

(define out-buffer (make-bytevector 1024))
(define out-position 0)

(define (out-word b1 b2 b3 b4)
  (if (>= out-position (bytevector-length out-buffer))
    (let ((len (bytevector-length out-buffer))
           (new-buffer (make-bytevector (* 2  len))))
      (bytevector-copy! out-buffer 0 new-buffer 0 len)
      (set! out-buffer new-buffer)))
  (bytevector-u8-set! out-buffer out-position (fxlogand b1 #xff))
  (bytevector-u8-set! out-buffer (+ 1 out-position) (fxlogand b2 #xff))
  (bytevector-u8-set! out-buffer (+ 2 out-position) (fxlogand b3 #xff))
  (bytevector-u8-set! out-buffer (+ 3 out-position) (fxlogand b4 #xff))
  (set! out-position (+ 4 out-position)))

(define (out opcode)
  (out-word opcode 0 0 0))

(define (out-int n)
  (out-word n (fxsra n 8) (fxsra n 16) (fxsra n 24)))

(define CONSTINT 103)
(define STOP 143)
(define PUSH 9)
;; (define MAKEBLOCK 62)
;; (define GETFIELD 71)
(define CONSTEMPTYLIST 148)
(define C-CALLN 98)

(define c-primitives '())

(define (slot-for-c-prim name)
  (cdr (or (assoc name c-primitives)
         (let* ((n (length c-primitives))
                 (entry (cons name n)))
           (set! c-primitives (cons entry c-primitives))
           entry))))

(define (emit-instr code)
  (case (car code)
    ((const)
      (let ((c (cadr code)))
        (cond
          ((null? c)
            (out CONSTEMPTYLIST))
          ((integer? c)
            (out CONSTINT)
            (out-int c))
          (else
            (error 'emit-instr "Unhandled constant" c))))
      (emit-instr (cddr code)))
    ((push)
      (out PUSH)
      (emit-instr (cdr code)))
    ;; ((makeblock)
    ;;   (out MAKEBLOCK)
    ;;   (out-int (cadr code))
    ;;   (out-int (caddr code))
    ;;   (emit-instr (cdddr code)))
    ;; ((getfield)
    ;;   (out GETFIELD)
    ;;   (out-int (cadr code))
    ;;   (emit-instr (cddr code)))
    ((c-call)
      (out C-CALLN)
      (out-int (caddr code))
      (out-int (slot-for-c-prim (cadr code)))
      (emit-instr (cdddr code)))
    ((stop)
      (out STOP))
    (else
      (error 'emit-instr "Not handled" code))))

(define section-table '())
(define section-beginning 0)

(define (init-record port)
  (set! section-beginning (port-position port))
  (set! section-table '()))

(define (record port name)
  (let ((pos (port-position port)))
    (set! section-table (cons (cons name (- pos section-beginning)) section-table))
    (set! section-beginning pos)))

(define exec-magic-number "Caml1999X011")

(define (output-string port string)
  (put-bytevector port (string->utf8 string)))

(define (output-primitive-table port)
  (for-each (lambda (entry)
              (let ((name (car entry)))
                (output-string port name)
                (put-u8 port 0)))
    (reverse c-primitives)))

(define (output-binary-int port n)
  (put-u8 port (fxlogand (fxsra n 24) #xff))
  (put-u8 port (fxlogand (fxsra n 16) #xff))
  (put-u8 port (fxlogand (fxsra n 8) #xff))
  (put-u8 port (fxlogand n #xff)))

(define (write-toc-and-trailer port)
  (for-each (lambda (entry)
          (let ((name (car entry)) (len (cdr entry)))
            (output-string port name)
            (output-binary-int port len)))
    (reverse section-table))
  (output-binary-int port (length section-table))
  (output-string port exec-magic-number)
  (set! section-table '()))

(define (link-bytecode exec-name)
  (if (file-exists? exec-name) (delete-file exec-name))
  (let ((port (open-file-output-port exec-name)))
    (init-record port)
    (put-bytevector port out-buffer 0 out-position)
    (record port "CODE")
    (output-primitive-table port)
    (record port "PRIM")
    (record port "DATA")
    (record port "SYMB")
    (record port "CRCS")
    (write-toc-and-trailer port)
    (close-port port)))

(define (main)
  (emit-instr (compile-phrase (read)))
  (link-bytecode "a.out"))

(define CODE-INT32 #x2)
(define CODE-BLOCK32 #x8)

(define-values (extern-port flush) (open-bytevector-output-port))

(define (store32 port v)
  (let ((get (lambda (i) (bitwise-and (bitwise-arithmetic-shift-right v i) #xff))))
    (put-u8 port (get 24))
    (put-u8 port (get 16))
    (put-u8 port (get 8))
    (put-u8 port (get 0))))

(define (writecode32 code v)
  (put-u8 extern-port code)
  (store32 extern-port v))

(define (hd tag sz)
  (bitwise-ior
    (bitwise-arithmetic-shift-left (bitwise-and sz #x3fffffffffffff) 10)
    (bitwise-and tag #xf)))

(define (extern-rec v)
  (cond
    ((integer? v)
      (writecode32 CODE-INT32 v))
    ((pair? v)
      (let* ((tag (car v)) (fields (cdr v)))
        (writecode32 CODE-BLOCK32 (hd tag (length fields)))
        (for-each extern-rec fields)))
    (else
      (error 'extern-rec "Not handled" v))))

(define (extern v)
  (extern-rec v)
  (flush))

(define object-tag 248)

(main)
