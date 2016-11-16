(import (chezscheme))

(define (comp-expr exp cont)
  (cond
    ((integer? exp) `(const ,exp . ,cont))
    (else (error 'comp-expr "Not handled" exp))))

(define (compile-phrase expr)
  (comp-expr expr '(stop)))

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

(define (emit-instr code)
  (case (car code)
    ((const)
      (out CONSTINT)
      (out-int (cadr code))
      (emit-instr (cddr code)))
    ((stop)
      (out STOP))
    (else
      (error 'emit-instr "Not hanlded" code))))

(define section-table '())
(define section-beginning 0)

(define (init-record port)
  (set! section-beginning (port-position port))
  (set! section-table '()))

(define (record port name)
  (let ((pos (port-position port)))
    (set! section-table (cons `(,name ,(- pos section-beginning)) section-table))
    (set! section-beginning pos)))

(define exec-magic-number "Caml1999X011")

(define (output-string port string)
  (put-bytevector port (string->utf8 string)))

(define (output-binary-int port n)
  (put-u8 port (fxlogand (fxsra n 24) #xff))
  (put-u8 port (fxlogand (fxsra n 16) #xff))
  (put-u8 port (fxlogand (fxsra n 8) #xff))
  (put-u8 port (fxlogand n #xff)))

(define (iter f l)
  (if (pair? l)
    (begin (f (car l)) (iter f (cdr l)))))

(define (write-toc-and-trailer port)
  (iter (lambda (entry)
          (let ((name (car entry)) (len (cadr entry)))
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
    (record port "PRIM")
    (record port "DATA")
    (record port "SYMB")
    (record port "CRCS")
    (write-toc-and-trailer port)
    (close-port port)))

(define (main)
  (emit-instr (compile-phrase (read)))
  (link-bytecode "a.out"))

(main)
