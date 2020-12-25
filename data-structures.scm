(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    (arr-val
      (arr arr-val?))
    )

  (define-datatype stack stack?
    (stack-type (counter number?) (stack-array arr-val?)))

  (define-datatype queue queue?
    (queue-type (counter number?) (queue-array arr-val?)))

;;; extractors:
(define queue->counter
    (lambda (que)
      (cases queue que
        (queue-type (counter queue-array) counter)
        (else (expval-extractor-error 'counter que)))))

  (define queue->array
    (lambda (que)
      (cases queue que
        (queue-type (counter queue-array) queue-array)
        (else (expval-extractor-error 'queue-array que)))))
 

  (define stack->counter
    (lambda (stk)
      (cases stack stk
        (stack-type (counter stack-array) counter)
        (else (expval-extractor-error 'counter stk)))))

  (define stack->array
    (lambda (stk)
      (cases stack stk
        (stack-type (counter stack-array) stack-array)
        (else (expval-extractor-error 'stack-array stk)))))


  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval->arr
    (lambda (v)
      (cases expval v
        (arr-val (arr) arr)
        (else (expval-extractor-error 'arr v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; arrays/stacks/queues ;;;;;;;;;;;;;;;;

  (define arr-val?
    (lambda (value)
      (if (list? value)
          (if (null? value)
              #t
              (reference? (car value)))
          #f)))
  
  (define create-array
    (lambda (length value)
      (if (= length 0)
          '()
          (cons (newref value) (create-array (- length 1) value)))
                         
    ))
  
  (define update-array
    (lambda (arr index value)
      (setref! (list-ref arr index) value)))

  (define read-array
    (lambda (arr index)
      (deref (list-ref arr index))))


  (define stack-size
    (lambda (arr)
      (read-array arr 0)))

  (define empty-stack?
    (lambda (arr)
      (= (expval->num (stack-size arr)) 0)))

  (define print-stack
    (lambda (arr n)
      (cases expval (read-array arr n)
        (bool-val (bool) (display "\nTop of the stack"))
        (num-val (num) (begin (display "\n") (display num) (print-stack arr (+ n 1))))
        (else (display "ERROR: UNEXPECTED ELEMENT IN STACK"))
        )))
  
   (define queue-size
    (lambda (arr)
     (num-val (- (expval->num (read-array arr 0)) (expval->num (read-array arr 1))))))

  (define empty-queue?
    (lambda (arr)
      (= (expval->num (queue-size arr)) 0)))

  (define print-queue
    (lambda (arr n)
      (cases expval (read-array arr n)
        (bool-val (bool) (display "\nHead of the queue"))
        (num-val (num) (begin (display "\n") (display num) (print-queue arr (+ n 1))))
        (else (display "ERROR: UNEXPECTED ELEMENT IN QUEUE"))
        )))
;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))


)
