;;; utils.scm - utility library for Scrisp

(module utils 
    (export puts either str thunk define-macro define*
	    interpose λ -> rest copy-pair push!
	    replace vector-for-each for-each/between
	    maybe-nth)
  (import scheme chicken srfi-1 ports extras)

  (define (puts . things)
    "Displays THINGS from left to right."
    (for-each display things)
    (newline))
  
  (define (either . stuff)
    "Returns a random argument."
    (if (null? stuff)
	'()
	(list-ref stuff
		  (random (length stuff)))))

  (define (str . items)
    "Converts ITEMS to string and appends them."
    (with-output-to-string
      (lambda ()
	(for-each display items))))

  (define-syntax thunk
    "(thunk) is equivalent to (lambda ())."
    (syntax-rules ()
      ((thunk body ...)
       (lambda () body ...))))

  (define-syntax define-macro
    "Define a defmacro-style macro."
    (syntax-rules ()
      ((define-macro (name . param) body ...)
       (define-syntax name
	 (er-macro-transformer
	  (lambda (exp rename compare)
	    (apply (lambda param body ...)
		   (cdr exp))))))))

  (define-syntax define*
    "Define multiple items."
    (syntax-rules ()
      ((define* symbol value)
       (define symbol value))
      ((define* symbol1 value1 symbol2 value2 ...)
       (begin
	 (define symbol1 value1)
	 (define* symbol2 value2 ...)))))

  (define (interpose separator items)
    "Returns a list of ITEMS with each element separated by SEPARATOR."
    (if (or (null? items) (single? items))
	items
	(cons* (car items)
	       separator
	       (interpose separator (cdr items)))))

  (define-syntax λ
    "Greek extension for lambda."
    (syntax-rules ()
      ((λ . forms)
       (lambda . forms))))

  (define-macro (-> . body)
    "Macro for piping expressions.

It subsequently executes forms from BODY and binds a result of each form
to a variable `<-'. Thus, `<-' always denote the result of a previous form.

  (-> 1 (+ 1 <-) (* <- 3)) ;=> 6"
    `(let* ,(map (lambda (form)
		   `(<- ,form))
		 body)
       <-))

  (define rest cdr)

  (define (copy-pair pair)
    "Returns a copy of PAIR."
    (cons (car pair) (cdr pair)))

  (define (push! x pair)
    "Modifies PAIR such that X becomes its car and copy of PAIR becomes its cdr."
    (set-cdr! pair (copy-pair pair))
    (set-car! pair x))

  (define (replace item with items)
    "Replaces every ITEM-eqv?al item from ITEMS with WITH."
    (map (lambda (x) (if (eqv? x item) with x)) items))

  (define (vector-for-each fun vec)
    "Applies FUN to each element in VEC for side effects, returns nothing."
    (let ((len (vector-length vec)))
      (do ((i 0 (+ 1 i)))
	  ((>= i len))
	(fun (vector-ref vec i)))))

  (define (single? lst)
    "Returns true if LST is a list of one element."
    (and (pair? lst)
	 (null? (cdr lst))))

  (define (for-each/between fun-each fun-between items)
    "Applies FUN-EACH to each item in ITEMS. Invokes FUN-BETWEEN in between each call.

FUN-BETWEEN is applied to arguments of previous and next applications of
FUN-EACH."
    #					;(format (current-error-port)
    "(FOR-EACH/BETWEEN FUN-EACH FUN-BETWEEN ~A)~%" items)
    (cond ((null? items))
	  ((single? items) (fun-each (car items)))
	  (else
	   (fun-each (car items))
	   (fun-between (car items) (cadr items))
	   (for-each/between fun-each fun-between
			     (cdr items)))))

  (define (maybe-nth n items)
    "Returns (NTH N ITEMS) if it exists, otherwise #f."
    (cond ((<= n 0) (car items))
	  ((null? (cdr items)) #f)
	  (else (maybe-nth (- n 1) (cdr items)))))
)

;;; utils.scm ends here.
