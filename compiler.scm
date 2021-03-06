;;; Scrisp: a Lisp syntax of average simplicity for JS.
;;; Useful for code generation.

(load "utils.so")
(import utils)

(define (default-names)
  (list '(document . document)
	'(alert . alert)
	'(prompt . prompt)
	'(eval-js . eval)
	'(sin . Math.sin)
	'(cos . Math.cos)))

(define-macro (-> . body)
  "Macro for piping expressions.

It subsequently executes forms from BODY and binds a result of each form
to a variable `<-'. Thus, `<-' always denote the result of a previous form.

  (-> 1 (+ 1 <-) (* <- 3)) ;=> 6"
  `(let* ,(map (lambda (form)
		 `(<- ,form))
	       body)
     <-))

(define (display-js-vector vec)
  "Displays a Scheme vector VEC in a Javascript-readable form."
  (display "[")
  (vector-for-each (lambda (x)
		     (display-js-atom x)
		     (display ", "))
		   vec)
  (display "]"))

(define display-js-number display)

(define (display-js-string str)
  "Displays string STR in a Javascript-readable form."
  (format #t "~S" str))

(define (display-js-boolean bool)
  (if bool
      (display "true ")
      (display "false ")))

(define (display-js-atom atom)
  "Displays Scheme atom as a Javascript atom."
  (cond ((vector? atom)
	 (display-js-vector atom))
	((number? atom)
	 (display-js-number atom))
	((string? atom)
	 (display-js-string atom))
	((boolean? atom)
	 (display-js-boolean atom))))

(define (symbol-first-char=? sym char)
  "Returns true if first character of symbol SYM is CHAR."
  (and (symbol? sym)
       (eqv? (string-ref (symbol->string sym) 0)
	     char)))

(define (at-symbol? sym)
  "Does SYM begin with #\\@?"
  (symbol-first-char=? sym #\@))

(define (dot-symbol? sym)
  "Does SYM begin with #\\.?"
  (symbol-first-char=? sym #\.))

(define (regular-symbol? sym)
  "Is SYM is a suitable variable name?"
  (and (symbol? sym)
       (not (or (at-symbol? sym)
		(dot-symbol? sym)))))

(define (at->dot sym)
  "Converts at-symbols to dot-symbols: (at->dot '@foo) => .foo."
  (assert (at-symbol? sym))
  (-> (symbol->string sym)
      #;(string-copy <-)
      (begin (string-set! <- 0 #\.)
	     <-)			; Too bad I have to do this.
      (string->symbol <-)))

(define (fresh-id sym)
  "Is supposed to return a non-used JS identifier."
  (-> (symbol->string sym)
      (string->list <-)
      (replace #\- #\_ <-)
      (replace #\? #\p <-)
      (cons #\_ <-)
      (list->string <-)
      (gensym <-)))

(define (make-env names)
  (map (lambda (x) (cons x (fresh-id x))) names))

(define (extend-env env-ext env)
  (if (null? env-ext)
      ;; We need to make sure ENV remains unique
      ;; so defines inside blocks work as expected.
      (copy-pair env)
      (append env-ext env)))

(define (compile-call caller args env)
  "Compiles a function call.

CALLER is a called object (weird, huh?), ARGS is arguments."
  (display "(")
  (compile caller env)
  (display ")(")
  (for-each/between (lambda (x)
		      (compile x env))
		    (lambda (x y)
		      (display ", "))
		    args)
  (display ")"))
      
(define (compile-dot-call obj dot-caller args env)
  "Compiles a member-function call.

OBJ is a parent object, DOT-CALLER is a name of callable field in form
of dot-symbol, ARGS is arguments."
  (display "(")
  (compile obj env)
  (display ")")
  (display dot-caller)
  (display "(")
  (for-each/between (lambda (x)
		      (compile x env))
		    (lambda (x y)
		      (display ", "))
		    args)
  (display ")"))

(define (compile-at-reference obj at-ref new-value env)
  "Compiles a field-reference.

OBJ is a referred object, at-ref is a referred field in form of at-symbol.
If NEW-VALUE is not false, it is used as a new value of the field."
  (display "(")
  (compile obj env)
  (display ")")
  (display (at->dot at-ref))
  (when new-value
    (display "=(")
    (compile new-value env)
    (display ")")))

(define (compile-block forms env #!optional return)
  "Compiles a sequence of FORMS into a {}-block."
  (display "{")
  (when return
    (display "return "))
  (for-each/between (lambda (x)
		      (compile x env))
		    (lambda (x y)
		      (display ","))
	    forms)
  (display "}"))

(define (compile-define name value env)
  (assert (regular-symbol? name))
  (let ((new-binding (cons name (fresh-id name))))
    ;; Now we populate ENV with new binding.
    ;; Note that after the next line ENV is destructively modified.
    (push! new-binding env)
    (display "var ")
    (display (cdr new-binding))
    (display "=")
    (compile value env)
    ;; This hack allows defines to reside withis blocks.
    (display ";null")))

(define (compile-lambda lambda-list body env)
  "Compiles an anonymous function. LAMBDA-LIST is formal arguments."
  (let ((env-ext (make-env lambda-list)))
    (display "function(")
    (for-each/between (lambda (x)
			(display (cdr x)))
		      (lambda (x y)
			(display ","))
		      env-ext)
    (display ")")
    (compile-block body (extend-env env-ext env) 'return)))

(define (compile-infix op-symbol forms env)
  "Compiles an operator as an infix-expression."
  (display "(")
  (for-each/between (lambda (x)
		      (compile x env))
		    (lambda (x y)
		      (display op-symbol))
		    forms)
  (display ")"))

(define (compile-the-if condition then-clause else-clause env)
  "Compiles an if form. Unconditionally."
  ;; It compiles THE if form, not performs a conditional compilation.
  (display "(")
  (compile condition env)
  (display "?")
  (compile then-clause env)
  (display ":")
  (compile else-clause env)
  (display ")"))

(define (compile-let bindings body env)
  (let ((env-ext (make-env (map car bindings))))
    (for-each (lambda (binding)
		(assert (regular-symbol? (car binding))
			"Dot-symbols or at-symbols cannot be used as variable names.")
		(display "var ")
		(display (retrieve-binding (car binding)
					   env-ext))
		(display "=")
		(compile (cadr binding) env)
		(display ","))
	      bindings)
    (compile-block body (extend-env env-ext env))))

(define (retrieve-binding name env)
  "Gets a NAME binging from ENVironment."
  (let ((binding (assv name env)))
    #;(write binding (current-error-port))
    (if binding
	(cdr binding)
	(error "Name is unbound in current scope" name))))

(define (compile form env)
  "Compiles a Scrisp expression to Javascript source. Result is displayed."
  #;(format (current-error-port)
	  "(COMPILE ~S ~S)~%~%" form env)
  (cond ((null? form)
	 (display "null "))
	((symbol? form)
	 (display (retrieve-binding form env)))
	((dot-symbol? form)
	 (error "Dot-symbol must be a first element of form."))
	((at-symbol? form)
	 (error "At-symbol must be a first element of form."))
	((atom? form)
	 (display-js-atom form))
	(else				; Is a list.
	 (let ((A (car form))
	       (D (cdr form)))
	   (case A
	     ((begin)
	      (compile-block D env))
	     ((define)
	      (compile-define (first D) (second D) env))
	     ((lambda)
	      (compile-lambda (car D) (cdr D) env))
	     ((* / + - < > <= >= ==) ; Treat operators as special forms
	      (compile-infix A D env))
	     ((if)
	      (compile-the-if (first D) (second D)
			      (third D) env))
	     ((let)
	      (compile-let (first D) (rest D) env))
	     (else
	      (cond ((dot-symbol? A)
		     (compile-dot-call (car D) A (cdr D)
				       env))
		    ((at-symbol? A)
		     (compile-at-reference (car D) A (maybe-nth 1 D) env))
		    (else
		     (compile-call A D env)))))))))
