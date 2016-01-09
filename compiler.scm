;;; Scrisp: a Lisp syntax of average simplicity for JS.
;;; Useful for code generation.

(define default-names
  '((document . document)
    (alert . alert)
    (prompt . prompt)
    (eval-js . eval)
    (sin . Math.sin)
    (cos . Math.cos)))

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
  #;(format (current-error-port)
	  "(FOR-EACH/BETWEEN FUN-EACH FUN-BETWEEN ~A)~%" items)
  (cond ((null? items))
	((single? items) (fun-each (car items)))
	(else
	 (fun-each (car items))
	 (fun-between (car items) (cadr items))
	 (for-each/between fun-each fun-between
			   (cdr items)))))

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
  "Compiles a sequens of FORMS into a {}-block."
  (display "{")
  (when return
    (display "return "))
  (for-each/between (lambda (x)
		      (compile x env))
		    (lambda (x y)
		      (display ","))
	    forms)
  (display "}"))

(define (make-env names)
  (map (lambda (x) (cons x (fresh-id x))) names))

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
    (compile-block body (append env-ext env) 'return)))

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
    (compile-block body (append env-ext env))))

(define (retrieve-binding name env)
  "Gets a NAME binging from ENVironment."
  (let ((binding (assv name env)))
    #;(write binding (current-error-port))
    (if binding
	(cdr binding)
	(error "Name is unbound in current scope" name))))

(define (maybe-nth n items)
  "Returns (NTH N ITEMS) if it exists, otherwise #f."
  (cond ((<= n 0) (car items))
	((null? (cdr items)) #f)
	(else (maybe-nth (- n 1) (cdr items)))))

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
