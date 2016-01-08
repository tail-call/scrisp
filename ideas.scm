;;; Test case

(with-output-to-file "~/script.js"
  (lambda ()
    (compile

     '(let ((display alert))
	(display "Hello world!"))

     default-names)
    ))

;;;; Ideas

;;; Here go some ideas on language features.

;;; Extern special form defines bindings from JS expressions (presumably names)
;;; to Scrisp symbols. Compiler should always check the lexical scoping so
;;; undeclared names don't appear anywhere in the code, making simply referring
;;; to JS names without extern-declaration impossible.

(extern document
	alert prompt
	Math.sin as: sin
	Math.cos as: cos
	"new Cons" as: cons
	#;? Cons as-constructor: cons)

;;; Creating programmatic macros this way is a misuse:

(extern i++ as: inc-i
	i-- as: dec-i)


;;; Dot-symbols and at-symbols are the means of accessing properties much
;;; like in Clojure.

(@length "This string is 34 characters long.") ; Getting property value
(@name worker "Ivan")			       ; Setting property value

(.sin Math)				       ; Invoking a member function
(@sin Math)				       ; Referencing a member function


(let ((canvas (.getElementById document "myCanvas"))
      (context (.getContext canvas "2d")))
  (doto context
    (.beginPath)
    (.moveTo 170 80)
    (.bezierCurveTo 130 100 130 150 230 150)
    (.closePath)
    (@lineWidth 5)
    (@strokeStyle "blue")
    (.stroke))
