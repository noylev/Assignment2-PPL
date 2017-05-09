#lang racket
(require "parser.rkt")



; Purpose: Produce a list of lists of length window each with the elements slided by factor window 
; Signature: sliding-window(lst,window)
; Type: [List * Number -> List]


; Purpose: Compute the greatest node data value in the tree
; Signature: greatest-node(tree)
; Type: [(List(Number) union Number) -> Number]
(define greatest-node
  (lambda (tree)
    'your-implementation-here))

; Purpose: Compute the number of nodes whose data value is equal to x
; Signature: count-node(tree,x)
; Type: [(List union T1) * T2 -> Number]
(define count-node
  (lambda (tree x)
    'your-implementation-here))

; Purpose: Compute the mirrored tree
; Signature: mirror-tree(tree)
; Type: [(List union T) -> List]
(define mirror-tree
  (lambda (tree)
    'your-implementation-here))

; Purpose: unparse a Scheme AST into Javascript syntax without considering infix notation
; Signature: unparse->js(ast,output-port)
; Type: [Exp * Output-Port -> Void]
(define unparse->js
  (lambda (ast output-port)

        (cond ((def-exp? ast)  (fprintf output-port "const ") (unparse->js (def-exp->var ast) output-port)
                                (fprintf output-port " = ")
                               (unparse->js (def-exp->val ast) output-port)
                               (fprintf output-port ";"))
                                
               ((var-exp? ast) (cond ((equal?(symbol->string(var-exp->var ast)) "=")
                                     (fprintf output-port "=="))                            
                                     ((equal?(symbol->string(var-exp->var ast)) "+")
                                     (fprintf output-port " +"))
                                     (else (fprintf output-port "~s" (var-exp->var ast)))))

                ;(if(equal?(symbol->string(var-exp->var ast)) "=")
                               ;(fprintf output-port "==")
                               ;(fprintf output-port "~s" (var-exp->var ast))))                
      ((cexp? ast)
      (cond ((num-exp? ast) (fprintf output-port "~s" (num-exp->val ast)) )
            ((app-exp? ast) (unparse->js (app-exp->rator ast) output-port)
                            
                              (unless(empty? (app-exp->rands ast))
                                 (begin (fprintf output-port "(")
                            (map (lambda (x) (unparse->js x output-port)                                                     
                                             (if(not(equal? (last(app-exp->rands ast)) x))
                                                (fprintf output-port ",")
                                                (fprintf output-port ")"))) (app-exp->rands ast))
                                                                        )))
                            
                   
            ((str-exp? ast) (fprintf output-port "~s" (str-exp->val ast)) )
            
            ((proc-exp? ast)  (fprintf output-port "(")
                              (map (lambda (x) (unparse->js x output-port)                                                          
                                               (if(not(equal? (last(proc-exp->params ast)) x))
                                                  (fprintf output-port ",")
                                                  (fprintf output-port ")"))) (proc-exp->params ast))
                               (fprintf output-port " => ")
                               (fprintf output-port "{ ")
                             ; (map (lambda (x) (unparse->js x output-port)) (proc-exp->body ast))
                               (map (lambda (x) (unparse->js x output-port)                                                          
                                               (if(not(equal? (last(proc-exp->params ast)) x))
                                                  (fprintf output-port "; ")
                                                  void)) (proc-exp->body ast))
                              (fprintf output-port " }")

                             )
            ((bool-exp? ast)
               (cond ((equal? (bool-exp->val ast) #t) (fprintf output-port "~a" "true") )
                     (else (fprintf output-port "~a" "false")))
             
            )
           ((let-exp? ast)
             (fprintf output-port "let ")
             (map (lambda (x)  (unparse->js (binding->var x) output-port)
                               (fprintf output-port " =")
                               (unparse->js (binding->val x) output-port)
                               (if(not(equal? (last(let-exp->bindings ast)) x))
                                  (fprintf output-port ", ")
                                  (fprintf output-port ";")))  (let-exp->bindings ast))
             
              (map (lambda (x) (unparse->js x output-port)
                                (if(not(equal? (last(let-exp->body ast)) x))
                                   (fprintf output-port "; ")
                                    void)) (let-exp->body ast))


             )

            ((if-exp? ast)                 
               (begin (unparse->js (if-exp->test ast) output-port)
                      (fprintf output-port " ? ")
                      (unparse->js (if-exp->then ast) output-port)
                      (fprintf output-port " : ")
                      (unparse->js (if-exp->else ast) output-port) ))
            
            (else 0))) (else 1))))

;(let ((var1 <exp1>) ... (varN <expN>)) <body1> ... <bodyK>)
;-> let var1 = <exp1>, ..., varN = <expN>; <body1>; ...; <bodyK>;

                            
; Purpose: unparse a Scheme AST into Javascript syntax while considering infix notation
; Signature: unparse->js-infix(ast,output-port)
; Type: [Exp * Output-Port -> Void]
(define unparse->js-infix
  (lambda (ast output-port)
    'your-implementation-here))




(define passed #t)

(define test-parse (lambda (test-num in out f g)
                     (letrec(
                             (op (open-output-string))
                             (close (lambda (func) (close-output-port op) func )))
                 
                       (f (g in) op)
                       (if(not(string=? 
                               (get-output-string op)
                               out))
                          (close
                           (begin (display (string-append "FAIL TEST " test-num " EXPECTED:" ))(newline)(newline)
                                  (display out)(newline)(newline)
                                  (display "GOT:")(newline)(newline)
                                  (display (get-output-string op))(newline)(newline)
                                  (set! passed #f)))
                          (void)))))

(define test (lambda (test-num in out)     
               (if(not(equal? 
                       in
                       out))
                  (begin (display (string-append "FAIL TEST " test-num " EXPECTED:" ))(newline)(newline)
                         (display out)(newline)(newline)
                         (display "GOT:")(newline)(newline)
                         (display in)(newline)(newline)
                         (set! passed #f))
                  (void))))

;unparse->js
(test-parse "unparse->js - 1"
            '(define x (f x (* 5 x) (= 3 2) y))
            "const x = f(x,*(5,x),==(3,2),y);"
            unparse->js
            parse)

(test-parse "unparse->js - 2"
            '(lambda (x y z) (display "hi!") (if (eq? x y) z (or #f (eq? x z))))
            "(x,y,z) => { display(\"hi!\"); eq?(x,y) ? z : or(false,eq?(x,z)) }"
            unparse->js
            parse)

(test-parse "unparse->js - 3"
            '(lambda (x) (let ((y (+ (f x) 5)) (z y)) (+ z y) #t))
            "(x) => { let y = +(f(x),5), z = y; +(z,y); true; }"
            unparse->js
            parse)

(test-parse "unparse->js - 4"
            '(lambda (x y z) x y z)
            "(x,y,z) => { x; y; z }"
            unparse->js
            parse)

(test-parse "unparse->js - 5"
            '(f (x (g (y (z x)))))
            "f(x(g(y(z(x)))))"
            unparse->js
            parse)

(test-parse "unparse->js - 6"
            '(= 4 5)
            "==(4,5)"
            unparse->js
            parse)

(test-parse "unparse->js - 7"
            '(if #t #f "sss")
            "true ? false : \"sss\""
            unparse->js
            parse)
