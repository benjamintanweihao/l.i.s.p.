;; `evaluate' takes a program and the environment and returns a value
(define (evaluate e env)
  (if (atom? e)
      ;; `e` is an atom
      (cond ((symbol? e) (lookup e env))                                     ;; lookup symbol in the env
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) ;; autoquotable so we can
             e)                                                              ;; return the expression
            (else (wrong "Cannot evaluate" e)))
      ;; `e` is a form (i.e. `quote`, `if` etc.)
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ((begin)  (eprogn (cdr e) env))
        ;;                <symbol> env (compute the updated value)
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ;;                        <args>   <body>
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env))))))                           ;; function application

;; a pair is not at atom, an atom is not a pair
(define (atom? x) (not (pair? x)))


;; sequential evaluation of a group of forms
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      ;; terminal case
      '()))

;; takes a list of exps and returns the values of the exps
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

(define (lookup id env)
  ;; go through each key->value pair
  (if (pair? env)
      ;; get the key
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))


(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))


(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

;; TODO: how to display string in e
(define (wrong msg e)
  (lambda()
    (display msg)
    (newline)))
