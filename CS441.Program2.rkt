#lang racket

;; Token Types
(define IF-TOKEN 'if-token)
(define READ-TOKEN 'read-token)
(define WRITE-TOKEN 'write-token) 
(define ENDIF-TOKEN 'endif-token)
(define ID-TOKEN 'id-token)
(define NUM-TOKEN 'num-token)
(define ASSIGN-TOKEN 'assign-token)
(define PLUS-TOKEN 'plus-token)
(define MINUS-TOKEN 'minus-token)
(define COMPARE-TOKEN 'compare-token)
(define SEMI-TOKEN 'semi-token)
(define LPAREN-TOKEN 'lparen-token)
(define RPAREN-TOKEN 'rparen-token)
(define DOLLARDOLLAR-TOKEN 'dollardollar-token)

(define (make-token type value line col)
  (list type value line col))

(define (letter? c) (and (char? c) (char-alphabetic? c)))
(define (digit? c) (and (char? c) (char-numeric? c)))
(define (whitespace? c) (and (char? c) (char-whitespace? c)))

(define (my-lexer port)
  (define (peek) (peek-char port))
  (define (next) (read-char port))
  (define (read-while pred)
    (let loop ([chars '()])
      (if (and (not (eof-object? (peek))) (pred (peek)))
          (loop (cons (next) chars))
          (list->string (reverse chars)))))
  (define line 1)
  (define col 1)
  
  (define (advance-position c)
    (if (equal? c #\newline)
        (begin (set! line (+ line 1)) (set! col 1))
        (set! col (+ col 1))))
  
  ; Skip whitespace
  (let whitespace-loop ()
    (when (and (not (eof-object? (peek))) (whitespace? (peek)))
      (let ([c (next)])
        (advance-position c)
        (whitespace-loop))))
  
  ; Handle EOF
  (when (eof-object? (peek))
    (make-token DOLLARDOLLAR-TOKEN #f line col))
  
  ; Handle tokens
  (let ([curr-line line]
        [curr-col col]
        [c (peek)])
    (cond
      ; Handle $$ token
      [(equal? c #\$)
       (next)
       (advance-position c)
       (if (and (not (eof-object? (peek))) (equal? (peek) #\$))
           (begin
             (next)
             (advance-position (peek))
             (make-token DOLLARDOLLAR-TOKEN #f curr-line curr-col))
           (error (format "Invalid character: standalone $ at line ~a, column ~a" curr-line curr-col)))]
      
      [(equal? c #\;) (next) (advance-position c) (make-token SEMI-TOKEN #f curr-line curr-col)]
      [(equal? c #\() (next) (advance-position c) (make-token LPAREN-TOKEN #f curr-line curr-col)]
      [(equal? c #\)) (next) (advance-position c) (make-token RPAREN-TOKEN #f curr-line curr-col)]
      
      [(equal? c #\=) 
       (next)
       (advance-position c)
       (if (and (not (eof-object? (peek))) (equal? (peek) #\=))
           (begin (next) (advance-position (peek)) (make-token COMPARE-TOKEN "==" curr-line curr-col))
           (make-token ASSIGN-TOKEN #f curr-line curr-col))]
      
      [(equal? c #\>) 
       (next)
       (advance-position c)
       (if (and (not (eof-object? (peek))) (equal? (peek) #\=))
           (begin (next) (advance-position (peek)) (make-token COMPARE-TOKEN ">=" curr-line curr-col))
           (make-token COMPARE-TOKEN ">" curr-line curr-col))]
      
      [(equal? c #\<) 
       (next)
       (advance-position c)
       (if (and (not (eof-object? (peek))) (equal? (peek) #\=))
           (begin (next) (advance-position (peek)) (make-token COMPARE-TOKEN "<=" curr-line curr-col))
           (make-token COMPARE-TOKEN "<" curr-line curr-col))]
      
      [(equal? c #\+) (next) (advance-position c) (make-token PLUS-TOKEN #f curr-line curr-col)]
      [(equal? c #\-)
       (next)
       (advance-position c)
       (if (and (not (eof-object? (peek))) (digit? (peek)))
           ; It's a negative number
           (let ([num (read-while digit?)])
             (for-each (lambda (char) (advance-position char)) (string->list num))
             (make-token NUM-TOKEN (string->number (string-append "-" num)) curr-line curr-col))
           ; It's a minus operator
           (make-token MINUS-TOKEN #f curr-line curr-col))]
      [(digit? c)
       (let ([num (string (next))])
         (advance-position c)
         (let ([rest (read-while digit?)])
           (for-each (lambda (char) (advance-position char)) (string->list rest))
           (make-token NUM-TOKEN (string->number (string-append num rest)) curr-line curr-col)))]
      
      [(letter? c)
       (let ([first-char (string (next))])
         (advance-position c)
         (let ([rest (read-while (lambda (c) (or (letter? c) (digit? c))))])
           (for-each (lambda (char) (advance-position char)) (string->list rest))
           (let ([id (string-append first-char rest)])
             (cond
               [(equal? id "if") (make-token IF-TOKEN #f curr-line curr-col)]
               [(equal? id "endif") (make-token ENDIF-TOKEN #f curr-line curr-col)]
               [(equal? id "read") (make-token READ-TOKEN #f curr-line curr-col)]
               [(equal? id "write") (make-token WRITE-TOKEN #f curr-line curr-col)]
               [else (make-token ID-TOKEN (string->symbol id) curr-line curr-col)]))))]
      
      [else (error (format "Invalid character: ~a at line ~a, column ~a" c curr-line curr-col))])))

(define (tokenize-input input-text)
  (let ([port (open-input-string input-text)])
    (let loop ([tokens '()])
      (let ([token (my-lexer port)])
        (if (eq? (car token) DOLLARDOLLAR-TOKEN)
            (reverse tokens)
            (loop (cons token tokens)))))))

(define tokens-stream '())

(define (peek) (if (null? tokens-stream) (make-token DOLLARDOLLAR-TOKEN #f 0 0) (car tokens-stream)))
(define (advance) (set! tokens-stream (cdr tokens-stream)))
(define (match expected-type)
  (let ([token (peek)])
    (if (eq? (car token) expected-type)
        (begin (advance) token)
         (error (format "Syntax error: expected ~a, got ~a at line ~a, column ~a" expected-type (car token) (caddr token) (cadddr token))))))

(define (parse-expr)
  (parse-comparison))

(define (parse-comparison)
  (let ([left (parse-add-sub)])
    (let ([next-tok (peek)])
      (if (eq? (car next-tok) COMPARE-TOKEN)
          (let ([op (cadr (match COMPARE-TOKEN))]
                [right (parse-add-sub)])
            (list 'expr 'compare op left right))
          left))))

(define (parse-add-sub)
  (let ([left (parse-term)])
    (let loop ([left left])
      (let ([next-tok (peek)])
        (cond
          [(eq? (car next-tok) PLUS-TOKEN) 
           (match PLUS-TOKEN) 
           (loop (list 'expr 'arith '+ left (parse-term)))]
          [(eq? (car next-tok) MINUS-TOKEN) 
           (match MINUS-TOKEN) 
           (loop (list 'expr 'arith '- left (parse-term)))]
          [else left])))))

(define (parse-term)
  (let ([token (peek)])
    (cond
      [(eq? (car token) NUM-TOKEN) 
       (advance) 
       (list 'expr 'num (cadr token))]
      [(eq? (car token) ID-TOKEN) 
       (advance) 
       (list 'expr 'id (cadr token))]
      [(eq? (car token) LPAREN-TOKEN) 
       (match LPAREN-TOKEN) 
       (let ([expr (parse-expr)]) 
         (match RPAREN-TOKEN) 
         expr)]
      [else (error (format "syntax error, unexpected token in: ~a at line ~a, column ~a" 
                         (car token) (caddr token) (cadddr token)))])))

;; Parses a list of statements
(define (parse-stmt-list)
  (if (or (null? tokens-stream) 
          (eq? (car (peek)) DOLLARDOLLAR-TOKEN)
          (eq? (car (peek)) ENDIF-TOKEN))  ; Stop when we see endif
      '()
      (let ([stmt (parse-stmt)])
        (if (eq? (car (peek)) SEMI-TOKEN)
            (advance) ; Consume semicolon
            (error (format "Syntax error: missing semicolon at line ~a, column ~a"
                           (caddr (peek)) (cadddr (peek)))))

        (cons stmt (parse-stmt-list)))))  ;; Recursively parse more statements

;; Parses a single statement
(define (parse-stmt)
  (let ([token (peek)])
    (displayln (format "Parsing statement: ~a" token))
    (cond
      [(eq? (car token) ID-TOKEN) 
       (let ([id (cadr token)]) 
         (advance) 
         (match ASSIGN-TOKEN) 
         (list 'stmt 'assign id (parse-expr)))]
      [(eq? (car token) IF-TOKEN) 
       (advance) 
       (match LPAREN-TOKEN)
       (let ([condition (parse-expr)])
         (match RPAREN-TOKEN)
         (let ([body (parse-stmt-list)])
           (match ENDIF-TOKEN)
           (list 'stmt 'if condition body)))]
      [(eq? (car token) READ-TOKEN) 
       (advance) 
       (list 'stmt 'read (cadr (match ID-TOKEN)))]
      [(eq? (car token) WRITE-TOKEN) 
       (advance) 
       (list 'stmt 'write (parse-expr))]
      [(eq? (car token) ENDIF-TOKEN)
       ; Not a statement, should be handled by the if-statement parsing
       (error (format "Unexpected endif token at line ~a, column ~a" (caddr token) (cadddr token)))]
      [else (error (format "syntax error, unexpected token in: ~a at line ~a, column ~a" 
                           (car token) (caddr token) (cadddr token)))])))



(define (parse-program tokens)
  (set! tokens-stream tokens)
  (let ([result (parse-stmt-list)])
    (if (null? tokens-stream) (cons "Accept" result) "Syntax error: unexpected tokens")))



(define (parse-program-text input-text) (parse-program (tokenize-input input-text)))
(define (parse-file filename) (parse-program-text (port->string (open-input-file filename))))


(parse-file "/Users/X/Desktop/ParserInput-1/file1.txt")
