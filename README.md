# CS441-Program-2-Parser-Scanner
This is a parser for a simple grammar written in Racket through prompting AI models.

Discussion document can be found in this repository or click [here](https://docs.google.com/document/d/1tghGYyjHnwpZntJjLf-CdSqQfFIY9pnM53fBpX84MLw/edit?usp=sharing) to view online.

Below is the output and the AI prompts & generated code.

****

# Output


    File 1:
    A = 6; 
    B = 5; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    endif; 
    write A + B - X; 
    $$ 

<img width="574" alt="Screen Shot 2025-03-31 at 4 14 30 PM" src="https://github.com/user-attachments/assets/f18bce7e-7cb9-47c6-a72c-3bf7f6a5556f" />


    File 2
    A = 6; 
    B = 5; 
    X = -2 
    if (A + B > 10)
       X = A + 2;
    endif; 
    write A + B - X; 
    $$

<img width="427" alt="Screen Shot 2025-03-31 at 4 14 44 PM" src="https://github.com/user-attachments/assets/c8a8737b-a459-4278-9567-6de0e19ccbe9" />


    File 3
    A = 6; 
    read B; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    write A + B - X; 
     $$ 

<img width="612" alt="Screen Shot 2025-03-31 at 4 14 55 PM" src="https://github.com/user-attachments/assets/69bf918c-1fb0-4c10-9a0d-32a72fb34526" />


    File 4
    A = 6; 
    B# = 5; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    endif;
    write A + B - X; 

<img width="352" alt="Screen Shot 2025-03-31 at 4 15 14 PM" src="https://github.com/user-attachments/assets/6f39e794-e626-4079-a7a4-09f80b83990e" />

****

# AI Prompts & Generated code:

**Note: Due to free access limitations on models, ChatGPT, DeepSeek, & Claude were used in this program.**


**CatL:** Create a racket tokenizer for the following grammar. May use parser-tools/lex. 

program -> {stmt_list} $$ 

stmt_list -> stmt stmt_list | epsilon 

stmt -> id = expr;
          | if (expr) stmt_list endif;
          | read id;
          | write expr;  

expr -> id etail | num etail

etail -> + expr 
         | - expr 
         | compare expr
         | epsilon 

id -> [a-zA-Z]+ 

num -> numsign digit digit* 

numsign -> + | - | epsilon 

compare -> < | <= | > | >= | == | !=

**ChatGPT:** Generated Code:

        #lang racket
        (require parser-tools/lex)
        
        (define-tokens tokens
          (ID NUM IF READ WRITE ENDIF ASSIGN PLUS MINUS COMPARE SEMI LPAREN RPAREN))
        
        (define-empty-tokens keywords
          (if read write endif))
        
        (define lexer
          (lexer
           [(eof) 'EOF]
           [#\; (token SEMI)]
           ["if" (token IF)]
           ["endif" (token ENDIF)]
           ["read" (token READ)]
           ["write" (token WRITE)]
           ["=" (token ASSIGN)]
           ["+" (token PLUS)]
           ["-" (token MINUS)]
           ["<" "<=" ">" ">=" "==" "!=" (token COMPARE)]
           [#\( (token LPAREN)]
           [#\) (token RPAREN)]
           [(:+ alphabetic) (token ID (string->symbol (lexeme)))]
           [(:? (char-set "+-") (:* numeric)) (token NUM (string->number (lexeme)))]
           [whitespace (lexer)]))


**CatL:** Now create a function that takes an input txt and makes the input tokens.

**ChatGPT:** Generated Code:

        (define (tokenize-input input-text)
          (let ([port (open-input-string input-text)])
            (let loop ([tokens '()])
              (let ([token (lexer port)])
                (if (eq? token 'EOF)
                    (reverse tokens)
                    (loop (cons token tokens)))))))

**CatL:** Add functions to have a top-down recursive-descent parser that produces a parse tree for a given input. Following the parse, return a string: either “Accept”, indicating the program is syntactically correct, and the parse tree; or a message as to which line the first syntax error was found.

**ChatGPT:** 

        ;; Parser Functions
        (define tokens-stream '())
        (define (advance) (set! tokens-stream (cdr tokens-stream)))
        (define (peek) (if (null? tokens-stream) 'EOF (car tokens-stream)))
        
        (define (match expected)
          (if (eq? (peek) expected)
              (advance)
              (error "Syntax error: expected" expected)))
        
        (define (parse-expr)
          (let ((token (peek)))
            (cond
              [(eq? token 'ID) (advance) '(expr ID)]
              [(eq? token 'NUM) (advance) '(expr NUM)]
              [else (error "Syntax error in expression")])))
        
        (define (parse-stmt)
          (let ((token (peek)))
            (cond
              [(eq? token 'ID) (advance) (match 'ASSIGN) (parse-expr) (match 'SEMI) '(stmt assignment)]
              [(eq? token 'IF) (advance) (match 'LPAREN) (parse-expr) (match 'RPAREN) (parse-stmt-list) (match 'ENDIF) '(stmt if)]
              [(eq? token 'READ) (advance) (match 'ID) (match 'SEMI) '(stmt read)]
              [(eq? token 'WRITE) (advance) (parse-expr) (match 'SEMI) '(stmt write)]
              [else (error "Syntax error in statement")]))
        
        (define (parse-stmt-list)
          (if (or (eq? (peek) 'ENDIF) (eq? (peek) 'EOF))
              '()
              (cons (parse-stmt) (parse-stmt-list))))
        
        (define (parse-program input-text)
          (set! tokens-stream (tokenize-input input-text))
          (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
            (let ((tree (parse-stmt-list)))
              (if (null? tokens-stream)
                  (cons "Accept" tree)
                  (format "Syntax error at token: ~a" (peek)))))))

        (define (parse-file filename)
          (let ([input-text (port->string (open-input-file filename))])
            (parse-program input-text)))

**CatL:** I'm getting an error message from line 13: token: unbound identifier in: token

**ChatGPT:** The issue is that token is not a built-in function in Racket. In parser-tools/lex, you should use token-ID (or token-NAME based on your token definition). I've updated your lexer definitions to correctly use token- prefixed identifiers. Now, the token issue should be resolved.

        (define lexer
          (lexer
           [(eof) 'EOF]
           [#\; (token-SEMI)]
           ["if" (token-IF)]
           ["endif" (token-ENDIF)]
           ["read" (token-READ)]
           ["write" (token-WRITE)]
           ["=" (token-ASSIGN)]
           ["+" (token-PLUS)]
           ["-" (token-MINUS)]
           ["<" "<=" ">" ">=" "==" "!=" (token-COMPARE)]
           [#\( (token-LPAREN)]
           [#\) (token-RPAREN)]
           [(char-set "a-zA-Z") (token-ID (string->symbol (lexeme)))]
           [(:? (char-set "+-") (:+ (char-set "0-9"))) (token-NUM (string->number (lexeme)))]
           [whitespace (lexer)]))

**CatL:** I am now getting error line 24:  :+: unbound identifier in: :+

**ChatGPT:** The error is caused by the :+ and :* syntax, which is not valid in parser-tools/lex. The correct way to define these patterns is by using predefined character sets like (:+ (char-set "a-zA-Z")) for identifiers and (:* (char-set "0-9")) for numbers.

        (define lexer
          (lexer
           [(eof) 'EOF]
           [#\; (token-SEMI)]
           ["if" (token-IF)]
           ["endif" (token-ENDIF)]
           ["read" (token-READ)]
           ["write" (token-WRITE)]
           ["=" (token-ASSIGN)]
           ["+" (token-PLUS)]
           ["-" (token-MINUS)]
           ["<" "<=" ">" ">=" "==" "!=" (token-COMPARE)]
           [#\( (token-LPAREN)]
           [#\) (token-RPAREN)]
           [(char-set "a-zA-Z") (token-ID (string->symbol (lexeme)))]
           [(:? (char-set "+-") (:+ (char-set "0-9"))) (token-NUM (string->number (lexeme)))]
           [whitespace (lexer)]))

**NOTE: at this point I ran out of my free access to ChatGPT, so I moved over to DeepSeek**

**CatL:** The grammar should recognize a program as follows:

program -> {stmt_list} $$ 

but currently isn't recognizing the $ character

**DeepSeek:** I'll add all the necessary modifications to implement the $$ end-of-program marker. Here's the complete updated program:

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
        (define DOLLARDOLLAR-TOKEN 'dollardollar-token)  ;; New token for $$
        (define EOF-TOKEN 'eof-token)
        
        ;; Token Constructor
        (define (make-token type value)
          (cons type value))
        
        ;; Helper functions
        (define (letter? c) (and (char? c) (char-alphabetic? c)))
        (define (digit? c) (and (char? c) (char-numeric? c)))
        (define (whitespace? c) (and (char? c) (char-whitespace? c)))
        
        ;; Custom lexer implementation
        (define (my-lexer port)
          (define (peek) (peek-char port))
          (define (next) (read-char port))
          
          ;; Helper to read a sequence of characters matching a predicate
          (define (read-while pred)
            (let loop ([chars '()])
              (if (and (not (eof-object? (peek))) (pred (peek)))
                  (loop (cons (next) chars))
                  (list->string (reverse chars)))))
          
          ;; Skip whitespace
          (let loop ()
            (when (and (not (eof-object? (peek))) (whitespace? (peek)))
              (next)
              (loop)))
          
          ;; Handle EOF
          (when (eof-object? (peek))
            (make-token EOF-TOKEN #f))
          
          ;; Match tokens
          (let ([c (peek)])
            (cond
              [(equal? c #\;) (next) (make-token SEMI-TOKEN #f)]
              [(equal? c #\() (next) (make-token LPAREN-TOKEN #f)]
              [(equal? c #\)) (next) (make-token RPAREN-TOKEN #f)]
              [(equal? c #\$)  ;; Handle $$ token
               (next)
               (if (equal? (peek) #\$)
                   (begin (next) (make-token DOLLARDOLLAR-TOKEN #f))
                   (error "Invalid character: standalone $"))]
              [(equal? c #\=) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "==")]
                 [else (make-token ASSIGN-TOKEN #f)])]
              [(equal? c #\<) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "<=")]
                 [else (make-token COMPARE-TOKEN "<")])]
              [(equal? c #\>) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN ">=")]
                 [else (make-token COMPARE-TOKEN ">")])]
              [(equal? c #\!) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "!=")]
                 [else (error "Invalid character after !")])]
              [(equal? c #\+) (next) (make-token PLUS-TOKEN #f)]
              [(equal? c #\-) (next) (make-token MINUS-TOKEN #f)]
              
              ;; Handle identifiers and keywords
              [(letter? c)
               (let ([id (read-while (lambda (c) (or (letter? c) (digit? c))))])
                 (cond
                   [(equal? id "if") (make-token IF-TOKEN #f)]
                   [(equal? id "endif") (make-token ENDIF-TOKEN #f)]
                   [(equal? id "read") (make-token READ-TOKEN #f)]
                   [(equal? id "write") (make-token WRITE-TOKEN #f)]
                   [else (make-token ID-TOKEN (string->symbol id))]))]
              
              ;; Handle numbers
              [(or (digit? c) (and (or (equal? c #\+) (equal? c #\-)) 
                                   (char? (peek-char port 1)) 
                                   (digit? (peek-char port 1))))
               (let* ([sign (if (or (equal? c #\+) (equal? c #\-)) 
                                (begin (next) (if (equal? c #\-) -1 1))
                                1)]
                      [num (read-while digit?)])
                 (make-token NUM-TOKEN (* sign (string->number num))))]
              
              [else (error (format "Invalid character: ~a" c))])))
        
        (define (tokenize-input input-text)
          (let ([port (open-input-string input-text)])
            (let loop ([tokens '()])
              (let ([token (my-lexer port)])
                (if (eq? (car token) EOF-TOKEN)
                    (reverse tokens)
                    (loop (cons token tokens)))))))
        
        ;; Parser Functions
        (define (token-type token) (car token))
        (define (token-value token) (cdr token))
        
        (define (parse-program tokens)
          (define tokens-stream tokens)
          
          (define (peek)
            (if (null? tokens-stream)
                (make-token EOF-TOKEN #f)
                (car tokens-stream)))
          
          (define (advance)
            (set! tokens-stream (cdr tokens-stream)))
          
          (define (match expected-type)
            (let ([token (peek)])
              (if (eq? (token-type token) expected-type)
                  (begin (advance) token)
                  (error (format "Syntax error: expected ~a, got ~a" 
                                 expected-type (token-type token))))))
          
          (define (parse-expr)
            (let ((token (peek)))
              (cond
                [(eq? (token-type token) ID-TOKEN) 
                 (advance) 
                 (list 'expr 'id (token-value token))]
                [(eq? (token-type token) NUM-TOKEN) 
                 (advance) 
                 (list 'expr 'num (token-value token))]
                [else (error "Syntax error in expression")])))
          
          (define (parse-stmt)
            (let ((token (peek)))
              (cond
                [(eq? (token-type token) ID-TOKEN) 
                 (let ([id-val (token-value token)])
                   (advance) 
                   (match ASSIGN-TOKEN)
                   (let ([expr (parse-expr)])
                     (match SEMI-TOKEN)
                     (list 'stmt 'assignment id-val expr)))]
                
                [(eq? (token-type token) IF-TOKEN) 
                 (advance)
                 (match LPAREN-TOKEN)
                 (let ([expr (parse-expr)])
                   (match RPAREN-TOKEN)
                   (let ([stmt-list (parse-stmt-list)])
                     (match ENDIF-TOKEN)
                     (list 'stmt 'if expr stmt-list)))]
                
                [(eq? (token-type token) READ-TOKEN) 
                 (advance)
                 (let ([id-token (match ID-TOKEN)])
                   (match SEMI-TOKEN)
                   (list 'stmt 'read (token-value id-token)))]
                
                [(eq? (token-type token) WRITE-TOKEN) 
                 (advance)
                 (let ([expr (parse-expr)])
                   (match SEMI-TOKEN)
                   (list 'stmt 'write expr))]
                
                [else (error (format "Syntax error in statement: ~a" (token-type token)))])))
          
          (define (parse-stmt-list)
            (let ([next-token (peek)])
              (if (or (eq? (token-type next-token) DOLLARDOLLAR-TOKEN)
                      (eq? (token-type next-token) EOF-TOKEN))
                  '()
                  (cons (parse-stmt) (parse-stmt-list)))))
          
          (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
            (let ((tree (parse-stmt-list)))
              (match DOLLARDOLLAR-TOKEN)  ; Expect $$ to end program
              (if (null? tokens-stream)
                  (cons "Accept" tree)
                  (format "Syntax error: unexpected tokens after $$: ~a" 
                         (map token-type tokens-stream))))))
        
        (define (parse-program-text input-text)
          (parse-program (tokenize-input input-text)))
        
        (define (parse-file filename)
          (let ([input-text (port->string (open-input-file filename))])
            (parse-program-text input-text)))

**CatL:** Does this txt work with the grammar?

A = 6; 
B = 5; 
X = -2; 
if (A + B > 10)
   X = A + 2;
endif; 
write A + B - X; 
 $$

 **DeepSeek:** 
1. Arithmetic Expressions: Your parser's parse-expr function currently only handles simple identifiers and numbers, but not:
       - Binary operations (A + B, A + B > 10, etc.)
       - Negative numbers (though X = -2; might work from the lexer)
2. Comparison Operators: The lexer recognizes comparisons but the parser doesn't handle them in expressions yet.
3. If Condition: The if statement condition currently expects a simple expression, not a comparison.

I'll enhance the parser to handle arithmetic expressions and comparisons. Here's the complete modified program with these improvements:

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
        (define EOF-TOKEN 'eof-token)
        
        ;; Token Constructor
        (define (make-token type value)
          (cons type value))
        
        ;; Helper functions
        (define (letter? c) (and (char? c) (char-alphabetic? c)))
        (define (digit? c) (and (char? c) (char-numeric? c)))
        (define (whitespace? c) (and (char? c) (char-whitespace? c)))
        
        ;; Custom lexer implementation
        (define (my-lexer port)
          (define (peek) (peek-char port))
          (define (next) (read-char port))
          
          (define (read-while pred)
            (let loop ([chars '()])
              (if (and (not (eof-object? (peek))) (pred (peek)))
                  (loop (cons (next) chars))
                  (list->string (reverse chars)))))
          
          ;; Skip whitespace
          (let loop ()
            (when (and (not (eof-object? (peek))) (whitespace? (peek)))
              (next)
              (loop)))
          
          (when (eof-object? (peek))
            (make-token EOF-TOKEN #f))
          
          (let ([c (peek)])
            (cond
              [(equal? c #\;) (next) (make-token SEMI-TOKEN #f)]
              [(equal? c #\() (next) (make-token LPAREN-TOKEN #f)]
              [(equal? c #\)) (next) (make-token RPAREN-TOKEN #f)]
              [(equal? c #\$)
               (next)
               (if (equal? (peek) #\$)
                   (begin (next) (make-token DOLLARDOLLAR-TOKEN #f))
                   (error "Invalid character: standalone $"))]
              [(equal? c #\=) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "==")]
                 [else (make-token ASSIGN-TOKEN #f)])]
              [(equal? c #\<) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "<=")]
                 [else (make-token COMPARE-TOKEN "<")])]
              [(equal? c #\>) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN ">=")]
                 [else (make-token COMPARE-TOKEN ">")])]
              [(equal? c #\!) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "!=")]
                 [else (error "Invalid character after !")])]
              [(equal? c #\+) (next) (make-token PLUS-TOKEN #f)]
              [(equal? c #\-) (next) (make-token MINUS-TOKEN #f)]
              
              [(letter? c)
               (let ([id (read-while (lambda (c) (or (letter? c) (digit? c))))])
                 (cond
                   [(equal? id "if") (make-token IF-TOKEN #f)]
                   [(equal? id "endif") (make-token ENDIF-TOKEN #f)]
                   [(equal? id "read") (make-token READ-TOKEN #f)]
                   [(equal? id "write") (make-token WRITE-TOKEN #f)]
                   [else (make-token ID-TOKEN (string->symbol id))]))]
              
              [(or (digit? c) (and (or (equal? c #\+) (equal? c #\-)) 
                                   (char? (peek-char port 1)) 
                                   (digit? (peek-char port 1))))
               (let* ([sign (if (or (equal? c #\+) (equal? c #\-)) 
                                (begin (next) (if (equal? c #\-) -1 1))
                                1)]
                      [num (read-while digit?)])
                 (make-token NUM-TOKEN (* sign (string->number num))))]
              
              [else (error (format "Invalid character: ~a" c))])))
        
        (define (tokenize-input input-text)
          (let ([port (open-input-string input-text)])
            (let loop ([tokens '()])
              (let ([token (my-lexer port)])
                (if (eq? (car token) EOF-TOKEN)
                    (reverse tokens)
                    (loop (cons token tokens)))))))
        
        ;; Parser Functions
        (define (token-type token) (car token))
        (define (token-value token) (cdr token))
        
        (define (parse-program tokens)
          (define tokens-stream tokens)
          
          (define (peek)
            (if (null? tokens-stream)
                (make-token EOF-TOKEN #f)
                (car tokens-stream)))
          
          (define (advance)
            (set! tokens-stream (cdr tokens-stream)))
          
          (define (match expected-type)
            (let ([token (peek)])
              (if (eq? (token-type token) expected-type)
                  (begin (advance) token)
                  (error (format "Syntax error: expected ~a, got ~a" 
                                 expected-type (token-type token))))))
          
          ;; Enhanced expression parser with operator precedence
          (define (parse-expr)
            (parse-comparison))
          
          (define (parse-comparison)
            (let ([left (parse-add-sub)])
              (let ([next-tok (peek)])
                (if (eq? (token-type next-tok) COMPARE-TOKEN)
                    (let ([op (token-value (match COMPARE-TOKEN))]
                          [right (parse-add-sub)])
                      (list 'expr 'compare op left right))
                    left))))
          
          (define (parse-add-sub)
            (let ([left (parse-term)])
              (let loop ([left left])
                (let ([next-tok (peek)])
                  (cond
                    [(eq? (token-type next-tok) PLUS-TOKEN)
                     (match PLUS-TOKEN)
                     (let ([right (parse-term)])
                       (loop (list 'expr 'arith '+ left right)))]
                    [(eq? (token-type next-tok) MINUS-TOKEN)
                     (match MINUS-TOKEN)
                     (let ([right (parse-term)])
                       (loop (list 'expr 'arith '- left right)))]
                    [else left]))))
          
          (define (parse-term)
            (let ([token (peek)])
              (cond
                [(eq? (token-type token) LPAREN-TOKEN)
                 (match LPAREN-TOKEN)
                 (let ([expr (parse-expr)])
                   (match RPAREN-TOKEN)
                   expr)]
                [(eq? (token-type token) ID-TOKEN)
                 (advance)
                 (list 'expr 'id (token-value token))]
                [(eq? (token-type token) NUM-TOKEN)
                 (advance)
                 (list 'expr 'num (token-value token))]
                [else (error "Syntax error in term")])))
          
          (define (parse-stmt)
            (let ((token (peek)))
              (cond
                [(eq? (token-type token) ID-TOKEN) 
                 (let ([id-val (token-value token)])
                   (advance) 
                   (match ASSIGN-TOKEN)
                   (let ([expr (parse-expr)])
                     (match SEMI-TOKEN)
                     (list 'stmt 'assignment id-val expr)))]
                
                [(eq? (token-type token) IF-TOKEN) 
                 (advance)
                 (match LPAREN-TOKEN)
                 (let ([condition (parse-expr)])
                   (match RPAREN-TOKEN)
                   (let ([stmt-list (parse-stmt-list)])
                     (match ENDIF-TOKEN)
                     (list 'stmt 'if condition stmt-list)))]
                
                [(eq? (token-type token) READ-TOKEN) 
                 (advance)
                 (let ([id-token (match ID-TOKEN)])
                   (match SEMI-TOKEN)
                   (list 'stmt 'read (token-value id-token)))]
                
                [(eq? (token-type token) WRITE-TOKEN) 
                 (advance)
                 (let ([expr (parse-expr)])
                   (match SEMI-TOKEN)
                   (list 'stmt 'write expr))]
                
                [else (error (format "Syntax error in statement: ~a" (token-type token)))])))
          
          (define (parse-stmt-list)
            (let ([next-token (peek)])
              (if (or (eq? (token-type next-token) DOLLARDOLLAR-TOKEN)
                      (eq? (token-type next-token) EOF-TOKEN))
                  '()
                  (cons (parse-stmt) (parse-stmt-list)))))
          
          (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
            (let ((tree (parse-stmt-list)))
              (match DOLLARDOLLAR-TOKEN)
              (if (null? tokens-stream)
                  (cons "Accept" tree)
                  (format "Syntax error: unexpected tokens after $$: ~a" 
                         (map token-type tokens-stream))))))
        
        (define (parse-program-text input-text)
          (parse-program (tokenize-input input-text)))
        
        (define (parse-file filename)
          (let ([input-text (port->string (open-input-file filename))])
            (parse-program-text input-text)))

**NOTE: consistent issue with unmatched parentheses and begin/return from DeepSeek generated code, I debugged and moved back to ChatGPT, but somehow lost the token for -**

**CatL:** I am getting error in lines 150-231, "begin (possibly implicit): the last form is not an expression in:"

**ChatGPT:** This usually happens when:
1. A function definition is missing a closing parenthesis.
2. A let, let*, or letrec block does not return a value.
3. A begin block (which may be implicitly created) does not have a valid expression as its last form.
4. A function body is structured incorrectly.

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
        (define EOF-TOKEN 'eof-token)
        
        (define (make-token type value)
          (cons type value))
        
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
          (let loop ()
            (when (and (not (eof-object? (peek))) (whitespace? (peek)))
              (next)
              (loop)))
          (when (eof-object? (peek))
            (make-token EOF-TOKEN #f))
          (let ([c (peek)])
            (cond
              [(equal? c #\;) (next) (make-token SEMI-TOKEN #f)]
              [(equal? c #\() (next) (make-token LPAREN-TOKEN #f)]
              [(equal? c #\)) (next) (make-token RPAREN-TOKEN #f)]
              [(equal? c #\$)
               (next)
               (if (equal? (peek) #\$)
                   (begin (next) (make-token DOLLARDOLLAR-TOKEN #f))
                   (error "Invalid character: standalone $"))]
              [(equal? c #\=) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "==")]
                 [else (make-token ASSIGN-TOKEN #f)])]
              [(letter? c)
               (let ([id (read-while (lambda (c) (or (letter? c) (digit? c))))])
                 (cond
                   [(equal? id "if") (make-token IF-TOKEN #f)]
                   [(equal? id "endif") (make-token ENDIF-TOKEN #f)]
                   [(equal? id "read") (make-token READ-TOKEN #f)]
                   [(equal? id "write") (make-token WRITE-TOKEN #f)]
                   [else (make-token ID-TOKEN (string->symbol id))]))]
              [(digit? c)
               (let ([num (read-while digit?)])
                 (make-token NUM-TOKEN (string->number num)))]
              [else (error (format "Invalid character: ~a" c))])))
        
        (define (tokenize-input input-text)
          (let ([port (open-input-string input-text)])
            (let loop ([tokens '()])
              (let ([token (my-lexer port)])
                (if (eq? (car token) EOF-TOKEN)
                    (reverse tokens)
                    (loop (cons token tokens)))))))
        
        (define tokens-stream '())
        
        (define (peek) (if (null? tokens-stream) (make-token EOF-TOKEN #f) (car tokens-stream)))
        (define (advance) (set! tokens-stream (cdr tokens-stream)))
        (define (match expected-type)
          (let ([token (peek)])
            (if (eq? (car token) expected-type)
                (begin (advance) token)
                (error (format "Syntax error: expected ~a, got ~a" expected-type (car token))))))
        
        (define (parse-expr) (parse-comparison))
        (define (parse-comparison)
          (let ([left (parse-add-sub)])
            (let ([next-tok (peek)])
              (if (eq? (car next-tok) COMPARE-TOKEN)
                  (let ([op (cdr (match COMPARE-TOKEN))] [right (parse-add-sub)])
                    (list 'expr 'compare op left right))
                  left))))
        
        (define (parse-add-sub)
          (let ([left (parse-term)])
            (let loop ([left left])
              (let ([next-tok (peek)])
                (cond
                  [(eq? (car next-tok) PLUS-TOKEN) (match PLUS-TOKEN) (loop (list 'expr 'arith '+ left (parse-term)))]
                  [(eq? (car next-tok) MINUS-TOKEN) (match MINUS-TOKEN) (loop (list 'expr 'arith '- left (parse-term)))]
                  [else left])))))
        
        (define (parse-term)
          (let ([token (peek)])
            (cond
              [(eq? (car token) NUM-TOKEN) (advance) (list 'expr 'num (cdr token))]
              [(eq? (car token) ID-TOKEN) (advance) (list 'expr 'id (cdr token))]
              [(eq? (car token) LPAREN-TOKEN)
               (match LPAREN-TOKEN)
               (let ([expr (parse-expr)])
                 (match RPAREN-TOKEN)
                 expr)]
              [else (error "Syntax error in term")])))  ;; This closing parenthesis was missing.

        
        (define (parse-stmt)
          (let ([token (peek)])
            (cond
              [(eq? (car token) ID-TOKEN) (let ([id (cdr token)]) (advance) (match ASSIGN-TOKEN) (list 'stmt 'assign id (parse-expr)))]
              [(eq? (car token) IF-TOKEN) (advance) (list 'stmt 'if (parse-expr) (parse-stmt-list) (match ENDIF-TOKEN))]
              [(eq? (car token) READ-TOKEN) (advance) (list 'stmt 'read (cdr (match ID-TOKEN)))]
              [(eq? (car token) WRITE-TOKEN) (advance) (list 'stmt 'write (parse-expr))]
              [else (error "Syntax error in statement")]))
        
        (define (parse-stmt-list)
          (if (or (null? tokens-stream) (eq? (car (peek)) DOLLARDOLLAR-TOKEN)) '() (cons (parse-stmt) (parse-stmt-list))))
        
        (define (parse-program tokens)
          (set! tokens-stream tokens)
          (let ([result (parse-stmt-list)])
            (if (null? tokens-stream) (cons "Accept" result) "Syntax error: unexpected tokens")))
        
        (define (parse-program-text input-text) (parse-program (tokenize-input input-text)))
        (define (parse-file filename) (parse-program-text (port->string (open-input-file filename))))

**CatL:** Since "$$" acts as our end terminal, can we remove EOF?

**ChatGPT:** Yes, if $$ is intended to act as the end terminal for your input (instead of EOF), we can remove the EOF-TOKEN and replace the check for EOF with a check for $$.

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
          
          (define (advance-position)
            (if (equal? (peek) #\newline)
                (begin (set! line (+ line 1)) (set! col 1))
                (set! col (+ col 1))))
          
          (let loop ()
            (when (and (not (eof-object? (peek))) (whitespace? (peek)))
              (next)
              (advance-position)
              (loop)))
          
          ;; Check for "$$" as the end of input
          (let ([c (peek)])
            (advance-position)
            (cond
              [(equal? c #\$)
               (if (equal? (peek) #\$)
                   (begin (next) (make-token DOLLARDOLLAR-TOKEN #f line col)) ;; matched `$$`
                   (error (format "Invalid character: $ at line ~a, column ~a" line col)))] ;; single `$` is invalid
              [(equal? c #\;) (next) (make-token SEMI-TOKEN #f line col)]
              [(equal? c #\() (next) (make-token LPAREN-TOKEN #f line col)]
              [(equal? c #\)) (next) (make-token RPAREN-TOKEN #f line col)]
              [(equal? c #\=) 
               (next)
               (cond
                 [(equal? (peek) #\=) (next) (make-token COMPARE-TOKEN "==" line col)]
                 [else (make-token ASSIGN-TOKEN #f line col)])]
              [(equal? c #\>) (next)
               (if (equal? (peek) #\=)
                   (begin (next) (make-token COMPARE-TOKEN ">=" line col))
                   (make-token COMPARE-TOKEN ">" line col))]
              [(equal? c #\<) (next)
               (if (equal? (peek) #\=)
                   (begin (next) (make-token COMPARE-TOKEN "<=" line col))
                   (make-token COMPARE-TOKEN "<" line col))]
              [(equal? c #\+) (next) (make-token PLUS-TOKEN #f line col)]
              [(equal? c #\-) (next) (make-token MINUS-TOKEN #f line col)]
              [(or (equal? c #\-) (digit? c))
               (let* ([neg? (if (equal? c #\-) (begin (next) #t) #f)]
                      [num (read-while digit?)])
                 (make-token NUM-TOKEN (string->number (if neg? (string-append "-" num) num)) line col))]
              [(letter? c)
               (let ([id (read-while (lambda (c) (or (letter? c) (digit? c))))])
                 (cond
                   [(equal? id "if") (make-token IF-TOKEN #f line col)]
                   [(equal? id "endif") (make-token ENDIF-TOKEN #f line col)]
                   [(equal? id "read") (make-token READ-TOKEN #f line col)]
                   [(equal? id "write") (make-token WRITE-TOKEN #f line col)]
                   [else (make-token ID-TOKEN (string->symbol id) line col)]))]
              [else (error (format "Invalid character: ~a at line ~a, column ~a" c line col))])))
        
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
        
        (define (parse-expr) (parse-comparison))
        
        (define (parse-comparison)
          (let ([left (parse-add-sub)])
            (let ([next-tok (peek)])
              (if (eq? (car next-tok) COMPARE-TOKEN)
                  (let ([op (cdr (match COMPARE-TOKEN))] [right (parse-add-sub)])
                    (list 'expr 'compare op left right))
                  left))))
        
        (define (parse-add-sub)
          (let ([left (parse-term)])
            (let loop ([left left])
              (let ([next-tok (peek)])
                (cond
                  [(eq? (car next-tok) PLUS-TOKEN) (match PLUS-TOKEN) (loop (list 'expr 'arith '+ left (parse-term)))]
                  [(eq? (car next-tok) MINUS-TOKEN) (match MINUS-TOKEN) (loop (list 'expr 'arith '- left (parse-term)))]
                  [else left])))))
        
        (define (parse-term)
          (let ([token (peek)])
            (cond
              [(eq? (car token) NUM-TOKEN) (advance) (list 'expr 'num (cdr token))]
              [(eq? (car token) ID-TOKEN) (advance) (list 'expr 'id (cdr token))]
              [(eq? (car token) LPAREN-TOKEN) (match LPAREN-TOKEN) (let ([expr (parse-expr)]) (match RPAREN-TOKEN) expr)]
              [else (error "Syntax error in term")])))
        
        (define (parse-stmt)
          (let ([token (peek)])
            (cond
              [(eq? (car token) ID-TOKEN) (let ([id (cdr token)]) (advance) (match ASSIGN-TOKEN) (list 'stmt 'assign id (parse-expr)))]
              [(eq? (car token) IF-TOKEN) (advance) (list 'stmt 'if (parse-expr) (parse-stmt-list) (match ENDIF-TOKEN))]
              [(eq? (car token) READ-TOKEN) (advance) (list 'stmt 'read (cdr (match ID-TOKEN)))]
              [(eq? (car token) WRITE-TOKEN) (advance) (list 'stmt 'write (parse-expr))]
              [else (error "Syntax error in statement")])))
        
        (define (parse-stmt-list)
          (if (or (null? tokens-stream) (eq? (car (peek)) DOLLARDOLLAR-TOKEN)) '() (cons (parse-stmt) (parse-stmt-list))))
        
        (define (parse-program tokens)
          (set! tokens-stream tokens)
          (let ([result (parse-stmt-list)])
            (if (null? tokens-stream) (cons "Accept" result) "Syntax error: unexpected tokens")))
        
        (define (parse-program-text input-text) (parse-program (tokenize-input input-text)))
        (define (parse-file filename) (parse-program-text (port->string (open-input-file filename))))
        
        (parse-program-text "A = 6; 
        B = 5; 
        X = -2; 
        if (A + B > 10)
           X = A + 2;
        endif; 
        write A + B - X; 
        $$
        ")

**NOTE: I started getting a bunch of errors, tokenize-input: unbound identifier in: tokenize-input, parse-expr is not defined in code, unbound identifier in: parse-stmt-list...But as I prompted ChatGPT to address the errors, it wouldn't update in place, causing parts of the program to be deleted, or updated without the rest of the code. I moved to Claude.**

**CatL:** I'm getting an error message for file1 that shouldn't occur. "Invalid character: $ at line 2, column 3"

**Claude:** The main changes I'd suggest:
1. Make sure the position tracking is correct (pass the character to advance-position)
2. Check for EOF before trying to process tokens
3. Fix the $$ token detection to handle it across lines and with whitespace
4. Make sure line and column counters are updated correctly

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
              [(equal? c #\-) (next) (advance-position c) (make-token MINUS-TOKEN #f curr-line curr-col)]
              
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

**CatL:** The handling of negative numbers is still incorrect.

**Claude:** Let's fix the lexer's handling of negative numbers:

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

**CatL:** Now the comparisons are causing errors.

**Claude:** 

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

**CatL:** endif and if are not parsing correctly.

**Claude:** The problem is that when you use (match ENDIF-TOKEN), it's consuming the endif token but then trying to continue parsing as if it were a statement. However, endif is not a statement by itself but a delimiter for the if block.

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

        (define (parse-stmt-list)
          (if (or (null? tokens-stream) 
                  (eq? (car (peek)) DOLLARDOLLAR-TOKEN)
                  (eq? (car (peek)) ENDIF-TOKEN))  ; Stop when we see endif
              '()
              (let ([stmt (parse-stmt)])
                (when (eq? (car (peek)) SEMI-TOKEN) 
                  (advance)) ; Consume semicolon but don't treat it as a statement
                (cons stmt (parse-stmt-list)))))  ;; Recursively parse more statements

**NOTE: Though the program was now correctly parsing file1, it was missing the missing semicolon in File2. I ran out of free use of Claude, so I went back to ChatGPT.**

**CatL:** My current parser is working well but is not identifying the missing semicolon on line 3.

**ChatGPT:** It does not explicitly enforce semicolon termination for each statement. The issue lies in your parse-stmt-list function:

        (if (eq? (car (peek)) SEMI-TOKEN)
            (advance) ; Consume semicolon
            (error (format "Syntax error: missing semicolon at line ~a, column ~a" 
                           (caddr (peek)) (cadddr (peek)))))

**CatL:** Include != in the comparison tokens.

**ChatGPT:** Add a case for != like this:

        [(equal? c #\!) 
         (next)
         (advance-position c)
         (if (and (not (eof-object? (peek))) (equal? (peek) #\=))
             (begin (next) (advance-position (peek)) (make-token COMPARE-TOKEN "!=" curr-line curr-col))
             (error (format "Invalid character: ! at line ~a, column ~a" curr-line curr-col)))]
