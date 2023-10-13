#lang racket
;;Author: Uri Jared Gopar Morales
;;Author: Maria Fernanda Moreno Gomez_A01708653
;;Date: 09/04/23

;;Librerias ocupadas
(require 2htdp/batch-io) ;;dentro de esta libreria se encuentran todas las que vamos a ocupar(string,file,match)


;; Define las categorías léxicas de C#
(define keywords
  '(
   "abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" "checked" "class" "const" "continue" "decimal" "default" "delegate" "do" "double" "else" "enum" "event" "explicit" "extern" "false" "finally" "fixed" "float" "for" "foreach" "goto" "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long" "namespace" "new" "null" "object" "operator" "out" "override" "params" "private" "protected" "public" "readonly" "ref" "return" "sbyte" "sealed" "short" "sizeof" "stackalloc" "static" "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint" "ulong" "unchecked" "unsafe" "ushort" "using" "virtual" "void" "volatile" "while"
   ))
;;Define todos los operadores
(define operators
  '(
   "+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" "?" ":" ";" "," "." "++" "--" "&&" "||" "==" "!=" "<=" ">=" "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<=" ">>=" "=>" "??"
   ))
;;Define los (),[],{}
(define separador
  '(";" "," "." "(" ")" "[" "]" "{" "}" "<" ">" ":" "::" "..." "=>" "??"))


;;Define los token
(define (tokens-class token)
  (cond
    ;;Se ocupa #rx para comentar que es una expresion regular en racket si no racket no reconoce la expresion
    [(regexp-match #rx"//."  token) "comentario"]
    [(regexp-match #rx"/*.*/" token) "comentario"]
    [(member token operators) "operador"]
    [(member token keywords) "keyword"]
    [(member token separador) "separadores"]
    [(regexp-match? #rx"^\".*\"$" token) "cadena"]
    [(regexp-match? #rx"^[0-9x]+$" token) "literales"]
    [(regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token) "identificadores"]
    [else #f]
    )
)

;;Ponerle el color al token
(define(sub-token token token-tipo)
  (cond
    [(equal? token-tipo "keyword")
     (string-append "<span class=\"keyword\">" token "</span>")]

    [(equal? token-tipo "operador")
     (string-append "<span class=\"operador\">" token "</span>")]

    [(equal? token-tipo "separadores")
     (string-append "<span class=\"separadores\">" token "</span>")]

    [(equal? token-tipo "cadena")
     (string-append "<span class=\"cadena\">" token "</span>")]

    [(equal? token-tipo "literales")
     (string-append "<span class=\"literales\">" token "</span>")]

    [(equal? token-tipo "comentario")
     (string-append "<span class=\"comentario\">" token "</span>")]

    [(equal? token-tipo "identificadores")
     (string-append "<span class=\"identificadores\">" token "</span>")]

    [else token]
    ))


;; Token line
(define (token-l line openb)
    (define word '())
    (define list-line '())
    (define tokenized-line '())
    (define open-quotes #f)

    (define possible-line-comment #f)
    (define openl #f)

    (define chars (regexp-split #px"" line))

     (for/last ([char chars])
      (when (and (eq? char (last chars)) (or openl openb))
        (set! list-line (append list-line (list word))))
       
      (cond
        [openb (set! word (append word (list char)))]
        [(regexp-match #rx"#" char) (set! word (append word (list char)))]
        [(regexp-match? #rx"[a-zA-Z0-9_]" char)
         (set! word (append word (list char)))]
        [(regexp-match #px"/" char) 
          (cond 
            [possible-line-comment 
              ((lambda () 
                (set! possible-line-comment #f)
                (set! openl #t)
                (set! word (append word (list char)))))]
            [else 
              ((lambda () 
                (set! possible-line-comment #t)
                (set! word (append word (list char)))))])]

        [openl (set! word (append word (list char)))]

        ;; Match en cadenas
        [(regexp-match? #px"\"" char)
         (cond
           [open-quotes 
            ((lambda ()
               (set! open-quotes #f)
               (set! word (append word (list char)))
               (set! list-line (append list-line (list word)))
               (set! word '())))]
            [else ((lambda () 
              (set! open-quotes #t)
              (set! word (append word (list char)))))])]

        [open-quotes (set! word (append word (list char)))]

        ;; Match for operadores
        [(regexp-match? #px"[\\.\\,\\;\\(\\)\\{\\}\\[\\]\\=\\+\\-\\*\\/\\%\\>\\<\\:]" char)
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())
            (set! word (append word (list char)))
            (set! list-line (append list-line (list word)))
            (set! word '())))]

        ;; Match para otros caracteres
        [else
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())))])
    )

    (define tokens (map (lambda (x) (string-join x "")) list-line))

    (for ([token tokens])
        (define token-type (tokens-class token))
        (when openb (set! token-type "comentario"))
        (if token-type
            (set! tokenized-line (append tokenized-line 
                                 (list (sub-token token token-type))))
            (set! tokenized-line (append tokenized-line (list token)))
        )
    )

    tokenized-line
)
;; Tokenizes a given file

(define (tokenize-file file-name)
    (let ((in-port (read-file file-name)))
        (let loop ((tokens '()))

        (let ((line (read-line in-port)))
            (if (eof-object? line)
                (reverse tokens)
                (loop (append tokens (token-l line))))
            )
        )
    )
)


;; Escribe una lista de lista

(define (write-file file-name lines)
    (define out (open-output-file file-name))

    (for ([line lines])
        (displayln line out)
    )

    (close-output-port out)
)

;;Leer archivo
(define (read-file file-name)
    (open-input-file file-name)
)

;; HTML
  (define html-header
    "<!DOCTYPE html>
     <html>
     <head>
     <meta charset=\"utf-8\">
     <title>Resaltador en Racket</title>
     <link rel=\"stylesheet\" href=\"style.css\" >
     </head>
     <body>"
    )
  (define html-footer
    "</body>
    </html>")
(define input-file "Ejemplo_Prueba.cs")

;; output file
(define output-file "Output.html")

  ;; Main function
  (define (run input-file output-file)
  (define input-lines (file->lines input-file))
  (define output-port (open-output-file output-file))
  
  (write-string html-header output-port)

  (define openb #f)

  (for-each (lambda (line)
              (write-string (string-append "<pre>") output-port)

              (when (not openb) 
                  (set! openb (regexp-match? #px"/\\*" line)))
                  
              (define tokens (token-l line openb))
              (define formatted-line (string-join tokens " "))

              (when openb
                  (set! openb (not (regexp-match? #px"\\*/" line))))

              (write-string (string-append formatted-line " ") output-port)
              (write-string (string-append "</pre>\n") output-port))
            input-lines)
  
  (write-string html-footer output-port)
  (close-output-port output-port))


(time(run input-file output-file))
                                                                                             
