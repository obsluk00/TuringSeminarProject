#lang racket
(require racket/gui/base "board.rkt" "botHarmon.rkt")

; define window
(define setup (new frame% [label "BotHarmon Client"]
                          ))


(define squareEdgeLength 50)
(define (drawSquares dc)
  (send dc set-brush (make-object color% 100 100 100) 'solid)
  (for* ([row (in-range 8)] [column (in-range 8)]
                            #:when (even? (+ row column)))
                                    (send dc draw-rectangle (* (+ 1 row) squareEdgeLength) (* (+ 1 column) squareEdgeLength) squareEdgeLength squareEdgeLength)))

; define board display
(define (displayBoard board dc)
  (begin [send dc clear]
         [drawSquares dc]
         [for* ([row (in-range 8)] [column (in-range 8)]
                            #:unless (empty? (list-ref (list-ref board column) row)))
           (let ([piece (list-ref (list-ref board column) row)])
             (send dc draw-text (pieceToText piece) (* (+ 1 row) squareEdgeLength) (* (+ 1 column) squareEdgeLength)))]))

(define pieceType  (hash  'queen "Q" 'king "K" 'knight "N" 'rook "R" 'bishop "B" 'pawn "P"))
(define pieceColor (hash 'white "W" 'black "B"))
(define (pieceToText piece)
  (string-append (hash-ref pieceType (car piece)) (hash-ref pieceColor (cadr piece))))

(define game
  (new frame% [label "BotHarmon Game"]
               [width 500]
               [height 650]))

(define canvas (new editor-canvas% [parent game]
                                   [style '(no-hscroll no-vscroll)]
                                   [horizontal-inset 25]
                                   [vertical-inset 25]))
(define gameDc (send canvas get-dc))
(define lossscreen (new dialog%
                        [label "you lost"]
                        [parent game]))
(define winscreen (new dialog%
                        [label "you won"]
                        [parent game]))
(define drawscreen (new dialog%
                        [label "you drew"]
                        [parent game]))

(define msg (new message% [parent setup]
                          [label "Which color would you like to play as?"]))
; define interactive elements
(define panel (new horizontal-panel% [parent setup]))
(define panel2 (new horizontal-panel% [parent setup]))
(define chosenColor 'black)

(new button% [parent panel]
             [label "White"]
             [callback (lambda (button event)
                         (begin [set! chosenColor 'white]
                                [send msg set-label "You will play white"]))])

(new button% [parent panel]
             [label "Black"]
             [callback (lambda (button event)
                         (begin [set! chosenColor 'black]
                                [send msg set-label "You will play black"]))])

(new button% [parent panel2]
             [label "Start Game"]
             [callback (lambda (button event)
                         (begin [send setup show #f]
                                [send game show #t]
                                [turn startingBoard 'white]))])

; call coresponds to color making a move via outsourced functions. recursiveness replaces a while true loop
(define (turn board color)
  (let ([boardAfter board]
        [opp (inverseColor color)])
    (begin
      [displayBoard boardAfter gameDc]
      [if (eq? chosenColor color)  ; performs move based on user or bot input
          (set! boardAfter (yourTurn board))
          (set! boardAfter (botsTurn board))]
      [if (gameOver? boardAfter opp) ; evaluates if the game is over and continues accordingly
          (gameOver color)
          (turn boardAfter opp)])))

      
(define (gameOver winner)
  (cond [(eq? winner chosenColor) (send winscreen show #t)]
        [(eq? winner (inverseColor chosenColor)) (send lossscreen show #t)]
        [else (send drawscreen show #t)]))

(define (botsTurn board)
  (movePiece board (cdr (alphaBetaMain board (inverseColor chosenColor) 6))))

(define (yourTurn board)
  (let ([moves (successors board chosenColor)])
    (begin [offer moves]
           [movePiece board (list-ref moves (car lastChoice))])))

; updates list of moves offered to user
(define (offer moves)
  (begin
    [send listBox set (stringed moves)]
    [send offerDialog show #t]))

(define (stringed moves)
  (for/list ([move moves])
    (move->string move)))

(define (move->string move)
  (string-append (pos->string (car move)) (pos->string (cadr move))))

(define (pos->string pos)
  (string-append (car pos)(number->string (cdr pos))))

(define (string->move input)
  (let ([help (string->list input)])
    (list (cons (string (car help)) (string->number (string (cadr help)))) (cons (string (caddr help)) (string->number (string (cadddr help)))))))

(define offerDialog
  (new dialog% [label "Choose your move"]
               [parent game]))


(define lastChoice empty)

(new button% [parent offerDialog]
             [label "Confirm"]
             [callback (lambda (button event)
                         (begin
                             [set! lastChoice (send listBox get-selections)]
                             [send offerDialog show #f]))])

(define listBox
  (new list-box% [label "Legal Moves"]
                 [choices empty]
                 [parent offerDialog]
                 [style '(single)]))

(send setup show #t)