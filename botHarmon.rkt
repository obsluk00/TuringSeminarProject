#lang racket
(require "board.rkt")
(provide (all-defined-out))

; heuristic used to evaluate gamestates. simple addition of the pieces relative values (1,3,3,5,9)
(define (evaluatePosition board)
  (cond
    ; check for winner
    [(gameOver? board 'white) +inf.0]
    [(gameOver? board 'black) -inf.0]
    [else
     (let ([h 0])
       (for* ([x (hash-values tolet)]
              [y '(1 2 3 4 5 6 7 8)]
              #:when (not (empty? (whatsOn board (cons x y)))))
         (let ([piece (whatsOn board (cons x y))])
           (if (eq? 'white (list-ref piece 1))
               (set! h (+ h (hash-ref pieceValue (list-ref piece 0))))
               (set! h (- h (hash-ref pieceValue (list-ref piece 0))))                         
         )))
       (values h))]))

(define pieceValue (hash 'pawn 1 'knight 3 'bishop 3 'rook 5 'queen 9 'king 0))
   
; defines minimax search with alpha beta pruning
(define (alphaBetaMain board color depth)
  (alphaBeta board depth -inf.0 +inf.0 color))

(define evalCounter 0)

(define (alphaBeta board depth alpha beta color)
  (if (or (gameOver? board color) (eq? depth 0))
      (cons (evaluatePosition board) empty)
      (let ([bestMove empty]
            [v (if (eq? color 'white) -inf.0 +inf.0)])
        (begin
          [for ([move (successors board color)]
                #:unless (empty? move))
                #:final (or (>= (car (alphaBeta (movePiece board move) (- depth 1) alpha beta (inverseColor color))) beta) (<= (car (alphaBeta (movePiece board move) (- depth 1) alpha beta (inverseColor color))) alpha))
            (let ([res (alphaBeta (movePiece board move) (- depth 1) alpha beta (inverseColor color))])
              (begin
                [cond ((or (and (eq? color 'white) (> (car res) v)) (and (eq? color 'black) (< (car res) v)))
                       (begin [set! v (car res)]
                              [set! bestMove move]))]                  
                [if (eq? color 'white)
                  (if (>= v beta)  (set! bestMove empty) (set! alpha (max alpha v)))
                  (if (<= v alpha) (set! bestMove empty) (set! beta  (min beta v)))]))]
          [cons v bestMove]))))

(define (inverseColor color) (if (eq? color 'white) 'black 'white))


