#lang racket
(provide (all-defined-out))
; this file holds everything necessary to play chess.

; pieces are defined a triples containing: the type of piece, its color and whether it has been moved
(define (piece type color) (list type color #f))

; playing around with recursion while building the starting position
(define (emptyRow n)
  (cond ((= n 0) '())
        (else (cons null (emptyRow (- n 1))))))

(define (pawnRow color n)
  (cond ((= n 0) '())
        (else (cons (piece 'pawn color) (pawnRow color (- n 1))))))

(define (backRow color)
  (list (piece 'rook color) (piece 'knight color) (piece 'bishop color) (piece 'queen color) (piece 'king color) (piece 'bishop color) (piece 'knight color) (piece 'rook color)))

; this here gives us our starting position. we will keep this and every action we do will be performed on it
(define startingBoard
  (list (backRow 'black) (pawnRow 'black 8) (emptyRow 8) (emptyRow 8) (emptyRow 8) (emptyRow 8) (pawnRow 'white 8) (backRow 'white)))

; dictionaries to translate column letters to indices and vice versa
(define fromlet (hash "a" 1 "A" 1 "b" 2 "B" 2 "c" 3 "C" 3 "d" 4 "D" 4 "e" 5 "E" 5 "f" 6 "F" 6 "g" 7 "G" 7 "h" 8 "H" 8))
(define tolet (hash 1 "A" 2 "B" 3 "C" 4 "D" 5 "E" 6 "F" 7 "G" 8 "H"))
; this is used to access specific fields of a board. pos is expected to be a pair in algebraic notation  (letter number)
(define (whatsOn board pos)
  (cond ((not (void? pos))
         (let* ([x (- (hash-ref fromlet (car pos)) 1)]
                [y (- 8 (cdr pos))])
           (list-ref (list-ref board y) x)))))

; updates a square on a board
(define (replaceSquare board at with)
  (let ([x (- (hash-ref fromlet (car at)) 1)]
        [y (- 8 (cdr at))])
   (list-set board y (list-set (list-ref board y) x with))))

; moves a piece to a new square
(define (movePiece board fromto)
  (let ([piece (whatsOn board (car fromto))])
  (cond ((eq? piece '()) (error "cant move a piece that aint there"))
        (else (replaceSquare (replaceSquare board (cadr fromto) (list-set piece 2 #t)) (car fromto) '())))))

; finds the position of the king
(define (isKingB? piece)
  (if (not (empty? piece))
   (and (eq? (car piece) 'king) (eq? (cadr piece) 'black))
   #f))
(define (containsKingB? row)
   (not (false? (index-where row isKingB?))))
(define (isKingW? piece)
  (if (not (empty? piece))
   (and (eq? (car piece) 'king) (eq? (cadr piece) 'white))
   #f))
(define (containsKingW? row)
   (not (false? (index-where row isKingW?))))

(define (whereKing board color)
  (if (eq? color 'white)
      (let* ([y (index-where board containsKingW?)]
             [x (+ 1 (index-where (list-ref board y) isKingW?))])
      (cons (hash-ref tolet x) (- 8 y)))
      (let* ([y (index-where board containsKingB?)]
             [x (+ 1 (index-where (list-ref board y) isKingB?))])
      (cons (hash-ref tolet x) (- 8 y)))))

; calculates resulting position from original position (algebraic) and cartesian move "vector"
(define (newPos orig move)
  (let ([x (+ (hash-ref fromlet (car orig)) (car move))]
        [y (+ (cdr orig) (cdr move))])
     (cond ((and (hash-has-key? tolet x) (member y '(1 2 3 4 5 6 7 8))) (cons (hash-ref tolet x) y)))))

; checks if the king of given color is checked
(define (kingChecked? board color)
  (if (eq? color 'white)
      (not (not (member (whereKing board 'white) (threats board 'black))))
      (not (not (member (whereKing board 'black) (threats board 'white))))))

; which squares are threatened by color?
(define (threats board color)
  (unpad (remove-duplicates (for*/list ([move (successorsHelper2 board color)])
    (cdr move)))))

; calculates all possible moves for a piece. if a move is possible and doesnt leave ones king in check it is legal
(define (possibleMoves board at)
  (let ([type (list-ref (whatsOn board at) 0)]) 
    (cond [(eq? type 'rook)   (legalRookMoves board at)]
          [(eq? type 'knight) (legalKnightMoves board at)]
          [(eq? type 'bishop) (legalBishopMoves board at)]
          [(eq? type 'queen)  (legalQueenMoves board at)]
          [(eq? type 'king)   (legalKingMoves board at)]
          [(eq? type 'pawn)   (legalPawnMoves board at)])))

; checks for legality of moves
(define (legalMoves moves board)
  (filter notVoid? (legalMovesHelper moves board)))

(define (legalMovesHelper moves board)
  (let ([col (list-ref (whatsOn board (caar moves)) 1)])
  (for*/list ([move moves])
    (let ([after (movePiece board move)])
      (cond ((not (kingChecked? after col)) move)))))) 
      

;this one actually works
(define (successors board color)
  (legalMoves (successorsHelper2 board color) board))

; calculates successors of a given board with specified colors turn
(define (successorsHelper2 board color)
  (unpad (filter notVoid? (successorsHelper board color))))

(define (successorsHelper board color)
  (for*/list ([x (hash-values tolet)]
              [y '(1 2 3 4 5 6 7 8)]
              #:when (and (not (empty? (whatsOn board (cons x y)))) (eq? (list-ref (whatsOn board (cons x y)) 1) color)))
      (possibleMoves board (cons x y))))

; cleaing messy results...
(define (notVoid? x) (not (void? x)))
(define (notEmpty? x) (not (empty? x)))
(define (unpad l)
  (if (or (empty? l) (void? l)) '()
    (for*/list ([sublist l]
                [el (filter notEmpty? (filter notVoid? sublist))])
      el)))

; implementing rules for moving rooks and according movegeneration
(define (legalRookMoves board pos)
  (unpad (legalRookMovesHelper board pos)))

(define (legalRookMovesHelper board pos)
  (for/list ([direction '((-1 0) (1 0) (0 -1) (0 1))])
    (for/list ([steps (in-range 1 8)]
         #:final (not (eq? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) '())))
      (let ([new (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))])
        (cond ((not (void? new))
        (cond ((or (empty? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))) (not (eq? (list-ref (whatsOn board pos) 1) (list-ref (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) 1))))
        (list pos (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))))))))))

; implementing rules for moving bishops and according movegeneration
(define (legalBishopMoves board pos)
  (unpad (legalBishopMovesHelper board pos)))

(define (legalBishopMovesHelper board pos)
  (for/list ([direction '((-1 1) (1 1) (-1 -1) (1 -1))])
    (for/list ([steps (in-range 1 8)]
         #:final (not (eq? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) '())))
      (let ([new (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))])
        (cond ((not (void? new))
        (cond ((or (empty? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))) (not (eq? (list-ref (whatsOn board pos) 1) (list-ref (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) 1))))
        (list pos (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))))))))))

; implementing rules for moving queens and according movegeneration
(define (legalQueenMoves board pos)
  (unpad (legalQueenMovesHelper board pos)))

(define (legalQueenMovesHelper board pos)
  (for/list ([direction '((-1 1) (1 1) (-1 -1) (1 -1) (-1 0) (1 0) (0 -1) (0 1))])
    (for/list ([steps (in-range 1 8)]
         #:final (not (eq? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) '())))
      (let ([new (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))])
        (cond ((not (void? new))
        (cond ((or (empty? (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))) (not (eq? (list-ref (whatsOn board pos) 1) (list-ref (whatsOn board (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps)))) 1))))
        (list pos (newPos pos (cons (* (car direction) steps) (* (cadr direction) steps))))))))))))

; implementing rules for moving kings and according movegeneration
(define (legalKingMoves board pos)
  (for/list ([direction (list (cons -1 1) (cons 1 1) (cons -1 -1) (cons 1 -1) (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1))])
      (let ([new (newPos pos direction)])
        (cond ((not (void? new))
        (cond ((or (empty? (whatsOn board new)) (not (eq? (list-ref (whatsOn board pos) 1) (list-ref (whatsOn board new) 1))))
        (list pos new))))))))

(define (getCol board pos)
  (if (not (or (empty? (whatsOn board pos)) (void? pos)))
      (list-ref (whatsOn board pos) 1) 'blue))
(define (hasMoved? board pos)
  (list-ref (whatsOn board pos) 2))

; rules for pawns
(define (legalPawnMoves board pos)
  (let* ([res empty]
         [colHelp (if (eq? (getCol board pos) 'white) 1 -1)]
         [moved (hasMoved? board pos)]
         [firstFree (empty? (whatsOn board (newPos pos (cons 0 colHelp))))])
    (begin
      [cond (firstFree (set! res (list (list pos (newPos pos (cons 0 colHelp))) res)))]
      [cond ((and (not moved)(and firstFree (empty? (whatsOn board (newPos pos (cons 0 (* colHelp 2))))))) (set! res (cons (list pos (newPos pos (cons 0 (* 2 colHelp)))) res)))]
      [cond ((and (not(eq? 'blue (getCol board (newPos pos (cons colHelp colHelp)))))  (not (eq? (getCol board pos) (getCol board (newPos pos (cons colHelp colHelp)))))) (set! res (cons (list pos (newPos pos (cons colHelp colHelp))) res)))]
      [cond ((and (not(eq? 'blue (getCol board (newPos pos (cons (* -1 colHelp) colHelp)))))  (not (eq? (getCol board pos) (getCol board (newPos pos (cons (* -1 colHelp) colHelp)))))) (set! res (cons (list pos (newPos pos (cons (* -1 colHelp) colHelp))) res)))]
      [values res])))       
    

; implementing rules for moving knights
(define knightMoves (list (cons 1 2) (cons 1 -2) (cons -1 2) (cons -1 -2) (cons 2 1) (cons 2 -1) (cons -2 1) (cons -2 -1)))
(define (legalKnightMoves board pos)
  (for/list ([move knightMoves])
    (cond ((pair? (newPos pos move))
           (cond ((or (eq? (whatsOn board (newPos pos move)) empty) (not (eq? (list-ref (whatsOn board pos) 1) (list-ref (whatsOn board (newPos pos move)) 1))))
           (list pos (newPos pos move))))))))

; game over if color has no move
(define (gameOver? board color)
  (empty? (successors board color)))


