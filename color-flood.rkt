#lang racket/gui

(require bazaar/gui/board
         bazaar/matrix
         bazaar/mutation
         bazaar/cond-let
         bazaar/loop
         bazaar/debug)

(define seed 1428341910 #;(current-seconds))
(displayln (list 'random-seed: seed))
(random-seed seed)

(module+ test
  (require rackunit))

(define n-cells 22)

(define color-names '("red" "blue" "darkgreen" "yellow" "violet" "turquoise" "orange"))
(define (color-name n) (list-ref color-names n))
(define n-colors (length color-names))
(define colors (build-list n-colors values))
(define n-plays 0)
(define mat #f)

(struct player (row col score)
  #:transparent
  #:mutable)
(define player1 (player 0 0 0))
(define player2 (player (- n-cells 1) (- n-cells 1) 0))

(define player1-col (player-col player1))
(define player1-row (player-row player1))
(define player2-col (player-col player2))
(define player2-row (player-row player2))

(define (player-color mat pl)
  (matrix-ref mat (player-row pl) (player-col pl)))

(define (forbidden-colors mat)
  (list (player-color mat player1)
        (player-color mat player2)))

(define (set-mat! m)
  (set! mat m))

(define (reset-game!)
  (set! n-plays 0)
  (set-player-score! player1 0)
  (set-player-score! player2 0)
  (matrix-map!
   mat
   (λ(row col v)
     (random n-colors))))

(define (mat-in-bounds? mat col row)
  (and (>= col 0)
       (>= row 0)
       (< col (matrix-ncols mat))
       (< row (matrix-nrows mat))))

;; Returns the number of cells painted with the same color as the one in (col row)
;; that can be reached for (col row).
(define (mat-score mat col row)
  (define from-color (matrix-ref mat row col))
  (define counted? (make-hash))
  (let loop ([col col] [row row])
    (define key (cons col row))
    (cond [(and (mat-in-bounds? mat col row)
                (= from-color (matrix-ref mat row col))
                (not (hash-ref counted? key #f)))
           (hash-set! counted? key #t)
           (+ 1
              (loop col (+ row 1))
              (loop col (- row 1))
              (loop (+ col 1) row)
              (loop (- col 1) row))]
          [else 0])))

(define (mat-flood! mat col row to-color)
  (define from-color (matrix-ref mat row col))
  (define n-tests 0)
  (when (not (= to-color from-color))
    (let loop ([col col] [row row])
      (set! n-tests (+ 1 n-tests))
      (when (and (mat-in-bounds? mat col row)
                 (= from-color (matrix-ref mat row col)))
        (matrix-set! mat row col to-color)
        (loop col (+ row 1))
        (loop col (- row 1))
        (loop (+ col 1) row)
        (loop (- col 1) row))))
  #;(displayln (list 'n-tests: n-tests))
  (not (= to-color from-color)))

;; Returns the number of cells that can be captured by flooding
(define (test-flood mat col row to-color)
  (define from-color (matrix-ref mat row col))
  (define counted? (make-hash))
  (if (= to-color from-color)
      0
      (let loop ([col col] [row row] [count? #f])
        (cond-let [(not (mat-in-bounds? mat col row)) 0]
                  #:let* ([c    (matrix-ref mat row col)]
                          [c=to (= c to-color)]
                          [key  (cons col row)])
                  [(and (or c=to (= c from-color))
                        (not (hash-ref counted? key #f)))
                   (hash-set! counted? key #t)
                   (+ (if c=to 1 0)
                      (loop col (+ row 1) c=to)
                      (loop col (- row 1) c=to)
                      (loop (+ col 1) row c=to)
                      (loop (- col 1) row c=to))]
                  [else 0]))))


;; Returns the final score after playing a sequence of moves (to-colors).
(define (test-flood* mat col row to-colors)
  (define from-color (matrix-ref mat row col))
  (define counted? (make-hash))
  (if (= (first to-colors) from-color)
      0
      (let loop ([col col] [row row]
                 [from-color from-color] [to-colors to-colors])
        (define key (cons col row))
        (cond-let [(or (not (mat-in-bounds? mat col row))
                       (hash-ref counted? key #f))
                   0]
                  #:let* ([c (matrix-ref mat row col)]
                          [to-color (if (empty? to-colors) -1 (first to-colors))]
                          [c=to (= c to-color)]
                          [next-from-color (if c=to to-color from-color)]
                          [next-to-colors  (if c=to (rest to-colors) to-colors)])
                  [(or c=to (= c from-color))
                   (hash-set! counted? key #t)
                   (+ 1
                      (loop col (+ row 1) next-from-color next-to-colors)
                      (loop col (- row 1) next-from-color next-to-colors)
                      (loop (+ col 1) row next-from-color next-to-colors)
                      (loop (- col 1) row next-from-color next-to-colors))]
                  [(not (empty? to-colors))
                   ; try again with the next color in line
                   (loop col row (first to-colors) (rest to-colors))]
                  [else 0]))))

;; From starting positions (the current frontier), returns the new frontier
;; of cells that have not yet been used.
;; The value associated with from-color is not a list but the number of pixels
;; colored with the specified color.
;; color-frontiers : alist? ; dict of (color . color-frontier-list) 
;; mat-used : matrix? ; must be initialized to #f
;; -> alist? number?; returns the new color-frontier, updated from color-frontiers
;; and the number of cells of the specified color that have been processed.
(define (color-frontier1 mat color-frontiers mat-used color)
  ;; new-frontier: dict of (color . color-frontier-list) of the touched pixels
  (let loop ([frontier (dict-ref color-frontiers color)]
             [new-frontiers (dict-remove color-frontiers color)]
             [changed 0])
    (if (empty? frontier)
        (values new-frontiers changed)
        (let* ([point (first frontier)]
               [col (car point)]
               [row (cdr point)])
          (if (or (not (mat-in-bounds? mat col row))
                  (matrix-ref mat-used row col))
              (loop (rest frontier) new-frontiers changed)
              (let ([c (matrix-ref mat row col)])
                (if (= color c)
                    (begin
                      ; changed the matrix only for the parsed element
                      (matrix-set! mat-used row col #t)
                      (loop (list* (cons (- col 1) row)
                                   (cons col       (- row 1))
                                   (cons col       (+ row 1))
                                   (cons (+ col 1) row)
                                   (rest frontier))
                            new-frontiers
                            (+ changed 1)))
                    (loop (rest frontier)
                          (dict-update new-frontiers c
                                       (λ(f)
                                         (define key (cons col row))
                                         (if (member key f) f (cons key f)))
                                       '())
                          changed))))))))

(define (test-frontier1 col row [frontiers #f] #:names? [names? #f])
  (define from-color (matrix-ref mat row col))
  (define mat-used (make-matrix (matrix-nrows mat) (matrix-ncols mat) #f))
  (define-values (new-frontier changed)
    (color-frontier1 mat (or frontiers (list (list from-color (cons col row))))
                     mat-used
                     from-color))
  (values
   (if names?
       (for/list ([(k v) (in-dict new-frontier)]) (cons (list-ref color-names k) v))
       new-frontier)
   changed))

(define (color-frontier mat col row depth [forbid-colors '()]
                       #:print? [print? #f])
  ; One matrix copy per depth
  (define dmat (build-vector depth (λ(i)(make-matrix (matrix-nrows mat) (matrix-ncols mat) #f))))
  (define mat-used (make-matrix (matrix-nrows mat) (matrix-ncols mat) #f))
  (define from-color (matrix-ref mat row col))
  (let loop ([d depth]
             [from-color from-color]
             [color-frontiers (list (list from-color (cons col row)))]
             [mat-used mat-used]
             [forbid-colors forbid-colors])
    (define mat-used2 (vector-ref dmat (- d 1)))
    (matrix-copy! mat-used2 mat-used)
    #;(disp-vars color-frontiers from-color)
    (define-values (new-color-frontiers changed)
      (color-frontier1 mat color-frontiers mat-used2 from-color))
    #;(disp-vars new-color-frontiers changed)
    (if (or (<= d 1) #;(= changed 0)) ; is (= changed 0) possible?
        changed
        (let ()
          (define-values (best-color best-score)
            (let ([found-colors (remove* forbid-colors (dict-keys new-color-frontiers))])
              (if (empty? found-colors)
                  (values (first (remove* forbid-colors colors)) 0) ; todo: list-choose
                  (for/best > ([color found-colors])
                            (define front (dict-ref new-color-frontiers color))
                            (define score (loop (- d 1) color
                                                (dict-remove new-color-frontiers from-color)
                                                mat-used2 (list from-color)))
                            (when print?
                              (displayln (string-append
                                          (make-string (* 2 (- depth d)) #\space)
                                          "color:" (~a (list-ref color-names color) #:min-width 13)
                                          "  score:" (~a score))))
                            (values color score)))))
          (when print?
            (displayln (string-append
                        (make-string (* 2 (- depth d)) #\space)
                        "best :" (~a (list-ref color-names best-color) #:min-width 13)
                        "  score:" (~a best-score)
                        "  changed:" (~a changed))))
          (if (= d depth)
              best-color
              (+ changed best-score))))))

;; Dynamic programming version to find the best first color to play
;; in the sequence of depth colors
;; forbid-colors: list of colors to forbid for the first move of the sequence
;; TODO: min-max algorithm
(define (best-color/DP mat col row depth [forbid-colors '()]
                       #:print? [print? #f])
  ; One matrix copy per depth
  (define dmat (build-vector depth (λ(i)(make-matrix (matrix-nrows mat) (matrix-ncols mat)))))
  (let loop ([d depth] [mat mat] [forbid-colors forbid-colors])
    (define-values (best-color best-score)
      (for/best > ([color (remove* forbid-colors colors)])
                (define mat2 (vector-ref dmat (- d 1)))
                (matrix-copy! mat2 mat)
                (mat-flood! mat2 col row color)
                (define score
                  (if (<= d 1)
                      (mat-score mat2 col row)
                      (loop (- d 1) mat2 (list color))))
                (when print?
                  (displayln (string-append
                              (make-string (* 2 (- depth d)) #\space)
                              "color:" (~a (list-ref color-names color) #:min-width 13)
                              "  score:" (~a score))))
                (values color score)))
    (when print?
      (displayln (string-append
                  (make-string (* 2 (- depth d)) #\space)
                  " best:" (~a (list-ref color-names best-color) #:min-width 13)
                  "  score:" (~a best-score))))
    (if (= d depth)
        best-color
        best-score)))


(define (play-color to-color depth)
  (when (not (memv to-color (forbidden-colors mat)))
    (mat-flood! mat player1-col player1-row to-color)
    ; Automatic opponent
    (mat-flood! mat player2-col player2-row
                (color-frontier mat player2-col player2-row (+ depth 1)
                                (forbidden-colors mat))
                #;(best-color/DP mat player2-col player2-row depth
                               (forbidden-colors mat))
                #;(best-color mat player2-col player2-row depth
                            (forbidden-colors mat)))
    (++ n-plays)
    (update-scores! player1 player2)))

(define (auto-play)
  (play-color (argmax
               (λ(c)
                 (define n (test-flood mat 0 0 c))
                 (displayln (list 'color: (list-ref color-names c) 'test-flood: n))
                 n)
               colors)))

;; Returns the list of all sequences of elements of lst of the specified depth.
;; If with-rep? is false, it does not include sequences where two adjacent elements are `equal?`.
;; (This can make a huge difference when lst is small. For example if lst is of size 2,
;; the number of possible sequences is always 2, vs 2^depth if with-rep is #t.)
(define (all-seqs lst depth [with-rep? #t])
  (let loop ([ll '(())] [depth depth])
    (if (= depth 0)
        ll
        (loop (for*/list ([l ll]
                          [x lst]
                          #:when (or with-rep?
                                     (empty? l)
                                     (not (equal? x (first l)))))
                (cons x l))
              (- depth 1)))))

(module+ test
  (check-equal? (all-seqs '(a b c) 0) '(()))
  (check-equal? (all-seqs '(a b c) 1) '((a) (b) (c)))
  (check-equal? (all-seqs '(a b c) 2)
                '((a a) (b a) (c a) (a b) (b b) (c b) (a c) (b c) (c c)))
  (check-equal? (all-seqs '(a b c) 2 #f)
                '((b a) (c a) (a b) (c b) (a c) (b c))))

;; TODO: Dynamic programming to make it more efficient
(define (best-color mat col row depth [forbid-colors '()])
  (first (argmax 
          (λ(l)(test-flood* mat col row l))
          (filter
           (λ(l)(not (memv (first l) forbid-colors)))
           (all-seqs colors depth #t)))))

(define (auto-play-depth depth)
  (play-color (best-color mat 0 0 depth)))

(define (update-scores! . players)
  (for ([pl players])
    (set-player-score! pl (mat-score mat (player-col pl) (player-row pl)))))

(define (game-over? mat . players)
  (= (* (matrix-ncols mat) (matrix-nrows mat))
     (apply + (map player-score players))))

;===========;
;=== GUI ===;
;===========;

(define (gui-reset-game!)
  (reset-game!)
  (refresh))

(define (gui-auto-play)
  (define depth (send slider-level get-value))
  #;(auto-play)
  #;(auto-play-depth depth)
  #;(refresh)
  (time
   (for ([i 36])
     (auto-play-depth depth)
     (refresh)
     (sleep/yield 0.1))))

(define (gui-play-color c)
  (play-color c (send slider-level get-value))
  (refresh)
  (when (game-over? mat player1 player2)
    (define d (- (player-score player1) (player-score player2)))
    (message-box "Game over"
                 (cond [(< d 0) "You lose"]
                       [(> d 0) "You win!"]
                       [else "Draw game"])
                 frame)))

(define (refresh)
  (define fc (forbidden-colors mat))
  (for ([cb color-buttons] [c (in-naturals)])
    (send cb enable (not (memv c fc))))
  (msg-n-plays-refresh)
  (send board draw)
  (send frame refresh))

(define (msg-n-plays-refresh)
  (send msg-n-plays set-label (format "Moves: ~a\tPlayer1: ~a\tPlayer2: ~a"
                                      n-plays (player-score player1) (player-score player2)))
  (send msg-n-plays refresh))

(define (on-char k-evt)
  (when (eq? (send k-evt get-key-code) 'release)
    (define chr (send k-evt get-key-release-code))
    (write (list 'char: chr))
    (newline)
    (case chr
      [(#\return)
       (for ([c n-colors])
         (displayln (list 'color: (list-ref color-names c)
                          'test-flood: (test-flood mat 0 0 c))))])))

(define (on-left-up xc yc)
  (define c (board-ref board xc yc))
  (displayln (list 'color: (list-ref color-names c)))
  (gui-play-color c))

(define (on-right-up xc yc)
  (define c (board-ref board xc yc))
  (displayln (list 'color: (list-ref color-names c)
                   'test-flood: (test-flood mat 0 0 c))))

(define frame (new frame% [label "Flood"]))
(define board (new board%
                   [parent frame]
                   [on-char on-char]
                   [on-left-up on-left-up]
                   [on-right-up on-right-up]
                   [num-cell-x n-cells]
                   [stretchable-width #f] [stretchable-height #f]
                   [cell-dx 20]))

(define msg-n-plays (new message% [parent frame] [label ""]
                         [auto-resize #t]))
(define color-button-panel (new horizontal-panel% [parent frame]
                                [stretchable-height #f]
                                [alignment '(center center)]))

(define color-vec
  (for/vector ([c color-names])
    (send board make-cell-pic c)))

(define color-buttons
  (for/list ([c-bmp color-vec] [c colors])
    (new button% [parent color-button-panel] [label c-bmp]
         [callback (λ(bt ev)(gui-play-color c))])))

(define button-panel (new horizontal-panel% [parent frame]
                          [stretchable-height #f]
                          [alignment '(center center)]))
(define button-init (new button% [parent button-panel] [label "Restart Game"]
                         [callback (λ(bt ev)(gui-reset-game!))]))
#;
(define button-auto (new button% [parent button-panel] [label "Auto play"]
                         [callback (λ(bt ev)(gui-auto-play))]))
(define max-level 5)
(define slider-level (new slider% [parent button-panel] [label "Level"]
                          [min-value 1] [max-value max-level] [init-value 1]
                          [min-width (* 40 max-level)]
                          [stretchable-width #f]))

(send board set-cell-pic 
      (λ(i j v)(vector-ref color-vec v)))

(set-mat! (board-get-matrix board))
(gui-reset-game!)

(send frame show #t)
