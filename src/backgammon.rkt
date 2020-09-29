#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))


;; === data definitions

(define-type Player (U 'Black 'White))

(define-struct OccupiedPoint
  ([color : Player]
   [count : Integer]))

(define-type Point (U OccupiedPoint 'EmptyPoint))

(define-struct Board
  ([points : (Listof Point)]
   [black-bar : Integer]
   [white-bar : Integer]
   [black-off : Integer]
   [white-off : Integer]))

(define-struct Style
  ([checker-radius : Integer]
   [spacing : Integer]
   [black-checker : (Integer -> Image)]
   [white-checker : (Integer -> Image)]
   [dark-point : (Integer Boolean -> Image)]
   [light-point : (Integer Boolean -> Image)]
   [background : (Integer Integer -> Image)]
   [label : (String -> Image)]
   [black-die : (Integer Integer -> Image)]
   [white-die : (Integer Integer -> Image)]))

(define-struct Game
  ([board : Board]
   [turn : Player]
   [moves : (Listof Integer)]))

(define-struct PointNum
  ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'Nowhere))

(define-struct World
  ([game : Game]
   [style : Style]
   [first-click : BoardLoc]
   [black-die-value1 : Integer]
   [black-die-value2 : Integer]
   [white-die-value1 : Integer]
   [white-die-value2 : Integer]
   [history : (Listof Game)]))

(define-struct (Some S) 
  ([value : S]))

(define-type (Option S) 
  (U 'None (Some S)))


;; === board setup

(: draw-bc : Integer -> Image)
;; draw a single black checker, given a radius
(define (draw-bc r)
  (circle r "solid" "black"))

(: draw-wc : Integer -> Image)
;; draw a single white checker, given a radius
(define (draw-wc r)
  (circle r "solid" "white"))

(: draw-dp : Integer Boolean -> Image)
;; draw a dark point
;; given a radius and the direction the triangle is pointing at
(define (draw-dp r b)
  (if b
      (rotate 180 (triangle/sss (sqrt (+ (* r r) (* r r 100)))
                                (sqrt (+ (* r r) (* r r 100)))
                                (* r 2) "solid" "brown"))
      (triangle/sss (sqrt (+ (* r r) (* r r 100)))
                    (sqrt (+ (* r r) (* r r 100)))
                    (* r 2) "solid" "brown")))

(: draw-lp : Integer Boolean -> Image)
;; draw a light point
;; given a radius and the direction the triangle is pointing at
(define (draw-lp r b)
  (if b
      (rotate 180 (triangle/sss (sqrt (+ (* r r) (* r r 100)))
                                (sqrt (+ (* r r) (* r r 100)))
                                (* r 2) "solid" "peru"))
      (triangle/sss (sqrt (+ (* r r) (* r r 100)))
                    (sqrt (+ (* r r) (* r r 100)))
                    (* r 2) "solid" "peru")))
          
(: draw-label : String -> Image)
;; draw a label, given the number of a point in string type
(define (draw-label s)
  (text s 10 "orange"))

(: draw-full-checkers : (Integer -> Image) Integer Integer -> Image)
;; draw the checker image when there are over five checkers on a point
;; given a function that takes in a radius and outputs an image of a checker,
;;       an initial index 1, and a radius
(define (draw-full-checkers f n r)
  (if (> n 5)
      empty-image
      (above (f r) (draw-full-checkers f (add1 n) r))))

(: draw-less-checkers : (Integer -> Image) Integer Integer -> Image)
;; draw the checker image when there are no more than five checkers on a point
;; given a function that takes in a radius and outputs an image of a checker,
;;       an initial index 1, and a radius
(define (draw-less-checkers f n r)
  (if (< n 0)
      empty-image
      (above (f r) (draw-less-checkers f (sub1 n) r))))

(: draw-checkers : Point Integer -> Image)
;; draw the checkers on a point, given the point and radius
(define (draw-checkers p r)
  (match p
    ['EmptyPoint empty-image]
    [(OccupiedPoint 'Black x)
     (if (> x 5)
         (overlay (draw-label (number->string x))
                  (draw-full-checkers draw-bc 1 r))
         (draw-less-checkers draw-bc (sub1 x) r))]
    [(OccupiedPoint 'White x)
     (if (> x 5)
         (overlay (draw-label (number->string x))
                  (draw-full-checkers draw-wc 1 r))
         (draw-less-checkers draw-wc (sub1 x) r))]))

(: draw-point : (Listof Point) Integer Integer -> Image)
;; draws the triangle and label for one point, given its position on the board
(define (draw-point ps i r)
  (cond
    [(> i 23) empty-image]
    [(and (odd? i) (> i 11))
     (overlay/align "middle" "top"
                    (draw-checkers (list-ref ps i) r) (draw-lp r #f))]
    [(and (odd? i) (> i -1))
     (overlay/align "middle" "bottom"
                    (draw-checkers (list-ref ps i) r) (draw-lp r #t))]
    [(and (even? i) (> i 11))
     (overlay/align "middle" "top"
                    (draw-checkers (list-ref ps i) r) (draw-dp r #f))]
    [else
     (overlay/align "middle" "bottom"
                    (draw-checkers (list-ref ps i) r) (draw-dp r #t))]))

(: draw-bottom : (Listof Point) Integer Integer Integer Integer -> Image)
;; draw points aligned the bottom of the board
;; given an index, radius, spacing, pointing direction
(define (draw-bottom ps i n r sp)
  (if (> i n)
      empty-image
      (beside/align "bottom" (draw-bottom ps (add1 i) n r sp)
                    (square sp "solid" "khaki")
                    (draw-point ps i r))))

(: draw-top : (Listof Point) Integer Integer Integer Integer -> Image)
;; draw points aligned the top of the board
;; given an index, radius, spacing, pointing direction
(define (draw-top ps i n r sp)
  (if (> i n)
      empty-image
      (beside/align "top" 
                    (square sp "solid" "khaki")
                    (draw-point ps i r)
                    (draw-top ps (add1 i) n r sp))))

(: guarantee-byte (Integer -> Byte))
;; guarantee that the input in text is of byte type
(define (guarantee-byte n)
  (if (byte? n) n (error "guarantee-byte: failure")))

(: draw-white-bar : Integer Integer Integer -> Image)
;; draw checkers on the white bar
;; given the number of checkers, radius, and spacing
(define (draw-white-bar wb r sp)
  (overlay/align
   "middle" "top"
   (overlay (text (number->string wb) (guarantee-byte r) "orange")
            (draw-wc r))
   (rectangle (* 2 r) (/ (+ (* r 10 2) (+ (* 4 r) (* 3 sp))) 2)
              "solid" "chocolate")))


(: draw-black-bar : Integer Integer Integer -> Image)
;; draw checkers on the black bar
;; given the number of checkers, radius, and spacing
(define (draw-black-bar bb r sp)
  (overlay/align
   "middle" "bottom"
   (overlay (text (number->string bb) (guarantee-byte r) "orange")
            (draw-bc r))
   (rectangle (* 2 r) (/ (+ (* r 10 2) (+ (* 4 r) (* 3 sp))) 2)
              "solid" "chocolate")))

(: draw-bar : Integer Integer Integer Integer -> Image)
;; draw the bar
;; given radius and number of black checkers and white checkers on the bar
(define (draw-bar bb wb r sp)
  (match* (bb wb)
    [(0 0) (rectangle (* 2 r) (+ (* r 10 2) (+ (* 4 r) (* 3 sp)))
                      "solid" "chocolate")]
    [(0 wb)
     (above (rectangle (* 2 r) (/ (+ (* r 10 2) (+ (* 4 r) (* 3 sp))) 2)
                       "solid" "chocolate")
            (draw-white-bar wb r sp))]
    [(bb 0)
     (above (draw-black-bar bb r sp)
            (rectangle (* 2 r) (/ (+ (* r 10 2) (+ (* 4 r) (* 3 sp))) 2)
                       "solid" "chocolate"))]
    [(bb wb) (above (draw-black-bar bb r sp) (draw-white-bar wb r sp))]))

(: draw-offb : Integer Integer -> Image)
;; draw black checkers that are borne off
;; given the number of black checkers that are borne off and the radius
(define (draw-offb wo r)
  (match wo
    [0 empty-image]
    [_ (overlay (text (number->string wo) (guarantee-byte r) "orange")
                (draw-bc r))]))

(: draw-offw : Integer Integer -> Image)
;; draw white checkers that are borne off
;; given the number of white checkers that are borne off and the radius
(define (draw-offw bo r)
  (match bo
    [0 empty-image]
    [_ (overlay (text (number->string bo) (guarantee-byte r) "orange")
                (draw-wc r))]))

(: die1 : Integer String -> Image)
;; draw a dot on a die
;; given the radius of the die and the color of the dot
(define (die1 r c)
  (circle (/ r 7) "solid" c))

(: die2 : Integer String -> Image)
;; draw two dots on a die
;; given the radius of the die and the color of the dot
(define (die2 r c)
  (beside (circle (/ r 7) "solid" c)
          (rectangle (/ r 3) 0 "outline" (color 0 0 0 0))
          (circle (/ r 7) "solid" c)))

(: dot-space : Integer -> Image)
;; produce a spacing between the dots, given the radius of the die
(define (dot-space r)
  (rectangle 0 (/ r 3) "outline" (color 0 0 0 0)))

(: draw-number : Integer Integer String -> Image)
;; produce image of the dots on a die
;; given the radius, the number shown on die, color
(define (draw-number r n c)
  (match n
    [0 (die1 r c)]
    [1 (die2 r c)]
    [2 (rotate 135 (beside (die1 r c)
                           (rectangle (/ r 3) 0 "outline" (color 0 0 0 0))
                           (die1 r c)
                           (rectangle (/ r 3) 0 "outline" (color 0 0 0 0))
                           (die1 r c)))]
    [3 (above (die2 r c) (dot-space r) (die2 r c))]
    [4 (above (die2 r c) (dot-space r) (die1 r c) (dot-space r) (die2 r c))]
    [5 (above (die2 r c) (dot-space r) (die2 r c) (dot-space r) (die2 r c))]))

(: draw-bd : Integer Integer -> Image)
;; draw a black die, given its radius and its value
(define (draw-bd r n)
  (if (= n -1) (square (* 2 r) "solid" "khaki")
      (overlay (draw-number r n "white")
               (circle r "solid" "brown")
               (square (* 2 r) "solid" "black"))))

(: draw-wd : Integer Integer -> Image)
;; draw a white die, given its radius and its value
(define (draw-wd r n)
  (if (= n -1) (square (* 2 r) "solid" "khaki")
      (overlay (draw-number r n "black")
               (circle r "solid" "white")
               (square (* 2 r) "solid" "whitesmoke"))))

(: draw-bg : Integer Integer -> Image)
;; draw the main background, given a radius and a spacing
(define (draw-bg r sp)
  (rectangle (+ (* 2 (+ (* r 6 2) (* 7 sp))) (* 2 r))
             (+ (* r 10 2) (+ (* 4 r) (* 3 sp)))
             "solid" "khaki"))

(: draw-board : Style Board -> Image)
;; draw the full board image
;; given a style and a board
(define (draw-board s b)
  (match* (s b)
    [((Style r sp bc wc dp lp bg label bd wd) (Board ps bb wb bo wo))
     (beside
      (overlay
       (overlay (draw-bar bb wb r sp)
                (above (beside/align "top"
                                     (draw-top ps 12 17 r sp)
                                     (square (+ (* 2 r) sp) "solid" "khaki")
                                     (draw-top ps 18 23 r sp)
                                     (square sp "solid" "khaki"))
                       (square (+ (* 4 r) (* 3 sp)) "solid" "khaki")
                       (beside/align "bottom"
                                     (draw-bottom ps 6 11 r sp)
                                     (square (+ (* 2 r) sp) "solid" "khaki")
                                     (draw-bottom ps 0 5 r sp)
                                     (square sp "solid" "khaki"))))
       (bg r sp))
      (above (overlay (draw-offb bo r)
                      (rectangle (* 2 r) (* r 5)"solid" "chocolate"))
             (rectangle (* 2 r) (+ (* 14 r) (* 3 sp)) "solid" "chocolate")
             (overlay (draw-offw wo r)
                      (rectangle (* 2 r) (* r 5) "solid" "chocolate"))))]))


;; === move operations

(: click-helper : Integer Integer Integer Integer Integer -> ClickLoc)
;; produces a ClickLoc that describes where the user clicked
;; given the x and y coordinates of a mouse click, the spacing,
;;       and an inital index 1
(define (click-helper x y r sp i)
  (cond
    [(< i 7)
     (if (and (< (+ (* 14 r) (* 7 sp) (* sp (- 7 i)) (* r 2 (- 6 i)))
                 x (+ (* 14 r) (* 7 sp) (* sp (- 7 i)) (* r 2 (- 7 i))))
              (> y (+ (* 10 r) (* 4 r) (* 3 sp))))
         (PointNum i) (click-helper x y r sp (add1 i)))]
    [(< i 13)
     (if (and (< (+ (* sp (- 13 i)) (* r 2 (- 12 i)))
                 x (+ (* sp (- 13 i)) (* r 2 (- 13 i))))
              (> y (+ (* 10 r) (* 4 r) (* 3 sp))))
         (PointNum i) (click-helper x y r sp (add1 i)))]
    [(< i 19)
     (if (and (< (+ (* sp (- i 12)) (* r 2 (- i 13)))
                 x (+ (* sp (- i 12)) (* r 2 (- i 12))))
              (< y (* 10 r)))
         (PointNum i) (click-helper x y r sp (add1 i)))]
    [(< i 25)
     (if (and (< (+ (* sp (- i 18)) (* r 2 (- i 19)) (* 7 sp) (* r 14))
                 x (+ (* sp (- i 18)) (* r 2 (- i 18)) (* 7 sp) (* r 14)))
              (< y (* 10 r)))
         (PointNum i) (click-helper x y r sp (add1 i)))]
    [(and (< (+ (* 4 r) (* 3.5 sp)) x (+ (* 8 r) (* 3.5 sp)))
          (< (+ (* 11 r) (* 1.5 sp)) y (+ (* 13 r) (* 1.5 sp))))
     'WhiteDice]
    [(and (< (+ (* 18 r) (* 10.5 sp)) x (+ (* 22 r) (* 10.5 sp)))
          (< (+ (* 11 r) (* 1.5 sp)) y (+ (* 13 r) (* 1.5 sp))))
     'BlackDice]
    [(and (< (+ (* 12 r) (* 7 sp)) x (+ (* 14 r) (* 7 sp)))
          (< (+ (* r 10) (* 1.5 sp)) y (+ (* 12 r) (* 1.5 sp))))
     'BlackBar]
    [(and (< (+ (* 12 r) (* 7 sp)) x (+ (* 14 r) (* 7 sp)))
          (< (+ (* r 12) (* 1.5 sp)) y (+ (* 14 r) (* 1.5 sp))))
     'WhiteBar]
    [(and (< (+ (* 26 r) (* 14 sp)) x (+ (* 28 r) (* 14 sp)))
          (< y (+ (* 12 r) (* 1.5 sp))))
     'BlackOff]
    [(and (< (+ (* 26 r) (* 14 sp)) x (+ (* 28 r) (* 14 sp)))
          (> y (+ (* 12 r) (* 1.5 sp))))
     'WhiteOff]
    [else 'Nowhere]))
(check-expect (click-helper 15 5 10 10 1) (PointNum 13))
(check-expect (click-helper 372 200 10 10 1) (PointNum 1))
(check-expect (click-helper 341 10 10 10 1) (PointNum 23))
(check-expect (click-helper 225 10 10 10 1) (PointNum 19))
(check-expect (click-helper 8 5 10 10 1) 'Nowhere)
(check-expect (click-helper 401 126 10 10 1) 'BlackOff)

(: click-where : Style Integer Integer -> ClickLoc)
;; produces a ClickLoc that describes where the user clicked
;; takes in a Style, and the x and y coordinates of a mouse click
(define (click-where s x y)
  (match* (s x y)
    [((Style r sp _ _ _ _ _ _ _ _) x y)
     (click-helper x y r sp 1)]))
(check-expect (click-where test-style 320 135) 'BlackDice)
(check-expect (click-where test-style 191 130) 'BlackBar)
(check-expect (click-where test-style 199 138) 'WhiteBar)
(check-expect (click-where test-style 200 135) 'Nowhere)
(check-expect (click-where test-style 225 10) (PointNum 19))

(: moves : Integer Integer -> (Listof Integer))
;; create the list of moves in Game
;; given two dice rolls
(define (moves v1 v2)
  (if (= v1 v2)
      (make-list 4 (add1 v1))
      (list (add1 v1) (add1 v2))))
(check-expect (moves 0 1) '(1 2))
(check-expect (moves 1 1) '(2 2 2 2))

(: distance : BoardLoc BoardLoc -> Integer)
;; takes in an origin and a destination, and returns the "distance" of that move
(define (distance ol dl)
  (match* (ol dl)
    [((PointNum oi) (PointNum di)) (- di oi)]
    [('BlackBar (PointNum di)) di]
    [('WhiteBar (PointNum di)) (- di 25)]
    [((PointNum oi) 'BlackOff) (- 25 oi)]
    [((PointNum oi) 'WhiteOff) (- 0 oi)]
    [(_ _) (error "illegitimate move")]))
(check-expect (distance (PointNum 1) (PointNum 4)) 3)
(check-expect (distance 'WhiteBar (PointNum 21)) -4)
(check-expect (distance (PointNum 22) 'BlackOff) 3)
(check-expect (distance (PointNum 3) 'WhiteOff) -3)

(: in-list? : Integer (Listof Integer) -> Boolean)
;; check if a roll is in the moves, given a roll number and the moves
(define (in-list? roll rolls)
  (ormap (lambda ([rs : Integer]) (= roll rs)) rolls))
(check-expect (in-list? 12 '(1 3 4)) #f)
(check-expect (in-list? 3 '(1 3 4)) #t)

(: remove-m : Integer (Listof Integer) -> (Listof Integer))
;; remove a move from the moves list, given a roll number and the moves
(define (remove-m roll rolls)
  (match rolls
    [(cons hd tl) (if (= roll hd) tl (cons hd (remove-m roll tl)))]))
(check-expect (remove-m 1 '(2 3 1 5)) '(2 3 5))

(: legal-move-black-point? : Game BoardLoc BoardLoc -> Boolean)
;; check if a move from an original point to a destination point is legal
;; given the player of the turn and the opponent player
(define (legal-move-black-point? g ol dl)
  (match* (g ol dl)
    [((Game (Board ps bb wb bo wo) t ms) (PointNum oi) (PointNum di))
     (if (= oi di) #f
         (match* ((list-ref ps (sub1 oi)) (list-ref ps (sub1 di)))
           [('EmptyPoint _) #f]
           [((OccupiedPoint 'Black _) (OccupiedPoint 'White 1)) #t]
           [((OccupiedPoint 'Black _) (OccupiedPoint 'Black _)) #t]
           [((OccupiedPoint 'Black _) 'EmptyPoint) #t]
           [(_ _) #f]))]
    [((Board ps bb wb bo wo) 'BlackBar (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'White 1) #t]
       [(OccupiedPoint 'Black _) #t]
       ['EmptyPoint #t]
       [_ #f])]
    [(_ _ _) #f]))
(check-expect (legal-move-black-point? test-game (PointNum 1) (PointNum 2)) #t)
(check-expect (legal-move-black-point? test-game (PointNum 1) (PointNum 6)) #f)

(: legal-move-white-point? : Game BoardLoc BoardLoc -> Boolean)
;; check if a move from an original point to a destination point is legal
;; given the player of the turn and the opponent player
(define (legal-move-white-point? g ol dl)
  (match* (g ol dl)
    [((Game (Board ps bb wb bo wo) t ms) (PointNum oi) (PointNum di))
     (if (= oi di) #f
         (match* ((list-ref ps (sub1 oi)) (list-ref ps (sub1 di)))
           [('EmptyPoint _) #f]
           [((OccupiedPoint 'White _) (OccupiedPoint 'Black 1)) #t]
           [((OccupiedPoint 'White _) (OccupiedPoint 'White _)) #t]
           [((OccupiedPoint 'White _) 'EmptyPoint) #t]
           [(_ _) #f]))]
    [((Board ps bb wb bo wo) 'BlackBar (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'Black 1) #t]
       [(OccupiedPoint 'WHite _) #t]
       ['EmptyPoint #t]
       [_ #f])]
    [(_ _ _) #f]))
(check-expect (legal-move-white-point? test-game (PointNum 6) (PointNum 1)) #f)
(check-expect (legal-move-white-point? test-game (PointNum 6) (PointNum 2)) #t)

(: take (All (A) (Integer (Listof A) -> (Listof A))))
;; take n items from the front of the list
;; ex: (take 3 '(a b c d e f)) -> '(a b c)
;; no error if too many items are requested
(define (take n xs)
  (cond
    [(<= n 0) '()]
    [(empty? xs) '()]
    [else (cons (first xs)
                (take (sub1 n) (rest xs)))]))
(check-expect (take 3 '(a b c d e)) '(a b c))
(check-expect (take 0 '(a b c)) '())
(check-expect (take 99 '(x y z)) '(x y z))

(: drop (All (A) (Integer (Listof A) -> (Listof A))))
;; drop n items from the front of the list
;; ex : (drop 3 '(a b c d e f)) -> '(d e f)
;; no error if too many items are requested
(define (drop n xs)
  (cond
    [(<= n 0) xs]
    [(empty? xs) '()]
    [else (drop (sub1 n) (rest xs))]))
(check-expect (drop 3 '(a b c d e f)) '(d e f))
(check-expect (drop 99 '(a b c)) '())
(check-expect (drop 0 '(a b c)) '(a b c))

(: bc-on-point : Point -> Integer)
;; count the number of black checkers on a point, given a point
(define (bc-on-point p)
  (match p
    [(OccupiedPoint 'Black n) n]
    [_ 0]))
(check-expect (bc-on-point (OccupiedPoint 'Black 4)) 4)
(check-expect (bc-on-point (OccupiedPoint 'White 4)) 0)

(: wc-on-point : Point -> Integer)
;; count the number of white checkers on a point, given a point
(define (wc-on-point p)
  (match p
    [(OccupiedPoint 'White n) n]
    [_ 0]))
(check-expect (wc-on-point 'EmptyPoint) 0)

(: start-borne-off? : Board Player -> Boolean)
;; see if the player can start to move checkers to borne-off area
;; given current board and current turn's player
(define (start-borne-off? b p1)
  (match* (b p1)
    [((Board ps bb wb bo wo) 'Black)
     (= 15 (+ bo (foldr + 0 (map bc-on-point (drop 18 ps)))))]
    [((Board ps bb wb bo wo) 'White)
     (= 15 (+ wo (foldr + 0 (map wc-on-point (take 6 ps)))))]))
(check-expect (start-borne-off? off-test-board 'White) #t)
(check-expect (start-borne-off? off-test-board 'Black) #f)

(: highest-white : (Listof Point) Integer -> Integer)
;; determine the highest point number of the white checkers on home board
;; given the list of white home board points, and an index to track position
(define (highest-white w-home-ps i)
  (match w-home-ps
    ['() (error "highest-white: borne-off not started or game has ended")]
    [(cons hd tl)
     (match (first w-home-ps)
       ['EmptyPoint (highest-white tl (sub1 i))]
       [(OccupiedPoint 'Black _) (highest-white tl (sub1 i))]
       [(OccupiedPoint 'White _) i])]))
(check-expect (highest-white (reverse (take 6 (Board-points off-test-board))) 6)
              1)

(: lowest-black : (Listof Point) Integer -> Integer)
;; determine the lowest point number of the black checkers on home board
;; given the list of black home board points, and an index to track position
(define (lowest-black b-home-ps i)
  (match b-home-ps
    ['() (error "lowest-black: borne-off not started or game has ended")]
    [(cons hd tl)
     (match (first b-home-ps)
       ['EmptyPoint (lowest-black tl (add1 i))]
       [(OccupiedPoint 'White _) (lowest-black tl (add1 i))]
       [(OccupiedPoint 'Black _) i])]))

(: max-move : (Listof Integer) -> Integer)
;; given moves, find the largest one
(define (max-move ms)
  (foldr max 0 ms))
(check-expect (max-move '(1 2 3 4)) 4)

(: checkers-in-bar? : Board Player -> Boolean)
;; see if the player has checkers on the bar
;; given current board and current turn's player
(define (checkers-in-bar? b p1)
  (match p1
    ['Black (not (= (Board-black-bar b) 0))]
    ['White (not (= (Board-white-bar b) 0))]))
(check-expect (checkers-in-bar? test-board 'Black) #t)

(: legal-move-wb? : Game BoardLoc BoardLoc -> Boolean)
;; determine if it is legal to move checkers from white bar to a location
;; given current board, original location, and destination location
(define (legal-move-wb? g ol dl)
  (match* (g ol)
    [((Game (Board ps bb wb bo wo) t ms) 'WhiteBar)
     (match dl
       [(PointNum di)
        (match (list-ref ps (sub1 di))
          ['EmptyPoint #t]
          [(OccupiedPoint 'Black 1) #t]
          [(OccupiedPoint 'White _) #t]
          [_ #f])]
       [_ #f])]
    [(_ _) #f]))
(check-expect (legal-move-wb? test-game 'WhiteBar (PointNum 24)) #t)

(: legal-move-bb? : Game BoardLoc BoardLoc -> Boolean)
;; determine if it is legal to move checkers from black bar to a location
;; given current board, original location, and destination location
(define (legal-move-bb? g ol dl)
  (match* (g ol)
    [((Game (Board ps bb wb bo wo) t ms) 'BlackBar)
     (match dl
       [(PointNum di)
        (match (list-ref ps (sub1 di))
          ['EmptyPoint #t]
          [(OccupiedPoint 'White 1) #t]
          [(OccupiedPoint 'Black _) #t]
          [_ #f])]
       [_ #f])]
    [(_ _) #f]))
(check-expect (legal-move-bb? test-game 'BlackBar (PointNum 2)) #t)
    
(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;; take in the current game situation and a proposed move
;; and evaluate it for legality
(define (legal-move? g ol dl)
  (local {(: d : Integer) (define d (distance ol dl))}
    (if (in-list? (abs d) (Game-moves g))
        (match* (g (Game-turn g))
          [((Game b t ms) 'Black)
           (if (checkers-in-bar? b 'Black)
               (match ol
                 ['BlackBar (legal-move-bb? g ol dl)]
                 [_ #f])
               (if (negative? d) #f
                   (match ol
                     [(or 'BlackOff 'WhiteOff 'Nowhere) #f]
                     [(PointNum oi)
                      (match dl
                        ['BlackOff (start-borne-off? b 'Black)]
                        [(PointNum di)
                         (legal-move-black-point? g ol dl)]
                        [_ #f])])))]
          [((Game b t ms) 'White)
           (if (checkers-in-bar? b 'White)
               (match ol
                 ['WhiteBar (legal-move-wb? g ol dl)]
                 [_ #f])
               (if (positive? d) #f
                   (match ol
                     [(or 'BlackOff 'WhiteOff 'Nowhere) #f]
                     [_
                      (match dl
                        ['WhiteOff (start-borne-off? b 'White)]
                        [(PointNum di)
                         (legal-move-white-point? g ol dl)]
                        [_ #f])])))])
        (if (empty? (Game-moves g)) #f
            (match* (g (Game-turn g) ol dl)
              [((Game b t ms) 'Black (PointNum oi) 'BlackOff)
               (and (start-borne-off? b 'Black)
                    (< d (max-move (Game-moves g)))
                    (= oi (lowest-black (drop 18 (Board-points b)) 19)))]
              [((Game b t ms) 'White (PointNum oi) 'WhiteOff)
               (and (start-borne-off? b 'White)
                    (< d (max-move (Game-moves g)))
                    (= oi (highest-white (reverse (take 6 (Board-points b)))
                                         6)))]
              [(_ _ _ _) #f])))))
(check-expect (legal-move? test-game (PointNum 19) 'BlackOff) #f)
(check-expect (legal-move? test-game (PointNum 1) (PointNum 3)) #f)
(check-expect (legal-move? (Game initial-board 'Black '(1 2 3 4))
                           (PointNum 1) (PointNum 3))
              #t)
(check-expect (legal-move? (Game initial-board 'White '(1 2 3 4))
                           (PointNum 6) (PointNum 5))
              #t)
(check-expect (legal-move? (Game initial-board 'White '(1 2 3 4))
                           (PointNum 6) (PointNum 8))
              #f)
(check-expect (legal-move? (initial-game 0 2) (PointNum 6) (PointNum 5)) #t)
(check-expect (legal-move? (Game off-test-board 'White '(1))
                           (PointNum 1) 'WhiteOff) #t)
(check-expect (legal-move? test-bar-game 'BlackBar (PointNum 1)) #f)
(check-expect (legal-move? test-game 'BlackBar (PointNum 3)) #t)

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
(define (replace-at i x xs)
  (match xs
    ['() '()]
    [(cons hd tl)
     (if (positive? i)
         (cons hd (replace-at (sub1 i) x tl))
         (cons x tl))]))
(check-expect (replace-at 0 'Z '(a b c)) '(Z b c))
(check-expect (replace-at 1 'Z '(a b c)) '(a Z c))

(: move-white-bar : Board BoardLoc -> Board)
;; move a checker from the white bar to a point
;; given a board and the destination location
(define (move-white-bar b dl)
  (match* (b dl)
    [((Board ps bb wb bo wo) (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'Black 1)
        (Board (replace-at (sub1 di) (OccupiedPoint 'White 1) ps)
               (add1 bb) (sub1 wb) bo wo)]
       [(OccupiedPoint 'Black n) (error "illegitimate move")]
       [(OccupiedPoint 'White n)
        (Board (replace-at (sub1 di) (OccupiedPoint 'White (add1 n)) ps)
               bb (sub1 wb) bo wo)]
       ['EmptyPoint
        (Board (replace-at (sub1 di) (OccupiedPoint 'White 1) ps)
               bb (sub1 wb) bo wo)])]
    [(_ _) (error "illegitimate move")]))

(: move-black-bar : Board BoardLoc -> Board)
;; move a checker from the white bar to a point
;; given a board and the destination location
(define (move-black-bar b dl)
  (match* (b dl)
    [((Board ps bb wb bo wo) (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'White 1)
        (Board (replace-at (sub1 di) (OccupiedPoint 'Black 1) ps)
               (sub1 bb) (add1 wb) bo wo)]
       [(OccupiedPoint 'White n) (error "illegitimate move")]
       [(OccupiedPoint 'Black n)
        (Board (replace-at (sub1 di) (OccupiedPoint 'Black (add1 n)) ps)
               (sub1 bb) wb bo wo)]
       ['EmptyPoint
        (Board (replace-at (sub1 di) (OccupiedPoint 'Black 1) ps)
               (sub1 bb) wb bo wo)])]
    [(_ _) (error "illegitimate move")]))

(: move-off-point : (Listof Point) Player Integer Integer -> (Listof Point))
;; move a checker off a point
;; given the list of points in Board, the checker color,
;;       the number of checkers on that point, and the location of that point
(define (move-off-point ps p n oi)
  (replace-at (sub1 oi) (if (= n 1) 'EmptyPoint (OccupiedPoint p (sub1 n))) ps))
(check-expect (move-off-point (Board-points test-board) 'Black 2  1)
              (list (OccupiedPoint 'Black 1) 'EmptyPoint 'EmptyPoint
                    'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 4)
                    'EmptyPoint (OccupiedPoint 'White 1) 'EmptyPoint
                    'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                    (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint
                    'EmptyPoint (OccupiedPoint 'Black 1) 'EmptyPoint
                    (OccupiedPoint 'Black 3) 'EmptyPoint 'EmptyPoint
                    'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)))

(: move-black-point : Board Integer BoardLoc -> Board)
;; move one checker off a point to another point
;; given the Board, index of the original point location,
;;       and the destination location
(define (move-black-point b oi dl)
  (match* (b (list-ref (Board-points b) (sub1 oi)) dl)
    [((Board ps bb wb bo wo) (OccupiedPoint 'Black n) (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'White 1)
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'Black 1) ps) 'Black n oi)
               bb (add1 wb) bo wo)]
       [(OccupiedPoint 'White n2) (error "illegitimate move")]
       ['EmptyPoint
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'Black 1) ps) 'Black n oi)
               bb wb bo wo)]
       [(OccupiedPoint 'Black n2)
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'Black (add1 n2)) ps)
                'Black n oi)
               bb wb bo wo)]
       [_ (error "illegitimate move")])]))

(: move-white-point : Board Integer BoardLoc -> Board)
;; move one checker off a point to another point
;; given the Board, index of the original point location,
;;       and the destination location
(define (move-white-point b oi dl)
  (match* (b (list-ref (Board-points b) (sub1 oi)) dl)
    [((Board ps bb wb bo wo) (OccupiedPoint 'White n) (PointNum di))
     (match (list-ref ps (sub1 di))
       [(OccupiedPoint 'Black 1)
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'White 1) ps) 'White n oi)
               (add1 bb) wb bo wo)]
       [(OccupiedPoint 'Black n2) (error "illegitimate move")]
       ['EmptyPoint
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'White 1) ps) 'White n oi)
               bb wb bo wo)]
       [(OccupiedPoint 'White n2)
        (Board (move-off-point
                (replace-at (sub1 di) (OccupiedPoint 'White (add1 n2)) ps)
                'White n oi)
               bb wb bo wo)]
       [_ (error "illegitimate move")])]))

(: move-to-bar-off : Board Player Integer BoardLoc -> Board)
;; move one checker off a point to bar or borne-off area
;; given the Board, the color of the checker,
;;       the index of the original point location, and the destination location
(define (move-to-bar-off b p oi dl)
  (match b
    [(Board ps bb wb bo wo)
     (match (list-ref ps (sub1 oi))
       [(OccupiedPoint p n)
        (match dl
          ['BlackBar
           (if (symbol=? 'Black p)
               (Board (move-off-point ps p n oi) (add1 bb) wb bo wo)
               (error "illegitimate move"))]
          ['WhiteBar
           (if (symbol=? 'Black p)
               (error "illegitimate move")
               (Board (move-off-point ps p n oi) bb (add1 wb) bo wo))]
          ['BlackOff
           (if (symbol=? 'Black p)
               (Board (move-off-point ps p n oi) bb wb (add1 bo) wo)
               (error "illegitimate move"))]
          ['WhiteOff
           (if (symbol=? 'Black p)
               (error "illegitimate move")
               (Board (move-off-point ps p n oi) bb wb bo (add1 wo)))]
          [_ (error "illegitimate move")])])]))

(: apply-move : Game BoardLoc BoardLoc -> Game)
;; given a board, an origin location, and a destination location
;; produces a new board with the move applied
(define (apply-move g ol dl)
  (if (legal-move? g ol dl)
      (match g
        [(Game b t ms)
         (match ol
           ['BlackBar (Game (move-black-bar b dl) 'Black
                            (remove-m (abs (distance ol dl)) ms))]
           ['WhiteBar (Game (move-white-bar b dl) 'White
                            (remove-m (abs (distance ol dl)) ms))]
           [(PointNum oi)
            (match (list-ref (Board-points b) (sub1 oi))
              [(OccupiedPoint 'Black n)
               (match dl
                 [(PointNum di)
                  (Game (move-black-point b oi dl) 'Black
                        (remove-m (abs (distance ol dl)) ms))]
                 ['BlackOff
                  (if (< (abs (distance ol dl)) (max-move (Game-moves g)))
                      (Game (move-to-bar-off b 'Black oi dl) 'Black
                            (remove-m (max-move ms) ms))
                      (Game (move-to-bar-off b 'Black oi dl) 'Black
                            (remove-m (abs (distance ol dl)) ms)))]
                 [(or 'WhiteOff 'BlackBar 'WhiteBar)
                  (Game (move-to-bar-off b 'Black oi dl) 'Black
                        (remove-m (abs (distance ol dl)) ms))])]
              [(OccupiedPoint 'White n)
               (match dl
                 [(PointNum di)
                  (Game (move-white-point b oi dl) 'White
                        (remove-m (abs (distance ol dl)) ms))]
                 ['WhiteOff
                  (if (< (abs (distance ol dl)) (max-move (Game-moves g)))
                      (Game (move-to-bar-off b 'White oi dl) 'White
                            (remove-m (max-move ms) ms))
                      (Game (move-to-bar-off b 'White oi dl) 'White
                            (remove-m (abs (distance ol dl)) ms)))]
                 [(or 'BlackOff 'WhiteOff 'BlackBar 'WhiteBar)
                  (Game (move-to-bar-off b 'White oi dl) 'White
                        (remove-m (abs (distance ol dl)) ms))])])]
           [_ (error "illegitimate move")])])
      (error "illegitimate move")))
(check-error (apply-move test-game (PointNum 2) (PointNum 1))
             "illegitimate move")
(check-error (apply-move test-game (PointNum 1) (PointNum 1))
             "illegitimate move")
(check-error (apply-move test-game 'BlackOff 'BlackOff)
             "illegitimate move")

(: player-point? : Board PointNum Player -> Boolean)
;; check if the point has the player's checkers on it
;; given a Board, the point location, and the player who's in turn
(define (player-point? b ol p1)
  (match (list-ref (Board-points b) (sub1 (PointNum-num ol)))
    [(OccupiedPoint p1 _) #t]
    [_ #f]))

(define point-list : (Listof Integer) (build-list 24 add1))
;; define a list that represents the index of the point list

(: available-from-point?-1 : Game Integer (Listof Integer) -> Boolean)
;; given a Game, a roll number, a list of point indexes
;; determine if there are available moves from a point with a given move
(define (available-from-point?-1 g move list)
  (match* (g list)
    [(_ '()) #f]
    [((Game b p ms) (cons hd tl))
     (if (player-point? b (PointNum hd) p)
         (match p
           ['Black
            (cond
              [(and (= (+ hd move) 25) (start-borne-off? b p)) #t]
              [(< (+ hd move) 25)
               (if (legal-move? g (PointNum hd) (PointNum (+ hd move))) #t
                   (available-from-point?-1 g move tl))]
              [(> (+ hd move) 25)
               (if
                (and (start-borne-off? b p)
                     (legal-move?
                      g
                      (PointNum (lowest-black (drop 18 (Board-points b))
                                              19)) 'BlackOff))
                #t (available-from-point?-1 g move tl))]
              [else (available-from-point?-1 g move tl)])]
           ['White
            (cond
              [(and (= (- hd move) 0) (start-borne-off? b p)) #t]
              [(> (- hd move) 0)
               (if (legal-move? g (PointNum hd) (PointNum (- hd move))) #t
                   (available-from-point?-1 g move tl))]
              [(< (- hd move) 0)
               (if
                (and (start-borne-off? b p)
                     (legal-move?
                      g
                      (PointNum (highest-white
                                 (reverse (take 6 (Board-points b))) 6))
                      'WhiteOff))
                   #t (available-from-point?-1 g move tl))]
              [else (available-from-point?-1 g move tl)])])
         (available-from-point?-1 g move tl))]))
(check-expect (available-from-point?-1 (Game initial-board 'White '(2 5))
                                       2 point-list) #t)

(: available-from-point? : Game (Listof Integer) -> Boolean)
;; given a list of moves and the current game
;; determine if there are available moves from points
(define (available-from-point? g ms)
  (match ms
    ['() #f]
    [(cons hd tl) (if (available-from-point?-1 g hd point-list) #t
                      (available-from-point? g tl))]))
(check-expect (available-from-point? test-game '(2 3)) #f)

(: available-from-bar? : Game (Listof Integer) -> Boolean)
;; determine if checkers on bar can be removed
;; given the current game and moves
(define (available-from-bar? g ms)
  (match* (ms (Game-turn g))
    [('() _) #f]
    [((cons f r) 'Black)
     (if (legal-move? g 'BlackBar (PointNum f))
         #t (available-from-bar? g r))]
    [((cons f r) 'White)
     (if (legal-move? g 'WhiteBar (PointNum (- 25 f)))
         #t (available-from-bar? g r))]))
(check-expect (available-from-bar? test-game '(2 3)) #t)
(check-expect (available-from-bar? test-bar-game '(2 2 2 2)) #f)

(: available-moves? : Game -> Boolean)
;; determine if the player whose turn it is has any remaining moves they can
;; make, with the available dice rolls
(define (available-moves? g)
  (match g
    [(Game b p ms)
     (local
       {(: available-helper : Board Player (Listof Integer) -> Boolean)
        (define (available-helper b p ms)
          (match ms
            ['() #f]
            [(cons f r)
             (if (checkers-in-bar? b p)
                 (available-from-bar? g ms)
                 (available-from-point? g ms))]))}
       (available-helper b p ms))]))
(check-expect (available-moves? (Game off-test-board 'White '(1 2))) #t)
(check-expect (available-moves? (Game off-test-board 'White '(3 2))) #t)
(check-expect (available-moves? (Game initial-board 'White '(1 1))) #t)
(check-expect (available-moves? test-bar-game) #f)

;; === click operation

(: draw-hl : Style Board BoardLoc -> Image)
;; draw highlight and the background
;; the highlight is implemented in the board background
;; given a Style, Board, and the highlight location
(define (draw-hl s b l)
  (match s
    [(Style r sp bc wc dp lp bg label bd wd)
     (match l
       ['BlackBar (place-image (circle r "outline" "green")
                               (+ (* 13 r) (* 7 sp)) (+ (* 11 r) (* 1.5 sp))
                               (draw-board s b))]
       ['WhiteBar (place-image (circle r "outline" "green")
                               (+ (* 13 r) (* 7 sp)) (+ (* 13 r) (* 1.5 sp))
                               (draw-board s b))]
       [(PointNum i)
        (cond
          [(< 0 i 7)
           (place-image (rectangle (* r 2) (* 10 r) "outline" "green")
                        (+ (* 14 r) (* 7 sp) (* sp (- 7 i)) (* r 2 (- 6.5 i)))
                        (+ (* 15 r) (* 4 r) (* 3 sp))
                        (draw-board s b))]
          [(< 6 i 13) (place-image
                       (rectangle (* r 2) (* 10 r) "outline" "green")
                       (+ (* sp (- 13 i)) (* r 2 (- 12.5 i)))
                       (+ (* 15 r) (* 4 r) (* 3 sp))
                       (draw-board s b))]
          [(< 12 i 19) (place-image
                        (rectangle (* r 2) (* 10 r) "outline" "green")
                        (+ (* sp (- i 12)) (* r 2 (- i 12.5)))
                        (* 5 r)
                        (draw-board s b))]
          [else (place-image
                 (rectangle (* r 2) (* 10 r) "outline" "green")
                 (+ (* sp (- i 12)) (* r 2 (- i 12.5)) sp (* r 2))
                 (* 5 r)
                 (draw-board s b))])]
       [_ (draw-board s b)])]))

(: draw : World -> Image)
;; produce world image from a given World
(define (draw w)
  (match w
    [(World g s c1 bv1 bv2 wv1 wv2 gs)
     (match s
       [(Style r sp _ _ _ _ _ _ draw-bd draw-wd)
        (overlay
         (if (game-over? g)
             (text (string-append "Winner: " (symbol->string (winner g)))
                   (guarantee-byte (* 4 r)) "red")
             empty-image)
         (place-image (beside (draw-wd r wv1) (draw-wd r wv2)
                              (rectangle (+ (* 10 r) (* 7 sp)) 0
                                         "outline" (color 0 0 0 0))
                              (draw-bd r bv1) (draw-bd r bv2))
                      (+ (* 13 r) (* 7 sp)) (+ (* 12 r) (* 1.5 sp))
                      (draw-hl s (Game-board g) c1)))])]))

(: apply-move-world : World BoardLoc -> World)
;; apply a move to the World
;; given the original world, the location of the destination
(define (apply-move-world w c2)
  (match w
    [(World g s c1 bv1 bv2 wv1 wv2 gs)
     (if
      (legal-move? g c1 c2)
      (match g
        [(Game b t ms)
         (World (apply-move g c1 c2) s 'Nowhere bv1 bv2 wv1 wv2 (cons g gs))])
      w)]))

(: game-over? : Game -> Boolean)
;; determine if the game is over, given current game
(define (game-over? g)
  (match g
    [(Game (Board ps bb wb bo wo) _ _)
     (or (= bo 15) (= wo 15))]))
     
(: winner : Game -> Player)
;; determines the winner, given current game
(define (winner g)
  (if (game-over? g)
      (match g
        [(Game (Board ps bb wb bo wo) _ _)
         (if (= bo 15) 'Black 'White)])
      (error "the game is not over")))

(: click : World Integer Integer Mouse-Event -> World)
;; produce a new World as a click reaction
;; given the original world, the x and y coordinates of the click,
;;       and the mouse action
(define (click w x y e)
  (match* (w e)
    [((World g s c1 bv1 bv2 wv1 wv2 gs) "button-down")
     (if (game-over? g) w
         (match g
           [(Game b t ms)
            (match c1
              ['Nowhere
                 (match (click-where s x y)
                   ['BlackDice
                    (cond
                      [(available-moves? g) w]
                      [(and (not (available-moves? g)) (symbol=? t 'White))
                       (local
                         {(define bv3 : Integer (random 6))
                          (define bv4 : Integer (random 6))}
                         (World (Game b 'Black (moves bv3 bv4))
                                s c1 bv3 bv4 wv1 wv2 (cons g gs)))]
                      [else w])]
                   ['WhiteDice
                    (cond
                      [(available-moves? g) w]
                      [(and (not (available-moves? g)) (symbol=? t 'Black))
                       (local
                         {(define wv3 : Integer (random 6))
                          (define wv4 : Integer (random 6))}
                         (World (Game b 'White (moves wv3 wv4))
                                s c1 bv1 bv2 wv3 wv4 (cons g gs)))]
                      [else w])]
                   ['BlackBar
                    (if (symbol=? 'White t) w
                        (if (and (available-moves? g)
                                 (not (= (Board-black-bar b) 0)))
                            (World g s 'BlackBar bv1 bv2 wv1 wv2 gs) w))]
                   ['WhiteBar
                    (if (symbol=? 'Black t) w
                        (if (and (available-moves? g)
                                 (not (= (Board-white-bar b) 0)))
                            (World g s 'WhiteBar bv1 bv2 wv1 wv2 gs) w))]
                   [(PointNum i)
                    (match (list-ref (Board-points b) (sub1 i))
                      ['EmptyPoint w]
                      [(OccupiedPoint p _) 
                       (if (and (available-moves? g) (symbol=? p t)
                                (not (checkers-in-bar? b t)))
                           (World g s (PointNum i) bv1 bv2 wv1 wv2 gs)
                           w)])]
                   [_ w])]
              [(PointNum i1)
               (match (click-where s x y)
                 [(PointNum i2)
                  (if (= i1 i2)
                      (World g s 'Nowhere bv1 bv2 wv1 wv2 gs)
                      (match (list-ref (Board-points b) (sub1 i2))
                        [(OccupiedPoint p 1)
                         (apply-move-world w (PointNum i2))]
                        [(OccupiedPoint p n)
                         (if (symbol=? p t)
                             (apply-move-world w (PointNum i2))
                             w)]
                        [_ (apply-move-world w (PointNum i2))]))]
                 ['WhiteOff
                  (if (and (symbol=? 'White t) (start-borne-off? b t))
                      (apply-move-world w 'WhiteOff) w)]
                 ['BlackOff
                  (if (and (symbol=? 'Black t) (start-borne-off? b t))
                      (apply-move-world w 'BlackOff) w)]
                 [_ w])]
              ['WhiteBar
               (match (click-where s x y)
                 ['WhiteBar (World g s 'Nowhere bv1 bv2 wv1 wv2 gs)]
                 [(PointNum i2)
                  (match (list-ref (Board-points b) (sub1 i2))
                    ['WhiteBar (World g s 'Nowhere bv1 bv2 wv1 wv2)]
                    [(OccupiedPoint 'Black 1)
                     (apply-move-world w (PointNum i2))]
                    [(OccupiedPoint 'Black n) w]
                    [_ (apply-move-world w (PointNum i2))])]
                 [_ w])]
              ['BlackBar
               (match (click-where s x y)
                 ['BlackBar (World g s 'Nowhere bv1 bv2 wv1 wv2 gs)]
                 [(PointNum i2)
                  (match (list-ref (Board-points b) (sub1 i2))
                    [(OccupiedPoint 'White 1)
                     (apply-move-world w (PointNum i2))]
                    [(OccupiedPoint 'White n) w]
                    [_ (apply-move-world w (PointNum i2))])]
                 [_ w])])]))]
    [(_ _) w]))


;; === undo operation

(: undo-dice? : World -> Boolean)
;; determine if dice values need to be changed for an undo action
;; given the current world
(define (undo-dice? w)
  (if (symbol=? (Game-turn (first (World-history w)))
                (Game-turn (World-game w)))
      #f #t))

(: fb : Game -> Boolean)
;; determine if a game is black player's turn
(define (fb g)
  (symbol=? 'Black (Game-turn g)))
(check-expect (fb test-game) #t)

(: fw : Game -> Boolean)
;; determine if a game is white player's turn
(define (fw g)
  (symbol=? 'White (Game-turn g)))
(check-expect (fw test-game) #f)

(: opponent : Player -> Player)
;; find the opponent of a given player
(define (opponent p)
  (if (symbol=? 'Black p) 'White 'Black))
(check-expect (opponent 'Black) 'White)

(: drop-last-turn : (Listof Game) Player -> (Listof Game))
;; drop histories of the last turn in the history list
;; given current history and current player
(define (drop-last-turn gs p)
  (match gs
    ['() '()]
    [(cons f r)
     (if (symbol=? (opponent p) (Game-turn f))
         (drop-last-turn r p) gs)]))

(: current-most-recent-i : (Listof Game) Integer Player -> (Option Integer))
;; find the index of the most recent move by the current turn's player
;; given current history list, an index to keep track of the position,
;;       abd current turn's player
(define (current-most-recent-i gs i p)
  (match gs
    ['() 'None]
    [(cons hd tl)
     (if (symbol=? (opponent p) (Game-turn hd))
         (Some i)
         (current-most-recent-i tl (add1 i) p))]))

(: find-moves : World -> (Listof Integer))
;; find the moves needed to return dice values from last turn
;; given current world
(define (find-moves w)
  (match w
    [(World g s c1 bv1 bv2 wv1 wv2 gs)
     (match
         (current-most-recent-i (drop-last-turn gs (Game-turn g))
                                0 (Game-turn g))
       ['None (Game-moves (list-ref gs (sub1 (length gs))))]
       [(Some i)
        (Game-moves (list-ref (drop-last-turn gs (Game-turn g)) (sub1 i)))])]))

(: first-turn? : Game (Listof Game) -> Boolean)
;; determine if it is the first turn of the round
;; given current game status and history
(define (first-turn? g gs)
  (match (Game-turn g)
    ['White (empty? (filter fw gs))]
    ['Black (empty? (filter fb gs))]))

(: second-turn? : Game (Listof Game) -> Boolean)
;; determine if it is the second turn of the round
;; given current game status and history
(define (second-turn? g gs)
  (match (Game-turn g)
    ['White
     (and (= (length (filter fw gs)) 3)
          (symbol=? 'White (Game-turn (list-ref gs (sub1 (length gs))))))]
    ['Black
     (and (= (length (filter fb gs)) 3)
          (symbol=? 'Black (Game-turn (list-ref gs (sub1 (length gs))))))]))


;; === universe setup

(define initial-board : Board
  ;; define board for test
  (Board
   (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
         'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
         'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
         (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
   0 0 0 0))

(define initial-style : Style
  ;; define a style for test
  (Style 15 15 draw-bc draw-wc draw-dp draw-lp draw-bg draw-label
         draw-bd draw-wd))

(: initial-game : Integer Integer -> Game)
(define (initial-game bv1 wv1)
  (cond
    [(= bv1 wv1) (initial-game (random 6) (random 6))]
    [(< bv1 wv1) (Game initial-board 'White (list (add1 bv1) (add1 wv1)))]
    [else (Game initial-board 'Black (list (add1 bv1) (add1 wv1)))]))

(define initial-world : World
  ;; define an initial world to start the game
  (local
    {(define ibv : Integer (random 6))
     (define iwv : Integer (random 6))
     (define ig : Game (initial-game ibv iwv))}
    (World ig initial-style 'Nowhere
           (sub1 (list-ref (Game-moves ig) 0)) -1
           (sub1 (list-ref (Game-moves ig) 1)) -1 '())))

(: run : Style -> World)
;; run the program
;; given the cell size for the row of symbols at the top of the window
(define (run s)
  (big-bang initial-world : World
    [to-draw draw]
    [on-mouse click]
    [on-key key]))

;; === save game

(: save-point : Point -> String)
;; serialize a point, given a point
(define (save-point p)
  (match p
    ['EmptyPoint "_"]
    [(OccupiedPoint 'Black n) (string-append "B" (number->string n))]
    [(OccupiedPoint 'White n) (string-append "W" (number->string n))]))
(check-expect (save-point (OccupiedPoint 'Black 2)) "B2")

(: save-points : (Listof Point) -> String)
;; serialize a point list, given a point list
(define (save-points ps)
  (match ps
    [(cons hd tl)
     (substring
      (foldr string-append ""
             (map (lambda ([s : String]) (string-append " " s))
                  (map save-point ps)))
      1)]))
(check-expect (save-points (Board-points test-board))
              "B2 _ _ _ _ W4 _ W1 _ _ _ B5 W6 _ _ _ B1 _ B3 _ _ _ _ W2")

(: save-board : Board -> String)
;; serialize a board, given a board
(define (save-board b)
  (match b
    [(Board ps bb wb bo wo)
     (string-append (save-points ps) "|" (number->string bb) "|"
                    (number->string wb) "|" (number->string bo) "|"
                    (number->string wo))]))

(: save-ms : (Listof Integer) -> String)
;; serialize the moves, given the moves
(define (save-ms ms)
  (match ms
    ['() ""]
    [(cons hd tl)
     (substring (foldr string-append ""
                       (map (lambda ([i : Integer])
                              (string-append " " (number->string i)))
                            ms)) 1)]))                            

(: save-game : Game -> String)
;; serialize a game, given a game
(define (save-game g)
  (match g
    [(Game b t ms)
     (string-append (save-board b) "@"
                    (if (symbol=? 'Black t) "B" "W") "@"
                    (save-ms ms))]))
(check-expect
 (save-game test-game)
 "B2 _ _ _ _ W4 _ W1 _ _ _ B5 W6 _ _ _ B1 _ B3 _ _ _ _ W2|2|1|2|1@B@1 2 3 6")

(: world->string : World -> String)
;; serialize the current world, given the world
(define (world->string w)
  (match w
    [(World g s c1 bv1 bv2 wv1 wv2 gs)
     (string-append (save-game g)
                    (foldr string-append ""
                           (map (lambda ([s : String]) (string-append "!" s))
                                (map save-game gs))))]))

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
      (begin
        (write-string (world->string w)
          (open-output-file path))
        (void))
      (void))))


;; === load game

(: string->integer : String -> Integer)
;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
      (error "string->integer: invalid integer"))))

(: load-point : String -> Point)
;; deserialize a point from given string
(define (load-point sp)
  (match (string-ref sp 0)
    [#\_ 'EmptyPoint]
    [#\B (OccupiedPoint 'Black (string->integer
                                (list->string (list (string-ref sp 1)))))]
    [#\W (OccupiedPoint 'White (string->integer
                                (list->string (list (string-ref sp 1)))))]))

(: load-points : String -> (Listof Point))
;; deserialize a point list from given string
(define (load-points sps)
  (map load-point (string-split sps " ")))
(check-expect (load-points
               "B2 _ _ _ _ W4 _ W1 _ _ _ B5 W6 _ _ _ B1 _ B3 _ _ _ _ W2")
              (Board-points test-board))

(: load-board : String -> Board)
;; deserialize a board from given string
(define (load-board sb)
  (local {(define string-list : (Listof String) (string-split sb "|"))}
    (Board (load-points (list-ref string-list 0))
           (string->integer (list-ref string-list 1))
           (string->integer (list-ref string-list 2))
           (string->integer (list-ref string-list 3))
           (string->integer (list-ref string-list 4)))))

(: load-ms : String -> (Listof Integer))
;; deserialize moves from given string
(define (load-ms sms)
  (map string->integer (string-split sms " ")))
(check-expect (load-ms "1 2 3 6") '(1 2 3 6))

(: load-one-game : String -> Game)
;; deserialize a game from given string
(define (load-one-game sg)
  (local {(define string-list : (Listof String) (string-split sg "@"))}
    (Game (load-board (list-ref string-list 0))
          (if (string=? "B" (list-ref string-list 1))
              'Black 'White)
          (if (= (length string-list) 3)
              (load-ms (list-ref string-list 2))
              '()))))
(check-expect (load-one-game (string-append "B2 _ _ _ _ W4 _ W1 _ _ _ B5"
                                            " W6 _ _ _ B1 _ B3 _ _ _ _ W2|2"
                                            "|1|2|1@B@1 2 3 6"))
              test-game)

(: drop-current-turn : (Listof Game) Player -> (Listof Game))
;; drop histories of current turn in the history list
;; given current history and current player
(define (drop-current-turn gs p)
  (match gs
    ['() '()]
    [(cons f r)
     (if (symbol=? p (Game-turn f))
         (drop-current-turn r p) gs)]))

(: load-first-turn? : (Listof Game) -> Boolean)
;; determine if the loaded game is in the second turn
;; given the current game status and history list
(define (load-first-turn? gs)
  (match (Game-turn (first gs))
    ['White (empty? (filter fb (drop-current-turn gs 'Black)))]
    ['Black (empty? (filter fw (drop-current-turn gs 'White)))]))

(: load-second-turn? : (Listof Game) -> Boolean)
;; determine if the loaded game is in the second turn
;; given the current game status and history list
(define (load-second-turn? gs)
  (match (Game-turn (first gs))
    ['Black (empty? (filter fb (drop-current-turn gs 'Black)))]
    ['White (empty? (filter fw (drop-current-turn gs 'White)))]))

(: find-current-turn-dice : (Listof Game) -> (Listof Integer))
;; find the dice value of current turn
;; given a history list with current status prepended
(define (find-current-turn-dice gs)
  (local
    {(define index : (Option Integer)
      (current-most-recent-i gs 0 (Game-turn (first gs))))}
    (match index
      ['None (map sub1 (Game-moves (list-ref gs (sub1 (length gs)))))]
      [(Some i) (map sub1 (Game-moves (list-ref gs (sub1 i))))])))

(: find-dice-helper : (Listof Game) Integer Player -> (Option Integer))
;; find the index of the most recent move by the current turn's player
;; given dropped current turn list, an index to keep track of the position,
;;       abd current turn's player
(define (find-dice-helper gs i p)
  (match gs
    ['() 'None]
    [(cons hd tl)
     (if (symbol=? p (Game-turn hd))
         (Some i)
         (find-dice-helper tl (add1 i) p))]))

(: find-last-turn-dice : (Listof Game) -> (Listof Integer))
;; find dice value of last turn
;; given the history list
(define (find-last-turn-dice gs)
  (local
    {(define dropped-list : (Listof Game)
       ;; the unserialized list with the most recent turn dropped
       (drop-current-turn gs (Game-turn (first gs))))
     (define index : (Option Integer)
       (current-most-recent-i dropped-list 0 (Game-turn (first dropped-list))))}
    (match index
      ['None (map sub1 (Game-moves (list-ref dropped-list
                                             (sub1 (length dropped-list)))))]
      [(Some i) (map sub1 (Game-moves (list-ref dropped-list (sub1 i))))])))

(: find-dice : (Listof Game) -> (Listof Integer))
;; find dice values for loading
;; given a history list with current status prepended
(define (find-dice gs)
  (cond
    [(load-first-turn? gs)
     (list (list-ref (find-current-turn-dice gs) 0) -1
           (list-ref (find-current-turn-dice gs) 1) -1)]
    [(load-second-turn? gs)
     (if (symbol=? (Game-turn (first gs)) 'Black)
         (append (find-current-turn-dice gs)
                 (list (list-ref (find-last-turn-dice gs) 1) -1))
         (append (list (list-ref (find-last-turn-dice gs) 0) -1)
                 (find-current-turn-dice gs)))]
    [else
     (if (symbol=? (Game-turn (first gs)) 'Black)
     (append (find-current-turn-dice gs)
                           (find-last-turn-dice gs))
     (append (find-last-turn-dice gs)
                           (find-current-turn-dice gs)))]))

(: string->world : Style String -> World)
;; deserialize a game from a string
;; given the string and current style
(define (string->world s sw)
  (local {(define string-list : (Listof String) (string-split sw "!"))
          (define dv : (Listof Integer)
            (find-dice (map load-one-game (string-split sw "!"))))}
    (World (load-one-game (first string-list)) s 'Nowhere
           (list-ref dv 0) (list-ref dv 1)
           (list-ref dv 2) (list-ref dv 3)
           (map load-one-game (rest string-list)))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : Style -> World)
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
      (string->world s (port->string (open-input-file path)))
      (error "load-game: user cancelled"))))


;; === key operation

(: key : World String -> World)
;; undo, save, load key operation
;; given current world and entered key
(define (key w k)
  (match* (w k)
    [((World g s c1 bv1 bv2 wv1 wv2 gs) "u")
     (if (empty? gs) w
         (cond
           [(and (undo-dice? w) (first-turn? g gs))
            (local
              {(define ms : (Listof Integer) (find-moves w))}
              (World (first gs) s 'Nowhere (sub1 (list-ref ms 0)) -1
                     (sub1 (list-ref ms 1)) -1 (rest gs)))]
           [(and (undo-dice? w) (second-turn? g gs))
            (local
              {(define ms : (Listof Integer) (find-moves w))}
              (match (Game-turn g)
                ['Black (World (first gs) s 'Nowhere
                               (sub1 (list-ref ms 0)) -1 wv1 wv2
                               (rest gs))]
                ['White (World (first gs) s 'Nowhere
                               bv1 bv2 (sub1 (list-ref ms 1)) -1
                               (rest gs))]))]
           [(undo-dice? w)
            (local
              {(define ms : (Listof Integer) (find-moves w))}
              (match (Game-turn g)
                ['Black
                 (World (first gs) s 'Nowhere (sub1 (list-ref ms 0))
                        (sub1 (list-ref ms 1)) wv1 wv2 (rest gs))]
                ['White
                 (World (first gs) s 'Nowhere  bv1 bv2 (sub1 (list-ref ms 0))
                        (sub1 (list-ref ms 1)) (rest gs))]))]
           [else (World (first gs) s 'Nowhere bv1 bv2 wv1 wv2 (rest gs))]))]
    [((World g s c1 bv1 bv2 wv1 wv2 gs) "s")
     (begin (save-game! w) w)]
    [((World g s c1 bv1 bv2 wv1 wv2 gs) "l")
     (load-game s)]
    [(_ _) w]))


;; === test definitions

(define test-board : Board
  ;; define board for test
  (Board
   (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 4)
         'EmptyPoint (OccupiedPoint 'White 1) 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
         (OccupiedPoint 'White 6) 'EmptyPoint 'EmptyPoint
         'EmptyPoint (OccupiedPoint 'Black 1) 'EmptyPoint
         (OccupiedPoint 'Black 3) 'EmptyPoint 'EmptyPoint
         'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
   2 1 2 1))

(define test-game : Game
  ;; define game for test
  (Game test-board 'Black '(1 2 3 6)))

(define test-style : Style
  ;; define a style for test
  (Style 10 10 draw-bc draw-wc draw-dp draw-lp draw-bg draw-label
         draw-bd draw-wd))

(define off-test-board : Board
  ;; define a board to test borne off cases
  (Board (list (OccupiedPoint 'White 15) 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
               'EmptyPoint) 0 0 0 0))

(define test-bar-game : Game
  ;; define game for test
  (Game
   (Board
    (list
     (OccupiedPoint 'White 1) (OccupiedPoint 'White 3) (OccupiedPoint 'White 2)
     'EmptyPoint (OccupiedPoint 'White 3) (OccupiedPoint 'White 4)
     'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
     (OccupiedPoint 'White 1) 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
     'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 1) (OccupiedPoint 'Black 5)
     (OccupiedPoint 'Black 2) 'EmptyPoint (OccupiedPoint 'Black 2))
    1 0 5 0)
   'Black '(2 2 2 2)))



(test)
