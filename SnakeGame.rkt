;;; =======================================================
;;;  CMPU-101, Spring 2013
;;;  Assignment 9 - Implementing the snake game
;;;  *** Edited and expanded through Spring 2014 ***
;;; =======================================================

(display "\n     CS101 Assignment 9, Fall 2013")
(display "\n       Mikey Saugstad\n\n")

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require racket/file)
(require htdp/dir)

;;-------------------> START CONSTANT DEFINITIONS <-------------------;;

;; The following three constants are used to draw a segment and food 
(define RADIUS 10)
(define DOT (circle RADIUS "solid" "red"))
(define FOOD (circle RADIUS "solid" "green"))

;; This constant is used when segments is moving
(define DIAMETER (* RADIUS 2))

;; side length of empty scene square and background scene
(define SIDE 400)
(define MT 
  (overlay (square SIDE "solid" "black")
           (empty-scene SIDE SIDE)))

;; The number of segment positions that exist in columns and rows
;; of the background scene.
(define POSITIONS (/ SIDE DIAMETER))

;; Clock rate: will change scene every 1/15 second
(define RATE 1/15)

;; Score: the current score (will be changed every time food is eaten)
(define SCORE 0)

;; keeps track of whether snake hit self of wall at end
(define WALL-OR-SELF "")

;;-------------------> END CONSTANT DEFINITIONS <--------------------;;


;;-------------------> START FILE WRITING <--------------------------;;

;; creates an intial highscore file with 0 as highscore if one doesn't exist
(if (not (file-exists? (string-append
                        (path->string (current-directory))
                        "HighScore.txt")))
    (display-to-file SCORE (string-append
                            (path->string (current-directory))
                            "HighScore.txt")))

;;-------------------> END FILE WRITING <---------------------------;;


;;-----------------> START WORLD STATE DEFINITION <-----------------;;

;; defines a single segment according to position and direction
(defstruct <seg> () x y dx dy)

;; defines "snake-world" as list of segments and a piece of food
(defstruct <sw> () segs food) 

;;------------------> END WORLD STATE DEFINITION <-------------------;;


;;----------------> START INITIAL WORLD DEFINITION <-----------------;;

;; initial seg placed near top left corner, moving down
(define INIT-SEG1 (make-seg 40 40 0 DIAMETER))
(define INIT-SEG2 (make-seg 40 30 0 DIAMETER))
(define INIT-SEG3 (make-seg 40 20 0 DIAMETER))

;; initial snake contains one segment and a piece of food in the center
(define INIT-SW (make-sw (list INIT-SEG1 INIT-SEG2 INIT-SEG3) (make-seg 200 200 0 0)))

;;---------------->  END INITIAL WORLD DEFINITION  <-----------------;;


;;---------------->  START TEST SNAKE CONSTANTS  <-------------------;;

;; The different snake-worlds at the bottom are made for various testers
;; in future methods.  The segments beforehand are only used within the
;; snake-worlds created in this section.  The segments are separated
;; to show they were created to be used in a new method.

(define SOME_FOOD (make-seg 200 200 0 0))
(define SEG1 (make-seg 40 300 0 DIAMETER))
(define SEG2 (make-seg 40 (- 300 DIAMETER) 0 DIAMETER))
(define SEG3 (make-seg 40 (- 300 (* 2 DIAMETER)) 0 DIAMETER))

(define SEG4 (make-seg (+ 40 DIAMETER) (- 300 DIAMETER) (* -1 DIAMETER) 0))
(define SEG5 (make-seg (+ 40 DIAMETER) DIAMETER 0 (* -1 DIAMETER)))

(define SEG6 (make-seg 40 (- 300 (* 3 DIAMETER)) 0 DIAMETER))

(define SEG7 (make-seg 40 300 0 (* -1 DIAMETER)))
(define SEG8 (make-seg 40 300 DIAMETER 0))
(define SEG9 (make-seg 40 300 (* -1 DIAMETER) 0))

(define SEG10 (make-seg 0 300 (* -1 DIAMETER) 0))
(define SEG11 (make-seg DIAMETER 300 (* -1 DIAMETER) 0))
(define SEG12 (make-seg 40 SIDE 0 DIAMETER))
(define SEG13 (make-seg 40 (- SIDE DIAMETER) 0 DIAMETER))

(define SW1 (make-sw (list SEG2 SEG3 SEG6) SOME_FOOD))
(define SW2 (make-sw (list SEG1 SEG2 SEG4 SEG5 SEG1) SOME_FOOD))
(define SW3 (make-sw (list SEG1 SEG2 SEG4 SEG5 SEG1) SEG1))
(define SW4 (make-sw (list SEG1 SEG2 SEG4 SEG5 SEG1) SEG4))
(define SW5 (make-sw (list SEG1 SEG2 SEG3) SOME_FOOD))
(define SW6 (make-sw (list SEG7 SEG2 SEG3) SOME_FOOD))
(define SW7 (make-sw (list SEG8 SEG2 SEG3) SOME_FOOD))
(define SW8 (make-sw (list SEG9 SEG2 SEG3) SOME_FOOD))
(define SW9 (make-sw (list SEG10 SEG11) SOME_FOOD))
(define SW10 (make-sw (list SEG12 SEG13) SOME_FOOD))

;;---------------->  END TEST SNAKE CONSTANTS  <---------------------;;


;;----------------> START MAIN FUNCTION DEFINITION <-----------------;;


;; Contract: (main) -> world state
;; Input:    none
;; Purpose:  Invoke the big-bang animation engine to start a simulation
;;           of the snake game.

;; Pre-function tests not easily done for images. Call to main at end tests.

;; Function Definition
(define main
  (lambda ()
    (big-bang INIT-SW
              ;; draw-segs will re-draw the scene when the snake moves
              (on-draw draw-segs)
              ;; move-segs will cons a new seg onto segs, leaving off last 
              ;; seg in segs, done every RATE second
              (on-tick move-segs RATE)
              ;; steer-segs! will destructively change the dx and
              ;; dy fields of the first segment
              (on-key steer-segs!)
              ;; off-scene-or-hit-self? will detect when the first segment is
              ;; off the scene, or when it is in the same place as another seg
              (stop-when off-scene-or-hit-self? draw-final))))

;;-----------------> END MAIN FUNCTION DEFINITION <------------------;;

;;-------------> START EVENT-HANDLER FUNCTION DEFINITIONS <----------;;


;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-DRAW CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Contract: (draw-segs ws) -> image (scene)
;; Input:    ws = world state
;; Purpose:  draw current score, food, and ws on scene at their x's and y's

;; Pre-function tests not easily done for images. Uncomment the call
;; to the on-draw clause in the main function above to test this 
;; function.

;; Function Definition
(define draw-segs
  (lambda (ws)
    ;; placing current score on top of everything else in bottom right corner
    (place-image
     (text (number->string SCORE) 24 "white" )
     (- SIDE (/ SIDE 10))
     (- SIDE (/ SIDE 10))
     (local
       [(define helper
          (lambda (list)
            (cond
              [(empty? list)
               ;; base case: before placing the snake, place food on MT scene
               (place-image
                FOOD
                (seg-x (sw-food ws))
                (seg-y (sw-food ws))
                MT)]
              [else
               ;; recursive case: place a DOT image at x and y of each
               ;; segment in the snake onto recursive call
               (place-image
                DOT
                (seg-x (first list))
                (seg-y (first list))
                ;; recursive call looks at next segment in snake
                (helper (rest list)))])))]
       ;; initial call to helper function receives only segments, not whole ws
       (helper (sw-segs ws))))))

;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-TICK CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contract: (hit-self? ws) -> boolean
;; Input:    ws = world state
;; Purpose:  helper function that says whether snake has hit itself

;; Pre-function tests:
(check-expect (hit-self? SW1) #f)
(check-expect (hit-self? SW2) #t)

;; Function Definition
(define hit-self?
  (lambda (ws)
    (local
      [(define helper
         (lambda (segments)
           (cond
             ;; base case 1: if we went through all segs and never found
             ;;              the first to be hitting one, return false
             [(empty? segments) #f]
             ;; base case 2: if we find a seg with same x and y as the first
             ;;              segment, return true
             [(and
               (= (seg-x (first segments))
                  (seg-x (first (sw-segs ws))))
               (= (seg-y (first segments))
                  (seg-y (first (sw-segs ws)))))
              #t]
             ;; recursive case: if the list isn't empty and the current seg isn't
             ;;                 hitting the first seg, recursive call rest of list
             [else
              (helper (rest segments))])))]
      ;; initial call to helper function takes only list of segs for ease of
      ;; access and so we can still access the first seg as we do recursion
      (helper (rest (sw-segs ws))))))


;; Contract: (eating? ws) -> boolean
;; Input:    ws = world state
;; Purpose:  helper function that says whether snake is eating food or not

;; Pre-function tests:
(check-expect (eating? SW1) #f)
(check-expect (eating? SW2) #f)
(check-expect (eating? SW3) #t)

;; Function Definition
(define eating?
  (lambda (ws)
    ;; returns true of the x and y values of the first seg is the same
    ;; as that of the food's x and y values
    (and
     (= 
      (seg-x (first (sw-segs ws)))
      (seg-x (sw-food ws)))
     (=
      (seg-y (first (sw-segs ws)))
      (seg-y (sw-food ws))))))


;; Contract: (new-food ws) -> seg
;; Input:    ws = world state
;; Purpose:  helper function that creates a new food seg for when one is eaten
;;           and also adds 1 to the score

;; Pre-function tests are difficult because food is placed randomly

;; Function Definition
(define new-food
  (lambda (ws)
    (define new
      ;; makes new randomly placed seg
      (make-seg
       (* DIAMETER (+ 1 (random (- POSITIONS 1))))
       (* DIAMETER (+ 1 (random (- POSITIONS 1))))
       0 0))
    ;; if this food is placed on top of the snake, try again
    (if 
     (hit-self?
      (make-sw
       (cons new (sw-segs ws))
       new))
     ;; tries again
     (new-food ws)
     (begin
       ;; adds 1 to score
       (set! SCORE (add1 SCORE))
       ;; returns the generated food
       new))))


;; Contract: (move-segs ws) -> ws
;; Input:    ws = world state
;; Purpose:  moves the snake forward by consing a new seg onto the front of
;;           the segs (with values x + dx, y + dy, dx, dy from the first seg)
;;           while checking if the snake is eating, and generating new food

;; Pre-function tests: can't test eating case because of random food
(check-expect (move-segs SW1) SW5)

;; Function Definition
(define move-segs
  (lambda (ws)
    (cond
      ;; if eating, same process of moving forward, but without taking off last
      ;; segment and calling new-food to generate a new food
      [(eating? ws)
       (make-sw
        (cons
         ;; new seg cons'd to the front
         (make-seg
          ( + (seg-x (first (sw-segs ws)))
              (seg-dx (first (sw-segs ws))))
          ( + (seg-y (first (sw-segs ws)))
              (seg-dy (first (sw-segs ws))))
          (seg-dx (first (sw-segs ws)))
          (seg-dy (first (sw-segs ws))))
         ;; whole previous snake
         (sw-segs ws))
        (new-food ws))]
      [else
       ;; in a non-eating case, cons new seg onto front of segs, leaving out
       ;; last seg if there is one, passing in same food
       (make-sw
        (cons
         ;; new seg with x = x + dx and y = y + dy of first seg in ws
         (make-seg
          ( + (seg-x (first (sw-segs ws)))
              (seg-dx (first (sw-segs ws))))
          ( + (seg-y (first (sw-segs ws)))
              (seg-dy (first (sw-segs ws))))
          (seg-dx (first (sw-segs ws)))
          (seg-dy (first (sw-segs ws))))
             ;; cons new seg onto rest of segs (sans last seg)
             (reverse (rest (reverse (sw-segs ws)))))
        ;; pass through same food
        (sw-food ws))])))

;;;;;;;;;;;;;;;;;; FUNCTION CALLED IN ON-KEY CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contract: (steer-segs! ws kee) -> ws
;; Input:    ws = world state, kee = keystroke string
;; Purpose:  changes dx and dy value of first seg in accordance with pressed key

;; Pre-function tests:
(check-expect (steer-segs! SW5 "down") SW5)
(check-expect (steer-segs! SW5 "up") SW5)
(check-expect (steer-segs! SW5 "right") SW7)
(check-expect (steer-segs! SW5 "left") SW8)

;; Function Definition
(define steer-segs!
  (lambda (ws kee)
    (cond
      ;; if up arrow pressed, change dy to -DIAMETER and dx to 0 (first seg)
      [(and (string=? kee "up")
             ;; fixes bug where pressing two keys too quickly results
             ;; in hitting self (done in all 4 clauses)
             ;; fix: make sure key wouldn't result in snake moving backwards
             (not (= (seg-y (second (sw-segs ws)))
                     (+ (seg-y (first (sw-segs ws))) (* -1 DIAMETER)))))
       ;; dx and dy values changed here
       (set-sw-segs! ws
                     (cons
                      (make-seg
                       (seg-x (first (sw-segs ws)))
                       (seg-y (first (sw-segs ws)))
                       0
                       (* -1 DIAMETER))
                      (rest (sw-segs ws))))]
      ;; if down arrow pressed, change dy to DIAMETER and dx to 0 (first seg)
      [(and (string=? kee "down")
             ;; fixes bug
             (not (= (seg-y (second (sw-segs ws)))
                     (+ (seg-y (first (sw-segs ws))) DIAMETER))))
       ;; dx and dy values changed here
       (set-sw-segs! ws
                     (cons
                      (make-seg
                       (seg-x (first (sw-segs ws)))
                       (seg-y (first (sw-segs ws)))
                       0
                       DIAMETER)
                      (rest (sw-segs ws))))]
      ;; if left arrow pressed, change dx to -DIAMETER and dy to 0 (first seg)
      [(and (string=? kee "left")
             ;; fixes bug
             (not (= (seg-x (second (sw-segs ws)))
                     (+ (seg-x (first (sw-segs ws))) (* -1 DIAMETER)))))
       ;; dx and dy values changed here
       (set-sw-segs! ws
                     (cons
                      (make-seg
                       (seg-x (first (sw-segs ws)))
                       (seg-y (first (sw-segs ws)))
                       (* -1 DIAMETER)
                       0)
                      (rest (sw-segs ws))))]
      ;; if left arrow pressed, change dx to DIAMETER and dy to 0 (first seg)
      [(and (string=? kee "right")
             ;; fixes bug
             (not (= (seg-x (second (sw-segs ws)))
                     (+ (seg-x (first (sw-segs ws))) DIAMETER))))
       ;; dx and dy values changed here
       (set-sw-segs! ws
                     (cons
                      (make-seg
                       (seg-x (first (sw-segs ws)))
                       (seg-y (first (sw-segs ws)))
                       DIAMETER
                       0)
                      (rest (sw-segs ws))))])
    ;;returns world state after mutation made
    ws))



;;;;;;;;;;;;;;;;;; FUNCTIONS CALLED IN STOP-WHEN CLAUSE ;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Contract: (off-scene-or-hit-self? ws) -> boolean
;; Input:    ws = world state
;; Purpose:  stop the simulation if segment is off the scene or hits itself and
;;           change the WALL-OR-SELF constant to reflect this

;; Pre-function tests:
(check-expect (off-scene-or-hit-self? SW1) #f)
(check-expect (off-scene-or-hit-self? SW2) #t)
(check-expect (off-scene-or-hit-self? SW9) #t)
(check-expect (off-scene-or-hit-self? SW10) #t)

;; Function Definition
(define off-scene-or-hit-self?
  (lambda (ws)
    ;; return true if the x coordinate of the first seg is 0 or SIDE or
    ;; if the y coordinate of the first seg is 0 or SIDE.
    (cond
      [(or (= (seg-x (first (sw-segs ws))) 0) 
           (= (seg-y (first (sw-segs ws))) 0) 
           (= (seg-x (first (sw-segs ws))) SIDE)
           (= (seg-y (first (sw-segs ws))) SIDE))
       (begin
         (set! WALL-OR-SELF "wall")
         #t)]
      ;; or if the snake hits itself
      [(hit-self? ws)
       (begin
         (set! WALL-OR-SELF "self")
         #t)]
      [else #f])))


;; Contract: (get-high-score) -> integer
;; Purpose:  go into highscore file and return the highscore

;; Pre-function tests:
(check-expect (integer? (get-high-score)) #t)

;; Function Definition
(define get-high-score
  (lambda ()
    (file->value (string-append
                  (path->string (current-directory))
                  "HighScore.txt"))))

;; Contract: (new-highscore? new-score) -> boolean
;; Inputs:   new-score = integer score from game played
;; Purpose:  go into highscore file and compare to input number

;; Pre-function tests:
(check-expect (new-highscore? -1) #f)

;; Function Definition
(define new-highscore?
  (lambda (new-score)
    (> new-score 
       (get-high-score))))

;; Contract: (change-highscore new-score) -> void
;; Inputs:   new-score = integer score from game played
;; Purpose:  delete old highscore file and create a new one with
;;           new-score as the highscore

;; Pre-function tests not possible because we don't want to change
;; the highscore file

;; Function Definition
(define change-highscore
  (lambda (new-score)
    (begin
      ;; change highscore by deleting old fild and 
      ;; creating new one with correct value
      (delete-file (string-append
                    (path->string (current-directory))
                    "HighScore.txt"))
      (display-to-file new-score
                       (string-append
                        (path->string (current-directory))
                        "HighScore.txt")))))


;; Contract: (draw-final ws) -> image (scene)
;; Input:    ws = world state
;; Purpose:  Create the last scene when the snake hits the wall including:
;;              i. message saying snake hit wall/self with your score
;;              ii. highscore

;; Pre-function tests not easily done for images. Uncomment the call to the
;; stop-when clause in the main function above to test this function.

;; Function Definition
(define draw-final
  (lambda (ws)
    ;; allows us to reset score to 0 if we want to play again, while using
    ;; old score within this function
    (let [(THIS-SCORE SCORE)]
      ;; sets score to 0 for next round
      (set! SCORE 0)
      ;; display that you lost and score of this game on center of screen
      ;; using WALL-OR-SELF constant to say how you lost
      (place-image
       (text (string-append "Snake hit " WALL-OR-SELF "! You Lose! Score: "
                            (number->string THIS-SCORE))
             24 "white")
       (/ SIDE 2)
       (/ SIDE 2)
       ;; display current high score directly below
       (if
        ;; if score is greater than highscore, change the highscore
        ;; and say they made a new highscore
        (new-highscore? THIS-SCORE)
        (begin
          (change-highscore THIS-SCORE)
          (place-image
           (text
            (string-append "NEW HIGH SCORE: " 
                           (number->string
                            THIS-SCORE))
            24 "white")
           (/ SIDE 2)
           (+ (/ SIDE 2) (/ SIDE 10))
           ;; overlay text image on call to draw-seg 
           (draw-segs ws)))
        ;; if they didn't make a new highscore, just display old highscore
        (place-image
         (text
          (string-append "High Score: " 
                         (number->string
                          (get-high-score)))
          24 "white")
         (/ SIDE 2)
         (+ (/ SIDE 2) (/ SIDE 10))
         ;; overlay text image on call to draw-seg 
         (draw-segs ws)))))))


;; Contract: (game-start) -> void
;; Purpose:  ask the user if they want to play Snake and call main to start
;;           the game if they want, then ask if they want to play again

;; Pre-function tests not possible due to void output

;; Function Definition
;; Function Definition
(define game-start
  (lambda ()
    ;; give the rules and such
    (printf "~%This is the game of Snake!~%")
    (printf "Move your snake around and try and collect as much food as ")
    (printf "possible without running into the wall or yourself, we'll ")
    (printf "even keep track of your highscore for you!~%")
    (printf "If you would like to play, type y and enter. If not, type anything and enter~%")
    ;; asking for user input
    (if (equal? 'y (read))
        (begin
          ;; start the game, ask if they want to play again
          (main)
          (printf "~%")
          (play-again))
        (printf "Okay, good-bye!"))))


;; Contract: (play-again) -> void
;; Purpose:  ask the user if they want to play Snake again, call main to
;;           start the game if they want, call play-again recursively

;; Pre-function tests not possible due to void output

;; Function Definition
;; Function Definition
(define play-again
  (lambda ()
    (printf "Would you like to play again?~%")
    (printf "If so, type y and enter. If not, type anything and enter~%")
    (if (equal? 'y (read))
        (begin
          (main)
          (printf "~%")
          (play-again))
        (printf "Okay, good-bye!"))))

;; run check-expects
(test)

;; automatically start the game upon running
(game-start)

    
    
    
