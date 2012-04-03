#lang class/1
(require 2htdp/image)
(require class/universe)

;; Constants:
(define SCREEN-WIDTH 600)
(define SCREEN-HEIGHT 600)
(define CHAR-SIZE 10)
(define P-SPD 100)
(define Z-SPD 200)
(define DEAD-COLOR "gray")
(define LIVE-COLOR "red")
(define PLAYER-COLOR "green")

;; A Color is a String

;; A Multiplier is either 1 or -1

;; A Modulo-Player is a (new modulo-player% Number Number Number Number Color)

;; A modulo-live-zombie is a (new modulo-live-zombie% Number Number Number Number String)

;; A Dead-Zombie is a (new dead-zombie% Number Number Number Number Color)

;; A Zombie is one of:
;;   - Modulo-Live-Zombie
;;   - Dead-Zombie

;; A Char is one of:
;;  - Modulo-Player
;;  - Zombie

(define-class char%
  (fields nx ny x y color)
  
  (define (draw)
    (circle CHAR-SIZE "solid" (field color)))
  
  (define (die? z)
    (<= (distance z) 10))
  
  (define (distance c)
    (+ (abs (- (field x)
               (send c x)))
       (abs (- (field y)
               (send c y)))))
  
  (define (wall?)
    (or (<= (field x) 0)
        (>= (field x) SCREEN-WIDTH)
        (<= (field y) 0)
        (>= (field y) SCREEN-HEIGHT)))
  
  (define (path-mult c a b)
    (cond [(<= (distance c)
               (+ (wall-dist a (send c x))
                  (abs (- b (send c y))))) 1]
          [else -1])))

;; HELPER FUNCTIONS FOR CHAR%

;; wall-dist : Number Number -> Number
;; calculate the distance to go through a wall,
;;   given two x or y coordinates, a and b
(define (wall-dist a b)
  (cond [(<= (+ (abs (- 400 a))
                (abs (- 0 b))) 
             (+ (abs (- 0 a))
                (abs (- 400 b)))) (+ (abs (- 400 a))
                                     (abs (- 0 b)))]
        [else (+ (abs (- 0 a))
                 (abs (- 400 b)))]))

;; wall : Number Number -> Number
;; determines which wall the given coordinate, a, of a char is touching and
;;  returns the new coordinate the char should be at --> s is SCREEN-WIDTH or SCREEN-HEIGHT
(define (wall a s)
  (cond [(<= a 0) (- s 1)]
        [(>= a s) 1]
        [else a]))


;; THE PLAYER

(define-interface player<%>
  [nx ny x y color
      ;; Draw : -> Scene
      ;; Places the modulo-player on an empty-scene
      draw
      ;; die? : Zombie -> Boolean
      ;; Determines if the modulo-Player has collided with the given Zombie
      die?
      ;; distance : Modulo-Player -> Number
      ;; returns the distance from the modulo-player to the mouse
      distance
      ;; wall? : -> Boolean
      ;; determines if the modulo-Player is touching a wall
      wall?
      ;; Move : -> modulo-player
      ;; Move the modulo-player
      move
      ;; path-mult : Char Number Number -> Multiplier
      ;; find the shortest distance to the given char for the given field, a, and
      ;;   returns a multiplier for the movement function --> b is the field opposite a
      ;; interp: if a is a player's (field x) then b is a player's (field y)
      path-mult])

(define-class player%
  (super char%)
  (implements player<%>)
  
  (define (move)
    (new player% 
         (field nx) (field ny)
         (+ (/ (round (- (field nx) (field x))) P-SPD) 
            (field x))
         (+ (/ (round (- (field ny) (field y))) P-SPD) 
            (field y))
         (field color))))

;; THE MODULO-PLAYER

(define-class modulo-player%
  (super char%)
  (implements player<%>)
  
  (define (move)
    (cond [(wall?) (new modulo-player%
                        (field nx) (field ny)
                        (wall (field x) SCREEN-WIDTH) (wall (field y) SCREEN-HEIGHT)
                        (field color))]
          [else (new modulo-player% 
                     (field nx) (field ny)
                     (+ (* (/ (round (- (field nx) (field x))) P-SPD) 
                           (path-mult (new char% 0 0 (field nx) (field ny) "black") (field x) (field y)))
                        (field x))
                     (+ (* (/ (round (- (field ny) (field y))) P-SPD) 
                           (path-mult (new char% 0 0 (field nx) (field ny) "black") (field y) (field x)))
                        (field y))
                     (field color))])))


;; THE ZOMBIES

(define-interface zombie<%>
  [nx ny x y color
      ;; Draw : -> Image
      ;; Returns an Image of the Zombie
      draw
      ;; die? : Zombie -> Boolean
      ;; Determines if this has collided with the given Zombie
      die?
      ;; wall? : -> Boolean
      ;; determines if the Zombie is touching a wall
      wall?
      ;; Move : -> Zombie
      ;; Move the Zombie
      move
      ;; path-mult : Char Number Number -> Multiplier
      ;; find the shortest distance to the given char for the given field, a, and
      ;;   returns a multiplier for the movement function --> b is the field opposite a
      ;; interp: if a is a player's (field x) then b is a player's (field y)
      path-mult])

;; LIVE-ZOMBIES

(define-class modulo-live-zombie%
  (super char%)
  (implements zombie<%>)
  
  ;; Give-Pos : Number Number -> modulo-live-zombie
  ;; gives the zombie the player position
  (define (give-pos nx ny)
    (new modulo-live-zombie%
         nx ny
         (field x) 
         (field y)
         (field color)))
  
  ;; Move : -> Zombie
  ;; Move the Zombie based on the player's position
  (define (move)
    (cond [(wall?) (new modulo-live-zombie%
                        (field nx) (field ny)
                        (wall (field x) SCREEN-WIDTH) (wall (field y) SCREEN-HEIGHT)
                        (field color))]
          [else (new modulo-live-zombie% 
                     (field nx) (field ny)
                     (+ (* (/ (round (- (field nx) (field x))) Z-SPD) 
                           (path-mult (new char% 0 0 (field nx) (field ny) "black") (field x) (field y)))
                        (field x))
                     (+ (* (/ (round (- (field ny) (field y))) Z-SPD) 
                           (path-mult (new char% 0 0 (field nx) (field ny) "black") (field y) (field x)))
                        (field y))
                     (field color))])))


;; DEAD-ZOMBIES

(define-class dead-zombie%
  (super char%)
  (implements zombie<%>)
  
  (define (move)
    this))

;; HELPER FUNCTIONS FOR ZOMBIES

;; List-Dead : [Listof modulo-live-zombies] -> [Listof modulo-live-zombies]
;; Creates a list of all the zombies that have juse died
;;   from a list of all the previously live zombies
(define (list-dead lolz loaz)
  (cond [(empty? lolz) empty]
        [(ormap (λ (z) (send (first lolz) die? z)) (rest loaz))
         (cons (first lolz) (list-dead (rest lolz) (rest loaz)))]
        [else (list-dead (rest lolz) (rest loaz))]))

;; List-Live : [Listof modulo-live-zombies] -> [Listof modulo-live-zombies]
;; Creates an updated list of live zombies without all those dead ones
(define (list-live lolz loaz)
  (cond [(empty? lolz) empty]
        [(ormap (λ (z) (send (first lolz) die? z)) (rest loaz))
         (list-live (rest lolz) (rest loaz))]
        [else (cons (first lolz) 
                    (list-live (rest lolz) (rest loaz)))]))

;; Make-Dead : [Listof modulo-live-zombies] -> [Listof Dead-Zombies]
;; Changes a list of modulo-live-zombies into dead-zombies
(define (make-dead lolz)
  (cond [(empty? lolz) empty]
        [else (map (λ (z) (new dead-zombie%
                               (send z nx) (send z ny)
                               (send z x) (send z y)
                               DEAD-COLOR)) lolz)]))


;; A World is a (new world% [Listof Dead-Zombie] [Listof modulo-live-zombie] Player)
(define-class world%
  (fields dead-zombies modulo-live-zombies player)
  
  ;; App-Zombies : -> [Listof Zombies]
  ;; Appends the lists of dead-zombies and modulo-live-zombies
  (define (app-zombies)
    (append (field modulo-live-zombies)
            (field dead-zombies)))
  
  ;; on-mouse : Number Number String -> World
  ;; Handles mouse events
  (define (on-mouse mx my m)
    (cond [(string=? "move" m) (move-mouse mx my)]
          [(string=? "button-down" m) (transport mx my)]
          [else this]))
  
  ;; move-mouse : -> World
  ;; Moves the mouse position
  (define (move-mouse mx my)
    (new world% 
         (field dead-zombies)
         (field modulo-live-zombies)
         (new modulo-player%
              mx my
              (send (field player) x)
              (send (field player) y)
              (send (field player) color))))
  
  ;; transport : -> World
  ;; transports the player to a random position
  (define (transport mx my)
    (new world%
         (field dead-zombies)
         (field modulo-live-zombies)
         (new modulo-player%
              mx my
              (random SCREEN-WIDTH)
              (random SCREEN-HEIGHT)
              (send (field player) color))))
  
  ;; Name : -> String
  ;; This is the name of the game!
  (define (name) "Zombie!")
  
  ;; to-draw : -> Scene
  ;; Draws everything into one Scene
  (define (to-draw)
    (foldr (λ (z scene) (place-image (send z draw)
                                     (send z x) 
                                     (send z y)
                                     scene))
           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)
           (cons (field player) (app-zombies))))
  
  ;; on-tick : -> World
  ;; Update the World
  (define (on-tick)
    (new world% 
         (append (make-dead (list-dead (field modulo-live-zombies) (app-zombies)))
                 (field dead-zombies))
         (foldr (λ (z last) (cons (send (send z 
                                              give-pos
                                              (send (field player) x)
                                              (send (field player) y)) move) last))
                empty
                (list-live (field modulo-live-zombies) (app-zombies)))
         (send (field player) move)))
  
  ;; tick-rate : -> Number
  ;; Tells on-tick how often to tick
  (define (tick-rate)
    1/50)
  
  ;; stop-when : -> World
  ;; Produces the last World when the player collides with any Zombies
  (define (stop-when)
    (ormap (λ (z) (send (field player) die? z)) (app-zombies))))

;; Zombs : -> [Listof Zombie]
;; Creates a list of n zombies in random locations
(define (zombs n)
  (cond [(zero? n) empty]
        [else (cons (new modulo-live-zombie% 0 0 (random SCREEN-WIDTH) (random SCREEN-HEIGHT) LIVE-COLOR)
                    (zombs (sub1 n)))]))

(big-bang (new world% 
               (list) 
               (zombs (+ 5 (random 8))) 
               (new modulo-player% 0 0 0 0 PLAYER-COLOR)))