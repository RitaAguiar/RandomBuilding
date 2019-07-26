#lang racket

(require rosetta/autocad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUTIONS

(define erase 0) ;0 - does not erase
(define structure-d 1) ;0 - does not draw
(define slabs-d 0) ;0 - does not draw
(define flooring-d 1) ;0 - does not draw
(define walls-d 0) ;0 - does not draw
(define interior-walls-d 0) ;0 - does not draw
(define glass-d 0) ;0 - does not draw
(define frames-d 0) ;0 - does not draw
(define handrails-d 0) ;0 - does not draw
(define ramp-d 0) ;0 - does not draw
(define lighting-d 1) ;0 - does not draw
(define render 0) ;0 - does not render

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFINITIONS

;main
(define p0 (u0)) ;base point
(define rb 20) ;base radius
(define e-slab 0.3) ;thickness slab
(define t-glass 0.03) ;glass thickness
(define h-rail 1) ;handrail height
(define n-floors 5) ;total number of floors
(define h-floor 3.6) ;floor height
(define h-d-floor (- h-floor e-slab)) ;floor height
(define h-total (* h-floor n-floors)) ;total height
(define h-total-1 (* h-floor (- n-floors 1))) ;total height minus one floor
(define n-walls 5) ;number of interior walls per floor
;spiral
(define r0-spiral (* rb 0.45)) ;radius of internal the spiral
(define r1-spiral (* rb 0.55)) ;radius of the external spiral
(define n0-spiral 200) ;number of points of the internal spiral
(define fa0-spiral 2pi) ;final angle of the internal spiral
(define a-pts (/ 2pi n0-spiral)) ;angle between each point of the spiral
(define fa0-spiral2 fa0-spiral #;(+ fa0-spiral a-pts)) ;final angle of the internal spiral
(define fa1-spiral (/ (* fa0-spiral (- r1-spiral 0.5)) r1-spiral)) ;2pi*12/13 2pi*25/26 final angle of the external spiral
(define n1-spiral (exact-round (* n0-spiral (/ fa1-spiral fa0-spiral)))) ;;number of points of the external spiral
(define ia0-spiral 0) ;initial angle of the internal spiral
(define ia1-spiral a-pts #;(* a-pts 2)) ;initial angle of the external spiral
(define n-pts-f (/ n0-spiral 20)) ;final handrail number of points
(define a-rail-f 0) ;final handrail angle  (/ pi 104) (/ (- 1 (/ a-g1 a-g0)) 1.4)
;ramp
(define rm-spiral (/ (+ r0-spiral r1-spiral) 2)) ;medium radius of the spiral
(define h-step 0.3) ;spiral ramp step blocks height
(define l-step (- r1-spiral r0-spiral)) ;spiral ramp step blocks length
(define h0-floor h-floor) ;maximum height interior spiral
(define h1-floor (* h-floor (/ fa1-spiral fa0-spiral))) ;maximum height exterior spiral
;glass and zenith lighting
(define n-z-frames 14) ;number of zenith roof frames
(define n-g-pts 8) ; glass windows number of points
;lights
(define n-lights 10) ;number of lights per floor
(define r-lights 0.1) ;radious of lamp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LAYERS

(define flooring-layer (create-layer "Flooring"))
(define slab-layer (create-layer "Slab"))
(define ramp-layer (create-layer "Ramp"))
(define wall-layer (create-layer "Wall"))
(define glass-layer (create-layer "Glass"))
(define lights-layer (create-layer "Lights"))
(define person-layer (create-layer "Person"))
(define frame-layer (create-layer "Frame"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FUNCTIONS

;slab
(define (slab p r r1-spiral e-slab t-glass h-d-floor h-total)
  (with-current-layer slab-layer
    (subtraction
     (cylinder (+z p (- e-slab)) (- r t-glass) p)
     (cylinder (+z p0 (- h-total)) r1-spiral (+z p0 h-total)))
    (subtraction
     (cylinder (+z p h-d-floor) r (+z p h-floor))
     (cylinder (+z p0 (- h-d-floor h-total)) r1-spiral (+z p0 h-total)))))

;walls
(define (wall p r a1 da1 a2 da2 h-d e)
  (with-current-layer wall-layer
                 (thicken
                  (extrusion
                   (arc p r a1 da1) h-d) (- e))
                 (thicken
                  (extrusion
                   (arc p r a2 da2) h-d) (- e))))

;interior wall
(define (interior-wall p r r1-spiral a e-slab h-d-floor)
  (define d0 (+ r1-spiral 2)) ;minimum radiaus
  (define d1 (- r 2 e-slab)) ;maximum radiaus
  (with-current-layer wall-layer
    (right-cuboid (+cyl p d0 a (/ h-d-floor 2))
                  (/ e-slab 2) h-d-floor
                  (+cyl p d1 a (/ h-d-floor 2)))))

;interior walls
(define (interior-walls p r r1-spiral e-slab h-d-floor)
  (define (interior-wall-rec n-wall )
    (define a (random-range 0 2pi))
    (interior-wall p r r1-spiral a e-slab h-d-floor))
  (map interior-wall-rec
       (division 0 n-walls n-walls #f)))

;glass
(define (glass p r a1 da1 a2 da2 h-d e n-g-pts)
  (with-current-layer glass-layer
    (thicken
     (extrusion
      (arc p r a1 da1) h-d) (- e))
    (thicken
     (extrusion
      (arc p r a2 da2) h-d) (- e)))
  ; (define n-da1-pts (exact-round (/ (* da1 r n-g-pts) h-d)))
; (define n-da2-pts (exact-round (/ (* da2 r n-g-pts) h-d)))
;   (with-current-layer glass-layer
;     ; (extrusion
; ;      (arc p r a1 da1) h-d)
; ;     (extrusion
; ;      (arc p r a2 da2) h-d)
; 
;     ; (surface-grid
; ;      (map-division
; ;       (lambda (fi z)
; ;         (+cyl p r fi z))
; ;       a1 (+ a1 da1) 60
; ;       0 h-d 20))
; ;     (surface-grid
; ;      (map-division
; ;       (lambda (fi z)
; ;         (+cyl p r fi z))
; ;       a2 (+ a2 da2) 60
; ;       0 h-d 20))
; 
;     ; (map-division
; ;       (lambda (fi z)
; ;         (define rs (random-range 0.01 0.1))
; ;         (sphere (+cyl p r fi z) rs))
; ;       a1 (+ a1 da1) n-da1-pts
; ;       0 h-d n-g-pts)
; ;     (map-division
; ;       (lambda (fi z)
; ;         (define rs (random-range 0.01 0.1))
; ;         (sphere (+cyl p r fi z) rs))
; ;       a2 (+ a2 da2) n-da2-pts
; ;       0 h-d n-g-pts)
; 
)

;zenith lighting
(define (frame p n r1-spiral fi-i fi-f e-slab)
  (with-current-layer frame-layer
      (right-cuboid (+cyl p r1-spiral fi-i 0)
                    (/ e-slab 2) (/ e-slab 2)
                    (+cyl p r1-spiral fi-f 0))))

(define (frames p n r1-spiral e-slab)
  (define (frame-rec n)
    (define fi-i (random-range 0 pi))
    (define fi-f (random-range pi 2pi))
    (frame p n r1-spiral fi-i fi-f e-slab))
  (map frame-rec
       (division 0 n-z-frames n-z-frames #f)))

;flooring
(define (flooring p r r1-spiral t-glass h-total)
  (with-current-layer flooring-layer
    (subtraction
     (cylinder p (- r t-glass) (+z p t-glass))
     (cylinder (+z p0 (- h-total)) r1-spiral (+z p0 h-total)))))

;lighting
(define (lights p r)
  (define h-lights (- h-floor r-lights))
  (define (light-rec a)
    (with-current-layer lights-layer
      (sphere (+cyl p r a (- (- e-slab) r-lights)) r-lights)))
  (map light-rec
       (division 0 2pi n-lights #f)))

;slabs, walls, glass, frames, flooring and lighting
(define (structure p r r1-spiral n-z-frames e-slab t-glass n-g-pts h-d-floor h-floor h-total n-floors)
  (define (structure-rec n-floor)
    ;main
    (define f (random-range 0.95 1.05))
    (define at (random-range 0 2pi))
    (define rr (+ (- r) (* r f)))
    (define pt (+cyl p rr at (* h-floor n-floor)))
    ;walls
    (define a1 (random-range 0 2pi))
    (define da1 (random-range (* 3 pi/4) (* 5 pi/4)))
    (define a2 (+ a1 da1))
    (define da2 (random-range pi/3 pi/4))
    (define a3 (+ a2 da2))
    (define da3 (random-range pi/4 pi))
    (define a4 (+ a3 da3))
    (define da4 (- (+ a1 2pi) a4))
    ;elements
    (cond ((= slabs-d 1)
           (slab pt r r1-spiral e-slab t-glass h-d-floor h-total)))
    (cond ((= flooring-d 1)
           (flooring pt r r1-spiral t-glass h-total)))
    (cond ((= walls-d 1)
           (wall pt r a1 da1 a3 da3 h-d-floor e-slab)))
    (cond ((= glass-d 1)
           (glass pt r a2 da2 a4 da4 h-d-floor t-glass n-g-pts)))
    ;interior walls
    (cond ((= interior-walls-d 1)
           (interior-walls pt r r1-spiral e-slab h-d-floor)))
    ;lighting
    (cond ((= lighting-d 1)
           (lights pt (- r r-lights)))))
  ;mapping
  (map structure-rec
       (division 0 n-floors n-floors #f))
  ;first slab
  (cond ((= slabs-d 1)
         (with-current-layer slab-layer
           (cylinder (+z p (- e-slab)) r p))))
  ;zenith
  (cond ((= glass-d 1)
         (with-current-layer glass-layer
           (cylinder (+z p (+ (- h-total (/ e-slab 2)) t-glass))
                     r1-spiral
                     (+z p (- (- h-total (/ e-slab 2)) t-glass))))))
  ;zenith frames
  (cond ((= frames-d 1)
         (frames (+z p (- h-total (/ e-slab 2))) n-z-frames r1-spiral e-slab)))
  )

;ramp
(define (ramp-block p ro c-step e-step h-floor alfa n-pts)
  (define (block fi z)
    (with-current-layer ramp-layer
      (right-cuboid (+cyl p ro
                          fi
                          (- z e-step))
                    c-step e-step
                    (+cyl p ro
                          (+ fi (* alfa 2))
                          (+ (- z e-step) alfa)))))
  (map block
       (division 0 2pi (- n-pts 1) #f)
       (division 0 h-floor (- n-pts 1) #f)))

;slab handrail
(define (slab-handrail p ro c e-v alfa a n-pts h-floor)
  (define (handrail fi)
    (with-current-layer glass-layer
      (right-cuboid (+cyl p ro
                          fi
                          (+ (/ c 2) h-floor))
                    e-v c
                    (+cyl p ro
                          (+ fi alfa)
                          (+ (/ c 2) h-floor)))))
  (map handrail
       (division alfa a (- n-pts 1))))

;ramp handrail
(define (ramp-handrail p ro c e-v alfa ai af n-pts h-floor)
  (define (handrail fi z)
    (with-current-layer glass-layer
      (right-cuboid (+cyl p (- ro (/ e-v 2))
                          fi
                          (+ z (- (/ c 2) e-v) h-floor))
                    e-v c
                    (+cyl p (- ro (/ e-v 2))
                          (+ fi alfa)
                          (+ z (- (/ c 2) e-v) (/ alfa 2) h-floor)))))
  (map handrail
       (division ai af (- n-pts 1))
       (division 0 h-floor (- n-pts 1))))

(define (ramp-hanrail-f p n t-glass fi alfa)
  (define (handrail ro)
    (with-current-layer glass-layer
      (right-cuboid (+cyl p ro
                          (+ fi alfa)
                          (- (/ h-rail 2) t-glass))
                    t-glass h-rail
                    (+cyl p (+ ro (/ (- r1-spiral r0-spiral) n))
                          (+ fi alfa)
                          (- (/ h-rail 2) t-glass)))))
  (map handrail
       (division r0-spiral (- r1-spiral (/ (- r1-spiral r0-spiral) n)) (- n 1))))

;handrails
(define (handrails p ro0 ro1 c e-v ia0-spiral ia1-spiral fa0-spiral fa1-spiral n0-spiral n1-spiral h-floor n-floors)
  (define (slab-handrail-rec n-floor)
    (slab-handrail (+z p (* h-floor n-floor)) ro1 c e-v ia1-spiral fa1-spiral n1-spiral h-floor))
  (map slab-handrail-rec
       (division 0 (- n-floors 1) (- n-floors 1) #f))
  (define (ramp-handrail-rec n-floor)
    (ramp-handrail (+z p (* h-floor (- n-floor 1))) ro0 c e-v a-pts ia0-spiral fa0-spiral2 n0-spiral h-floor)
    (ramp-handrail (+z p (* h-floor (- n-floor 1))) ro1 c e-v a-pts ia1-spiral fa1-spiral n1-spiral h-floor))
  (map ramp-handrail-rec
       (division 0 (- n-floors 1) (- n-floors 1) #f))
  (ramp-hanrail-f (+z p0 h-total-1) n-pts-f t-glass a-rail-f a-pts))

;ramp
(define (ramp p ro c-step e-step h-floor alfa n-pts n-floors)
  (define (ramp-rec n-floor)
    (ramp-block (+z p (+ (* h-floor n-floor) (/ e-step 2))) ro c-step e-step h-floor alfa n-pts))
    (map ramp-rec
       (division 0 (- n-floors 1) (- n-floors 1) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;EXECUTIONS

(cond ((= erase 1)
       (delete-all-shapes)))

(cond ((= handrails-d 1)
       (handrails p0 r0-spiral r1-spiral h-rail t-glass a-pts ia1-spiral fa0-spiral fa1-spiral n0-spiral n1-spiral h-floor n-floors)))

(cond ((= ramp-d 1)
       (ramp p0 rm-spiral l-step h-step h-floor a-pts n0-spiral n-floors)))

(cond ((= structure-d 1)
       (structure p0 rb r1-spiral n-z-frames e-slab t-glass n-g-pts h-d-floor h-floor h-total n-floors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;RENDER

(render-dir "D:\\Rita\\ADA\\Bolo")
(render-size 3840 2160)

(cond ((= render 1)
       ;exterior
; (view
;  (xyz 44.1201 -67.5078 16.4944)
;  (xyz 0.232974 -1.46878 5.65119)
;  65.0)

       ;interior
       ; (view
;         (xyz -2.3385 -17.6693 8.54147)
;         (xyz -1.48016 -2.68599 7.29326)
;         30.0)

       ;interior 2
       ; (view
;         (xyz -6.6379 12.1606 5.9747)
;         (xyz -2.47136 10.791 5.98723)
;         30.0)

       ;interior 3
       (view
        (xyz -1.36325 -12.9293 8.515)
        (xyz -0.776988 -2.69552 7.66246)
        30.0)
       ;render
       (render-view "RandomBuilding")))
