;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FROM REPMUS(PATCHWORK): FEUILLETE - TEMPO-INTP ;; PHRaposo/2021
;;
;;
;;            Librairie RepMus
;;
;;            Gerard Assayag, Claudy Malherbe  © IRCAM 1996

;; fugit uses the screamer software package. Credits are below:

;;; LaHaShem HaAretz U'Mloah

;;; Screamer
;;; A portable efficient implementation of nondeterministic CommonLisp
;;; Version 3.12
;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.

;;; Written by:
;;; Jeffrey Mark Siskind
;;; Institute for Research in Cognitive Science
;;; 3401 Walnut Street Room 407C
;;; Philadelphia PA 19104
;;; Qobi@CIS.UPenn.EDU
;;; 215/898-0367
;;; and:
;;; David Allen McAllester
;;; MIT Artificial Intelligence Laboratory
;;; 545 Technology Square Room NE43-412
;;; Cambridge MA 02139
;;; DAM@AI.MIT.EDU
;;; 617/253-6599

;;; You are free to use, copy and distribute this software provided that:
;;;  1. You report *ALL* bugs to Bug-Screamer@AI.MIT.EDU whether or not you
;;;     need them fixed. Include the version number (3.12) in the message.
;;;  2. You report *ALL* bugs that you fixed to Bug-Screamer@AI.MIT.EDU.
;;;     Include the version number (3.12) in the message.
;;;  3. Every time you run Screamer on a machine or using a Lisp compiler not
;;;     mentioned below, you send a message stating the new environment and the
;;;     version number (3.12) to Bug-Screamer@AI.MIT.EDU.
;;;  4. You inform us that you obtained a copy of Screamer by sending a message
;;;     to Info-Screamer-Request@AI.MIT.EDU to be put on the
;;;     Info-Screamer@AI.MIT.EDU mailing list.

(in-package :screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; NEW FUNCTIONS - OM INTERFACE ;;;;;;

(cl::defun cons-voices (voices)
 (let ((concat-1-2 (om::concat (first voices) 
                           (second voices)))) 
(if (= (length voices) 1)
    (first voices)
(if (= (length voices) 2)
    (write concat-1-2)
    (cons-voices
     (om::x-append concat-1-2
              (cddr voices)))))))

(cl::defun build-rtm (puls-list beat-list tempo-list)
(let* ((timsign-tree (mapcar #'list puls-list beat-list))
        (corrected-tree (mapcar #'(lambda (input)
                                  (list 'om::? (list input))) timsign-tree))
        (voices (mapcar #'(lambda (input1 input2)
                       (om::make-instance 'om::voice :tree input1 :tempo input2)) corrected-tree tempo-list)))
(cons-voices voices)))

(cl::defun build-rtm-intp (puls-list beat-list tempo-list begin end)
(let* ((timsign-tree (mapcar #'list puls-list beat-list))
        (corrected-tree (mapcar #'(lambda (input)
                                  (list 'om::? (list input))) timsign-tree))
        (voices (mapcar #'(lambda (input1 input2)
                       (om::make-instance 'om::voice :tree input1 :tempo input2)) corrected-tree tempo-list)))
 (if (> begin end) 
(cons-voices (reverse voices))
(cons-voices voices))))

(cl::defun get-intp-results (result1)
    (let ((results1-matrix (om::mat-trans (om::flat-once result1))))
    (list 
     ;(list 'om::?
      (mapcar #'(lambda (input)
        (list 'om::? (list (list (second input) (third input))))) results1-matrix)
     (mapcar #'first results1-matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -------------------------------------------------- modifications to screamer

;;; allow screamer to state that 3/2 * 2/3 is an integer although the operands are not;

(let ((ccl::*warn-if-redefine* nil))

  (defun *-rule-up (z x y) ;(print "rule: *-rule-up")
    (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
    ;; note: We can't assert that Z in not an integer when either X or Y are not
    ;;       integers since they may be Gaussian integers. But we can if either
    ;;       X or Y is real. If the Screamer type system could distinguish
    ;;       Gaussian integers from other complex numbers we could whenever X or
    ;;       Y was not a Gaussian integer.
    ;(if (and (or (variable-noninteger? x) (variable-noninteger? y))
    ;	  (or (variable-real? x) (variable-real? y)))
    ;  (progn (format t  "in *-rule-up : restrict non-integer : ")
    ;         (format t "~S = ~S * ~S~%" (variable-name z)) (variable-name x) (variable-name y)
    ;         (restrict-noninteger! z)))
    (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
    ;; note: Ditto.
    (if (and (or (variable-nonreal? x) (variable-nonreal? y))
	     (or (variable-real? x) (variable-real? y)))
      (restrict-nonreal! z))
    (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      ;; note: Can sometimes do better than the following even when ranges are
      ;;       not finite.
      (restrict-bounds!
       z
       (infinity-min
        (infinity-* (variable-lower-bound x) (variable-lower-bound y))
        (infinity-min
         (infinity-* (variable-lower-bound x) (variable-upper-bound y))
         (infinity-min
          (infinity-* (variable-upper-bound x) (variable-lower-bound y))
          (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))
       (infinity-max
        (infinity-* (variable-lower-bound x) (variable-lower-bound y))
        (infinity-max
         (infinity-* (variable-lower-bound x) (variable-upper-bound y))
         (infinity-max
          (infinity-* (variable-upper-bound x) (variable-lower-bound y))
          (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))))
    (let ((x (value-of x))
          (y (value-of y))
          (z (value-of z)))
      (if (and (not (variable? x))
	       (not (variable? y))
	       (not (variable? z))
	       (/= z (* x y)))
        (fail))))
  
  (defun *-rule-down (z x y)
    ;; note: We can't assert that X and Y are integers when Z is an integer since
    ;;       Z may be an integer when X and Y are Gaussian integers. But we can
    ;;       make such an assertion if either X or Y is real. If the Screamer
    ;;       type system could distinguish Gaussian integers from other complex
    ;;       numbers we could make such an assertion whenever either X or Y was
    ;;       not a Gaussian integer.
    ;(if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
    ;    (restrict-integer! x))
    ;; note: Ditto.
    (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
    (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (/-rule z y x))
    (let ((x (value-of x))
          (y (value-of y))
          (z (value-of z)))
      (if (and (not (variable? x))
	       (not (variable? y))
	       (not (variable? z))
	       (/= z (* x y)))
        (fail))))
  )

;;; a new value sequencer for screamer. It deals correctly with real number variables.



(defmacro-compile-time value-list (&body forms)
  `(let ((values '())
	 (last-value-cons nil)
         (counter -1))
     (for-effects
       (let ((value (progn ,@forms)))
         (global (let ((a-solution (map-solution-bounds value)))
                   (when a-solution 
                     (format t "~S: ~S~%" (incf counter) a-solution)
                     (cond ((null values)
		            (setf last-value-cons (list a-solution))
		            (setf values last-value-cons))
		           (t (setf (rest last-value-cons) (list  a-solution))
		              (setf last-value-cons (rest last-value-cons)))))))))
     values))

#|
(defmethod map-solution-bounds ((var-list cons))
  (block map
    (mapcar #'(lambda (x) 
                (if (bound? x) 
                  x
                  (if (< (-   (variable-upper-bound x) (variable-lower-bound x)) 1e-2)
                    (/ (+ (variable-upper-bound x) (variable-lower-bound x)) 2)
                    (return-from map nil))))
          var-list)))

|#

(defun map-solution-bounds (var-list)
  (and var-list
       (if (listp var-list)
         (cons (map-solution-bounds (first var-list)) (map-solution-bounds (rest var-list)))
         (if (bound?  var-list) 
           var-list
           (if (< (-   (variable-upper-bound var-list) (variable-lower-bound var-list)) 1e-2)
             (/ (+ (variable-upper-bound var-list) (variable-lower-bound var-list)) 2)
             nil)))))




;;; -------------------------------------------------------------  metrics modulation 

(defun ratio-set (num-max denom-max &key test)
  (let (ratios)
    (dotimes (n num-max)
      (dotimes (d denom-max)
        (when (or (null test) (funcall test n d))
          (push (/ (1+ n) (1+ d)) ratios))))
    (sort (remove-duplicates (nreverse ratios)) '<)))

  


(setf *ratios-1-20* (ratio-set 1 20))
(setf  *ratios-20-20*  (ratio-set 20 20))
(setf  *ratios-5-5*  (ratio-set 5 5))
(setf *ratios-20-20>1* (ratio-set 20 20 :test #'(lambda (x y) (>= x y))))



;;; The time equation by F. Nicolas in Screamer.

(defun time-eqn (impulsion t-impulsion pulsation t-pulsation bar t-bar speed beats)
  (value-list
    (solution
     (let ((i (a-realv)) (t-i (a-real-betweenv 10 500)) (p (a-realv)) (t-p (a-real-betweenv 10 500))
           (m (a-realv)) (t-m (a-realv)) (v (a-realv)) (b (a-realv))
           (p2 (a-realv)) (v2 (a-realv)) (b2 (a-realv)) (pp1 (a-realv)) (pp2 (a-realv)))
       (mapc #'(lambda (inv outv)
                 (typecase inv
                   (number (assert! (=v outv inv)))
                   (list
                    (typecase (first inv)
                      (symbol
                       (cond 
                        ((string-equal (symbol-name (first inv)) "b") 
                         (assert! (andv (>=v outv (second inv))
                                        (<=v outv (third inv)))))))
                      (t (assert! (memberv outv inv)))))))
             (list impulsion t-impulsion pulsation t-pulsation bar t-bar speed beats)
             (list i t-i p t-p m t-m v b))
       (assert! 
        (andv (memberv  p  *ratios-20-20*)
              (memberv m   *ratios-20-20*)
              (memberv v   *ratios-20-20>1*)
              (memberv p2 '(1 1/2 1/4 1/8 1/16 1/32))
              (memberv v2 *ratios-20-20*)
              (memberv b  '(12 11 10 9 8 7 6 5 4 3 2 1))
              (memberv b2 '(12 11 10 9 8 7 6 5 4 3 2 1))
              (memberv pp1 *ratios-20-20*)
              (memberv pp2 *ratios-20-20*)
              ))
       (assert!  (=v (*v i v) p))
       (assert!  (=v (*v t-p v) t-i))
       (assert!  (=v (*v p b) m))
       (assert!  (=v (*v t-m b) t-p))
       (assert! (integerpv (*v v b)))
       (assert! (=v (*v p b) (*v p2 b2)))
       (assert! (=v (/v p v) (/v p2 v2)))
       (assert! (=v pp1 (/v p2 p)))
       (assert! (=v pp2 (/v p p2)))
       ;(assert! (notv (memberv  pp '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
       (assert! (orv  (=v pp1 1) (andv (notv (integerpv pp1))(notv (integerpv pp2)))))
       (list  i t-i p t-p m t-m v  b p2 v2 b2 )) ; p2 v2 b2      
     (reorder #'domain-size
              #'(lambda (x) (declare (ignore x)) nil)
              ;#'(lambda (x) (< x 1e-6))
              #'<
              #'linear-force
              ;#'divide-and-conquer-force
              ))))

;;; PW environment

(defun make-metrics (tpulsation pulsation vitesse nbpulsations)
  (setf tpulsation
        (/ (round tpulsation (/ 100.)) 100.0))
  (cond
   ((and (integerp vitesse) (integerp nbpulsations))
    (list
     (float tpulsation)
     (list nbpulsations (denominator pulsation))
     (make-list nbpulsations :initial-element `(1 ,(make-list vitesse :initial-element 1)))))
   ((and (integerp vitesse) (ccl::ratiop nbpulsations))
    (multiple-value-bind (nbeat residue)
                         (floor (numerator nbpulsations) vitesse)
      (list (float tpulsation)
            ;'(4 4)
            (list (float nbpulsations) (denominator pulsation))
            `(,.(make-list nbeat :initial-element `(1 ,(make-list vitesse :initial-element 1)))
              (, (float (/ residue vitesse)) ,(make-list residue :initial-element 1))))))
   ((and (ccl::ratiop vitesse) (integerp nbpulsations))
    (list (float tpulsation)
          (list nbpulsations (denominator pulsation))
          (make-imp-puls-overlap vitesse nbpulsations)))))



(defun make-imp-puls-overlap (vitesse nbpulsations)
  (do* ((nbiter nbpulsations (1- nbiter))
        (module (denominator vitesse))
        (period (numerator vitesse))
        (residue module)
        (beat-list nil))
       ((zerop nbiter)  (reverse beat-list))
    (let ((save-residue residue))
      (push 
       (do ((count period) (beat nil))
           ((<= count 0)
            (setf residue (if (zerop count) module (- count)))
            (setf beat (reverse beat))
            (when  (< save-residue module)
              (setf (car beat) (float (car beat))))
            (list 1 beat))
         (push (min residue  module count) beat)
         (setf count (- count residue))
         (when (< residue module) (setf residue module)))
       beat-list))))


(cl::defun build-measure (var-list)
  (if var-list
    (destructuring-bind (impulsion t-impulsion pulsation t-pulsation bar t-bar speed beats
                                   pulsation2 speed2 beats2)
                        var-list
      (make-metrics  t-pulsation        ; 
                     pulsation2
                     speed2 
                     beats2 ))
    '(60 (4 4) ((4 (1 1 1 1))))))

#|
(defun build-rtm (puls-list beat-list tempo-list &optional chords)
  (make-instance 
    'pw::C-measure-line
    :measures
    (loop for puls in puls-list
          for beats in beat-list
          for tempo in tempo-list
          for beat-chords = (pop chords)
          collect (pw::make-measure
                   puls
                   (mapcar #'(lambda (beat) 
                               (pw::beat-constructor (first beat) (second beat) (pop beat-chords) ))
                           beats)
                   tempo))))
|#

(om::defmethod! feuillete ((imp t) (timp t) (puls t) (tpuls t) (mes t) (tmes t) (vit t) (npuls t))
    :initvals '(-1 -1 -1 -1 -1 -1 -1 -1)
    :indoc '("value of the impulse"  "impulse tempo" "value of the pulses" "pulse tempo" "value of the measure"  
 "measure tempo" "speed of impulse tempo" "number of pulsation in a measure") 
    :icon 250
    :doc "ORIGINAL DOCUMENTATION FROM REPMUS-FEUILLETE (WITH MODIFICATIONS):
Builds a series of measures that obey to some constraints on metrics structure.
The metrics structure is defined with 3 levels : the measure (a group of pulsations),
the pulsation (the unit denotated by the measure signature's denominator),
the impulsion (the subdivision of the pulsation, i.e. triplets inside quarter notes in a 4/4 measure).
All the parameters can take a value of -1 which means : UNDEFINED. Generally you
specify only some parameters, put -1 in the others. This defines a constraint system that 
is solved for you by 'feuillete'.
All the parameter can take a list instead of a single value. A list (v1 v2 ... vn) means
that the considered parameter can take any value among v1,v2,...,vn.
All the parameters can take a list of the form (b v1 v2). This means the considered parameter
can take all the values BETWEEN v1 and v2.
You can specify strange values like 5/16 for the pulsation. This means that there is a first
level of WRITTEN pulsation which is the quarter note (1/4), subdivided into 4 smaller unit
(sixteenth notes). The smaller units are linked 5 by 5 (5/16) which lets you hear another
pulsation. This is combinable with any impulsion speed (i.e. you can put triplets in that 
perceived pulsation.
This kind of manipulation can be very complex but you still have a precise control over
what you are building. It is very easy to generate for instance metrics modulation a la Carter.

This function is inspired by Francois Nicolas paper : 'Le feuillete du tempo' thus the name.
This module uses the Constraint Solver 'screamer' by J.F. Siskind and D.A. McAllester from
Univ.of Pennsylvanny and MIT.

parameters :

imp : integer or  ratio, impulsion (1/16 = sixteenth note, 1/12 a triplet unit etc.)
timp : integer, impulsion tempo (120 means 120 impulses in a mn)
puls : integer or  ratio, pulsation (1/4 = quarter note, 1/8 = eighth note etc.)
tpuls : integer, pulsation tempo (60 means 60 pulsation in a mn)
mes : integer or ratio, measure signature (3/4 means 3 quarter notes)
tmes : integer, measure tempo (20 means 20 measures in a mn)
vit : integer or ratio, number of subdivision of the pulsation (3 : triplet, 2/3: triplets with notes linked 2 by 2)
npuls : integer, number of pulsation in a measure, an alternative to 'mes' parameter

output : a voice object.
"

  (let* ((solutions
          (apply 'time-eqn
                 (mapcar #'(lambda (x) (and (or (listp x) (and (numberp x) (> x 0)))
                                            x))
                         (list imp timp puls tpuls mes tmes vit npuls))))
         ;(solutions (print(mapcan  #'(lambda (solution) (and (not (om-api::memq nil solution)) (list solution)))
         ;                          solutions)))
         (sequence 
          (om::mat-trans  (or (mapcar 'build-measure solutions)
                              (list nil)))) ;; due to a bug in mat-trans
         (counter -1))
    (format t "~%")
    (when sequence
      (mapc #'(lambda (solution)
                (format t "S~S : ~S  ~S en vitesse ~S sur ~S temps" (incf counter) (third solution) 
                        (fourth solution) (seventh solution) (eighth solution))
                (when (/= (third solution) (ninth solution))
                  (format t " / ~S  ~S en vitesse ~S sur ~S temps"  (ninth solution) 
                          (round  (* (fourth solution) (/  (nth 8  solution) (third solution))))
                          (nth 9  solution) (nth 10 solution)))
                (format t "~%"))
            solutions)
       (build-rtm (second sequence) (third sequence) (first sequence)))))
;      (build-rtm (mapcar #'second (second sequence))
;                 (third sequence)
;                 (first sequence)))))

(defun transitions (begin end nbsteps tol ratio-set curve)
  (value-list
    (solution
     (let ((steps ()) (pi-steps 1))
       (dotimes (i (1- nbsteps))
         (let ((step (a-realv)))
           (assert! (memberv step ratio-set))
           (assert! (/=v step 1))
           (push  step steps)
           (setf pi-steps (*v pi-steps step))
           ))
       (setf steps (cons 1 (nreverse steps)))
       (let ((rate1 (/ end begin)) (rate2 pi-steps))
         (assert! (<v (-v (/v (maxv rate1 rate2) (minv rate1 rate2)) 1) tol)))
       (mapl #'(lambda (steps)
                 (when (rest steps)
                   (case curve
                     (1 (assert! (<v (first steps) (second steps))))
                     (0 (assert! (=v (first steps) (second steps))))
                     (-1 (assert! (>v (first steps) (second steps)))))
                   (assert! (funcallv #'(lambda (ratio1 ratio2)
                                          (<  (denominator ratio1)  (numerator ratio2))
                                          )
                                      (first steps) (second steps)))
                   ))
             (rest steps))
       steps)
     (reorder #'domain-size
              #'(lambda (x) (declare (ignore x)) nil)
              #'<
              #'linear-force
              ))))

;(transitions 60 167 4 '(1 2 3 4 5) '(1 2 3 4 5))

#|
(defun accent-notes (measure-list midic vel-low vel-high)
  (let ((dur 5) (channel 1) ) 
    (mapcar #'(lambda (beat-list)
                (mapcar #'(lambda (beat) 
                            (let ((first? t))
                              (mapcar #'(lambda (impulse) (declare (ignore impulse))
                                         (prog1
                                           (om::make-instance 
                                             'om::chord 
                                              :lmidic (list midic)
                                              :lvel (list (if first? vel-high vel-low)) 
                                              :ldur (list dur)) 
                                           ;(pw::mk-chord (list midic) (list dur) () (list (if first? vel-high vel-low)))
                                           ;(pw::make-instrument-note  midic dur channel (if first? vel-high vel-low) () ())
                                           (setf first? nil)))
                                      (second beat))))
                        beat-list))
            measure-list)))
|#

(defun interpol (begin end nsteps tol dom curve)
  (let ((solution (transitions begin end nsteps tol
                               (ratio-set dom dom :test #'(lambda (x y) (>= x y)))
                               curve)))
    (list 
     solution
     (mapcar #'(lambda (s)
                 (rest (reverse (reduce   #'(lambda ( x y ) (cons (* (first x) y) x))
                                          (cons (float begin) (rest s))
                                          :initial-value (list 1)))))
             solution))))

                         

;(interpol 60 180 6 1.01 6)


(defun modulate-once (source-tempo target-tempo ratio)
  (list (make-metrics source-tempo 1/4 (numerator ratio) (denominator ratio))
        (make-metrics target-tempo 1/4  (denominator ratio) (numerator ratio))))


(om::defmethod! tempo-intp ((begin integer) (end integer) (nsteps integer) (tol% integer) (dom integer) (ratio t) &optional (sol 1))
    :initvals '(60 200 3 30 5 1 1)
    :indoc '("begin" "end" "steps"  "tolerance" "domain" "ratio-mode" "solution-mode")
    :menuins '( (5 (("Any" 1) ("=" 2) ("->" 3) ("<-" 4) ))
                      (6 (("voice" 1) ("list" 2) ))
                    )
    :icon 250
    :doc "ORIGINAL DOCUMENTATION FROM REPMUS TEMPO-INTP (WITH MODIFICATIONS):
	Builds a series of measures where the tempo changes smoothly from a starting value
to an end value. At each step, a metrics modulation is performed.
Typical subdivisions of the beat (impulsions) are computed to optimize the modulation.
A set of ratios is used to pass from a measure to an other. You have control over
these ratios : they can be always the same, or increasing, or decreasing, or in any order.
You can specify a domain for the ratio. Dom=3 means, 1, 2, 3, 1/2, 1/3, 2/3, 3/2 are allowed.
The more ratios allowed, the more solutions.
'tempo-intp' yields all the possible solutions in the constraint system specified by
the parameter values. The solutions are concatenated in a voice or gathered into
a list, depending on the parameter 'sol'.

This function uses the Constraint Solver 'screamer' by J.F. Siskind and D.A. McAllester from
Univ.of Pennsylvanny and MIT.


parameters : 

begin : number, initial tempo
end : number, final tempo
nsteps : integer, number of interpolation steps
tol% : integer, allowed deviation when reaching the final tempo.
dom : integer, all ratios whose num and denum are smaller or equal to dom may be used.
ratio: menu, 'any' means any ratio will do, '=' means all ratios must be equal, 
'->' means ratios are increasing, '<-' means ratios are decreasing.
sol : (optional, menu) 'voice' means all solutions are concatenated into a voice object, 'list' means' all solutions are put into a list (rhythmic trees and tempos).

output : 

A voice object or a list of rythmic trees and tempos.
"
  (let* 
    ((tol (/ tol% 100.0))
     (curve (case ratio (1 nil) (2 0) (3 1) (4 -1)))
     (solution  (om::mat-trans 
                      (if (> begin end) 
                          (interpol end begin nsteps tol dom curve)
                          (interpol begin end nsteps tol dom curve))))
     (result1 (mapcar #'(lambda (one-solution)
                          (let* ((sequence
                                  (om::mat-trans 
                                   (mapcon #'(lambda (ratiol tempol)
                                               (when  ratiol
                                                 (modulate-once (first tempol) (second tempol) (first ratiol))))
                                           (rest (first one-solution))
                                           (second one-solution))))
                                 (beats (third sequence)))
                            (list (first sequence) (second sequence) beats))) ;(accent-notes beats 6000 70 115)))) 
                      solution)))
 (when result1
  (cond ((= sol 2)
   (get-intp-results result1))     
            (t
             (let ((result2 (mapcar #'om::flat-once  (om::mat-trans (or result1 (list nil))))))       
               (build-rtm-intp (second result2)
                          (third result2)
                          (first result2)
                          begin end)))))))
