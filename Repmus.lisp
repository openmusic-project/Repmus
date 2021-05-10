;;;;============================================================================
;;;;               Repmus.lib
;;;;
;;;; repmus library
;;;; authors: G.Assayag, C. Malherbe
;;;; Thanks to J. Fineberg, M. Malt, F. Nicolas
;;;; date: 1996
;;;; © IRCAM 1996
;;;; © IRCAM 1998 
;;;; © IRCAM 1996/2021 - Metrics Modulations - From PatchWork to OpenMusic: PHRaposo
;;;;============================================================================ 

(in-package :om)

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (file) 
          (compile&load (om-make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name file))) 
      '(
        "chords"
        "graph"
        "chordmap"
        "lc1"
        "as2om"
        "om-fugit"
        ))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library '( ("Spectral analysis" nil nil (AS->OM) nil)
         ("Chords" nil nil (autotransp mutation map-chords chseq->poly tie-all) nil)
         ("Cribles" nil nil (lc crible-list crible-voice eval-crible) nil)
         ("Graphs" nil nil (make-graph graph-tour) nil)
         ("Metrics-Modulations" nil nil (s::feuillete s::tempo-intp) nil)
         ))

(set-lib-release 1.3)



