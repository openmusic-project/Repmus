;;;;============================================================================
;;;;               Repmus.lib
;;;;
;;;; repmus library
;;;; authors: G.Assayag, C. Malherbe
;;;; Thanks to J. Fineberg, M. Malt, F. Nicolas
;;;; date: 1996
;;;; © IRCAM 1996
;;;; © IRCAM 1998 
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
        ))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library '( ("Spectral analysis" nil nil (AS->OM) nil)
         ("Chords" nil nil (autotransp mutation map-chords chseq->poly tie-all) nil)
         ("Cribles" nil nil (lc crible-list crible-voice eval-crible) nil)
         ("Graphs" nil nil (make-graph graph-tour) nil)
         ))

(set-lib-release 1.1)



