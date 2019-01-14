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
      '("as2om"
        "chords"
        "graph"
        "chordmap"
        "lc1"
        #+sdif "as2om-sdif"
        ))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library '( ("Audiosculpt" nil nil (AS->OM) nil)
         ("Chords" nil nil (Autotransp Mutation map-chords Chseq->poly tie-all) nil)
         ("Cribles" nil nil (lc crible-list crible-voice eval-crible pulsemaker ) nil)
         ("Graphs" nil nil (make-graph graph-tour) nil)
         ))

(set-lib-release 1.0)



