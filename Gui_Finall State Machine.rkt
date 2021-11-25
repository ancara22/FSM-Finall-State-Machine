#lang racket/base

(require racket/gui)



;Main Functions__________________________________________________________________________________________________

(define next-state(lambda(st ev table)                                     ;Next state function change, st=state, ev=event, table=state table !!! 
             (cond
                ((and (= st (first(first(first table)))) (equal? ev (last(first(first table)))))
                  (printf (string-append (~a st) " -> "))
                  (last(first table)))
                (else (next-state st ev (rest table))))))


(define run-state(lambda(st ev table)                                      ;Function for multiple events !!!
                   (cond
                     ((not(empty? ev))
                      (run-state (next-state st (first ev) table) (rest ev) table))
                     ((empty? ev)
                        (print st)
                        (send resultFrame set-label (string-append  "     Final State is: " (~a st)))))))



(define (convertList lst)                                                  ;Table convert functions
  (cond
    ((not(empty? lst))
     (append (list(list (list (first lst) (second lst)) (third lst))) (convertList (rest(rest(rest lst))))))
    (else '())))


(define (getTable str)
  (convertList (map (lambda (el)
                         (cond
                           ((number? (string->number el))(string->number el))
                           (else el)))
                       (string-split str)))) 
  



;GUI______________________________________________________________________________________________________________


(define the-font (make-font #:size 21 #:family 'default #:weight 'bold))   ;Font

(define frame (new frame%                                                  ;Main frame            
                   [label "Final State Machine"]
                   (width 400)
                   [height 200]))

(define input-box (new group-box-panel% [parent frame] [label " "]))       ;Input Table & State & Event box

(define tableTextForm(new text-field%                                      ;Table input form
                     [label "Table"]  	
                     [parent  input-box]
                     [style '(multiple vertical-label hscroll)]
                     [horiz-margin 100]
                     [vert-margin 10]
                     [min-height 200]
                     [init-value "  1 a 2\n  1 b 3\n  2 a 1\n  2 b 3\n  3 a 2\n  3 b 1\n"]))


(define startStateInput (new text-field%                                   ;Radio Box for States 
               [label "Start State:"]         
               [parent input-box ]
               [vert-margin 20]
               [horiz-margin 50]))


(define eventTextForm (new text-field%                                     ;Input Event text form   
                     [label "Event list:   "]  	
                     [parent input-box ]
                     [horiz-margin 50]))

                                                                           ;Start button
(new button%
     [parent input-box] 
     [label "Start"]
     [callback (lambda(this event)
                 (run-state (string->number (send startStateInput get-value)) (string-split (send eventTextForm get-value) " ")   (getTable (send tableTextForm get-value))))])



(define resultFrame(new message%                                           ;Result Output frame
                         [label "        ->  Result  <-       "]
                         [parent frame]
                         [font the-font]
                         [vert-margin 35]))


;_________________________________________________________________________________________________________________



(send frame show #t) ;GUI start app

 
