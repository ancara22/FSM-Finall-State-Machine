#lang racket




(define state-tab '(                        ;State list with events and final resulting state !!!
                    ((1 "a")2)
                    ((1 "b")3)
                    ((2 "a")1)
                    ((2 "b")3)
                    ((3 "a")2)
                    ((3 "b")1)
                   ))


(define next-state(lambda(st ev table)      ;Next state function change, st=state, ev=event, table=state table !!!   
             (cond
                ((and (= st (first(first(first table)))) (equal? ev (last(first(first table)))))
                 (sleep 1)
                  (printf (string-append (~a st) " -> "))
                  (last(first table)))
                (else (next-state st ev (rest table))))))




(define run-state(lambda(st ev table)        ;Function for multiple events !!!
                   (cond
                     ((not(empty? ev))
                      (run-state (next-state st (first ev) table) (rest ev) table))
                     ((empty? ev)
                        (sleep 1)
                        (print st)
                        (printf "\nEnter next event(Or C to exit): ")
                        (define nxtEvent (string-split (read-line) " "))
                        (cond
                          ((not(equal? nxtEvent '("C")))
                              (run-state st nxtEvent table))
                        (else "Good Bye!"))))))


(define start-program (lambda()              ;intput/output For Terminal


                     
                  (display "Enter start State: ")   
                  (define st (string->number (read-line)))
                  
                  (display "Enter event list: ")
                  (define ev (string-split (read-line) " "))

                  (cond   
                    ((and (number? st) (not(empty? ev))) 
                     (printf "From start state:  ") 
                     (run-state st ev state-tab)) 
                    (else
                    (printf "ERROR! INCORRECT VALUES.\nPlease enter the correct values.
 STATE: number 1-3  EVENTS: letters a-b introduced by space\n\n") (start-program) ))))


             
(start-program) ;Terminal App

