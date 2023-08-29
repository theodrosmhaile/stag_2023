;;-------------------------------------------
;;------- start rehearse - this production runs regardless of number of digits 
;; so that all items that need rehearsing are retrieved. E.g., item 1 of 1, and 2 of 2. 
;; clear_wm slot functions here to ensure that continue-rehearse production always runs 
;; after start-rehearse. Also, the flag 'no' helps make sure this does not run before 
;; imaginal is not cleared. 


(p rehearse
  =goal>
  respond nil ;;tmh 08/24 was - respond yes
   ndigits =N
   rehearsing =R

  ?retrieval>
  - state busy
    buffer empty

  
==>

  +retrieval>
    isa wmobj
    kind wm
    position =R

  !output! (retrieving position =R for ndigits =N)
    =goal>
)

(p continue-rehearse
  =goal>
 respond nil ;;tmh 08/24 was - respond yes
    ndigits =N
  < rehearsing =N

  ?retrieval>
  - state busy
  
   =retrieval>
     kind wm
     next =NEXT


==>
   =goal>
     rehearsing =NEXT
   
   !output! (set next retrieval to =NEXT for ndigits =N)
)

(p recover-after-error
  =goal>
 respond nil ;;tmh 08/24 was - respond yes
    ndigits =N
  < rehearsing =N

  ?retrieval>
    state error
    buffer empty
   
    
   
==>
   !bind! =NEXT (incf =N)
   =goal>
     rehearsing =NEXT
   
   !output! (recover next retrieval to =NEXT for ndigits =N)
)

(p reset-rehearse 
  =goal>
  respond nil ;;tmh 08/24 was - respond yes
    read yes
    ndigits =N
    rehearsing =N

  =retrieval>
    kind wm
    position =current
    next =next
   

==>
  -retrieval>
  *goal>
    rehearsing 1    
)

