;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Load the arrays holding trial information
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Wundt:MCL 4.2:LoadSpringData") 

(load-trial)
(load-strings)
(load-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  declare some global variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *answers* nil)           ; holds the model's response on each trial

(defvar *correct-answers* nil)   ; holds the correct digits for each trial
(defvar *cor-ans* nil)

(defvar *number* nil)            ; number of presentations
(defvar *correct* nil)           ; number of correct responses
(defvar *results* nil)           ; correct/number

(defvar *rehearsals* 0)

(defvar last 0) ; a flag variable indicating that this is the last character 
                ; of the string

(defvar stimulus 0) ; used to access the stimulus strings in the data array

(defvar *delay* 1.0
  "time between trials")

(defvar perfect t) ; a flag indicating perfect recall 

(defvar *ga* 1.00)          ; variable for goal activation - W
(defvar *an* 0.04)          ; variable for activation noise
(defvar *rt* 0.194)           ; retrieval threshold
(defvar *mp* 1.87)           ; mismatch penalty
(defvar *bll* 0.5)          ; base level learning

(defvar *increment* 0.50)    ; variable for delay between characters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the Oakhill model itself              
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-model "Wundt:MCL 4.2:Oakhill")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function name: run-experiment()
;; takes: nothing
;; returns: nothing
;; does: Runs the model 30 times and writes out the predictions for the
;;       proportion of strings completely correct as a function of
;;       memory set size for a particular value of W.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-experiment( &key (runs 30)(screen-output nil)(file-output nil)(w-varying 0.0625)(file 1)(sound nil)(fitting nil))
  (setf *number* (make-array 40 :element-type 'float :initial-element 0.0))
  (setf *correct* (make-array 40 :element-type 'float :initial-element 0.0))
  (setf *results* (make-array 40 :element-type 'float :initial-element 0.0))
  
  (dotimes (run runs)                    ; run the model x times with run as the index of the current run
      (reset)                           ; resets ACT-R
      (when w-varying (setf *ga* (max 0.0 (+ 1.0 (noise w-varying)))))
      (sgp-fct (list ':rt *rt* ':mp *mp* ':an *an* ':bll *bll* ':ga *ga*))        ; sets the ACT-R goal activation parameter
      (when screen-output (format t "~A " (+ run 1)))
      (let ((stimulus 0)                ; set stimulus to 0 - a variable to point at the correct character
            (real-time 0.0))             ; ACT-R timing variable
        (dotimes (trial 67)             ; runs through all 67 trials using trial as an index
          (setf *correct-answers* nil)  ; empty the *correct-answers* string
          (incf *time* *delay*)         ; adds the time between trials
          (setf real-time *time*)       ; tells ACT-R what time it is
          (mod-chunk-fct 'problem (list 'trial trial 'flag nil 'position 'first 'rehearse nil)) ; modifies the goal chunk
          
          ; this next bit presents the stimuli to ACT-R
          
          (dotimes (string (aref *trial* trial 1))  ; go through all the strings on the trial - string is the index
            (dotimes (char (+ 1 (aref *strsizes* trial string)))  ; goes through the characters in a string - char is the index
              (setf last (= char (aref *strsizes* trial string))) ; if this is the last character set last to true
              (mod-chunk-fct 'problem (list 'vision (aref *data* stimulus char) 'status nil 'flag nil 'rehearse 'first))
              (when last (mod-chunk-fct 'problem (list 'flag 'last))  ; if this is the last character change flag to last and add
                    (setf *correct-answers* (nconc *correct-answers* (list (aref *data* stimulus char)))))  ; the character to *correct-answers*
              (incf real-time *increment*) ; tell ACT-R what time it is
              (Goal-Focus problem)       ; focus on the problem
              (run-fct *increment*))       ; runs ACT-R for the length of the delay between characters
            (incf stimulus))             ; increments stimulus so we get the next one

          ; this next bit gets ACT-R to recall

          (Goal-Focus solution)          ; focus on recalling
          (mod-chunk-fct 'solution (list 'trial trial 'item nil 'position nil 'status nil
                                         'limit (nth (aref *trial* trial 1) '(zeroth first second third fourth fifth sixth))
                                         ))
          (setf *answers* nil)           ; clears the answers string
          (run)                          ; runs ACT-R
          
          (when file-output (with-open-file (out (merge-pathnames (format nil "test~A.dat" file))
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :append)
            (format out "~A~T~A~T~A~T~A~T~A~T~A~%" (+ run 1) (+ trial 1) (aref *trial* trial 0) *ga* *answers* *correct-answers*)))

          (when fitting
            (when (< 2 trial)
              (let ((memory nil)
                    (correct nil)
                    (index nil)
                    (perfect t))
                (setf *cor-ans* *correct-answers*)
                (dotimes (string (aref *trial* trial 1))
                  (when (eq (aref *trial* trial 1) 3)
                    (setf index 0))
                  (when (eq (aref *trial* trial 1) 4)
                    (setf index 3))
                  (when (eq (aref *trial* trial 1) 5)
                    (setf index 7))
                  (when (eq (aref *trial* trial 1) 6)
                    (setf index 12))
                  (setf memory (pop *answers*))
                  (setf correct (pop *correct-answers*))
                  
                  (incf (aref *number* (+ index string)))
                  (incf (aref *number* (+ (+ index 18) string)))
                  (when (eq memory correct) 
                    (incf (aref *correct* (+ index string))))
                  (when (not (eq memory correct))
                    (setf perfect nil)
                    (when (member memory *cor-ans*)
                      (incf (aref *correct* (+ (+ index 18) string))))))
                (incf (aref *number* (+ (- (aref *trial* trial 1) 3) 36)))
                (when perfect (incf (aref *correct* (+ (- (aref *trial* trial 1) 3) 36))))))) 
          )))
  (when sound (beep))
  (when fitting
    (dotimes (index 36)
      (setf (aref *results* index) (/ (aref *correct* index) (aref *number* index)))))

)

(defun make-ws()
  (let ((w .5))
  (dotimes (i 13)
    (setf *ga* w)
    (run-experiment :screen-output t :file-output t :file *ga*)
    (incf w .1)))
)

(defun make-blls()
  (let ((decay 0.1))
  (dotimes (i 9)
    (setf *bll* decay)
    (format t "~%~A~%" *bll*)
    (run-experiment :screen-output t :file-output t :file *bll* :w-varying nil)
    (incf decay .1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function name: showme()
;; takes: nothing
;; returns: nothing
;; does: Outputs the values of the parameters I'm playing with
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun showme()
  (format t "AN = ~A~%RT = ~A~%MP = ~A~%" *an* *rt* *mp*)
)

