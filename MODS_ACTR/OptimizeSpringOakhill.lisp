;; Runs the optimizer for the Oakhill task

(load "Wundt:MCL 4.2:Run Spring Oakhill Model.lisp")

(defvar *target* nil)

(setf *target* '(.826873
                 .851421
                 .966408
                 .616963
                 .556772
                 .699042
                 .932969
                 .521802
                 .372093
                 .379360
                 .556686
                 .885174
                 .421512
                 .337209
                 .251453
                 .271802
                 .482558
                 .821221
                 .084052
                 .107759
                 .025862
                 .157328
                 .267241
                 .181034
                 .045259
                 .209052
                 .303879
                 .362069
                 .275862
                 .060345
                 .273707
                 .301724
                 .325431
                 .355603
                 .297414
                 .092672
                 .771552
                 .420259
                 .170259
                 .075436))

(defun optimize()
  (optimize-multidimensional
   '((*eff* 0.5 0.1 0.001)(*an* .10 0.01 .001)(*mp* 2.50 .1)(*rt* 1.1 .1))
   '(run-experiment :fitting t)
   '(lms *target* *results*)
   :format "~9,6F"))
