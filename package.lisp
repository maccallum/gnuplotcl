(defpackage #:gnuplot
  (:use #:cl)
  (:export
   :make-gp
   :start-gp
   :quit-gp
   :restart-gp
   :is-gp-running?
   :plot
   :lplot
   :gp-command
   :list-gp-functions
   :gp-get-term-type
   :gp-set-term-type
   :gp-set-output-file
   :gp-new-window
   :gp
   :gp-flush
   :*gp-proc*
   :x11
   :png
   :eps
   :eps-tabloid-landscape))

