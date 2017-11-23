;;; vim: sts=2 sw=2 et sm lisp :

(add-piece  7 6 ("R"
                 "R")
            :color (al:map-rgb-f 1.0 1.0 1.0))
(add-piece -7 2 ("##     #############"
                 "##               ###"
                 "##     #     #######"
                 "##     #     #### ##"
                 "##                ##"
                 "##     #######    ##"
                 "##     #     #    ##"
                 "##     #######    ##"
                 "##           #    ##"
                 "##           #    ##"
                 "##           #    ##"
                 "##           #    ##"
                 "##           #     #"
                 "##           #     #"
                 "##           # #####"
                 "##           #######")
           :color (al:map-rgb-f 0.5 0.5 0.5)
           :weight +world-weight+)
(add-piece  0 6 ("##    ##"
                 "        "
                 "  ####  ")
           :color (al:map-rgb-f 0.8 0.8 0.5)
           :weight +light-weight+)
(add-piece -5 3 ("##########")
           :color (al:map-rgb-f 0.8 0.8 0.8)
           :weight +light-weight+)
(add-piece 2 4 ("# R"
                "  #")
           :color (al:map-rgb-f 0.8 0.5 0.5)
           :weight +light-weight+)
(add-piece 1 5 ("r #"
                "#  ")
           :color (al:map-rgb-f 0.5 0.8 0.5)
           :weight +light-weight+)

(setf *player* (add-piece -3 10 ("@")
                          :color (al:map-rgb-f 1.0 1.0 0.0)
                          :weight +ship-weight+))
