(import (chicken fixnum))
(import chickollect)

(time (call/cc
       (lambda (k)
         (let ((iters 0)
               (max-iters 1000))
           (collect-loop
            (lambda (data)
              (if (fx= iters max-iters)
                  (k 'exit)
                  (set! iters (fx+ iters 1))))
            collect-interval: 0)))))
