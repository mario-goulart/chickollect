(module chickollect (collect-loop)

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use extras data-structures files posix srfi-1 utils)
  (define file-readable? file-read-access?))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken fixnum)
          (chicken io)
          (chicken pathname)
          (chicken string)
          (chicken time posix))
  (import srfi-1))
 (else
  (error "Unsupported CHICKEN version.")))

;; Faster than SRFI-13's
(define (string-prefix? prefix str)
  (let ((idx (substring-index prefix str)))
    (and idx (fx= idx 0))))

;;;
;;; CPU usage statistics
;;;
;; http://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux
(define-record cpu-stat
  user
  nice
  system
  idle
  iowait
  irq
  softirq
  steal
  guest
  guest-nice)

(define num-cpus
  (let ((cpuinfo (with-input-from-file "/proc/cpuinfo" read-lines)))
    (count (lambda (line)
             (string-prefix? "processor\t" line))
           cpuinfo)))

(define cpu-stats (make-vector num-cpus #f))

(define (cpu-idle stat)
  (fx+ (cpu-stat-idle stat) (cpu-stat-iowait stat)))

(define (cpu-nonidle stat)
  (+ (cpu-stat-user stat)
     (cpu-stat-nice stat)
     (cpu-stat-system stat)
     (cpu-stat-irq stat)
     (cpu-stat-steal stat)))

(define-inline (prevent-0 val)
  (if (fx= 0 val)
      1
      val))

(define (cpu-usage prev stat)
  (if prev
      (let* ((cur-idle (cpu-idle stat))
             (cur-total (fx+ cur-idle (cpu-nonidle stat)))
             (prev-idle (cpu-idle prev))
             (prev-total (fx+ prev-idle (cpu-nonidle prev))))
        (/ (fx- (fx- cur-total prev-total)
                (fx- cur-idle prev-idle))
           (prevent-0 (fx- cur-total prev-total))))
      (let* ((cur-idle (cpu-idle stat))
             (cur-total (fx+ cur-idle (cpu-nonidle stat))))
        (/ (fx- cur-total cur-idle)
           (prevent-0 cur-total)))))

(define (parse-cpu-stat!)
  (let ((stat-data (with-input-from-file "/proc/stat" read-lines)))
    (let loop ((cpuno 0)
               (lines stat-data))
      (unless (or (fx> cpuno num-cpus)
                  (null? lines))
        (let ((line (car lines)))
          (if (string-prefix? (conc "cpu" cpuno) line)
              (let ((prev (let ((p (vector-ref cpu-stats cpuno)))
                            (and p (cdr p))))
                    (cur (apply make-cpu-stat
                                (map string->number
                                     (cdr (string-split line))))))
                (vector-set! cpu-stats cpuno (cons prev cur))
                (loop (fx+ cpuno 1) (cdr lines)))
              (loop cpuno (cdr lines))))))))


(define (cpus-usage)
  (parse-cpu-stat!)
  (let ((usage (make-vector num-cpus)))
    (let loop ((cpuno 0))
      (when (fx< cpuno num-cpus)
        (let ((stat (vector-ref cpu-stats cpuno)))
          (vector-set! usage
                       cpuno
                       (* 100 (cpu-usage (car stat) (cdr stat)))))
        (loop (fx+ cpuno 1))))
    (vector->list usage)))

;;;
;;; Memory statistics
;;;
(define-record meminfo total free buffers cached swap-total swap-free)

(define (parse-meminfo)
  (let ((lines (with-input-from-file "/proc/meminfo" read-lines))
        (meminfo (make-meminfo #f #f #f #f #f #f))
        (get-val (lambda (line)
                   (string->number (cadr (string-split line))))))
    (for-each
     (lambda (line)
       (cond ((string-prefix? "MemTotal:" line)
              (meminfo-total-set! meminfo (get-val line)))
             ((string-prefix? "MemFree:" line)
              (meminfo-free-set! meminfo (get-val line)))
             ((string-prefix? "Buffers:" line)
              (meminfo-buffers-set! meminfo (get-val line)))
             ((string-prefix? "Cached:" line)
              (meminfo-cached-set! meminfo (get-val line)))
             ((string-prefix? "Cached:" line)
              (meminfo-cached-set! meminfo (get-val line)))
             ((string-prefix? "SwapTotal:" line)
              (meminfo-swap-total-set! meminfo (get-val line)))
             ((string-prefix? "SwapFree:" line)
              (meminfo-swap-free-set! meminfo (get-val line)))))
     lines)
    meminfo))

(define (memory-in-use meminfo)
  ;; Return the percentages of RAM and swap in use as a pair (<ram> . <swap>)
  (let ((ram-avail (+ (meminfo-free meminfo)
                      (meminfo-buffers meminfo)
                      (meminfo-cached meminfo)))
        (ram-total (meminfo-total meminfo))
        (swap-total (meminfo-swap-total meminfo))
        (swap-free (meminfo-swap-free meminfo)))
    (cons (- 100 (/ (* 100 ram-avail) ram-total))
          (if (zero? swap-total)
              0
              (- 100 (/ (* swap-free 100)
                        swap-total))))))


;;;
;;; Battery
;;;
(define (battery-status)
  (if (file-readable? "/sys/class/power_supply")
      (map (lambda (battery-dir)
             (let* ((status-file (make-pathname battery-dir "status"))
                    (status
                     (and (file-readable? status-file)
                          (with-input-from-file status-file read)))
                    (capacity-file (make-pathname battery-dir "capacity"))
                    (capacity
                     (and (file-readable? capacity-file)
                          (with-input-from-file capacity-file read))))
               (cons status capacity)))
           (glob "/sys/class/power_supply/BAT*"))
      '()))

;;;
;;; Network
;;;
(define-record netdev iface bytes-recv bytes-sent)

(define (parse-netdev)
  (let ((lines (cddr (with-input-from-file "/proc/net/dev" read-lines))))
    (let loop ((lines lines))
      (if (null? lines)
          '()
          (let* ((line (car lines))
                 (tokens (list->vector (string-split line))))
            (cons (make-netdev
                   (let ((iface (vector-ref tokens 0)))
                     (string->symbol
                      (substring iface 0 (fx- (string-length iface) 1))))
                   (string->number (vector-ref tokens 1))  ;; bytes received
                   (string->number (vector-ref tokens 9))) ;; bytes transmitted
                  (loop (cdr lines))))))))

(define prev-netdev-stats #f)

(define (netdev-stats collect-interval)
  (let* ((stats (parse-netdev))
         (interval (prevent-0 collect-interval))
         (diff-stats
          (if prev-netdev-stats
              (map (lambda (prev cur)
                     ;; Let's sloppily assume ifaces order won't change
                     (list (netdev-iface cur)
                           (/ (- (netdev-bytes-recv cur)
                                 (netdev-bytes-recv prev))
                              interval)
                           (/ (- (netdev-bytes-sent cur)
                                 (netdev-bytes-sent prev))
                              interval)))
                   prev-netdev-stats stats)
              (map (lambda (cur)
                     (list (netdev-iface cur) 0 0))
                   stats))))
    (set! prev-netdev-stats stats)
    diff-stats))

;;;
;;; Collect loop
;;;
(define (collect-loop handler #!key (date/time-format "%T %F")
                                    (collect-interval 1)
                                    (monitors
                                     '(memory cpu date/time battery network)))
  (let loop ()
    (let ((data
           (let loop-monitors ((monitors monitors))
             (if (null? monitors)
                 '()
                 (let ((monitor (car monitors)))
                   (cons
                    (cons monitor
                          (case monitor
                            ((memory)
                             (memory-in-use (parse-meminfo)))
                            ((date/time)
                             (time->string (seconds->local-time) date/time-format))
                            ((cpu)
                             (cpus-usage))
                            ((battery)
                             (battery-status))
                            ((network)
                             (netdev-stats collect-interval))
                            (else (error 'collect-loop "Invalid monitor" monitor))))
                    (loop-monitors (cdr monitors))))))))
      (handler data))
      (sleep collect-interval)
      (loop)))

) ;; end module
