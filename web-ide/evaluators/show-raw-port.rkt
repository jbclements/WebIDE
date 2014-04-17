#lang racket

(define listener (tcp-listen 8025))

(let loop ()
  (let-values ([(ip op) (tcp-accept listener)])
    (close-output-port op)
    (let loop ()
      (let ([line (read-line ip)])
        (printf "> ~v\n" line)
        (unless (eof-object? line)
          (loop))))
    (close-input-port ip)
    (loop)))
