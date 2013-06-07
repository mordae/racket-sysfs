#lang racket/base
;
; Private Utilities
;

(provide (all-defined-out))


(define-syntax-rule (throw exn msg arg ...)
  (raise (exn msg (current-continuation-marks) arg ...)))


; vim:set ts=2 sw=2 et:
