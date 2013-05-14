#lang racket/base
;
; Utilities for specialized APIs.
;

(require (for-syntax racket/base)
         (for-syntax racket/syntax)
         racket/string)

(require "main.rkt")

(provide (all-defined-out))


(define (convert-with convert value)
  (if value (convert value) #f))


(define-syntax (define-named-getter stx)
  (syntax-case stx ()
    ((_ name (leading-path ...) (trailing-path ...))
     (with-syntax ((getter-name (format-id #'name "get-~a" (syntax-e #'name))))
       #'(define (getter-name node-name)
           (sysfs-get leading-path ... node-name trailing-path ...))))

    ((_ name (leading-path ...) (trailing-path ...) convert)
     (with-syntax ((getter-name (format-id #'name "get-~a" (syntax-e #'name))))
       #'(define (getter-name node-name)
           (convert-with convert
             (sysfs-get leading-path ... node-name trailing-path ...)))))))


(define-syntax (define-named-setter stx)
  (syntax-case stx ()
    ((_ name (leading-path ...) (trailing-path ...))
     (with-syntax ((setter-name
                     (format-id #'name "set-~a!" (syntax-e #'name))))
       #'(define (setter-name node-name value)
           (sysfs-set! leading-path ...
                       node-name
                       trailing-path ...
                       #:value value))))

    ((_ name (leading-path ...) (trailing-path ...) convert)
     (with-syntax ((setter-name
                     (format-id #'name "set-~a!" (syntax-e #'name))))
       #'(define (setter-name node-name value)
           (sysfs-set! leading-path ...
                       node-name
                       trailing-path ...
                       #:value (convert value)))))))


(define-syntax (define-named-accessors stx)
  (syntax-case stx ()
    ((_ name (leading-path ...) (trailing-path ...))
     #'(begin
         (define-named-getter name (leading-path ...) (trailing-path ...))
         (define-named-setter name (leading-path ...) (trailing-path ...))))

    ((_ name (leading-path ...) (trailing-path ...) convert-get convert-set)
     #'(begin
         (define-named-getter name (leading-path ...) (trailing-path ...)
                              convert-get)
         (define-named-setter name (leading-path ...) (trailing-path ...)
                              convert-set)))))


(define-syntax (define-named-lister stx)
  (syntax-case stx ()
    ((_ name (leading-path ...))
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
     #'(define (lister-name)
         (sysfs-list leading-path ...))))

    ((_ name (leading-path ...) #:keys)
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
       #'(define (lister-name)
           (sysfs-list/keys leading-path ...))))

    ((_ name (leading-path ...) #:nodes)
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
       #'(define (lister-name)
           (sysfs-list/nodes leading-path ...))))

    ((_ name (leading-path ...) filter-function)
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
     #'(define (lister-name)
         (filter filter-function
           (sysfs-list leading-path ...)))))

    ((_ name (leading-path ...) #:keys filter-function)
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
       #'(define (lister-name)
           (filter filter-function
             (sysfs-list/keys leading-path ...)))))

    ((_ name (leading-path ...) #:nodes filter-function)
     (with-syntax ((lister-name
                     (format-id #'name "list-~a" (syntax-e #'name))))
       #'(define (lister-name)
           (filter filter-function
             (sysfs-list/nodes leading-path ...)))))))


(define (string->boolean str)
  (equal? "1" str))

(define (boolean->string bool)
  (if bool "1" "0"))

(define (first-word str)
  (car (string-split str)))


; vim:set ts=2 sw=2 et:
