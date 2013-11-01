#lang racket/base
;
; Block Device Manipulation
;

(require racket/string)

(require "main.rkt"
         "util.rkt")

(provide (all-defined-out))


;; All block devices.

(define-named-lister block-devices
                     ("class" "block") #:nodes)

(define-named-getter block-device-size
                     ("class" "block")
                     ("size")
                     (lambda (v)
                       (* 512 (string->number v))))

(define (list-block-device-holders name)
  (sysfs-list/nodes "class" "block" name "holders"))

(define (list-block-device-slaves name)
  (sysfs-list/nodes "class" "block" name "slaves"))


;; Device mapper devices

(define-named-getter block-device-dm-suspended?
                     ("class" "block")
                     ("dm" "suspended")
                     (lambda (v)
                       (not (= (string->number v) 0))))

(define-named-getter block-device-dm-name
                     ("class" "block")
                     ("dm" "name"))

(define-named-getter block-device-dm-uuid
                     ("class" "block")
                     ("dm" "uuid"))

(define (block-device-dm? name)
  (and (get-block-device-dm-name name) #t))



; vim:set ts=2 sw=2 et:
