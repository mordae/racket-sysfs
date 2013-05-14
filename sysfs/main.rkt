#lang racket/base
;
; Linux /sys utilities.
;

(require racket/contract
         racket/string
         racket/file)

(provide (all-defined-out))


(define/contract (sysfs-list #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (listof path-string?))
  (map path->string
       (directory-list
         (apply build-path base-path path-items))))


(define/contract (sysfs-list/keys #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (listof path-string?))
  (let ((node-path (apply build-path base-path path-items)))
    (filter (lambda (item)
              (file-exists? (build-path node-path item)))
            (map path->string
                 (directory-list node-path)))))


(define/contract (sysfs-list/nodes #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (listof path-string?))
  (let ((node-path (apply build-path base-path path-items)))
    (filter (lambda (item)
              (directory-exists? (build-path node-path item)))
            (map path->string
                 (directory-list node-path)))))


(define/contract (sysfs-get #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      string?)
  (string-trim (file->string (apply build-path base-path path-items))))


(define/contract (sysfs-set! #:base-path (base-path "/sys")
                             #:value value
                             . path-items)
                 (->* (#:value any/c)
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      void?)
  (display-to-file value (apply build-path base-path path-items)
                   #:exists 'truncate))


; vim:set ts=2 sw=2 et:
