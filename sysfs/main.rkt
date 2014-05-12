#lang racket/base
;
; Linux /sys utilities.
;

(require racket/contract
         racket/string
         racket/file
         racket/list
         throw)

(provide (all-defined-out))


(define-struct/contract (exn:fail:sysfs exn:fail)
  ((path path-string?))
  #:transparent)


(define/contract (sysfs-list #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (or/c #f (listof path-string?)))
  (let ((path (apply build-path base-path path-items)))
    (if (directory-exists? path)
      (map path->string
           (directory-list
             path))
      #f)))


(define/contract (sysfs-list/keys #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (or/c #f (listof path-string?)))
  (let ((node-path (apply build-path base-path path-items)))
    (if (directory-exists? node-path)
      (filter (lambda (item)
                (file-exists? (build-path node-path item)))
              (map path->string
                   (directory-list node-path)))
      #f)))


(define/contract (sysfs-list/nodes #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (or/c #f (listof path-string?)))
  (let ((node-path (apply build-path base-path path-items)))
    (if (directory-exists? node-path)
      (filter (lambda (item)
                (directory-exists? (build-path node-path item)))
              (map path->string
                   (directory-list node-path)))
      #f)))


(define/contract (sysfs-get #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (or/c #f string?))
  (let ((path (apply build-path base-path path-items)))
    (if (file-exists? path)
      (with-handlers ((exn:fail:filesystem:errno?
                        (lambda (exn)
                          (throw (exn:fail:sysfs path)
                                 'sysfs "read failed"
                                 "path" path))))
        (let ((value (file->string path)))
          (if (eof-object? value) "" (string-trim value))))
        #f)))


(define/contract (sysfs-get-name #:base-path (base-path "/sys") . path-items)
                 (->* ()
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      (or/c #f string?))
  (let ((path (apply build-path base-path path-items)))
    (if (or (file-exists? path)
            (link-exists? path))
        (with-handlers ((exn:fail:filesystem:errno?
                          (lambda (exn)
                            (throw (exn:fail:sysfs path)
                                   'sysfs "readlink failed"
                                   "path" path))))
          (path->string (last (explode-path (resolve-path path)))))
        #f)))


(define/contract (sysfs-set! #:base-path (base-path "/sys")
                             #:value value
                             . path-items)
                 (->* (#:value any/c)
                      (#:base-path path-string?)
                      #:rest (listof path-string?)
                      void?)
  (let ((path (apply build-path base-path path-items)))
    (with-handlers ((exn:fail:filesystem:errno?
                      (lambda (exn)
                        (throw (exn:fail:sysfs path)
                               'sysfs "write failed"
                               "path" path
                               "value" value))))
      (display-to-file value path #:exists 'truncate))))


; vim:set ts=2 sw=2 et:
