#lang racket/base
;
; Network Interface Manipulation
;

(require racket/string)

(require "main.rkt"
         "util.rkt")

(provide (all-defined-out))


;; All interface types

(define-named-lister interfaces
                     ("class" "net") #:nodes)

(define-named-getter interface-address
                     ("class" "net")
                     ("address"))

(define-named-getter interface-broadcast
                     ("class" "net")
                     ("broadcast"))

(define-named-getter interface-address-length
                     ("class" "net")
                     ("addr_len"))

(define-named-getter interface-index
                     ("class" "net")
                     ("ifindex")
                     string->number)

(define-named-getter interface-link-index
                     ("class" "net")
                     ("iflink")
                     string->number)

(define-named-getter interface-carrier
                     ("class" "net")
                     ("carrier")
                     string->boolean)

(define-named-getter interface-operstate
                     ("class" "net")
                     ("operstate")
                     string->symbol)

(define-named-accessors interface-mtu
                        ("class" "net")
                        ("mtu")
                        string->number
                        number->string)

(define-named-accessors interface-tx-queue-len
                        ("class" "net")
                        ("tx_queue_len")
                        string->number
                        number->string)

(define (interface-physical? name)
  (and (sysfs-list "class" "net" name "device") #t))


;; Bridges themselves

(define-named-lister bridges
                     ("class" "net") #:nodes
                     interface-bridge?)

(define (list-bridge-ports bridge)
  (sysfs-list/nodes "class" "net" bridge "brif"))

(define (interface-bridge? name)
  (and (get-bridge-id name) #t))

(define-named-accessors bridge-id
                        ("class" "net")
                        ("bridge" "bridge_id"))

(define-named-accessors bridge-fd
                        ("class" "net")
                        ("bridge" "forward_delay")
                        string->number
                        number->string)

(define-named-accessors bridge-ageing
                        ("class" "net")
                        ("bridge" "ageing_time")
                        string->number
                        number->string)

(define-named-accessors bridge-priority
                        ("class" "net")
                        ("bridge" "priority")
                        string->number
                        number->string)

(define-named-accessors bridge-hello
                        ("class" "net")
                        ("bridge" "hello_time")
                        string->number
                        number->string)

(define-named-accessors bridge-maxage
                        ("class" "net")
                        ("bridge" "max_age")
                        string->number
                        number->string)

(define-named-accessors bridge-path-cost
                        ("class" "net")
                        ("bridge" "root_path_cost")
                        string->number
                        number->string)

(define-named-accessors bridge-stp
                        ("class" "net")
                        ("bridge" "stp_state")
                        string->boolean
                        boolean->string)


;; Bridge ports

(define-named-accessors bridge-port-hairpin
                        ("class" "net")
                        ("brport" "hairpin_mode")
                        string->boolean
                        boolean->string)

(define-named-accessors bridge-port-path-cost
                        ("class" "net")
                        ("brport" "path_cost")
                        string->number
                        number->string)

(define-named-accessors bridge-port-path-priority
                        ("class" "net")
                        ("brport" "path_cost")
                        string->number
                        number->string)


;; Bonding masters

(define-named-lister bonds
                     ("class" "net") #:nodes
                     interface-bond?)

(define (interface-bond? name)
  (and (get-bond-xmit-hash-policy name) #t))

(define (list-bond-slaves bond)
  (string-split (sysfs-get "class" "net" bond "bonding" "slaves")))

(define (bond-slave-add bond slave)
  (sysfs-set! "class" "net" bond "bonding" "slaves"
              #:value (string-append "+" slave)))

(define (bond-slave-remove bond slave)
  (sysfs-set! "class" "net" bond "bonding" "slaves"
              #:value (string-append "-" slave)))

(define (add-bond name)
  (sysfs-set! "class" "net" "bonding_masters"
              #:value (string-append "+" name)))

(define (remove-bond name)
  (sysfs-set! "class" "net" "bonding_masters"
              #:value (string-append "-" name)))

(define-named-accessors bond-xmit-hash-policy
                        ("class" "net")
                        ("bonding" "xmit_hash_policy")
                        first-word
                        values)

(define-named-accessors bond-miimon
                        ("class" "net")
                        ("bonding" "miimon")
                        string->number
                        number->string)

(define-named-accessors bond-lacp-rate
                        ("class" "net")
                        ("bonding" "lacp_rate")
                        first-word
                        values)

(define-named-accessors bond-arp-interval
                        ("class" "net")
                        ("bonding" "arp_interval")
                        string->number
                        number->string)

(define-named-accessors bond-arp-validate
                        ("class" "net")
                        ("bonding" "arp_validate")
                        first-word
                        values)

(define-named-accessors bond-arp-ip-target
                        ("class" "net")
                        ("bonding" "arp_ip_target"))

(define-named-accessors bond-mode
                        ("class" "net")
                        ("bonding" "mode")
                        first-word
                        values)

(define-named-accessors bond-primary
                        ("class" "net")
                        ("bonding" "primary"))

(define-named-accessors bond-downdelay
                        ("class" "net")
                        ("bonding" "downdelay")
                        string->number
                        number->string)

(define-named-accessors bond-updelay
                        ("class" "net")
                        ("bonding" "updelay")
                        string->number
                        number->string)

(define-named-accessors bond-ad-select
                        ("class" "net")
                        ("bonding" "ad_select")
                        first-word
                        values)

(define-named-accessors bond-min-links
                        ("class" "net")
                        ("bonding" "min_links")
                        string->number
                        number->string)

(define-named-accessors bond-use-carrier
                        ("class" "net")
                        ("bonding" "use_carrier")
                        string->boolean
                        boolean->string)

(define-named-accessors bond-primary-reselect
                        ("class" "net")
                        ("bonding" "primary_reselect")
                        first-word
                        values)

(define-named-accessors bond-fail-over-mac
                        ("class" "net")
                        ("bonding" "fail_over_mac")
                        first-word
                        values)

(define-named-accessors bond-all-slaves-active
                        ("class" "net")
                        ("bonding" "all_slaves_active")
                        string->boolean
                        boolean->string)

(define-named-accessors bond-resend-igmp
                        ("class" "net")
                        ("bonding" "resend_igmp")
                        string->boolean
                        boolean->string)

(define-named-getter bond-ad-actor-key
                     ("class" "net")
                     ("bonding" "ad_actor_key"))

(define-named-getter bond-aggregator
                     ("class" "net")
                     ("bonding" "ad_aggregator"))

(define-named-getter bond-ad-num-ports
                     ("class" "net")
                     ("bonding" "ad_num_ports")
                     string->number)

(define-named-getter bond-ad-parter-key
                     ("class" "net")
                     ("bonding" "ad_partner_key"))

(define-named-getter bond-ad-partner-mac
                     ("class" "net")
                     ("bonding" "ad_partner_mac"))

(define-named-getter bond-num-grat-arp
                     ("class" "net")
                     ("bonding" "num_grat_arp")
                     string->number)

(define-named-getter bond-num-unsol-na
                     ("class" "net")
                     ("bonding" "num_unsol_na")
                     string->number)


; vim:set ts=2 sw=2 et:
