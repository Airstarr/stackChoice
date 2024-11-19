
;; title: benefit-marketplace
;; version:
;; summary:
;; description:

;; Employee Benefits Marketplace Contract
;; Manages benefit listings, purchases, and refunds in the employee benefits system

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant TOKEN_CONTRACT 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token-distribution)
(define-constant REFUND_PERIOD u4320) ;; ~30 days in blocks
(define-constant MIN_PRICE u1)
(define-constant MAX_PRICE u1000000)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PRICE (err u101))
(define-constant ERR-BENEFIT-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-PURCHASED (err u103))
(define-constant ERR-REFUND-EXPIRED (err u104))
(define-constant ERR-NOT-PURCHASER (err u105))
(define-constant ERR-INVALID-BENEFIT (err u106))

;; Benefit types
(define-constant TYPE-HEALTHCARE u1)
(define-constant TYPE-FITNESS u2)
(define-constant TYPE-INSURANCE u3)
(define-constant TYPE-WELLNESS u4)

;; Data structures
(define-map vendors 
  principal 
  {name: (string-ascii 64), 
   active: bool,
   total-sales: uint})

(define-map benefits 
  uint 
  {vendor: principal,
   name: (string-ascii 64),
   description: (string-ascii 256),
   benefit-type: uint,
   base-price: uint,
   current-price: uint,
   available: bool,
   seasonal-multiplier: uint,
   demand-multiplier: uint,
   total-purchased: uint})

(define-map purchases
  {benefit-id: uint, purchaser: principal}
  {purchase-height: uint,
   purchaser: principal,
   price-paid: uint,
   active: bool,
   refunded: bool})

(define-map user-benefits
  principal
  (list 10 uint))

;; Data vars
(define-data-var benefit-counter uint u0)
(define-data-var season uint u1) ;; 1-4 for quarters

;; Authorization check
(define-private (is-vendor (account principal))
  (get active (default-to {name: "", active: false, total-sales: u0} 
    (map-get? vendors account))))

;; Register vendor
(define-public (register-vendor (name (string-ascii 64)))
  (begin
    (asserts! (not (is-vendor tx-sender)) ERR-NOT-AUTHORIZED)
    (ok (map-set vendors tx-sender 
         {name: name,
          active: true,
          total-sales: u0}))))

;; List new benefit
(define-public (list-benefit 
    (name (string-ascii 64))
    (description (string-ascii 256))
    (benefit-type uint)
    (base-price uint))
  (let ((benefit-id (var-get benefit-counter)))
    (asserts! (is-vendor tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= base-price MIN_PRICE) (<= base-price MAX_PRICE)) ERR-INVALID-PRICE)
    
    (map-set benefits benefit-id
      {vendor: tx-sender,
       name: name,
       description: description,
       benefit-type: benefit-type,
       base-price: base-price,
       current-price: base-price,
       available: true,
       seasonal-multiplier: u100,
       demand-multiplier: u100,
       total-purchased: u0})
    
    (var-set benefit-counter (+ benefit-id u1))
    (ok benefit-id)))

;; Calculate dynamic price
(define-private (calculate-price (benefit-id uint))
  (let 
    (
      (benefit (unwrap! (map-get? benefits benefit-id) ERR-BENEFIT-NOT-FOUND))
      (adjusted-price (* (get base-price benefit) (/ (* (get seasonal-multiplier benefit) (get demand-multiplier benefit)) u10000)) )
    )
    (ok adjusted-price)
  )
)

;; Update price multipliers
(define-public (update-multipliers 
    (benefit-id uint)
    (seasonal-mult uint)
    (demand-mult uint))
  (let ((benefit (unwrap! (map-get? benefits benefit-id) ERR-BENEFIT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get vendor benefit)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= seasonal-mult u50) (<= seasonal-mult u200)) ERR-INVALID-PRICE)
    (asserts! (and (>= demand-mult u50) (<= demand-mult u200)) ERR-INVALID-PRICE)
    
    (ok (map-set benefits benefit-id
      (merge benefit 
        {seasonal-multiplier: seasonal-mult,
         demand-multiplier: demand-mult,
         current-price: (try! (calculate-price benefit-id))}
  )))))

;; Purchase benefit
(define-public (purchase-benefit (benefit-id uint))
  (let ((benefit (unwrap! (map-get? benefits benefit-id) ERR-BENEFIT-NOT-FOUND))
        (price (try! (calculate-price benefit-id)))
        (current-benefits (default-to (list) (map-get? user-benefits tx-sender))))
    
    (asserts! (get available benefit) ERR-INVALID-BENEFIT)
    (asserts! (not (is-some (map-get? purchases {benefit-id: benefit-id, purchaser: tx-sender}))) 
              ERR-ALREADY-PURCHASED)
    
    ;; Transfer tokens from purchaser to vendor
    (try! (contract-call? TOKEN_CONTRACT transfer (get vendor benefit) price))
    
    ;; Record purchase
    (map-set purchases 
      {benefit-id: benefit-id, purchaser: tx-sender}
      {purchase-height: block-height,
      purchaser: tx-sender,
       price-paid: price,
       active: true,
       refunded: false})
    
    ;; Update user's benefits list
    (map-set user-benefits tx-sender (unwrap! (as-max-len? (append current-benefits benefit-id) u10)
                                             ERR-INVALID-BENEFIT))
    
    ;; Update benefit stats
    (map-set benefits benefit-id
      (merge benefit
        {total-purchased: (+ (get total-purchased benefit) u1)}))
    
    ;; Update vendor stats
    (map-set vendors (get vendor benefit)
      (merge (unwrap! (map-get? vendors (get vendor benefit)) ERR-NOT-AUTHORIZED)
        {total-sales: (+ price (get total-sales 
          (unwrap! (map-get? vendors (get vendor benefit)) ERR-NOT-AUTHORIZED)))}))
    
    (ok true)))

;; Request refund
(define-public (request-refund (benefit-id uint))
  (let 
    (
      (purchase (unwrap! (map-get? purchases {benefit-id: benefit-id, purchaser: tx-sender}) ERR-BENEFIT-NOT-FOUND))
      (benefit (unwrap! (map-get? benefits benefit-id) ERR-BENEFIT-NOT-FOUND))
    )
    
    (asserts! (is-eq tx-sender (get purchaser purchase)) ERR-NOT-PURCHASER)
    (asserts! (not (get refunded purchase)) ERR-REFUND-EXPIRED)
    (asserts! (<= (- block-height (get purchase-height purchase)) REFUND_PERIOD) 
              ERR-REFUND-EXPIRED)
    
    ;; Transfer tokens back to purchaser
    (try! (contract-call? TOKEN_CONTRACT transfer 
           tx-sender 
           (get price-paid purchase)))
    
    ;; Update purchase record
    (map-set purchases 
      {benefit-id: benefit-id, purchaser: tx-sender}
      (merge purchase {active: false, refunded: true}))
    
    ;; Update benefit stats
    (map-set benefits benefit-id
      (merge benefit
        {total-purchased: (- (get total-purchased benefit) u1)}))
    
    (ok true)))

;; Getter functions
(define-read-only (get-benefit (benefit-id uint))
  (map-get? benefits benefit-id))

(define-read-only (get-purchase-info (benefit-id uint) (purchaser principal))
  (map-get? purchases {benefit-id: benefit-id, purchaser: purchaser}))

(define-read-only (get-user-benefits (user principal))
  (map-get? user-benefits user))

(define-read-only (get-vendor-info (vendor principal))
  (map-get? vendors vendor))

;; Update seasonal multiplier (admin only)
(define-public (update-season (new-season uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-season u1) (<= new-season u4)) ERR-INVALID-BENEFIT)
    (var-set season new-season)
    (ok true)))