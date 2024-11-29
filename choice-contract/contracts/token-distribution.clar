
;; title: token-distribution
;; version:
;; summary:
;; description:

;; Employee Token Distribution Contract
;; Manages token distribution, transfers, and balances for an employee rewards system

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant TOKEN_SYMBOL "EMPT")
(define-constant TOKEN_NAME "Employee Token")
(define-constant TOKEN_DECIMALS u6)
(define-constant EXPIRATION_BLOCKS u52560) ;; ~1 year in blocks

;; Data vars
(define-data-var total-supply uint u0)

;; Data maps
(define-map balances principal uint)
(define-map token-metadata
  {holder: principal}
  {last-allocation: uint, expiration-height: uint})
(define-map authorized-employers principal bool)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-RECIPIENT (err u102))
(define-constant ERR-EXPIRED-TOKENS (err u103))

;; Authorization check
(define-private (is-authorized)
  (default-to false (some (get-authorized-employer tx-sender))))

;; Get balance
(define-read-only (get-balance (account principal))
  (default-to u0 (map-get? balances account)))

;; Get authorized employer status
(define-read-only (get-authorized-employer (employer principal))
  (default-to false (map-get? authorized-employers employer)))

;; Add authorized employer
(define-public (add-employer (employer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-employers employer true))))

;; Remove authorized employer
(define-public (remove-employer (employer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-employers employer false))))

;; Allocate tokens to employee
(define-public (allocate-tokens (employee principal) (amount uint))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-RECIPIENT)
    
    (let ((current-balance (get-balance employee))
          (new-balance (+ current-balance amount))
          (expiration-height (+ block-height EXPIRATION_BLOCKS)))
      
      (map-set balances employee new-balance)
      (map-set token-metadata
        {holder: employee}
        {last-allocation: block-height,
         expiration-height: expiration-height})
      
      (var-set total-supply (+ (var-get total-supply) amount))
      (ok true))))

;; Transfer tokens between employees
(define-public (transfer (recipient principal) (amount uint))
  (let ((sender-balance (get-balance tx-sender))
        (recipient-balance (get-balance recipient))
        (sender-metadata (unwrap! (map-get? token-metadata {holder: tx-sender}) ERR-INVALID-RECIPIENT)))
    
    (asserts! (<= amount sender-balance) ERR-INSUFFICIENT-BALANCE)
    (asserts! (< block-height (get expiration-height sender-metadata)) ERR-EXPIRED-TOKENS)
    
    (map-set balances
      tx-sender
      (- sender-balance amount))
    
    (map-set balances
      recipient
      (+ recipient-balance amount))
    
    (ok true)))

;; Burn expired tokens
(define-public (burn-expired-tokens (employee principal))
  (let ((balance (get-balance employee))
        (metadata (unwrap! (map-get? token-metadata {holder: employee}) ERR-INVALID-RECIPIENT)))
    
    (asserts! (>= block-height (get expiration-height metadata)) ERR-EXPIRED-TOKENS)
    
    (map-set balances employee u0)
    (var-set total-supply (- (var-get total-supply) balance))
    (ok true)))

;; Top-up tokens (for special events, performance rewards, etc.)
(define-public (top-up-tokens (employee principal) (amount uint))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-RECIPIENT)
    
    (let ((current-balance (get-balance employee))
          (new-balance (+ current-balance amount)))
      
      (map-set balances employee new-balance)
      (var-set total-supply (+ (var-get total-supply) amount))
      (ok true))))

;; Get token metadata for an employee
(define-read-only (get-token-metadata (employee principal))
  (map-get? token-metadata {holder: employee}))

;; Get total supply
(define-read-only (get-total-supply)
  (ok (var-get total-supply)))
;;

