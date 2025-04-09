;; Fund Contribution Contract
;; Manages deposits from property owners

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))

;; Data structures
(define-map fund-balances
  { property-id: uint }
  { balance: uint }
)

(define-map contribution-history
  { property-id: uint, tx-id: uint }
  {
    amount: uint,
    block: uint,
    contributor: principal
  }
)

(define-data-var total-fund-balance uint u0)
(define-data-var next-tx-id uint u1)

;; Contribute to the fund
(define-public (contribute (property-id uint) (amount uint))
  (let ((property-contract (contract-call? .property-registration get-property property-id))
        (tx-id (var-get next-tx-id)))

    ;; Check property exists and sender is authorized
    (asserts! (is-some property-contract) err-not-found)
    (asserts! (is-eq (get owner (unwrap-panic property-contract)) tx-sender) err-unauthorized)

    ;; Check if sender has enough tokens
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)

    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update balances
    (var-set total-fund-balance (+ (var-get total-fund-balance) amount))

    ;; Update property's fund balance
    (match (map-get? fund-balances { property-id: property-id })
      existing-balance (map-set fund-balances
                         { property-id: property-id }
                         { balance: (+ (get balance existing-balance) amount) })
      (map-insert fund-balances
                 { property-id: property-id }
                 { balance: amount })
    )

    ;; Record contribution history
    (map-insert contribution-history
               { property-id: property-id, tx-id: tx-id }
               {
                 amount: amount,
                 block: block-height,
                 contributor: tx-sender
               })

    ;; Increment transaction ID counter
    (var-set next-tx-id (+ tx-id u1))

    (ok tx-id)
  )
)

;; Get fund balance for a property
(define-read-only (get-property-balance (property-id uint))
  (default-to { balance: u0 }
              (map-get? fund-balances { property-id: property-id }))
)

;; Get total fund balance
(define-read-only (get-total-balance)
  (var-get total-fund-balance)
)

;; Get contribution history for a property
(define-read-only (get-contribution (property-id uint) (tx-id uint))
  (map-get? contribution-history { property-id: property-id, tx-id: tx-id })
)
