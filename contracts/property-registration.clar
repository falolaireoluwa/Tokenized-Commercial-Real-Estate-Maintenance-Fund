;; Property Registration Contract
;; Records details of commercial buildings

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-found (err u102))

;; Data structures
(define-map properties
  { property-id: uint }
  {
    owner: principal,
    address: (string-ascii 100),
    square-footage: uint,
    construction-year: uint,
    last-updated: uint
  }
)

(define-map property-owners
  { owner: principal }
  { property-count: uint }
)

;; Property registration
(define-public (register-property
                (property-id uint)
                (address (string-ascii 100))
                (square-footage uint)
                (construction-year uint))
  (let ((owner tx-sender))
    (asserts! (is-none (map-get? properties { property-id: property-id })) err-already-registered)

    ;; Update property count for owner
    (match (map-get? property-owners { owner: owner })
      existing-data (map-set property-owners
                      { owner: owner }
                      { property-count: (+ u1 (get property-count existing-data)) })
      (map-insert property-owners
                  { owner: owner }
                  { property-count: u1 })
    )

    ;; Store property details
    (map-insert properties
                { property-id: property-id }
                {
                  owner: owner,
                  address: address,
                  square-footage: square-footage,
                  construction-year: construction-year,
                  last-updated: block-height
                })
    (ok property-id)
  )
)

;; Update property information
(define-public (update-property
                (property-id uint)
                (address (string-ascii 100))
                (square-footage uint))
  (let ((property (map-get? properties { property-id: property-id })))
    (asserts! (is-some property) err-not-found)
    (asserts! (is-eq (get owner (unwrap-panic property)) tx-sender) err-owner-only)

    (map-set properties
              { property-id: property-id }
              (merge (unwrap-panic property)
                    {
                      address: address,
                      square-footage: square-footage,
                      last-updated: block-height
                    }))
    (ok property-id)
  )
)

;; Read property information
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

;; Get property count for an owner
(define-read-only (get-property-count (owner principal))
  (default-to { property-count: u0 } (map-get? property-owners { owner: owner }))
)
