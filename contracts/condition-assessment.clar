;; Condition Assessment Contract
;; Tracks building systems and components

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))

;; System condition ratings
(define-constant CONDITION-EXCELLENT u5)
(define-constant CONDITION-GOOD u4)
(define-constant CONDITION-FAIR u3)
(define-constant CONDITION-POOR u2)
(define-constant CONDITION-CRITICAL u1)

;; Data structures
(define-map building-systems
  { property-id: uint, system-id: uint }
  {
    name: (string-ascii 50),
    condition: uint,
    installation-year: uint,
    expected-life: uint,
    last-assessment: uint
  }
)

;; Add or update building system
(define-public (add-system
                (property-id uint)
                (system-id uint)
                (name (string-ascii 50))
                (condition uint)
                (installation-year uint)
                (expected-life uint))
  (let ((property-contract (contract-call? .property-registration get-property property-id)))
    (asserts! (is-some property-contract) err-not-found)
    (asserts! (is-eq (get owner (unwrap-panic property-contract)) tx-sender) err-unauthorized)

    (map-set building-systems
              { property-id: property-id, system-id: system-id }
              {
                name: name,
                condition: condition,
                installation-year: installation-year,
                expected-life: expected-life,
                last-assessment: block-height
              })
    (ok true)
  )
)

;; Update system condition
(define-public (update-system-condition
                (property-id uint)
                (system-id uint)
                (new-condition uint))
  (let ((system (map-get? building-systems { property-id: property-id, system-id: system-id }))
        (property-contract (contract-call? .property-registration get-property property-id)))
    (asserts! (is-some system) err-not-found)
    (asserts! (is-some property-contract) err-not-found)
    (asserts! (is-eq (get owner (unwrap-panic property-contract)) tx-sender) err-unauthorized)

    (map-set building-systems
              { property-id: property-id, system-id: system-id }
              (merge (unwrap-panic system)
                    {
                      condition: new-condition,
                      last-assessment: block-height
                    }))
    (ok true)
  )
)

;; Get system details
(define-read-only (get-system (property-id uint) (system-id uint))
  (map-get? building-systems { property-id: property-id, system-id: system-id })
)

;; Get remaining system life
(define-read-only (get-system-remaining-life (property-id uint) (system-id uint))
  (match (map-get? building-systems { property-id: property-id, system-id: system-id })
    system (let ((age (- block-height (get installation-year system))))
             (if (> (get expected-life system) age)
                 (ok (- (get expected-life system) age))
                 (ok u0)))
    (err err-not-found)
  )
)
