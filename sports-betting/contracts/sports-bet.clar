;; Sports Betting Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-does-not-exist (err u102))
(define-constant err-bet-closed (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-settled (err u105))

;; Data variables
(define-data-var next-betting-event-id uint u0)

;; Define bet structure
(define-map bets
  { betting-event-id: uint }
  {
    event-creator: principal,
    event-description: (string-ascii 256),
    betting-options: (list 10 (string-ascii 64)),
    total-staked-amount: uint,
    is-betting-open: bool,
    winning-option: (optional uint),
    betting-close-height: uint
  }
)

;; Define stakes structure
(define-map stakes
  { betting-event-id: uint, bettor: principal }
  { chosen-option: uint, staked-amount: uint }
)

;; Read-only functions

(define-read-only (get-bet (betting-event-id uint))
  (map-get? bets { betting-event-id: betting-event-id })
)

(define-read-only (get-stake (betting-event-id uint) (bettor principal))
  (map-get? stakes { betting-event-id: betting-event-id, bettor: bettor })
)

;; Public functions

(define-public (create-bet (event-description (string-ascii 256)) (betting-options (list 10 (string-ascii 64))) (betting-close-height uint))
  (let
    (
      (new-betting-event-id (var-get next-betting-event-id))
    )
    (asserts! (> (len betting-options) u1) (err u106))
    (asserts! (< betting-close-height block-height) (err u107))
    (map-set bets
      { betting-event-id: new-betting-event-id }
      {
        event-creator: tx-sender,
        event-description: event-description,
        betting-options: betting-options,
        total-staked-amount: u0,
        is-betting-open: true,
        winning-option: none,
        betting-close-height: betting-close-height
      }
    )
    (var-set next-betting-event-id (+ new-betting-event-id u1))
    (ok new-betting-event-id)
  )
)

(define-public (place-stake (betting-event-id uint) (chosen-option uint) (stake-amount uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
      (existing-stake (default-to { chosen-option: u0, staked-amount: u0 } (get-stake betting-event-id tx-sender)))
    )
    (asserts! (get is-betting-open bet) err-bet-closed)
    (asserts! (>= (len (get betting-options bet)) chosen-option) (err u108))
    (asserts! (<= (get betting-close-height bet) block-height) (err u109))
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set stakes
      { betting-event-id: betting-event-id, bettor: tx-sender }
      {
        chosen-option: chosen-option,
        staked-amount: (+ stake-amount (get staked-amount existing-stake))
      }
    )
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { total-staked-amount: (+ (get total-staked-amount bet) stake-amount) })
    )
    (ok true)
  )
)

(define-public (close-bet (betting-event-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
    )
    (asserts! (is-eq (get event-creator bet) tx-sender) err-unauthorized)
    (asserts! (get is-betting-open bet) err-bet-closed)
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { is-betting-open: false })
    )
    (ok true)
  )
)

(define-public (settle-bet (betting-event-id uint) (winning-option-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
    )
    (asserts! (is-eq contract-owner tx-sender) err-unauthorized)
    (asserts! (not (get is-betting-open bet)) err-bet-closed)
    (asserts! (is-none (get winning-option bet)) err-already-settled)
    (asserts! (>= (len (get betting-options bet)) winning-option-id) (err u110))
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { winning-option: (some winning-option-id) })
    )
    (ok true)
  )
)

(define-public (claim-winnings (betting-event-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
      (stake (unwrap! (get-stake betting-event-id tx-sender) err-does-not-exist))
      (winning-option-id (unwrap! (get winning-option bet) err-does-not-exist))
    )
    (asserts! (is-eq (get chosen-option stake) winning-option-id) (err u111))
    (let
      (
        (total-event-stake (get total-staked-amount bet))
        (winner-total-stake (get staked-amount stake))
        (payout (/ (* winner-total-stake total-event-stake) (fold + (map get-option-total-staked-amount (get betting-options bet)) u0)))
      )
      (try! (as-contract (stx-transfer? payout tx-sender tx-sender)))
      (map-delete stakes { betting-event-id: betting-event-id, bettor: tx-sender })
      (ok payout)
    )
  )
)

;; Private functions

(define-private (get-option-total-staked-amount (option (string-ascii 64)))
  (default-to u0 (get staked-amount (map-get? stakes { betting-event-id: betting-event-id, bettor: tx-sender })))
)

;; Contract initialization
(begin
  (var-set next-betting-event-id u0)
)