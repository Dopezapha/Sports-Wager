;; Sports Betting Contract

;; Error Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-does-not-exist (err u102))
(define-constant err-bet-closed (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-settled (err u105))
(define-constant err-bet-not-closable (err u106))
(define-constant err-bet-not-cancelable (err u107))

;; Data variables
(define-data-var next-betting-event-id uint u0)

;; Bet types
(define-data-var bet-types (list 10 (string-ascii 20)) (list "winner-take-all" "proportional" "fixed-odds"))

;; Define bet structure
(define-map bets
  { betting-event-id: uint }
  {
    event-creator: principal,
    event-description: (string-ascii 256),
    betting-options: (list 10 (string-ascii 64)),
    total-staked-amount: uint,
    is-betting-open: bool,
    winning-options: (list 5 uint),
    betting-close-height: uint,
    bet-type: (string-ascii 20),
    odds: (optional (list 10 uint))
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

(define-read-only (get-current-block-height)
  block-height
)

;; Public functions

(define-public (create-bet (event-description (string-ascii 256)) (betting-options (list 10 (string-ascii 64))) (betting-close-height uint) (bet-type (string-ascii 20)) (odds (optional (list 10 uint))))
  (let
    (
      (new-betting-event-id (var-get next-betting-event-id))
    )
    (asserts! (> (len betting-options) u1) (err u108))
    (asserts! (> betting-close-height block-height) (err u109))
    (asserts! (is-some (index-of (var-get bet-types) bet-type)) (err u110))
    (asserts! (or (is-eq bet-type "winner-take-all") (is-eq bet-type "proportional") (is-some odds)) (err u111))
    (map-set bets
      { betting-event-id: new-betting-event-id }
      {
        event-creator: tx-sender,
        event-description: event-description,
        betting-options: betting-options,
        total-staked-amount: u0,
        is-betting-open: true,
        winning-options: (list),
        betting-close-height: betting-close-height,
        bet-type: bet-type,
        odds: odds
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
    (asserts! (>= (len (get betting-options bet)) chosen-option) (err u112))
    (asserts! (< block-height (get betting-close-height bet)) (err u113))
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
    (asserts! (or (is-eq (get event-creator bet) tx-sender) (is-eq contract-owner tx-sender)) err-unauthorized)
    (asserts! (get is-betting-open bet) err-bet-closed)
    (asserts! (>= block-height (get betting-close-height bet)) err-bet-not-closable)
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { is-betting-open: false })
    )
    (ok true)
  )
)

(define-public (cancel-bet (betting-event-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
    )
    (asserts! (is-eq (get event-creator bet) tx-sender) err-unauthorized)
    (asserts! (get is-betting-open bet) err-bet-closed)
    (asserts! (< block-height (get betting-close-height bet)) err-bet-not-cancelable)
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { is-betting-open: false })
    )
    (ok (refund-all-stakes betting-event-id))
  )
)

(define-public (settle-bet (betting-event-id uint) (winning-option-ids (list 5 uint)))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
    )
    (asserts! (is-eq contract-owner tx-sender) err-unauthorized)
    (asserts! (not (get is-betting-open bet)) err-bet-closed)
    (asserts! (is-eq (len (get winning-options bet)) u0) err-already-settled)
    (asserts! (> (len winning-option-ids) u0) (err u114))
    (asserts! (<= (len winning-option-ids) u5) (err u115))
    (asserts! (fold and (map check-option-validity winning-option-ids (get betting-options bet)) true) (err u116))
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { winning-options: winning-option-ids })
    )
    (ok true)
  )
)

(define-public (claim-winnings (betting-event-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) err-does-not-exist))
      (stake (unwrap! (get-stake betting-event-id tx-sender) err-does-not-exist))
      (winning-option-ids (get winning-options bet))
    )
    (asserts! (is-some (index-of winning-option-ids (get chosen-option stake))) (err u117))
    (let
      (
        (payout (calculate-payout bet stake winning-option-ids))
      )
      (try! (as-contract (stx-transfer? payout tx-sender tx-sender)))
      (map-delete stakes { betting-event-id: betting-event-id, bettor: tx-sender })
      (ok payout)
    )
  )
)

;; Private functions

(define-private (calculate-payout (bet { event-creator: principal, event-description: (string-ascii 256), betting-options: (list 10 (string-ascii 64)), total-staked-amount: uint, is-betting-open: bool, winning-options: (list 5 uint), betting-close-height: uint, bet-type: (string-ascii 20), odds: (optional (list 10 uint)) }) (stake { chosen-option: uint, staked-amount: uint }) (winning-option-ids (list 5 uint)))
  (let
    (
      (bet-type (get bet-type bet))
      (total-event-stake (get total-staked-amount bet))
      (winner-stake (get staked-amount stake))
    )
    (if (is-eq bet-type "winner-take-all")
      (/ (* winner-stake total-event-stake) (fold + (map get-option-total-staked-amount winning-option-ids) u0))
      (if (is-eq bet-type "proportional")
        (/ (* winner-stake total-event-stake) (get-option-total-staked-amount (get chosen-option stake)))
        ;; Fixed-odds payout
        (let
          (
            (odds-list (unwrap! (get odds bet) u0))
            (chosen-odds (unwrap! (element-at odds-list (- (get chosen-option stake) u1)) u0))
          )
          (+ winner-stake (* winner-stake (/ chosen-odds u100)))
        )
      )
    )
  )
)

(define-private (get-option-total-staked-amount (option uint))
  (default-to u0 (fold + (map get-stake-amount-for-option (map-to-list stakes)) u0))
)

(define-private (get-stake-amount-for-option (stake { betting-event-id: uint, bettor: principal, chosen-option: uint, staked-amount: uint }))
  (if (is-eq (get chosen-option stake) option)
    (get staked-amount stake)
    u0
  )
)

(define-private (check-option-validity (option uint) (valid-options (list 10 (string-ascii 64))))
  (< option (len valid-options))
)

(define-private (refund-all-stakes (betting-event-id uint))
  (map refund-stake (map-to-list stakes))
)

(define-private (refund-stake (stake { betting-event-id: uint, bettor: principal, chosen-option: uint, staked-amount: uint }))
  (if (is-eq (get betting-event-id stake) betting-event-id)
    (begin
      (try! (as-contract (stx-transfer? (get staked-amount stake) (get bettor stake) (get bettor stake))))
      (map-delete stakes { betting-event-id: betting-event-id, bettor: (get bettor stake) })
      true
    )
    false
  )
)

;; Contract initialization
(begin
  (var-set next-betting-event-id u0)
)

;; Export the Component function
(define-public (Component)
  (ok true)
)