;; Sports Betting Contract

;; Error Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-DOES-NOT-EXIST (err u102))
(define-constant ERR-BET-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-ALREADY-SETTLED (err u105))
(define-constant ERR-BET-NOT-CLOSABLE (err u106))
(define-constant ERR-BET-NOT-CANCELABLE (err u107))
(define-constant ERR-INVALID-OPTION-COUNT (err u108))
(define-constant ERR-INVALID-CLOSE-HEIGHT (err u109))
(define-constant ERR-INVALID-BET-TYPE (err u110))
(define-constant ERR-MISSING-ODDS (err u111))
(define-constant ERR-INVALID-OPTION (err u112))
(define-constant ERR-BET-EXPIRED (err u113))
(define-constant ERR-NO-WINNING-OPTIONS (err u114))
(define-constant ERR-TOO-MANY-WINNERS (err u115))
(define-constant ERR-INVALID-WINNER (err u116))
(define-constant ERR-NOT-A-WINNER (err u117))
(define-constant ERR-REFUND-FAILED (err u118))
(define-constant ERR-REFUND-PROCESSING (err u119))
(define-constant ERR-INVALID-DESCRIPTION (err u120))
(define-constant ERR-INVALID-STAKE-AMOUNT (err u121))

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

;; Private functions

(define-private (calculate-payout (bet { event-creator: principal, event-description: (string-ascii 256), betting-options: (list 10 (string-ascii 64)), total-staked-amount: uint, is-betting-open: bool, winning-options: (list 5 uint), betting-close-height: uint, bet-type: (string-ascii 20), odds: (optional (list 10 uint)) }) (stake { chosen-option: uint, staked-amount: uint }) (winning-option-ids (list 5 uint)))
  (let
    (
      (bet-type (get bet-type bet))
      (total-event-stake (get total-staked-amount bet))
      (winner-stake (get staked-amount stake))
    )
    (if (is-eq bet-type "winner-take-all")
      ;; For winner-take-all, divide total pot by number of winning options
      (/ total-event-stake (len winning-option-ids))
      (if (is-eq bet-type "proportional")
        ;; For proportional, payout based on stake ratio
        (/ (* winner-stake total-event-stake) total-event-stake)
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

(define-private (get-stake-amount-for-option-and-bet (option uint) (betting-event-id uint))
  (let
    (
      (stake (get-stake betting-event-id tx-sender))
    )
    (if (is-some stake)
      (let
        ((stake-data (unwrap! stake u0)))
        (if (is-eq (get chosen-option stake-data) option)
          (get staked-amount stake-data)
          u0
        )
      )
      u0
    )
  )
)

(define-private (get-option-total-staked-amount (option uint))
  (get-stake-amount-for-option-and-bet option (var-get next-betting-event-id))
)

(define-private (process-refunds (betting-event-id uint))
  (let
    ((stake (get-stake betting-event-id tx-sender)))
    (match stake
      stake-data (match (as-contract (stx-transfer? (get staked-amount stake-data) tx-sender tx-sender))
        success (begin
          (map-delete stakes { betting-event-id: betting-event-id, bettor: tx-sender })
          (ok true)
        )
        error ERR-REFUND-FAILED
      )
      ERR-REFUND-PROCESSING
    )
  )
)

(define-private (validate-options-helper (options (list 5 uint)) (max-option uint))
  (let
    (
      (option-1 (element-at options u0))
      (option-2 (element-at options u1))
      (option-3 (element-at options u2))
      (option-4 (element-at options u3))
      (option-5 (element-at options u4))
    )
    (and
      ;; Check if first option exists and is valid
      (match option-1
        value (and (> value u0) (<= value max-option))
        true)
      ;; For remaining options, they're either valid or none
      (match option-2
        value (and (> value u0) (<= value max-option))
        true)
      (match option-3
        value (and (> value u0) (<= value max-option))
        true)
      (match option-4
        value (and (> value u0) (<= value max-option))
        true)
      (match option-5
        value (and (> value u0) (<= value max-option))
        true)
    )
  )
)

;; Public functions

(define-public (create-bet (event-description (string-ascii 256)) (betting-options (list 10 (string-ascii 64))) (betting-close-height uint) (bet-type (string-ascii 20)) (odds (optional (list 10 uint))))
  (let
    (
      (new-betting-event-id (var-get next-betting-event-id))
    )
    (asserts! (> (len event-description) u0) ERR-INVALID-DESCRIPTION)
    (asserts! (> (len betting-options) u1) ERR-INVALID-OPTION-COUNT)
    (asserts! (> betting-close-height block-height) ERR-INVALID-CLOSE-HEIGHT)
    (asserts! (is-some (index-of (var-get bet-types) bet-type)) ERR-INVALID-BET-TYPE)
    (asserts! (or (is-eq bet-type "winner-take-all") (is-eq bet-type "proportional") (is-some odds)) ERR-MISSING-ODDS)
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
      (bet (unwrap! (get-bet betting-event-id) ERR-DOES-NOT-EXIST))
      (existing-stake (default-to { chosen-option: u0, staked-amount: u0 } (get-stake betting-event-id tx-sender)))
    )
    (asserts! (> stake-amount u0) ERR-INVALID-STAKE-AMOUNT)
    (asserts! (get is-betting-open bet) ERR-BET-CLOSED)
    (asserts! (>= (len (get betting-options bet)) chosen-option) ERR-INVALID-OPTION)
    (asserts! (< block-height (get betting-close-height bet)) ERR-BET-EXPIRED)
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
      (bet (unwrap! (get-bet betting-event-id) ERR-DOES-NOT-EXIST))
    )
    (asserts! (or (is-eq (get event-creator bet) tx-sender) (is-eq contract-owner tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (get is-betting-open bet) ERR-BET-CLOSED)
    (asserts! (>= block-height (get betting-close-height bet)) ERR-BET-NOT-CLOSABLE)
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
      (bet (unwrap! (get-bet betting-event-id) ERR-DOES-NOT-EXIST))
    )
    (asserts! (is-eq (get event-creator bet) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (get is-betting-open bet) ERR-BET-CLOSED)
    (asserts! (< block-height (get betting-close-height bet)) ERR-BET-NOT-CANCELABLE)
    
    ;; First set the bet as closed
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { is-betting-open: false })
    )
    
    ;; Then process refunds
    (process-refunds betting-event-id)
  )
)

(define-public (claim-winnings (betting-event-id uint))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) ERR-DOES-NOT-EXIST))
      (stake (unwrap! (get-stake betting-event-id tx-sender) ERR-DOES-NOT-EXIST))
      (winning-option-ids (get winning-options bet))
    )
    (asserts! (is-some (index-of winning-option-ids (get chosen-option stake))) ERR-NOT-A-WINNER)
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

(define-public (settle-bet (betting-event-id uint) (winning-option-ids (list 5 uint)))
  (let
    (
      (bet (unwrap! (get-bet betting-event-id) ERR-DOES-NOT-EXIST))
    )
    (asserts! (is-eq contract-owner tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (get is-betting-open bet)) ERR-BET-CLOSED)
    (asserts! (is-eq (len (get winning-options bet)) u0) ERR-ALREADY-SETTLED)
    (asserts! (> (len winning-option-ids) u0) ERR-NO-WINNING-OPTIONS)
    (asserts! (<= (len winning-option-ids) u5) ERR-TOO-MANY-WINNERS)
    
    ;; Validate each winning option
    (asserts! (validate-options-helper winning-option-ids (len (get betting-options bet))) ERR-INVALID-WINNER)
    
    (map-set bets
      { betting-event-id: betting-event-id }
      (merge bet { winning-options: winning-option-ids })
    )
    (ok true)
  )
)

;; Contract initialization
(begin
  (var-set next-betting-event-id u0)
)

;; Export the Component function (required for v0)
(define-public (Component)
  (ok true))