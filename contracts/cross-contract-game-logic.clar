(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_GAME_NOT_FOUND (err u101))
(define-constant ERR_INVALID_MOVE (err u102))
(define-constant ERR_GAME_FINISHED (err u103))
(define-constant ERR_NOT_PLAYER_TURN (err u104))
(define-constant ERR_GAME_FULL (err u105))
(define-constant ERR_ALREADY_JOINED (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))

(define-constant GAME_STATE_WAITING u0)
(define-constant GAME_STATE_ACTIVE u1)
(define-constant GAME_STATE_FINISHED u2)

(define-constant CELL_EMPTY u0)
(define-constant CELL_X u1)
(define-constant CELL_O u2)

(define-data-var game-counter uint u0)
(define-data-var contract-balance uint u0)

(define-map games
  uint
  {
    player1: principal,
    player2: (optional principal),
    current-player: uint,
    state: uint,
    winner: (optional principal),
    bet-amount: uint,
    created-at: uint
  }
)

(define-map game-boards
  uint
  {
    board: (list 9 uint),
    move-count: uint
  }
)

(define-map player-stats
  principal
  {
    games-played: uint,
    games-won: uint,
    total-winnings: uint
  }
)

(define-public (create-game (bet-amount uint))
  (let
    (
      (game-id (+ (var-get game-counter) u1))
      (player tx-sender)
    )
    (asserts! (>= (stx-get-balance tx-sender) bet-amount) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)))
    (var-set game-counter game-id)
    (var-set contract-balance (+ (var-get contract-balance) bet-amount))
    (map-set games game-id
      {
        player1: player,
        player2: none,
        current-player: u1,
        state: GAME_STATE_WAITING,
        winner: none,
        bet-amount: bet-amount,
        created-at: stacks-block-height
      }
    )
    (map-set game-boards game-id
      {
        board: (list u0 u0 u0 u0 u0 u0 u0 u0 u0),
        move-count: u0
      }
    )
    (ok game-id)
  )
)

(define-public (join-game (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
      (player tx-sender)
      (bet-amount (get bet-amount game))
    )
    (asserts! (is-eq (get state game) GAME_STATE_WAITING) ERR_GAME_FINISHED)
    (asserts! (is-none (get player2 game)) ERR_GAME_FULL)
    (asserts! (not (is-eq (get player1 game) player)) ERR_ALREADY_JOINED)
    (asserts! (>= (stx-get-balance tx-sender) bet-amount) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) bet-amount))
    (map-set games game-id
      (merge game {
        player2: (some player),
        state: GAME_STATE_ACTIVE
      })
    )
    (ok true)
  )
)

;; (define-public (make-move (game-id uint) (position uint))
;;   (let
;;     (
;;       (game (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
;;       (board-data (unwrap! (map-get? game-boards game-id) ERR_GAME_NOT_FOUND))
;;       (player tx-sender)
;;       (current-board (get board board-data))
;;       (move-count (get move-count board-data))
;;     )
;;     (asserts! (is-eq (get state game) GAME_STATE_ACTIVE) ERR_GAME_FINISHED)
;;     (asserts! (< position u9) ERR_INVALID_MOVE)
;;     (asserts! (is-eq (unwrap! (element-at current-board position) ERR_INVALID_MOVE) u0) ERR_INVALID_MOVE)
;;     (asserts! (is-valid-player-turn game player) ERR_NOT_PLAYER_TURN)
;;     (let
;;       (
;;         (player-symbol (if (is-eq player (get player1 game)) CELL_X CELL_O))
;;         (new-board (unwrap! (list-replace-at current-board position player-symbol) ERR_INVALID_MOVE))
;;         (new-move-count (+ move-count u1))
;;       )
;;       (map-set game-boards game-id
;;         {
;;           board: new-board,
;;           move-count: new-move-count
;;         }
;;       )
;;       (let
;;         (
;;           (winner-check (check-winner new-board))
;;           (is-draw (and (is-none winner-check) (is-eq new-move-count u9)))
;;         )
;;         (if (or (is-some winner-check) is-draw)
;;           (finish-game game-id game winner-check)
;;           (map-set games game-id
;;             (merge game {
;;               current-player: (if (is-eq (get current-player game) u1) u2 u1)
;;             })
;;           )
;;         )
;;       )
;;       (ok true)
;;     )
;;   )
;; )

(define-private (is-valid-player-turn (game {player1: principal, player2: (optional principal), current-player: uint, state: uint, winner: (optional principal), bet-amount: uint, created-at: uint}) (player principal))
  (if (is-eq (get current-player game) u1)
    (is-eq player (get player1 game))
    (is-eq player (unwrap! (get player2 game) false))
  )
)

(define-private (check-winner (board (list 9 uint)))
  (let
    (
      (winning-combinations (list
        (list u0 u1 u2) (list u3 u4 u5) (list u6 u7 u8)
        (list u0 u3 u6) (list u1 u4 u7) (list u2 u5 u8)
        (list u0 u4 u8) (list u2 u4 u6)
      ))
    )
    (fold check-combination winning-combinations none)
  )
)

(define-private (check-combination (combination (list 3 uint)) (current-winner (optional uint)))
  (if (is-some current-winner)
    current-winner
    (let
      (
        (pos1 (unwrap! (element-at combination u0) none))
        (pos2 (unwrap! (element-at combination u1) none))
        (pos3 (unwrap! (element-at combination u2) none))
      )
      (check-line-winner pos1 pos2 pos3)
    )
  )
)

(define-private (check-line-winner (pos1 uint) (pos2 uint) (pos3 uint))
  (let
    (
      (game-board (unwrap! (map-get? game-boards (var-get game-counter)) none))
      (board (get board game-board))
      (cell1 (unwrap! (element-at board pos1) none))
      (cell2 (unwrap! (element-at board pos2) none))
      (cell3 (unwrap! (element-at board pos3) none))
    )
    (if (and (> cell1 u0) (is-eq cell1 cell2) (is-eq cell2 cell3))
      (some cell1)
      none
    )
  )
)

(define-private (finish-game (game-id uint) (game {player1: principal, player2: (optional principal), current-player: uint, state: uint, winner: (optional principal), bet-amount: uint, created-at: uint}) (winner-symbol (optional uint)))
  (let
    (
      (winner-principal 
        (if (is-some winner-symbol)
          (if (is-eq (unwrap-panic winner-symbol) CELL_X)
            (some (get player1 game))
            (get player2 game)
          )
          none
        )
      )
      (total-pot (* (get bet-amount game) u2))
    )
    (map-set games game-id
      (merge game {
        state: GAME_STATE_FINISHED,
        winner: winner-principal
      })
    )
    (if (is-some winner-principal)
      (begin
        (try! (as-contract (stx-transfer? total-pot tx-sender (unwrap-panic winner-principal))))
        (var-set contract-balance (- (var-get contract-balance) total-pot))
        (update-player-stats (unwrap-panic winner-principal) true)
        (update-player-stats (unwrap-panic (get player2 game)) false)
      )
      (begin
        (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (get player1 game))))
        (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (unwrap-panic (get player2 game)))))
        (var-set contract-balance (- (var-get contract-balance) total-pot))
      )
    )
    (ok true)
  )
)

(define-private (update-player-stats (player principal) (won bool))
  (let
    (
      (current-stats (default-to {games-played: u0, games-won: u0, total-winnings: u0} (map-get? player-stats player)))
    )
    (map-set player-stats player
      {
        games-played: (+ (get games-played current-stats) u1),
        games-won: (if won (+ (get games-won current-stats) u1) (get games-won current-stats)),
        total-winnings: (get total-winnings current-stats)
      }
    )
  )
)

(define-read-only (get-game (game-id uint))
  (map-get? games game-id)
)

(define-read-only (get-game-board (game-id uint))
  (map-get? game-boards game-id)
)

(define-read-only (get-player-stats (player principal))
  (map-get? player-stats player)
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-game-count)
  (var-get game-counter)
)

;; Helper function to replace an element at a specific index in a list (non-recursive)
;; (define-private (list-replace-at (lst (list 9 uint)) (idx uint) (val uint))
;;   (if (>= idx u9)
;;     (err u102)
;;     (let
;;       (
;;         (result
;;           (fold 
;;             (lambda (acc item)
;;               (let
;;                 (
;;                   (i (len acc))
;;                 )
;;                 (append acc (list (if (is-eq i idx) val item)))
;;               )
;;             )
;;             (list)
;;             lst
;;           )
;;         )
;;       )
;;       (ok result)
;;     )
;;   )
;; )

;; (define-public (call-external-contract (contract-address principal) (function-name (string-ascii 50)))
;;   (contract-call? .game-state-manager get-global-stats)
;; )