(define-constant CONTRACT_OWNER tx-sender)
(define-data-var paused bool false)

(define-public (pause)
  (if (is-eq tx-sender CONTRACT_OWNER)
      (begin (var-set paused true) (ok true))
      (err u100)
  )
)

(define-public (unpause)
  (if (is-eq tx-sender CONTRACT_OWNER)
      (begin (var-set paused false) (ok true))
      (err u100)
  )
)

(define-read-only (get-paused)
  (var-get paused)
)

(define-private (ensure-not-paused)
  (if (var-get paused)
      (err u101)
      (ok true)
  )
)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_GAME_NOT_FOUND (err u101))
(define-constant ERR_INVALID_MOVE (err u102))
(define-constant ERR_GAME_FINISHED (err u103))
(define-constant ERR_NOT_PLAYER_TURN (err u104))
(define-constant ERR_GAME_FULL (err u105))
(define-constant ERR_ALREADY_JOINED (err u106))
(define-constant ERR_INSUFFICIENT_FUNDS (err u107))
(define-constant ERR_GAME_EXPIRED (err u108))
(define-constant ERR_SELF_REFERRAL (err u109))
(define-constant ERR_ALREADY_HAS_REFERRER (err u110))

(define-constant GAME_STATE_WAITING u0)
(define-constant GAME_STATE_ACTIVE u1)
(define-constant GAME_STATE_FINISHED u2)

(define-constant GAME_EXPIRY_BLOCKS u144)

(define-constant CELL_EMPTY u0)
(define-constant CELL_X u1)
(define-constant CELL_O u2)

(define-data-var game-counter uint u0)
(define-data-var contract-balance uint u0)

(define-data-var leaderboard-size uint u10)

(define-data-var referral-bonus-percentage uint u5)
(define-data-var total-referral-rewards uint u0)

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

(define-map player-reputation
  principal
  {
    games-completed: uint,
    games-abandoned: uint,
    total-response-time: uint,
    reputation-score: uint,
    last-activity: uint
  }
)

(define-map leaderboard-wins
  uint
  {
    player: principal,
    wins: uint
  }
)

(define-map leaderboard-winnings
  uint
  {
    player: principal,
    winnings: uint
  }
)

(define-map leaderboard-reputation
  uint
  {
    player: principal,
    reputation: uint
  }
)

(define-map leaderboard-activity
  uint
  {
    player: principal,
    games: uint
  }
)

(define-map referrals
  principal
  {
    referrer: (optional principal),
    referral-count: uint,
    total-rewards: uint,
    joined-at: uint
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
    (update-player-reputation player u0)
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
    (update-player-reputation player u0)
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
    (begin
      (if (is-some winner-principal)
        (begin
          (try! (as-contract (stx-transfer? total-pot tx-sender (unwrap-panic winner-principal))))
          (var-set contract-balance (- (var-get contract-balance) total-pot))
          (update-player-stats (unwrap-panic winner-principal) true)
          (update-player-stats (unwrap-panic (get player2 game)) false)
          (update-player-reputation (get player1 game) u1)
          (update-player-reputation (unwrap-panic (get player2 game)) u1)
          (update-leaderboards (unwrap-panic winner-principal) total-pot)
          (try! (process-referral-reward (unwrap-panic winner-principal) total-pot))
        )
        (begin
          (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (get player1 game))))
          (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (unwrap-panic (get player2 game)))))
          (var-set contract-balance (- (var-get contract-balance) total-pot))
          (update-player-reputation (get player1 game) u1)
          (update-player-reputation (unwrap-panic (get player2 game)) u1)
        )
      )
      (ok true)
    )
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

(define-read-only (get-player-reputation (player principal))
  (map-get? player-reputation player)
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-game-count)
  (var-get game-counter)
)

(define-public (expire-game (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
      (current-block stacks-block-height)
      (game-age (- current-block (get created-at game)))
    )
    (asserts! (> game-age GAME_EXPIRY_BLOCKS) ERR_GAME_NOT_FOUND)
    (asserts! (not (is-eq (get state game) GAME_STATE_FINISHED)) ERR_GAME_FINISHED)
    (if (is-eq (get state game) GAME_STATE_WAITING)
      (begin
        (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (get player1 game))))
        (var-set contract-balance (- (var-get contract-balance) (get bet-amount game)))
        (update-player-reputation (get player1 game) u2)
        (map-set games game-id (merge game { state: GAME_STATE_FINISHED }))
        (ok "waiting-game-expired")
      )
      (begin
        (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (get player1 game))))
        (try! (as-contract (stx-transfer? (get bet-amount game) tx-sender (unwrap-panic (get player2 game)))))
        (var-set contract-balance (- (var-get contract-balance) (* (get bet-amount game) u2)))
        (update-player-reputation (get player1 game) u2)
        (update-player-reputation (unwrap-panic (get player2 game)) u2)
        (map-set games game-id (merge game { state: GAME_STATE_FINISHED }))
        (ok "active-game-expired")
      )
    )
  )
)

(define-read-only (is-game-expired (game-id uint))
  (match (map-get? games game-id)
    game (let
      (
        (current-block stacks-block-height)
        (game-age (- current-block (get created-at game)))
      )
      (and 
        (> game-age GAME_EXPIRY_BLOCKS)
        (not (is-eq (get state game) GAME_STATE_FINISHED))
      )
    )
    false
  )
)

(define-read-only (get-game-expiry-block (game-id uint))
  (match (map-get? games game-id)
    game (some (+ (get created-at game) GAME_EXPIRY_BLOCKS))
    none
  )
)

(define-private (update-player-reputation (player principal) (action uint))
  (let
    (
      (current-rep (default-to 
        {games-completed: u0, games-abandoned: u0, total-response-time: u0, reputation-score: u1000, last-activity: u0} 
        (map-get? player-reputation player)
      ))
      (current-block stacks-block-height)
      (response-time (- current-block (get last-activity current-rep)))
    )
    (if (is-eq action u0)
      (map-set player-reputation player
        (merge current-rep {
          last-activity: current-block,
          total-response-time: (+ (get total-response-time current-rep) response-time)
        })
      )
      (if (is-eq action u1)
        (let
          (
            (new-completed (+ (get games-completed current-rep) u1))
            (completion-rate (/ (* new-completed u1000) (+ new-completed (get games-abandoned current-rep))))
            (avg-response (if (> new-completed u0) (/ (get total-response-time current-rep) new-completed) u0))
            (new-score (calculate-reputation-score completion-rate avg-response))
          )
          (map-set player-reputation player
            (merge current-rep {
              games-completed: new-completed,
              reputation-score: new-score,
              last-activity: current-block
            })
          )
        )
        (let
          (
            (new-abandoned (+ (get games-abandoned current-rep) u1))
            (completion-rate (/ (* (get games-completed current-rep) u1000) (+ (get games-completed current-rep) new-abandoned)))
            (avg-response (if (> (get games-completed current-rep) u0) (/ (get total-response-time current-rep) (get games-completed current-rep)) u0))
            (new-score (calculate-reputation-score completion-rate avg-response))
          )
          (map-set player-reputation player
            (merge current-rep {
              games-abandoned: new-abandoned,
              reputation-score: new-score,
              last-activity: current-block
            })
          )
        )
      )
    )
  )
)

(define-private (calculate-reputation-score (completion-rate uint) (avg-response-time uint))
  (let
    (
      (completion-bonus (if (> completion-rate u800) u200 (/ completion-rate u4)))
      (response-penalty (if (> avg-response-time u100) u200 (/ avg-response-time u2)))
      (base-score u1000)
    )
    (if (> (+ completion-bonus base-score) response-penalty)
      (- (+ completion-bonus base-score) response-penalty)
      u100
    )
  )
)

(define-read-only (get-reputation-tier (player principal))
  (match (map-get? player-reputation player)
    rep (let
      (
        (score (get reputation-score rep))
      )
      (if (>= score u1400)
        "legendary"
        (if (>= score u1200)
          "expert"
          (if (>= score u1000)
            "reliable" 
            (if (>= score u800)
              "average"
              "novice"
            )
          )
        )
      )
    )
    "unrated"
  )
)

(define-private (update-leaderboards (winner principal) (winnings uint))
  (let
    (
      (player-stats-data (default-to {games-played: u0, games-won: u0, total-winnings: u0} (map-get? player-stats winner)))
      (player-rep-data (default-to {games-completed: u0, games-abandoned: u0, total-response-time: u0, reputation-score: u1000, last-activity: u0} (map-get? player-reputation winner)))
    )
    (simple-update-leaderboard winner (get games-won player-stats-data) (get total-winnings player-stats-data) (get reputation-score player-rep-data) (get games-played player-stats-data))
  )
)

(define-private (simple-update-leaderboard (player principal) (wins uint) (winnings uint) (reputation uint) (activity uint))
  (begin
    (map-set leaderboard-wins u1 {player: player, wins: wins})
    (map-set leaderboard-winnings u1 {player: player, winnings: winnings})
    (map-set leaderboard-reputation u1 {player: player, reputation: reputation})
    (map-set leaderboard-activity u1 {player: player, games: activity})
  )
)

(define-read-only (get-wins-leaderboard)
  (list
    (map-get? leaderboard-wins u1)
    (map-get? leaderboard-wins u2)
    (map-get? leaderboard-wins u3)
    (map-get? leaderboard-wins u4)
    (map-get? leaderboard-wins u5)
    (map-get? leaderboard-wins u6)
    (map-get? leaderboard-wins u7)
    (map-get? leaderboard-wins u8)
    (map-get? leaderboard-wins u9)
    (map-get? leaderboard-wins u10)
  )
)

(define-read-only (get-winnings-leaderboard)
  (list
    (map-get? leaderboard-winnings u1)
    (map-get? leaderboard-winnings u2)
    (map-get? leaderboard-winnings u3)
    (map-get? leaderboard-winnings u4)
    (map-get? leaderboard-winnings u5)
    (map-get? leaderboard-winnings u6)
    (map-get? leaderboard-winnings u7)
    (map-get? leaderboard-winnings u8)
    (map-get? leaderboard-winnings u9)
    (map-get? leaderboard-winnings u10)
  )
)

(define-read-only (get-reputation-leaderboard)
  (list
    (map-get? leaderboard-reputation u1)
    (map-get? leaderboard-reputation u2)
    (map-get? leaderboard-reputation u3)
    (map-get? leaderboard-reputation u4)
    (map-get? leaderboard-reputation u5)
    (map-get? leaderboard-reputation u6)
    (map-get? leaderboard-reputation u7)
    (map-get? leaderboard-reputation u8)
    (map-get? leaderboard-reputation u9)
    (map-get? leaderboard-reputation u10)
  )
)

(define-read-only (get-activity-leaderboard)
  (list
    (map-get? leaderboard-activity u1)
    (map-get? leaderboard-activity u2)
    (map-get? leaderboard-activity u3)
    (map-get? leaderboard-activity u4)
    (map-get? leaderboard-activity u5)
    (map-get? leaderboard-activity u6)
    (map-get? leaderboard-activity u7)
    (map-get? leaderboard-activity u8)
    (map-get? leaderboard-activity u9)
    (map-get? leaderboard-activity u10)
  )
)

(define-read-only (get-player-leaderboard-rank (player principal))
  (let
    (
      (wins-entry (map-get? leaderboard-wins u1))
      (winnings-entry (map-get? leaderboard-winnings u1))
      (reputation-entry (map-get? leaderboard-reputation u1))
      (activity-entry (map-get? leaderboard-activity u1))
    )
    {
      wins: (if (and (is-some wins-entry) (is-eq (get player (unwrap-panic wins-entry)) player)) (some u1) none),
      winnings: (if (and (is-some winnings-entry) (is-eq (get player (unwrap-panic winnings-entry)) player)) (some u1) none),
      reputation: (if (and (is-some reputation-entry) (is-eq (get player (unwrap-panic reputation-entry)) player)) (some u1) none),
      activity: (if (and (is-some activity-entry) (is-eq (get player (unwrap-panic activity-entry)) player)) (some u1) none)
    }
  )
)

(define-public (set-referrer (referrer-address principal))
  (let
    (
      (player tx-sender)
      (current-referral (map-get? referrals player))
    )
    (asserts! (not (is-eq player referrer-address)) ERR_SELF_REFERRAL)
    (asserts! (is-none current-referral) ERR_ALREADY_HAS_REFERRER)
    (map-set referrals player
      {
        referrer: (some referrer-address),
        referral-count: u0,
        total-rewards: u0,
        joined-at: stacks-block-height
      }
    )
    (match (map-get? referrals referrer-address)
      existing-data
        (map-set referrals referrer-address
          (merge existing-data {
            referral-count: (+ (get referral-count existing-data) u1)
          })
        )
      (map-set referrals referrer-address
        {
          referrer: none,
          referral-count: u1,
          total-rewards: u0,
          joined-at: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-private (process-referral-reward (winner principal) (winnings uint))
  (match (map-get? referrals winner)
    referral-data
      (match (get referrer referral-data)
        referrer-principal
          (let
            (
              (bonus-percentage (var-get referral-bonus-percentage))
              (reward-amount (/ (* winnings bonus-percentage) u100))
            )
            (if (> reward-amount u0)
              (begin
                (try! (as-contract (stx-transfer? reward-amount tx-sender referrer-principal)))
                (var-set total-referral-rewards (+ (var-get total-referral-rewards) reward-amount))
                (match (map-get? referrals referrer-principal)
                  referrer-data
                    (map-set referrals referrer-principal
                      (merge referrer-data {
                        total-rewards: (+ (get total-rewards referrer-data) reward-amount)
                      })
                    )
                  true
                )
                (ok true)
              )
              (ok true)
            )
          )
        (ok true)
      )
    (ok true)
  )
)

(define-read-only (get-referral-info (player principal))
  (map-get? referrals player)
)

(define-read-only (get-referral-stats (player principal))
  (match (map-get? referrals player)
    data
      (some {
        has-referrer: (is-some (get referrer data)),
        referral-count: (get referral-count data),
        total-rewards: (get total-rewards data),
        joined-at: (get joined-at data)
      })
    none
  )
)

(define-read-only (get-referral-bonus-percentage)
  (var-get referral-bonus-percentage)
)

(define-read-only (get-total-referral-rewards)
  (var-get total-referral-rewards)
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
