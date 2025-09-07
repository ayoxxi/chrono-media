;; ChronoMedia - Temporal IP Rights & Automated Licensing
;; Streamlined smart contract for timestamped IP protection and licensing automation

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1002))
(define-constant ERR_MEDIA_NOT_FOUND (err u1003))
(define-constant ERR_INVALID_THRESHOLD (err u1004))
(define-constant ERR_ALREADY_ACTIVATED (err u1005))
(define-constant ERR_INVALID_COORDINATES (err u1006))
(define-constant ERR_CREATOR_NOT_CERTIFIED (err u1007))
(define-constant ERR_ORACLE_TIMEOUT (err u1008))
(define-constant ERR_INSUFFICIENT_STAKE (err u1009))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u1010))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration constants
(define-constant MIN_CREATOR_STAKE u100000) ;; 100 STX minimum stake
(define-constant ORACLE_CONSENSUS_THRESHOLD u3) ;; Minimum oracle confirmations
(define-constant IMPACT_REWARD_MULTIPLIER u150) ;; 1.5x reward multiplier
(define-constant BLOCKS_PER_DAY u144) ;; Approximately 24 hours

;; Core data structures
(define-map media-works
    { media-id: uint }
    {
        media-type: (string-ascii 32),
        latitude: int,
        longitude: int,
        popularity-level: uint,
        licensing-pool: uint,
        status: (string-ascii 16),
        creation-block: uint,
        creator: principal
    }
)

(define-map certified-creators
    { creator: principal }
    {
        stake-amount: uint,
        total-works: uint,
        certification-expires: uint
    }
)

(define-map revenue-certificates
    { certificate-id: uint }
    {
        media-id: uint,
        creator: principal,
        usage-score: uint,
        users-reached: uint,
        verification-status: (string-ascii 16),
        reward-amount: uint
    }
)

(define-map oracle-consensus
    { media-id: uint }
    { confirmation-count: uint }
)

;; Global state variables
(define-data-var media-counter uint u0)
(define-data-var certificate-counter uint u0)
(define-data-var total-licensing-funds uint u0)
(define-data-var system-active bool true)

;; Helper functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-valid-coordinates (lat int) (lon int))
    (and 
        (<= lat 90000000) 
        (>= lat -90000000)
        (<= lon 180000000) 
        (>= lon -180000000)
    )
)

(define-private (is-certified-creator (creator principal))
    (match (map-get? certified-creators { creator: creator })
        creator-data (> (get certification-expires creator-data) block-height)
        false
    )
)

(define-private (get-oracle-confirmations (media-id uint))
    (default-to u0 (get confirmation-count (map-get? oracle-consensus { media-id: media-id })))
)

(define-private (validate-oracle-consensus (media-id uint))
    (>= (get-oracle-confirmations media-id) ORACLE_CONSENSUS_THRESHOLD)
)

(define-private (calculate-usage-reward (usage-score uint) (base-amount uint))
    (/ (* base-amount usage-score IMPACT_REWARD_MULTIPLIER) u10000)
)

;; Admin Functions
(define-public (set-system-status (active bool))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set system-active active)
        (ok true)
    )
)

(define-public (add-licensing-funds (amount uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-licensing-funds (+ (var-get total-licensing-funds) amount))
        (ok true)
    )
)

;; Creator Functions
(define-public (become-certified-creator (stake-amount uint))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (>= stake-amount MIN_CREATOR_STAKE) ERR_INSUFFICIENT_STAKE)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set certified-creators
            { creator: tx-sender }
            {
                stake-amount: stake-amount,
                total-works: u0,
                certification-expires: (+ block-height u52560) ;; ~1 year
            }
        )
        (ok true)
    )
)

(define-public (register-media-work (media-type (string-ascii 32)) (latitude int) (longitude int) (popularity uint))
    (let ((media-id (+ (var-get media-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-creator tx-sender) ERR_CREATOR_NOT_CERTIFIED)
            (asserts! (is-valid-coordinates latitude longitude) ERR_INVALID_COORDINATES)
            (asserts! (<= popularity u10) ERR_INVALID_THRESHOLD)
            (map-set media-works
                { media-id: media-id }
                {
                    media-type: media-type,
                    latitude: latitude,
                    longitude: longitude,
                    popularity-level: popularity,
                    licensing-pool: u0,
                    status: "registered",
                    creation-block: block-height,
                    creator: tx-sender
                }
            )
            (var-set media-counter media-id)
            (ok media-id)
        )
    )
)

(define-public (activate-licensing (media-id uint) (fund-allocation uint))
    (let ((media-work (unwrap! (map-get? media-works { media-id: media-id }) ERR_MEDIA_NOT_FOUND)))
        (begin
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-eq (get status media-work) "registered") ERR_ALREADY_ACTIVATED)
            (asserts! (validate-oracle-consensus media-id) ERR_ORACLE_TIMEOUT)
            (asserts! (<= fund-allocation (var-get total-licensing-funds)) ERR_INSUFFICIENT_FUNDS)
            (map-set media-works
                { media-id: media-id }
                (merge media-work { 
                    status: "licensed",
                    licensing-pool: fund-allocation
                })
            )
            (var-set total-licensing-funds (- (var-get total-licensing-funds) fund-allocation))
            (ok true)
        )
    )
)

(define-public (submit-usage-report (media-id uint) (users-reached uint) (usage-score uint))
    (let ((certificate-id (+ (var-get certificate-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-creator tx-sender) ERR_CREATOR_NOT_CERTIFIED)
            (asserts! (is-some (map-get? media-works { media-id: media-id })) ERR_MEDIA_NOT_FOUND)
            (asserts! (<= usage-score u100) ERR_INVALID_THRESHOLD)
            (map-set revenue-certificates
                { certificate-id: certificate-id }
                {
                    media-id: media-id,
                    creator: tx-sender,
                    usage-score: usage-score,
                    users-reached: users-reached,
                    verification-status: "pending",
                    reward-amount: u0
                }
            )
            (var-set certificate-counter certificate-id)
            (ok certificate-id)
        )
    )
)

(define-public (submit-oracle-report (media-id uint))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (is-some (map-get? media-works { media-id: media-id })) ERR_MEDIA_NOT_FOUND)
        (let ((current-count (get-oracle-confirmations media-id)))
            (map-set oracle-consensus 
                { media-id: media-id }
                { confirmation-count: (+ current-count u1) }
            )
        )
        (ok true)
    )
)

(define-public (verify-revenue-certificate (certificate-id uint))
    (let (
        (certificate (unwrap! (map-get? revenue-certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
        (media-id (get media-id certificate))
        (media-work (unwrap! (map-get? media-works { media-id: media-id }) ERR_MEDIA_NOT_FOUND))
        (reward (calculate-usage-reward (get usage-score certificate) (get licensing-pool media-work)))
    )
        (begin
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-eq (get verification-status certificate) "pending") ERR_ALREADY_ACTIVATED)
            (asserts! (validate-oracle-consensus media-id) ERR_ORACLE_TIMEOUT)
            ;; Transfer reward to creator
            (try! (as-contract (stx-transfer? reward tx-sender (get creator certificate))))
            ;; Update certificate
            (map-set revenue-certificates
                { certificate-id: certificate-id }
                (merge certificate {
                    verification-status: "verified",
                    reward-amount: reward
                })
            )
            (ok reward)
        )
    )
)

;; Query Functions
(define-read-only (get-media-work (media-id uint))
    (map-get? media-works { media-id: media-id })
)

(define-read-only (get-creator-info (creator principal))
    (map-get? certified-creators { creator: creator })
)

(define-read-only (get-revenue-certificate (certificate-id uint))
    (map-get? revenue-certificates { certificate-id: certificate-id })
)

(define-read-only (get-system-stats)
    {
        media-counter: (var-get media-counter),
        certificate-counter: (var-get certificate-counter),
        total-licensing-funds: (var-get total-licensing-funds),
        system-active: (var-get system-active)
    }
)

(define-read-only (get-oracle-confirmations-count (media-id uint))
    (get-oracle-confirmations media-id)
)