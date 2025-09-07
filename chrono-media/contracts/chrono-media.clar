;; ChronoMedia - Temporal IP Rights & Automated Licensing Marketplace
;; A comprehensive smart contract for timestamped IP protection and programmable licensing automation

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1002))
(define-constant ERR_MEDIA_NOT_FOUND (err u1003))
(define-constant ERR_INVALID_THRESHOLD (err u1004))
(define-constant ERR_ALREADY_ACTIVATED (err u1005))
(define-constant ERR_INVALID_COORDINATES (err u1006))
(define-constant ERR_CREATOR_NOT_CERTIFIED (err u1007))
(define-constant ERR_INVALID_IMPACT_DATA (err u1008))
(define-constant ERR_ORACLE_TIMEOUT (err u1009))
(define-constant ERR_INSUFFICIENT_STAKE (err u1010))
(define-constant ERR_RESOURCE_ALREADY_DEPLOYED (err u1011))
(define-constant ERR_INVALID_MEDIA_TYPE (err u1012))
(define-constant ERR_LICENSE_NOT_FOUND (err u1013))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u1014))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration constants
(define-constant MIN_CREATOR_STAKE u100000) ;; 100 STX minimum stake
(define-constant ORACLE_CONSENSUS_THRESHOLD u3) ;; Minimum oracle confirmations
(define-constant MAX_MEDIA_RADIUS u10000) ;; Maximum media radius in meters
(define-constant IMPACT_REWARD_MULTIPLIER u150) ;; 1.5x reward multiplier for impact
(define-constant MAX_SEVERITY_LEVEL u10)
(define-constant BLOCKS_PER_DAY u144) ;; Approximately 24 hours

;; Data structures
(define-map media-works
    { media-id: uint }
    {
        media-type: (string-ascii 32),
        latitude: int,
        longitude: int,
        popularity-level: uint,
        predicted-revenue: uint,
        licensing-pool: uint,
        status: (string-ascii 16),
        creation-block: uint,
        licensing-deadline: uint,
        creator: principal
    }
)

(define-map licensing-pool
    { region-id: uint }
    {
        total-funds: uint,
        allocated-funds: uint,
        community-stake: uint,
        governance-threshold: uint
    }
)

(define-map certified-creators
    { creator: principal }
    {
        certification-level: uint,
        stake-amount: uint,
        total-works: uint,
        success-rate: uint,
        geographic-radius: uint,
        certification-expires: uint
    }
)

(define-map content-libraries
    { library-id: uint }
    {
        latitude: int,
        longitude: int,
        content-type: (string-ascii 32),
        quantity-available: uint,
        licensing-cost: uint,
        last-updated: uint,
        managed-by: principal
    }
)

(define-map revenue-certificates
    { certificate-id: uint }
    {
        media-id: uint,
        creator: principal,
        usage-score: uint,
        users-reached: uint,
        licenses-deployed: uint,
        verification-status: (string-ascii 16),
        oracle-confirmations: uint,
        reward-amount: uint
    }
)

(define-map oracle-reports
    { oracle-id: principal, media-id: uint }
    {
        popularity-assessment: uint,
        revenue-estimate: uint,
        users-affected: uint,
        urgent-needs: (string-ascii 64),
        report-timestamp: uint,
        confidence-level: uint
    }
)

(define-map market-analytics-data
    { region-id: uint }
    {
        demand-index: uint,
        creator-density: uint,
        historical-works: uint,
        average-licensing-time: uint,
        monetization-rate: uint,
        optimization-score: uint
    }
)

(define-map media-oracle-consensus
    { media-id: uint }
    { confirmation-count: uint }
)

;; Global state variables
(define-data-var media-counter uint u0)
(define-data-var certificate-counter uint u0)
(define-data-var library-counter uint u0)
(define-data-var total-licensing-funds uint u0)
(define-data-var global-demand-level uint u0)
(define-data-var system-active bool true)

;; Authorization helper
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

;; Validation helpers
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
        creator-data (and 
            (> (get stake-amount creator-data) u0)
            (> (get certification-expires creator-data) block-height)
        )
        false
    )
)

;; Oracle consensus validation
(define-private (validate-oracle-consensus (media-id uint))
    (let ((confirmations (get-oracle-confirmations media-id)))
        (>= confirmations ORACLE_CONSENSUS_THRESHOLD)
    )
)

(define-private (get-oracle-confirmations (media-id uint))
    (default-to u0 (get confirmation-count (map-get? media-oracle-consensus { media-id: media-id })))
)

(define-private (increment-oracle-confirmations (media-id uint))
    (let ((current-count (get-oracle-confirmations media-id)))
        (map-set media-oracle-consensus 
            { media-id: media-id }
            { confirmation-count: (+ current-count u1) }
        )
    )
)

;; Calculate reward based on usage
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

(define-public (update-global-demand-level (level uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! (<= level u5) ERR_INVALID_THRESHOLD)
        (var-set global-demand-level level)
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

(define-public (register-content-library (latitude int) (longitude int) (content-type (string-ascii 32)) (quantity uint) (cost uint))
    (let ((library-id (+ (var-get library-counter) u1)))
        (begin
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-valid-coordinates latitude longitude) ERR_INVALID_COORDINATES)
            (map-set content-libraries
                { library-id: library-id }
                {
                    latitude: latitude,
                    longitude: longitude,
                    content-type: content-type,
                    quantity-available: quantity,
                    licensing-cost: cost,
                    last-updated: block-height,
                    managed-by: tx-sender
                }
            )
            (var-set library-counter library-id)
            (ok library-id)
        )
    )
)

;; Creator Functions
(define-public (become-certified-creator (stake-amount uint) (geographic-radius uint))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (>= stake-amount MIN_CREATOR_STAKE) ERR_INSUFFICIENT_STAKE)
        (asserts! (<= geographic-radius MAX_MEDIA_RADIUS) ERR_INVALID_COORDINATES)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set certified-creators
            { creator: tx-sender }
            {
                certification-level: u1,
                stake-amount: stake-amount,
                total-works: u0,
                success-rate: u100,
                geographic-radius: geographic-radius,
                certification-expires: (+ block-height u52560) ;; ~1 year
            }
        )
        (ok true)
    )
)

(define-public (register-media-work (media-type (string-ascii 32)) (latitude int) (longitude int) (popularity uint) (predicted-revenue uint))
    (let ((media-id (+ (var-get media-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-creator tx-sender) ERR_CREATOR_NOT_CERTIFIED)
            (asserts! (is-valid-coordinates latitude longitude) ERR_INVALID_COORDINATES)
            (asserts! (<= popularity MAX_SEVERITY_LEVEL) ERR_INVALID_THRESHOLD)
            (map-set media-works
                { media-id: media-id }
                {
                    media-type: media-type,
                    latitude: latitude,
                    longitude: longitude,
                    popularity-level: popularity,
                    predicted-revenue: predicted-revenue,
                    licensing-pool: u0,
                    status: "registered",
                    creation-block: block-height,
                    licensing-deadline: (+ block-height BLOCKS_PER_DAY),
                    creator: tx-sender
                }
            )
            (var-set media-counter media-id)
            (ok media-id)
        )
    )
)

(define-public (activate-licensing-program (media-id uint) (fund-allocation uint))
    (let ((media-work (unwrap! (map-get? media-works { media-id: media-id }) ERR_MEDIA_NOT_FOUND)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
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

(define-public (deploy-licenses (media-id uint) (library-id uint) (quantity uint))
    (let (
        (media-work (unwrap! (map-get? media-works { media-id: media-id }) ERR_MEDIA_NOT_FOUND))
        (library (unwrap! (map-get? content-libraries { library-id: library-id }) ERR_LICENSE_NOT_FOUND))
    )
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-creator tx-sender) ERR_CREATOR_NOT_CERTIFIED)
            (asserts! (is-eq (get status media-work) "licensed") ERR_ALREADY_ACTIVATED)
            (asserts! (<= quantity (get quantity-available library)) ERR_INSUFFICIENT_FUNDS)
            (map-set content-libraries
                { library-id: library-id }
                (merge library { 
                    quantity-available: (- (get quantity-available library) quantity),
                    last-updated: block-height
                })
            )
            (ok true)
        )
    )
)

(define-public (submit-usage-report (media-id uint) (users-reached uint) (licenses-used uint) (usage-score uint))
    (let ((certificate-id (+ (var-get certificate-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-creator tx-sender) ERR_CREATOR_NOT_CERTIFIED)
            (asserts! (is-some (map-get? media-works { media-id: media-id })) ERR_MEDIA_NOT_FOUND)
            (asserts! (<= usage-score u100) ERR_INVALID_IMPACT_DATA)
            (map-set revenue-certificates
                { certificate-id: certificate-id }
                {
                    media-id: media-id,
                    creator: tx-sender,
                    usage-score: usage-score,
                    users-reached: users-reached,
                    licenses-deployed: licenses-used,
                    verification-status: "pending",
                    oracle-confirmations: u0,
                    reward-amount: u0
                }
            )
            (var-set certificate-counter certificate-id)
            (ok certificate-id)
        )
    )
)

(define-public (submit-oracle-report (media-id uint) (popularity uint) (revenue-estimate uint) (users-affected uint) (urgent-needs (string-ascii 64)))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (is-some (map-get? media-works { media-id: media-id })) ERR_MEDIA_NOT_FOUND)
        (asserts! (<= popularity MAX_SEVERITY_LEVEL) ERR_INVALID_THRESHOLD)
        (map-set oracle-reports
            { oracle-id: tx-sender, media-id: media-id }
            {
                popularity-assessment: popularity,
                revenue-estimate: revenue-estimate,
                users-affected: users-affected,
                urgent-needs: urgent-needs,
                report-timestamp: block-height,
                confidence-level: u80
            }
        )
        (increment-oracle-confirmations media-id)
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
            (asserts! (is-eq (get verification