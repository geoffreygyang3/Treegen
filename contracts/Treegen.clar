(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u105))
(define-constant ERR_PROOF_NOT_PENDING (err u106))
(define-constant ERR_INSUFFICIENT_STAKE (err u107))
(define-constant ERR_INVALID_TRADE (err u108))
(define-constant ERR_INSUFFICIENT_CREDITS (err u109))
(define-constant ERR_CREDIT_NOT_FOUND (err u110))
(define-constant ERR_INVALID_PRICE (err u111))
(define-constant ERR_CANNOT_BUY_OWN_CREDIT (err u112))
(define-constant ERR_CREDIT_NOT_FOR_SALE (err u113))
(define-constant ERR_INVALID_EXPIRY (err u114))
(define-constant ERR_INVALID_HEALTH_STATUS (err u115))
(define-constant ERR_MONITORING_NOT_ACTIVE (err u116))
(define-constant ERR_ALREADY_MONITORED (err u117))
(define-constant ERR_INSUFFICIENT_VALIDATORS (err u118))
(define-constant ERR_ALREADY_VALIDATED (err u119))
(define-constant ERR_MONITORING_EXPIRED (err u120))

(define-data-var proposal-counter uint u0)
(define-data-var proof-counter uint u0)
(define-data-var total-trees-planted uint u0)
(define-data-var total-stx-distributed uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var credit-counter uint u0)
(define-data-var total-credits-minted uint u0)
(define-data-var total-credits-traded uint u0)
(define-data-var credit-to-tree-ratio uint u10)
(define-data-var monitoring-counter uint u0)
(define-data-var total-trees-monitored uint u0)
(define-data-var total-health-assessments uint u0)
(define-data-var survival-bonus-per-tree uint u25000)
(define-data-var min-validators-required uint u3)

(define-map proposals uint {
    id: uint,
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 100),
    tree-count: uint,
    funding-goal: uint,
    funding-raised: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    created-at: uint,
    deadline: uint
})

(define-map votes {proposal-id: uint, voter: principal} {
    vote: bool,
    amount: uint
})

(define-map user-stakes principal uint)

(define-map planting-proofs uint {
    id: uint,
    proposal-id: uint,
    planter: principal,
    tree-count: uint,
    location: (string-ascii 100),
    photo-hash: (string-ascii 64),
    description: (string-ascii 300),
    status: (string-ascii 20),
    submitted-at: uint,
    verified-at: (optional uint),
    verifier: (optional principal)
})

(define-map user-planted-trees principal uint)
(define-map user-rewards principal uint)

(define-map carbon-credits uint {
    id: uint,
    owner: principal,
    tree-count: uint,
    credits-amount: uint,
    minted-at: uint,
    expiry-date: uint,
    proof-id: uint,
    is-retired: bool,
    price: uint,
    for-sale: bool
})

(define-map user-carbon-balances principal uint)
(define-map credit-ownership-history uint (list 10 {owner: principal, transferred-at: uint}))
(define-map retired-credits principal uint)

(define-map tree-monitoring-records uint {
    id: uint,
    proof-id: uint,
    planter: principal,
    tree-count: uint,
    location: (string-ascii 100),
    species: (string-ascii 50),
    monitoring-start: uint,
    monitoring-end: uint,
    status: (string-ascii 20),
    survival-rate: uint,
    health-score: uint,
    validators-needed: uint,
    validators-confirmed: uint,
    total-bonus-earned: uint
})

(define-map health-assessments uint {
    id: uint,
    monitoring-id: uint,
    validator: principal,
    assessment-date: uint,
    health-status: uint,
    survival-count: uint,
    notes: (string-ascii 200),
    photo-hash: (string-ascii 64)
})

(define-map monitoring-validators {monitoring-id: uint, validator: principal} {
    validated: bool,
    assessment-id: uint
})

(define-map user-monitoring-stats principal {
    total-monitored: uint,
    total-survived: uint,
    total-bonus-earned: uint,
    validator-assessments: uint
})

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (location (string-ascii 100)) (tree-count uint) (funding-goal uint))
    (let ((proposal-id (+ (var-get proposal-counter) u1))
          (user-stake (default-to u0 (map-get? user-stakes tx-sender))))
        (asserts! (>= user-stake (var-get min-proposal-stake)) ERR_INSUFFICIENT_STAKE)
        (asserts! (> tree-count u0) ERR_INVALID_AMOUNT)
        (asserts! (> funding-goal u0) ERR_INVALID_AMOUNT)
        (map-set proposals proposal-id {
            id: proposal-id,
            proposer: tx-sender,
            title: title,
            description: description,
            location: location,
            tree-count: tree-count,
            funding-goal: funding-goal,
            funding-raised: u0,
            votes-for: u0,
            votes-against: u0,
            status: "active",
            created-at: stacks-block-height,
            deadline: (+ stacks-block-height u144)
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote bool) (amount uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_NOT_FOUND))
          (vote-key {proposal-id: proposal-id, voter: tx-sender}))
        (asserts! (is-none (map-get? votes vote-key)) ERR_ALREADY_VOTED)
        (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get deadline proposal)) ERR_PROPOSAL_NOT_ACTIVE)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set votes vote-key {vote: vote, amount: amount})
        (if vote
            (map-set proposals proposal-id (merge proposal {
                votes-for: (+ (get votes-for proposal) amount),
                funding-raised: (+ (get funding-raised proposal) amount)
            }))
            (map-set proposals proposal-id (merge proposal {
                votes-against: (+ (get votes-against proposal) amount)
            })))
        (ok true)))

(define-public (finalize-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_NOT_FOUND)))
        (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_NOT_ACTIVE)
        (asserts! (>= stacks-block-height (get deadline proposal)) ERR_PROPOSAL_NOT_ACTIVE)
        (let ((new-status (if (> (get votes-for proposal) (get votes-against proposal)) "approved" "rejected")))
            (map-set proposals proposal-id (merge proposal {status: new-status}))
            (ok new-status))))

(define-public (submit-planting-proof (proposal-id uint) (tree-count uint) (location (string-ascii 100)) (photo-hash (string-ascii 64)) (description (string-ascii 300)))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_NOT_FOUND))
          (proof-id (+ (var-get proof-counter) u1)))
        (asserts! (is-eq (get status proposal) "approved") ERR_PROPOSAL_NOT_ACTIVE)
        (asserts! (> tree-count u0) ERR_INVALID_AMOUNT)
        (map-set planting-proofs proof-id {
            id: proof-id,
            proposal-id: proposal-id,
            planter: tx-sender,
            tree-count: tree-count,
            location: location,
            photo-hash: photo-hash,
            description: description,
            status: "pending",
            submitted-at: stacks-block-height,
            verified-at: none,
            verifier: none
        })
        (var-set proof-counter proof-id)
        (ok proof-id)))

(define-public (verify-proof (proof-id uint) (approved bool))
    (let ((proof (unwrap! (map-get? planting-proofs proof-id) ERR_NOT_FOUND)))
        (asserts! (is-eq (get status proof) "pending") ERR_PROOF_NOT_PENDING)
        (let ((new-status (if approved "verified" "rejected"))
              (planter (get planter proof))
              (tree-count (get tree-count proof)))
            (map-set planting-proofs proof-id (merge proof {
                status: new-status,
                verified-at: (some stacks-block-height),
                verifier: (some tx-sender)
            }))
            (if approved
                (begin
                    (let ((current-planted (default-to u0 (map-get? user-planted-trees planter)))
                          (reward-amount (* tree-count u50000)))
                        (map-set user-planted-trees planter (+ current-planted tree-count))
                        (var-set total-trees-planted (+ (var-get total-trees-planted) tree-count))
                        (map-set user-rewards planter (+ (default-to u0 (map-get? user-rewards planter)) reward-amount))
                        (var-set total-stx-distributed (+ (var-get total-stx-distributed) reward-amount))
                        (try! (as-contract (stx-transfer? reward-amount tx-sender planter)))
                        true))
                false)
            (ok new-status))))

(define-public (mint-carbon-credits (proof-id uint))
    (let ((proof (unwrap! (map-get? planting-proofs proof-id) ERR_NOT_FOUND))
          (credit-id (+ (var-get credit-counter) u1))
          (tree-count (get tree-count proof))
          (credits-amount (/ tree-count (var-get credit-to-tree-ratio))))
        (asserts! (is-eq (get status proof) "verified") ERR_PROOF_NOT_PENDING)
        (asserts! (is-eq (get planter proof) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> credits-amount u0) ERR_INVALID_AMOUNT)
        (map-set carbon-credits credit-id {
            id: credit-id,
            owner: tx-sender,
            tree-count: tree-count,
            credits-amount: credits-amount,
            minted-at: stacks-block-height,
            expiry-date: (+ stacks-block-height u52560),
            proof-id: proof-id,
            is-retired: false,
            price: u0,
            for-sale: false
        })
        (map-set user-carbon-balances tx-sender (+ (default-to u0 (map-get? user-carbon-balances tx-sender)) credits-amount))
        (map-set credit-ownership-history credit-id (list {owner: tx-sender, transferred-at: stacks-block-height}))
        (var-set credit-counter credit-id)
        (var-set total-credits-minted (+ (var-get total-credits-minted) credits-amount))
        (ok credit-id)))

(define-public (list-credit-for-sale (credit-id uint) (price uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) ERR_CREDIT_NOT_FOUND)))
        (asserts! (is-eq (get owner credit) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (not (get is-retired credit)) ERR_INVALID_TRADE)
        (asserts! (> price u0) ERR_INVALID_PRICE)
        (asserts! (< stacks-block-height (get expiry-date credit)) ERR_INVALID_EXPIRY)
        (map-set carbon-credits credit-id (merge credit {
            price: price,
            for-sale: true
        }))
        (ok true)))

(define-public (remove-credit-from-sale (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) ERR_CREDIT_NOT_FOUND)))
        (asserts! (is-eq (get owner credit) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (get for-sale credit) ERR_CREDIT_NOT_FOR_SALE)
        (map-set carbon-credits credit-id (merge credit {
            price: u0,
            for-sale: false
        }))
        (ok true)))

(define-public (buy-carbon-credit (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) ERR_CREDIT_NOT_FOUND))
          (seller (get owner credit))
          (price (get price credit))
          (credits-amount (get credits-amount credit)))
        (asserts! (not (is-eq seller tx-sender)) ERR_CANNOT_BUY_OWN_CREDIT)
        (asserts! (get for-sale credit) ERR_CREDIT_NOT_FOR_SALE)
        (asserts! (not (get is-retired credit)) ERR_INVALID_TRADE)
        (asserts! (< stacks-block-height (get expiry-date credit)) ERR_INVALID_EXPIRY)
        (try! (stx-transfer? price tx-sender seller))
        (map-set carbon-credits credit-id (merge credit {
            owner: tx-sender,
            price: u0,
            for-sale: false
        }))
        (map-set user-carbon-balances seller (- (default-to u0 (map-get? user-carbon-balances seller)) credits-amount))
        (map-set user-carbon-balances tx-sender (+ (default-to u0 (map-get? user-carbon-balances tx-sender)) credits-amount))
        (let ((current-history (default-to (list) (map-get? credit-ownership-history credit-id))))
            (map-set credit-ownership-history credit-id 
                (unwrap-panic (as-max-len? (append current-history {owner: tx-sender, transferred-at: stacks-block-height}) u10))))
        (var-set total-credits-traded (+ (var-get total-credits-traded) credits-amount))
        (ok true)))

(define-public (transfer-carbon-credit (credit-id uint) (recipient principal))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) ERR_CREDIT_NOT_FOUND))
          (credits-amount (get credits-amount credit)))
        (asserts! (is-eq (get owner credit) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (not (get is-retired credit)) ERR_INVALID_TRADE)
        (asserts! (< stacks-block-height (get expiry-date credit)) ERR_INVALID_EXPIRY)
        (map-set carbon-credits credit-id (merge credit {
            owner: recipient,
            price: u0,
            for-sale: false
        }))
        (map-set user-carbon-balances tx-sender (- (default-to u0 (map-get? user-carbon-balances tx-sender)) credits-amount))
        (map-set user-carbon-balances recipient (+ (default-to u0 (map-get? user-carbon-balances recipient)) credits-amount))
        (let ((current-history (default-to (list) (map-get? credit-ownership-history credit-id))))
            (map-set credit-ownership-history credit-id 
                (unwrap-panic (as-max-len? (append current-history {owner: recipient, transferred-at: stacks-block-height}) u10))))
        (ok true)))

(define-public (retire-carbon-credit (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) ERR_CREDIT_NOT_FOUND))
          (credits-amount (get credits-amount credit)))
        (asserts! (is-eq (get owner credit) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (not (get is-retired credit)) ERR_INVALID_TRADE)
        (map-set carbon-credits credit-id (merge credit {
            is-retired: true,
            for-sale: false,
            price: u0
        }))
        (map-set user-carbon-balances tx-sender (- (default-to u0 (map-get? user-carbon-balances tx-sender)) credits-amount))
        (map-set retired-credits tx-sender (+ (default-to u0 (map-get? retired-credits tx-sender)) credits-amount))
        (ok true)))

(define-public (start-tree-monitoring (proof-id uint) (species (string-ascii 50)) (monitoring-duration uint))
    (let ((proof (unwrap! (map-get? planting-proofs proof-id) ERR_NOT_FOUND))
          (monitoring-id (+ (var-get monitoring-counter) u1)))
        (asserts! (is-eq (get status proof) "verified") ERR_PROOF_NOT_PENDING)
        (asserts! (is-eq (get planter proof) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> monitoring-duration u0) ERR_INVALID_AMOUNT)
        (asserts! (is-none (map-get? tree-monitoring-records monitoring-id)) ERR_ALREADY_MONITORED)
        (map-set tree-monitoring-records monitoring-id {
            id: monitoring-id,
            proof-id: proof-id,
            planter: tx-sender,
            tree-count: (get tree-count proof),
            location: (get location proof),
            species: species,
            monitoring-start: stacks-block-height,
            monitoring-end: (+ stacks-block-height monitoring-duration),
            status: "active",
            survival-rate: u0,
            health-score: u0,
            validators-needed: (var-get min-validators-required),
            validators-confirmed: u0,
            total-bonus-earned: u0
        })
        (var-set monitoring-counter monitoring-id)
        (var-set total-trees-monitored (+ (var-get total-trees-monitored) (get tree-count proof)))
        (let ((current-stats (default-to {total-monitored: u0, total-survived: u0, total-bonus-earned: u0, validator-assessments: u0} 
                                        (map-get? user-monitoring-stats tx-sender))))
            (map-set user-monitoring-stats tx-sender (merge current-stats {
                total-monitored: (+ (get total-monitored current-stats) (get tree-count proof))
            })))
        (ok monitoring-id)))

(define-public (submit-health-assessment (monitoring-id uint) (health-status uint) (survival-count uint) (notes (string-ascii 200)) (photo-hash (string-ascii 64)))
    (let ((monitoring-record (unwrap! (map-get? tree-monitoring-records monitoring-id) ERR_NOT_FOUND))
          (assessment-id (+ (var-get total-health-assessments) u1))
          (validator-key {monitoring-id: monitoring-id, validator: tx-sender}))
        (asserts! (is-eq (get status monitoring-record) "active") ERR_MONITORING_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get monitoring-end monitoring-record)) ERR_MONITORING_EXPIRED)
        (asserts! (is-none (map-get? monitoring-validators validator-key)) ERR_ALREADY_VALIDATED)
        (asserts! (<= health-status u100) ERR_INVALID_HEALTH_STATUS)
        (asserts! (<= survival-count (get tree-count monitoring-record)) ERR_INVALID_AMOUNT)
        (map-set health-assessments assessment-id {
            id: assessment-id,
            monitoring-id: monitoring-id,
            validator: tx-sender,
            assessment-date: stacks-block-height,
            health-status: health-status,
            survival-count: survival-count,
            notes: notes,
            photo-hash: photo-hash
        })
        (map-set monitoring-validators validator-key {
            validated: true,
            assessment-id: assessment-id
        })
        (let ((updated-record (merge monitoring-record {
                validators-confirmed: (+ (get validators-confirmed monitoring-record) u1)
            })))
            (map-set tree-monitoring-records monitoring-id updated-record)
            (var-set total-health-assessments assessment-id)
            (let ((current-stats (default-to {total-monitored: u0, total-survived: u0, total-bonus-earned: u0, validator-assessments: u0} 
                                            (map-get? user-monitoring-stats tx-sender))))
                (map-set user-monitoring-stats tx-sender (merge current-stats {
                    validator-assessments: (+ (get validator-assessments current-stats) u1)
                })))
            (if (>= (get validators-confirmed updated-record) (get validators-needed updated-record))
                (begin
                    (try! (finalize-monitoring monitoring-id))
                    (ok assessment-id))
                (ok assessment-id)))))

(define-public (finalize-monitoring (monitoring-id uint))
    (let ((monitoring-record (unwrap! (map-get? tree-monitoring-records monitoring-id) ERR_NOT_FOUND)))
        (asserts! (is-eq (get status monitoring-record) "active") ERR_MONITORING_NOT_ACTIVE)
        (asserts! (>= (get validators-confirmed monitoring-record) (get validators-needed monitoring-record)) ERR_INSUFFICIENT_VALIDATORS)
        (let ((assessments (get-monitoring-assessments monitoring-id))
              (avg-survival-rate (calculate-average-survival-rate monitoring-id))
              (avg-health-score (calculate-average-health-score monitoring-id))
              (planter (get planter monitoring-record))
              (bonus-amount (* avg-survival-rate (var-get survival-bonus-per-tree))))
            (map-set tree-monitoring-records monitoring-id (merge monitoring-record {
                status: "completed",
                survival-rate: avg-survival-rate,
                health-score: avg-health-score,
                total-bonus-earned: bonus-amount
            }))
            (if (> bonus-amount u0)
                (begin
                    (map-set user-rewards planter (+ (default-to u0 (map-get? user-rewards planter)) bonus-amount))
                    (var-set total-stx-distributed (+ (var-get total-stx-distributed) bonus-amount))
                    (let ((current-stats (default-to {total-monitored: u0, total-survived: u0, total-bonus-earned: u0, validator-assessments: u0} 
                                                    (map-get? user-monitoring-stats planter))))
                        (map-set user-monitoring-stats planter (merge current-stats {
                            total-survived: (+ (get total-survived current-stats) avg-survival-rate),
                            total-bonus-earned: (+ (get total-bonus-earned current-stats) bonus-amount)
                        }))))
                false)
            (ok true))))

(define-private (calculate-average-survival-rate (monitoring-id uint))
    (let ((assessments (get-assessments-for-monitoring monitoring-id)))
        (if (> (len assessments) u0)
            (/ (fold + (map get-survival-count assessments) u0) (len assessments))
            u0)))

(define-private (calculate-average-health-score (monitoring-id uint))
    (let ((assessments (get-assessments-for-monitoring monitoring-id)))
        (if (> (len assessments) u0)
            (/ (fold + (map get-health-status assessments) u0) (len assessments))
            u0)))

(define-private (get-assessments-for-monitoring (monitoring-id uint))
    (filter is-matching-assessment (list 
        (map-get? health-assessments u1) (map-get? health-assessments u2) (map-get? health-assessments u3)
        (map-get? health-assessments u4) (map-get? health-assessments u5))))

(define-private (is-matching-assessment (assessment (optional {id: uint, monitoring-id: uint, validator: principal, assessment-date: uint, health-status: uint, survival-count: uint, notes: (string-ascii 200), photo-hash: (string-ascii 64)})))
    (match assessment
        some-assessment true
        false))

(define-private (get-survival-count (assessment (optional {id: uint, monitoring-id: uint, validator: principal, assessment-date: uint, health-status: uint, survival-count: uint, notes: (string-ascii 200), photo-hash: (string-ascii 64)})))
    (match assessment
        some-assessment (get survival-count some-assessment)
        u0))

(define-private (get-health-status (assessment (optional {id: uint, monitoring-id: uint, validator: principal, assessment-date: uint, health-status: uint, survival-count: uint, notes: (string-ascii 200), photo-hash: (string-ascii 64)})))
    (match assessment
        some-assessment (get health-status some-assessment)
        u0))

(define-public (update-monitoring-settings (new-min-validators uint) (new-survival-bonus uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-min-validators u0) ERR_INVALID_AMOUNT)
        (asserts! (> new-survival-bonus u0) ERR_INVALID_AMOUNT)
        (var-set min-validators-required new-min-validators)
        (var-set survival-bonus-per-tree new-survival-bonus)
        (ok true)))

(define-public (update-credit-to-tree-ratio (new-ratio uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-ratio u0) ERR_INVALID_AMOUNT)
        (var-set credit-to-tree-ratio new-ratio)
        (ok true)))

(define-public (stake-tokens (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-stakes tx-sender (+ (default-to u0 (map-get? user-stakes tx-sender)) amount))
        (ok true)))

(define-public (withdraw-stake (amount uint))
    (let ((current-stake (default-to u0 (map-get? user-stakes tx-sender))))
        (asserts! (>= current-stake amount) ERR_INVALID_AMOUNT)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (map-set user-stakes tx-sender (- current-stake amount))
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (ok true)))

(define-public (claim-rewards)
    (let ((reward-amount (default-to u0 (map-get? user-rewards tx-sender))))
        (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
        (map-delete user-rewards tx-sender)
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (ok reward-amount)))

(define-public (update-min-stake (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set min-proposal-stake new-amount)
        (ok true)))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-proof (proof-id uint))
    (map-get? planting-proofs proof-id))

(define-read-only (get-user-vote (proposal-id uint) (user principal))
    (map-get? votes {proposal-id: proposal-id, voter: user}))

(define-read-only (get-user-stake (user principal))
    (default-to u0 (map-get? user-stakes user)))

(define-read-only (get-user-planted-trees (user principal))
    (default-to u0 (map-get? user-planted-trees user)))

(define-read-only (get-user-rewards (user principal))
    (default-to u0 (map-get? user-rewards user)))

(define-read-only (get-contract-stats)
    {
        total-proposals: (var-get proposal-counter),
        total-proofs: (var-get proof-counter),
        total-trees-planted: (var-get total-trees-planted),
        total-stx-distributed: (var-get total-stx-distributed),
        min-proposal-stake: (var-get min-proposal-stake)
    })

(define-read-only (get-proposal-funding-progress (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (let ((progress (if (> (get funding-goal proposal) u0)
                                    (/ (* (get funding-raised proposal) u100) (get funding-goal proposal))
                                    u0)))
                    (some {
                        funding-raised: (get funding-raised proposal),
                        funding-goal: (get funding-goal proposal),
                        progress-percentage: progress
                    }))
        none))

(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and (is-eq (get status proposal) "active")
                     (< stacks-block-height (get deadline proposal)))
        false))

(define-read-only (get-carbon-credit (credit-id uint))
    (map-get? carbon-credits credit-id))

(define-read-only (get-user-carbon-balance (user principal))
    (default-to u0 (map-get? user-carbon-balances user)))

(define-read-only (get-user-retired-credits (user principal))
    (default-to u0 (map-get? retired-credits user)))

(define-read-only (get-credit-ownership-history (credit-id uint))
    (default-to (list) (map-get? credit-ownership-history credit-id)))

(define-read-only (get-carbon-credit-stats)
    {
        total-credits-minted: (var-get total-credits-minted),
        total-credits-traded: (var-get total-credits-traded),
        credit-counter: (var-get credit-counter),
        credit-to-tree-ratio: (var-get credit-to-tree-ratio)
    })

(define-read-only (is-credit-expired (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (>= stacks-block-height (get expiry-date credit))
        true))

(define-read-only (get-credits-for-sale)
    (let ((credit-ids (map get-credit-if-for-sale (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))))
        (filter is-credit-some credit-ids)))

(define-private (is-credit-some (credit-option (optional uint)))
    (is-some credit-option))

(define-private (get-credit-if-for-sale (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (if (and (get for-sale credit) 
                       (not (get is-retired credit))
                       (< stacks-block-height (get expiry-date credit)))
                   (some credit-id)
                   none)
        none))

(define-read-only (calculate-credits-from-trees (tree-count uint))
    (/ tree-count (var-get credit-to-tree-ratio)))

(define-read-only (get-monitoring-record (monitoring-id uint))
    (map-get? tree-monitoring-records monitoring-id))

(define-read-only (get-health-assessment (assessment-id uint))
    (map-get? health-assessments assessment-id))

(define-read-only (get-user-monitoring-stats (user principal))
    (default-to {total-monitored: u0, total-survived: u0, total-bonus-earned: u0, validator-assessments: u0} 
                (map-get? user-monitoring-stats user)))

(define-read-only (get-monitoring-system-stats)
    {
        total-monitoring-records: (var-get monitoring-counter),
        total-trees-monitored: (var-get total-trees-monitored),
        total-health-assessments: (var-get total-health-assessments),
        survival-bonus-per-tree: (var-get survival-bonus-per-tree),
        min-validators-required: (var-get min-validators-required)
    })

(define-read-only (is-monitoring-active (monitoring-id uint))
    (match (map-get? tree-monitoring-records monitoring-id)
        record (and (is-eq (get status record) "active")
                   (< stacks-block-height (get monitoring-end record)))
        false))

(define-read-only (get-monitoring-progress (monitoring-id uint))
    (match (map-get? tree-monitoring-records monitoring-id)
        record (some {
            validators-confirmed: (get validators-confirmed record),
            validators-needed: (get validators-needed record),
            monitoring-end: (get monitoring-end record),
            blocks-remaining: (if (> (get monitoring-end record) stacks-block-height)
                                (- (get monitoring-end record) stacks-block-height)
                                u0)
        })
        none))

(define-read-only (calculate-expected-survival-bonus (monitoring-id uint) (expected-survival-rate uint))
    (match (map-get? tree-monitoring-records monitoring-id)
        record (* expected-survival-rate (var-get survival-bonus-per-tree))
        u0))

(define-private (get-monitoring-assessments (monitoring-id uint))
    (list))

(define-read-only (get-validator-status (monitoring-id uint) (validator principal))
    (map-get? monitoring-validators {monitoring-id: monitoring-id, validator: validator}))



