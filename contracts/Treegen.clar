(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u105))
(define-constant ERR_PROOF_NOT_PENDING (err u106))
(define-constant ERR_INSUFFICIENT_STAKE (err u107))

(define-data-var proposal-counter uint u0)
(define-data-var proof-counter uint u0)
(define-data-var total-trees-planted uint u0)
(define-data-var total-stx-distributed uint u0)
(define-data-var min-proposal-stake uint u1000000)

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
