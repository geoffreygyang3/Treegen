;; TreeGen Community Achievement System
;; Tracks and rewards community milestones and achievements in the TreeGen ecosystem

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_AMOUNT (err u203))
(define-constant ERR_ALREADY_CLAIMED (err u204))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Achievement types
(define-constant BADGE_FIRST_PLANTER "first-planter")
(define-constant BADGE_TREE_MASTER "tree-master") 
(define-constant BADGE_CARBON_CHAMPION "carbon-champion")
(define-constant BADGE_COMMUNITY_LEADER "community-leader")
(define-constant BADGE_VALIDATOR_EXPERT "validator-expert")

;; Data variables
(define-data-var total-badges-earned uint u0)

;; Achievement definitions
(define-map achievement-definitions (string-ascii 30) {
    name: (string-ascii 50),
    description: (string-ascii 150),
    requirement-value: uint,
    reward-points: uint,
    is-active: bool
})

;; User achievements
(define-map user-achievements principal {
    total-points: uint,
    badges-earned: uint,
    trees-planted: uint,
    credits-earned: uint,
    proposals-created: uint,
    validations-completed: uint,
    community-rank: (string-ascii 20)
})

;; User badges
(define-map user-badges {user: principal, badge-type: (string-ascii 30)} {
    earned-at: uint,
    claimed: bool,
    points-awarded: uint
})

;; Initialize achievement definitions
(map-set achievement-definitions BADGE_FIRST_PLANTER {
    name: "First Tree Planter", description: "Plant your first tree in the TreeGen ecosystem",
    requirement-value: u1, reward-points: u100, is-active: true
})
(map-set achievement-definitions BADGE_TREE_MASTER {
    name: "Tree Master", description: "Plant 100 or more trees",
    requirement-value: u100, reward-points: u1000, is-active: true
})
(map-set achievement-definitions BADGE_CARBON_CHAMPION {
    name: "Carbon Champion", description: "Earn 50 or more carbon credits",
    requirement-value: u50, reward-points: u750, is-active: true
})
(map-set achievement-definitions BADGE_COMMUNITY_LEADER {
    name: "Community Leader", description: "Create 5 or more successful proposals",
    requirement-value: u5, reward-points: u500, is-active: true
})
(map-set achievement-definitions BADGE_VALIDATOR_EXPERT {
    name: "Validator Expert", description: "Complete 25 or more proof validations",
    requirement-value: u25, reward-points: u600, is-active: true
})

;; Public functions

;; Update user stats (called from main TreeGen contract)
(define-public (update-user-stats (user principal) (trees-planted uint) (credits-earned uint) (proposals-created uint) (validations-completed uint))
    (let ((current-stats (default-to {total-points: u0, badges-earned: u0, trees-planted: u0, credits-earned: u0, proposals-created: u0, validations-completed: u0, community-rank: "newcomer"} 
                                   (map-get? user-achievements user))))
        (map-set user-achievements user (merge current-stats {
            trees-planted: trees-planted,
            credits-earned: credits-earned,
            proposals-created: proposals-created,
            validations-completed: validations-completed
        }))
        (try! (check-and-unlock-achievements user))
        (try! (update-community-rank user))
        (ok true)))

;; Check and unlock achievements for a user
(define-public (check-and-unlock-achievements (user principal))
    (let ((user-stats (default-to {total-points: u0, badges-earned: u0, trees-planted: u0, credits-earned: u0, proposals-created: u0, validations-completed: u0, community-rank: "newcomer"} 
                                 (map-get? user-achievements user))))
        (begin
            (try! (check-badge-unlock user BADGE_FIRST_PLANTER (get trees-planted user-stats)))
            (try! (check-badge-unlock user BADGE_TREE_MASTER (get trees-planted user-stats)))
            (try! (check-badge-unlock user BADGE_CARBON_CHAMPION (get credits-earned user-stats)))
            (try! (check-badge-unlock user BADGE_COMMUNITY_LEADER (get proposals-created user-stats)))
            (try! (check-badge-unlock user BADGE_VALIDATOR_EXPERT (get validations-completed user-stats)))
            (ok true))))

;; Internal badge checking logic
(define-private (check-badge-unlock (user principal) (badge-type (string-ascii 30)) (user-value uint))
    (let ((achievement-def (unwrap! (map-get? achievement-definitions badge-type) ERR_NOT_FOUND))
          (badge-key {user: user, badge-type: badge-type})
          (existing-badge (map-get? user-badges badge-key)))
        (if (and (is-none existing-badge)
                 (>= user-value (get requirement-value achievement-def))
                 (get is-active achievement-def))
            (begin
                (map-set user-badges badge-key {
                    earned-at: stacks-block-height,
                    claimed: false,
                    points-awarded: (get reward-points achievement-def)
                })
                (var-set total-badges-earned (+ (var-get total-badges-earned) u1))
                (ok true))
            (ok false))))

;; Claim achievement rewards
(define-public (claim-achievement-rewards (badge-type (string-ascii 30)))
    (let ((badge-key {user: tx-sender, badge-type: badge-type})
          (badge (unwrap! (map-get? user-badges badge-key) ERR_NOT_FOUND))
          (achievement-def (unwrap! (map-get? achievement-definitions badge-type) ERR_NOT_FOUND)))
        (asserts! (not (get claimed badge)) ERR_ALREADY_CLAIMED)
        (map-set user-badges badge-key (merge badge {claimed: true}))
        (let ((current-stats (default-to {total-points: u0, badges-earned: u0, trees-planted: u0, credits-earned: u0, proposals-created: u0, validations-completed: u0, community-rank: "newcomer"} 
                                        (map-get? user-achievements tx-sender))))
            (map-set user-achievements tx-sender (merge current-stats {
                total-points: (+ (get total-points current-stats) (get points-awarded badge)),
                badges-earned: (+ (get badges-earned current-stats) u1)
            })))
        (ok (get points-awarded badge))))

;; Update community rank based on points
(define-private (update-community-rank (user principal))
    (let ((user-stats (unwrap! (map-get? user-achievements user) ERR_NOT_FOUND))
          (points (get total-points user-stats)))
        (let ((new-rank (if (>= points u2000) 
                            "master-planter"
                            (if (>= points u1000)
                                "tree-veteran"
                                (if (>= points u500)
                                    "green-contributor"
                                    (if (>= points u100)
                                        "sapling-grower"
                                        "newcomer"))))))
            (map-set user-achievements user (merge user-stats {community-rank: new-rank}))
            (ok new-rank))))

;; Read-only functions

;; Get user's achievement profile
(define-read-only (get-user-profile (user principal))
    (let ((stats (map-get? user-achievements user))
          (badges (get-user-badges user)))
        {
            stats: stats,
            badges: badges,
            unclaimed-rewards: (get-unclaimed-rewards user)
        }))

;; Get user's badges
(define-read-only (get-user-badges (user principal))
    (list
        (map-get? user-badges {user: user, badge-type: BADGE_FIRST_PLANTER})
        (map-get? user-badges {user: user, badge-type: BADGE_TREE_MASTER})
        (map-get? user-badges {user: user, badge-type: BADGE_CARBON_CHAMPION})
        (map-get? user-badges {user: user, badge-type: BADGE_COMMUNITY_LEADER})
        (map-get? user-badges {user: user, badge-type: BADGE_VALIDATOR_EXPERT})))

;; Get unclaimed rewards for user
(define-read-only (get-unclaimed-rewards (user principal))
    (let ((badges (get-user-badges user)))
        (fold + (map calculate-unclaimed-points badges) u0)))

;; Helper function for calculating unclaimed points
(define-private (calculate-unclaimed-points (badge (optional {earned-at: uint, claimed: bool, points-awarded: uint})))
    (match badge
        some-badge (if (get claimed some-badge) u0 (get points-awarded some-badge))
        u0))

;; Get achievement definition
(define-read-only (get-achievement-definition (badge-type (string-ascii 30)))
    (map-get? achievement-definitions badge-type))
(define-read-only (get-achievement-stats)
    {
        total-badges-earned: (var-get total-badges-earned)
    })

;; Check if user has specific badge
(define-read-only (has-badge (user principal) (badge-type (string-ascii 30)))
    (is-some (map-get? user-badges {user: user, badge-type: badge-type})))

;; Get user's rank in community
(define-read-only (get-user-rank (user principal))
    (match (map-get? user-achievements user)
        stats (get community-rank stats)
        "newcomer"))
;; Administrative functions
;; Add new achievement type (admin only)
(define-public (add-achievement-type (badge-type (string-ascii 30)) (name (string-ascii 50)) (description (string-ascii 150)) (requirement-value uint) (reward-points uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? achievement-definitions badge-type)) ERR_ALREADY_EXISTS)
        (asserts! (> requirement-value u0) ERR_INVALID_AMOUNT)
        (asserts! (> reward-points u0) ERR_INVALID_AMOUNT)
        (map-set achievement-definitions badge-type {
            name: name,
            description: description,
            requirement-value: requirement-value,
            reward-points: reward-points,
            is-active: true
        })
        (ok true)))

;; Toggle achievement active status (admin only)
(define-public (toggle-achievement (badge-type (string-ascii 30)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (let ((achievement (unwrap! (map-get? achievement-definitions badge-type) ERR_NOT_FOUND)))
            (map-set achievement-definitions badge-type (merge achievement {
                is-active: (not (get is-active achievement))
            }))
            (ok (not (get is-active achievement))))))
