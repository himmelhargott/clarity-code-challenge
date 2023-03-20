(define-constant CONTRACT_ADDRESS (as-contract tx-sender))
(define-constant DEPLOYER tx-sender)
(define-constant ERR_TITLE_DESCRIPTION_LINK_EMPTY (err u101))
(define-constant ERR_INVALID_FUND_GOAL (err u102))
(define-constant ERR_START_NOT_VALID (err u103))
(define-constant ERR_END_NOT_VALID (err u104))
(define-constant ERR_ID_NOT_FOUND (err u105))
(define-constant ERR_CANNOT_CANCEL (err u106))
(define-constant ERR_NOT_OWNER (err u107))
(define-constant ERR_NOT_STARTED (err u108))
(define-constant ERR_ENDED (err u109))
(define-constant ERR_PLEDGE_GREATER_THAN_ZERO (err u110))
(define-constant ERR_STX_TRANSFER_FAILED (err u111))
(define-constant ERR_NOT_PLEDGED (err u112))
(define-constant ERR_INVALID_UNPLEDGE_AMT (err u113))
(define-constant ERR_NOT_ENDED (err u114))
(define-constant ERR_GOAL_NOT_MET (err u115))
(define-constant ERR_ALREADY_CLAIMED (err u116))
(define-constant ERR_TARGET_NOT_REACHED (err u117))



;; calculate roughly 90 days based on block times of 10 minutes
(define-constant FUNDING_TIME_LIMIT u12960)

(define-data-var last-id uint u0)

(define-map Campaigns uint {
    title: (string-utf8 256),
    description: (buff 33),
    link: (string-utf8 256),
    fundGoal: uint,
    startsAt: uint,
    endsAt: uint,
    campaignOwner: principal,
    pledgedCount: uint,
    pledgedAmount: uint,
    claimed: bool,
    targetReached: bool,
    targetReachedBy: uint
})

(define-map Investments {contributor: principal, campaignId: uint} {amount: uint})

;;private functions:
(define-private (is-investor (investor principal) (cId uint)) 
    (is-some (map-get? Investments {contributor: investor, campaignId: cId}))
)


;;public functions:
(define-public (launch 
                    (cTitle (string-utf8 256))
                    (cDescr (buff 33))
                    (cLink (string-utf8 256))
                    (cGoal uint)
                    (cStart uint)
                    (cEnd uint)
                ) 
    (let (
            (newId (+ (var-get last-id) u1))
        )
        (asserts! (> cGoal u0) ERR_INVALID_FUND_GOAL)
        (asserts! (< block-height cStart) ERR_START_NOT_VALID)
        (asserts! (< block-height cEnd) ERR_END_NOT_VALID)
        (asserts! (< cEnd (+ cStart FUNDING_TIME_LIMIT )) ERR_END_NOT_VALID)
        (asserts!   (and    (> (len cTitle) u0)
                            (and    (> (len cDescr) u0)  
                                    (> (len cLink) u0) 
                            )
                    ) 
                    ERR_TITLE_DESCRIPTION_LINK_EMPTY)

        (map-set Campaigns newId  {     title: cTitle, 
                                        description: cDescr, 
                                        link: cLink, 
                                        fundGoal: cGoal,
                                        startsAt: cStart,
                                        endsAt: cEnd,
                                        campaignOwner: tx-sender,
                                        pledgedCount: u0,
                                        pledgedAmount: u0,
                                        claimed: false,
                                        targetReached: false,
                                        targetReachedBy: u0
                                    })
        (ok newId)
    )
)


(define-public (cancel (cId uint)) 
    (begin
        (asserts!   (< block-height (get startsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND ))) ERR_CANNOT_CANCEL)
        (asserts! (is-eq (get campaignOwner (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND )) tx-sender) ERR_NOT_OWNER)
        
        (map-delete Campaigns cId)
        (ok cId)
    )
)


(define-public (update  (cId uint) 
                        (cTitle (string-utf8 50)) 
                        (cDescr (buff 33)) 
                        (cLink (string-utf8 256))
                )
    (begin 
        (asserts! (is-eq tx-sender (get campaignOwner (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_NOT_OWNER)
        (asserts! (< block-height (get endsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_ENDED)
        (asserts!   (and    (> (len cTitle) u0)
                            (and    (> (len cDescr) u0)  
                                    (> (len cLink) u0) 
                            )
                    ) 
                    ERR_TITLE_DESCRIPTION_LINK_EMPTY)

        (ok (map-set Campaigns cId  (merge (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)  {title: cTitle, description: cDescr, link: cLink} )))
    )
)


(define-public (claim (cId uint)) 
    (let (
            (cOwner (get campaignOwner (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
        )  
        (asserts! (is-eq tx-sender cOwner) ERR_NOT_OWNER)
        (asserts! (get targetReached (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)) ERR_TARGET_NOT_REACHED)
        (asserts! (not (get claimed (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_ALREADY_CLAIMED)
        
        (map-set Campaigns cId (merge (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND) {claimed: true}))
        (as-contract (stx-transfer? (get pledgedAmount (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)) CONTRACT_ADDRESS cOwner))
    )    
)


(define-public (pledge (cId uint) (investAmount uint)) 
    (let (
            (investor tx-sender)
            (cGoal (get fundGoal (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
            (cReachedBy (get targetReachedBy (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
            (cReached (get targetReached (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
            (cPledgedAmount (+ investAmount (get pledgedAmount (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))))
            (contributedAmount (default-to u0 (get amount (map-get? Investments {contributor: tx-sender, campaignId: cId}))))
        )
        (asserts! (> block-height (get startsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_NOT_STARTED)
        (asserts! (< block-height (get endsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_ENDED)
        (asserts! (> investAmount u0) ERR_PLEDGE_GREATER_THAN_ZERO)

        (if (> investAmount u500)
            (try! (as-contract (contract-call? .donorpass mint investor)))
            true
        )

        (map-set Investments {contributor: tx-sender, campaignId: cId} {amount: (+ investAmount contributedAmount)})
        (map-set Campaigns cId (merge   (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND) 
                                        {   pledgedCount: (+    (if (is-eq contributedAmount u0) u1 u0) 
                                                                (get pledgedCount (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND ))),
                                            pledgedAmount: cPledgedAmount,
                                            targetReached: (if (> cPledgedAmount cGoal) true false),
                                            targetReachedBy:  (+ block-height u1)
                                        }))
        (stx-transfer? investAmount tx-sender CONTRACT_ADDRESS)
    )
)


(define-public (unpledge (cId uint) (unpledgeAmount uint)) 
    (let (
            (investor tx-sender)
            (pledgedAmount (get amount (unwrap! (map-get? Investments {contributor: investor, campaignId: cId}) ERR_NOT_PLEDGED)))
            (pledgeCount (get pledgedCount (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND )))
            (cPledgedAmount (get pledgedAmount (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND )))
        )
        (asserts! (< block-height (get endsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))) ERR_ENDED)
        (asserts! (<= unpledgeAmount pledgedAmount) ERR_INVALID_UNPLEDGE_AMT)
        (if (is-eq unpledgeAmount pledgedAmount) 
            (begin 
                (map-delete Investments {contributor: investor, campaignId: cId})
                (map-set Campaigns cId (merge (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND ) {pledgedCount: (- pledgeCount u1)})) 
            )
            (map-set Investments {contributor: investor, campaignId: cId} {amount: (- pledgedAmount unpledgeAmount)})
        )
        (map-set Campaigns cId (merge (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND ) {pledgedAmount: (- cPledgedAmount unpledgeAmount)}))
        (as-contract (stx-transfer? unpledgeAmount CONTRACT_ADDRESS investor))
    )
)


(define-public (refund (cId uint)) 
    (let (
            (customer tx-sender)
            (cEnd (get endsAt (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
            (cTargetReached (get targetReached (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND)))
            (iAmount (get amount (unwrap! (map-get? Investments {contributor: customer, campaignId: cId}) ERR_NOT_PLEDGED)))
        ) 

        (asserts! (> block-height cEnd) ERR_NOT_ENDED)
        (asserts! (not cTargetReached) ERR_GOAL_NOT_MET)
        (as-contract (stx-transfer? iAmount tx-sender customer))
     )
)


;;read-only functions:
(define-read-only (get-campaign (cId uint)) 
    (ok (unwrap! (map-get? Campaigns cId) ERR_ID_NOT_FOUND))
)

(define-read-only (get-investment (cId uint) (investor principal)) 
    (ok (map-get? Investments {contributor: investor, campaignId: cId}) )
)

