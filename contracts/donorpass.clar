(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-constant CLEARFUND_CONTRACT .clearfund)
(define-constant ERR_CLEARFUND_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))

(define-non-fungible-token donorpass uint)

(define-data-var lastTokenId uint u0)

(define-map nftUri { tokenId: uint } (string-ascii 50))

;; public functions
;;
(define-public (transfer (id uint) (sender principal) (recipient principal)) 
    (begin 
        (asserts! (is-eq sender tx-sender) ERR_NOT_TOKEN_OWNER)
        (nft-transfer? donorpass id sender recipient)
    )
    
)

(define-public (mint (recipient principal)) 
    (let (
        (newTokenId (+ (var-get lastTokenId) u1))
        )
        (asserts! (is-eq tx-sender CLEARFUND_CONTRACT ) ERR_CLEARFUND_ONLY)
        (var-set lastTokenId newTokenId)
        (nft-mint? donorpass newTokenId recipient) 
    )
)

;; read only functions
;;
(define-read-only (get-last-token-id) 
    (ok (var-get lastTokenId))
)

(define-read-only (get-owner (id uint)) 
    (ok (nft-get-owner? donorpass id))
)

(define-read-only (get-token-uri (id uint)) 
    (ok (map-get? nftUri {tokenId: id}))
)

