;; ===============================================
;; DAO Governance Smart Contract
;; ===============================================
;; Author: Claude
;; Version: 1.0
;; Description: A comprehensive DAO governance smart contract for the Stacks blockchain
;; with voting, proposal management, treasury, and member management capabilities.

;; ===============================================
;; Constants
;; ===============================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-PROPOSAL-EXISTS (err u402))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u403))
(define-constant ERR-VOTING-CLOSED (err u404))
(define-constant ERR-ALREADY-VOTED (err u405))
(define-constant ERR-INSUFFICIENT-TOKENS (err u406))
(define-constant ERR-PROPOSAL-ACTIVE (err u407))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u408))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u409))
(define-constant ERR-PROPOSAL-NOT-READY (err u410))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u411))
(define-constant ERR-QUORUM-NOT-REACHED (err u412))
(define-constant ERR-MEMBER-NOT-FOUND (err u413))
(define-constant ERR-MEMBER-EXISTS (err u414))
(define-constant ERR-TREASURY-INSUFFICIENT-FUNDS (err u415))
(define-constant ERR-INVALID-AMOUNT (err u416))
(define-constant ERR-INVALID-PARAMETER (err u417))
(define-constant ERR-MEMBERSHIP-EXPIRED (err u418))
(define-constant ERR-COOLDOWN-PERIOD (err u419))
(define-constant ERR-TOKEN-TRANSFER-FAILED (err u420))

;; Proposal types
(define-constant PROPOSAL-TYPE-GOVERNANCE u1)
(define-constant PROPOSAL-TYPE-TREASURY u2)
(define-constant PROPOSAL-TYPE-MEMBERSHIP u3)
(define-constant PROPOSAL-TYPE-CONTRACT u4)
(define-constant PROPOSAL-TYPE-PARAMETER u5)

;; Voting options
(define-constant VOTE-FOR u1)
(define-constant VOTE-AGAINST u2)
(define-constant VOTE-ABSTAIN u3)

;; Governance parameters
(define-constant DEFAULT-VOTING-PERIOD u144)  ;; ~1 day at 10 min blocks
(define-constant DEFAULT-QUORUM-THRESHOLD u500)  ;; 50.0% (scaled by 10)
(define-constant DEFAULT-EXECUTION-DELAY u72)  ;; ~12 hours at 10 min blocks
(define-constant DEFAULT-APPROVAL-THRESHOLD u667)  ;; 66.7% (scaled by 10)
(define-constant DEFAULT-MEMBERSHIP-DURATION u52560)  ;; ~1 year at 10 min blocks

;; ===============================================
;; Data Variables
;; ===============================================

;; DAO configuration
(define-data-var dao-name (string-ascii 50) "DefaultDAO")
(define-data-var dao-description (string-ascii 255) "A decentralized autonomous organization built on Stacks")
(define-data-var dao-token principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token)
(define-data-var dao-admin principal tx-sender)
;; Governance parameters
(define-data-var voting-period uint DEFAULT-VOTING-PERIOD)
(define-data-var quorum-threshold uint DEFAULT-QUORUM-THRESHOLD)
(define-data-var execution-delay uint DEFAULT-EXECUTION-DELAY)
(define-data-var approval-threshold uint DEFAULT-APPROVAL-THRESHOLD)
(define-data-var membership-duration uint DEFAULT-MEMBERSHIP-DURATION)
(define-data-var proposal-cooldown uint u72)  ;; 12 hours

;; Counters
(define-data-var proposal-count uint u0)
(define-data-var member-count uint u0)
(define-data-var treasury-balance uint u0)

;; ===============================================
;; Maps
;; ===============================================

;; Proposals storage
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    proposal-type: uint,
    start-block-height: uint,
    end-block-height: uint,
    execution-block-height: uint,
    votes-for: uint,
    votes-against: uint,
    votes-abstain: uint,
    total-voting-power: uint,
    status: uint,  ;; 0:Active, 1:Passed, 2:Rejected, 3:Executed, 4:Failed
    executed: bool,
    execution-data: (optional (buff 1024))
  }
)

;; Votes tracking
(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: uint, voting-power: uint }
)

;; DAO members
(define-map members
  { member: principal }
  {
    joining-block: uint,
    expiry-block: uint,
    voting-power: uint,
    roles: (list 10 (string-ascii 20)),
    active: bool
  }
)

;; Proposal type specific data
(define-map treasury-proposals
  { proposal-id: uint }
  {
    recipient: principal,
    amount: uint,
    description: (string-ascii 200)
  }
)

(define-map membership-proposals
  { proposal-id: uint }
  {
    target-member: principal,
    action: uint,  ;; 1:Add, 2:Remove, 3:UpdateRoles
    roles: (list 10 (string-ascii 20)),
    voting-power: uint
  }
)

(define-map parameter-proposals
  { proposal-id: uint }
  {
    parameter-name: (string-ascii 50),
    parameter-value: uint
  }
)

(define-map contract-proposals
  { proposal-id: uint }
  {
    contract-address: principal,
    function-name: (string-ascii 128),
    function-args: (list 10 (string-ascii 100))
  }
)

;; DAO treasury transactions
(define-map treasury-transactions
  { tx-id: uint }
  {
    tx-type: uint,  ;; 1:Deposit, 2:Withdrawal
    amount: uint,
    sender-receiver: principal,
    block-height: uint,
    proposal-id: (optional uint),
    description: (string-ascii 200)
  }
)

;; ===============================================
;; Authorization and Helper Functions
;; ===============================================

;; Check if caller is the DAO admin
(define-private (is-dao-admin)
  (is-eq tx-sender (var-get dao-admin))
)

;; Check if caller is an active member
(define-private (is-active-member (user principal))
  (match (map-get? members { member: user })
    member-data (and 
                  (get active member-data)
                  (>= (get expiry-block member-data) block-height))
    false
  )
)

;; Get the voting power of a member
(define-read-only (get-voting-power (user principal))
  (match (map-get? members { member: user })
    member-data (if (and
                     (get active member-data)
                     (>= (get expiry-block member-data) block-height))
                    (get voting-power member-data)
                    u0)
    u0
  )
)

;; ===============================================
;; Initialization and DAO Configuration
;; ===============================================

;; Initialize DAO with custom parameters
(define-public (initialize-dao
                (name (string-ascii 50))
                (description (string-ascii 255))
                (token principal)
                (admin principal)
                (v-period uint)
                (q-threshold uint)
                (e-delay uint)
                (a-threshold uint)
                (m-duration uint))
  (begin
    ;; Only allow initialization once
    (asserts! (is-eq (var-get proposal-count) u0) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq tx-sender admin) ERR-NOT-AUTHORIZED)
    
    ;; Set DAO configuration
    (var-set dao-name name)
    (var-set dao-description description)
    (var-set dao-token token)
    (var-set dao-admin admin)
    
    ;; Set governance parameters
    (var-set voting-period v-period)
    (var-set quorum-threshold q-threshold)
    (var-set execution-delay e-delay)
    (var-set approval-threshold a-threshold)
    (var-set membership-duration m-duration)
    
    ;; Add admin as first member
    (map-set members
      { member: admin }
      {
        joining-block: block-height,
        expiry-block: (+ block-height m-duration),
        voting-power: u100,
        roles: (list "admin"),
        active: true
      }
    )
    
    ;; Increment member count
    (var-set member-count u1)
    
    (ok true)
  )
)
