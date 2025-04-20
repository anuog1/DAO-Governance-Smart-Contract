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
;; Transfer DAO admin rights
(define-public (transfer-dao-admin (new-admin principal))
  (begin
    (asserts! (is-dao-admin) ERR-NOT-AUTHORIZED)
    (var-set dao-admin new-admin)
    (ok true)
  )
)

;; Update a governance parameter
(define-public (update-governance-parameter (parameter-name (string-ascii 50)) (parameter-value uint))
  (begin
    (asserts! (is-dao-admin) ERR-NOT-AUTHORIZED)
    
    (match parameter-name
      "voting-period" (var-set voting-period parameter-value)
      "quorum-threshold" (var-set quorum-threshold parameter-value)
      "execution-delay" (var-set execution-delay parameter-value)
      "approval-threshold" (var-set approval-threshold parameter-value)
      "membership-duration" (var-set membership-duration parameter-value)
      "proposal-cooldown" (var-set proposal-cooldown parameter-value)
      (err ERR-INVALID-PARAMETER)
    )
    
    (ok true)
  )
)

;; ===============================================
;; Proposal Management
;; ===============================================

;; Create a new governance proposal
(define-public (create-governance-proposal
                (title (string-ascii 100))
                (description (string-ascii 500))
                (proposal-type uint)
                (execution-data (optional (buff 1024))))
  (let
    (
      (new-proposal-id (var-get proposal-count))
      (start-block (+ block-height u1))
      (end-block (+ start-block (var-get voting-period)))
      (execution-block (+ end-block (var-get execution-delay)))
      (user-voting-power (get-voting-power tx-sender))
    )
    
    ;; Check if user is an active member
    (asserts! (is-active-member tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check if proposal type is valid
    (asserts! (or
                (is-eq proposal-type PROPOSAL-TYPE-GOVERNANCE)
                (is-eq proposal-type PROPOSAL-TYPE-TREASURY)
                (is-eq proposal-type PROPOSAL-TYPE-MEMBERSHIP)
                (is-eq proposal-type PROPOSAL-TYPE-CONTRACT)
                (is-eq proposal-type PROPOSAL-TYPE-PARAMETER))
              ERR-INVALID-PROPOSAL-TYPE)
    
    ;; Check if user has enough voting power
    (asserts! (>= user-voting-power u10) ERR-INSUFFICIENT-TOKENS)
    
    ;; Create proposal
    (map-set proposals
      { proposal-id: new-proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        proposal-type: proposal-type,
        start-block-height: start-block,
        end-block-height: end-block,
        execution-block-height: execution-block,
        votes-for: u0,
        votes-against: u0,
        votes-abstain: u0,
        total-voting-power: u0,
        status: u0,  ;; Active
        executed: false,
        execution-data: execution-data
      }
    )
    
    ;; Increment proposal count
    (var-set proposal-count (+ new-proposal-id u1))
    
    (ok new-proposal-id)
  )
)

;; Create a treasury proposal
(define-public (create-treasury-proposal
                (title (string-ascii 100))
                (desc (string-ascii 500))
                (recipient principal)
                (amount uint)
                (tx-description (string-ascii 200)))
  (let
    (
      (proposal-result (try! (create-governance-proposal title desc PROPOSAL-TYPE-TREASURY none)))
    )
    
    ;; Validate amount
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Store treasury-specific proposal data
    (map-set treasury-proposals
      { proposal-id: proposal-result }
      {
        recipient: recipient,
        amount: amount,
        description: tx-description
      }
    )
    
    (ok proposal-result)
  )
)

;; Create a membership proposal
(define-public (create-membership-proposal
                (title (string-ascii 100))
                (desc (string-ascii 500))
                (target principal)
                (action uint)
                (roles (list 10 (string-ascii 20)))
                (power uint))
  (let
    (
      (proposal-result (try! (create-governance-proposal title desc PROPOSAL-TYPE-MEMBERSHIP none)))
    )
    
    ;; Validate action (1:Add, 2:Remove, 3:UpdateRoles)
    (asserts! (or (is-eq action u1) (is-eq action u2) (is-eq action u3)) ERR-INVALID-PARAMETER)
    
    ;; Store membership-specific proposal data
    (map-set membership-proposals
      { proposal-id: proposal-result }
      {
        target-member: target,
        action: action,
        roles: roles,
        voting-power: power
      }
    )
    
    (ok proposal-result)
  )
)

;; Create a parameter change proposal
(define-public (create-parameter-proposal
                (title (string-ascii 100))
                (desc (string-ascii 500))
                (param-name (string-ascii 50))
                (param-value uint))
  (let
    (
      (proposal-result (try! (create-governance-proposal title desc PROPOSAL-TYPE-PARAMETER none)))
    )
    
    ;; Store parameter-specific proposal data
    (map-set parameter-proposals
      { proposal-id: proposal-result }
      {
        parameter-name: param-name,
        parameter-value: param-value
      }
    )
    
    (ok proposal-result)
  )
)

;; Create a contract call proposal
(define-public (create-contract-proposal
                (title (string-ascii 100))
                (desc (string-ascii 500))
                (contract principal)
                (function (string-ascii 128))
                (args (list 10 (string-ascii 100))))
  (let
    (
      (proposal-result (try! (create-governance-proposal title desc PROPOSAL-TYPE-CONTRACT none)))
    )
    
    ;; Store contract-specific proposal data
    (map-set contract-proposals
      { proposal-id: proposal-result }
      {
        contract-address: contract,
        function-name: function,
        function-args: args
      }
    )
    
    (ok proposal-result)
  )
)

;; ===============================================
;; Voting Functions
;; ===============================================

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-type uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (user-voting-power (get-voting-power tx-sender))
      (curr-block block-height)
    )
    
    ;; Check if user is an active member
    (asserts! (is-active-member tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check if proposal is still active for voting
    (asserts! (<= curr-block (get end-block-height proposal)) ERR-VOTING-CLOSED)
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Check if vote type is valid
    (asserts! (or (is-eq vote-type VOTE-FOR) (is-eq vote-type VOTE-AGAINST) (is-eq vote-type VOTE-ABSTAIN)) ERR-INVALID-PARAMETER)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-type, voting-power: user-voting-power }
    )
    
    ;; Update proposal vote counts
    (match vote-type
      VOTE-FOR (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { 
                  votes-for: (+ (get votes-for proposal) user-voting-power),
                  total-voting-power: (+ (get total-voting-power proposal) user-voting-power)
                }))
      VOTE-AGAINST (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { 
                  votes-against: (+ (get votes-against proposal) user-voting-power),
                  total-voting-power: (+ (get total-voting-power proposal) user-voting-power)
                }))
      VOTE-ABSTAIN (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { 
                  votes-abstain: (+ (get votes-abstain proposal) user-voting-power),
                  total-voting-power: (+ (get total-voting-power proposal) user-voting-power)
                }))
      ERR-INVALID-PARAMETER
    )
    
    (ok true)
  )
)
;; Finalize a proposal after voting period
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (curr-block block-height)
      (votes-for (get votes-for proposal))
      (votes-against (get votes-against proposal))
      (total-votes (get total-voting-power proposal))
      (quorum (/ (* (var-get quorum-threshold) (var-get member-count)) u1000))  ;; Scale quorum threshold
      (approval (/ (* (var-get approval-threshold) total-votes) u1000))  ;; Scale approval threshold
      (new-status (if (< total-votes quorum)
                      u2  ;; Rejected due to quorum not reached
                      (if (>= votes-for approval)
                          u1  ;; Passed
                          u2)))  ;; Rejected
    )
    
    ;; Check if proposal voting period has ended
    (asserts! (> curr-block (get end-block-height proposal)) ERR-PROPOSAL-ACTIVE)
    
    ;; Check if proposal is still active (not finalized yet)
    (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-NOT-READY)
    
    ;; Update proposal status
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: new-status })
    )
    
    (ok new-status)
  )
)

;; ===============================================
;; Proposal Execution
;; ===============================================

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (curr-block block-height)
    )
    
    ;; Check if proposal passed
    (asserts! (is-eq (get status proposal) u1) ERR-PROPOSAL-NOT-PASSED)
    
    ;; Check if proposal is ready for execution
    (asserts! (>= curr-block (get execution-block-height proposal)) ERR-PROPOSAL-NOT-READY)
    
    ;; Check if proposal has not been executed yet
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
    
    ;; Execute proposal based on type
    (match (get proposal-type proposal)
      PROPOSAL-TYPE-GOVERNANCE (execute-governance-proposal proposal-id)
      PROPOSAL-TYPE-TREASURY (execute-treasury-proposal proposal-id)
      PROPOSAL-TYPE-MEMBERSHIP (execute-membership-proposal proposal-id)
      PROPOSAL-TYPE-CONTRACT (execute-contract-proposal proposal-id)
      PROPOSAL-TYPE-PARAMETER (execute-parameter-proposal proposal-id)
      ERR-INVALID-PROPOSAL-TYPE
    )
  )
)

;; Execute a governance proposal
(define-private (execute-governance-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, status: u3 })
    )
    
    (ok true)
  )
)

;; Execute a treasury proposal
(define-private (execute-treasury-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (treasury-data (unwrap! (map-get? treasury-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (recipient (get recipient treasury-data))
      (amount (get amount treasury-data))
      (description (get description treasury-data))
      (current-balance (var-get treasury-balance))
      (tx-id (var-get proposal-count))
    )
    
    ;; Check if treasury has enough funds
    (asserts! (>= current-balance amount) ERR-TREASURY-INSUFFICIENT-FUNDS)
    
    ;; Transfer tokens
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update treasury balance
    (var-set treasury-balance (- current-balance amount))
    
    ;; Record transaction
    (map-set treasury-transactions
      { tx-id: tx-id }
      {
        tx-type: u2,  ;; Withdrawal
        amount: amount,
        sender-receiver: recipient,
        block-height: block-height,
        proposal-id: (some proposal-id),
        description: description
      }
    )
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, status: u3 })
    )
    
    (ok true)
  )
)

;; Execute a membership proposal
(define-private (execute-membership-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (membership-data (unwrap! (map-get? membership-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (target (get target-member membership-data))
      (action (get action membership-data))
      (roles (get roles membership-data))
      (power (get voting-power membership-data))
      (curr-member-count (var-get member-count))
    )
    
    ;; Execute based on action type
    (match action
      ;; Add member
      u1 (begin
            ;; Check if member already exists
            (asserts! (is-none (map-get? members { member: target })) ERR-MEMBER-EXISTS)
            
            ;; Add new member
            (map-set members
              { member: target }
              {
                joining-block: block-height,
                expiry-block: (+ block-height (var-get membership-duration)),
                voting-power: power,
                roles: roles,
                active: true
              }
            )
            
            ;; Increment member count
            (var-set member-count (+ curr-member-count u1))
          )
      
      ;; Remove member
      u2 (begin
            ;; Check if member exists
            (asserts! (is-some (map-get? members { member: target })) ERR-MEMBER-NOT-FOUND)
            
            ;; Remove member
            (map-delete members { member: target })
            
            ;; Decrement member count
            (var-set member-count (- curr-member-count u1))
          )
      
      ;; Update member roles
      u3 (begin
            ;; Get existing member data
            (match (map-get? members { member: target })
              member-data (map-set members
                            { member: target }
                            (merge member-data {
                              voting-power: power,
                              roles: roles
                            })
                          )
              ERR-MEMBER-NOT-FOUND
            )
          )
      
      ERR-INVALID-PARAMETER
    )
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, status: u3 })
    )
    
    (ok true)
  )
)

;; Execute a parameter change proposal
(define-private (execute-parameter-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (parameter-data (unwrap! (map-get? parameter-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (param-name (get parameter-name parameter-data))
      (param-value (get parameter-value parameter-data))
    )
    
    ;; Update parameter
    (try! (update-governance-parameter param-name param-value))
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, status: u3 })
    )
    
    (ok true)
  )
)

;; Execute a contract call proposal (simplified for compatibility)
(define-private (execute-contract-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    
    ;; Note: In a real implementation, we would parse the contract-proposals data
    ;; and execute the contract call, but for simplicity, we'll just mark as executed
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, status: u3 })
    )
    
    (ok true)
  )
)
;; ===============================================
;; Treasury Management
;; ===============================================

;; Deposit funds to the DAO treasury
(define-public (deposit-to-treasury (amount uint) (description (string-ascii 200)))
  (let
    (
      (tx-id (var-get proposal-count))
      (current-balance (var-get treasury-balance))
    )
    
    ;; Check if amount is valid
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update treasury balance
    (var-set treasury-balance (+ current-balance amount))
    
    ;; Record transaction
    (map-set treasury-transactions
      { tx-id: tx-id }
      {
        tx-type: u1,  ;; Deposit
        amount: amount,
        sender-receiver: tx-sender,
        block-height: block-height,
        proposal-id: none,
        description: description
      }
    )
    
    ;; Increment counter for next tx ID
    (var-set proposal-count (+ tx-id u1))
    
    (ok true)
  )
)

;; Admin can withdraw emergency funds
(define-public (emergency-withdrawal (amount uint) (recipient principal) (description (string-ascii 200)))
  (let
    (
      (tx-id (var-get proposal-count))
      (current-balance (var-get treasury-balance))
    )
    
    ;; Check if caller is admin
    (asserts! (is-dao-admin) ERR-NOT-AUTHORIZED)
    
    ;; Check if amount is valid and treasury has enough funds
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= current-balance amount) ERR-TREASURY-INSUFFICIENT-FUNDS)
    
    ;; Transfer STX to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update treasury balance
    (var-set treasury-balance (- current-balance amount))
    
    ;; Record transaction
    (map-set treasury-transactions
      { tx-id: tx-id }
      {
        tx-type: u2,  ;; Withdrawal
        amount: amount,
        sender-receiver: recipient,
        block-height: block-height,
        proposal-id: none,
        description: (concat "EMERGENCY: " description)
      }
    )
    
    ;; Increment counter for next tx ID
    (var-set proposal-count (+ tx-id u1))
    
    (ok true)
  )
)

;; ===============================================
;; Membership Management
;; ===============================================

;; Admin can add members directly
(define-public (admin-add-member (new-member principal) (roles (list 10 (string-ascii 20))) (power uint))
  (let
    (
      (curr-member-count (var-get member-count))
    )
    
    ;; Check if caller is admin
    (asserts! (is-dao-admin) ERR-NOT-AUTHORIZED)
    
    ;; Check if member already exists
    (asserts! (is-none (map-get? members { member: new-member })) ERR-MEMBER-EXISTS)
    
    ;; Add new member
    (map-set members
      { member: new-member }
      {
        joining-block: block-height,
        expiry-block: (+ block-height (var-get membership-duration)),
        voting-power: power,
        roles: roles,
        active: true
      }
    )
    
    ;; Increment member count
    (var-set member-count (+ curr-member-count u1))
    
    (ok true)
  )
)

;; Admin can remove members directly
(define-public (admin-remove-member (target principal))
  (let
    (
      (curr-member-count (var-get member-count))
    )
    
    ;; Check if caller is admin
    (asserts! (is-dao-admin) ERR-NOT-AUTHORIZED)
