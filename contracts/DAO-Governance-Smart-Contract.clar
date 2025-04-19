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
