;; HealthRecord-Access
;; A medical record sharing platform that manages patient consent and doctor access

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-registered (err u102))

;; Data Maps
(define-map patients 
    principal 
    {consent-status: bool}
)

(define-map doctors
    principal 
    {verified: bool}
)

(define-map access-permissions
    {patient: principal, doctor: principal}
    {granted: bool, emergency-access: bool}
)

;; Public Functions
(define-public (register-patient)
    (if (is-some (map-get? patients tx-sender))
        err-already-registered
        (ok (map-set patients tx-sender {consent-status: false}))
    )
)

(define-public (register-doctor)
    (if (is-some (map-get? doctors tx-sender))
        err-already-registered
        (ok (map-set doctors tx-sender {verified: false}))
    )
)

(define-public (verify-doctor (doctor-principal principal))
    (if (is-eq tx-sender contract-owner)
        (ok (map-set doctors doctor-principal {verified: true}))
        err-not-authorized
    )
)

(define-public (grant-access (doctor-principal principal))
    (let (
        (patient-exists (map-get? patients tx-sender))
        (doctor-exists (map-get? doctors doctor-principal))
    )
    (if (and (is-some patient-exists) 
             (is-some doctor-exists)
             (get verified (unwrap! doctor-exists err-not-registered)))
        (ok (map-set access-permissions 
            {patient: tx-sender, doctor: doctor-principal}
            {granted: true, emergency-access: false}))
        err-not-authorized))
)

(define-public (revoke-access (doctor-principal principal))
    (ok (map-delete access-permissions {patient: tx-sender, doctor: doctor-principal}))
)

;; Read-only Functions
(define-read-only (check-access (patient-principal principal) (doctor-principal principal))
    (default-to 
        {granted: false, emergency-access: false}
        (map-get? access-permissions {patient: patient-principal, doctor: doctor-principal})
    )
)

(define-read-only (is-verified-doctor (doctor-principal principal))
    (default-to 
        false
        (get verified (map-get? doctors doctor-principal))
    )
)



(define-read-only (is-registered-patient (patient-principal principal))
    (default-to 
        false
        (get consent-status (map-get? patients patient-principal))
    )
)
(define-read-only (is-registered-doctor (doctor-principal principal))
    (default-to 
        false
        (get verified (map-get? doctors doctor-principal))
    )
)
(define-read-only (get-consent-status (patient-principal principal))
    (default-to 
        false
        (get consent-status (map-get? patients patient-principal))
    )
)
(define-read-only (get-doctor-status (doctor-principal principal))
    (default-to 
        false
        (get verified (map-get? doctors doctor-principal))
    )
)
(define-read-only (get-patient-status (patient-principal principal))
    (default-to 
        false
        (get consent-status (map-get? patients patient-principal))
    )
)
(define-read-only (get-emergency-access (patient-principal principal) (doctor-principal principal))
    (default-to 
        false
        (get emergency-access (map-get? access-permissions {patient: patient-principal, doctor: doctor-principal}))
    )
)
(define-read-only (get-granted-access (patient-principal principal) (doctor-principal principal))
    (default-to 
        false
        (get granted (map-get? access-permissions {patient: patient-principal, doctor: doctor-principal}))
    )
)
(define-read-only (get-patient-consent-status (patient-principal principal))
    (default-to 
        false
        (get consent-status (map-get? patients patient-principal))
    )
)
(define-read-only (get-doctor-verified-status (doctor-principal principal))
    (default-to 
        false
        (get verified (map-get? doctors doctor-principal))
    )
)



;; Add to data maps
(define-map access-durations
    {patient: principal, doctor: principal}
    {expiry: uint}
)

;; New functions
(define-public (grant-timed-access (doctor-principal principal) (duration uint))
    (let ((current-block stacks-block-height))
        (if (get verified (default-to {verified: false} (map-get? doctors doctor-principal)))
            (begin
                (try! (grant-access doctor-principal))
                (ok (map-set access-durations 
                    {patient: tx-sender, doctor: doctor-principal}
                    {expiry: (+ current-block duration)})))
            err-not-authorized)
    )
)

(define-read-only (is-access-valid (patient-principal principal) (doctor-principal principal))
    (let ((expiry (get expiry (default-to {expiry: u0} (map-get? access-durations {patient: patient-principal, doctor: doctor-principal})))))
        (< stacks-block-height expiry))
)


;; Add to data maps
(define-map doctor-specializations
    principal
    {specialization: (string-ascii 30), certification: (string-ascii 50)}
)

;; New functions
(define-public (add-specialization (specialization (string-ascii 30)) (certification (string-ascii 50)))
    (if (is-verified-doctor tx-sender)
        (ok (map-set doctor-specializations
            tx-sender
            {specialization: specialization, certification: certification}))
        err-not-authorized)
)

(define-read-only (get-doctor-specialization (doctor-principal principal))
    (default-to 
        {specialization: "", certification: ""}
        (map-get? doctor-specializations doctor-principal))
)


;; Add to data maps
(define-map access-history
    {patient: principal}
    {accesses: (list 50 {doctor: principal, block: uint})}
)

;; New functions
(define-public (log-access (patient-principal principal))
    (let (
        (current-history (default-to {accesses: (list )} (map-get? access-history {patient: patient-principal})))
        (new-access {doctor: tx-sender, block: stacks-block-height})
    )
    (if (get granted (check-access patient-principal tx-sender))
        (ok (map-set access-history
            {patient: patient-principal}
            {accesses: (unwrap! (as-max-len? (append (get accesses current-history) new-access) u50) err-not-authorized)}))
        err-not-authorized))
)

(define-read-only (get-access-history (patient-principal principal))
    (get accesses (default-to {accesses: (list )} (map-get? access-history {patient: patient-principal})))
)


;; Add to data maps
(define-map sharing-preferences
    principal
    {research-allowed: bool, anonymous-sharing: bool, third-party-sharing: bool}
)

;; New functions
(define-public (set-sharing-preferences (research-allowed bool) (anonymous-sharing bool) (third-party-sharing bool))
    (if (is-some (map-get? patients tx-sender))
        (ok (map-set sharing-preferences
            tx-sender
            {research-allowed: research-allowed, 
             anonymous-sharing: anonymous-sharing, 
             third-party-sharing: third-party-sharing}))
        err-not-registered)
)

(define-read-only (get-sharing-preferences (patient-principal principal))
    (default-to
        {research-allowed: false, anonymous-sharing: false, third-party-sharing: false}
        (map-get? sharing-preferences patient-principal))
)


(define-read-only (get-patient-sharing-preferences (patient-principal principal))
    (default-to
        {research-allowed: false, anonymous-sharing: false, third-party-sharing: false}
        (map-get? sharing-preferences patient-principal))
)
(define-read-only (get-doctor-sharing-preferences (doctor-principal principal))
    (default-to
        {research-allowed: false, anonymous-sharing: false, third-party-sharing: false}
        (map-get? sharing-preferences doctor-principal))
)



;; Add to data maps
(define-map emergency-responders
    principal
    {authorized: bool, organization: (string-ascii 50)}
)

(define-map emergency-access-logs
    {patient: principal}
    {logs: (list 20 {responder: principal, reason: (string-ascii 100), timestamp: uint})}
)

;; New functions
(define-public (register-emergency-responder (responder-principal principal) (organization (string-ascii 50)))
    (if (is-eq tx-sender contract-owner)
        (ok (map-set emergency-responders
            responder-principal
            {authorized: true, organization: organization}))
        err-not-authorized)
)

(define-public (emergency-access-override (patient-principal principal) (reason (string-ascii 100)))
    (let (
        (responder-status (default-to {authorized: false, organization: ""} (map-get? emergency-responders tx-sender)))
        (current-logs (default-to {logs: (list)} (map-get? emergency-access-logs {patient: patient-principal})))
        (new-log {responder: tx-sender, reason: reason, timestamp: stacks-block-height})
    )
        (if (get authorized responder-status)
            (begin
                (map-set access-permissions 
                    {patient: patient-principal, doctor: tx-sender}
                    {granted: true, emergency-access: true})
                (map-set emergency-access-logs
                    {patient: patient-principal}
                    {logs: (unwrap! (as-max-len? (append (get logs current-logs) new-log) u20) err-not-authorized)})
                (ok true))
            err-not-authorized)
    )
)

(define-public (revoke-emergency-access (patient-principal principal))
    (if (or (is-eq tx-sender contract-owner) (is-eq tx-sender patient-principal))
        (ok (map-set access-permissions 
            {patient: patient-principal, doctor: tx-sender}
            {granted: false, emergency-access: false}))
        err-not-authorized)
)

(define-read-only (get-emergency-access-logs (patient-principal principal))
    (get logs (default-to {logs: (list)} (map-get? emergency-access-logs {patient: patient-principal})))
)

(define-read-only (is-emergency-responder (responder-principal principal))
    (get authorized (default-to {authorized: false, organization: ""} (map-get? emergency-responders responder-principal)))
)


;; Add to data maps
(define-map delegated-managers
    {patient: principal, delegate: principal}
    {active: bool, relationship: (string-ascii 30), expiry: (optional uint)}
)



;; New functions


(define-constant err-invalid-validator (err u108))
(define-constant err-insufficient-validators (err u109))
(define-constant err-validation-expired (err u110))
(define-constant err-data-already-validated (err u111))
(define-constant err-validation-threshold-not-met (err u112))

(define-map data-validators
    principal
    {
        active: bool,
        specialty: (string-ascii 40),
        validation-count: uint,
        reputation-score: uint
    }
)

(define-map data-validation-proposals
    uint
    {
        patient: principal,
        data-hash: (string-ascii 64),
        data-type: (string-ascii 30),
        submitter: principal,
        required-validations: uint,
        expiry-block: uint,
        validated: bool,
        validation-count: uint
    }
)

(define-map validation-votes
    {proposal-id: uint, validator: principal}
    {
        approved: bool,
        notes: (string-ascii 200),
        timestamp: uint
    }
)

(define-data-var validation-proposal-counter uint u0)

(define-public (register-data-validator (validator-principal principal) (specialty (string-ascii 40)))
    (if (is-eq tx-sender contract-owner)
        (ok (map-set data-validators
            validator-principal
            {
                active: true,
                specialty: specialty,
                validation-count: u0,
                reputation-score: u100
            }))
        err-not-authorized)
)

(define-public (submit-data-for-validation 
    (patient-principal principal) 
    (data-hash (string-ascii 64)) 
    (data-type (string-ascii 30))
    (required-validations uint)
    (duration uint))
    (let (
        (proposal-id (+ (var-get validation-proposal-counter) u1))
        (has-access (get granted (check-access patient-principal tx-sender)))
        (is-verified (is-verified-doctor tx-sender))
    )
        (if (and has-access is-verified)
            (begin
                (var-set validation-proposal-counter proposal-id)
                (map-set data-validation-proposals
                    proposal-id
                    {
                        patient: patient-principal,
                        data-hash: data-hash,
                        data-type: data-type,
                        submitter: tx-sender,
                        required-validations: required-validations,
                        expiry-block: (+ stacks-block-height duration),
                        validated: false,
                        validation-count: u0
                    })
                (ok proposal-id))
            err-not-authorized)
    )
)

(define-public (validate-data (proposal-id uint) (approved bool) (notes (string-ascii 200)))
    (let (
        (proposal (unwrap! (map-get? data-validation-proposals proposal-id) err-proposal-not-found))
        (validator-info (unwrap! (map-get? data-validators tx-sender) err-invalid-validator))
        (already-voted (is-some (map-get? validation-votes {proposal-id: proposal-id, validator: tx-sender})))
        (is-expired (> stacks-block-height (get expiry-block proposal)))
        (is-validated (get validated proposal))
    )
        (if (and 
                (get active validator-info)
                (not already-voted)
                (not is-expired)
                (not is-validated))
            (let (
                (current-count (get validation-count proposal))
                (new-count (+ current-count u1))
                (required-count (get required-validations proposal))
            )
                (map-set validation-votes
                    {proposal-id: proposal-id, validator: tx-sender}
                    {approved: approved, notes: notes, timestamp: stacks-block-height})
                (map-set data-validation-proposals
                    proposal-id
                    (merge proposal {validation-count: new-count}))
                (map-set data-validators
                    tx-sender
                    (merge validator-info {validation-count: (+ (get validation-count validator-info) u1)}))
                (if (and approved (>= new-count required-count))
                    (begin
                        (map-set data-validation-proposals
                            proposal-id
                            (merge proposal {validated: true, validation-count: new-count}))
                        (ok {validated: true, count: new-count}))
                    (ok {validated: false, count: new-count})))
            (if already-voted err-already-voted
                (if is-expired err-validation-expired
                    (if is-validated err-data-already-validated
                        err-invalid-validator))))
    )
)

(define-public (finalize-validation (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? data-validation-proposals proposal-id) err-proposal-not-found))
        (validation-count (get validation-count proposal))
        (required-count (get required-validations proposal))
        (is-expired (> stacks-block-height (get expiry-block proposal)))
        (is-validated (get validated proposal))
    )
        (if (and 
                (not is-expired)
                (not is-validated)
                (>= validation-count required-count)
                (is-verified-doctor tx-sender))
            (begin
                (map-set data-validation-proposals
                    proposal-id
                    (merge proposal {validated: true}))
                (ok true))
            (if is-expired err-validation-expired
                (if is-validated err-data-already-validated
                    (if (< validation-count required-count) err-validation-threshold-not-met
                        err-not-authorized))))
    )
)

(define-public (deactivate-validator (validator-principal principal))
    (if (is-eq tx-sender contract-owner)
        (let (
            (validator-info (unwrap! (map-get? data-validators validator-principal) err-invalid-validator))
        )
            (ok (map-set data-validators
                validator-principal
                (merge validator-info {active: false}))))
        err-not-authorized)
)

(define-read-only (get-validation-proposal (proposal-id uint))
    (map-get? data-validation-proposals proposal-id)
)

(define-read-only (get-validator-info (validator-principal principal))
    (map-get? data-validators validator-principal)
)

(define-read-only (get-validation-vote (proposal-id uint) (validator-principal principal))
    (map-get? validation-votes {proposal-id: proposal-id, validator: validator-principal})
)

(define-read-only (is-data-validated (proposal-id uint))
    (let (
        (proposal (map-get? data-validation-proposals proposal-id))
    )
        (match proposal
            proposal-data (get validated proposal-data)
            false)
    )
)

(define-read-only (get-current-validation-id)
    (var-get validation-proposal-counter)
)

(define-read-only (is-active-validator (validator-principal principal))
    (let (
        (validator-info (map-get? data-validators validator-principal))
    )
        (match validator-info
            info (get active info)
            false)
    )
)

(define-read-only (get-validator-reputation (validator-principal principal))
    (let (
        (validator-info (map-get? data-validators validator-principal))
    )
        (match validator-info
            info (get reputation-score info)
            u0)
    )
)


(define-public (add-delegate (delegate-principal principal) (relationship (string-ascii 30)) (expiry (optional uint)))
    (let (
        (patient-exists (is-some (map-get? patients tx-sender)))
    )
        (if patient-exists
            (ok (map-set delegated-managers
                {patient: tx-sender, delegate: delegate-principal}
                {active: true, relationship: relationship, expiry: expiry}))
            err-not-registered)
    )
)

(define-public (remove-delegate (delegate-principal principal))
    (ok (map-delete delegated-managers {patient: tx-sender, delegate: delegate-principal}))
)

(define-public (delegate-grant-access (patient-principal principal) (doctor-principal principal))
    (let (
        (delegation-info (default-to {active: false, relationship: "", expiry: none} 
                         (map-get? delegated-managers {patient: patient-principal, delegate: tx-sender})))
        (doctor-exists (is-some (map-get? doctors doctor-principal)))
        (current-block stacks-block-height)
    )
        (if (and 
                (get active delegation-info)
                doctor-exists
                (match (get expiry delegation-info)
                    expiry-value (< current-block expiry-value)
                    true
                )
            )
            (ok (map-set access-permissions 
                {patient: patient-principal, doctor: doctor-principal}
                {granted: true, emergency-access: false}))
            err-not-authorized)
    )
)

(define-public (delegate-revoke-access (patient-principal principal) (doctor-principal principal))
    (let (
        (delegation-info (default-to {active: false, relationship: "", expiry: none} 
                         (map-get? delegated-managers {patient: patient-principal, delegate: tx-sender})))
    )
        (if (get active delegation-info)
            (ok (map-delete access-permissions {patient: patient-principal, doctor: doctor-principal}))
            err-not-authorized)
    )
)

;; (define-read-only (get-patient-delegates (patient-principal principal))
;;     (map-keys delegated-managers {patient: patient-principal, delegate: principal})
;; )

(define-read-only (is-active-delegate (patient-principal principal) (delegate-principal principal))
    (let (
        (delegation-info (default-to {active: false, relationship: "", expiry: none} 
                         (map-get? delegated-managers {patient: patient-principal, delegate: delegate-principal})))
        (current-block stacks-block-height)
    )
        (and 
            (get active delegation-info)
            (match (get expiry delegation-info)
                expiry-value (< current-block expiry-value)
                true
            )
        )
    )
)

(define-constant err-proposal-not-found (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-threshold-not-met (err u105))
(define-constant err-proposal-expired (err u106))
(define-constant err-proposal-executed (err u107))

(define-map medical-proposals
    uint
    {
        patient: principal,
        proposer: principal,
        procedure: (string-ascii 100),
        risk-level: uint,
        required-approvals: uint,
        expiry-block: uint,
        executed: bool,
        approved: bool
    }
)

(define-map proposal-approvals
    {proposal-id: uint, approver: principal}
    {approved: bool, timestamp: uint}
)

(define-map authorized-approvers
    {patient: principal, approver: principal}
    {authorized: bool, role: (string-ascii 20)}
)

(define-data-var proposal-counter uint u0)

(define-public (authorize-approver (patient-principal principal) (approver-principal principal) (role (string-ascii 20)))
    (if (or (is-eq tx-sender patient-principal) 
            (is-active-delegate patient-principal tx-sender))
        (ok (map-set authorized-approvers
            {patient: patient-principal, approver: approver-principal}
            {authorized: true, role: role}))
        err-not-authorized)
)

(define-public (create-medical-proposal 
    (patient-principal principal) 
    (procedure (string-ascii 100)) 
    (risk-level uint) 
    (required-approvals uint) 
    (duration uint))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (doctor-verified (is-verified-doctor tx-sender))
        (has-access (get granted (check-access patient-principal tx-sender)))
    )
        (if (and doctor-verified has-access)
            (begin
                (var-set proposal-counter proposal-id)
                (map-set medical-proposals
                    proposal-id
                    {
                        patient: patient-principal,
                        proposer: tx-sender,
                        procedure: procedure,
                        risk-level: risk-level,
                        required-approvals: required-approvals,
                        expiry-block: (+ stacks-block-height duration),
                        executed: false,
                        approved: false
                    })
                (ok proposal-id))
            err-not-authorized)
    )
)

(define-public (approve-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? medical-proposals proposal-id) err-proposal-not-found))
        (patient-principal (get patient proposal))
        (already-voted (is-some (map-get? proposal-approvals {proposal-id: proposal-id, approver: tx-sender})))
        (is-authorized (get authorized (default-to {authorized: false, role: ""} 
                       (map-get? authorized-approvers {patient: patient-principal, approver: tx-sender}))))
        (is-expired (> stacks-block-height (get expiry-block proposal)))
        (is-executed (get executed proposal))
    )
        (if (and 
                (not already-voted)
                (not is-expired)
                (not is-executed)
                (or is-authorized 
                    (is-verified-doctor tx-sender)
                    (is-eq tx-sender patient-principal)))
            (begin
                (map-set proposal-approvals
                    {proposal-id: proposal-id, approver: tx-sender}
                    {approved: true, timestamp: stacks-block-height})
                (ok true))
            (if already-voted err-already-voted
                (if is-expired err-proposal-expired
                    (if is-executed err-proposal-executed
                        err-not-authorized))))
    )
)

(define-public (reject-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? medical-proposals proposal-id) err-proposal-not-found))
        (patient-principal (get patient proposal))
        (already-voted (is-some (map-get? proposal-approvals {proposal-id: proposal-id, approver: tx-sender})))
        (is-authorized (get authorized (default-to {authorized: false, role: ""} 
                       (map-get? authorized-approvers {patient: patient-principal, approver: tx-sender}))))
    )
        (if (and 
                (not already-voted)
                (or is-authorized 
                    (is-verified-doctor tx-sender)
                    (is-eq tx-sender patient-principal)))
            (begin
                (map-set proposal-approvals
                    {proposal-id: proposal-id, approver: tx-sender}
                    {approved: false, timestamp: stacks-block-height})
                (ok true))
            (if already-voted err-already-voted err-not-authorized))
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? medical-proposals proposal-id) err-proposal-not-found))
        (approval-count u20)
        (required-approvals (get required-approvals proposal))
        (is-expired (> stacks-block-height (get expiry-block proposal)))
        (is-executed (get executed proposal))
    )
        (if (and 
                (not is-expired)
                (not is-executed)
                (>= approval-count required-approvals)
                (is-verified-doctor tx-sender))
            (begin
                (map-set medical-proposals
                    proposal-id
                    (merge proposal {executed: true, approved: true}))
                (ok true))
            (if is-expired err-proposal-expired
                (if is-executed err-proposal-executed
                    (if (< approval-count required-approvals) err-threshold-not-met
                        err-not-authorized))))
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? medical-proposals proposal-id)
)

(define-read-only (get-approval-status (proposal-id uint) (approver-principal principal))
    (default-to
        {approved: false, timestamp: u0}
        (map-get? proposal-approvals {proposal-id: proposal-id, approver: approver-principal}))
)



(define-read-only (is-proposal-ready (proposal-id uint))
    (let (
        (proposal (default-to 
            {patient: tx-sender, proposer: tx-sender, procedure: "", risk-level: u0, 
             required-approvals: u0, expiry-block: u0, executed: false, approved: false}
            (map-get? medical-proposals proposal-id)))
        (approval-count u20)
        (required-approvals (get required-approvals proposal))
        (is-expired (> stacks-block-height (get expiry-block proposal)))
    )
        (and 
            (not is-expired)
            (not (get executed proposal))
            (>= approval-count required-approvals))
    )
)

(define-read-only (get-current-proposal-id)
    (var-get proposal-counter)
)

(define-read-only (is-authorized-approver (patient-principal principal) (approver-principal principal))
    (get authorized (default-to {authorized: false, role: ""} 
        (map-get? authorized-approvers {patient: patient-principal, approver: approver-principal})))
)

;; Medical Record Versioning and Audit Trail System
;; Constants for versioning system
(define-constant err-record-not-found (err u113))
(define-constant err-version-not-found (err u114))
(define-constant err-invalid-record-type (err u115))
(define-constant err-unauthorized-modification (err u116))
(define-constant err-rollback-not-allowed (err u117))

;; Data structures for medical record versioning
(define-map medical-records
    {patient: principal, record-id: uint}
    {
        record-type: (string-ascii 50),
        current-version: uint,
        created-by: principal,
        created-at: uint,
        is-active: bool,
        encryption-key: (string-ascii 64)
    }
)

(define-map record-versions
    {patient: principal, record-id: uint, version: uint}
    {
        content-hash: (string-ascii 64),
        modified-by: principal,
        modified-at: uint,
        change-reason: (string-ascii 150),
        previous-hash: (optional (string-ascii 64)),
        signature: (string-ascii 128)
    }
)

(define-map audit-trail
    {patient: principal, record-id: uint}
    {
        modifications: (list 100 {
            version: uint,
            action: (string-ascii 20),
            modifier: principal,
            timestamp: uint,
            change-summary: (string-ascii 150)
        })
    }
)

(define-map record-permissions
    {patient: principal, record-id: uint, accessor: principal}
    {
        can-read: bool,
        can-modify: bool,
        can-delete: bool,
        granted-by: principal,
        granted-at: uint
    }
)

(define-data-var record-counter uint u0)

;; Create new medical record with initial version
(define-public (create-medical-record 
    (patient-principal principal)
    (record-type (string-ascii 50))
    (content-hash (string-ascii 64))
    (encryption-key (string-ascii 64))
    (signature (string-ascii 128)))
    (let (
        (record-id (+ (var-get record-counter) u1))
        (has-access (get granted (check-access patient-principal tx-sender)))
        (is-doctor (is-verified-doctor tx-sender))
        (is-patient (is-eq tx-sender patient-principal))
    )
        (if (and (or has-access is-patient) is-doctor)
            (begin
                ;; Update record counter
                (var-set record-counter record-id)
                
                ;; Create main record entry
                (map-set medical-records
                    {patient: patient-principal, record-id: record-id}
                    {
                        record-type: record-type,
                        current-version: u1,
                        created-by: tx-sender,
                        created-at: stacks-block-height,
                        is-active: true,
                        encryption-key: encryption-key
                    })
                
                ;; Create initial version
                (map-set record-versions
                    {patient: patient-principal, record-id: record-id, version: u1}
                    {
                        content-hash: content-hash,
                        modified-by: tx-sender,
                        modified-at: stacks-block-height,
                        change-reason: "Initial record creation",
                        previous-hash: none,
                        signature: signature
                    })
                
                ;; Initialize audit trail
                (map-set audit-trail
                    {patient: patient-principal, record-id: record-id}
                    {
                        modifications: (list {
                            version: u1,
                            action: "CREATE",
                            modifier: tx-sender,
                            timestamp: stacks-block-height,
                            change-summary: "Medical record created"
                        })
                    })
                
                (ok record-id))
            err-not-authorized)
    )
)

;; Update existing medical record with new version
(define-public (update-medical-record
    (patient-principal principal)
    (record-id uint)
    (new-content-hash (string-ascii 64))
    (change-reason (string-ascii 150))
    (signature (string-ascii 128)))
    (let (
        (record (unwrap! (map-get? medical-records {patient: patient-principal, record-id: record-id}) err-record-not-found))
        (current-version (get current-version record))
        (new-version (+ current-version u1))
        (current-version-data (map-get? record-versions {patient: patient-principal, record-id: record-id, version: current-version}))
        (has-permission (get can-modify (default-to {can-read: false, can-modify: false, can-delete: false, granted-by: tx-sender, granted-at: u0}
                        (map-get? record-permissions {patient: patient-principal, record-id: record-id, accessor: tx-sender}))))
        (is-owner (is-eq tx-sender patient-principal))
        (current-trail (default-to {modifications: (list)} (map-get? audit-trail {patient: patient-principal, record-id: record-id})))
    )
        (if (and (get is-active record) (or has-permission is-owner))
            (begin
                ;; Update main record with new version number
                (map-set medical-records
                    {patient: patient-principal, record-id: record-id}
                    (merge record {current-version: new-version}))
                
                ;; Create new version entry
                (map-set record-versions
                    {patient: patient-principal, record-id: record-id, version: new-version}
                    {
                        content-hash: new-content-hash,
                        modified-by: tx-sender,
                        modified-at: stacks-block-height,
                        change-reason: change-reason,
                        previous-hash: (match current-version-data
                            version-data (some (get content-hash version-data))
                            none),
                        signature: signature
                    })
                
                ;; Update audit trail
                (map-set audit-trail
                    {patient: patient-principal, record-id: record-id}
                    {
                        modifications: (unwrap! (as-max-len? 
                            (append (get modifications current-trail) {
                                version: new-version,
                                action: "UPDATE",
                                modifier: tx-sender,
                                timestamp: stacks-block-height,
                                change-summary: change-reason
                            }) u100) err-not-authorized)
                    })
                
                (ok new-version))
            err-unauthorized-modification)
    )
)

;; Grant record access permissions
(define-public (grant-record-permission
    (record-id uint)
    (accessor-principal principal)
    (can-read bool)
    (can-modify bool)
    (can-delete bool))
    (let (
        (record (unwrap! (map-get? medical-records {patient: tx-sender, record-id: record-id}) err-record-not-found))
    )
        (if (get is-active record)
            (ok (map-set record-permissions
                {patient: tx-sender, record-id: record-id, accessor: accessor-principal}
                {
                    can-read: can-read,
                    can-modify: can-modify,
                    can-delete: can-delete,
                    granted-by: tx-sender,
                    granted-at: stacks-block-height
                }))
            err-record-not-found)
    )
)

;; Soft delete medical record
(define-public (deactivate-medical-record (record-id uint))
    (let (
        (record (unwrap! (map-get? medical-records {patient: tx-sender, record-id: record-id}) err-record-not-found))
        (current-trail (default-to {modifications: (list)} (map-get? audit-trail {patient: tx-sender, record-id: record-id})))
    )
        (if (get is-active record)
            (begin
                ;; Deactivate record
                (map-set medical-records
                    {patient: tx-sender, record-id: record-id}
                    (merge record {is-active: false}))
                
                ;; Log deactivation in audit trail
                (map-set audit-trail
                    {patient: tx-sender, record-id: record-id}
                    {
                        modifications: (unwrap! (as-max-len? 
                            (append (get modifications current-trail) {
                                version: (get current-version record),
                                action: "DEACTIVATE",
                                modifier: tx-sender,
                                timestamp: stacks-block-height,
                                change-summary: "Record deactivated by patient"
                            }) u100) err-not-authorized)
                    })
                
                (ok true))
            err-record-not-found)
    )
)

;; Read-only functions for record retrieval
(define-read-only (get-medical-record (patient-principal principal) (record-id uint))
    (map-get? medical-records {patient: patient-principal, record-id: record-id})
)

(define-read-only (get-record-version (patient-principal principal) (record-id uint) (version uint))
    (map-get? record-versions {patient: patient-principal, record-id: record-id, version: version})
)

(define-read-only (get-current-record-version (patient-principal principal) (record-id uint))
    (let (
        (record (map-get? medical-records {patient: patient-principal, record-id: record-id}))
    )
        (match record
            record-data (map-get? record-versions {patient: patient-principal, record-id: record-id, version: (get current-version record-data)})
            none)
    )
)

(define-read-only (get-record-audit-trail (patient-principal principal) (record-id uint))
    (get modifications (default-to {modifications: (list)} (map-get? audit-trail {patient: patient-principal, record-id: record-id})))
)

(define-read-only (get-record-permissions (patient-principal principal) (record-id uint) (accessor-principal principal))
    (map-get? record-permissions {patient: patient-principal, record-id: record-id, accessor: accessor-principal})
)

(define-read-only (get-total-records)
    (var-get record-counter)
)

(define-read-only (can-access-record (patient-principal principal) (record-id uint) (accessor-principal principal))
    (let (
        (permissions (map-get? record-permissions {patient: patient-principal, record-id: record-id, accessor: accessor-principal}))
        (is-patient (is-eq accessor-principal patient-principal))
        (has-general-access (get granted (check-access patient-principal accessor-principal)))
    )
        (or is-patient 
            has-general-access
            (match permissions
                perm-data (get can-read perm-data)
                false))
    )
)

(define-read-only (verify-record-integrity (patient-principal principal) (record-id uint) (version uint))
    (let (
        (version-data (map-get? record-versions {patient: patient-principal, record-id: record-id, version: version}))
    )
        (match version-data
            data (and 
                (> (len (get content-hash data)) u0)
                (> (len (get signature data)) u0)
                (> (get modified-at data) u0))
            false)
    )
)



