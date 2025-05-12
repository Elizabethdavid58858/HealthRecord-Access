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