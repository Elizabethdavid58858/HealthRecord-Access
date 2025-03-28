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
