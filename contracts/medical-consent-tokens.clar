;; Medical Consent Tokens
;; Allows patients to create time-bound, purpose-specific consent tokens for medical data sharing

;; Error constants
(define-constant err-not-authorized (err u200))
(define-constant err-invalid-token (err u201))
(define-constant err-token-expired (err u202))
(define-constant err-token-not-found (err u203))
(define-constant err-patient-not-registered (err u204))
(define-constant err-insufficient-scope (err u205))

;; Data scope constants
(define-constant SCOPE-BASIC u1)        ;; Basic medical info
(define-constant SCOPE-DIAGNOSTICS u2)  ;; Diagnostic results
(define-constant SCOPE-TREATMENT u4)    ;; Treatment history
(define-constant SCOPE-FULL u7)         ;; All data (1+2+4)

;; Purpose constants  
(define-constant PURPOSE-RESEARCH "RESEARCH")
(define-constant PURPOSE-SECOND-OPINION "SECOND_OPINION")
(define-constant PURPOSE-INSURANCE "INSURANCE")
(define-constant PURPOSE-EMERGENCY "EMERGENCY")

;; Data structures
(define-map consent-tokens
    uint
    {
        patient: principal,
        requester: principal,
        purpose: (string-ascii 20),
        data-scope: uint,
        expiry-block: uint,
        usage-count: uint,
        max-usage: uint,
        is-active: bool,
        created-at: uint,
        token-hash: (string-ascii 64)
    }
)

(define-map token-usage-log
    {token-id: uint, usage-index: uint}
    {
        accessed-by: principal,
        access-timestamp: uint,
        data-accessed: (string-ascii 100)
    }
)

(define-map patient-consent-settings
    principal
    {
        auto-approve-research: bool,
        max-token-duration: uint,
        default-scope: uint,
        require-justification: bool
    }
)

(define-data-var token-counter uint u0)

;; Create a new consent token
(define-public (create-consent-token
    (requester-principal principal)
    (purpose (string-ascii 20))
    (data-scope uint)
    (duration-blocks uint)
    (max-usage uint)
    (justification (string-ascii 200)))
    (let (
        (token-id (+ (var-get token-counter) u1))
        (expiry-block (+ stacks-block-height duration-blocks))
        (patient-settings (map-get? patient-consent-settings tx-sender))
        (is-valid-scope (<= data-scope SCOPE-FULL))
        (is-valid-duration (match patient-settings
            settings (<= duration-blocks (get max-token-duration settings))
            true))
        (token-hash (unwrap-panic (to-consensus-buff? (+ token-id stacks-block-height))))
    )
    (if (and is-valid-scope is-valid-duration (> max-usage u0))
        (begin
            (var-set token-counter token-id)
            (map-set consent-tokens
                token-id
                {
                    patient: tx-sender,
                    requester: requester-principal,
                    purpose: purpose,
                    data-scope: data-scope,
                    expiry-block: expiry-block,
                    usage-count: u0,
                    max-usage: max-usage,
                    is-active: true,
                    created-at: stacks-block-height,
                    token-hash: (unwrap-panic (as-max-len? (int-to-ascii (+ token-id stacks-block-height)) u64))
                })
            (ok token-id))
        err-not-authorized))
)

;; Use a consent token to access data
(define-public (use-consent-token 
    (token-id uint) 
    (data-description (string-ascii 100)))
    (let (
        (token (unwrap! (map-get? consent-tokens token-id) err-token-not-found))
        (current-usage (get usage-count token))
        (is-expired (>= stacks-block-height (get expiry-block token)))
        (is-requester (is-eq tx-sender (get requester token)))
        (usage-available (< current-usage (get max-usage token)))
    )
    (if (and (get is-active token) (not is-expired) is-requester usage-available)
        (begin
            ;; Log the usage
            (map-set token-usage-log
                {token-id: token-id, usage-index: current-usage}
                {
                    accessed-by: tx-sender,
                    access-timestamp: stacks-block-height,
                    data-accessed: data-description
                })
            ;; Update usage count
            (map-set consent-tokens
                token-id
                (merge token {usage-count: (+ current-usage u1)}))
            (ok true))
        (if is-expired err-token-expired
            (if (not usage-available) err-insufficient-scope
                err-not-authorized))))
)

;; Revoke a consent token
(define-public (revoke-consent-token (token-id uint))
    (let (
        (token (unwrap! (map-get? consent-tokens token-id) err-token-not-found))
        (is-patient (is-eq tx-sender (get patient token)))
    )
    (if is-patient
        (ok (map-set consent-tokens
            token-id
            (merge token {is-active: false})))
        err-not-authorized))
)

;; Set patient consent preferences
(define-public (set-consent-preferences
    (auto-approve-research bool)
    (max-token-duration uint)
    (default-scope uint)
    (require-justification bool))
    (if (<= default-scope SCOPE-FULL)
        (ok (map-set patient-consent-settings
            tx-sender
            {
                auto-approve-research: auto-approve-research,
                max-token-duration: max-token-duration,
                default-scope: default-scope,
                require-justification: require-justification
            }))
        err-not-authorized)
)

;; Read-only functions
(define-read-only (get-consent-token (token-id uint))
    (map-get? consent-tokens token-id)
)

(define-read-only (is-token-valid (token-id uint))
    (let (
        (token (map-get? consent-tokens token-id))
    )
    (match token
        token-data (and 
            (get is-active token-data)
            (< stacks-block-height (get expiry-block token-data))
            (< (get usage-count token-data) (get max-usage token-data)))
        false))
)

(define-read-only (check-token-permissions (token-id uint) (required-scope uint))
    (let (
        (token (map-get? consent-tokens token-id))
    )
    (match token
        token-data (>= (get data-scope token-data) required-scope)
        false))
)

(define-read-only (get-token-usage-history (token-id uint))
    (list 
        (map-get? token-usage-log {token-id: token-id, usage-index: u0})
        (map-get? token-usage-log {token-id: token-id, usage-index: u1})
        (map-get? token-usage-log {token-id: token-id, usage-index: u2})
        (map-get? token-usage-log {token-id: token-id, usage-index: u3})
        (map-get? token-usage-log {token-id: token-id, usage-index: u4}))
)

(define-read-only (get-patient-settings (patient-principal principal))
    (default-to
        {auto-approve-research: false, max-token-duration: u1000, default-scope: SCOPE-BASIC, require-justification: true}
        (map-get? patient-consent-settings patient-principal))
)

(define-read-only (get-current-token-id)
    (var-get token-counter)
)

(define-read-only (verify-token-authenticity (token-id uint) (provided-hash (string-ascii 64)))
    (let (
        (token (map-get? consent-tokens token-id))
    )
    (match token
        token-data (is-eq (get token-hash token-data) provided-hash)
        false))
)

;; Helper function to check if requester has permissions for token scope
(define-read-only (has-scope-permission (token-id uint) (required-permission uint))
    (let (
        (token (map-get? consent-tokens token-id))
    )
    (match token
        token-data (>= (bit-and (get data-scope token-data) required-permission) required-permission)
        false))
)
