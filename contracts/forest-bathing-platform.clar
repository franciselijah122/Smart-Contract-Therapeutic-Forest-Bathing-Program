;; ===================================================================
;; SMART CONTRACT THERAPEUTIC FOREST BATHING PROGRAM
;; ===================================================================
;; A comprehensive system for coordinating shinrin-yoku (forest bathing)
;; sessions with forest access management, guide certification, and
;; wellness outcome tracking.
;;
;; Contract 1: forest-therapy-core.clar
;; Contract 2: wellness-outcomes.clar
;; ===================================================================

;; ===================================================================
;; CONTRACT 1: forest-therapy-core.clar
;; ===================================================================
;; Core functionality for forest access, guide management, and session coordination

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-session-full (err u104))
(define-constant err-session-cancelled (err u105))
(define-constant err-invalid-time (err u106))
(define-constant err-guide-not-certified (err u107))
(define-constant err-forest-unavailable (err u108))
(define-constant err-accessibility-conflict (err u109))

;; Data Variables
(define-data-var next-session-id uint u1)
(define-data-var next-guide-id uint u1)
(define-data-var next-forest-id uint u1)

;; Data Maps

;; Forest locations with ecological and accessibility information
(define-map forests
  { forest-id: uint }
  {
    name: (string-ascii 64),
    location: (string-ascii 128),
    ecosystem-type: (string-ascii 32),
    max-capacity: uint,
    accessibility-features: (list 10 (string-ascii 32)),
    ecological-highlights: (list 5 (string-ascii 64)),
    seasonal-availability: (list 4 bool), ;; spring, summer, fall, winter
    created-at: uint,
    is-active: bool
  }
)

;; Certified guides with specializations
(define-map guides
  { guide-id: uint }
  {
    principal: principal,
    name: (string-ascii 64),
    certification-level: (string-ascii 16), ;; "basic", "advanced", "master"
    specializations: (list 5 (string-ascii 32)),
    languages: (list 3 (string-ascii 16)),
    accessibility-training: bool,
    certification-expiry: uint,
    total-sessions: uint,
    rating: uint, ;; 0-100 scale
    is-active: bool
  }
)

;; Shinrin-yoku sessions
(define-map sessions
  { session-id: uint }
  {
    forest-id: uint,
    guide-id: uint,
    session-name: (string-ascii 64),
    description: (string-ascii 256),
    start-time: uint,
    duration-minutes: uint,
    max-participants: uint,
    current-participants: uint,
    accessibility-accommodations: (list 5 (string-ascii 32)),
    therapy-focus: (string-ascii 32), ;; "stress-relief", "mindfulness", "healing", "education"
    price-ustx: uint,
    status: (string-ascii 16), ;; "scheduled", "in-progress", "completed", "cancelled"
    created-at: uint
  }
)

;; Session participants with accessibility needs
(define-map session-participants
  { session-id: uint, participant: principal }
  {
    registered-at: uint,
    accessibility-needs: (list 3 (string-ascii 32)),
    emergency-contact: (string-ascii 64),
    medical-notes: (string-ascii 128),
    participation-status: (string-ascii 16) ;; "registered", "attended", "no-show", "cancelled"
  }
)

;; Guide-to-principal mapping
(define-map guide-principals
  { principal: principal }
  { guide-id: uint }
)

;; Forest access permissions
(define-map forest-access
  { forest-id: uint, date: uint }
  {
    available-slots: uint,
    reserved-slots: uint,
    weather-conditions: (string-ascii 32),
    ecological-status: (string-ascii 32) ;; "optimal", "good", "limited", "closed"
  }
)

;; Read-only functions

(define-read-only (get-forest (forest-id uint))
  (map-get? forests { forest-id: forest-id })
)

(define-read-only (get-guide (guide-id uint))
  (map-get? guides { guide-id: guide-id })
)

(define-read-only (get-session (session-id uint))
  (map-get? sessions { session-id: session-id })
)

(define-read-only (get-session-participant (session-id uint) (participant principal))
  (map-get? session-participants { session-id: session-id, participant: participant })
)

(define-read-only (get-guide-by-principal (guide-principal principal))
  (match (map-get? guide-principals { principal: guide-principal })
    guide-data (map-get? guides { guide-id: (get guide-id guide-data) })
    none
  )
)

(define-read-only (get-forest-availability (forest-id uint) (date uint))
  (map-get? forest-access { forest-id: forest-id, date: date })
)

(define-read-only (is-guide-certified (guide-id uint))
  (match (map-get? guides { guide-id: guide-id })
    guide-data (and
      (get is-active guide-data)
      (> (get certification-expiry guide-data) stacks-block-height)
    )
    false
  )
)

(define-read-only (can-guide-handle-accessibility (guide-id uint) (needs (list 3 (string-ascii 32))))
  (match (map-get? guides { guide-id: guide-id })
    guide-data (get accessibility-training guide-data)
    false
  )
)

;; Administrative functions

(define-public (register-forest
  (name (string-ascii 64))
  (location (string-ascii 128))
  (ecosystem-type (string-ascii 32))
  (max-capacity uint)
  (accessibility-features (list 10 (string-ascii 32)))
  (ecological-highlights (list 5 (string-ascii 64)))
  (seasonal-availability (list 4 bool))
)
  (let ((forest-id (var-get next-forest-id)))
    (asserts! (is-eq tx-sender wellness-contract-owner) err-wellness-owner-only)
    (asserts! (> max-capacity u0) err-invalid-input)

    (map-set forests
      { forest-id: forest-id }
      {
        name: name,
        location: location,
        ecosystem-type: ecosystem-type,
        max-capacity: max-capacity,
        accessibility-features: accessibility-features,
        ecological-highlights: ecological-highlights,
        seasonal-availability: seasonal-availability,
        created-at: stacks-block-height,
        is-active: true
      }
    )

    (var-set next-forest-id (+ forest-id u1))
    (ok forest-id)
  )
)

(define-public (certify-guide
  (guide-principal principal)
  (name (string-ascii 64))
  (certification-level (string-ascii 16))
  (specializations (list 5 (string-ascii 32)))
  (languages (list 3 (string-ascii 16)))
  (accessibility-training bool)
  (certification-duration-blocks uint)
)
  (let ((guide-id (var-get next-guide-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> certification-duration-blocks u0) err-invalid-input)

    (map-set guides
      { guide-id: guide-id }
      {
        principal: guide-principal,
        name: name,
        certification-level: certification-level,
        specializations: specializations,
        languages: languages,
        accessibility-training: accessibility-training,
        certification-expiry: (+ stacks-block-height certification-duration-blocks),
        total-sessions: u0,
        rating: u80, ;; Start with good rating
        is-active: true
      }
    )

    (map-set guide-principals
      { principal: guide-principal }
      { guide-id: guide-id }
    )

    (var-set next-guide-id (+ guide-id u1))
    (ok guide-id)
  )
)

;; Session management functions

(define-public (create-session
  (forest-id uint)
  (session-name (string-ascii 64))
  (description (string-ascii 256))
  (start-time uint)
  (duration-minutes uint)
  (max-participants uint)
  (accessibility-accommodations (list 5 (string-ascii 32)))
  (therapy-focus (string-ascii 32))
  (price-ustx uint)
)
  (let (
    (session-id (var-get next-session-id))
    (guide-data (unwrap! (get-guide-by-principal tx-sender) err-unauthorized))
    (guide-id (unwrap! (map-get? guide-principals { principal: tx-sender }) err-unauthorized))
    (forest-data (unwrap! (get-forest forest-id) err-not-found))
  )
    ;; Verify guide is certified and active
    (asserts! (is-guide-certified (get guide-id guide-id)) err-guide-not-certified)
    (asserts! (get is-active forest-data) err-forest-unavailable)
    (asserts! (> start-time stacks-block-height) err-invalid-time)
    (asserts! (and (> duration-minutes u0) (<= duration-minutes u480)) err-invalid-input) ;; Max 8 hours
    (asserts! (and (> max-participants u0) (<= max-participants (get max-capacity forest-data))) err-invalid-input)

    (map-set sessions
      { session-id: session-id }
      {
        forest-id: forest-id,
        guide-id: (get guide-id guide-id),
        session-name: session-name,
        description: description,
        start-time: start-time,
        duration-minutes: duration-minutes,
        max-participants: max-participants,
        current-participants: u0,
        accessibility-accommodations: accessibility-accommodations,
        therapy-focus: therapy-focus,
        price-ustx: price-ustx,
        status: "scheduled",
        created-at: stacks-block-height
      }
    )

    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (register-for-session
  (session-id uint)
  (accessibility-needs (list 3 (string-ascii 32)))
  (emergency-contact (string-ascii 64))
  (medical-notes (string-ascii 128))
)
  (let (
    (session-data (unwrap! (get-session session-id) err-not-found))
    (guide-data (unwrap! (get-guide (get guide-id session-data)) err-not-found))
  )
    ;; Check session availability and status
    (asserts! (is-eq (get status session-data) "scheduled") err-session-cancelled)
    (asserts! (< (get current-participants session-data) (get max-participants session-data)) err-session-full)
    (asserts! (> (get start-time session-data) stacks-block-height) err-invalid-time)

    ;; Check if guide can handle accessibility needs
    (asserts! (or
      (is-eq (len accessibility-needs) u0)
      (can-guide-handle-accessibility (get guide-id session-data) accessibility-needs)
    ) err-accessibility-conflict)

    ;; Transfer payment
    (try! (stx-transfer? (get price-ustx session-data) tx-sender (get principal guide-data)))

    ;; Register participant
    (map-set session-participants
      { session-id: session-id, participant: tx-sender }
      {
        registered-at: stacks-block-height,
        accessibility-needs: accessibility-needs,
        emergency-contact: emergency-contact,
        medical-notes: medical-notes,
        participation-status: "registered"
      }
    )

    ;; Update session participant count
    (map-set sessions
      { session-id: session-id }
      (merge session-data { current-participants: (+ (get current-participants session-data) u1) })
    )

    (ok true)
  )
)

(define-public (update-session-status
  (session-id uint)
  (new-status (string-ascii 16))
)
  (let (
    (session-data (unwrap! (get-session session-id) err-not-found))
    (guide-id-data (unwrap! (map-get? guide-principals { principal: tx-sender }) err-unauthorized))
  )
    ;; Only the assigned guide can update session status
    (asserts! (is-eq (get guide-id session-data) (get guide-id guide-id-data)) err-unauthorized)

    (map-set sessions
      { session-id: session-id }
      (merge session-data { status: new-status })
    )

    ;; Update guide's session count if completed
    (if (is-eq new-status "completed")
      (let ((guide-data (unwrap! (get-guide (get guide-id session-data)) err-not-found)))
        (map-set guides
          { guide-id: (get guide-id session-data) }
          (merge guide-data { total-sessions: (+ (get total-sessions guide-data) u1) })
        )
      )
      true
    )

    (ok true)
  )
)

;; Forest access management

(define-public (set-forest-access
  (forest-id uint)
  (date uint)
  (available-slots uint)
  (weather-conditions (string-ascii 32))
  (ecological-status (string-ascii 32))
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (get-forest forest-id)) err-not-found)

    (map-set forest-access
      { forest-id: forest-id, date: date }
      {
        available-slots: available-slots,
        reserved-slots: u0,
        weather-conditions: weather-conditions,
        ecological-status: ecological-status
      }
    )

    (ok true)
  )
)

;; Emergency and safety functions

(define-public (cancel-session
  (session-id uint)
  (refund-participants bool)
)
  (let (
    (session-data (unwrap! (get-session session-id) err-not-found))
    (guide-id-data (unwrap! (map-get? guide-principals { principal: tx-sender }) err-unauthorized))
  )
    ;; Only guide or contract owner can cancel
    (asserts! (or
      (is-eq tx-sender contract-owner)
      (is-eq (get guide-id session-data) (get guide-id guide-id-data))
    ) err-unauthorized)

    (map-set sessions
      { session-id: session-id }
      (merge session-data { status: "cancelled" })
    )

    ;; Note: Refund logic would be implemented here for production
    ;; This would require tracking all participants and their payments

    (ok true)
  )
)

;; ===================================================================
;; CONTRACT 2: wellness-outcomes.clar
;; ===================================================================
;; Wellness outcome tracking and community nature connection analytics

;; Import dependencies (in production, this would reference the first contract)
;; For this example, we'll include necessary constants and basic structures

;; Constants for wellness outcomes contract
(define-constant wellness-contract-owner tx-sender)
(define-constant err-wellness-owner-only (err u200))
(define-constant err-wellness-not-found (err u201))
(define-constant err-wellness-unauthorized (err u202))
(define-constant err-wellness-invalid-input (err u203))
(define-constant err-session-not-completed (err u204))
(define-constant err-already-submitted (err u205))

;; Data Variables
(define-data-var next-outcome-id uint u1)
(define-data-var next-community-event-id uint u1)

;; Wellness outcome tracking
(define-map wellness-outcomes
  { outcome-id: uint }
  {
    session-id: uint,
    participant: principal,
    pre-session-metrics: {
      stress-level: uint, ;; 1-10 scale
      mood-score: uint, ;; 1-10 scale
      anxiety-level: uint, ;; 1-10 scale
      connection-to-nature: uint ;; 1-10 scale
    },
    post-session-metrics: {
      stress-level: uint,
      mood-score: uint,
      anxiety-level: uint,
      connection-to-nature: uint
    },
    subjective-feedback: (string-ascii 512),
    would-recommend: bool,
    accessibility-rating: uint, ;; 1-10 scale for accommodation quality
    learning-outcomes: (list 5 (string-ascii 64)),
    follow-up-interest: (list 3 (string-ascii 32)),
    recorded-at: uint
  }
)

;; Community engagement and education
(define-map community-events
  { event-id: uint }
  {
    event-type: (string-ascii 32), ;; "workshop", "cleanup", "research", "celebration"
    title: (string-ascii 64),
    description: (string-ascii 256),
    forest-id: uint,
    organizer: principal,
    start-time: uint,
    duration-minutes: uint,
    max-participants: uint,
    current-participants: uint,
    focus-area: (string-ascii 32), ;; "ecology", "conservation", "wellness", "education"
    accessibility-features: (list 5 (string-ascii 32)),
    created-at: uint,
    status: (string-ascii 16)
  }
)

;; Participant wellness journey tracking
(define-map participant-journeys
  { participant: principal }
  {
    total-sessions: uint,
    first-session-date: uint,
    last-session-date: uint,
    preferred-therapy-focus: (string-ascii 32),
    average-stress-improvement: uint, ;; Percentage improvement
    average-mood-improvement: uint,
    favorite-forest-ids: (list 3 uint),
    accessibility-needs: (list 3 (string-ascii 32)),
    wellness-goals: (list 5 (string-ascii 64)),
    achievement-badges: (list 10 (string-ascii 32))
  }
)

;; Aggregate wellness metrics for research
(define-map forest-wellness-stats
  { forest-id: uint, month: uint, year: uint }
  {
    total-sessions: uint,
    total-participants: uint,
    average-stress-reduction: uint,
    average-mood-improvement: uint,
    average-satisfaction: uint,
    most-common-therapy-focus: (string-ascii 32),
    accessibility-accommodation-requests: uint,
    return-participant-rate: uint ;; Percentage
  }
)

;; Guide performance and wellness impact
(define-map guide-wellness-impact
  { guide-id: uint, month: uint, year: uint }
  {
    sessions-conducted: uint,
    participants-served: uint,
    average-participant-satisfaction: uint,
    average-wellness-improvement: uint,
    accessibility-accommodations-provided: uint,
    specialization-effectiveness: (list 5 { focus: (string-ascii 32), impact-score: uint }),
    community-engagement-score: uint
  }
)

;; Read-only functions

(define-read-only (get-wellness-outcome (outcome-id uint))
  (map-get? wellness-outcomes { outcome-id: outcome-id })
)

(define-read-only (get-participant-journey (participant principal))
  (map-get? participant-journeys { participant: participant })
)

(define-read-only (get-forest-wellness-stats (forest-id uint) (month uint) (year uint))
  (map-get? forest-wellness-stats { forest-id: forest-id, month: month, year: year })
)

(define-read-only (get-guide-wellness-impact (guide-id uint) (month uint) (year uint))
  (map-get? guide-wellness-impact { guide-id: guide-id, month: month, year: year })
)

(define-read-only (get-community-event (event-id uint))
  (map-get? community-events { event-id: event-id })
)

(define-read-only (calculate-wellness-improvement (pre-metrics (tuple (stress-level uint) (mood-score uint) (anxiety-level uint) (connection-to-nature uint))) (post-metrics (tuple (stress-level uint) (mood-score uint) (anxiety-level uint) (connection-to-nature uint))))
  (let (
    (stress-improvement (if (> (get stress-level pre-metrics) (get stress-level post-metrics))
      (- (get stress-level pre-metrics) (get stress-level post-metrics))
      u0))
    (mood-improvement (if (> (get mood-score post-metrics) (get mood-score pre-metrics))
      (- (get mood-score post-metrics) (get mood-score pre-metrics))
      u0))
    (anxiety-improvement (if (> (get anxiety-level pre-metrics) (get anxiety-level post-metrics))
      (- (get anxiety-level pre-metrics) (get anxiety-level post-metrics))
      u0))
    (nature-connection-improvement (if (> (get connection-to-nature post-metrics) (get connection-to-nature pre-metrics))
      (- (get connection-to-nature post-metrics) (get connection-to-nature pre-metrics))
      u0))
  )
    {
      stress-improvement: stress-improvement,
      mood-improvement: mood-improvement,
      anxiety-improvement: anxiety-improvement,
      nature-connection-improvement: nature-connection-improvement,
      overall-improvement: (/ (+ stress-improvement mood-improvement anxiety-improvement nature-connection-improvement) u4)
    }
  )
)

;; Outcome tracking functions

(define-public (submit-wellness-outcome
  (session-id uint)
  (pre-session-stress uint) (pre-session-mood uint) (pre-session-anxiety uint) (pre-session-nature-connection uint)
  (post-session-stress uint) (post-session-mood uint) (post-session-anxiety uint) (post-session-nature-connection uint)
  (subjective-feedback (string-ascii 512))
  (would-recommend bool)
  (accessibility-rating uint)
  (learning-outcomes (list 5 (string-ascii 64)))
  (follow-up-interest (list 3 (string-ascii 32)))
)
  (let ((outcome-id (var-get next-outcome-id)))
    ;; Validate input ranges
    (asserts! (and (<= pre-session-stress u10) (>= pre-session-stress u1)) err-wellness-invalid-input)
    (asserts! (and (<= pre-session-mood u10) (>= pre-session-mood u1)) err-wellness-invalid-input)
    (asserts! (and (<= post-session-stress u10) (>= post-session-stress u1)) err-wellness-invalid-input)
    (asserts! (and (<= post-session-mood u10) (>= post-session-mood u1)) err-wellness-invalid-input)
    (asserts! (and (<= accessibility-rating u10) (>= accessibility-rating u1)) err-wellness-invalid-input)

    ;; Check if outcome already exists for this participant and session
    (asserts! (is-none (map-get? wellness-outcomes { outcome-id: session-id })) err-already-submitted)

    (map-set wellness-outcomes
      { outcome-id: outcome-id }
      {
        session-id: session-id,
        participant: tx-sender,
        pre-session-metrics: {
          stress-level: pre-session-stress,
          mood-score: pre-session-mood,
          anxiety-level: pre-session-anxiety,
          connection-to-nature: pre-session-nature-connection
        },
        post-session-metrics: {
          stress-level: post-session-stress,
          mood-score: post-session-mood,
          anxiety-level: post-session-anxiety,
          connection-to-nature: post-session-nature-connection
        },
        subjective-feedback: subjective-feedback,
        would-recommend: would-recommend,
        accessibility-rating: accessibility-rating,
        learning-outcomes: learning-outcomes,
        follow-up-interest: follow-up-interest,
        recorded-at: stacks-block-height
      }
    )

    ;; Update participant journey
    (update-participant-journey session-id)

    (var-set next-outcome-id (+ outcome-id u1))
    (ok outcome-id)
  )
)

(define-private (update-participant-journey (session-id uint))
  (let (
    (current-journey (default-to
      {
        total-sessions: u0,
        first-session-date: stacks-block-height,
        last-session-date: stacks-block-height,
        preferred-therapy-focus: "mindfulness",
        average-stress-improvement: u0,
        average-mood-improvement: u0,
        favorite-forest-ids: (list),
        accessibility-needs: (list),
        wellness-goals: (list),
        achievement-badges: (list)
      }
      (map-get? participant-journeys { participant: tx-sender })
    ))
  )
    (map-set participant-journeys
      { participant: tx-sender }
      (merge current-journey {
        total-sessions: (+ (get total-sessions current-journey) u1),
        last-session-date: stacks-block-height
      })
    )
    true
  )
)

;; Community engagement functions

(define-public (create-community-event
  (event-type (string-ascii 32))
  (title (string-ascii 64))
  (description (string-ascii 256))
  (forest-id uint)
  (start-time uint)
  (duration-minutes uint)
  (max-participants uint)
  (focus-area (string-ascii 32))
  (accessibility-features (list 5 (string-ascii 32)))
)
  (let ((event-id (var-get next-community-event-id)))
    (asserts! (> start-time stacks-block-height) err-wellness-invalid-input)
    (asserts! (and (> duration-minutes u0) (<= duration-minutes u600)) err-wellness-invalid-input) ;; Max 10 hours
    (asserts! (> max-participants u0) err-wellness-invalid-input)

    (map-set community-events
      { event-id: event-id }
      {
        event-type: event-type,
        title: title,
        description: description,
        forest-id: forest-id,
        organizer: tx-sender,
        start-time: start-time,
        duration-minutes: duration-minutes,
        max-participants: max-participants,
        current-participants: u0,
        focus-area: focus-area,
        accessibility-features: accessibility-features,
        created-at: stacks-block-height,
        status: "scheduled"
      }
    )

    (var-set next-community-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (join-community-event (event-id uint))
  (let ((event-data (unwrap! (get-community-event event-id) err-wellness-not-found)))
    (asserts! (is-eq (get status event-data) "scheduled") err-session-cancelled)
    (asserts! (< (get current-participants event-data) (get max-participants event-data)) err-session-full)
    (asserts! (> (get start-time event-data) stacks-block-height) err-invalid-time)

    (map-set community-events
      { event-id: event-id }
      (merge event-data { current-participants: (+ (get current-participants event-data) u1) })
    )

    (ok true)
  )
)

;; Analytics and reporting functions

(define-public (generate-forest-wellness-report (forest-id uint) (month uint) (year uint))
  (let (
    (current-stats (default-to
      {
        total-sessions: u0,
        total-participants: u0,
        average-stress-reduction: u0,
        average-mood-improvement: u0,
        average-satisfaction: u0,
        most-common-therapy-focus: "mindfulness",
        accessibility-accommodation-requests: u0,
        return-participant-rate: u0
      }
      (map-get? forest-wellness-stats { forest-id: forest-id, month: month, year: year })
    ))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    ;; In a production system, this would aggregate data from multiple outcome records
    ;; For this example, we'll demonstrate the structure
    (map-set forest-wellness-stats
      { forest-id: forest-id, month: month, year: year }
      current-stats
    )

    (ok current-stats)
  )
)

(define-public (award-wellness-badge (participant principal) (badge-name (string-ascii 32)))
  (let (
    (journey-data (unwrap! (get-participant-journey participant) err-wellness-not-found))
    (current-badges (get achievement-badges journey-data))
  )
    (asserts! (is-eq tx-sender wellness-contract-owner) err-wellness-owner-only)

    ;; Add badge if not already present
    (if (is-none (index-of current-badges badge-name))
      (map-set participant-journeys
        { participant: participant }
        (merge journey-data {
          achievement-badges: (unwrap! (as-max-len? (append current-badges badge-name) u10) err-wellness-invalid-input)
        })
      )
      false
    )

    (ok true)
  )
)

;; Research and insights functions

(define-read-only (get-wellness-insights-summary)
  {
    total-outcomes-recorded: (var-get next-outcome-id),
    total-community-events: (var-get next-community-event-id),
    ;; Additional aggregate metrics would be calculated here
    data-collection-since: stacks-block-height
  }
)
