;; ResearchValidator - Academic paper peer review and validation system
;; Researchers earn tokens through paper submissions and quality peer reviews

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_ALREADY_REVIEWED (err u104))
(define-constant ERR_ALREADY_EVALUATED (err u105))
(define-constant ERR_SELF_REVIEW (err u106))
(define-constant ERR_EMPTY_STRING (err u107))
(define-constant ERR_INVALID_SCORE (err u108))
(define-constant ERR_INVALID_PAPER_ID (err u109))
(define-constant ERR_EMPTY_HASH (err u110))

;; Constants
(define-constant MAX_SCORE u10)
(define-constant SUBMISSION_REWARD u15)
(define-constant QUALITY_REVIEW_REWARD u25)
(define-constant PUBLICATION_REWARD u75)

;; Data maps
(define-map researchers
  { researcher-id: principal }
  { name: (string-ascii 50), field: (string-ascii 50), h-index: uint, tokens: uint, verified: bool }
)

(define-map papers
  { paper-id: uint }
  { 
    author: principal, 
    title: (string-ascii 500), 
    abstract-hash: (buff 32),
    submission-time: uint, 
    published: bool,
    review-count: uint,
    citation-count: uint,
    quality-score: uint,
    evaluation-count: uint
  }
)

(define-map peer-reviews
  { paper-id: uint, reviewer: principal }
  { reviewed: bool }
)

(define-map citations
  { paper-id: uint, citing-researcher: principal }
  { cited: bool, citation-context: (string-ascii 100) }
)

(define-map quality-evaluations
  { paper-id: uint, evaluator: principal }
  { score: uint }
)

;; Variables
(define-data-var next-paper-id uint u1)
(define-data-var submission-counter uint u0)

;; Helper functions
(define-private (is-valid-paper-id (paper-id uint))
  (< paper-id (var-get next-paper-id))
)

;; Researcher functions
(define-public (register-researcher (name (string-ascii 50)) (field (string-ascii 50)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (> (len field) u0) ERR_EMPTY_STRING)
    (asserts! (is-none (map-get? researchers {researcher-id: caller})) ERR_ALREADY_EXISTS)
    (ok (map-set researchers 
      {researcher-id: caller} 
      {name: name, field: field, h-index: u0, tokens: u100, verified: false}))
  )
)

(define-public (update-researcher-profile (name (string-ascii 50)) (field (string-ascii 50)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (> (len field) u0) ERR_EMPTY_STRING)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (ok (map-set researchers 
      {researcher-id: caller} 
      (merge (unwrap! (map-get? researchers {researcher-id: caller}) ERR_NOT_FOUND)
             {name: name, field: field})))
  )
)

;; Paper functions
(define-public (submit-paper (title (string-ascii 500)) (abstract-hash (buff 32)))
  (let ((caller tx-sender)
        (paper-id (var-get next-paper-id)))
    (asserts! (> (len title) u0) ERR_EMPTY_STRING)
    (asserts! (> (len abstract-hash) u0) ERR_EMPTY_HASH)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (var-set submission-counter (+ (var-get submission-counter) u1))
    
    (map-set papers 
      {paper-id: paper-id} 
      { 
        author: caller, 
        title: title, 
        abstract-hash: abstract-hash,
        submission-time: (var-get submission-counter), 
        published: false,
        review-count: u0,
        citation-count: u0,
        quality-score: u0,
        evaluation-count: u0
      })
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

(define-public (peer-review-paper (paper-id uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get author paper))) ERR_SELF_REVIEW)
      (asserts! (is-none (map-get? peer-reviews {paper-id: paper-id, reviewer: caller})) ERR_ALREADY_REVIEWED)
      
      (map-set peer-reviews 
        {paper-id: paper-id, reviewer: caller} 
        {reviewed: true})
      
      (let ((new-review-count (+ (get review-count paper) u1))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND))
            (reviewer (unwrap! (map-get? researchers {researcher-id: caller}) ERR_NOT_FOUND)))
        
        (map-set papers 
          {paper-id: paper-id} 
          (merge paper {
            review-count: new-review-count,
            published: (>= new-review-count u3)
          }))
        
        (map-set researchers 
          {researcher-id: caller} 
          (merge reviewer {
            tokens: (+ (get tokens reviewer) u8),
            h-index: (+ (get h-index reviewer) u1)
          }))
        
        (if (and (>= new-review-count u3) (not (get published paper)))
          (map-set researchers 
            {researcher-id: (get author paper)} 
            (merge paper-author {
              tokens: (+ (get tokens paper-author) PUBLICATION_REWARD),
              h-index: (+ (get h-index paper-author) u15),
              verified: true
            }))
          true)
        
        (ok new-review-count)
      )
    )
  )
)

(define-public (cite-paper (paper-id uint) (citation-context (string-ascii 100)))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (> (len citation-context) u0) ERR_EMPTY_STRING)
    (asserts! (is-some (map-get? papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (is-none (map-get? citations {paper-id: paper-id, citing-researcher: caller})) ERR_ALREADY_EXISTS)
      
      (map-set citations 
        {paper-id: paper-id, citing-researcher: caller} 
        {cited: true, citation-context: citation-context})
      
      (let ((new-citation-count (+ (get citation-count paper) u1))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND)))
        
        (map-set papers 
          {paper-id: paper-id} 
          (merge paper {citation-count: new-citation-count}))
        
        (map-set researchers 
          {researcher-id: (get author paper)} 
          (merge paper-author {
            tokens: (+ (get tokens paper-author) SUBMISSION_REWARD)
          }))
        
        (ok new-citation-count)
      )
    )
  )
)

(define-public (evaluate-paper-quality (paper-id uint) (score uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (and (>= score u1) (<= score MAX_SCORE)) ERR_INVALID_SCORE)
    (asserts! (is-some (map-get? papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get author paper))) ERR_SELF_REVIEW)
      (asserts! (is-some (map-get? citations {paper-id: paper-id, citing-researcher: caller})) ERR_NOT_FOUND)
      (asserts! (is-none (map-get? quality-evaluations {paper-id: paper-id, evaluator: caller})) ERR_ALREADY_EVALUATED)
      
      (map-set quality-evaluations 
        {paper-id: paper-id, evaluator: caller} 
        {score: score})
      
      (let ((current-total-score (* (get quality-score paper) (get evaluation-count paper)))
            (new-evaluation-count (+ (get evaluation-count paper) u1))
            (new-total-score (+ current-total-score score))
            (new-average-score (/ new-total-score new-evaluation-count))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND)))
        
        (map-set papers 
          {paper-id: paper-id} 
          (merge paper {
            quality-score: new-average-score,
            evaluation-count: new-evaluation-count
          }))
        
        (if (>= score u8)
          (map-set researchers 
            {researcher-id: (get author paper)} 
            (merge paper-author {
              tokens: (+ (get tokens paper-author) QUALITY_REVIEW_REWARD),
              h-index: (+ (get h-index paper-author) u5)
            }))
          true)
        
        (ok new-average-score)
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-researcher-info (researcher-id principal))
  (map-get? researchers {researcher-id: researcher-id})
)

(define-read-only (get-paper (paper-id uint))
  (map-get? papers {paper-id: paper-id})
)

(define-read-only (get-peer-review (paper-id uint) (reviewer principal))
  (map-get? peer-reviews {paper-id: paper-id, reviewer: reviewer})
)

(define-read-only (get-citation (paper-id uint) (citing-researcher principal))
  (map-get? citations {paper-id: paper-id, citing-researcher: citing-researcher})
)

(define-read-only (get-quality-evaluation (paper-id uint) (evaluator principal))
  (map-get? quality-evaluations {paper-id: paper-id, evaluator: evaluator})
)

(define-read-only (get-total-papers)
  (- (var-get next-paper-id) u1)
)
