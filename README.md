# ResearchValidator

A decentralized academic paper peer review and validation system built on Stacks blockchain. Researchers earn tokens through quality paper submissions and thorough peer reviews.

## Features

- **Researcher Registration**: Register with academic credentials and research field
- **Paper Submission**: Submit research papers with cryptographic proof of authenticity
- **Peer Review System**: Collaborative review process with token incentives
- **Citation Tracking**: Track paper citations and reward impactful research
- **Quality Evaluation**: Community-driven quality scoring system
- **Token Rewards**: Earn tokens for contributions to academic validation

## Smart Contract Functions

### Public Functions
- `register-researcher` - Register as a researcher
- `submit-paper` - Submit a research paper for review
- `peer-review-paper` - Review submitted papers
- `cite-paper` - Cite papers in your research
- `evaluate-paper-quality` - Rate paper quality

### Read-Only Functions
- `get-researcher-info` - Get researcher profile
- `get-paper` - Get paper details
- `get-total-papers` - Get total number of papers

## Token Economics

- **Publication Reward**: 75 tokens for papers with 3+ peer reviews
- **Quality Review Reward**: 25 tokens for highly rated papers
- **Submission Reward**: 15 tokens per citation received

## Getting Started

1. Deploy the contract using Clarinet
2. Register as a researcher
3. Submit papers for peer review
4. Participate in the review process to earn tokens

## License

MIT License
\`\`\`

```clarity file="project-2-freelancer-skills/contracts/skill-verifier.clar"
;; SkillVerifier - Freelancer skill verification and endorsement platform
;; Freelancers earn tokens through skill demonstrations and client endorsements

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_ALREADY_ENDORSED (err u104))
(define-constant ERR_ALREADY_RATED (err u105))
(define-constant ERR_SELF_ENDORSEMENT (err u106))
(define-constant ERR_EMPTY_STRING (err u107))
(define-constant ERR_INVALID_RATING (err u108))
(define-constant ERR_INVALID_SKILL_ID (err u109))
(define-constant ERR_EMPTY_HASH (err u110))

;; Constants
(define-constant MAX_RATING u5)
(define-constant PROJECT_REWARD u12)
(define-constant EXCELLENCE_REWARD u30)
(define-constant MASTERY_REWARD u60)

;; Data maps
(define-map freelancers
  { freelancer-id: principal }
  { name: (string-ascii 50), expertise: (string-ascii 50), reputation: uint, tokens: uint, verified: bool }
)

(define-map skills
  { skill-id: uint }
  { 
    freelancer: principal, 
    description: (string-ascii 500), 
    portfolio-hash: (buff 32),
    timestamp: uint, 
    certified: bool,
    endorsement-count: uint,
    project-count: uint,
    client-rating: uint,
    rating-count: uint
  }
)

(define-map skill-endorsements
  { skill-id: uint, endorser: principal }
  { endorsed: bool }
)

(define-map projects
  { skill-id: uint, client: principal }
  { completed: bool, project-notes: (string-ascii 100) }
)

(define-map client-ratings
  { skill-id: uint, rater: principal }
  { rating: uint }
)

;; Variables
(define-data-var next-skill-id uint u1)
(define-data-var activity-counter uint u0)

;; Helper functions
(define-private (is-valid-skill-id (skill-id uint))
  (&lt; skill-id (var-get next-skill-id))
)

;; Freelancer functions
(define-public (register-freelancer (name (string-ascii 50)) (expertise (string-ascii 50)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (> (len expertise) u0) ERR_EMPTY_STRING)
    (asserts! (is-none (map-get? freelancers {freelancer-id: caller})) ERR_ALREADY_EXISTS)
    (ok (map-set freelancers 
      {freelancer-id: caller} 
      {name: name, expertise: expertise, reputation: u0, tokens: u100, verified: false}))
  )
)

(define-public (update-freelancer-profile (name (string-ascii 50)) (expertise (string-ascii 50)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (> (len expertise) u0) ERR_EMPTY_STRING)
    (asserts! (is-some (map-get? freelancers {freelancer-id: caller})) ERR_NOT_FOUND)
    (ok (map-set freelancers 
      {freelancer-id: caller} 
      (merge (unwrap! (map-get? freelancers {freelancer-id: caller}) ERR_NOT_FOUND)
             {name: name, expertise: expertise})))
  )
)

;; Skill functions
(define-public (showcase-skill (description (string-ascii 500)) (portfolio-hash (buff 32)))
  (let ((caller tx-sender)
        (skill-id (var-get next-skill-id)))
    (asserts! (> (len description) u0) ERR_EMPTY_STRING)
    (asserts! (> (len portfolio-hash) u0) ERR_EMPTY_HASH)
    (asserts! (is-some (map-get? freelancers {freelancer-id: caller})) ERR_NOT_FOUND)
    (var-set activity-counter (+ (var-get activity-counter) u1))
    
    (map-set skills 
      {skill-id: skill-id} 
      { 
        freelancer: caller, 
        description: description, 
        portfolio-hash: portfolio-hash,
        timestamp: (var-get activity-counter), 
        certified: false,
        endorsement-count: u0,
        project-count: u0,
        client-rating: u0,
        rating-count: u0
      })
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)
  )
)

(define-public (endorse-skill (skill-id uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-skill-id skill-id) ERR_INVALID_SKILL_ID)
    (asserts! (is-some (map-get? freelancers {freelancer-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? skills {skill-id: skill-id})) ERR_NOT_FOUND)
    
    (let ((skill (unwrap! (map-get? skills {skill-id: skill-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get freelancer skill))) ERR_SELF_ENDORSEMENT)
      (asserts! (is-none (map-get? skill-endorsements {skill-id: skill-id, endorser: caller})) ERR_ALREADY_ENDORSED)
      
      (map-set skill-endorsements 
        {skill-id: skill-id, endorser: caller} 
        {endorsed: true})
      
      (let ((new-endorsement-count (+ (get endorsement-count skill) u1))
            (skill-freelancer (unwrap! (map-get? freelancers {freelancer-id: (get freelancer skill)}) ERR_NOT_FOUND))
            (endorser-freelancer (unwrap! (map-get? freelancers {freelancer-id: caller}) ERR_NOT_FOUND)))
        
        (map-set skills 
          {skill-id: skill-id} 
          (merge skill {
            endorsement-count: new-endorsement-count,
            certified: (>= new-endorsement-count u3)
          }))
        
        (map-set freelancers 
          {freelancer-id: caller} 
          (merge endorser-freelancer {
            tokens: (+ (get tokens endorser-freelancer) u7),
            reputation: (+ (get reputation endorser-freelancer) u1)
          }))
        
        (if (and (>= new-endorsement-count u3) (not (get certified skill)))
          (map-set freelancers 
            {freelancer-id: (get freelancer skill)} 
            (merge skill-freelancer {
              tokens: (+ (get tokens skill-freelancer) MASTERY_REWARD),
              reputation: (+ (get reputation skill-freelancer) u12),
              verified: true
            }))
          true)
        
        (ok new-endorsement-count)
      )
    )
  )
)

(define-public (complete-project (skill-id uint) (project-notes (string-ascii 100)))
  (let ((caller tx-sender))
    (asserts! (is-valid-skill-id skill-id) ERR_INVALID_SKILL_ID)
    (asserts! (> (len project-notes) u0) ERR_EMPTY_STRING)
    (asserts! (is-some (map-get? skills {skill-id: skill-id})) ERR_NOT_FOUND)
    
    (let ((skill (unwrap! (map-get? skills {skill-id: skill-id}) ERR_NOT_FOUND)))
      (asserts! (is-none (map-get? projects {skill-id: skill-id, client: caller})) ERR_ALREADY_EXISTS)
      
      (map-set projects 
        {skill-id: skill-id, client: caller} 
        {completed: true, project-notes: project-notes})
      
      (let ((new-project-count (+ (get project-count skill) u1))
            (skill-freelancer (unwrap! (map-get? freelancers {freelancer-id: (get freelancer skill)}) ERR_NOT_FOUND)))
        
        (map-set skills 
          {skill-id: skill-id} 
          (merge skill {project-count: new-project-count}))
        
        (map-set freelancers 
          {freelancer-id: (get freelancer skill)} 
          (merge skill-freelancer {
            tokens: (+ (get tokens skill-freelancer) PROJECT_REWARD)
          }))
        
        (ok new-project-count)
      )
    )
  )
)

(define-public (rate-freelancer-work (skill-id uint) (rating uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-skill-id skill-id) ERR_INVALID_SKILL_ID)
    (asserts! (and (>= rating u1) (&lt;= rating MAX_RATING)) ERR_INVALID_RATING)
    (asserts! (is-some (map-get? skills {skill-id: skill-id})) ERR_NOT_FOUND)
    
    (let ((skill (unwrap! (map-get? skills {skill-id: skill-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get freelancer skill))) ERR_SELF_ENDORSEMENT)
      (asserts! (is-some (map-get? projects {skill-id: skill-id, client: caller})) ERR_NOT_FOUND)
      (asserts! (is-none (map-get? client-ratings {skill-id: skill-id, rater: caller})) ERR_ALREADY_RATED)
      
      (map-set client-ratings 
        {skill-id: skill-id, rater: caller} 
        {rating: rating})
      
      (let ((current-total-rating (* (get client-rating skill) (get rating-count skill)))
            (new-rating-count (+ (get rating-count skill) u1))
            (new-total-rating (+ current-total-rating rating))
            (new-average-rating (/ new-total-rating new-rating-count))
            (skill-freelancer (unwrap! (map-get? freelancers {freelancer-id: (get freelancer skill)}) ERR_NOT_FOUND)))
        
        (map-set skills 
          {skill-id: skill-id} 
          (merge skill {
            client-rating: new-average-rating,
            rating-count: new-rating-count
          }))
        
        (if (>= rating u4)
          (map-set freelancers 
            {freelancer-id: (get freelancer skill)} 
            (merge skill-freelancer {
              tokens: (+ (get tokens skill-freelancer) EXCELLENCE_REWARD),
              reputation: (+ (get reputation skill-freelancer) u6)
            }))
          true)
        
        (ok new-average-rating)
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-freelancer-info (freelancer-id principal))
  (map-get? freelancers {freelancer-id: freelancer-id})
)

(define-read-only (get-skill (skill-id uint))
  (map-get? skills {skill-id: skill-id})
)

(define-read-only (get-skill-endorsement (skill-id uint) (endorser principal))
  (map-get? skill-endorsements {skill-id: skill-id, endorser: endorser})
)

(define-read-only (get-project (skill-id uint) (client principal))
  (map-get? projects {skill-id: skill-id, client: client})
)

(define-read-only (get-client-rating (skill-id uint) (rater principal))
  (map-get? client-ratings {skill-id: skill-id, rater: rater})
)

(define-read-only (get-total-skills)
  (- (var-get next-skill-id) u1)
)
