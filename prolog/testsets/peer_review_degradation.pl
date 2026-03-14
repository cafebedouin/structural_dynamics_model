% ============================================================================
% CONSTRAINT STORY: peer_review_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peer_review_degradation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: peer_review_degradation
 *   human_readable: Peer Review Degradation as Coordination-Extraction Hybrid
 *   domain: academic_publishing/epistemic_governance
 *
 * SUMMARY:
 *   Peer review degradation represents the structural decay of a coordination
 *   mechanism under exponential demand growth. The system's original function
 *   — filtering obvious errors, detecting plagiarism, and certifying novelty
 *   — remains genuine. However, the mechanism has decayed because submission
 *   volume has grown 5-10x faster than reviewer capacity, creating systematic
 *   bottlenecks that are now filled by theater rather than substantive
 *   review. Theater has risen from ~0.52 to 0.81 over the 15-year interval,
 *   while extractiveness has risen from 0.35 to 0.62, indicating that the
 *   constraint has transitioned from mixed coordination-extraction toward
 *   extraction-dominant while theater has increased. The constraint is
 *   classified as Tangled Rope at the base level because it retains genuine
 *   coordination function (error filtering, novelty checking) while imposing
 *   asymmetric extraction (unpaid labor from early-career researchers, career
 *   dependency on journal acceptance, gatekeeping advantage for high-status
 *   authors). The constraint exhibits all six DR types from different
 *   structural positions: pure extraction (snare) for trapped early-career
 *   researchers; mixed coordination-extraction for field knowledge quality;
 *   pure coordination (rope) for publishers and high-status researchers;
 *   temporary problem with sunset (scaffold) for open science infrastructure;
 *   degraded ritual (piton) for traditional review itself; and false natural
 *   law (mountain candidate falsified) for the civilizational analytical
 *   perspective.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victims (powerless/trapped) — dependent on journal acceptance for career survival; unpaid review contributions; no arbitrage options
 *   - Field Knowledge Quality: Secondary victim (moderate/constrained) — genuine verification function degraded by review theater and false positives
 *   - Journal Publishers: Primary beneficiary (institutional/arbitrage) — control access to prestige; low cost for review labor; can shift business models if needed
 *   - High-Status Researchers: Beneficiaries (powerful/mobile) — can publish anywhere; extract value from review system without being trapped by it
 *   - Open Science Infrastructure: Organized agents (organized/constrained) — building alternative verification pathways; sunset mechanism is real and measurable
 *   - Review System Itself: Institutional piton (institutional/arbitrage) — maintains ritual through inertia; theater has become primary function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional decay as inherent to knowledge production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peer_review_degradation, 0.58).
domain_priors:suppression_score(peer_review_degradation, 0.62).
domain_priors:theater_ratio(peer_review_degradation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peer_review_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(peer_review_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(peer_review_degradation, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peer_review_degradation, tangled_rope).
narrative_ontology:human_readable(peer_review_degradation, "Peer Review Degradation as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(peer_review_degradation, "academic_publishing/epistemic_governance").

domain_priors:requires_active_enforcement(peer_review_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peer_review_degradation, journal_publishers).
narrative_ontology:constraint_beneficiary(peer_review_degradation, high_status_researchers).
narrative_ontology:constraint_victim(peer_review_degradation, field_knowledge_quality).
narrative_ontology:constraint_victim(peer_review_degradation, early_career_researchers).
narrative_ontology:constraint_victim(peer_review_degradation, replication_science).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped by career dependence on publication metrics and journal acceptance. No exit option: alternative publication routes (preprints, non-peer-reviewed venues) do not count for hiring/promotion. Peers with tenure have arbitrage; junior scholars have none. Experiences peer review as pure extraction: labor contributions unpaid, revisions demanded without reciprocal scrutiny of reviewer competence, accept-or-perish pressure. Maximum experienced extraction due to trapped exit and institutional power asymmetry.
constraint_indexing:constraint_classification(peer_review_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD KNOWLEDGE QUALITY (TANGLED ROPE) — Genuine coordination function: peer review identifies obvious errors, plagiarism, and methodological flaws. Asymmetric extraction: review quality has declined (theater increased) while capacity demands have grown, creating a false positive rate that pollutes the literature. Constrained exit: cannot abandon review system without collapse, but can degrade toward pure theater. The coordinate-and-extract tension is explicit: review maintains minimum epistemic standards while theater masks that standards have decayed.
constraint_indexing:constraint_classification(peer_review_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JOURNAL PUBLISHERS (ROPE) — Primary beneficiary with arbitrage. Peer review provides the core coordination service: editorial judgment, error filtering, quality certification. Publishers have exit options: they can shift to preprint-centric models, overlay review, or post-publication scrutiny. They experience review as enabling their product (certification) with manageable costs. Network effects and reputation make exit costly but not impossible — true arbitrage condition.
constraint_indexing:constraint_classification(peer_review_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-STATUS RESEARCHERS (ROPE) — Beneficiaries with mobile exit options. Can publish in any journal, preprint freely, and maintain citation impact independent of review theater. Review system coordinates their work: editorial handling, visibility, collaboration signals. Experience review as low-cost coordination; extraction falls on junior researchers and reviewers. High status creates mobile exit — could publish outside traditional venues with minimal career cost.
constraint_indexing:constraint_classification(peer_review_degradation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE INFRASTRUCTURE (SCAFFOLD) — Organized actors (arXiv, bioRxiv, medRxiv, open science centers) are building an exit pathway with generational sunset: rapid preprint dissemination combined with post-publication open review and registered reports. Low effective extraction because alternative verification mechanisms are viable and have measurable success. The review theater persists for career signaling (journal names still matter) but is losing monopoly on verification. Sunset clause is real: within 15-20 years, institutional hiring/promotion metrics should stabilize around preprint records and collaborative review rather than journal acceptance.
constraint_indexing:constraint_classification(peer_review_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL PEER REVIEW RITUAL (PITON) — The formal peer review process (2-3 reviewers, 3-6 month turnaround, accept/reject/revise cycles) is substantially performative at civilizational scale. The theater ratio (0.78) reflects that review quality has decayed relative to submission volume: reviewers are overloaded, incentives are misaligned, and detection of subtle errors is rare. The ritual persists through institutional inertia — journal prestige, hiring committee reliance on journal names, author expectations — not because review effectiveness is high. The Piton classification is appropriate: a degraded mechanism maintained by theater rather than function.
constraint_indexing:constraint_classification(peer_review_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE MOUNTAIN (MOUNTAIN CANDIDATE) — At civilizational scale, some quality control is inherent to knowledge production: claims must be scrutinized before acceptance. This perspective sees peer review as a natural law — verification requires peer examination. However, the structural data contradicts the mountain classification. The peer review system's specific form (journal-mediated, closed review, accept/reject binary) is contingent. The false summit indicator is high: the constraint is not immutable naturalization but institutional arrangement sustained by reputation networks and career structures.
constraint_indexing:constraint_classification(peer_review_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peer_review_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peer_review_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peer_review_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peer_review_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peer_review_degradation, TR),
    TR >= 0.70.

:- end_tests(peer_review_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting clear asymmetric extraction alongside genuine coordination function. Early-career researchers bear labor and career-dependency costs; publishers and high-status researchers capture benefits; the field's epistemic quality degrades. The value (not the original 0.72) reflects that some genuine coordination persists — errors are still caught, obvious plagiarism is still detected — preventing classification as pure snare. The 15-year trajectory (0.35 → 0.58) shows extraction has been accumulating faster than coordination, indicating the system's degradation. Suppression (0.62): High. Multiple barriers prevent exit: journal prestige remains hiring-metric dominant; preprints do not yet count equally for promotion; reviewer labor has no compensation and limited recognition; early-career career trajectories are path-dependent on initial publications. Suppression is structural (not just performance metrics) but not absolute — preprint alternatives exist and do provide partial exit, justifying 0.62 rather than 0.85+. Theater ratio (0.78): Very high and rising. Traditional peer review's performative content has increased dramatically. Reviewers often lack time for substantive engagement; desk rejections and form rejections increase without detailed feedback; 3-month delays provide theater of deliberation rather than actual scrutiny; the accept/reject binary masks that most papers undergo revision rather than genuine first-read acceptance. Theater has become the primary mechanism by which the system signals quality (journal brand) rather than substantive verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across all power levels. The snare perspective (early-career/trapped) and rope perspective (publishers/arbitrage) are internally inconsistent — the same structural fact (peer review gatekeeping) appears as pure extraction to one agent and pure coordination to another. This inconsistency is not an error in analysis; it reveals the constraint's hybrid structure. The key diagnostic: the beneficiaries have arbitrage or mobile exits (can publish outside the system, shift business models, ignore review outcomes), while the victims have trapped or constrained exits (must participate, cannot ignore career consequences). The theater ratio rise (0.78 and climbing) suggests the constraint is drifting from tangled rope toward snare — the coordination function is degrading while extraction mechanics persist. The scaffold perspective (sunset in 15-20 years via open science norms) provides the external view: the constraint is temporary in structural time, maintained by institutional inertia rather than necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) differs sharply across agent positions. Early-career researchers: d ≈ 0.95 (nearly total target) — they bear labor costs, face career suppression, have no arbitrage. Field knowledge quality: d ≈ 0.60 (moderate target) — benefits from error filtering but bears cost of false positives and review delays. Journal publishers: d ≈ 0.10 (near-beneficiary) — extract value from prestige certification, have arbitrage via business model flexibility. High-status researchers: d ≈ 0.08 (near-beneficiary) — reap citation benefits without career dependency on review. Open science coalition: d ≈ 0.50 (symmetric) — constrained by need to build alternatives but benefiting from network effects of coordination. The derivation chain (beneficiary/victim + exit options → d → f(d) → χ scaling) produces these values automatically. The perspectival gap (snare to rope) emerges naturally from the d distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy resolves by recognizing that peer review IS a coordination mechanism (verification, error detection, novelty filtering are genuinely necessary) AND an extraction mechanism (asymmetric labor, career gatekeeping, false positive persistence). The tangled_rope classification prevents two errors: (1) calling it pure snare, which would erase the real coordination function that field knowledge still depends on, and (2) calling it pure rope, which would erase the extraction and theater that have accumulated. The presence of genuine beneficiaries (publishers, high-status researchers) with asymmetric exit options confirms the hybrid. The presence of victims (field knowledge quality, early-career researchers) confirms the extraction. The theater rise (0.52 → 0.81) and extractiveness rise (0.35 → 0.58) show the system drifting toward snare over time. The scaffold perspective confirms the mandatrophy: open science infrastructure is building genuine alternatives that would dissolve the extraction IF career signaling could be decoupled from journal prestige. The constraint is neither purely benign nor purely malicious — it is a real coordination failure where the mechanism has become dominated by its own theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    review_quality_measurement,
    'How much of peer review''s current suppression derives from true capacity limits (too many submissions, too few reviewers) versus structural incentive misalignment (unpaid labor, no accountability, status games)?',
    'Comparative analysis of review quality/speed in systems with different incentive structures (paid review, signed review, reputation-tracked review, overlay review) and submission volume controls',
    'If capacity limits dominate: suppression is structural (mountain-adjacent). If incentives dominate: suppression is institutional choice (snare/tangled rope). Changes the diagnostic interpretation of whether reform is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_quality_measurement, empirical, 'Capacity limits vs. incentive misalignment in review degradation').

omega_variable(
    preprint_verification_equivalence,
    'Do arXiv-style rapid preprints plus post-publication open review achieve equivalent error detection and false-positive filtering as traditional peer review?',
    'Longitudinal tracking of false positive rates: manuscripts first posted as preprints + open review vs. those going through traditional review; analysis of error detection rates by mechanism (pre vs post publication)',
    'If equivalent: scaffold sunset is real, and this constraint has genuine time-bounded dissolution. If inferior: preprints are complementary not substitutive, and degradation may be permanent (piton/snare rather than scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preprint_verification_equivalence, empirical, 'Whether preprint + open review achieves equivalent verification').

omega_variable(
    journal_brand_persistence,
    'Will prestigious journal brand names (Nature, Science, Cell) retain hiring/promotion signaling power as preprint infrastructure and alternative review systems mature?',
    'Hiring committee decisions and grant agency evaluations over 10-year horizon: tracking whether journal names fade as evaluators shift to direct assessment of preprint records and collaborative review patterns',
    'If brand persists: early-career researcher trap persists (snare classification holds). If brand fades: exit becomes more arbitrage-like (rope/mobile reclassification). Determines whether escape paths are genuinely available or merely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(journal_brand_persistence, empirical, 'Persistence of journal prestige in hiring/promotion decisions').

omega_variable(
    reviewer_surplus_condition,
    'Is there sufficient qualified reviewer capacity available (underdeployed, unpaid reviewers) to restore peer review quality, or has capacity genuinely collapsed relative to submission volume?',
    'Survey of reviewer pool: willingness-to-review rates conditional on compensation/time/incentives; comparison of current reviewer time investment to estimated full-capacity review time for current submission volume',
    'If surplus exists: current degradation is institutional failure (fixable by better incentives). If collapsed: degradation is structural (requires system redesign). Affects whether tangled rope can be restored to pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reviewer_surplus_condition, empirical, 'Reviewer capacity surplus or deficit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peer_review_degradation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prd_tr_t0, peer_review_degradation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(prd_tr_t5, peer_review_degradation, theater_ratio, 5, 0.65).
narrative_ontology:measurement(prd_tr_t10, peer_review_degradation, theater_ratio, 10, 0.78).
narrative_ontology:measurement(prd_tr_t15, peer_review_degradation, theater_ratio, 15, 0.81).

% Extraction over time
narrative_ontology:measurement(prd_be_t0, peer_review_degradation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prd_be_t5, peer_review_degradation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prd_be_t10, peer_review_degradation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(prd_be_t15, peer_review_degradation, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peer_review_degradation, information_standard).
narrative_ontology:boltzmann_floor_override(peer_review_degradation, 0.12).
narrative_ontology:affects_constraint(peer_review_degradation, verification_bottleneck).
narrative_ontology:affects_constraint(peer_review_degradation, research_publication_bias).
narrative_ontology:affects_constraint(peer_review_degradation, academic_hiring_dependency).

% DUAL FORMULATION NOTE:
% Peer review degradation is downstream of research publication bias (which creates submission volume explosion) and upstream of academic hiring dependency (which makes journal prestige sticky despite open alternatives). The three constraints form a causal chain: publication bias → volume explosion → review degradation → career dependency persistence. Each has its own extractiveness and perspectives; decomposing them separately enables identification of intervention points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peer_review_degradation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
