% ============================================================================
% CONSTRAINT STORY: verification_lag_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_verification_lag_asymmetry, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: verification_lag_asymmetry
 *   human_readable: Verification Lag Asymmetry Across Domains
 *   domain: epistemology/institutional_verification
 *
 * SUMMARY:
 *   The verification lag asymmetry is a structural feature of knowledge
 *   production where claiming novel findings confers immediate benefits
 *   (priority, funding, citations) while the costs of
 *   verification—confirming, replicating, or refuting claims—are distributed
 *   across a later-arriving verification community. This asymmetry is neither
 *   a natural law nor a mere coordination problem; it is a tangled hybrid of
 *   genuine coordination needs (establishing truth requires distributed
 *   effort) and extractive mechanisms (early claimants capture benefits while
 *   verification bearers absorb costs). The constraint exhibits six
 *   perspectival readings: snare for the epistemic commons (trapped, no
 *   exit), tangled rope for the verification community (constrained but also
 *   supported), rope for early claimants (beneficiaries with arbitrage),
 *   scaffold for organized verification movements (building exit pathways),
 *   piton for legacy review systems (performative persistence), and false
 *   summit for those who naturalize the lag as immutable. The theater ratio
 *   (0.65) reflects that much verification work is performative—peer review
 *   assesses novelty and plausibility but cannot verify complex
 *   methodological choices, data quality, or statistical validity. The
 *   extractiveness trajectory (0.28 → 0.52 over the interval) shows that as
 *   domains mature and complexity increases, the lag asymmetry worsens:
 *   early-mover advantages compound while verification capacity lags behind
 *   claim production.
 *
 * KEY AGENTS:
 *   - Early Claimants: Institutional beneficiaries (institutional/arbitrage) — capture priority, funding, and citation benefits during lag window
 *   - Epistemic Commons: Powerless victim (powerless/trapped) — abstract collective good that cannot organize or exit; absorbs costs of unverified claims
 *   - Verification Community: Moderate victim/beneficiary (moderate/constrained) — bears verification burden but also benefits from structured verification work and grant funding
 *   - Open Verification Movement: Organized coalition (organized/constrained) — building alternative verification pathways with institutional sunset logic
 *   - Legacy Review System: Institutional performer (institutional/arbitrage) — maintains gatekeeping role through career incentive alignment despite recognized limitations
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as epistemological necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verification_lag_asymmetry, 0.52).
domain_priors:suppression_score(verification_lag_asymmetry, 0.58).
domain_priors:theater_ratio(verification_lag_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verification_lag_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(verification_lag_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(verification_lag_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verification_lag_asymmetry, tangled_rope).
narrative_ontology:human_readable(verification_lag_asymmetry, "Verification Lag Asymmetry Across Domains").
narrative_ontology:topic_domain(verification_lag_asymmetry, "epistemology/institutional_verification").

domain_priors:requires_active_enforcement(verification_lag_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verification_lag_asymmetry, early_claimants).
narrative_ontology:constraint_beneficiary(verification_lag_asymmetry, first_movers).
narrative_ontology:constraint_victim(verification_lag_asymmetry, verification_capacity).
narrative_ontology:constraint_victim(verification_lag_asymmetry, late_replicators).
narrative_ontology:constraint_victim(verification_lag_asymmetry, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — Cannot exit the lag asymmetry; absorbs full cost of unverified claims entering the knowledge base. No exit option and no advocate. Experiences maximum extraction as false positives accumulate without self-correction mechanism.
constraint_indexing:constraint_classification(verification_lag_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VERIFICATION COMMUNITY (TANGLED ROPE) — Constrained by resource scarcity and publication bias against negative results, but also benefits from structured verification work (grants, career advancement, methodological development). Genuine coordination function (establishing truth) alongside asymmetric extraction (late replicators bear verification burden while early claimants capture priority).
constraint_indexing:constraint_classification(verification_lag_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY CLAIMANT (ROPE) — Nets positive returns from the lag asymmetry through citation priority, funding advantage, and reputation gains. Experiences the constraint as coordination: announcing findings to enable follow-up work. Beneficiary with high exit flexibility (can pivot research directions, move between institutions).
constraint_indexing:constraint_classification(verification_lag_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN VERIFICATION MOVEMENT (SCAFFOLD) — Organized agents (preprint platforms, registered reports, open data initiatives, reproducibility organizations) are building parallel verification structures that reduce lag asymmetry. Sees the current bottleneck as temporary and solvable through institutional design. Has agency and perceives a sunset: distributed verification and transparency norms will replace centralized gatekeeper models within 15-30 years.
constraint_indexing:constraint_classification(verification_lag_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY VERIFICATION SYSTEM (PITON) — The traditional peer review and journal gatekeeping apparatus maintains theater without commensurate functional verification. Reviewers assess novelty and plausibility but cannot verify complex experimental protocols, statistical choices, or data quality. The system persists through institutional inertia (career advancement tied to journal impact factors, funding committees rewarding publications) despite recognized limitations. Theater ratio reflects the gap between what review claims to do and what it actually does.
constraint_indexing:constraint_classification(verification_lag_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risks classifying the lag asymmetry as a natural law of knowledge production: complex claims always require time to verify, replication is inherently slower than discovery, and the asymmetry between first-mover and late-replicator is an immutable feature of how science works. This naturalization obscures that the asymmetry is structurally contingent on institutional arrangements (publication bias, career incentive alignment, funding concentration) rather than epistemologically necessary.
constraint_indexing:constraint_classification(verification_lag_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verification_lag_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(verification_lag_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(verification_lag_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(verification_lag_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(verification_lag_asymmetry, TR),
    TR >= 0.70.

:- end_tests(verification_lag_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Early claimants capture significant benefits during the lag window (2-5 years typically), and verification costs are real but distributed. The extractiveness is not maximal because: (1) many early claimants are legitimately taking on high risk, justifying priority rewards; (2) the verification community does receive recognition and funding; (3) some claims are genuinely difficult to verify regardless of incentives. However, the extractiveness exceeds simple coordination because the lag creates a ratchet effect—false claims enter the literature during the lag window and are harder to correct than they were to make, creating asymmetric costs. Suppression (0.58): Moderate-high. Barriers to early verification include: resource scarcity (replication requires equipment, personnel, funding that rivals original research), publication bias (journals preferentially publish novel findings over negative results), and career risk (early-career researchers who spend time replicating rather than producing novel work are disadvantaged). However, suppression is not total because: organized efforts (preprints, registered reports, open data) are lowering barriers; some fields have stronger verification cultures than others. Theater ratio (0.65): Moderate-high. Peer review for novel claims assesses novelty and plausibility but cannot verify: complex experimental protocols, parameter choices, data quality issues, hidden statistical degrees of freedom, or undisclosed failure attempts. The review theater persists because it serves functions (legitimacy, novelty assessment, journal branding) even when verification is limited. Open science platforms reduce theater by exposing methods and data before review.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lag_threshold_definition,
    'What verification lag duration constitutes an asymmetry worth constraining, versus a legitimate discovery-confirmation cycle?',
    'Empirical analysis: distribution of replication timelines across disciplines; correlation between lag duration and claim validity; field-specific norms for what counts as ''timely'' verification',
    'If threshold ≤ 2 years: many legitimate discoveries counted as extraction. If threshold ≥ 10 years: severe extraction persists unaddressed. Threshold shifts across fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lag_threshold_definition, empirical, 'Definition of actionable verification lag versus legitimate discovery cycle').

omega_variable(
    replication_resource_scaling,
    'Is the lag asymmetry driven by resource scarcity (replication requires dedicated funding) or institutional incentives (careers reward novelty over verification)?',
    'Counterfactual analysis: hypothetical increased replication funding without incentive reform; comparison of field outcomes where incentive structures differ; cost accounting for replication work across domain types',
    'If resource-driven: policy solutions focus on funding mechanisms. If incentive-driven: solutions require career structure reform. Likely both, but proportions determine intervention priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_resource_scaling, empirical, 'Whether lag asymmetry is resource scarcity or incentive misalignment').

omega_variable(
    distributed_verification_sufficiency,
    'Can distributed verification mechanisms (crowd-sourced scrutiny, open platforms, registered reports) achieve comparable error detection rates to traditional centralized review?',
    'Comparative study of error detection rates: distributed versus traditional verification for matched claim sets; longitudinal tracking of false positive rates in preprint vs journal populations; systematic review of platform-native error correction dynamics',
    'If sufficient: scaffold sunset is achievable — alternative verification pathways can replace gatekeeping. If insufficient: distributed systems miss errors that centralized review catches (or vice versa) — may require hybrid models rather than wholesale replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_verification_sufficiency, empirical, 'Whether distributed verification can replace centralized gatekeeping').

omega_variable(
    cross_domain_asymmetry_variance,
    'Does the lag asymmetry vary fundamentally across domains (physics vs biology vs social science) due to verification difficulty, or is variance driven by institutional structure (journal culture, funding models)?',
    'Cross-domain comparison controlling for claim complexity and resource intensity; analysis of fields where institutional structures shifted (e.g., adoption of preprints, open data mandates) to measure lag changes; causality analysis on incentive reform outcomes',
    'If domain-intrinsic: one-size-fits-all solutions fail — need domain-specific approaches. If institutional: reforms to incentives and structures can meaningfully reduce asymmetry across all domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_domain_asymmetry_variance, empirical, 'Whether lag asymmetry is domain-intrinsic or institutionally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_lag_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(verlag_tr_t0, verification_lag_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(verlag_tr_t10, verification_lag_asymmetry, theater_ratio, 10, 0.58).
narrative_ontology:measurement(verlag_tr_t20, verification_lag_asymmetry, theater_ratio, 20, 0.65).
narrative_ontology:measurement(verlag_tr_t5, verification_lag_asymmetry, theater_ratio, 5, 0.53).

% Extraction over time
narrative_ontology:measurement(verlag_be_t0, verification_lag_asymmetry, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(verlag_be_t10, verification_lag_asymmetry, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(verlag_be_t20, verification_lag_asymmetry, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(verlag_be_t5, verification_lag_asymmetry, base_extractiveness, 5, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_lag_asymmetry, information_standard).
narrative_ontology:affects_constraint(verification_lag_asymmetry, publication_bias_against_replication).
narrative_ontology:affects_constraint(verification_lag_asymmetry, research_career_incentive_misalignment).
narrative_ontology:affects_constraint(verification_lag_asymmetry, epistemic_commons_tragedy).

% DUAL FORMULATION NOTE:
% Verification lag asymmetry is a cross-domain constraint that manifests differently in specific domains (physics, biology, social science, machine learning). Domain-specific constraint stories should decompose this general structure to account for field-level verification costs and institutional cultures. This story captures the general institutional and incentive structure; domain stories capture domain-specific ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
