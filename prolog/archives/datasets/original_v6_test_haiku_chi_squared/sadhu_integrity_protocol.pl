% ============================================================================
% CONSTRAINT STORY: sadhu_integrity_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sadhu_integrity_protocol, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sadhu_integrity_protocol
 *   human_readable: The Integrity Requirement (Sadhu's Sugar)
 *   domain: social/ethical
 *
 * SUMMARY:
 *   The integrity requirement mandates that an advisor embody their own
 *   counsel before delivering it to others. This constraint appears across
 *   spiritual traditions, mentorship frameworks, and ethical systems as a
 *   foundational principle: the sadhu (renunciate teacher) must have tasted
 *   the fruit they offer. However, the constraint creates a structural
 *   asymmetry: seekers cannot verify whether an advisor actually embodies
 *   their counsel, yet must accept guidance as if they did. The protocol
 *   functions as both coordination (it signals authenticity intent and
 *   creates accountability pressure) and extraction (it shields advisors from
 *   verification while concentrating their authority). The theater ratio has
 *   increased over time as the constraint's verification function degraded —
 *   modern seekers increasingly accept advice from advisors who explicitly
 *   disclaim embodiment (life coaches, therapists, strategists), yet the
 *   ideal of embodied authority persists as a cultural norm. The constraint
 *   exemplifies how ethical requirements can become instruments of
 *   institutional control when the cost of embodiment is asymmetrically
 *   distributed.
 *
 * KEY AGENTS:
 *   - Advice Seeker: Primary victim (powerless/trapped) — cannot exit without losing access to guidance; bears the cost of verification asymmetry
 *   - Authority Figure/Advisor: Primary beneficiary (institutional/arbitrage) — benefits from the requirement's signaling function while remaining unverifiable
 *   - Tradition Holder: Institutional actor (powerful/constrained) — maintains the protocol through cultural inertia despite degraded verification function
 *   - Skeptical Student: Secondary victim (moderate/constrained) — sees the mixed coordination-extraction but remains constrained by status asymmetry
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as ethical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sadhu_integrity_protocol, 0.58).
domain_priors:suppression_score(sadhu_integrity_protocol, 0.62).
domain_priors:theater_ratio(sadhu_integrity_protocol, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sadhu_integrity_protocol, extractiveness, 0.58).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sadhu_integrity_protocol, snare).
narrative_ontology:human_readable(sadhu_integrity_protocol, "The Integrity Requirement (Sadhu's Sugar)").
narrative_ontology:topic_domain(sadhu_integrity_protocol, "social/ethical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sadhu_integrity_protocol, advisory_authority_figures).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, advice_seekers).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, advisor_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADVICE SEEKER (SNARE) — Cannot exit the requirement without losing access to guidance. Bears the cost of enforcing advisor hypocrisy: must accept counsel while knowing the advisor does not embody it. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AUTHORITY FIGURE (ROPE) — Experiences the constraint as coordination: maintaining the authority-seeker boundary through the integrity protocol enables the advisory relationship to function. The protocol signals competence and trustworthiness. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.04. Net beneficiary from the coordination function.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: TRADITION HOLDER (PITON) — Maintains the protocol through inertia and theatrical appeal. The requirement has lost much of its original verification function (seekers cannot actually assess advisor hypocrisy). Instead, it persists as a performance of virtue and authority. theater_ratio=0.48 is borderline; cultural maintenance keeps the norm alive despite degraded function. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.29.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SKEPTICAL STUDENT (TANGLED ROPE) — Sees both coordination (the protocol does signal authenticity intent) and extraction (the advisor is not actually required to embody their counsel, only to claim it). The student benefits from access to wisdom while bearing the cost of verification asymmetry. d≈0.68, f(d)≈1.02, σ=0.8 → χ≈0.48.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal ethical perspective, integrity as a prerequisite for authority might appear immutable: moral authority requires embodiment. However, the structural data (ε=0.58, suppression=0.62, theater=0.48) contradicts this. The engine will compute a false summit, revealing that the protocol naturalizes a contingent institutional arrangement rather than expressing a law of ethics.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sadhu_integrity_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sadhu_integrity_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sadhu_integrity_protocol, TR),
    TR >= 0.70.

:- end_tests(sadhu_integrity_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The protocol creates significant asymmetry: advisors must claim embodiment but cannot be held accountable for hypocrisy (verification is structurally impossible for seekers), while seekers must accept guidance on the claim alone. The extraction is not maximal because advice quality itself provides some countervailing benefit. Suppression (0.62): Moderate-high. Seekers have limited alternatives: rejecting an advisor costs access to their wisdom; publicly questioning the requirement costs social standing. The suppression is not total because seekers can privately doubt while publicly accepting, and some communities allow advisors to disclaim embodiment. Theater ratio (0.48): Moderate. The protocol retains genuine coordination function (it does signal authenticity intent) but increasingly performative (the signal cannot be verified). The value reflects the constraint's middle position: not yet fully degraded to pure theater (piton) but no longer purely functional.
 *
 * PERSPECTIVAL GAP:
 *   The advice seeker sees a snare: they are trapped by the requirement and cannot verify its justification. The advisor sees a rope: the requirement coordinates their authority by giving seekers a ground for trust. The tradition holder sees a piton: the protocol is performative cultural maintenance. The skeptical student sees tangled rope: genuine coordination (the requirement does signal intent) mixed with extraction (the signal is unverifiable). The analytical observer risks seeing a mountain: integrity appears as an immutable ethical law. The perspectival gap reveals how the same requirement appears as constraint, coordination, and cultural inertia depending on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Advice seeker: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. Advisor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through signaling advantage. Tradition holder: Powerful + constrained → d≈0.50, f(d)≈0.65. Moderate extraction through cultural authority. Skeptical student: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction through information asymmetry. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification risks false summit (naturalizing contingent institutional requirement as ethical law).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypocrisy_detectability,
    'Can advice seekers actually detect whether an advisor embodies their own counsel, or is the protocol a performance that seekers cannot verify?',
    'Empirical comparison of advisor behavior claims vs actual practice; interviews with seekers about their verification methods',
    'If detectability is high: snare classification holds. If low: the constraint becomes pure theater (escalates to piton). If zero: the protocol is entirely performative and extraction is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypocrisy_detectability, empirical, 'Whether seekers can detect advisor hypocrisy').

omega_variable(
    authority_necessity,
    'Does good advice require the advisor to embody it, or can valuable counsel come from advisors who have not yet achieved their own teachings?',
    'Comparative effectiveness: advice from embodied vs non-embodied sources; historical cases of transformative counsel from advisors in personal struggle',
    'If embodiment is necessary: the constraint is protective (coordination function is real). If not: the protocol is arbitrary suppression masquerading as virtue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_necessity, conceptual, 'Whether advice effectiveness depends on advisor embodiment').

omega_variable(
    institutional_capture_risk,
    'Does the integrity requirement, by making authority self-verifying, create incentives for advisors to claim embodiment they do not possess?',
    'Case studies of institutional corruption; analysis of claims-verification gap in advisory contexts; historical evolution of the protocol',
    'If capture risk is high: the protocol becomes a mechanism for extractive authority (snare intensifies). If low: coordination function is genuinely protected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Whether the integrity requirement enables false authority claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sadhu_integrity_protocol, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sadhu_tr_t0, sadhu_integrity_protocol, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sadhu_tr_t25, sadhu_integrity_protocol, theater_ratio, 25, 0.38).
narrative_ontology:measurement(sadhu_tr_t50, sadhu_integrity_protocol, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(sadhu_be_t0, sadhu_integrity_protocol, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sadhu_be_t25, sadhu_integrity_protocol, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(sadhu_be_t50, sadhu_integrity_protocol, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sadhu_integrity_protocol, information_standard).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, authority_verification_gap).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, mentor_accountability_asymmetry).

% DUAL FORMULATION NOTE:
% The integrity requirement can be understood as two distinct constraints: (1) the ethical principle that authentic authority requires embodiment (mountain-like, from civilizational perspective) and (2) the institutional mechanism that uses unverifiable embodiment claims to concentrate advisory authority (snare, from seeker perspective). These are decomposed into separate stories to distinguish the philosophical claim from the structural extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sadhu_integrity_protocol, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
