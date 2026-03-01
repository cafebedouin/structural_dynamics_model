% ============================================================================
% CONSTRAINT STORY: meritocratic_ideology_as_error_propagation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meritocratic_ideology_as_error_propagation, []).

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
 *   constraint_id: meritocratic_ideology_as_error_propagation
 *   human_readable: Meritocratic Ideology as Error Propagation Mechanism
 *   domain: social_systems/institutional_dynamics/stratification_mechanics
 *
 * SUMMARY:
 *   Meritocratic ideology functions as an error propagation mechanism in
 *   institutional evaluation systems. The constraint exhibits structural
 *   position divergence: institutional evaluators experience it as pure
 *   coordination (efficient allocation standards), while marginalized
 *   analytical actors experience it as extraction (systematic misrecognition
 *   despite demonstrated competence). The core mechanism is error type
 *   divergence by power position: Type I errors (false positives:
 *   credentialing actors who lack competence) systematically favor
 *   institutional positions, while Type II errors (false negatives: failing
 *   to credential competent actors) systematically burden marginalized
 *   positions. This error distribution is not random noise but a structural
 *   feature that accumulates as extraction over biographical time horizons.
 *   The ideology's coordination function is genuine — societies require
 *   competence allocation mechanisms — but the specific implementation
 *   propagates systematic bias that benefits credentialed gatekeepers while
 *   extracting from non-credentialed contributors. The constraint's theater
 *   ratio (0.68) reflects increasing decoupling between meritocratic rhetoric
 *   and actual competence measurement: credential signals have degraded
 *   through grade inflation and teaching-to-test dynamics, yet the
 *   performative evaluation rituals intensify. The Cassandra character
 *   appears in this constraint: marginalized analytical actors may possess
 *   superior analytical capacity (can see the error distribution) but are
 *   systematically disbelieved because the meritocratic frame attributes
 *   their marginalization to lack of competence rather than to systematic
 *   bias.
 *
 * KEY AGENTS:
 *   - Marginalized Analytical Actors: Primary victims (powerless/identity_locked) — possess analytical capacity to see structural errors but cognitively captured by meritocratic frame; cannot exit without abandoning professional identity; bear full cost of Type II errors
 *   - Institutional Evaluators: Primary beneficiaries (institutional/arbitrage) — experience constraint as pure coordination; benefit from Type I error bias that validates credentialed candidates; maintain evaluator authority through meritocratic legitimacy narrative
 *   - Aspiring Credentialed Professionals: Secondary victims (moderate/constrained) — face credential acquisition costs and gatekeeping barriers; also benefit from coordination function once credentials are obtained; mixed extraction experience
 *   - Reform Coalition: Organized actors (organized/mobile) — blind review advocates, portfolio assessment movements, anti-bias training programs; see both coordination and extraction; building alternative evaluation pathways but constrained by institutional inertia
 *   - Legacy Credentialing Institutions: Institutional actors (institutional/constrained) — maintain meritocratic rhetoric while credential signal value degrades; high theater ratio; constrained by legitimacy dependence on meritocratic frame
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — collective good of accurate competence allocation; bears cost of systematic error propagation; no advocate and no exit option
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meritocratic_ideology_as_error_propagation, 0.48).
domain_priors:suppression_score(meritocratic_ideology_as_error_propagation, 0.62).
domain_priors:theater_ratio(meritocratic_ideology_as_error_propagation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meritocratic_ideology_as_error_propagation, extractiveness, 0.48).
narrative_ontology:constraint_metric(meritocratic_ideology_as_error_propagation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(meritocratic_ideology_as_error_propagation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meritocratic_ideology_as_error_propagation, tangled_rope).
narrative_ontology:human_readable(meritocratic_ideology_as_error_propagation, "Meritocratic Ideology as Error Propagation Mechanism").
narrative_ontology:topic_domain(meritocratic_ideology_as_error_propagation, "social_systems/institutional_dynamics/stratification_mechanics").

domain_priors:requires_active_enforcement(meritocratic_ideology_as_error_propagation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meritocratic_ideology_as_error_propagation, institutional_evaluators).
narrative_ontology:constraint_beneficiary(meritocratic_ideology_as_error_propagation, credentialed_gatekeepers).
narrative_ontology:constraint_beneficiary(meritocratic_ideology_as_error_propagation, legacy_position_holders).
narrative_ontology:constraint_victim(meritocratic_ideology_as_error_propagation, marginalized_analytical_actors).
narrative_ontology:constraint_victim(meritocratic_ideology_as_error_propagation, non_credentialed_contributors).
narrative_ontology:constraint_victim(meritocratic_ideology_as_error_propagation, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED ANALYTICAL ACTOR (SNARE) — Identity-locked rather than structurally trapped: possesses analytical capacity to see structural errors but cannot exit the meritocratic frame without abandoning professional identity. The ideology functions as cognitive capture — the actor internalizes 'if I were truly competent, I would be recognized' even while observing systematic misrecognition. Experiences maximum extraction: contributes analytical labor that is systematically misattributed or ignored, bears full cost of Type II errors (false negatives on their contributions), while beneficiaries capture credit through Type I error bias (false positives on credentialed work).
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING CREDENTIALED PROFESSIONAL (TANGLED ROPE) — Constrained by credential acquisition costs and institutional access barriers, but also benefits from the coordination function: meritocratic ideology provides legible pathways and evaluation standards. Experiences mixed extraction: must perform costly signaling and navigate gatekeeping, but successful navigation yields genuine coordination benefits. Sees both the ladder and the rungs that have been removed.
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL EVALUATOR (ROPE) — Benefits from coordination narrative: meritocratic ideology legitimizes evaluation authority and provides decision heuristics that reduce cognitive load. Experiences the constraint as pure coordination: 'we need standards to allocate positions efficiently.' Arbitrage exit option reflects ability to move between institutions while maintaining evaluator status. Does not perceive extraction because the error distribution (Type I bias favoring credentialed candidates) systematically benefits this position.
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Organized actors (blind review advocates, portfolio assessment movements, anti-bias training programs) see both coordination function and extraction mechanism. Mobile exit reflects ability to build alternative evaluation systems, but constrained by institutional inertia. Experiences the constraint as genuinely hybrid: meritocratic standards serve coordination needs while systematically producing biased error distributions. Sees the ideology as reformable rather than requiring full replacement.
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CREDENTIALING INSTITUTION (PITON) — Maintains meritocratic rhetoric while primary function has atrophied: credential signals have decoupled from competence measures through grade inflation, credential proliferation, and teaching-to-test dynamics. High theater ratio reflects performative evaluation rituals that no longer reliably distinguish capability. Institution is constrained rather than arbitrage because declining signal value threatens institutional legitimacy — cannot easily exit the meritocratic frame it helped construct.
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, meritocratic ideology serves genuine coordination function (societies need competence allocation mechanisms) while producing systematic extraction through error type divergence. The constraint is not a false summit — some form of evaluation standard is structurally necessary — but the specific implementation propagates errors that accumulate as extraction. Analytical position sees both the coordination floor (Boltzmann minimum for any evaluation system) and the extractive overhead (systematic bias in error distribution).
constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meritocratic_ideology_as_error_propagation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meritocratic_ideology_as_error_propagation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meritocratic_ideology_as_error_propagation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meritocratic_ideology_as_error_propagation, TR),
    TR >= 0.70.

:- end_tests(meritocratic_ideology_as_error_propagation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts through systematic error distribution: Type I errors (false positives on credentialed actors) flow benefits to institutional positions, while Type II errors (false negatives on marginalized actors) impose costs on non-credentialed positions. The extraction is not total because the coordination function is genuine — meritocratic standards do allocate some competence accurately — but the error bias accumulates as extraction over time. The value reflects that roughly half of the constraint's effect is coordination (necessary evaluation overhead) and half is extraction (systematic bias). Suppression (0.62): Moderate-high. Significant barriers to challenging meritocratic legitimacy include: identity fusion (marginalized actors internalize 'I would be recognized if I were competent'), credential requirements for institutional access, publication bias favoring credentialed authors, and social penalty for claiming systematic bias rather than individual inadequacy. Suppression is not total — reform movements exist and some actors do exit the frame — but the ideology's hegemonic status makes alternatives costly. Theater ratio (0.68): High and increasing. Meritocratic evaluation rituals have become substantially performative: standardized tests measure test-taking skill rather than domain competence, credential proliferation has decoupled degrees from capability, grade inflation has compressed signal range, and teaching-to-test dynamics have optimized for metric gaming rather than learning. The theater has increased over the 45-year interval as Goodhart's Law operates on evaluation metrics: when a measure becomes a target, it ceases to be a good measure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates structural position divergence across power indices. Institutional evaluators see pure coordination (Rope) — meritocratic standards legitimize their authority and provide efficient decision heuristics. They do not perceive extraction because the error distribution systematically benefits their position: Type I errors validate their credentialing decisions, while Type II errors are invisible (rejected candidates disappear from view). Marginalized analytical actors see pure extraction (Snare) — they observe systematic misrecognition despite demonstrated competence, and the meritocratic frame prevents them from attributing this to structural bias without abandoning their professional identity. The identity lock is cognitive rather than material: the actor could physically exit institutional pathways but cannot do so without rejecting the meritocratic legitimacy that constitutes their professional self-concept. Aspiring professionals and reform coalitions see the hybrid (Tangled Rope) — genuine coordination function entangled with systematic extraction. The analytical observer confirms the Tangled Rope classification at civilizational scope: some evaluation mechanism is structurally necessary (coordination floor), but the specific meritocratic implementation produces extractive overhead through error type divergence. The piton perspective (legacy institutions) reveals that the coordination function itself is degrading: credential signals no longer reliably indicate competence, yet the performative rituals intensify.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural relationship to the error distribution. Institutional evaluators are primary beneficiaries: they benefit from Type I error bias (false positives on credentialed candidates validate their evaluation authority) and from the coordination narrative (meritocratic legitimacy). Their arbitrage exit option and institutional power produce low directionality — extraction flows toward them. Marginalized analytical actors are primary victims: they bear the cost of Type II errors (false negatives on their contributions) and cannot exit without identity dissolution. Their powerless status and identity_locked exit produce high directionality — extraction flows away from them. Aspiring professionals occupy a middle position: they face extraction during credential acquisition (gatekeeping costs, signaling requirements) but become beneficiaries once credentialed. Their moderate power and constrained exit produce intermediate directionality. The epistemic commons (abstract collective good of accurate competence allocation) is a victim with no advocate and no exit — maximum directionality. Reform coalitions have organized power and mobile exit, producing lower directionality despite their victim status — they can build alternatives. Legacy institutions have institutional power but are constrained rather than arbitrage because declining credential signal value threatens their legitimacy — they cannot easily exit the meritocratic frame they constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope classification at the analytical level is structurally correct: the coordination function (competence allocation standards) is genuine and necessary, while the extraction mechanism (error type divergence by power position) is also real and systematic. This is not a case of mislabeling pure coordination as extraction, nor of mislabeling pure extraction as coordination. The constraint genuinely coordinates (provides evaluation standards, enables competence signaling, reduces information asymmetry in labor markets) AND genuinely extracts (systematically produces Type I errors favoring institutional positions and Type II errors burdening marginalized positions). The perspectival gap between institutional evaluators (Rope) and marginalized actors (Snare) is not a measurement error but a structural feature: the error distribution is asymmetric, so different positions experience different extraction levels from the same constraint. The Cassandra character (marginalized analytical actor with superior analytical capacity who is systematically disbelieved) is the diagnostic signature: if meritocratic ideology were pure coordination, analytical capacity would correlate with institutional recognition. The fact that it does not — that actors can possess the analytical capacity to see structural errors while being denied institutional credibility — reveals the extraction mechanism. The constraint's increasing theater ratio (0.42 → 0.68 over 45 years) shows lifecycle drift: the coordination function is degrading (credential signals decoupling from competence) while the performative rituals intensify, consistent with Goodhart dynamics and piton formation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    error_type_measurement_basis,
    'What constitutes the ground truth for measuring Type I vs Type II error rates in competence evaluation?',
    'Longitudinal outcome tracking: do initially rejected candidates demonstrate competence through alternative pathways? Do initially accepted candidates maintain performance? Requires multi-decade tracking and agreement on competence observables.',
    'If no ground truth exists: the constraint may be unfalsifiable (conceptual rather than empirical). If ground truth exists but is inaccessible: the extraction mechanism is real but unmeasurable from within the system. If ground truth is accessible: error rates become empirically testable and the ideology''s extractiveness can be quantified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(error_type_measurement_basis, conceptual, 'Ground truth basis for competence evaluation error rates').

omega_variable(
    coordination_floor_threshold,
    'What is the minimum extractiveness inherent to any competence evaluation system, below which we are measuring coordination cost rather than extraction?',
    'Cross-cultural comparison of evaluation systems with different ideological frames (meritocratic vs kinship-based vs lottery-based allocation); identification of shared error rate floors across systems; Boltzmann analysis of information-theoretic limits on evaluation accuracy.',
    'If floor is near current extractiveness (0.48): most of measured extraction is actually coordination cost, and the constraint is closer to Rope than Tangled Rope. If floor is substantially lower (< 0.20): current system has significant extractive overhead beyond coordination needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_threshold, empirical, 'Minimum extractiveness floor for evaluation systems').

omega_variable(
    identity_lock_vs_structural_trap,
    'For marginalized analytical actors, is the binding mechanism primarily cognitive (identity fusion with meritocratic frame) or material (actual barriers to alternative pathways)?',
    'Comparative exit analysis: do actors who break the identity frame (reject meritocratic legitimacy) gain access to alternative pathways, or do material barriers persist? Interview data on actors who attempted exit: what proportion cite cognitive vs material obstacles?',
    'If primarily cognitive: identity_locked classification is correct and the constraint operates through internalized ideology. If primarily material: trapped classification is more accurate and the ideology is epiphenomenal to structural barriers. If mixed: the constraint operates through both mechanisms and extractiveness may be higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Cognitive vs material binding mechanism for marginalized actors').

omega_variable(
    cassandra_analytical_capacity,
    'Do marginalized analytical actors actually possess superior analytical capacity (Cassandra character: sees truth but is not believed), or does the meritocratic frame produce a selection effect where only those with exceptional capacity persist in marginalized positions?',
    'Controlled comparison of analytical output quality across power positions, blinded to author identity; tracking of initially marginalized actors who gain institutional position (does analytical quality change or remain constant?); examination of historical cases where marginalized analytical claims were later validated.',
    'If Cassandra character is real: the constraint produces systematic epistemic loss (Type II errors on high-value contributions). If selection effect: marginalized actors are not systematically more capable, and the extraction is distributive (positional) rather than epistemic. If both: the constraint both selects for resilient analytical actors AND produces epistemic loss.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cassandra_analytical_capacity, empirical, 'Whether marginalized position correlates with analytical capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meritocratic_ideology_as_error_propagation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(merit_err_tr_t0, meritocratic_ideology_as_error_propagation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(merit_err_tr_t15, meritocratic_ideology_as_error_propagation, theater_ratio, 15, 0.55).
narrative_ontology:measurement(merit_err_tr_t30, meritocratic_ideology_as_error_propagation, theater_ratio, 30, 0.62).
narrative_ontology:measurement(merit_err_tr_t45, meritocratic_ideology_as_error_propagation, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(merit_err_be_t0, meritocratic_ideology_as_error_propagation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(merit_err_be_t15, meritocratic_ideology_as_error_propagation, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(merit_err_be_t30, meritocratic_ideology_as_error_propagation, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(merit_err_be_t45, meritocratic_ideology_as_error_propagation, base_extractiveness, 45, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meritocratic_ideology_as_error_propagation, identity_coordination).
narrative_ontology:boltzmann_floor_override(meritocratic_ideology_as_error_propagation, 0.08).
narrative_ontology:affects_constraint(meritocratic_ideology_as_error_propagation, structural_position_constraint_divergence).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural_position_constraint_divergence (the mountain-level claim that different structural positions produce different constraint experiences). The upstream constraint establishes that perspectival divergence is a general feature of indexical classification; this constraint instantiates that divergence in the specific domain of meritocratic evaluation systems. The error type divergence (Type I bias for institutional positions, Type II bias for marginalized positions) is a concrete mechanism through which structural position determines constraint experience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meritocratic_ideology_as_error_propagation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
