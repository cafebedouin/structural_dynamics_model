% ============================================================================
% CONSTRAINT STORY: certainty_as_foreclosure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_certainty_as_foreclosure, []).

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
 *   constraint_id: certainty_as_foreclosure
 *   human_readable: Certainty as Inquiry-Terminating Mechanism
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   Psychological certainty functions as an inquiry-terminating mechanism
 *   across multiple domains: medical diagnosis, strategic planning,
 *   scientific research, legal judgment, and everyday decision-making. The
 *   constraint exhibits a genuine coordination function (enables decisive
 *   action under uncertainty, prevents infinite regress of doubt) alongside
 *   asymmetric extraction (forecloses inquiry precisely when revision would
 *   improve accuracy, with costs concentrated on those least able to reopen
 *   investigation). This is a diagnostic exemplar of Tangled Rope
 *   classification: the same mechanism that solves the stopping problem
 *   creates the foreclosure problem. The constraint is downstream of
 *   theory_laden_perception (mountain) and model_invisibility_mechanism
 *   (rope) — agents cannot recognize premature certainty from within because
 *   their perceptual access is already theory-filtered and their models are
 *   operationally invisible. The theater_ratio (0.38) reflects moderate
 *   performative content: confidence expression in professional contexts
 *   often serves signaling functions (projecting authority, enabling
 *   institutional coordination) distinct from actual epistemic warrant.
 *   Measurements show extraction accumulation over the interval as
 *   institutional incentives increasingly reward confident closure over
 *   epistemic humility.
 *
 * KEY AGENTS:
 *   - Misdiagnosed Patient: Primary victim (powerless/trapped) — bears full cost of foreclosed inquiry with no agency to reopen diagnostic investigation; extraction is maximal and one-directional
 *   - Junior Clinician: Secondary victim and beneficiary (moderate/constrained) — constrained by training norms rewarding confidence; benefits from cognitive efficiency but bears cost of diagnostic errors; mixed extraction
 *   - Healthcare System: Primary beneficiary (institutional/arbitrage) — captures throughput optimization gains; externalizes diagnostic error costs to patients and individual clinicians
 *   - Metacognitive Training Coalition: Organized agents (organized/mobile) — building calibration training, Bayesian reasoning curricula, diagnostic checklists; see both coordination and extraction; can exit through alternative paradigms
 *   - Expert Diagnostician: Powerful agent (powerful/mobile) — experiences certainty as temporary heuristic with implicit revision triggers; low extraction because expertise includes meta-knowledge of when to reopen inquiry
 *   - Cognitive Architecture Theorist: Analytical observer (analytical/analytical) — risks naturalizing trainable metacognitive pattern as immutable cognitive architecture; false summit perspective
 *   - Structural Analyst: Analytical observer (analytical/analytical) — canonical analytical context; sees genuine coordination function and asymmetric extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(certainty_as_foreclosure, 0.48).
domain_priors:suppression_score(certainty_as_foreclosure, 0.52).
domain_priors:theater_ratio(certainty_as_foreclosure, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(certainty_as_foreclosure, extractiveness, 0.48).
narrative_ontology:constraint_metric(certainty_as_foreclosure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(certainty_as_foreclosure, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(certainty_as_foreclosure, tangled_rope).
narrative_ontology:human_readable(certainty_as_foreclosure, "Certainty as Inquiry-Terminating Mechanism").
narrative_ontology:topic_domain(certainty_as_foreclosure, "epistemology/cognitive_science/philosophy_of_mind").

domain_priors:requires_active_enforcement(certainty_as_foreclosure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(certainty_as_foreclosure, cognitive_efficiency_systems).
narrative_ontology:constraint_beneficiary(certainty_as_foreclosure, decision_making_under_time_pressure).
narrative_ontology:constraint_beneficiary(certainty_as_foreclosure, institutional_authority_structures).
narrative_ontology:constraint_victim(certainty_as_foreclosure, epistemic_accuracy).
narrative_ontology:constraint_victim(certainty_as_foreclosure, error_correction_capacity).
narrative_ontology:constraint_victim(certainty_as_foreclosure, diagnostic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISDIAGNOSED PATIENT (SNARE) — Trapped within physician's premature diagnostic closure. Cannot exit the certainty frame that forecloses re-examination. Bears full cost of foreclosed inquiry with no agency to reopen investigation. Maximum extraction: certainty terminates inquiry precisely when revision is most needed.
constraint_indexing:constraint_classification(certainty_as_foreclosure, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: JUNIOR CLINICIAN (TANGLED ROPE) — Constrained by training norms that reward diagnostic confidence and penalize uncertainty expression. Benefits from cognitive efficiency (can process more cases) but bears cost when premature certainty forecloses accurate diagnosis. Mixed experience: the constraint coordinates clinical workflow while extracting from diagnostic accuracy.
constraint_indexing:constraint_classification(certainty_as_foreclosure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE SYSTEM (ROPE) — Benefits from throughput optimization. Certainty enables rapid case closure and resource allocation. Experiences constraint as coordination mechanism: confident diagnoses allow efficient patient flow and billing. Net beneficiary of the efficiency gains; diagnostic errors are externalized to patients and individual clinicians.
constraint_indexing:constraint_classification(certainty_as_foreclosure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: METACOGNITIVE TRAINING COALITION (TANGLED ROPE) — Organized agents (calibration training programs, Bayesian reasoning curricula, diagnostic checklists) see both coordination function (certainty enables action) and extraction mechanism (premature closure prevents correction). Can exit through alternative training paradigms but constrained by institutional inertia. Mixed extraction: building tools to preserve efficiency while reducing foreclosure.
constraint_indexing:constraint_classification(certainty_as_foreclosure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EXPERT DIAGNOSTICIAN (SCAFFOLD) — Powerful agents with pattern recognition expertise experience certainty as temporary heuristic with implicit sunset: initial confidence enables rapid triage, but expertise includes meta-knowledge of when to reopen inquiry. Sees the constraint as coordination tool with built-in revision triggers. Low extraction because exit is available when certainty proves premature.
constraint_indexing:constraint_classification(certainty_as_foreclosure, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From civilizational/universal perspective, certainty as inquiry termination appears as immutable feature of bounded rationality: finite cognitive resources require stopping rules, and subjective confidence is the brain's evolved mechanism for resource allocation. This perspective naturalizes the constraint as inherent to human cognition. However, the structural data contradicts mountain classification — the engine will compute this as false summit, revealing that 'inherent cognitive architecture' framing naturalizes what is actually a trainable metacognitive pattern with institutional reinforcement.
constraint_indexing:constraint_classification(certainty_as_foreclosure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL VIEW (TANGLED ROPE) — From generational/global analytical perspective, certainty functions as both coordination mechanism (enables decisive action under uncertainty) and extraction mechanism (forecloses inquiry when revision would improve accuracy). The constraint genuinely solves the infinite regress problem (must stop inquiry somewhere) while asymmetrically distributing costs (errors concentrate on those with least power to reopen investigation). Canonical analytical context for this constraint.
constraint_indexing:constraint_classification(certainty_as_foreclosure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(certainty_as_foreclosure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(certainty_as_foreclosure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(certainty_as_foreclosure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(certainty_as_foreclosure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(certainty_as_foreclosure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint genuinely solves the infinite regress problem (inquiry must terminate somewhere) but asymmetrically distributes error costs. Premature certainty forecloses inquiry when revision would improve accuracy, with costs concentrated on powerless agents (misdiagnosed patients, strategic failure victims) who cannot reopen investigation. The extraction is real but not maximal — some agents (experts, organized coalitions) can recognize and exit premature closure. Suppression (0.52): Moderate-high. Significant barriers to reopening foreclosed inquiry include institutional incentives rewarding confidence over humility, cognitive dissonance costs of admitting error, social costs of uncertainty expression in professional contexts, and metacognitive inaccessibility (certainty feels like knowledge from inside). But suppression is not total — calibration training, diagnostic checklists, and institutional reforms can reduce premature closure. Theater ratio (0.38): Moderate. Confidence expression in professional contexts serves genuine epistemic function (communicating degree of warrant) but also substantial signaling function (projecting authority, enabling coordination, satisfying institutional expectations). The performative component has increased over the interval as institutional pressures for decisive action have intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full indexical range from a single structural phenomenon. The misdiagnosed patient sees pure extraction (Snare) — certainty terminates inquiry when revision is most needed, with no exit option. The junior clinician sees mixed coordination and extraction (Tangled Rope) — benefits from efficiency but bears cost of diagnostic errors. The healthcare system sees coordination (Rope) — certainty enables throughput optimization with externalized error costs. The metacognitive training coalition sees mixed coordination and extraction (Tangled Rope) — building tools to preserve efficiency while reducing foreclosure. The expert diagnostician sees temporary support (Scaffold) — certainty as heuristic with implicit revision triggers. The cognitive architecture theorist sees immutable law (Mountain) — certainty as inherent feature of bounded rationality — but this is a false summit naturalizing trainable metacognitive patterns. The structural analyst sees the canonical Tangled Rope: genuine coordination function (solves stopping problem) with asymmetric extraction (forecloses inquiry when revision would improve accuracy).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cognitive_efficiency_systems, decision_making_under_time_pressure, institutional_authority_structures) experience low directionality — certainty enables rapid throughput, resource allocation, and authoritative communication. These agents capture the coordination gains while externalizing error costs. Victims (epistemic_accuracy, error_correction_capacity, diagnostic_reliability) experience high directionality — they bear the cost of foreclosed inquiry with minimal benefit from the efficiency gains. The misdiagnosed patient has maximum directionality (d ≈ 0.95): trapped within the certainty frame, cannot reopen investigation, bears full cost of premature closure. The junior clinician has moderate directionality (d ≈ 0.55): constrained by training norms but benefits from cognitive efficiency; mixed extraction. The healthcare system has low directionality (d ≈ 0.10): institutional beneficiary with arbitrage exit options; captures throughput gains. The expert diagnostician has low directionality (d ≈ 0.20): powerful agent with mobile exit; expertise includes meta-knowledge of when certainty is premature. The metacognitive training coalition has moderate directionality (d ≈ 0.45): organized agents building alternative paradigms; see both coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that certainty genuinely solves a coordination problem (the infinite regress of doubt, the need for decisive action under uncertainty) while simultaneously creating an extraction mechanism (premature foreclosure that prevents error correction). The coordination function is real: without stopping rules, inquiry never terminates and action becomes impossible. The extraction is also real: certainty forecloses inquiry precisely when revision would improve accuracy, with costs concentrated on those least able to reopen investigation. The constraint requires active enforcement (institutional incentives rewarding confidence, professional norms penalizing uncertainty expression, metacognitive inaccessibility making premature certainty self-concealing) and has clear beneficiaries (efficiency systems, authority structures) and victims (epistemic accuracy, error correction capacity). This is not coordination mislabeled as extraction, nor extraction mislabeled as coordination — it is both simultaneously, which is the defining feature of Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_trainability_threshold,
    'To what extent is overconfidence bias trainable vs. hardwired in cognitive architecture?',
    'Longitudinal studies of calibration training effectiveness across domains; neural plasticity research on confidence estimation circuits; cross-cultural variation in certainty expression norms',
    'If highly trainable: constraint is institutional/cultural (Tangled Rope from more perspectives, Scaffold sunset is real). If hardwired: constraint is cognitive architecture (Mountain from more perspectives, extraction is inherent cost of bounded rationality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_trainability_threshold, empirical, 'Whether overconfidence is trainable or hardwired cognitive architecture').

omega_variable(
    optimal_stopping_threshold,
    'What is the optimal balance between inquiry continuation cost and error cost across domains?',
    'Domain-specific cost-benefit analysis: medical diagnosis (cost of delayed treatment vs. misdiagnosis), strategic planning (cost of analysis paralysis vs. strategic error), scientific research (cost of premature publication vs. missed discovery)',
    'If optimal threshold is near current practice: extraction is minimal, constraint is primarily coordination (Rope from more perspectives). If optimal threshold requires substantially more inquiry: current practice is extractive (Snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimal_stopping_threshold, preference, 'Optimal inquiry continuation threshold varies by domain and value weights').

omega_variable(
    institutional_reinforcement_mechanism,
    'How much of observed premature certainty is individual cognitive bias vs. institutional incentive structure?',
    'Comparison of certainty expression and diagnostic accuracy across institutional contexts with different incentive structures (fee-for-service vs. salaried medicine, publish-or-perish vs. slow science, quarterly earnings pressure vs. long-term investment)',
    'If primarily institutional: constraint is policy-modifiable (Tangled Rope with clear intervention points). If primarily individual: constraint is cognitive architecture (Mountain or Rope depending on trainability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_reinforcement_mechanism, empirical, 'Relative contribution of individual bias vs. institutional incentives').

omega_variable(
    metacognitive_accessibility,
    'Can agents reliably detect when their certainty is premature, or is the foreclosure mechanism self-concealing?',
    'Metacognitive monitoring studies: correlation between subjective confidence and objective accuracy; effectiveness of confidence interval elicitation; neural correlates of metacognitive awareness',
    'If self-concealing: constraint operates as identity_locked mechanism (certainty feels like knowledge from inside). If accessible: constraint is constrained-exit (agents can recognize premature closure but face costs to reopen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_accessibility, empirical, 'Whether premature certainty is metacognitively accessible to the agent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(certainty_as_foreclosure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cert_fc_theater_initial, certainty_as_foreclosure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cert_fc_theater_early, certainty_as_foreclosure, theater_ratio, 3, 0.3).
narrative_ontology:measurement(cert_fc_theater_final, certainty_as_foreclosure, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(cert_fc_extract_initial, certainty_as_foreclosure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cert_fc_extract_early, certainty_as_foreclosure, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cert_fc_extract_final, certainty_as_foreclosure, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(certainty_as_foreclosure, identity_coordination).
narrative_ontology:boltzmann_floor_override(certainty_as_foreclosure, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is downstream of theory_laden_perception (mountain) and model_invisibility_mechanism (rope). Theory-laden perception means agents cannot recognize premature certainty from within because their perceptual access is already filtered by the theory they're certain about. Model invisibility means the certainty-generating mechanism is operationally invisible to the agent experiencing it. These upstream constraints explain why the foreclosure mechanism is partially self-concealing and why metacognitive accessibility (omega variable) is an empirical question rather than an obvious feature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(certainty_as_foreclosure, moderate, 0.55).
constraint_indexing:directionality_override(certainty_as_foreclosure, powerful, 0.2).
constraint_indexing:directionality_override(certainty_as_foreclosure, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
