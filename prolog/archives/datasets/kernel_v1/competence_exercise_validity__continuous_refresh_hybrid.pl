% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Competence Exercise Validity: Continuous Refresh Hybrid (Process-Dependent Retention)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The continuous-refresh hybrid reading asserts that competence retention
 *   is fundamentally process-dependent, not state-validated. A person
 *   certified as competent at time T0 is NOT guaranteed to remain competent
 *   at time T0+N years without intervening practice. This reading occupies
 *   the middle ground between two extreme positions: the
 *   real-catastrophe-only reading (only genuine high-stakes events exercise
 *   true competence) and the simulation-as-proxy reading (drills are
 *   sufficient proxy exercises that maintain competence). The
 *   continuous-refresh hybrid reading claims that empirical safety records
 *   demonstrate that regular drill cycles prevent both the atrophy that would
 *   occur under no-cycle regimes AND the false confidence that
 *   simulation-only cycles can induce. The constraint exhibits extraction
 *   from operators (mandatory cycle burden) alongside genuine coordination
 *   benefit (safety outcomes). Extractiveness has risen over the measurement
 *   interval (0.38 → 0.52) as regulatory scope has expanded and cycle
 *   frequency has increased, while theater_ratio has similarly increased
 *   (0.42 → 0.58), indicating that administrative overhead and
 *   compliance-signaling have grown faster than genuine competence testing
 *   content.
 *
 * KEY AGENTS:
 *   - Operators: Primary targets (powerless/trapped) — bear mandatory cycle burden; cannot exit without losing licensure
 *   - Safety-Conscious Organizations: Mixed victims/beneficiaries (moderate/constrained) — benefit from demonstrably improved emergency response but bear infrastructure and scheduling costs
 *   - Regulatory Authority: Primary beneficiary (institutional/arbitrage) — gains compliance-verification mechanism and liability decoupling without operational consequence
 *   - Competency Assessment Coalition: Organized innovators (organized/mobile) — developing individualized competency models that could sunset the uniform-cycle requirement
 *   - Certification-Training Industry: Secondary beneficiary (institutional/arbitrage) — receives guaranteed contract flow from mandate; maintains through inertia despite degraded function
 *   - Analytical Observer: Civilization-level view (analytical/analytical) — risks naturalizing process-dependence as fixed-state law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.52).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.48).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Competence Exercise Validity: Continuous Refresh Hybrid (Process-Dependent Retention)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '11cbce49-7652-4037-b5dc-bd6c9baaf33d').
narrative_ontology:cs_kernel_codification('11cbce49-7652-4037-b5dc-bd6c9baaf33d', formalized).
narrative_ontology:cs_authority_grounding('11cbce49-7652-4037-b5dc-bd6c9baaf33d', expertise).
narrative_ontology:cs_interpretation_layer_present('11cbce49-7652-4037-b5dc-bd6c9baaf33d').
narrative_ontology:cs_reading_relation('11cbce49-7652-4037-b5dc-bd6c9baaf33d', competence_exercise_validity__simulation_as_proxy, influences).
narrative_ontology:cs_reading_relation('11cbce49-7652-4037-b5dc-bd6c9baaf33d', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('11cbce49-7652-4037-b5dc-bd6c9baaf33d', foundational, procedural_skill_decay_empirically_demonstrable).
narrative_ontology:cs_axiom_status(procedural_skill_decay_empirically_demonstrable, holdable).
narrative_ontology:cs_axiom_grounding('11cbce49-7652-4037-b5dc-bd6c9baaf33d', procedural_skill_decay_empirically_demonstrable, empirically_contingent).
narrative_ontology:cs_axiom('11cbce49-7652-4037-b5dc-bd6c9baaf33d', secondary, uniform_cycle_frequency_is_crude_proxy).
narrative_ontology:cs_axiom_status(uniform_cycle_frequency_is_crude_proxy, holdable).
narrative_ontology:cs_axiom_grounding('11cbce49-7652-4037-b5dc-bd6c9baaf33d', uniform_cycle_frequency_is_crude_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('11cbce49-7652-4037-b5dc-bd6c9baaf33d', skill_decay_empiricism).
narrative_ontology:cs_drift_state('11cbce49-7652-4037-b5dc-bd6c9baaf33d', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11cbce49-7652-4037-b5dc-bd6c9baaf33d', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operational_organizations).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, competent_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, public_safety_record).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATOR (SNARE) — Trapped in the continuous drill requirement. Cannot opt out of recertification cycles without losing operational license. Extraction: time and cognitive load from mandatory cycles that may exceed functional necessity. Suppression: regulatory mandate backed by legal penalty for non-compliance. No exit option within the system.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY-CONSCIOUS ORGANIZATION (TANGLED ROPE) — Both benefits from and bears costs of continuous-cycle requirement. Genuine coordination function: continuous drills demonstrably improve emergency response and prevent skill atrophy (the safety benefit is real). Asymmetric extraction: organization must maintain drill infrastructure and operator scheduling overhead at fixed cost per cycle, regardless of demonstrated operator competence. Constrained exit: could theoretically lobby for relaxed standards but faces reputational and liability barriers.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Primary beneficiary. Experiences continuous-cycle mandate as pure coordination: establishes measurable compliance signal (recertification completion) that decouples the regulator from liability for operator competence failures. Arbitrage exit: can revise standards without operational consequence. Net position: benefits from clear compliance metric and liability shield.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASSESSMENT INNOVATION COALITION (SCAFFOLD) — Organized agents (safety researchers, advanced simulation developers, competency psychologists) see the fixed-cycle requirement as a temporary proxy awaiting better alternatives. They are building competency-retention models that would replace one-size-fits-all cycles with individualized refresh schedules based on actual skill decay curves and operator-specific performance data. Mobile exit through technical innovation: if these models mature and gain regulatory acceptance, the uniform cycle sunset becomes feasible. Theater component: current cycles contain performative elements (checking boxes vs. testing competence) that newer assessment methods could eliminate.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CERTIFICATION-TRAINING INDUSTRY (PITON) — Institutional beneficiary with degraded primary function. The industry's activity (delivering mandated drills) was originally justified as ensuring competence. But the mechanism has become largely performative: pass-through certification that signals compliance to regulators, not genuine competence testing. Theater ratio: 0.70+ — most cycle time is administrative (form-filling, attendance logging) rather than testing actual emergency response. The industry persists through inertia and regulatory coupling, not because drills have demonstrable superior efficacy vs. alternative assessment methods.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPETENCE-AS-FIXED-STATE (MOUNTAIN) — From a civilizational view, competence could be treated as a validated state property: once an operator demonstrates mastery (through initial certification), the competence persists as a fixed capability. From this perspective, continuous drills appear to defy an immutable truth about human learning — that mastery, once achieved, is stable. However, this reading is a false summit. Empirical evidence contradicts the premise: operator performance in actual emergencies correlates strongly with recent drill frequency; skills genuinely decay without practice; initial certification predicts nothing about current capability. The mountain reading naturalizes what is actually a process-dependent fact.
constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_exercise_validity__continuous_refresh_hybrid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, TR),
    TR >= 0.70.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The continuous-cycle requirement extracts operator time and cognitive load for mandatory drill participation. Initial extractiveness (0.38) reflected the genuine coordination benefit when cycles were less frequent and more competence-focused. Current extractiveness (0.52) reflects accumulation: regulatory scope has expanded, cycle frequency increased, and administrative burden has grown faster than competence-testing content. The value reflects that genuine safety coordination exists (preventing atrophy) alongside genuine extraction (over-cycle burden that exceeds minimum necessary refresh). Suppression (0.48): Moderate. The requirement is enforced via regulatory mandate backed by legal penalties (loss of licensure), but not via physical coercion. Operators can theoretically lobby for standard relaxation, though reputational and liability barriers make this costly. Theater ratio (0.58): Moderate-high and rising. Drill content includes genuine competence testing (emergency scenarios, decision-making under stress) but increasingly padded with administrative compliance-signaling (attendance logging, form completion, check-box audits). The rising trajectory reflects that bureaucratic overhead has grown faster than scenario fidelity, indicating Piton-type degradation in the certification-training industry's function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence from the operator position (Snare: maximum extraction, no exit, full suppression) to the regulatory authority position (Rope: pure coordination, arbitrage exit, beneficiary). The organization position shows the constraint as mixed-benefit tangled rope: genuine safety coordination alongside extraction burden. The assessment coalition sees it as temporary (Scaffold) — the fixed-cycle regime is a placeholder awaiting better assessment science. The certification industry sees its own function as degraded (Piton) — maintenance through regulatory coupling rather than demonstrated efficacy. The analytical observer risks the false summit (Mountain) — treating process-dependence as if it were a fixed law of nature rather than an empirical fact about skill decay. This perspectival distribution reveals the constraint's structure: the uniform-cycle requirement is a coordination solution layered over with extraction that could be substantially reduced through better assessment methods.
 *
 * DIRECTIONALITY LOGIC:
 *   Operator position (powerless/trapped): d ≈ 0.95. Trapped exit means no structural path out; full target of extraction. The regulatory mandate is non-negotiable. Experienced extractiveness chi is high. Organization position (moderate/constrained): d ≈ 0.50. Mixed beneficiary (safety outcomes) and victim (cycle costs); constrained exit (could theoretically relax but face liability/reputation barriers). Regulatory authority (institutional/arbitrage): d ≈ 0.05. Pure beneficiary; arbitrage exit (can revise standards without operational consequence). Low chi — extraction flows toward them. Assessment coalition (organized/mobile): d ≈ 0.35. Mobile exit through technical innovation (new assessment models); somewhat victim (excluded from current mandate structure) but with path to reshape it. Moderate-low chi. Certification industry (institutional/arbitrage): d ≈ 0.10. Pure beneficiary; arbitrage exit through regulatory lobbying. Low chi but stable contract flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that both the coordination function (preventing skill atrophy) and the extraction mechanism (operator burden, regulatory liability-shifting) are real and coexist. The continuous-refresh-hybrid reading asserts that the coordination is necessary — skill decay is empirically demonstrable — while rejecting both extremes: real-catastrophe-only (unacceptable risk tolerance) and simulation-as-proxy (undercounting true exercise need). The measurement trajectory (rising extractiveness and theater ratio) indicates that the extraction component is growing faster than the coordination component, suggesting the constraint is drifting toward Piton status or pure Snare. The false summit detection (mountain perspective) is crucial: if the mountain reading (competence-as-fixed-state) were correct, the entire constraint would be misguided. But empirical evidence contradicts it — skill decay IS real — so the mountain is false. The true mandatrophy is between two readings that the continuous-refresh-hybrid must navigate: Can we maintain the genuine safety coordination (skill retention) while reducing the extraction overhead? The assessment coalition's scaffold perspective suggests yes — individualized competency models could maintain safety outcomes at lower operator burden. The resolution path is not to abandon drills but to optimize them to the empirical decay curve rather than the current one-size-fits-all uniform cycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_decay_measurement_ambiguity,
    'How do we distinguish genuine skill decay (loss of procedural capability) from performance decay (stress response, situational decision-making) in high-consequence environments?',
    'Longitudinal operator performance data: track operators with varying inter-drill intervals; measure actual emergency response times, error rates, and decision quality as function of time since last drill; distinguish performance variance attributable to skill loss vs. environmental/situational factors.',
    'If decay is primarily skill-based: continuous cycles are necessary (constraint remains Tangled Rope/Snare). If decay is primarily performance/stress-based: skill-decay argument fails and cycles are revealed as extraction mechanism (constraint shifts toward pure Snare or Piton). If decay is mixed: the proportional split determines whether continuous cycles are proportionally justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_decay_measurement_ambiguity, empirical, 'Whether observed performance decay is skill loss or stress/performance decay').

omega_variable(
    simulation_sufficiency_boundary,
    'At what fidelity and complexity does simulation approach sufficiency as competence exercise? Does this boundary exist, or is real-world catastrophe irreducible?',
    'Competency transfer studies: measure skill retention after scenario-based simulation vs. real-emergency exposure; test whether operators trained on high-fidelity simulation show equivalent performance in actual high-consequence situations; identify specific failure modes in simulation that don''t appear in real emergencies.',
    'If simulation approaches sufficiency at achievable fidelity: continuous cycles could be satisfied by periodic high-fidelity simulation (relocation of constraint, not elimination). If simulation fundamentally insufficient: only real catastrophe or sufficiently catastrophic mock-emergency can sustain competence (this reading forecloses simulation_as_proxy reading). If simulation shows asymptotic approach: there is a discontinuity between simulation adequacy and real-catastrophe necessity, and the choice between them is value-dependent, not factual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_sufficiency_boundary, empirical, 'Whether simulation sufficiency boundary exists for competence exercise').

omega_variable(
    cycle_frequency_optimization_frontier,
    'Is there an optimal inter-drill interval that minimizes both skill decay and excessive cycle burden, or does the tradeoff have no single optimum?',
    'Competency decay models: empirically identify skill-retention curves for domain-specific high-consequence tasks (emergency response, flight crew procedures, surgical technique, reactor operation); determine the inter-practice interval at which decay falls below some risk threshold; test whether this interval varies by operator, by task complexity, or by domain.',
    'If sharp optimum exists and is narrower than current uniform intervals: continuous-cycle requirement is justified but oversized (constraint becomes Rope/Tangled Rope; cycles are necessary coordination). If optimum is wider than current cycles: current requirement is extraction (constraint becomes Snare). If no single optimum (highly individual or task-dependent): uniform cycles are crude proxy that over-regulate some operators and under-protect others (constraint degrades toward Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cycle_frequency_optimization_frontier, empirical, 'Whether optimal inter-drill interval exists and how it compares to current mandates').

omega_variable(
    reading_contest__continuous_vs_real_vs_proxy,
    'Is this constraint one instantiation of a deeper contest over what ''counts as competence exercise''? How do the three readings (continuous_refresh_hybrid, real_catastrophe_only, simulation_as_proxy) relate structurally?',
    'Kernel analysis: the three readings hold incompatible assumptions about competence retention. Continuous_refresh_hybrid (this reading): process-dependent, exercises must be frequent and regular. Real_catastrophe_only: only high-stakes real events test true competence; simulation and drills are insufficient substitutes. Simulation_as_proxy: simulation counts as valid exercise; drills satisfy the requirement because they proxy catastrophe. Resolve by examining which reading''s assumption is empirically defensible (the omega above on simulation_sufficiency_boundary) and which reading''s authority grounding is operative (regulatory practice, safety science, or operator culture?).',
    'If continuous_refresh_hybrid assumption is correct (skill decay is real and process-dependent): this reading stands; real_catastrophe_only is shown to accept unacceptable risk; simulation_as_proxy undercounts true exercise need. If real_catastrophe_only assumption is correct: continuous cycles are revealed as futile theater; only high-stakes incidents retain true competence (reading coexists with continuous reading through observer disagreement). If simulation_as_proxy assumption is correct: drills satisfy competence requirement through proxy mechanism; continuous cycles are unnecessary extraction on operators (this reading forecloses simulation_as_proxy). These outcomes are mutually exclusive for a SINGLE FRAMEWORK; different parties can hold them simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest__continuous_vs_real_vs_proxy, conceptual, 'Structural relationship among three readings of the competence_exercise_validity kernel').

omega_variable(
    false_summit_competence_as_state,
    'Is the mountain perspective (competence-as-validated-state) a genuine natural law of human learning, or a false summit that naturalizes what is actually a process-dependent fact?',
    'Empirical evidence: if actual operator performance in real emergencies correlates strongly with recent drill frequency, the mountain is false (skill decay is empirically observable). If operators maintain competence indefinitely post-certification without further practice, the mountain may be real (but empirical evidence strongly contradicts this). The mountain reading assumes competence is like a diploma — once earned, it stays earned. Psychological and neuroscience evidence contradicts this: procedural skills decay without practice; stress-response patterns degrade; muscle memory fades; decision trees atrophy.',
    'If false summit is confirmed: the mountain perspective is a rationalization used to argue against continuous-cycle requirements and lower training burden on operators. It naturalizes the (extractive) desire to minimize training load as if it were a fact about competence. If mountain is somehow correct: the entire continuous-cycle requirement is misguided; initial certification would suffice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_competence_as_state, empirical, 'Whether competence-as-fixed-state is a genuine natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cevcrh_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cevcrh_tr_t5, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cevcrh_tr_t10, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cevcrh_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cevcrh_be_t5, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cevcrh_be_t10, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cevcrh_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cevcrh_su_t5, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(cevcrh_su_t10, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel: competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy, and competence_exercise_validity__real_catastrophe_only. All three are sisters, not parent-child. The continuous_refresh_hybrid reading claims empirical support from safety records showing drill-cycle effectiveness. It both influences (creates pressure on) and coexists with the other readings: influences by asserting empirical grounding for continuous cycles, coexists because different stakeholders hold different readings simultaneously (regulator, training industry, operators, safety researchers occupy different epistemic positions). The network links these readings so the constraint engine can track their interdependence and identify empirical resolution points (the omegas above).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__continuous_refresh_hybrid, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
