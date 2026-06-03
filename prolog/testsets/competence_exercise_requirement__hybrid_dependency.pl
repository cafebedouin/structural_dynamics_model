% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Competence Exercise Requirement: Hybrid Simulation-Real-World Dependency
 *   domain: safety_engineering/organizational_learning/high_reliability_operations
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_dependency reading of a contested
 *   kernel about how competence is maintained in safety-critical aviation
 *   operations. The reading asserts that competence — the ability to handle
 *   non-routine situations, exercise judgment under uncertainty, and recover
 *   from failures — requires BOTH a foundation of high-fidelity simulation
 *   AND periodic anchoring in real-world operations with irreducible
 *   uncertainty. Pure simulation creates a fragile equilibrium vulnerable to
 *   simulator-to-real-world transfer failures; pure real-world exercise
 *   (catastrophe-driven training) is ethically unjustifiable and practically
 *   impossible to manage. The hybrid regime — combining simulation training,
 *   non-jeopardy line audits, and actual aircraft time — bridges the
 *   epistemological gap by providing controlled competence measurement
 *   (simulation + audits) alongside irreducible reality testing (real
 *   aircraft operations). The constraint exhibits tangled_rope structure: it
 *   enforces genuine coordination (maintaining competence across the aviation
 *   system) while imposing asymmetric extraction (costs concentrated on
 *   pilots and operators). The constraint's extractiveness has risen over 20
 *   years (0.38 → 0.52) as simulation has become more expensive and
 *   comprehensive, increasing the total training burden. Theater ratio has
 *   also risen (0.48 → 0.64), indicating that an increasing portion of the
 *   real-aircraft component is conducted under controlled conditions designed
 *   to minimize risk — the 'real world' is becoming more scripted, reducing
 *   the authenticity of the anchoring function.
 *
 * KEY AGENTS:
 *   - Line Pilots: Primary target (powerless/trapped) — must complete both simulation and real aircraft requirements; cannot exit the constraint
 *   - Regional Airline Operators: Victim and partial beneficiary (moderate/constrained) — bear training costs but benefit from safety assurance; constrained by regulatory mandates but with some scheduling control
 *   - Safety Regulatory Authority: Primary beneficiary (institutional/arbitrage) — benefits from constraint as coordination mechanism; has power to adjust balance between simulation and real components
 *   - Flight Training Organizations: Beneficiary and enforcer (organized/constrained) — profit from training requirement but constrained by regulatory specifications for real aircraft time
 *   - Pilot Unions / Crew Resource Coalitions: Organized defender (organized/mobile) — have sufficient power to resist unilateral extraction; benefit from safety culture protection
 *   - Safety Culture Institutions: Implicit beneficiary — benefit from constraint as a legible, measurable competence assurance system
 *   - Analytical Observer: Sees natural law vs. contingent arrangement tension — risks naturalizing regulatory choice as cognitive necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.52).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.58).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Competence Exercise Requirement: Hybrid Simulation-Real-World Dependency").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_operations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '75b21a63-475f-4649-be50-b72579f4e1b6').
narrative_ontology:cs_kernel_codification('75b21a63-475f-4649-be50-b72579f4e1b6', formalized).
narrative_ontology:cs_authority_grounding('75b21a63-475f-4649-be50-b72579f4e1b6', expertise).
narrative_ontology:cs_interpretation_layer_present('75b21a63-475f-4649-be50-b72579f4e1b6').
narrative_ontology:cs_reading_relation('75b21a63-475f-4649-be50-b72579f4e1b6', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('75b21a63-475f-4649-be50-b72579f4e1b6', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_axiom('75b21a63-475f-4649-be50-b72579f4e1b6', foundational, irreducible_uncertainty_requirement).
narrative_ontology:cs_axiom_status(irreducible_uncertainty_requirement, holdable).
narrative_ontology:cs_axiom_grounding('75b21a63-475f-4649-be50-b72579f4e1b6', irreducible_uncertainty_requirement, empirically_contingent).
narrative_ontology:cs_axiom('75b21a63-475f-4649-be50-b72579f4e1b6', foundational, catastrophe_ethically_unjustifiable).
narrative_ontology:cs_axiom_status(catastrophe_ethically_unjustifiable, holdable).
narrative_ontology:cs_axiom_grounding('75b21a63-475f-4649-be50-b72579f4e1b6', catastrophe_ethically_unjustifiable, deontological).
narrative_ontology:cs_reference_frame('75b21a63-475f-4649-be50-b72579f4e1b6', hybrid_competence_maintenance_regime).
narrative_ontology:cs_drift_state('75b21a63-475f-4649-be50-b72579f4e1b6', contemporary_high_automation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75b21a63-475f-4649-be50-b72579f4e1b6', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, safety_culture_institutions).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flight_training_organizations).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, pilot_training_cost_burden).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, operational_efficiency).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, crew_schedule_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINE PILOT (SNARE) — Trapped in the hybrid requirement with no exit. Must accumulate both simulation hours AND line operations time to maintain competence certification. The constraint extracts time, financial cost (training expenses), schedule disruption, and cognitive load without proportional benefit to the individual pilot. The pilot cannot argue for simulation-only or real-world-only — the hybrid is legally mandated. Maximum experienced extraction because exit is legal and structural impossibility.
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL AIRLINE OPERATOR (TANGLED ROPE) — Constrained by regulation but also benefits from the competence assurance the hybrid regime provides. The airline benefits from reduced accident risk (real customer trust, reduced liability exposure, regulatory compliance). But the airline bears significant costs: pilot training expenses, scheduling complexity, operational inefficiency from pilots in recurrent training. The constraint provides genuine coordination (assuring competence) alongside asymmetric extraction (cost concentration on operators and crews).
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY REGULATORY AUTHORITY (ROPE) — Benefits from the hybrid regime as a mechanism for maintaining operational safety culture without requiring catastrophic events. The authority has arbitrage — it can shift the balance between simulation and real-world components through regulation. The constraint functions primarily as coordination: it solves the collective action problem of maintaining competence across the global aviation system. The authority experiences this as a pure coordination mechanism, not extraction.
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FLIGHT TRAINING INDUSTRY (TANGLED ROPE) — Organized institutional actor that benefits from the hybrid requirement (guaranteed training market, high-fidelity simulator demand) but is constrained by the regulatory mandates on minimum real-aircraft hours. The industry extracts revenue from the requirement while also being forced to provide the real-world anchoring component at cost. The constraint creates coordination (ensuring competence) and extraction (training revenue streams and operational costs).
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL COMPETENCE VERIFICATION RITUAL (PITON) — The hybrid regime has become substantially performative: many real-aircraft audits and line operations checks are conducted under controlled conditions designed to minimize risk and maximize measurement. The authentic challenge of unscripted real-world decision-making is substantially absent. The system persists because it maintains the institutional appearance of competence assurance, but much of its function is theatrical — the ritual of 'real world' operations without the irreducible uncertainty that makes real-world experience competence-building. Theater ratio reflects this degradation: the constraint is maintained by institutional inertia rather than demonstrated efficacy.
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the hybrid requirement reflects an immutable property of human competence: people cannot maintain complex decision-making skills through simulation alone because simulation lacks the irreducible uncertainty and consequence-binding of real performance. The constraint appears as a natural law of learning psychology and cognitive neuroscience. However, the structural data reveals this as a false summit candidate — the requirement is contingent on specific training regimes and regulatory choices, not on universal cognitive architecture.
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PILOT UNION / CREW RESOURCE COALITION (ROPE) — Organized agents with mobile exit options (can advocate for regulatory change, coordinate across carriers, leverage collective bargaining). The union experiences the hybrid requirement as a coordination mechanism that protects crew safety and maintains professional standards. The coalition benefits from the requirement because it prevents cost-cutting corners on training. The constraint functions as coordination without significant extraction — the union has sufficient power to prevent unilateral extraction.
constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_exercise_requirement__hybrid_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, TR),
    TR >= 0.70.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes real costs on pilots (time, financial) and operators (training budget, scheduling complexity) without equivalent direct benefit to the individuals. However, extractiveness is not maximal (would be 0.72+) because genuine coordination function exists — the hybrid regime does maintain competence and reduce accident risk. The rise over 20 years (0.38 → 0.52) reflects increasing simulation comprehensiveness and cost, expanding the training burden. Suppression (0.58): Moderate-high. Pilots and operators have limited alternatives: simulation-only training lacks the real-world anchoring signal, and catastrophe-driven training is ethically and legally impossible. The barriers are structural (regulatory mandate) and practical (no substitute training regime has been validated). Suppression has risen slightly (0.52 → 0.58) as real aircraft costs have increased, making alternatives (pure simulation) more tempting but also more tightly prohibited. Theater ratio (0.64): Moderate-high. The real-aircraft component includes substantial performative elements: non-jeopardy audits are conducted under controlled conditions, checkrides are scheduled with known evaluators, line operations checkers are present (reducing real operational stress). Many pilots report that checkrides bear little resemblance to actual emergency decision-making under full operational uncertainty. The rise in theater (0.48 → 0.64) reflects increasing risk-aversion in the training system — more audit constraints, more scheduled checkpoints, less authentic unscripted operations. The constraint is tangled_rope (not snare) because coordination function is genuine and experienced: pilots do learn from both simulation and real operations, and the system does maintain safety.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates structural tension between perspectives. The line pilot sees snare (extraction without benefit). The airline operator sees tangled_rope (mixed coordination and cost). The regulator sees rope (pure coordination). The training industry sees tangled_rope (profit from requirement; constrained by real aircraft mandates). The traditional verification ritual sees piton (performative competence assessment). The analytical observer risks seeing mountain (immutable law of learning). The pilot union sees rope (coordination that protects collective interests). The perspectival gap reveals that the constraint is NOT a natural law but a regulatory choice with distributional consequences — benefiting safety culture institutions and training vendors while extracting costs from operators and crews. The gap between piton and rope perspectives is particularly diagnostic: if the real-aircraft component were genuinely functional (rope perspective), theater_ratio would be lower; the rising theater indicates the constraint is becoming more ritualistic (piton trajectory) even as its total cost increases (snare trajectory from pilot perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. Pilots (powerless/trapped) experience maximum d (~0.95) — they are structurally constrained targets with no exit, no alternative path to competence certification. Airline operators (moderate/constrained) experience moderate-high d (~0.65) — they bear costs but have some regulatory leverage and benefit from safety assurance. The regulator (institutional/arbitrage) experiences low d (~0.15) — a net beneficiary with power to adjust the constraint. The training industry (organized/constrained) experiences moderate d (~0.55) — benefits from the requirement but constrained by real aircraft mandates. The pilot union (organized/mobile) experiences low d (~0.35) — organized enough to resist unilateral extraction and benefit from safety culture protection. The analytical observer (analytical/analytical) experiences moderate d (~0.72) — sees the full structure but risks being captured by the 'natural law' framing. Chi (effective extractiveness) is scaled by f(d) for each perspective, producing the differential classification: the same base extractiveness (0.52) yields snare for trapped agents, rope for beneficiaries, tangled_rope for mixed agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the hybrid requirement is BOTH a genuine coordination mechanism AND an extractive regime, depending on the agent's structural position. The constraint prevents mislabeling it as pure rope (coordination without extraction) by including the cost/benefit asymmetry in the model. It prevents mislabeling it as pure snare (extraction without coordination) by recognizing the safety coordination function. The tangled_rope classification correctly captures the simultaneity: real competence maintenance (coordination) coupled with structural cost concentration (extraction). The piton and mountain perspectives represent alternative misclassifications: piton mistakes the growing theater (ritual performativity) for the loss of function; mountain mistakes the regulatory constancy for a law of nature. The union's rope perspective and the snare perspective of individual pilots are both structurally correct — they simply occupy different positions relative to the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_saturation_threshold,
    'At what point does accumulated high-fidelity simulation experience constitute sufficient exercise of competence such that real-aircraft time provides diminishing returns rather than essential anchoring?',
    'Longitudinal analysis of pilot error rates and recovery performance across simulation-heavy vs. hybrid-experienced cohorts; controlled comparison of pilot competence decay with different training mixtures; identification of simulator-to-real-world transfer degradation points.',
    'If threshold exists at moderate simulation levels: hybrid requirement could shift toward simulation-heavier with minimal real aircraft time (influences sibling reading simulation_as_adequate_exercise). If threshold is very high or absent: real aircraft time remains irreducible (supports this reading). If threshold varies by competence domain: separate constraints needed (ε-invariance decomposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_saturation_threshold, empirical, 'Simulation saturation threshold for competence maintenance').

omega_variable(
    unscripted_uncertainty_necessity,
    'Is the irreducible uncertainty and consequence-binding of actual line operations structurally necessary for maintaining competence, or is uncertainty-simulation (high-fidelity random scenario generation) sufficient?',
    'Comparison of error recovery rates in genuinely unscripted situations vs. randomized simulation scenarios; analysis of pilot cognitive load and decision-making under novel vs. previously-encountered threats; longitudinal tracking of competence decay patterns for simulation-only vs. hybrid-trained cohorts.',
    'If unscripted uncertainty IS necessary: catastrophe_as_necessary_anchor reading gains force (periodic exposure to real operational unpredictability required). If simulation uncertainty suffices: simulation_as_adequate_exercise reading is supported. If both work but via different mechanisms: both readings coexist and this reading''s hybrid model is empirically justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unscripted_uncertainty_necessity, empirical, 'Whether unscripted real-world uncertainty is necessary or simulable').

omega_variable(
    knowledge_transfer_failure_modes,
    'What categories of pilot error or competence failure are NOT prevented by the current hybrid regime (simulation + non-jeopardy audits + line operations)?',
    'Root cause analysis of pilot-error accidents and serious incidents; identification of error categories that survived the hybrid training and certification regime; correlation of failure categories with training exposure patterns (which pilots trained primarily in simulation vs. hybrid vs. catastrophe-experienced).',
    'If significant failure categories exist despite hybrid training: may indicate the regime''s theater_ratio is high (many audits are non-jeopardy, missing real-world triggers) and a shift toward catastrophe_as_necessary_anchor reading is warranted. If failure categories are rare or unpredictable: hybrid regime''s efficacy is supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transfer_failure_modes, empirical, 'Categories of pilot error not prevented by hybrid regime').

omega_variable(
    reading_contest_empirical_resolution,
    'Which of the three sibling readings (catastrophe_as_necessary_anchor, simulation_as_adequate_exercise, hybrid_dependency) is empirically supported by longitudinal safety data?',
    '30-year cohort analysis comparing accident rates, error types, and recovery performance across training regimes; identification of which training philosophy produces measurably better outcomes and lower-cost path to competence; international comparison of regulation-to-outcome relationships.',
    'This omega documents that the three readings are not equally valid empirically — one reading may foreclose the others, or all three may coexist with different validity domains (certain pilot types, certain aircraft classes, certain threat profiles). Current corpus treats this as unresolved contestation; empirical resolution would reclassify the kernel''s reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_resolution, empirical, 'Empirical resolution of the three competing readings of the competence exercise kernel').

omega_variable(
    regulatory_theater_extent,
    'To what extent are mandatory line operations and non-jeopardy audits performative compliance rather than functional competence exercise?',
    'Analysis of audit/check data: what proportion are conducted under controlled conditions (known scenarios, scripted checkpoints, risk-mitigation constraints) vs. genuinely unscripted line operations? Pilot interviews about perceived risk and cognitive load during audits vs. actual line operations. Comparison of error rates during checkrides vs. actual line performance.',
    'If theater is high (>0.65): the constraint''s real-aircraft component is largely performative, shifting classification toward Piton and supporting catastrophe_as_necessary_anchor reading. If theater is low (<0.45): real-aircraft requirement is genuinely functional, supporting hybrid_dependency. Theater ratio (0.64) suggests moderate performativity — some real exercising, significant ritual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_theater_extent, empirical, 'Extent to which real-aircraft audits are performative vs. functional').

omega_variable(
    hybrid_regime_reading_vs_natural_law,
    'Is the hybrid requirement a discovered natural law of competence maintenance, or a constructed institutional arrangement that benefits safety culture gatekeepers by making competence assessment legible and controllable?',
    'Historical analysis: has the hybrid requirement existed across different regulatory regimes and training philosophies, or is it specific to modern aviation post-1970s? Comparative analysis: how do non-aviation high-stakes domains maintain competence (surgery, military command, nuclear operations)? Do they converge on hybrid models or use different architectures? Logical analysis: is ''simulation + real anchoring'' a necessity derived from learning science, or a compromise satisfying multiple stakeholders (regulators want measurable competence, operators want cost control, pilots want safety)?',
    'If natural law: mountain classification is defensible, and the constraint''s beneficiaries are incidental. If constructed: false_summit_mountain signature fires; the constraint naturalizes a contingent arrangement that benefits safety culture institutions and training vendors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_regime_reading_vs_natural_law, conceptual, 'Whether hybrid requirement is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compex_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.48).
narrative_ontology:measurement(compex_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.58).
narrative_ontology:measurement(compex_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(compex_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(compex_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(compex_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(compex_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(compex_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(compex_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, pilot_workload_and_fatigue_management).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, training_cost_escalation__commercial_aviation).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulator_fidelity_development).

% DUAL FORMULATION NOTE:
% The competence exercise requirement has three structurally distinct readings with different ε values. catastrophe_as_necessary_anchor (ε~0.70, Snare) models competence as only achievable through real-world catastrophic failure exposure. simulation_as_adequate_exercise (ε~0.25, Rope) models competence as achievable through high-fidelity simulation alone. hybrid_dependency (ε~0.52, Tangled Rope — THIS reading) models competence as requiring both, with each reading representing a different institutional commitment about what competence is and how it is maintained. All three readings reference the same underlying phenomenon (pilot decision-making under uncertainty) but with different ε values because they instantiate different measurement regimes and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__hybrid_dependency, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
