% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Exercise-as-Competence-Maintenance: Hybrid Decay Reading
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'exercise-as-competence-maintenance.' The hybrid-decay reading asserts
 *   that simulation exercises can retain procedural competence (muscle
 *   memory, communication protocols, escalation sequences) but cannot
 *   maintain judgment capacity under genuine stakes (recognizing scenario
 *   divergence, improvising outside the simulated envelope, making
 *   irreversible decisions with true uncertainty). The kernel has two
 *   structurally distinct components — procedures (rehearsable via
 *   simulation) and judgment (requiring stakes-activation) — but the
 *   constraint treats them as a unitary whole. The organization enforces
 *   exercise compliance under the rubric that 'competence is maintained'
 *   without specifying which competence component is preserved and which
 *   decays. The constraint persists because it satisfies the regulatory
 *   requirement (exercises are conducted), maintains procedural competence
 *   (real benefit), and obscures the judgment-decay component (extractive
 *   benefit to the organization and infrastructure operators). Personnel
 *   exposed to judgment-failure scenarios, and publics downstream of degraded
 *   improvisation, bear the cost of the architectural asymmetry.
 *
 * KEY AGENTS:
 *   - organization_maintaining_compliance: Institutional agenda-setter. Designs and enforces the exercise regimen. Benefits from satisfying compliance while permitting judgment-decay risk. Extractive position.
 *   - simulation_infrastructure_operators: Organized beneficiary. Operate the systems, design scenarios, conduct reviews. Depend on the exercise regimen and the framing that simulation suffices. Concentrated benefit.
 *   - trained_crisis_responders: Dual-positioned (beneficiary of procedural coordination + payer of judgment-decay cost). Retain muscle memory; lose improvisation capacity between exercises.
 *   - personnel_exposed_to_judgment_failure_scenarios: Powerless victim. Harmed when responders' judgment fails because it was not rehearsed. Trapped in the scenario.
 *   - public_downstream_of_degraded_improvisation: Powerless victim class. Exposed to systemic judgment failures. Cannot exit.
 *   - regulatory_authority: Observer. Audits compliance with exercise requirements; typically does not measure judgment-retention.
 *   - alternative_validation_researchers: Excluded. Propose partition the kernel and design domain-specific validation. Excluded from the design conversation.
 *   - crisis_victims_from_procedural_failure: Excluded. Harmed when procedures fail entirely; heard only in post-incident inquiries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.42).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Exercise-as-Competence-Maintenance: Hybrid Decay Reading").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '9637a87d-bb44-4c54-9269-6b747f907f9e').
narrative_ontology:cs_kernel_codification('9637a87d-bb44-4c54-9269-6b747f907f9e', distributed).
narrative_ontology:cs_authority_grounding('9637a87d-bb44-4c54-9269-6b747f907f9e', extraction).
narrative_ontology:cs_interpretation_layer_present('9637a87d-bb44-4c54-9269-6b747f907f9e').
narrative_ontology:cs_reading_relation('9637a87d-bb44-4c54-9269-6b747f907f9e', exercise_as_competence_maintenance__simulation_sufficiency_reading, influences).
narrative_ontology:cs_reading_relation('9637a87d-bb44-4c54-9269-6b747f907f9e', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('9637a87d-bb44-4c54-9269-6b747f907f9e', foundational, judgment_capacity_requires_stakes_activation).
narrative_ontology:cs_axiom_status(judgment_capacity_requires_stakes_activation, holdable).
narrative_ontology:cs_axiom_grounding('9637a87d-bb44-4c54-9269-6b747f907f9e', judgment_capacity_requires_stakes_activation, empirically_contingent).
narrative_ontology:cs_axiom('9637a87d-bb44-4c54-9269-6b747f907f9e', secondary, procedures_rehearsable_via_simulation).
narrative_ontology:cs_axiom_status(procedures_rehearsable_via_simulation, holdable).
narrative_ontology:cs_axiom_grounding('9637a87d-bb44-4c54-9269-6b747f907f9e', procedures_rehearsable_via_simulation, empirically_contingent).
narrative_ontology:cs_axiom('9637a87d-bb44-4c54-9269-6b747f907f9e', foundational, kernel_partitionability_acknowledged).
narrative_ontology:cs_axiom_status(kernel_partitionability_acknowledged, holdable).
narrative_ontology:cs_axiom_grounding('9637a87d-bb44-4c54-9269-6b747f907f9e', kernel_partitionability_acknowledged, deontological).
narrative_ontology:cs_reference_frame('9637a87d-bb44-4c54-9269-6b747f907f9e', unified_competence_exercised_via_simulation).
narrative_ontology:cs_drift_state('9637a87d-bb44-4c54-9269-6b747f907f9e', contemporary_incident_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9637a87d-bb44-4c54-9269-6b747f907f9e', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organization_maintaining_compliance).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_infrastructure_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, personnel_exposed_to_judgment_failure_scenarios).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, public_downstream_of_degraded_improvisation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, trained_crisis_responders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, trained_crisis_responders).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, partial_competence_retention_via_procedural_exercise).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, judgment_capacity_requires_stakes_activation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under regulatory mandate to conduct annual exercises to maintain crisis-response competence. Designs and schedules simulations to satisfy compliance requirements. Interprets 'competence maintenance' as procedural rehearsal. Extracts operational benefit from exercising staff without exposing the organization to the stakes-dependent judgment degradation that occurs between exercises. The constraint allows the organization to claim competence is maintained while actual judgment-under-pressure capacity decays.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organization_maintaining_compliance, agenda_setter,
    institutional, generational, constrained, national).

% Operate the simulation systems, design exercise scenarios, and conduct after-action reviews. Their position depends on exercises being scheduled and conducted regularly; a shift toward lived-catastrophe-only validation would eliminate their primary revenue and institutional role. They benefit from the constraint's framing that simulation constitutes competence maintenance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_infrastructure_operators, beneficiary,
    organized, biographical, arbitrage, national).

% Conduct exercises annually, retain procedural knowledge and muscle memory, build confidence in their role execution. They experience genuine coordination benefit from shared scenario rehearsal. They also bear the cost: judgment capacity decays between exercises in components that cannot be simulated (real-time resource scarcity, actual personnel loss, irreversible decisions under genuine uncertainty). When actual crises occur, they face judgment demands that exercises did not prepare them for.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, trained_crisis_responders, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, trained_crisis_responders, beneficiary).

% Are harmed when trained responders encounter crisis situations in which judgment-under-stakes is demanded and has not been rehearsed. They experience the degraded improvisation that occurs when procedural competence is retained but judgment capacity has atrophied. Their harm is localized and immediate; the constraint treats this as an acceptable residual risk.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, personnel_exposed_to_judgment_failure_scenarios, payer,
    powerless, immediate, trapped, local).

% Experiences systemic vulnerability to judgment failures in critical-infrastructure or public-safety crises. The constraint's architecture ensures procedures are maintained but improvisation capacity decays, creating structural exposure to scenarios that require judgment the exercise regimen did not prepare for. They cannot exit.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, public_downstream_of_degraded_improvisation, payer,
    powerless, generational, trapped, regional).

% Sets the mandate that exercises be conducted to maintain competence. Audits compliance with the exercise requirement. Does not typically measure or audit judgment-capacity retention, only procedural completion. Observer seat: sees both the coordination function (exercises do maintain procedures) and the extraction (the constraint permits judgment-capacity decay while claiming full competence maintenance).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_authority, observer,
    institutional, generational, analytical, national).

% Propose that judgment capacity requires high-fidelity or lived-stakes validation, not simulation alone. Are excluded from designing the competence mandate because their findings would undermine the constraint's architecture. Would advocate for measuring judgment-decay metrics and scheduling more frequent lived-catastrophe validation or higher-fidelity judgment-training components.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, alternative_validation_researchers, excluded,
    moderate, biographical, constrained, global).

% Are harmed when responders' procedural knowledge breaks down entirely — when the crisis scenario falls outside the simulated envelope and responders lack the improvisation capacity to adapt. Their voices are heard only in post-incident inquiries; they are not in the conversation that designs the exercise architecture.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_victims_from_procedural_failure, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, organization_maintaining_compliance).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Exercises maintain shared procedural knowledge and muscle memory across a trained cohort: responders practice the sequence of actions, communication protocols, resource allocation sequences, and escalation procedures that allow coordinated crisis response. Simulation solves the problem of maintaining procedural competence without incurring the cost of live catastrophes.
% TRANSFER_FUNCTION: Moves institutional risk from 'catastrophe occurrence' to 'judgment-capacity decay between exercises.' The organization transfers the cost of maintaining judgment competence to the personnel exposed to scenarios in which only judgment was degraded (procedural competence was retained). Simulation infrastructure operators extract institutional rent through the constraint's architecture: organizations cannot shift to alternative validation regimes without dismantling the simulation infrastructure layer.
% ABSENT_VOICES: Alternative-validation researchers and victims of previous judgment-failure incidents are excluded from the exercise-design conversation. They would argue that the kernel should be partitioned — procedures can be exercised via simulation, but judgment requires different validation mechanisms. They would call for metrics tracking judgment-capacity decay and for architecture redesign around the hybrid competence requirement.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, organizations would face genuine choice: invest in lived-catastrophe validation (with catastrophic risk) or decompose the kernel and design separate exercise regimens for procedures (simulation) and judgment (embedded decision-making training, high-fidelity judgment scenarios, or acceptance of judgment-decay risk). The current constraint forecloses this choice by treating simulation as sufficient for the whole kernel; disappearance would immediately expose the architectural split.
% FOUNDING_PROBLEM: Organizations needed a mechanism to maintain crisis-response competence without incurring the expense and risk of live-catastrophe training. Simulation was developed as an economical, reproducible way to exercise procedures and build muscle memory in coordinated teams.
% FOUNDING_PROBLEM_CORROBORATION: The organization and simulation infrastructure operators attest the founding problem is still live: live catastrophes are rare and expensive, simulation is cost-effective. Researchers in judgment under uncertainty and incident investigators from previous crises attest the founding problem was partially solved (procedures maintained) but revealed an architectural flaw: judgment capacity was not addressed. The constraint persists despite the acknowledged incompleteness.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-rising (0.42→0.58 over interval), not high, because the constraint does deliver genuine coordination: procedures are maintained, responders build confidence, communication protocols are shared. The extraction is the organization's ability to claim full competence maintenance while permitting judgment-decay. Theater ratio is high (0.67) and rising, indicating that an increasing proportion of exercise activity is performative completion of compliance rather than genuine judgment-capacity rehearsal. Suppression is moderate (0.42) and rising: the constraint persists through organizational rule enforcement (exercises are mandated) and internalized framing (responders accept that simulation constitutes competence). Resistance is high (0.71) because alternative-validation researchers and incident investigators actively contest the framing, though they are excluded from the design process. Accessibility-collapse is moderate (0.48): alternatives (lived-catastrophe validation, judgment-training specialization, acceptance of judgment-decay risk) are technically possible and conceptually articulated by excluded voices, but the regulatory mandate and institutional inertia keep them inaccessible. The measurement series show extractiveness and theater rising over time (judgment-decay becomes more pronounced as exercises age without real-stakes activation), while suppression stabilizes at a moderate level once the routine is institutionalized. One shared time grid ensures all metrics are authored at each examined point.
 *
 * PERSPECTIVAL GAP:
 *   The organization and infrastructure operators experience this as genuine coordination: 'We maintain competence through exercises.' Trained responders experience it as partial coordination with hidden cost: 'We retain procedures but lose judgment capacity.' Personnel exposed to judgment failures and publics experience it as pure extraction: 'The system is prepared for procedures but not for the scenarios we actually encounter.' The regulatory authority is between these: they see the procedural coordination (exercises are conducted) but do not typically measure the judgment-decay component. The engine's per-seat classification should diverge: the organization and infrastructure operators may compute as benefiting from rope or shallow tangled_rope (coordination with modest extraction); responders may compute as targets of tangled_rope (coordinated on procedures, extracted on judgment-decay risk); powerless victims may compute as targets of snare (no coordination benefit, pure extraction of judgment-failure risk). These divergences are structurally inherent to the constraint and should emerge from the authored directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   Organization (institutional, arbitrage): declared as agenda_setter beneficiary. High directionality toward beneficiary (d ~0.15) because they control the rules, design the exercises, collect compliance value. Simulation operators (organized, arbitrage): declared as beneficiary. Concentrated extraction benefit; high directionality toward beneficiary (d ~0.20). Trained responders (moderate power, constrained exit): declared as dual-positioned. Derive d ~0.50 (symmetric) because they gain procedural coordination and lose judgment capacity at roughly equivalent structural weight. Personnel exposed to judgment failures (powerless, trapped): declared as payer. High directionality toward target (d ~0.85) because they bear the judgment-failure risk without choice or benefit. Public downstream (powerless, trapped): declared as payer. High directionality toward target (d ~0.90) because they are exposed to systemic judgment failures without recourse. Regulatory authority (institutional, analytical): observer seat, d ~0.50 (symmetric position observing the structure). Alternative-validation researchers (moderate, constrained): excluded from the design conversation; their position is not captured in the directionality derivation (they are absent from the stakeholder roles).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (crisis-response competence maintenance) is contested in status but structurally still live: crises still occur, competence still degrades without rehearsal. The constraint addresses the founding problem *incompletely*: it solves procedural competence but not judgment competence. The hybrid-decay reading acknowledges this incompleteness as a structural feature, not a failure. Mandatrophy would obtain if the founding problem (all crises still require judgment) had been solved but the constraint persisted anyway. This reading names the mandate without resolving it — the mandate to maintain 'competence' is divided into procedural (met) and judgment (unmet), and the constraint persists because the organization can claim compliance without measuring the unmet component. The classification as tangled_rope (not snare) reflects the genuine coordination on procedures; the extraction (judgment-decay risk to powerless victims) is asymmetric and active but not the sole function. Decomposing the kernel (as the omega on partitionability suggests) would be the structural move toward mandatrophy resolution: separate exercises for procedures and judgment would permit each to be validated appropriately and prevent the false unification that permits extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judgment_decay_measurement_ambiguity,
    'Is judgment-capacity decay between exercises an inherent property of the competence kernel, or is it contingent on the specific fidelity and frequency of exercises?',
    'Longitudinal measurement of decision-quality metrics in crisis scenarios (response time, option-set breadth, uncertainty handling) correlated against exercise frequency, fidelity, and time-since-exercise. High-fidelity judgment-training cohorts tracked against standard-simulation cohorts.',
    'If decay is inherent to simulation, the hybrid reading stands: procedures are maintained, judgment decays. If decay is contingent (high-fidelity simulation can maintain judgment), the simulation_sufficiency_reading gains credibility. If decay is irreversible without real stakes, the lived_catastrophe_necessity_reading gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_decay_measurement_ambiguity, empirical, 'Whether judgment decay is kernel-inherent or exercise-contingent.').

omega_variable(
    kernel_partitionability,
    'Is the competence kernel genuinely unitary, or are procedural competence and judgment capacity separable domains with different exercise requirements?',
    'Decompose crisis-response competence into procedures (communication, escalation, resource sequencing), judgment (decision-making under irreversible uncertainty, improvisation when scenarios diverge from procedure), and meta-competence (recognizing when procedures fail). Test whether exercises that target only one domain maintain that domain while the others decay.',
    'If separable, the constraint''s architecture is a false unification that permits judgment-decay while claiming full competence. If unitary, simulation sufficiency may be correct and this reading overstates the decay risk. If separable, redesign should decompose the exercise regimen into domain-specific validation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_partitionability, conceptual, 'Whether the competence kernel is unitary or partitionable across procedure/judgment/meta-domains.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (organizational rule enforcement against non-compliance) or internalized (responders internalize the framing that simulation suffices, and suppress their own judgment-decay concerns)?',
    'Post-incident interviews with responders after actual crises: do they report having expressed concerns about judgment-capacity decay before the incident? Were concerns routed to decision-makers or suppressed internally? Anonymous surveys on how responders perceive the adequacy of exercise-based validation.',
    'If suppression is structural (external enforcement), removing the rule would enable alternative validation regimes. If internalized (responders believe simulation suffices), the constraint persists through conviction rather than enforcement, making it more durable and harder to shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression of alternative validation proposals.').

omega_variable(
    extraction_beneficiary_divergence,
    'Does the organization genuinely benefit from the constraint (procedures maintained, judgment decay accepted as cost), or is the extraction concentrated in the simulation infrastructure operators while the organization incurs judgment-failure risk?',
    'Cost-accounting: compare the cost savings from simulation-based validation vs. lived-catastrophe or high-fidelity judgment training, against the measured incident cost increases attributable to judgment failures. Track which institutional parties capture the savings and which bear the risk.',
    'If the organization benefits (savings > judgment-failure costs), the tangled_rope framing holds. If extraction is concentrated in infrastructure operators and the organization bears judgment-failure risk, the constraint is closer to a snare with a thin coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_divergence, empirical, 'Distribution of constraint benefits and costs across the agenda-setter and infrastructure operators.').

omega_variable(
    kernel_reading_interpretation_ambiguity,
    'Is the authority interpreting ''competence maintenance'' as requiring unitary kernel validation, or does it acknowledge the kernel partition but choose simulation as the administratively tractable mechanism despite knowing about judgment decay?',
    'Review regulatory authority''s internal documentation: do guidance memos, training curricula, or compliance audits acknowledge judgment capacity as a separate domain? Do post-incident investigations recommend judgment-specific validation mechanisms? Is the partition acknowledged privately but treated as unitary publicly?',
    'If the authority is genuinely unaware of the partition, this is a conceptual omega (false unification). If aware but treating it as unitary for administrative convenience, this is a preference omega (the authority is choosing simplicity over completeness). The distinction changes which reading is most credible: simulation_sufficiency if genuinely unaware, hybrid_decay if aware but choosing simplicity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_interpretation_ambiguity, conceptual, 'Whether the kernel partition is unacknowledged (conceptual ambiguity) or acknowledged but treated as unitary administratively (preference ambiguity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(exer_tr_t0, observed).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.57).
narrative_ontology:measurement_basis(exer_tr_t4, observed).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.61).
narrative_ontology:measurement_basis(exer_tr_t8, observed).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.64).
narrative_ontology:measurement_basis(exer_tr_t12, observed).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.66).
narrative_ontology:measurement_basis(exer_tr_t16, observed).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.67).
narrative_ontology:measurement_basis(exer_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(exer_be_t0, observed).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(exer_be_t4, observed).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(exer_be_t8, observed).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(exer_be_t12, observed).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(exer_be_t16, observed).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(exer_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(exer_su_t0, observed).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement_basis(exer_su_t4, observed).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(exer_su_t8, observed).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(exer_su_t12, observed).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(exer_su_t16, observed).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(exer_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'exercise-as-competence-maintenance.' The family comprises three distinct constraint stories, each instantiating a different reading of the same kernel with different ε values and victim sets. The hybrid-decay reading (this story) asserts that simulation maintains procedural competence but judgment decays; the simulation_sufficiency_reading asserts that high-fidelity simulation maintains the whole competence kernel; the lived_catastrophe_necessity_reading asserts that only real stakes can exercise the full kernel. Each reading has distinct beneficiaries and victims: hybrid-decay includes judgment-failure victims as a distinct victim class. The constraint family is linked by the shared founding problem (competence maintenance) and the shared kernel (exercise mechanisms). ε diverges because each reading makes different claims about what simulation can accomplish: hybrid-decay sees simulation as partial (~0.58 extractiveness), simulation_sufficiency sees it as complete (lower extraction expected), lived_catastrophe_necessity sees alternatives as ineffective (higher extraction expected). The readings compete in institutional design conversations; no single framework holds all three simultaneously, but different organizations may adopt different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, powerless, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
