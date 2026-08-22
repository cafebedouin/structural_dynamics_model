% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation-Based Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A regulatory authority has mandated that high-fidelity simulation with
 *   structured debriefing constitutes adequate exercise of pilot competence,
 *   making recurrent simulator training the standard for maintaining flight
 *   crew qualifications. The constraint coordinates a distributed aviation
 *   training system and enables cost minimization for airlines, but it
 *   simultaneously extracts from flight crews by deferring real-world
 *   validation indefinitely and from safety culture advocates by foreclosing
 *   hybrid or catastrophe-anchored models. The reading is ONE of three
 *   contested interpretations of the kernel 'competence exercise
 *   requirement': the other readings (catastrophe-as-necessary-anchor and
 *   hybrid-dependency) rest on the empirical claim that simulation alone is
 *   insufficient to maintain authentic competence under stress. This
 *   constraint story instantiates only the simulation-adequate reading,
 *   treating it as a fixed, stable epistemic position from which to measure
 *   enforcement, beneficiary capture, and structural extraction.
 *
 * KEY AGENTS:
 *   - Regulatory Authority: institutional power, sets and enforces the simulation-based standard via approval authority and audit
 *   - Training Operators: powerful institutional beneficiaries, collect revenue from airlines for simulator facility operation and training delivery
 *   - Airline Cost-Minimizers: powerful institutional beneficiaries, save substantial operational costs by avoiding line-flying recurrence requirements
 *   - Flight Crews: moderate power, payers bearing the cost of identity-fused constraint: competence exercised only in abstract space without real-world consequences
 *   - Safety-Culture Purists: moderate power, identity-locked payers; professional identity depends on regulatory acceptance, making exit prohibitively costly
 *   - Excluded parties: catastrophe survivors and hybrid-model proponents who would dispute the reading's core claim that simulation is adequate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.72).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation-Based Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, 'eac71164-dd2a-489d-8fb7-4adf6444cdf2').
narrative_ontology:cs_kernel_codification('eac71164-dd2a-489d-8fb7-4adf6444cdf2', formalized).
narrative_ontology:cs_authority_grounding('eac71164-dd2a-489d-8fb7-4adf6444cdf2', extraction).
narrative_ontology:cs_interpretation_layer_present('eac71164-dd2a-489d-8fb7-4adf6444cdf2').
narrative_ontology:cs_reading_relation('eac71164-dd2a-489d-8fb7-4adf6444cdf2', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('eac71164-dd2a-489d-8fb7-4adf6444cdf2', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('eac71164-dd2a-489d-8fb7-4adf6444cdf2', foundational, simulation_fidelity_suffices_for_competence_maintenance).
narrative_ontology:cs_axiom_status(simulation_fidelity_suffices_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('eac71164-dd2a-489d-8fb7-4adf6444cdf2', simulation_fidelity_suffices_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('eac71164-dd2a-489d-8fb7-4adf6444cdf2', foundational, catastrophe_free_decades_validate_adequacy).
narrative_ontology:cs_axiom_status(catastrophe_free_decades_validate_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('eac71164-dd2a-489d-8fb7-4adf6444cdf2', catastrophe_free_decades_validate_adequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('eac71164-dd2a-489d-8fb7-4adf6444cdf2', simulation_adequacy_doctrine).
narrative_ontology:cs_drift_state('eac71164-dd2a-489d-8fb7-4adf6444cdf2', post_40_year_validation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eac71164-dd2a-489d-8fb7-4adf6444cdf2', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authority).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, training_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_cost_minimizers).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, safety_culture_purists).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_fidelity_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_free_operation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Specifies simulation-based recurrent training as the standard for maintaining pilot competence. Conducts audits to verify training hours and simulator scenario completion. Justifies the standard by citing improved safety metrics in decades without major crashes and cost-effectiveness relative to line-flying requirements. Sets and enforces the measurement criteria: flight hours in high-fidelity simulator + structured debriefing = competence maintenance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Operate the simulator facilities and conduct the recurrent training programs. Collect revenue from airlines and flight schools based on training hours logged. Benefit from the regulatory requirement: the mandate ensures steady demand for simulator time and training services. Their business model depends on the constraint remaining in place.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, training_operators, beneficiary,
    powerful, biographical, arbitrage, national).

% Operate large flight operations under the regulatory requirement. Benefit from simulation-based training because it is cheaper than releasing crews for weeks of line flying, avoids operational disruption from training schedules, and provides a stable, predictable competence assurance mechanism they can document to investors and regulators. Cost advantage is substantial compared to hybrid or catastrophe-anchored models.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_cost_minimizers, beneficiary,
    powerful, biographical, arbitrage, global).

% Required to maintain recurrent simulation training on the regulatory schedule. Trade real-world decision-making under uncertainty (which would occur in hybrid or catastrophe-anchored models) for controlled, repeatable scenarios with no operational consequence. Competence is exercised in an abstract space; real-world validation is deferred indefinitely. Exit option is to leave the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews, payer,
    moderate, biographical, constrained, global).

% Professional safety researchers and experienced pilots who hold that authentic competence requires periodic encounter with real-world complexity and consequences. Constrained by regulatory authority that dismisses catastrophe-anchoring and hybrid approaches as outdated or unsafe. Their professional standing depends on working within the system they critique. Identity fused with the principle that simulation alone cannot substitute for the decision-making pressure of actual operations.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_culture_purists, payer,
    moderate, generational, identity_locked, national).

% Families and survivors of aviation accidents that occurred despite simulator-trained crews. Would argue that simulation had failed to exercise the competence required to avoid the accident; they are effectively excluded from the regulatory process by the authority's interpretation of the evidence base.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_survivors, excluded,
    powerless, biographical, trapped, local).

% Organized groups within aviation (some experienced pilots, some safety researchers) who advocate for periodic real-world operation or high-stakes evaluation to anchor competence. Structurally excluded from regulatory authority decision-making by the authority's closure: they can publish, consult, but cannot alter the binding standard without regulatory reversal.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_model_proponents, excluded,
    organized, generational, constrained, national).

% External analysts of safety systems and organizational learning who observe the constraint's operation and can measure divergence between the reading's claims and the actual competence profile of crews trained under the constraint.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, observer_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authority).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining standardized, measurable, repeatable pilot competence across a distributed aviation system without grounding every recurrent evaluation in actual line operations. Creates a uniform standard that airlines can budget for, crews can schedule, and regulators can audit.
% TRANSFER_FUNCTION: Transfers the risk and cost of competence maintenance from airlines and line operations to simulator training operators and regulatory compliance overhead. Transfers the cognitive load of real-world decision-making uncertainty from flight crews to training scenario designers. Transfers financial savings (relative to hybrid models) from airlines to shareholders and reduced ticket prices.
% ABSENT_VOICES: Catastrophe survivors and hybrid-model proponents are excluded from the regulatory authority's deliberation process. They would argue that simulation-based competence is a cover story for cost minimization and that authentic competence requires periodic real-world validation. The authority's closure (its decision to treat the absence of major crashes as validation of the reading rather than as luck or favorable circumstances) keeps these voices at bay.
% DISAPPEARANCE_RATIONALE: If the simulation-based requirement disappeared overnight, airlines would revert to hybrid models or catastrophe-anchored practices; training costs would rise substantially; regulatory compliance would shift to real-world metrics and periodic line-flying requirements; the training operators' business model would collapse; and the safety culture would reorganize around acknowledgment of competence gaps simulation cannot address.
% FOUNDING_PROBLEM: Early aviation training relied on expensive, operationally disruptive line-flying recurrence and on the assumption that safety came from catastrophe-level incidents or near-misses that forced crew learning. As aviation operations became more distributed and cost-conscious, the founding problem was reframed: how to maintain pilot competence without expensive, repeated line operations and without waiting for catastrophes?
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority and training operators attest the founding problem is solved: decades without major crashes in developed aviation systems validate the reading. Hybrid-model proponents and safety culture researchers attest the founding problem is reframed, not solved: the absence of crashes reflects favorable conditions and selection effects (best crews, best aircraft, automated safety systems), not proof that simulation is adequate; their analysis cites near-miss incident patterns and simulator-to-line-operation transfer failures documented in independent safety studies.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the constraint decouples the exercise of competence (simulator scenarios, standardized, repeatable) from the validation of competence (absence of major crashes over 40+ years in developed aviation). The authority collects rents from airlines' cost savings; training operators collect direct revenue; flight crews and safety advocates pay through deferred real-world validation and foreclosure of alternative frameworks. Suppression (0.72) is high because the regulatory authority actively excludes hybrid and catastrophe-anchored models by treating the absence of crashes as validation of the reading rather than as selection effects or luck. Theater ratio (0.41) is moderate-to-high: a significant portion of enforcement activity goes to measuring compliance (training hours, scenario checkboxes) rather than to validating competence itself. The measurements span 32 years and show rising extractiveness and suppression requirement over time as the constraint hardens (regulatory closure tightens, hybrid models become less culturally viable, cost savings accumulate). Accessibility collapse (0.62) reflects that alternatives exist in principle (hybrid models have been advocated, catastrophe-anchoring has historical precedent) but regulatory closure has made them structurally inaccessible. Resistance (0.58) is moderate: safety culture critics mount real resistance, but their voice is constrained by institutional position dependence.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority and training operators experience this constraint as genuine coordination: they perceive the problem it solves (distributed, repeatable, measurable competence assurance) and the solution as aligned. From the seat of flight crews and safety-culture purists, the same constraint appears as regulatory theater masking cost minimization: competence is exercised only in abstract space; the decades-long absence of crashes is treated as proof rather than as possibly reflecting favorable circumstances, automation, and selection effects independent of training adequacy. The engine should compute these seats as experiencing fundamentally different effective extractiveness from the same structural arrangement. The authority's seat would show lower χ (it benefits from the coordination function and the cost savings). The payer seats would show higher χ (they bear the deferred validation cost and the identity-lock suppression). This divergence is a core signal the constraint is tangled rather than pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authority: beneficiary (agenda-setter, collects legitimacy from the constraint, shapes what counts as evidence). d near 0.1–0.2 (full beneficiary). Training operators: beneficiary (institutional power, arbitrage access, direct revenue). d near 0.15–0.25. Airline cost-minimizers: beneficiary (substantial cost savings, strategic advantage). d near 0.20–0.30. Flight crews: target (constrained exit, identity-locked safety advocates are especially trapped). d near 0.70–0.85 for crews generally; near 0.85+ for safety-culture purists. Catastrophe survivors: excluded but would be high-d if included (victims). The directionality profile is asymmetric: concentrated beneficiary power at the institutional level (authority + trainers + airlines) vs. diffuse payer vulnerability at the individual and professional-culture level (crews + safety advocates). No directionality override is needed; the structural derivation from beneficiary/victim + power + exit captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain competence without expensive line-flying recurrence and without waiting for catastrophes) was genuinely live in the 1970s–1980s when simulation technology was nascent. The regulatory reading that simulation-with-debriefing is adequate rested on an empirical hypothesis: high-fidelity simulation exercises competence sufficiently such that absence of major crashes validates the approach. After 40+ years without major crashes in developed aviation, the authority treats this as proof of adequacy. However, the mandatrophy candidate is whether the founding problem is dead (solved by the constraint) or merely appears solved due to selection effects (best aircraft, best operators, automation, favorable macro conditions, and the worst-case disasters being rare anyway). The hybrid and catastrophe-anchor readings assert the problem is not solved, only obscured. A classically tangled rope shows mandatrophy when the coordination function (standardized, repeatable training) persists even after the founding extraction (cost minimization via simulation vs. line-flying) is no longer necessary because automation and safety systems have made real-world competence validation redundant. The constraint persists because the beneficiary institutions (authority, trainers, airlines) have no incentive to revise it, not because the coordination is indispensable. Mandatrophy is not resolved; it is the open question that the omegas address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_floor_ambiguity,
    'Is there a minimum threshold of simulation fidelity and scenario realism below which competence maintenance becomes inadequate? If so, where is it?',
    'Empirical analysis of accident cases involving simulator-trained crews: correlation between simulator scenario coverage and actual accident conditions. Comparative study of competence gaps identified in post-accident debriefing vs. pre-accident simulator performance.',
    'If a clear floor exists and current simulators fall below it, the reading collapses into an inferior false-economy; if no floor is identifiable, the reading holds but loses its empirical anchor and becomes convention-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_floor_ambiguity, empirical, 'Existence and location of simulation fidelity adequacy threshold.').

omega_variable(
    stress_authenticity_gap,
    'Does simulation with debriefing exercise the emotional and cognitive stress responses that real operations under jeopardy require? If not, is that gap competence-relevant?',
    'Neuroscience and decision-science studies of stress-response training transfer: comparison of crew decision patterns under high-consequence simulation (where failures have no real-world consequence) vs. line operations (where they do).',
    'If the gap is genuine and competence-relevant, simulation is necessary but insufficient, supporting the hybrid model; if the gap is performance-irrelevant or fully closed by structured debriefing, the reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stress_authenticity_gap, empirical, 'Transfer of stress-response competence from simulation to actual operations.').

omega_variable(
    luck_vs_validation_ambiguity,
    'Does the 40+ year absence of major crashes in developed aviation validate simulation-based training, or does it reflect selection effects (the best aircraft, best operators, automated systems), survivorship bias, and favorable macro conditions?',
    'Controlled study comparing developing-world aviation (similar or worse training standards, worse aircraft/infrastructure) with developed-world baselines. Analysis of near-miss patterns and safety culture metrics independent of accident statistics.',
    'If the absence of crashes is largely selection effects and luck, the reading''s empirical warrant collapses; the constraint persists as regulatory theater and cost minimization. If validation is genuine, the reading holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(luck_vs_validation_ambiguity, conceptual, 'Whether decades without major crashes validate the reading or reflect favorable circumstances independent of training adequacy.').

omega_variable(
    identity_lock_mechanism,
    'For safety-culture purists (identity_locked exit), what specific identity fusion makes the hybrid or catastrophe-anchored alternative unthinkable? Is it professional identity (belief in measurable standards), ideological identity (commitment to scientific efficiency), or institutional identity (career within regulatory systems)?',
    'Qualitative analysis of professional discourse: how safety advocates describe their commitment to simulation-based training and what they would have to renounce (credentials, standing, career) to advocate for hybrid alternatives.',
    'If identity lock is primarily professional/institutional, exit becomes possible with career cost; if ideological, it is more persistent. The depth of lock modulates the suppression metric: internalized suppression lingers after the constraint is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Nature and depth of identity fusion for safety-culture critics of the reading.').

omega_variable(
    reading_vs_catastrophe_anchor_foreclosure,
    'Does this reading (simulation-adequate) logically foreclose the catastrophe-as-anchor reading, or do they coexist as different legitimate positions?',
    'Logical analysis of core premises: if simulation-adequate is true, can catastrophe-anchoring still be true? Do they rest on incompatible empirical claims (about competence maintenance mechanisms) or on compatible empirical claims with different evaluative frames (one prioritizes cost, one prioritizes resilience)?',
    'If they foreclose, this reading is in zero-sum contest with catastrophe-anchoring (typical kernel contest structure). If they coexist, both can hold legitimately within different institutional frameworks or priorities, suggesting the kernel is genuinely underdetermined by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_catastrophe_anchor_foreclosure, conceptual, 'Logical relationship between simulation-adequate and catastrophe-anchoring readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(comp_tr_t0, projected).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(comp_tr_t8, observed).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(comp_tr_t16, observed).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(comp_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(comp_be_t0, projected).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(comp_be_t8, observed).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(comp_be_t16, observed).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(comp_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, projected).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(comp_su_t8, observed).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(comp_su_t16, observed).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(comp_su_t24, observed).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(comp_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'competence_exercise_requirement'. The kernel contest is driven by irreducible disagreement about what constitutes adequate exercise of pilot competence: simulation alone (this reading), simulation + periodic real-world anchoring (hybrid), or real catastrophic events (catastrophe-anchor). Each reading is compiled as a separate constraint story with its own ε value, beneficiary/victim structure, and stakeholder situation, because the three readings rest on incompatible empirical claims about competence maintenance and different evaluative priorities. The three stories are linked via network.affects_constraints to indicate kernel kinship and to enable the contamination propagation system to track how shifts in one reading's empirical warrant affect the others. See commentary.kernel_context for the logical relationships (foreclosure, coexistence, influence) between the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
