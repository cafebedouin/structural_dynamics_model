% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: High-Fidelity Simulation as Adequate Exercise of Operator Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the 'simulation_as_adequate_exercise'
 *   reading of the competence-exercise-requirement kernel: the position, held
 *   by operators, regulators, and simulator manufacturers, that scheduled
 *   high-fidelity simulation with structured debriefing fully discharges the
 *   requirement to exercise and maintain operational competence for rare,
 *   high-consequence failure modes. This reading treats decades without
 *   catastrophic system failure as validation, and treats regulatory
 *   compliance with certified simulator-hour minimums as sufficient. It is
 *   one of three structurally distinct readings of the same kernel; the other
 *   two (catastrophe_as_necessary_anchor and hybrid_dependency) are separate
 *   constraints with their own ε and stakeholder structures, not alternative
 *   measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.55).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "High-Fidelity Simulation as Adequate Exercise of Operator Competence").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, 'f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3').
narrative_ontology:cs_kernel_codification('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', formalized).
narrative_ontology:cs_authority_grounding('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', expertise).
narrative_ontology:cs_interpretation_layer_present('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3').
narrative_ontology:cs_reading_relation('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', foundational, fidelity_plus_debriefing_is_sufficient_exercise).
narrative_ontology:cs_axiom_status(fidelity_plus_debriefing_is_sufficient_exercise, holdable).
narrative_ontology:cs_axiom_grounding('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', fidelity_plus_debriefing_is_sufficient_exercise, empirically_contingent).
narrative_ontology:cs_axiom('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', secondary, catastrophe_free_interval_constitutes_validation).
narrative_ontology:cs_axiom_status(catastrophe_free_interval_constitutes_validation, holdable).
narrative_ontology:cs_axiom_grounding('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', catastrophe_free_interval_constitutes_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', simulator_certification_sufficiency_standard).
narrative_ontology:cs_drift_state('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', post_multidecade_catastrophe_free_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6b8cf6f-9f7c-456a-99fe-b1c1407c99e3', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_manufacturers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, training_departments).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews_facing_novel_failures).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, passengers).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_equals_competence_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the training curriculum and recurrent schedule, choosing simulator hours over line-operation exposure because simulation is cheaper, scalable, schedulable, and produces auditable records. They collect the cost savings directly and bear no personal exposure to whether the resulting competence holds under a genuinely novel failure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary).

% Sell increasingly high-fidelity simulator hardware and software, and their commercial interest is directly served by the doctrine that simulation constitutes adequate exercise. They fund and publicize validation studies that support this reading of the kernel.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_manufacturers, beneficiary,
    organized, generational, arbitrage, global).

% Certify simulator fidelity standards and set minimum recurrent-training hours, treating documented simulator sessions as sufficient proof of maintained competence. Compliance with the certified curriculum discharges their oversight obligation and shields them from liability if outcomes go wrong, since the process was followed.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies, beneficiary).

% Design and deliver the simulation-based curriculum, are evaluated on completion rates and pass rates within the simulator paradigm, and have professional incentive to defend simulation's sufficiency since their institutional function depends on it being adequate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, training_departments, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, training_departments, beneficiary).

% Undergo recurrent simulator checks on the schedule set by the operator and regulator, are certified competent on that basis, and carry the risk if simulator fidelity fails to capture the specific perceptual, physiological, or decision-making demands of an actual novel emergency. They have little power to demand additional real-world exposure; requesting it can read as questioning the adequacy of mandated training, a career risk.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots, payer,
    moderate, biographical, constrained, global).

% In the specific moment of an unmodeled or outside-the-training-envelope failure, must respond using competence built entirely on simulated scenarios that were themselves built from previously known failure modes. If the failure falls outside what simulator scenario libraries anticipated, the exercise the kernel required turns out not to have covered the case at hand.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews_facing_novel_failures, payer,
    powerless, immediate, trapped, local).

% Rely entirely on crew competence during an emergency without any visibility into how that competence was built or validated. They bear the ultimate consequence if simulation-based training proves inadequate to a real, unanticipated event, with no voice in how competence-maintenance policy is set.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, passengers, payer,
    powerless, immediate, trapped, local).

% Study accident and incident data to assess whether simulator-trained crews perform comparably to those with real-world exposure during genuine emergencies. Their findings are contested by industry-funded studies and constrained by the rarity of real catastrophic events to study.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, safe method for exposing large numbers of pilots to a wide library of failure scenarios without risking aircraft, crew, or passengers, and generates auditable records that competence was exercised on a defined schedule.
% TRANSFER_FUNCTION: Moves the cost and risk of maintaining operational competence away from real-world exposure (expensive, schedule-disrupting, genuinely risky) and onto a controlled artificial environment, while moving liability exposure away from operators and regulators (who can point to compliance with the certified curriculum) toward line crews and passengers who bear the consequence if the artificial environment under-modeled the real emergency encountered.
% ABSENT_VOICES: Passengers have no representation in setting recurrent-training standards despite bearing the terminal risk. Line pilots who believe simulator scenario libraries have gaps relative to real operational hazards have limited channels to escalate this without appearing to challenge certified competence, and safety researchers whose findings complicate the sufficiency claim compete against industry-funded validation studies for regulatory attention.
% DISAPPEARANCE_RATIONALE: If the doctrine that simulation constitutes adequate exercise were abandoned, operators would need to fund substantially more expensive real-world exposure (line operating experience, non-jeopardy audits, actual aircraft time), regulatory certification frameworks built around simulator-hour minimums would require rewriting, simulator manufacturers would lose the exclusive validation claim underlying their commercial position, and training budgets and schedules across the industry would restructure around a costlier hybrid model.
% FOUNDING_PROBLEM: Real catastrophic and near-catastrophic events are rare, and deliberately manufacturing real emergencies to train crews is unacceptably dangerous and destructive — some artificial substitute for direct catastrophic exposure was needed to maintain crew competence at scale without killing people to do it.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and simulator manufacturers attest the founding problem is fully solved by current high-fidelity simulation standards, citing decades without catastrophic system failure attributable to training gaps. Independent safety researchers and some pilot unions attest the founding problem persists in modified form: simulator scenario libraries are built retrospectively from known failure modes and may not capture genuinely novel failures, and the catastrophe-free record may partly reflect underlying reliability improvements unrelated to training method rather than proof that simulation alone suffices.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).
:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and rising slowly: the coordination function is real (simulation genuinely lets many pilots rehearse many failure modes safely and cheaply), but the asymmetry is that operators and regulators capture the cost savings and liability shielding of the compliance-equals-competence framing while crews and passengers absorb the tail risk of unmodeled failures. Theater ratio rises over the interval (0.20 to 0.38) as certification and documentation activity increasingly substitutes for direct evidence that trained competence transfers to genuinely novel real-world scenarios — a Goodhart-style drift where passing the simulator check becomes the measured target rather than the underlying competence it was meant to proxy. Suppression is moderate-high because raising doubts about simulator sufficiency inside training or regulatory institutions carries professional cost, not because of overt coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators, regulators, training departments, and manufacturers all sit near the beneficiary end: they set the standard, are evaluated by compliance with it, and capture cost or commercial benefit from its adequacy claim. Line pilots sit closer to target: they are certified competent on this basis without much power to contest the standard's adequacy. Flight crews facing an actual novel failure and passengers sit at the extreme target end — trapped, immediate exposure, zero voice in how the standard was set — because for them the abstract adequacy question becomes a concrete, non-hypothetical stake at the moment it matters most.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (safe, scalable exposure to failure scenarios) is genuine and should not be flattened into pure extraction — banning simulation-based training would be an overcorrection with its own catastrophic costs. But treating regulatory compliance with the simulator standard as fully discharging the competence obligation risks mandatrophy in the other direction: a standard justified by a coordination problem (how do we train safely at scale) drifting into a self-validating claim (compliance proves competence) that forecloses examination of whether the coordination function still matches the risk it was built to cover. The tangled_rope classification holds both truths: real coordination value, plus asymmetric extraction of cost-savings and liability-shielding paid for in tail-risk borne by crews and passengers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_free_record_as_validation,
    'Does a multi-decade record without catastrophic failure validate the sufficiency of simulation-based training, or does it reflect independent reliability improvements (better hardware, redundancy, weather forecasting, ATC systems) that would have suppressed catastrophic outcomes regardless of training method?',
    'Comparative analysis isolating training-method contribution from other reliability factors across operators/eras with differing training regimes but similar hardware generations; counterfactual modeling of near-miss data to assess whether outcomes in edge cases correlate with real-world flight-hour exposure independent of simulator hours.',
    'If the catastrophe-free record is substantially attributable to non-training factors, this reading''s central evidentiary claim collapses and the constraint''s classification would move toward pure extraction (a compliance regime that does not actually track the competence it claims to certify); if the record is genuinely attributable to training adequacy, the coordination function is stronger than currently scored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_free_record_as_validation, empirical, 'Whether the absence of catastrophe is evidence of training sufficiency or a confound from unrelated reliability gains.').

omega_variable(
    simulator_scenario_coverage_completeness,
    'Do simulator scenario libraries, built retrospectively from documented failure modes, adequately anticipate the space of genuinely novel failures crews may encounter, or is there an irreducible category of failure that simulation-based training structurally cannot rehearse in advance?',
    'Systematic review of accident investigations to classify failures by whether they fell inside or outside pre-existing simulator scenario coverage at the time of the event; longitudinal tracking of how quickly novel failure types are incorporated into training libraries after they first occur in the real world.',
    'If a persistent category of unanticipated failure exists that simulation cannot pre-model by construction, the sufficiency claim central to this reading is structurally bounded regardless of fidelity improvements, strengthening the case for the hybrid_dependency or catastrophe_as_necessary_anchor readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_scenario_coverage_completeness, empirical, 'Whether simulation training has a structural blind spot for genuinely novel failure modes.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct unit of analysis ''competence for the certified scenario envelope'' (which simulation plausibly satisfies) or ''competence for the actual operational risk envelope including unmodeled tail events'' (which is what passengers and crews actually need covered)? These are two coherent framings of the same kernel that could produce different classifications for this reading.',
    'Would require an authoritative definition of what ''the competence kernel'' is meant to guarantee — a definitional/regulatory question, not purely an empirical one — and would need buy-in from regulators, unions, and manufacturers on which framing governs certification.',
    'Under the narrower ''certified envelope'' framing, this reading looks closer to a genuine rope (the coordination problem as scoped is solved adequately). Under the broader ''actual risk envelope'' framing, the same facts support a tangled_rope or even snare reading, since the standard is being used to certify something broader than what it demonstrably covers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative framings of what the competence kernel is supposed to guarantee produce different classifications for the same reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.25).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.29).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.32).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.35).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_exercise_requirement kernel. All three share the underlying question of what constitutes adequate exercise of operational competence for rare high-consequence failures, but instantiate structurally distinct claims with different ε: this reading (simulation is sufficient alone) authors moderate extraction reflecting genuine but incompletely verified coordination value; catastrophe_as_necessary_anchor would author a very different structure (near-impossible-to-satisfy standard, potentially high extraction if used to argue current training is inherently inadequate regardless of improvement); hybrid_dependency sits between, requiring both simulation and real-world anchoring and likely showing lower extraction because it hedges against the single-point-of-failure risk this reading accepts. The three are not measurements of one constraint at different fidelity settings — they are readings that would remain distinct even if evidence resolved decisively in favor of one, because they encode different normative commitments about what evidence would count as sufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
