% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint story instantiates the competence_reading of the
 *   preparedness_retention kernel: the claim that drills and inspections are
 *   genuine competence-preserving practices, not ceremonial performances. The
 *   reading asserts a low ceremony-to-competence ratio — resource allocation
 *   optimizes for skill retention and adaptive capacity. The primary
 *   beneficiary is population safety; the only potential victim is fiscal
 *   efficiency when investment exceeds the competence-retention inflection
 *   point. This reading stands in structural tension with two sibling
 *   readings: husk_reading (which claims drills are memorial performance
 *   lacking live competence) and hybrid_reading (which claims competence is
 *   stratified — retained in specialized institutions but ceremonial
 *   elsewhere).
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional/constrained) — designs and mandates drill regimes
 *   - population_at_risk: beneficiary (organized/constrained) — receives safety dividend
 *   - emergency_response_personnel: beneficiary/payer (organized/mobile) — gains skill retention, bears participation cost
 *   - critical_infrastructure_operators: beneficiary/payer (powerful/arbitrage) — funds specialized drills, gains reliability
 *   - fiscal_authorities_when_overinvested: payer (institutional/constrained) — bears marginal cost past inflection point
 *   - accreditation_and_standards_bodies: observer (analytical/analytical) — sets competence benchmarks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '45072d1c-eb29-48b4-99d6-2b2aa2fe0033').
narrative_ontology:cs_kernel_codification('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', distributed).
narrative_ontology:cs_authority_grounding('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', practice).
narrative_ontology:cs_interpretation_layer_present('45072d1c-eb29-48b4-99d6-2b2aa2fe0033').
narrative_ontology:cs_reading_relation('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', foundational, drills_preserve_operational_competence).
narrative_ontology:cs_axiom_status(drills_preserve_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', drills_preserve_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', foundational, ceremony_competence_ratio_is_low).
narrative_ontology:cs_axiom_status(ceremony_competence_ratio_is_low, holdable).
narrative_ontology:cs_axiom_grounding('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', ceremony_competence_ratio_is_low, empirically_contingent).
narrative_ontology:cs_axiom('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', secondary, resource_allocation_optimizes_for_skill_retention).
narrative_ontology:cs_axiom_status(resource_allocation_optimizes_for_skill_retention, holdable).
narrative_ontology:cs_axiom_grounding('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', resource_allocation_optimizes_for_skill_retention, instrumental).
narrative_ontology:cs_reference_frame('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', live_exercised_competence_framework).
narrative_ontology:cs_drift_state('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', contemporary_institutional_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('45072d1c-eb29-48b4-99d6-2b2aa2fe0033', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_at_risk).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, critical_infrastructure_operators).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, fiscal_authorities_when_overinvested).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, critical_infrastructure_operators).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, competence_decays_without_exercise).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, adaptive_capacity_requires_regular_stress_testing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, mandate, and fund drill cycles and inspection regimes. They set scenario complexity, frequency standards, and after-action review protocols. Their authority derives from statutory mandate and professional accreditation. Exit means leaving the profession or jurisdiction — costly but possible.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive the safety dividend of maintained response competence — faster evacuation, effective sheltering, restored services. They do not run drills but their survival depends on them. Exit is geographic relocation or political advocacy; neither is trivial.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_at_risk, beneficiary,
    organized, biographical, constrained, regional).

% Gain skill retention and team cohesion from regular exercises; bear the time, physical risk, and opportunity cost of participation. Many are volunteers or cross-trained staff. Exit is changing role or employer — feasible but loses institutional memory.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_personnel, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_personnel, payer).

% Water boards, grid operators, port authorities — they rely on exercised competence for continuity of service. They fund specialized drills and gain operational reliability. Exit means regulatory non-compliance or asset sale; both carry extreme cost.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, critical_infrastructure_operators, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, critical_infrastructure_operators, payer).

% Bear the marginal cost when drill frequency or scale exceeds the competence-retention inflection point. They audit cost-effectiveness and can redirect budgets. Exit means political reallocation — constrained by mandate and public scrutiny.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_authorities_when_overinvested, payer,
    institutional, immediate, constrained, national).

% Define competence benchmarks, certify exercise programs, and publish after-action methodologies. They do not run drills but set the yardstick for 'live exercised knowledge.' Their exit is intellectual — revising standards.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, accreditation_and_standards_bodies, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the competence decay problem: individual and collective emergency response skills degrade without regular, realistic exercise. Drills and inspections synchronize distributed actors, validate procedures, and reveal gaps before a real event.
% TRANSFER_FUNCTION: Moves resources (budget, personnel time, equipment wear) from fiscal authorities and operating agencies into exercised competence — maintained procedural fluency, calibrated team coordination, verified equipment readiness, and updated situational awareness.
% ABSENT_VOICES: Communities that have never experienced a major disaster and therefore cannot viscerally validate the competence claim; future generations who inherit the preparedness level but had no say in its calibration; marginalized groups whose specific vulnerabilities may not be exercised in standard scenarios.
% DISAPPEARANCE_RATIONALE: If mandated drills and inspections vanished, competence would decay on a known curve (skills half-life 6–18 months). Response times would lengthen, coordination would fracture, equipment failures would go undetected. The safety dividend would erode measurably within 2–3 cycles.
% FOUNDING_PROBLEM: After several major disasters where response failed due to unexercised plans, rusty skills, and untested coordination, governments instituted mandatory drill cycles to convert static plans into living capability.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from recent events (e.g., 2021 European floods, 2023 Türkiye-Syria earthquake) show exercised units outperformed non-exercised ones. Independent academic studies (e.g., PERC 2022, NIST disaster resilience grants) corroborate the competence-decay curve. No major emergency management body disputes the founding problem's persistence.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the transfer function moves resources into demonstrated competence, not into private capture. Suppression is low (0.15) — enforcement is professional accreditation and statutory mandate, not coercion of unwilling participants; most response personnel volunteer for exercises. Theater ratio is low (0.18) — after-action reviews genuinely change procedures; the gap between exercise and real event is the target of improvement, not a performance. Accessibility collapse is moderate (0.35) — alternatives (ad-hoc response, mutual aid) exist but are reliably inferior. Resistance is low (0.25) — the constraint is widely accepted as necessary by those it governs. The slight upward drift in all three metrics over 40 years reflects bureaucratization of exercise programs (more paperwork, compliance checking) without proportional competence gain.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies sit near the beneficiary end (d ~ 0.2) — they control the constraint and gain institutional legitimacy. Population at risk is full beneficiary (d ~ 0.05) — pure safety gain, no cost. Response personnel are near symmetric (d ~ 0.45) — gain skills, pay time/risk. Infrastructure operators are beneficiary-leaning (d ~ 0.3) — high gain, high voluntary investment. Fiscal authorities when overinvested are the only target (d ~ 0.75) — they pay marginal excess. Standards bodies are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence decay without exercise) remains live — corroborated by independent after-action studies. The constraint has not outlived its function. Mandatrophy risk is low but rising slightly as exercise programs accumulate compliance overhead (theater_ratio drift from 0.12 to 0.18). The reading's axioms are empirically contingent — if evidence showed drills don't improve outcomes, the reading would be falsified, not foreclosed by drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_reading_kernel_framing,
    'Does the competence_reading accurately describe the dominant preparedness regime, or does it describe an aspirational ideal that the hybrid_reading better captures as a stratified reality?',
    'Comparative analysis of drill outcomes across institution types: if specialized institutions (water boards, nuclear regulators) show high competence-retention while generalist agencies show ceremonial patterns, the hybrid_reading''s stratification claim is empirically supported and the competence_reading applies only to a subset.',
    'If hybrid_reading''s stratification is validated, the competence_reading''s claim of uniformly low ceremony-to-competence ratio is falsified for the general case — the constraint family would need domain-specific decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_kernel_framing, empirical, 'Whether the competence_reading describes the whole kernel or only the specialized-institution stratum.').

omega_variable(
    inflection_point_fiscal_victim,
    'At what investment level does preparedness spending shift from competence-retention to fiscal waste, making fiscal authorities genuine victims?',
    'Marginal analysis of exercise frequency vs. measured competence retention curves; identify the plateau where additional drills yield no measurable skill improvement.',
    'If the inflection point is low and current spending exceeds it, the fiscal_authorities_when_overinvested victim class is large and the constraint trends toward tangled_rope. If spending is below inflection, the victim class is theoretical and the constraint remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inflection_point_fiscal_victim, empirical, 'Location of the competence-retention inflection point relative to current expenditure.').

omega_variable(
    husk_reading_foreclosure_boundary,
    'Does the competence_reading''s core premise (drills preserve live competence) logically foreclose the husk_reading (drills are ceremonial), or can both be true at different organizational levels?',
    'Logical analysis of the two claims: if ''drills preserve competence'' is a universal claim about the practice, it contradicts ''drills are ceremonial.'' If it is an existential claim (''some drills preserve competence''), they coexist. The reading_relations declaration depends on this distinction.',
    'If forecloses, the kernel has a genuine logical schism; if coexists_with, the kernel hosts a stratification that the hybrid_reading explicitly models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(husk_reading_foreclosure_boundary, conceptual, 'Logical relationship between competence_reading and husk_reading premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__competence_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__competence_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__competence_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__competence_reading, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__competence_reading, base_extractiveness, 16, 0.1).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.11).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__competence_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__competence_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__competence_reading, suppression_requirement, 16, 0.13).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__competence_reading, suppression_requirement, 24, 0.14).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__competence_reading, suppression_requirement, 32, 0.15).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the preparedness_retention kernel into three structurally distinct readings. The competence_reading claims uniformly low ceremony-to-competence ratio (rope). The husk_reading claims high ceremony-to-competence ratio (piton/snare). The hybrid_reading claims stratified competence (tangled_rope at system level). They are linked because each cites the same institutional practices but interprets their function differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__competence_reading, institutional, 0.2).
constraint_indexing:directionality_override(preparedness_retention__competence_reading, organized, 0.15).
constraint_indexing:directionality_override(preparedness_retention__competence_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
