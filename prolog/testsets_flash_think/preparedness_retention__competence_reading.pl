% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: Disaster Preparedness / Institutional Memory / Governance
 *
 * SUMMARY:
 *   This constraint represents the 'competence reading' of preparedness,
 *   where drills and inspections are understood as genuinely
 *   competence-preserving practices. It emphasizes that preparedness is not a
 *   static state but a dynamic, live exercised knowledge that maintains
 *   operational capacity. Resource allocation is optimized for skill
 *   retention and adaptive capacity, with the primary beneficiary being
 *   population safety and no identifiable victims of extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "Disaster Preparedness / Institutional Memory / Governance").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'b0910cc8-4b98-4c83-aae7-007af914ef10').
narrative_ontology:cs_kernel_codification('b0910cc8-4b98-4c83-aae7-007af914ef10', formalized).
narrative_ontology:cs_authority_grounding('b0910cc8-4b98-4c83-aae7-007af914ef10', expertise).
narrative_ontology:cs_interpretation_layer_present('b0910cc8-4b98-4c83-aae7-007af914ef10').
narrative_ontology:cs_reading_relation('b0910cc8-4b98-4c83-aae7-007af914ef10', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('b0910cc8-4b98-4c83-aae7-007af914ef10', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b0910cc8-4b98-4c83-aae7-007af914ef10', foundational, competence_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(competence_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('b0910cc8-4b98-4c83-aae7-007af914ef10', competence_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('b0910cc8-4b98-4c83-aae7-007af914ef10', secondary, adaptive_capacity_is_paramount).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b0910cc8-4b98-4c83-aae7-007af914ef10', adaptive_capacity_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('b0910cc8-4b98-4c83-aae7-007af914ef10', adaptive_competence_framework).
narrative_ontology:cs_drift_state('b0910cc8-4b98-4c83-aae7-007af914ef10', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b0910cc8-4b98-4c83-aae7-007af914ef10', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, public_health_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, government_budget_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and overseeing preparedness drills and inspections. They allocate resources, set standards, and ensure compliance to maintain operational capacity. Their legitimacy depends on effective outcomes.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, public_safety_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Directly participate in drills and benefit from the maintained competence and coordination. They are the primary operational agents during a crisis, and their effectiveness is a direct measure of the constraint's success. They bear the time cost of training.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).

% Integrate their response plans with other agencies through drills, ensuring a coordinated public health response to disasters. They benefit from shared knowledge and tested protocols, contributing to overall population resilience.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, public_health_agencies, beneficiary,
    institutional, generational, constrained, national).

% The ultimate beneficiary of effective preparedness, gaining safety and resilience in the face of disasters. They rely on the competence of response agencies and bear indirect costs through taxes funding preparedness efforts.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population, beneficiary,
    powerless, immediate, trapped, local).

% Allocate public funds for preparedness activities, including drills, training, and equipment maintenance. They face pressure to optimize fiscal efficiency but recognize the long-term costs of inadequate preparedness.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, government_budget_managers, payer,
    institutional, biographical, constrained, national).

% Academics, think tanks, and international bodies who study disaster response, institutional memory, and organizational learning. They assess the effectiveness of preparedness practices and identify best practices or areas for improvement.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure that diverse agencies and personnel can effectively coordinate complex responses during a crisis, by maintaining shared operational knowledge, skills, and adaptive capacity through regular exercise.
% TRANSFER_FUNCTION: Transfers theoretical knowledge and procedural guidelines into practical, exercised competence and operational readiness across all levels of disaster response, from individual first responders to inter-agency command structures.
% ABSENT_VOICES: Those who prioritize short-term fiscal savings over long-term readiness, arguing that drills are an unnecessary expense or that 'common sense' is sufficient. Also, those who believe technology alone can substitute for human competence.
% DISAPPEARANCE_RATIONALE: If the constraint of 'live exercised knowledge' vanished, operational capacity would rapidly degrade. Agencies would lose coordination, skills would atrophy, and the ability to respond effectively to any significant disaster would collapse, leading to catastrophic failures and loss of life. The entire framework of public safety would need to be rebuilt from scratch.
% FOUNDING_PROBLEM: Preventing catastrophic failures in complex, high-stakes environments where ad-hoc responses are insufficient and institutional memory is prone to decay, ensuring a reliable capacity to act under pressure.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-disaster reviews consistently highlight the critical role of exercised competence in successful responses and the severe consequences of its absence. Scientific studies of organizational learning and human factors in high-reliability organizations corroborate the necessity of continuous practice for skill retention, from sources like the National Academies of Sciences and academic journals in emergency management.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) reflects that the costs associated with this constraint are primarily investments in collective safety and operational efficiency, not rents. The low suppression (0.20) indicates that compliance with drills and inspections is largely driven by a shared understanding of their necessity for public good, rather than coercion. The very low theater ratio (0.10) is central to this reading, asserting that activities are functional and effective, not merely performative. Accessibility collapse is moderate (0.40) because while ad-hoc alternatives are less effective, they are not entirely foreclosed, and genuine competence requires specific, non-trivial training paths. Resistance is low (0.15) as the value of true preparedness is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in stark contrast to the 'husk reading' (where practices are purely ceremonial) and the 'hybrid reading' (where competence is stratified and often degraded). From this perspective, any observed failures in preparedness are due to a deviation from the ideal of live exercised knowledge, not an inherent flaw in the concept itself. The engine's classification will highlight whether the metrics align with this ideal 'rope' type or if there's a divergence indicating a more extractive or performative reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies, first responders, and public health agencies are direct beneficiaries, gaining the operational capacity and coordination necessary for their missions. The population is the ultimate beneficiary, receiving enhanced safety. Government budget managers are payers, allocating resources for these essential services. There are no victims in this reading, as the investment is considered optimal for the collective good.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_ambiguity,
    'Is the observed ''exercised knowledge'' truly competence-preserving, or is it primarily a ceremonial performance that lacks live operational capacity?',
    'Post-crisis performance analysis, independent operational audits, and ''no-notice'' drills with objective performance metrics. If performance consistently degrades under real-world conditions despite ''exercised knowledge'', reclassify as ceremonial.',
    'If ceremonial, the constraint''s effective extractiveness and theater_ratio are significantly higher, shifting its classification towards a ''snare'' or ''piton'' as resources are consumed for non-functional activity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_vs_ceremony_ambiguity, empirical, 'Distinguishing genuine competence from performative ritual in preparedness practices.').

omega_variable(
    stratified_competence_ambiguity,
    'Is competence uniformly maintained across all relevant institutions and levels, or is it stratified, with high competence in specialized areas and ceremonial performance elsewhere (as per the ''hybrid_reading'')?',
    'Granular, multi-level assessment of competence across different institutional actors and operational tiers. If significant disparities are found, the ''hybrid_reading'' gains empirical support.',
    'If stratified, the ''competence_reading'' is an incomplete description. The constraint would need to be decomposed further, or its classification would become a ''tangled_rope'' or ''snare'' for those parts of the system where competence is not genuinely maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratified_competence_ambiguity, empirical, 'Assessing the uniformity of competence retention across the preparedness system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__competence_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(prep_tr_t1997, preparedness_retention__competence_reading, theater_ratio, 1997, 0.09).
narrative_ontology:measurement(prep_tr_t2004, preparedness_retention__competence_reading, theater_ratio, 2004, 0.09).
narrative_ontology:measurement(prep_tr_t2011, preparedness_retention__competence_reading, theater_ratio, 2011, 0.1).
narrative_ontology:measurement(prep_tr_t2018, preparedness_retention__competence_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__competence_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__competence_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(prep_be_t1997, preparedness_retention__competence_reading, base_extractiveness, 1997, 0.13).
narrative_ontology:measurement(prep_be_t2004, preparedness_retention__competence_reading, base_extractiveness, 2004, 0.14).
narrative_ontology:measurement(prep_be_t2011, preparedness_retention__competence_reading, base_extractiveness, 2011, 0.14).
narrative_ontology:measurement(prep_be_t2018, preparedness_retention__competence_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__competence_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_retention__competence_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(prep_su_t1997, preparedness_retention__competence_reading, suppression_requirement, 1997, 0.19).
narrative_ontology:measurement(prep_su_t2004, preparedness_retention__competence_reading, suppression_requirement, 2004, 0.19).
narrative_ontology:measurement(prep_su_t2011, preparedness_retention__competence_reading, suppression_requirement, 2011, 0.2).
narrative_ontology:measurement(prep_su_t2018, preparedness_retention__competence_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(prep_su_t2025, preparedness_retention__competence_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
