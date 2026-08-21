% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Drills and Inspections as Exercised Competence
 *   domain: Disaster Risk Management / Institutional Memory / Civil Defense Systems
 *
 * SUMMARY:
 *   This constraint describes preparedness drills and inspections as a
 *   mechanism for maintaining and transmitting live, exercised operational
 *   knowledge and adaptive capacity across generations of personnel and
 *   evolving threats. It emphasizes the genuine function of these activities
 *   in re-validating capabilities and fostering improvisation under scenario
 *   variation, ensuring that institutional memory translates into effective
 *   action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.4).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Drills and Inspections as Exercised Competence").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "Disaster Risk Management / Institutional Memory / Civil Defense Systems").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '48173bac-e8ad-4baa-a5af-cae9ca30b878').
narrative_ontology:cs_kernel_codification('48173bac-e8ad-4baa-a5af-cae9ca30b878', formalized).
narrative_ontology:cs_authority_grounding('48173bac-e8ad-4baa-a5af-cae9ca30b878', expertise).
narrative_ontology:cs_interpretation_layer_present('48173bac-e8ad-4baa-a5af-cae9ca30b878').
narrative_ontology:cs_reading_relation('48173bac-e8ad-4baa-a5af-cae9ca30b878', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('48173bac-e8ad-4baa-a5af-cae9ca30b878', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('48173bac-e8ad-4baa-a5af-cae9ca30b878', foundational, operational_knowledge_is_exercised).
narrative_ontology:cs_axiom_status(operational_knowledge_is_exercised, holdable).
narrative_ontology:cs_axiom_grounding('48173bac-e8ad-4baa-a5af-cae9ca30b878', operational_knowledge_is_exercised, empirically_contingent).
narrative_ontology:cs_axiom('48173bac-e8ad-4baa-a5af-cae9ca30b878', foundational, adaptive_capacity_is_learned).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_learned, holdable).
narrative_ontology:cs_axiom_grounding('48173bac-e8ad-4baa-a5af-cae9ca30b878', adaptive_capacity_is_learned, empirically_contingent).
narrative_ontology:cs_reference_frame('48173bac-e8ad-4baa-a5af-cae9ca30b878', proactive_adaptive_preparedness).
narrative_ontology:cs_drift_state('48173bac-e8ad-4baa-a5af-cae9ca30b878', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('48173bac-e8ad-4baa-a5af-cae9ca30b878', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, public_safety).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, citizens).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, budget_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates, designs, and evaluates drills and inspections to ensure public safety. Benefits from validated competence and institutional legitimacy derived from effective preparedness.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Participate in drills, gaining practical experience, validating operational procedures, and building inter-agency trust. They bear the time and effort cost but benefit directly from enhanced readiness and reduced risk in real emergencies.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).

% The ultimate beneficiary of effective preparedness, protected by the competence maintained through drills and inspections. Bears indirect costs through taxes but receives direct benefit of reduced harm.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, public_safety, beneficiary,
    powerless, generational, trapped, local).

% Conduct inspections, identify gaps in preparedness, and ensure compliance with safety standards. Their expertise is validated and enhanced through this process, contributing to overall system competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors, agenda_setter,
    organized, biographical, constrained, regional).

% Bear the indirect costs of preparedness through taxes and minor disruptions during drills. They are direct beneficiaries of the safety and resilience provided by the system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, citizens, payer,
    powerless, immediate, trapped, local).

% Allocate funds for drills and inspections, balancing preparedness needs against other public spending priorities. They bear the financial cost but benefit from public trust and reduced post-disaster recovery expenses.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, budget_authorities, payer,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a shared, validated operational capability across diverse agencies and personnel for effective disaster response, preventing chaotic or ineffective reactions and fostering adaptive capacity.
% TRANSFER_FUNCTION: Transfers time, effort, and financial resources from participating agencies and taxpayers into validated operational knowledge, inter-agency trust, and adaptive capacity for disaster response.
% ABSENT_VOICES: Those who would argue for purely theoretical or 'paper' preparedness, or those who prioritize immediate cost savings over long-term resilience, are often marginalized in the discourse around active drills and inspections.
% DISAPPEARANCE_RATIONALE: If regular drills and inspections vanished overnight, operational knowledge would atrophy, inter-agency coordination would break down, and the capacity for effective disaster response would severely degrade, leading to catastrophic failures in real events and a loss of public trust.
% FOUNDING_PROBLEM: The historical experience of uncoordinated and ineffective responses to major disasters, leading to preventable loss of life and property due to a lack of practiced competence and adaptive capacity.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management experts, historical disaster analyses, and independent risk assessments consistently corroborate the ongoing need for active preparedness to mitigate real and evolving threats, including novel failure signatures.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the costs are primarily for genuine public good, not rent-seeking. Suppression is moderate (0.40) as compliance is mandated by regulation and professional standards, but participants generally recognize the value. Theater ratio is low (0.10) because the focus is on substantive learning and validation, with minimal performative maintenance. Accessibility collapse is moderate-high (0.70) because while alternatives to drills exist (e.g., theoretical training), they are widely recognized as insufficient for building and maintaining true operational competence. Resistance is low (0.20) as the value of preparedness is generally accepted by those involved.
 *
 * PERSPECTIVAL GAP:
 *   While all stakeholders generally agree on the necessity of preparedness, the 'competence_reading' emphasizes the active, adaptive learning aspect, which might be downplayed by budget authorities focused on cost, or by those who view drills as mere compliance exercises. However, the structural benefits are broadly distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and first responders are primary beneficiaries, gaining validated competence and operational readiness. Public safety is the ultimate beneficiary. Citizens and budget authorities are payers, bearing the financial costs. All parties ultimately benefit from the coordination and risk reduction, making it a Rope. The enforcement is for collective good, not asymmetric extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_ambiguity,
    'Is this constraint a genuine competence-building mechanism, or has it degraded into a ritualistic performance (husk_reading) where operational knowledge has hollowed out?',
    'Empirical assessment of drill outcomes under novel, unscripted scenarios; post-incident analysis of adaptive responses to unforeseen challenges; independent audits of inspection efficacy beyond mere compliance checklists.',
    'If found to be ritualistic, the constraint''s effective extractiveness and theater_ratio would be higher, and its classification would shift towards a Piton or Snare, indicating a loss of functional value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_ambiguity, empirical, 'Distinguishing genuine competence from performative ritual in preparedness activities.').

omega_variable(
    uniform_vs_stratified_competence,
    'Is preparedness transmission uniformly effective across all domains, or is it stratified (hybrid_reading) with some areas (e.g., physical infrastructure) maintaining high competence while others (e.g., civilian coordination) have decayed?',
    'Comparative analysis of competence levels across different sub-domains of disaster preparedness (e.g., engineering vs. public communication); cross-jurisdictional studies of drill efficacy in varied contexts.',
    'If stratified, the ''competence_reading'' would need to be refined or decomposed into sub-constraints, with some components potentially reclassified as more extractive or degraded (e.g., Tangled Rope or Piton for decayed areas).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_vs_stratified_competence, empirical, 'Assessing the uniformity of competence transmission across different aspects of preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_transmission__competence_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__competence_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__competence_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__competence_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__competence_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__competence_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_transmission__competence_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__competence_reading, base_extractiveness, 1990, 0.16).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__competence_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__competence_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__competence_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__competence_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_transmission__competence_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__competence_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__competence_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__competence_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__competence_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__competence_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
