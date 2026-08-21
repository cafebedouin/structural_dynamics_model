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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'competence_reading' of the
 *   'preparedness_retention' kernel. It describes preparedness as the active,
 *   ongoing maintenance of operational capacity through genuine drills and
 *   inspections, where knowledge is live and exercised. The focus is on
 *   effective resource allocation to achieve population safety, with minimal
 *   extraction or theatricality. This reading posits that the constraint
 *   functions as a genuine coordination mechanism.
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
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '4657a58f-7918-4b05-b463-0123c0c4e5ca').
narrative_ontology:cs_kernel_codification('4657a58f-7918-4b05-b463-0123c0c4e5ca', implicit).
narrative_ontology:cs_authority_grounding('4657a58f-7918-4b05-b463-0123c0c4e5ca', practice).
narrative_ontology:cs_interpretation_layer_present('4657a58f-7918-4b05-b463-0123c0c4e5ca').
narrative_ontology:cs_reading_relation('4657a58f-7918-4b05-b463-0123c0c4e5ca', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('4657a58f-7918-4b05-b463-0123c0c4e5ca', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4657a58f-7918-4b05-b463-0123c0c4e5ca', foundational, competence_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(competence_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('4657a58f-7918-4b05-b463-0123c0c4e5ca', competence_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('4657a58f-7918-4b05-b463-0123c0c4e5ca', foundational, operational_capacity_requires_active_maintenance).
narrative_ontology:cs_axiom_status(operational_capacity_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('4657a58f-7918-4b05-b463-0123c0c4e5ca', operational_capacity_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('4657a58f-7918-4b05-b463-0123c0c4e5ca', adaptive_capacity_paradigm).
narrative_ontology:cs_drift_state('4657a58f-7918-4b05-b463-0123c0c4e5ca', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4657a58f-7918-4b05-b463-0123c0c4e5ca', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, governing_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, first_responders).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, taxpayers).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, adaptive_capacity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, institutional_resilience_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for establishing and funding preparedness policies, including mandates for drills and inspections. They benefit from a safe, resilient population and effective crisis response, which enhances their legitimacy.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, governing_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Participate in and conduct drills, training, and inspections, investing significant time and effort. They directly benefit from enhanced operational capacity and competence, leading to safer and more effective responses during actual emergencies.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, first_responders, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, first_responders, beneficiary).

% The ultimate beneficiary of genuine preparedness. A well-prepared system directly reduces harm, loss of life, and disruption during crises. This is an abstract good, not an active agent.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_safety, beneficiary,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(preparedness_retention__competence_reading, population_safety).

% Fund the resources required for drills, training, and inspections through taxes. While they bear the fiscal cost, they indirectly benefit from the safety and stability provided by effective preparedness.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, taxpayers, payer,
    moderate, immediate, constrained, national).

% Analyze preparedness frameworks, evaluate drill effectiveness, and advise on best practices. They provide an external, evidence-based perspective on whether practices genuinely maintain competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_management_experts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the knowledge, skills, and resources of various agencies and individuals to ensure a coherent and effective response to large-scale emergencies, preventing fragmentation and ensuring interoperability.
% TRANSFER_FUNCTION: Transfers public funds and personnel time into training, drills, and maintenance of operational capacity, yielding a collective good of enhanced safety and resilience for the population.
% ABSENT_VOICES: Those who prioritize short-term fiscal savings or political expediency over long-term, sustained investment in genuine operational capacity. They would argue for reduced budgets for 'non-crisis' activities.
% DISAPPEARANCE_RATIONALE: If the practices of live exercised knowledge vanished, operational capacity would rapidly degrade, leading to catastrophic failures, increased casualties, and prolonged recovery times during any significant crisis. The entire disaster response ecosystem would collapse.
% FOUNDING_PROBLEM: Preventing catastrophic societal collapse and loss of life from natural disasters, pandemics, technological failures, or other large-scale emergencies by ensuring a ready and capable response.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management experts, public safety advocates, and historical analyses of past disasters (e.g., Hurricane Katrina, COVID-19 response failures) consistently corroborate the ongoing need for robust preparedness and the live nature of the problem.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.15) is low, representing the necessary overhead and investment for maintaining complex operational capacity, not rent-seeking. Suppression (0.2) is also low, reflecting the active but generally accepted enforcement of training standards and participation in drills, which are seen as beneficial by participants. The theater ratio (0.1) is minimal, as this reading emphasizes functional, competence-building activities over performative rituals. Accessibility collapse (0.4) is moderate because achieving genuine competence requires significant investment and effort, making 'easy' alternatives less viable. Resistance (0.1) is low because the value of true preparedness is widely acknowledged, though fiscal pressures can create friction.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes genuine competence, other readings of the 'preparedness_retention' kernel (e.g., 'husk_reading') would argue that much of what is called 'preparedness' is performative rather than functional. This story, however, focuses on the ideal operation where competence is truly maintained.
 *
 * DIRECTIONALITY LOGIC:
 *   Governing institutions and first responders are both beneficiaries (of safety and competence) and payers (of resources and effort). The population is the primary beneficiary of the resulting safety. Taxpayers are payers of the financial costs. The constraint's structure is largely symmetric, aiming for collective benefit through coordinated action.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_ambiguity,
    'Is the observed ''preparedness'' truly competence-preserving (as per this reading) or merely ceremonial performance (as per the ''husk_reading'')?',
    'Post-event performance analysis: if actual crisis response consistently fails despite high ''preparedness'' activity, it indicates ceremonial rather than live competence.',
    'If ceremonial, the constraint''s true theater_ratio is higher, and its classification shifts towards Piton or Snare, as resources are extracted for a non-functional purpose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_vs_ceremony_ambiguity, empirical, 'Distinguishing genuine competence from ritualistic performance in preparedness activities.').

omega_variable(
    uniform_vs_stratified_competence,
    'Is operational competence uniformly maintained across all relevant institutions and levels, or is it stratified (as per the ''hybrid_reading'')?',
    'Cross-institutional audit: detailed assessment of competence levels in different sectors (e.g., specialized agencies vs. general public services) to detect significant disparities.',
    'If stratified, this reading''s assumption of broad competence is challenged, and the constraint''s effective coordination function is limited to specific, highly competent pockets, while other areas may operate as a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_vs_stratified_competence, empirical, 'Assessing the distribution and uniformity of maintained operational competence.').

omega_variable(
    fiscal_efficiency_tradeoff,
    'Is the current investment in preparedness optimal for competence retention, or is it over-invested, leading to fiscal inefficiency?',
    'Cost-benefit analysis comparing preparedness investment to avoided damages and recovery costs, benchmarked against international best practices.',
    'If over-invested, the constraint, while still a Rope, might be seen as having a minor extractive component from taxpayers due to inefficient resource allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_efficiency_tradeoff, empirical, 'Evaluating the fiscal efficiency of preparedness investments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__competence_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(prep_tr_t2000, preparedness_retention__competence_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(prep_tr_t2010, preparedness_retention__competence_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__competence_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__competence_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(prep_be_t2000, preparedness_retention__competence_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(prep_be_t2010, preparedness_retention__competence_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__competence_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_retention__competence_reading, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(prep_su_t2000, preparedness_retention__competence_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(prep_su_t2010, preparedness_retention__competence_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(prep_su_t2025, preparedness_retention__competence_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, disaster_response_effectiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
