% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'national liberation' reading of combatant
 *   status, primarily derived from Additional Protocol I (AP I) Article 1(4).
 *   It extends combatant status to non-state armed groups fighting against
 *   colonial domination, alien occupation, or racist regimes, provided they
 *   meet criteria of organization and command control. This reading is highly
 *   contested by many states, particularly those engaged in such conflicts,
 *   who prefer a more restrictive, state-centric definition. The constraint
 *   functions as a Tangled Rope: it offers a coordination mechanism
 *   (reciprocal LOAC adherence) but also imposes significant extraction on
 *   the states it targets by legitimizing their adversaries and obligating
 *   them to grant POW status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.65).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.78).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '3009f438-db96-4788-a603-d477dea492ae').
narrative_ontology:cs_kernel_codification('3009f438-db96-4788-a603-d477dea492ae', formalized).
narrative_ontology:cs_authority_grounding('3009f438-db96-4788-a603-d477dea492ae', lineage).
narrative_ontology:cs_interpretation_layer_present('3009f438-db96-4788-a603-d477dea492ae').
narrative_ontology:cs_reading_relation('3009f438-db96-4788-a603-d477dea492ae', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('3009f438-db96-4788-a603-d477dea492ae', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('3009f438-db96-4788-a603-d477dea492ae', foundational, self_determination_struggle_legitimacy).
narrative_ontology:cs_axiom_status(self_determination_struggle_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3009f438-db96-4788-a603-d477dea492ae', self_determination_struggle_legitimacy, deontological).
narrative_ontology:cs_axiom('3009f438-db96-4788-a603-d477dea492ae', foundational, api_1_4_expands_combatant_status).
narrative_ontology:cs_axiom_status(api_1_4_expands_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('3009f438-db96-4788-a603-d477dea492ae', api_1_4_expands_combatant_status, conventional).
narrative_ontology:cs_reference_frame('3009f438-db96-4788-a603-d477dea492ae', post_api_1_adoption_framework).
narrative_ontology:cs_drift_state('3009f438-db96-4788-a603-d477dea492ae', contemporary_counter_terrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3009f438-db96-4788-a603-d477dea492ae', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_regimes).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, racist_regimes).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, anti_colonial_struggle_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain conditional combatant status, which, if recognized, grants their captured members POW protections. This legitimizes their struggle under international law, but requires adherence to LOAC themselves. Their identity is often fused with the struggle for liberation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, identity_locked, regional).

% These states are obligated to grant combatant status and POW protections to members of qualifying national liberation movements, which complicates their counter-insurgency operations and confers legitimacy on their adversaries. They often resist this interpretation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, biographical, constrained, national).

% Similar to occupying powers, these regimes face legal obligations to treat captured members of national liberation movements as combatants, undermining their claims of internal policing actions against 'terrorists' or 'criminals'.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_regimes, payer,
    institutional, generational, constrained, national).

% Regimes founded on racial discrimination are challenged by this reading, which grants combatant status to those fighting against them, forcing them to adhere to LOAC standards for their adversaries and eroding their domestic and international legitimacy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, racist_regimes, payer,
    institutional, generational, constrained, national).

% Analyze and interpret the application of AP I Article 1(4), often advocating for its broader application to protect those fighting for self-determination. Their work influences state practice and judicial interpretation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% Can prosecute individuals for war crimes, including denying POW status to those entitled to it under AP I. This provides a mechanism for enforcement, but its jurisdiction is limited by state ratification and political will.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_criminal_court, agenda_setter,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing legitimate armed struggle for self-determination from mere banditry or terrorism, offering a pathway for conditional combatant status and reciprocal adherence to LOAC by both sides.
% TRANSFER_FUNCTION: Transfers conditional combatant immunity and POW protections to members of qualifying national liberation movements, and imposes corresponding obligations on the states they fight against.
% ABSENT_VOICES: States that are not party to AP I, or those that have made reservations to Article 1(4), are effectively absent from the direct application of this reading, though they may be influenced by customary international law. Non-state armed groups that do not meet the organization and command-control criteria are also excluded.
% DISAPPEARANCE_RATIONALE: If this reading vanished, national liberation movements would lose a key legal basis for their struggle, potentially leading to their members being universally treated as criminals or terrorists, increasing the brutality of conflicts, and removing a mechanism for accountability for occupying powers.
% FOUNDING_PROBLEM: The original Geneva Conventions (1949) did not adequately address the status of combatants in non-international armed conflicts, particularly those involving national liberation struggles against colonial or racist regimes, leaving such fighters without clear protections.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and many states (particularly those that emerged from colonial rule) corroborate that the problem of protecting fighters for self-determination remains live, citing ongoing conflicts and debates over status. Opposing states often contest the scope of the problem, arguing existing law is sufficient.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading imposes significant legal and operational costs on states fighting against national liberation movements, forcing them to treat adversaries as combatants rather than criminals. Suppression (0.78) is also high, as states actively resist this interpretation through legal arguments, non-ratification of AP I, and practical denial of status. Theater ratio is low (0.20) because while there's some performative adherence, the core function of legitimizing liberation movements and constraining opposing states is real and actively fought over. Accessibility collapse is moderate (0.40) as states have other legal avenues (e.g., Common Article 3) but this specific pathway to combatant status is crucial. Resistance is very high (0.85) due to the direct challenge this reading poses to state sovereignty and military operations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national liberation movements, this is a vital Rope, providing necessary protections and legitimizing their struggle. From the perspective of the targeted states, it is a Snare, imposing unjust obligations and undermining their sovereignty. The engine's classification as Tangled Rope reflects this inherent tension between coordination (for the movements) and extraction (from the states).
 *
 * DIRECTIONALITY LOGIC:
 *   National liberation movements are beneficiaries (d=0.0-0.2) as they gain legal protections and legitimacy. Occupying, colonial, and racist regimes are clear targets (d=0.8-1.0) as the constraint imposes obligations and costs on them. IHL scholars and the ICC act as observers/agenda-setters, influencing the interpretation and enforcement of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organization_command_control_ambiguity,
    'What specific criteria constitute ''organized'' and ''command-controlled'' for non-state armed groups under AP I Article 1(4), and how consistently are they applied?',
    'Development of clearer international jurisprudence and state practice, potentially through ICJ advisory opinions or consistent ICC rulings.',
    'Clearer criteria would reduce the ability of states to deny combatant status arbitrarily, increasing the effective protection for national liberation movements and reducing the ambiguity for all parties. Ambiguity allows states to maintain higher extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organization_command_control_ambiguity, empirical, 'Ambiguity in criteria for non-state armed group combatant status.').

omega_variable(
    state_acceptance_of_api_1_4,
    'To what extent has AP I Article 1(4) achieved customary international law status, binding even states not party to the Protocol?',
    'Analysis of state practice (opinio juris) and judicial decisions over time, particularly from non-signatory states.',
    'If customary status is widely accepted, the constraint''s reach and extractiveness on targeted states would increase significantly, as they could not simply opt out by non-ratification. If not, its application remains limited to treaty parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_acceptance_of_api_1_4, empirical, 'Customary international law status of AP I Article 1(4).').

omega_variable(
    kernel_reading_divergence,
    'Is this ''national liberation'' reading fundamentally compatible with the ''state-centric'' reading within a single coherent framework of international law, or do they represent irreconcilable normative commitments?',
    'Continued legal scholarship and international judicial decisions attempting to reconcile or explicitly choose between these interpretations. The persistence of active legal and political contestation suggests deep incompatibility.',
    'If irreconcilable, the ''combatant status definition'' kernel is fundamentally unstable, leading to persistent legal uncertainty and politicized application. If reconcilable, a more unified framework could emerge, reducing the ''tangled'' aspect of this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Conceptual compatibility of national liberation and state-centric readings of combatant status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(comb_tr_t2000, combatant_status_definition__national_liberation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(comb_be_t2000, combatant_status_definition__national_liberation_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.7).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(comb_su_t2000, combatant_status_definition__national_liberation_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, detainee_treatment_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel. It focuses on AP I Article 1(4) and its application to national liberation movements. Its extractiveness and beneficiary/victim structure differ significantly from the 'state-centric' and 'functional protection' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
