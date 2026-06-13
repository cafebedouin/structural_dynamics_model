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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Art. 1(4) Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'national liberation' reading of combatant
 *   status, primarily derived from Additional Protocol I (AP I) Article 1(4).
 *   It extends combatant status, and thus potential prisoner of war (POW)
 *   protections, to members of non-state armed groups fighting against
 *   colonial domination, alien occupation, or racist regimes, provided they
 *   meet criteria of organization and command. This reading is highly
 *   contested by states that prefer a more restrictive, state-centric
 *   definition of combatant status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.65).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.75).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Art. 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '0e204610-b5cb-4b6d-a03a-b452d86c8187').
narrative_ontology:cs_kernel_codification('0e204610-b5cb-4b6d-a03a-b452d86c8187', fixed_text).
narrative_ontology:cs_authority_grounding('0e204610-b5cb-4b6d-a03a-b452d86c8187', lineage).
narrative_ontology:cs_interpretation_layer_present('0e204610-b5cb-4b6d-a03a-b452d86c8187').
narrative_ontology:cs_reading_relation('0e204610-b5cb-4b6d-a03a-b452d86c8187', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e204610-b5cb-4b6d-a03a-b452d86c8187', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('0e204610-b5cb-4b6d-a03a-b452d86c8187', foundational, struggle_against_colonialism_occupation_racism_is_legitimate).
narrative_ontology:cs_axiom_status(struggle_against_colonialism_occupation_racism_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0e204610-b5cb-4b6d-a03a-b452d86c8187', struggle_against_colonialism_occupation_racism_is_legitimate, deontological).
narrative_ontology:cs_axiom('0e204610-b5cb-4b6d-a03a-b452d86c8187', foundational, non_state_actors_can_be_combatants_if_organized).
narrative_ontology:cs_axiom_status(non_state_actors_can_be_combatants_if_organized, holdable).
narrative_ontology:cs_axiom_grounding('0e204610-b5cb-4b6d-a03a-b452d86c8187', non_state_actors_can_be_combatants_if_organized, conventional).
narrative_ontology:cs_reference_frame('0e204610-b5cb-4b6d-a03a-b452d86c8187', ap1_expanded_combatant_status).
narrative_ontology:cs_drift_state('0e204610-b5cb-4b6d-a03a-b452d86c8187', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e204610-b5cb-4b6d-a03a-b452d86c8187', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, detained_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_racist_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain conditional combatant status and potential POW protections for their fighters, legitimizing their struggle under international law. Their identity is fused with the cause of liberation, making exit unthinkable.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, identity_locked, regional).

% Individual fighters, if captured, are entitled to POW status and humane treatment, shielding them from prosecution as common criminals. Their situation is entirely dependent on the recognition of their movement's status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, detained_fighters, beneficiary,
    powerless, immediate, trapped, local).

% These states bear the cost of granting combatant immunity to insurgents, which complicates their counter-insurgency operations and limits their ability to prosecute captured fighters. They are constrained by international legal obligations but seek to minimize their application.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, biographical, constrained, national).

% These regimes are obligated to recognize the legitimacy of armed resistance against them, undermining their claims to sovereign authority and increasing the political and legal costs of their rule. Their exit options are limited by the international community's condemnation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_racist_regimes, payer,
    institutional, generational, constrained, national).

% These bodies interpret and enforce international humanitarian law, including AP I Article 1(4), by prosecuting war crimes. Their rulings shape the application of combatant status and influence state behavior.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_criminal_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% These organizations monitor compliance with international humanitarian law, advocate for the rights of national liberation movements and their fighters, and pressure states to adhere to AP I Article 1(4). They provide critical external scrutiny.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal framework for armed conflicts involving non-state actors fighting specific types of regimes, providing a basis for distinguishing legitimate combatants from criminals and ensuring minimum protections.
% TRANSFER_FUNCTION: Transfers legal immunity from prosecution for acts of war from states to organized non-state armed groups fighting colonial, occupation, or racist regimes, in exchange for adherence to the laws of armed conflict.
% ABSENT_VOICES: States that are not party to AP I, or those that maintain a strictly state-centric view of combatant status, are absent from the interpretive community that actively applies this reading. They would argue against extending combatant status to non-state actors under any circumstances.
% DISAPPEARANCE_RATIONALE: If this reading vanished, national liberation movements would lose a crucial legal basis for their struggle, their fighters would be universally treated as criminals, and occupying powers would face fewer legal constraints in suppressing resistance. The legal landscape of asymmetric conflict would fundamentally shift.
% FOUNDING_PROBLEM: The original Geneva Conventions (1949) did not adequately address the status of fighters in national liberation struggles against colonial or racist regimes, leaving them vulnerable to prosecution as criminals rather than prisoners of war.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the UN General Assembly (which adopted AP I) corroborate that the problem of protecting fighters in national liberation struggles remains live, particularly in ongoing occupation contexts. The International Criminal Court's jurisprudence also reflects this ongoing concern.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).

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
 *   The extractiveness (0.65) is moderate-high because it imposes significant legal obligations on states (occupying powers, colonial/racist regimes) to treat insurgents as combatants, which they resist. Suppression (0.75) is high due to the active legal and political efforts by these states to deny this status and prosecute fighters as criminals. Theater ratio (0.20) is low; while there's some performative adherence, the core function of legitimizing liberation struggles and protecting fighters is real and actively pursued by international bodies and advocates. The resistance (0.80) is high, reflecting the ongoing struggle by liberation movements and their allies to enforce this reading against state opposition.
 *
 * PERSPECTIVAL GAP:
 *   National liberation movements and their fighters experience this as a crucial 'rope' or 'scaffold' that provides legal protection and legitimacy. Occupying powers and colonial/racist regimes, however, experience it as a 'snare' that extracts legal concessions and undermines their authority. International criminal courts and human rights advocates act as 'agenda-setters' and 'observers' who actively work to enforce this reading, creating a significant divergence in how the constraint is perceived and experienced across different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   National liberation movements and their detained fighters are clear beneficiaries (d near 0.0) as they gain legal protections and legitimacy. Occupying powers and colonial/racist regimes are targets (d near 1.0) as they bear the costs of these obligations. International criminal courts and human rights advocates have a more symmetric directionality (d near 0.5), as they uphold the law, which benefits some and constrains others, but they do not directly extract or pay in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its founding problem (protecting fighters in national liberation struggles) is still live and actively contested. The classification as a 'tangled_rope' reflects its dual function: it genuinely coordinates a legal framework for asymmetric conflicts while simultaneously extracting concessions from powerful states. The high resistance and active enforcement indicate it is far from a 'piton' or 'snare' where function has atrophied or coordination is merely cover; the coordination function is real, but the extraction is asymmetric and actively resisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organization_command_control_ambiguity,
    'What level of ''organization'' and ''command and control'' is required for a non-state armed group to qualify for combatant status under this reading, and how is it verified in practice?',
    'Jurisprudence from international criminal tribunals and consistent state practice in specific conflict contexts.',
    'A stricter interpretation would narrow the scope of beneficiaries, increasing the effective extraction on liberation movements. A more lenient interpretation would broaden beneficiaries, increasing extraction on occupying powers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organization_command_control_ambiguity, empirical, 'Ambiguity in criteria for non-state armed group combatant status.').

omega_variable(
    ap1_ratification_scope,
    'Given that not all states have ratified AP I, to what extent does this reading apply as customary international law to non-signatory states?',
    'Analysis of state practice and opinio juris (state belief that a practice is legally obligatory) to determine the customary law status of AP I Article 1(4).',
    'If widely recognized as customary international law, the constraint''s scope and effective extraction on non-signatory states would increase significantly. If not, its application remains treaty-bound, limiting its reach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ap1_ratification_scope, conceptual, 'Scope of AP I Article 1(4) as customary international law.').

omega_variable(
    legitimacy_of_struggle_vs_status,
    'Does the ''national liberation'' reading imply a judgment on the political legitimacy of the struggle, or is it purely a legal determination of status based on conduct?',
    'Further clarification from international legal bodies and consistent application in cases where the political legitimacy is disputed.',
    'If it implies political legitimacy, it could be seen as a ''preference'' omega, reflecting a value judgment. If purely legal, it reinforces the ''empirical'' nature of status determination, even if contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_struggle_vs_status, conceptual, 'Whether the reading implies political legitimacy or is purely a legal determination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(comb_tr_t2000, combatant_status_definition__national_liberation_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.55).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(comb_be_t2000, combatant_status_definition__national_liberation_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.7).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(comb_su_t2000, combatant_status_definition__national_liberation_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel. It focuses on extending combatant status to national liberation movements. It is linked to the 'state_centric_reading' and 'functional_protection_reading' as part of a contested legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
