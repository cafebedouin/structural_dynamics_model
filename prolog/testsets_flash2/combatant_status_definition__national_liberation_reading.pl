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
 *   status, primarily derived from Article 1(4) of Additional Protocol I (AP
 *   I) to the Geneva Conventions. It extends combatant status, and thus
 *   prisoner of war (POW) protections, to members of non-state armed groups
 *   fighting against colonial domination, alien occupation, or racist
 *   regimes, provided they meet criteria of organization and command control.
 *   This reading is contested by states adhering to a more traditional
 *   'state-centric' view of combatant status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.75).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'eb1d7347-bc1a-4e17-bad4-a4b3812bbd55').
narrative_ontology:cs_kernel_codification('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', fixed_text).
narrative_ontology:cs_authority_grounding('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', lineage).
narrative_ontology:cs_interpretation_layer_present('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55').
narrative_ontology:cs_reading_relation('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', foundational, self_determination_as_international_armed_conflict).
narrative_ontology:cs_axiom_status(self_determination_as_international_armed_conflict, holdable).
narrative_ontology:cs_axiom_grounding('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', self_determination_as_international_armed_conflict, conventional).
narrative_ontology:cs_axiom('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', foundational, non_state_actors_can_be_legitimate_combatants).
narrative_ontology:cs_axiom_status(non_state_actors_can_be_legitimate_combatants, holdable).
narrative_ontology:cs_axiom_grounding('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', non_state_actors_can_be_legitimate_combatants, deontological).
narrative_ontology:cs_reference_frame('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', post_api_humanitarian_expansion).
narrative_ontology:cs_drift_state('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', contemporary_asymmetric_warfare_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('eb1d7347-bc1a-4e17-bad4-a4b3812bbd55', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_racist_regimes).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, human_rights_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain conditional combatant status, offering their members protection as prisoners of war if captured, provided they meet organizational and command-control criteria. This legitimizes their struggle under international law, but requires adherence to LOAC.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, constrained, regional).

% These states are obligated to grant POW status to captured members of qualifying national liberation movements, which complicates their counter-insurgency efforts and limits their ability to prosecute such individuals as criminals. This imposes significant legal and political costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, biographical, constrained, national).

% Similar to occupying powers, these regimes face legal and political pressure to recognize combatant status for groups fighting against them, undermining their claims of internal policing actions and elevating the conflict to an international armed conflict. This challenges their legitimacy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_racist_regimes, payer,
    institutional, biographical, constrained, national).

% Interprets and enforces international humanitarian law, including AP I. Its rulings and investigations can affirm or challenge the application of combatant status to national liberation movements, influencing state practice and legal precedent.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_criminal_court, agenda_setter,
    institutional, civilizational, analytical, global).

% Monitor compliance with international humanitarian law and advocate for the broadest possible application of protections, including combatant status for non-state actors in specific contexts. They exert moral and political pressure.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Adhere to a strict interpretation of combatant status tied exclusively to state armed forces, viewing AP I Art 1(4) as an overreach that blurs the lines of international armed conflict. Their arguments are often marginalized in progressive IHL discourse but remain influential in some state legal departments.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_centric_legal_scholars, excluded,
    analytical, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing legitimate armed resistance from mere banditry or terrorism in specific contexts (colonial, occupation, racist regimes), aiming to bring such conflicts under the protective umbrella of international humanitarian law.
% TRANSFER_FUNCTION: Transfers legal protections (e.g., POW status) from state armed forces to qualifying non-state national liberation movements, while imposing obligations of LOAC adherence on these movements and legal constraints on the regimes they fight.
% ABSENT_VOICES: The 'state-centric' legal scholars and states that adhere strictly to the traditional definition of combatant status are often marginalized in discussions promoting this reading. They would argue that expanding combatant status undermines state sovereignty and the distinction between international and non-international armed conflicts.
% DISAPPEARANCE_RATIONALE: If this reading of combatant status vanished, national liberation movements would lose a key legal tool for legitimizing their struggle and protecting their members, likely leading to increased prosecution as criminals and a further erosion of IHL protections in these conflicts. The legal landscape of asymmetric warfare would fundamentally shift.
% FOUNDING_PROBLEM: The traditional state-centric definition of combatant status failed to adequately address conflicts arising from decolonization and struggles against racist regimes, leaving combatants in such conflicts without IHL protections.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, human rights organizations, and many states (particularly those that emerged from colonial rule) corroborate that the problem of protecting combatants in these specific contexts remains live, citing ongoing conflicts and the need for IHL to adapt to contemporary realities. This is attested by UN resolutions and ICJ advisory opinions.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high for occupying powers and regimes, as it imposes significant legal obligations and limits their ability to treat captured combatants as criminals. Suppression (0.75) is also high, as these powers actively resist this interpretation and suppress its application, often through legal challenges and political pressure. The theater ratio (0.20) is relatively low, as the legal framework, while contested, is genuinely applied in some contexts, and the debate is substantive rather than purely performative. The increasing extractiveness and suppression over time reflect the ongoing struggle for this interpretation to gain wider acceptance against state resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national liberation movements, this is a vital 'rope' or 'scaffold' for justice and protection. From the perspective of occupying powers, it is a 'snare' that undermines their security and sovereignty. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   National liberation movements are beneficiaries (d=0.0-0.2) as they gain crucial legal protections. Occupying powers and colonial/racist regimes are targets (d=0.8-1.0) as they bear the costs of these expanded protections. The ICC and human rights advocates act as agenda-setters and observers, pushing for this interpretation. State-centric legal scholars are excluded, as their arguments are actively resisted by the proponents of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_criteria_ambiguity,
    'How strictly are the ''organized'' and ''command-controlled'' criteria for national liberation movements applied in practice, and by whom?',
    'Analysis of case law from international tribunals and national courts, and state practice in specific conflicts, to identify consistent application standards.',
    'If criteria are applied loosely, it expands the scope of beneficiaries but may dilute the legitimacy of the status. If applied too strictly, it may exclude deserving groups, undermining the reading''s intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_criteria_ambiguity, empirical, 'Ambiguity in applying organizational criteria for combatant status.').

omega_variable(
    regime_definition_contest,
    'What constitutes a ''colonial domination, alien occupation, or racist regime'' in contemporary international law, and who adjudicates this definition?',
    'Consensus among UN member states, ICJ advisory opinions, and consistent practice of international organizations.',
    'A narrow definition limits the applicability of this reading, reducing its impact. A broad definition expands its scope, increasing the burden on states and potentially leading to more disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_definition_contest, conceptual, 'Contestation over the definition of regimes qualifying for AP I Art 1(4).').

omega_variable(
    state_resistance_efficacy,
    'How effective is the resistance of state-centric powers in preventing the broader application and acceptance of this reading?',
    'Tracking the number of states ratifying AP I, reservations made, and instances where states explicitly deny combatant status to qualifying groups despite international pressure.',
    'High efficacy of state resistance would indicate a stronger ''snare'' for national liberation movements, as the promised protections are frequently denied. Low efficacy would suggest a more robust ''rope'' for these movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_resistance_efficacy, empirical, 'The impact of state resistance on the practical application of this IHL reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(comb_tr_t2000, combatant_status_definition__national_liberation_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(comb_tr_t2020, combatant_status_definition__national_liberation_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(comb_be_t2000, combatant_status_definition__national_liberation_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(comb_be_t2020, combatant_status_definition__national_liberation_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(comb_su_t2000, combatant_status_definition__national_liberation_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(comb_su_t2020, combatant_status_definition__national_liberation_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, definition_of_international_armed_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel. It focuses on the extension of combatant status to national liberation movements under AP I Article 1(4), distinct from state-centric or purely functional protection readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
