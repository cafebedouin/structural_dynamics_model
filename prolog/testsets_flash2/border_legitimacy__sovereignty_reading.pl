% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty reading' of border
 *   legitimacy, where the state's authority to control its borders and
 *   exclude non-citizens is derived from its territorial sovereignty. This
 *   reading views border enforcement as a legitimate exercise of state power,
 *   essential for national self-determination and the protection of its
 *   citizenry. The metrics reflect a high degree of extraction from those
 *   excluded and significant suppression required to maintain the border
 *   regime.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.9).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, snare).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '29214462-8ef1-46ac-87e0-570149412576').
narrative_ontology:cs_kernel_codification('29214462-8ef1-46ac-87e0-570149412576', formalized).
narrative_ontology:cs_authority_grounding('29214462-8ef1-46ac-87e0-570149412576', lineage).
narrative_ontology:cs_interpretation_layer_present('29214462-8ef1-46ac-87e0-570149412576').
narrative_ontology:cs_reading_relation('29214462-8ef1-46ac-87e0-570149412576', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('29214462-8ef1-46ac-87e0-570149412576', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('29214462-8ef1-46ac-87e0-570149412576', foundational, territorial_sovereignty_absolute).
narrative_ontology:cs_axiom_status(territorial_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('29214462-8ef1-46ac-87e0-570149412576', territorial_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('29214462-8ef1-46ac-87e0-570149412576', foundational, state_right_to_exclude_unqualified).
narrative_ontology:cs_axiom_status(state_right_to_exclude_unqualified, holdable).
narrative_ontology:cs_axiom_grounding('29214462-8ef1-46ac-87e0-570149412576', state_right_to_exclude_unqualified, conventional).
narrative_ontology:cs_reference_frame('29214462-8ef1-46ac-87e0-570149412576', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('29214462-8ef1-46ac-87e0-570149412576', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('29214462-8ef1-46ac-87e0-570149412576', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, sovereign_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizenry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary actor asserting and enforcing border controls, deriving its legitimacy from the principle of territorial sovereignty. It defines who may enter and exit, and deploys resources to maintain these boundaries. Its 'exit' is constrained by international norms and the practicalities of global interdependence.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, sovereign_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the state's ability to control its borders, which is perceived to protect national identity, economic resources, and security. While individual citizens have limited direct control, their collective political will often underpins the state's actions. Their 'exit' from the national framework is constrained by identity and practicalities.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Individuals who are denied entry or face significant barriers to crossing borders, experiencing direct extraction in terms of lost opportunities, separation from family, and often dangerous journeys. Their options are to attempt illegal crossings, remain in precarious situations, or return to their origin, making them effectively trapped.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals seeking protection who, under this reading, may still be subject to state discretion regarding entry, even if international law suggests a right to seek asylum. They face detention, deportation, and denial of basic rights, making their situation highly extractive and their exit options severely limited or non-existent.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Organizations and individuals who argue for broader rights for migrants and asylum seekers, often challenging the absolute nature of state sovereignty. While they influence discourse, their direct power to alter state policy is limited, and their arguments are often dismissed by states adhering strictly to the sovereignty reading.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_advocates, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear framework for state control over its territory and population, enabling the state to manage resources, maintain public order, and define national identity without external interference.
% TRANSFER_FUNCTION: Transfers the right to determine who resides within a territory from individuals (migrants) to the sovereign state, allowing the state to allocate resources and opportunities primarily to its citizenry.
% ABSENT_VOICES: Migrants and asylum seekers, particularly those excluded, are largely absent from the policy-making processes that determine their fate. International human rights advocates speak on their behalf but lack direct political power within the sovereign state's decision-making.
% DISAPPEARANCE_RATIONALE: If the principle of territorial sovereignty and the state's right to exclude vanished overnight, borders would become permeable. This would lead to massive population shifts, a redefinition of citizenship, and a fundamental reorganization of global political and economic structures, as states would lose their primary mechanism for self-definition and control.
% FOUNDING_PROBLEM: The need to define and defend a political community, manage resources within a defined territory, and protect a population from external threats and uncontrolled influxes.
% FOUNDING_PROBLEM_CORROBORATION: States and their citizenry consistently attest that the founding problem of maintaining a distinct political community and managing its resources remains live, citing ongoing challenges related to security, economic stability, and cultural cohesion. This is corroborated by historical patterns of state formation and international relations, though the specific interpretation of 'threat' is often contested by international bodies.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.90) scores reflect the direct costs imposed on excluded migrants and the coercive force required to maintain borders against human movement. The low theater ratio (0.10) indicates that the enforcement is largely functional, directly serving the state's goal of exclusion, rather than being merely performative. Accessibility collapse is high (0.75) because for many, legal alternatives to entry are severely limited or non-existent. Resistance (0.70) is also high, reflecting the persistent efforts of migrants and advocates to challenge or circumvent these controls.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the sovereign state and its citizenry, this constraint is a legitimate exercise of self-determination and resource management. From the perspective of excluded migrants, it is a highly extractive and suppressive barrier to fundamental human needs and rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign state and its citizenry are the primary beneficiaries, as they gain control over territory and resources. Excluded migrants and asylum seekers are the clear victims, bearing the full cost of exclusion. International human rights advocates are excluded from the direct decision-making process but exert pressure from an analytical/moral standpoint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Does the principle of state sovereignty categorically override individual human rights, or are there limits to sovereign exclusion based on universal human rights?',
    'International legal precedent from cases where human rights claims directly challenge sovereign border controls, or a shift in global normative consensus on the hierarchy of these principles.',
    'If human rights are deemed to limit sovereignty, the extractiveness and suppression of this constraint would be re-evaluated downward for certain categories of migrants (e.g., asylum seekers), potentially reclassifying it from a Snare to a Tangled Rope or even a Scaffold for those groups. If sovereignty remains paramount, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'The fundamental conceptual conflict between state sovereignty and universal human rights in border control.').

omega_variable(
    economic_impact_of_exclusion,
    'What is the net economic impact of excluding migrants on the sovereign state, considering both direct costs of enforcement and foregone economic contributions?',
    'Comprehensive, independent economic modeling that accounts for both the costs of border enforcement and the potential economic benefits (labor, innovation, consumption) of various migrant populations.',
    'If the economic costs of exclusion significantly outweigh the benefits, the ''beneficiary'' status of the citizenry might be re-evaluated, potentially shifting the constraint''s overall extractiveness profile and challenging the rationale for its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_exclusion, empirical, 'The empirical economic trade-offs of border exclusion for the receiving state.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers, legal penalties) or internalized (fear, hopelessness, identity fusion with ''illegal'' status)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., fear of deportation, difficulty integrating) after the immediate physical/legal barriers are removed (e.g., through amnesty programs), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making ''freedom'' less accessible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1648, border_legitimacy__sovereignty_reading, theater_ratio, 1648, 0.05).
narrative_ontology:measurement(bord_tr_t1800, border_legitimacy__sovereignty_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(bord_tr_t1900, border_legitimacy__sovereignty_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(bord_tr_t1950, border_legitimacy__sovereignty_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1648, border_legitimacy__sovereignty_reading, base_extractiveness, 1648, 0.7).
narrative_ontology:measurement(bord_be_t1800, border_legitimacy__sovereignty_reading, base_extractiveness, 1800, 0.75).
narrative_ontology:measurement(bord_be_t1900, border_legitimacy__sovereignty_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(bord_be_t1950, border_legitimacy__sovereignty_reading, base_extractiveness, 1950, 0.82).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__sovereignty_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1648, border_legitimacy__sovereignty_reading, suppression_requirement, 1648, 0.65).
narrative_ontology:measurement(bord_su_t1800, border_legitimacy__sovereignty_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(bord_su_t1900, border_legitimacy__sovereignty_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(bord_su_t1950, border_legitimacy__sovereignty_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__sovereignty_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_legitimacy' kernel. Its structural claims about state sovereignty and the right to exclude directly influence (and are influenced by) alternative readings focused on freedom of movement and humanitarian obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
