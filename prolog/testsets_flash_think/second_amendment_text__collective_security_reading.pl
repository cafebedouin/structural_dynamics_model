% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'collective security' reading of
 *   the Second Amendment, which interprets the 'well regulated Militia'
 *   clause as conditioning the right to keep and bear arms on its utility for
 *   organized civic defense. This reading grants the state significant power
 *   to regulate firearms to serve collective security, making state
 *   regulatory bodies beneficiaries and individual gun owners a constrained
 *   class. The metrics reflect the substantial extraction and suppression
 *   inherent in this interpretation's application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.78).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.82).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '4a5e4911-2455-42bf-b143-290d65e87411').
narrative_ontology:cs_kernel_codification('4a5e4911-2455-42bf-b143-290d65e87411', fixed_text).
narrative_ontology:cs_authority_grounding('4a5e4911-2455-42bf-b143-290d65e87411', lineage).
narrative_ontology:cs_interpretation_layer_present('4a5e4911-2455-42bf-b143-290d65e87411').
narrative_ontology:cs_reading_relation('4a5e4911-2455-42bf-b143-290d65e87411', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('4a5e4911-2455-42bf-b143-290d65e87411', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('4a5e4911-2455-42bf-b143-290d65e87411', foundational, militia_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('4a5e4911-2455-42bf-b143-290d65e87411', militia_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('4a5e4911-2455-42bf-b143-290d65e87411', foundational, state_power_to_regulate_arms_for_security).
narrative_ontology:cs_axiom_status(state_power_to_regulate_arms_for_security, holdable).
narrative_ontology:cs_axiom_grounding('4a5e4911-2455-42bf-b143-290d65e87411', state_power_to_regulate_arms_for_security, instrumental).
narrative_ontology:cs_reference_frame('4a5e4911-2455-42bf-b143-290d65e87411', well_regulated_militia_framework).
narrative_ontology:cs_drift_state('4a5e4911-2455-42bf-b143-290d65e87411', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4a5e4911-2455-42bf-b143-290d65e87411', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, collective_security_advocates).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment as conditioning the right to bear arms on service in a well-regulated militia, thereby justifying state power to regulate firearms for collective security. Benefits from expanded regulatory authority and public safety outcomes.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to licensing, registration, and restrictions on types of firearms and ammunition, as well as storage requirements. They bear the direct costs of compliance and the curtailment of their perceived individual right.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Advocate for robust state regulation of firearms, believing it essential for public safety and the prevention of gun violence. They benefit from the perceived reduction in societal risk and the legal framework that supports such regulation.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, collective_security_advocates, beneficiary,
    organized, biographical, mobile, national).

% Face restrictions on the types of firearms they can produce and sell, as well as increased compliance costs for tracking and reporting. Their market is constrained by state regulations justified by this reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_manufacturers, payer,
    powerful, biographical, constrained, national).

% The ultimate arbiter of constitutional meaning, whose rulings shape the scope and enforcement of this reading. While it sets the legal framework, it also observes and responds to challenges from other readings.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, judicial_system, observer).

% Strongly oppose the collective security reading, arguing for an individual right to bear arms independent of militia service. They are structurally excluded from the interpretive framework of this reading, which dismisses their core premise.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_right_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure the security of a free state by enabling the state to organize and regulate a militia, and by extension, to manage firearms for collective public safety.
% TRANSFER_FUNCTION: Transfers individual autonomy and property rights regarding firearms to the state, in exchange for enhanced collective security and public order.
% ABSENT_VOICES: Advocates for an individual right to bear arms for self-defense, independent of militia service, are excluded. They would argue that this reading fundamentally misinterprets the Second Amendment and unjustly curtails individual liberty.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal basis for significant state firearms regulation would disappear, leading to a dramatic deregulation of gun ownership. This would fundamentally alter the landscape of public safety, individual rights, and the balance of power between citizens and the state.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state, particularly against foreign invasion and domestic insurrection, by maintaining a well-regulated militia while also acknowledging the right of the people to keep and bear arms.
% FOUNDING_PROBLEM_CORROBORATION: Public safety officials, many legal scholars, and historical analyses from outside the direct beneficiaries corroborate that the problem of collective security and the need for state capacity to manage arms remain live concerns, even if the specific solutions are contested.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) and suppression (0.82) reflect the significant costs imposed on individual gun owners and manufacturers through licensing, restrictions, and enforcement, for a collective benefit (public safety) that is managed by the state. The low theater ratio (0.15) indicates that the regulatory functions are genuinely active and enforced, not merely performative. Resistance is high (0.75) due to ongoing legal and political challenges from other interpretive camps. Accessibility collapse is substantial (0.70) as alternatives to regulated ownership are legally curtailed.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading view it as a necessary and legitimate exercise of state power for public good, emphasizing coordination for collective security. Opponents, particularly individual gun owners, experience it as an extractive and suppressive curtailment of fundamental rights. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus and collective security advocates are clear beneficiaries, gaining expanded authority and perceived safety. Individual gun owners and firearms manufacturers are targets, bearing the costs of regulation and restrictions. The judicial system acts as an agenda-setter, shaping the interpretation and its application. Individual right advocates are excluded, as their core premise is rejected by this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_and_structural_impact,
    'To what extent does this ''collective_security_reading'' of the Second Amendment structurally differ from other readings, particularly the ''individual_right_reading''?',
    'Comparative legal analysis of judicial precedents and legislative outcomes under different dominant readings, focusing on changes in beneficiary/victim sets and enforcement mechanisms.',
    'If the structural deltas are less pronounced than claimed, the readings may be ''coexisting_with'' rather than ''foreclosing'' each other, suggesting a more fluid interpretive landscape. If the deltas are substantial, it confirms distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_and_structural_impact, conceptual, 'Clarifying the distinct structural implications of the collective security reading versus other Second Amendment interpretations.').

omega_variable(
    efficacy_of_regulation_for_security,
    'How effectively do state firearms regulations, justified by this reading, actually contribute to collective security and reduce gun violence?',
    'Empirical studies comparing gun violence rates and public safety outcomes in jurisdictions with varying levels of regulation, controlling for confounding factors.',
    'If regulations are found to be ineffective, the ''instrumental'' grounding of the ''state_power_to_regulate_arms_for_security'' axiom would be challenged, potentially weakening the reading''s legitimacy and shifting its classification towards a Snare if extraction persists without coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_regulation_for_security, empirical, 'Empirical validation of the instrumental claim that state regulation enhances collective security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__collective_security_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(seco_tr_t1989, second_amendment_text__collective_security_reading, theater_ratio, 1989, 0.12).
narrative_ontology:measurement(seco_tr_t1998, second_amendment_text__collective_security_reading, theater_ratio, 1998, 0.13).
narrative_ontology:measurement(seco_tr_t2007, second_amendment_text__collective_security_reading, theater_ratio, 2007, 0.14).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_text__collective_security_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_text__collective_security_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__collective_security_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(seco_be_t1989, second_amendment_text__collective_security_reading, base_extractiveness, 1989, 0.72).
narrative_ontology:measurement(seco_be_t1998, second_amendment_text__collective_security_reading, base_extractiveness, 1998, 0.74).
narrative_ontology:measurement(seco_be_t2007, second_amendment_text__collective_security_reading, base_extractiveness, 2007, 0.76).
narrative_ontology:measurement(seco_be_t2016, second_amendment_text__collective_security_reading, base_extractiveness, 2016, 0.77).
narrative_ontology:measurement(seco_be_t2025, second_amendment_text__collective_security_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1980, second_amendment_text__collective_security_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(seco_su_t1989, second_amendment_text__collective_security_reading, suppression_requirement, 1989, 0.77).
narrative_ontology:measurement(seco_su_t1998, second_amendment_text__collective_security_reading, suppression_requirement, 1998, 0.79).
narrative_ontology:measurement(seco_su_t2007, second_amendment_text__collective_security_reading, suppression_requirement, 2007, 0.8).
narrative_ontology:measurement(seco_su_t2016, second_amendment_text__collective_security_reading, suppression_requirement, 2016, 0.81).
narrative_ontology:measurement(seco_su_t2025, second_amendment_text__collective_security_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_text' kernel, each with its own structural properties and classification. This reading focuses on the state's power to regulate for collective security.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
