% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Jurisdiction (Universalist Reading)
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the Rome
 *   Statute's jurisdiction, asserting that the International Criminal Court's
 *   (ICC) mandate for international criminal justice transcends strict state
 *   consent, particularly through territorial jurisdiction over non-party
 *   states or UNSC referrals. This reading emphasizes the supremacy of
 *   international criminal law over national sovereignty for core crimes. The
 *   metrics reflect the ongoing contestation and the active enforcement
 *   required to sustain this interpretation against strong state resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.7).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Jurisdiction (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '0de479f7-8bcd-4d30-b379-957b94e0f759').
narrative_ontology:cs_kernel_codification('0de479f7-8bcd-4d30-b379-957b94e0f759', fixed_text).
narrative_ontology:cs_authority_grounding('0de479f7-8bcd-4d30-b379-957b94e0f759', lineage).
narrative_ontology:cs_interpretation_layer_present('0de479f7-8bcd-4d30-b379-957b94e0f759').
narrative_ontology:cs_reading_relation('0de479f7-8bcd-4d30-b379-957b94e0f759', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('0de479f7-8bcd-4d30-b379-957b94e0f759', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('0de479f7-8bcd-4d30-b379-957b94e0f759', foundational, impunity_is_unacceptable).
narrative_ontology:cs_axiom_status(impunity_is_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('0de479f7-8bcd-4d30-b379-957b94e0f759', impunity_is_unacceptable, deontological).
narrative_ontology:cs_axiom('0de479f7-8bcd-4d30-b379-957b94e0f759', foundational, jus_cogens_trumps_sovereignty).
narrative_ontology:cs_axiom_status(jus_cogens_trumps_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0de479f7-8bcd-4d30-b379-957b94e0f759', jus_cogens_trumps_sovereignty, deontological).
narrative_ontology:cs_reference_frame('0de479f7-8bcd-4d30-b379-957b94e0f759', post_nuremberg_universal_justice).
narrative_ontology:cs_drift_state('0de479f7-8bcd-4d30-b379-957b94e0f759', contemporary_geopolitical_fragmentation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0de479f7-8bcd-4d30-b379-957b94e0f759', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_individuals_from_non_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, jus_cogens_norms).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, universal_jurisdiction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution tasked with investigating and prosecuting individuals for core international crimes. This reading asserts its jurisdiction can extend to non-party states under specific conditions (territoriality, UNSC referral), transcending direct state consent. It benefits from an expanded mandate but faces political and logistical constraints.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who have suffered genocide, crimes against humanity, war crimes, or the crime of aggression. This reading ensures their access to justice regardless of their state's ratification status or willingness to prosecute, offering a pathway to accountability where national systems fail.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, global).

% States that have not ratified the Rome Statute and thus do not consent to ICC jurisdiction. This reading asserts that their nationals or territory can fall under ICC jurisdiction, which they perceive as an infringement on their sovereignty and a cost to their autonomy in international relations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    powerful, generational, constrained, global).

% Individuals from non-party states who are accused of core international crimes and become targets of ICC investigation or prosecution. They bear the direct cost of this expanded jurisdiction, facing potential arrest and trial without their state's consent to the ICC.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_individuals_from_non_party_states, payer,
    powerless, immediate, trapped, global).

% Can refer situations to the ICC, thereby triggering jurisdiction over non-party states. This body holds significant power in shaping the practical application of the universalist reading, acting as a gatekeeper and enforcer of international justice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, united_nations_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Academics and legal experts who advocate for and interpret the Rome Statute as embodying a universal mandate for international criminal justice, emphasizing the supremacy of jus cogens norms and the imperative to end impunity for core crimes.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_law_scholars_universalist, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mechanism for international accountability for the most heinous crimes, ensuring that perpetrators do not escape justice due to national inaction or state sovereignty claims, thereby coordinating global efforts to uphold fundamental human rights.
% TRANSFER_FUNCTION: Transfers the authority to prosecute core international crimes from national jurisdictions (especially those unwilling or unable) to the ICC, potentially from non-party states to the international community, and transfers the burden of accountability to individuals regardless of their state's consent.
% ABSENT_VOICES: States that actively resist the ICC's jurisdiction, particularly those that are non-parties to the Rome Statute, are often absent from the direct interpretive debates within the ICC's framework, though their objections are voiced in other international fora. Their 'absence' from the ICC's internal legal discourse allows the universalist reading to gain traction.
% DISAPPEARANCE_RATIONALE: If the universalist reading of ICC jurisdiction vanished, it would significantly weaken the international criminal justice system. Perpetrators from non-party states would face fewer avenues for accountability, potentially leading to increased impunity for core crimes and a retreat to strict state sovereignty, fundamentally altering the landscape of international law and human rights enforcement.
% FOUNDING_PROBLEM: The problem of impunity for genocide, war crimes, and crimes against humanity, where national courts are unwilling or unable to prosecute, and state sovereignty often shielded perpetrators from international accountability.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and a significant portion of international legal scholarship corroborate that impunity remains a live problem, particularly in conflict zones and authoritarian states. They attest that the universalist reading is a necessary tool to address this ongoing challenge, even if imperfectly applied.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it imposes accountability on individuals and states without their direct consent, representing a significant cost to traditional notions of sovereignty. Suppression (0.70) is high due to the active political and legal efforts required by the ICC and its proponents to assert and defend this jurisdiction against non-party states, which often actively resist. Theater ratio (0.20) is relatively low, as the ICC's actions, while contested, are generally aimed at genuine prosecution, not mere performance. Resistance (0.75) is high, reflecting the strong opposition from non-party states and their allies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and victims, this is a necessary mechanism for justice, a 'rope' coordinating global efforts against impunity. From the perspective of non-party states, it is an overreach, a 'snare' extracting sovereignty and imposing an unwanted legal framework. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims of core crimes are beneficiaries, as this reading expands the court's reach and provides a path to justice for victims. Non-party states and their accused nationals are victims, as they bear the costs of jurisdiction asserted without their consent. The UNSC acts as an agenda-setter, capable of triggering this jurisdiction. International law scholars supporting this view are observers, providing analytical support.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_non_party_jurisdiction,
    'Is the ICC''s jurisdiction over non-party states, particularly via territoriality or UNSC referral, genuinely legitimate under international law, or is it an overreach?',
    'Further development of customary international law, advisory opinions from the ICJ, or a definitive ruling by a widely accepted international tribunal on the interpretation of the Vienna Convention on the Law of Treaties regarding third-party effects.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be re-evaluated as pure coercion, potentially reclassifying it as a Snare. If affirmed, it would strengthen the Rope aspect, reducing perceived extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_non_party_jurisdiction, conceptual, 'Ambiguity regarding the legal basis for ICC jurisdiction over non-party states.').

omega_variable(
    effectiveness_vs_resistance,
    'Does the assertion of universalist jurisdiction, despite strong resistance from non-party states, genuinely enhance international criminal justice, or does it undermine cooperation and the ICC''s long-term legitimacy?',
    'Empirical studies on the long-term impact of ICC interventions in non-party states, including cooperation levels, arrest rates, and the deterrent effect on core crimes, balanced against diplomatic fallout and state withdrawals.',
    'If found to undermine cooperation, the constraint''s theater_ratio might increase (as actions become more symbolic than effective), and its overall utility as a Rope would be questioned, potentially shifting towards Piton or Snare. If effective, its Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_resistance, empirical, 'The practical efficacy of universalist jurisdiction in the face of state resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_humanitarian_law_compliance).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, un_security_council_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Rome Statute's jurisdiction, alongside the 'sovereigntist_reading' and 'hybrid_complementarity_reading'. Each reading represents a distinct structural claim about the ICC's authority and its relationship to state sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
