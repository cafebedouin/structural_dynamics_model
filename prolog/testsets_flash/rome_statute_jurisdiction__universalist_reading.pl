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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universalist Jurisdiction Mandate
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the Rome
 *   Statute's jurisdiction, asserting that the International Criminal Court
 *   (ICC) has a mandate for international criminal justice that can transcend
 *   state consent, particularly through territorial jurisdiction over state
 *   parties or UN Security Council referrals. This reading emphasizes
 *   accountability for core crimes regardless of the nationality of the
 *   accused or the consent of their state. It is a contested interpretation,
 *   with strong resistance from states asserting absolute sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.75).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universalist Jurisdiction Mandate").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '9d94f3ab-3afa-49fc-9e89-f8008f45558d').
narrative_ontology:cs_kernel_codification('9d94f3ab-3afa-49fc-9e89-f8008f45558d', fixed_text).
narrative_ontology:cs_authority_grounding('9d94f3ab-3afa-49fc-9e89-f8008f45558d', lineage).
narrative_ontology:cs_interpretation_layer_present('9d94f3ab-3afa-49fc-9e89-f8008f45558d').
narrative_ontology:cs_reading_relation('9d94f3ab-3afa-49fc-9e89-f8008f45558d', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('9d94f3ab-3afa-49fc-9e89-f8008f45558d', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('9d94f3ab-3afa-49fc-9e89-f8008f45558d', foundational, universal_jurisdiction_for_core_crimes).
narrative_ontology:cs_axiom_status(universal_jurisdiction_for_core_crimes, holdable).
narrative_ontology:cs_axiom_grounding('9d94f3ab-3afa-49fc-9e89-f8008f45558d', universal_jurisdiction_for_core_crimes, deontological).
narrative_ontology:cs_axiom('9d94f3ab-3afa-49fc-9e89-f8008f45558d', foundational, sovereignty_subordinate_to_international_justice).
narrative_ontology:cs_axiom_status(sovereignty_subordinate_to_international_justice, holdable).
narrative_ontology:cs_axiom_grounding('9d94f3ab-3afa-49fc-9e89-f8008f45558d', sovereignty_subordinate_to_international_justice, deontological).
narrative_ontology:cs_reference_frame('9d94f3ab-3afa-49fc-9e89-f8008f45558d', post_nuremberg_universal_accountability).
narrative_ontology:cs_drift_state('9d94f3ab-3afa-49fc-9e89-f8008f45558d', contemporary_state_resistance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d94f3ab-3afa-49fc-9e89-f8008f45558d', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_nationals_of_non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, states_asserting_absolute_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution tasked with interpreting and enforcing the Rome Statute's jurisdiction. It asserts its mandate to prosecute individuals for core international crimes regardless of their state's consent, particularly when referred by the UNSC or when crimes occur on the territory of a state party.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the assertion of universal jurisdiction, as it offers a potential avenue for justice when national courts are unwilling or unable to prosecute, and when their own state is not a party to the Rome Statute. Their access to justice is often contingent on the ICC's ability to assert this jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, global).

% Actively promote and defend the universalist interpretation of the Rome Statute, viewing it as a critical tool for accountability and the advancement of international human rights law. They benefit from the expanded scope of justice it enables.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% States that have not ratified the Rome Statute and object to the ICC asserting jurisdiction over their nationals or for crimes committed on their territory without their explicit consent. They bear the cost of potential prosecution of their citizens and the perceived erosion of their sovereign authority.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    powerful, generational, constrained, global).

% Individuals from non-party states who are accused of core international crimes and face potential prosecution by the ICC, despite their state not recognizing the court's jurisdiction. They are directly targeted by the universalist interpretation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_nationals_of_non_party_states, payer,
    powerless, immediate, trapped, global).

% These states fundamentally reject any international legal authority that purports to override national sovereignty, viewing the universalist reading as an illegitimate infringement on their foundational principles. They actively resist the ICC's claims and seek to undermine its jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, states_asserting_absolute_sovereignty, payer,
    institutional, civilizational, identity_locked, global).

% Analyze the legal basis, implications, and effectiveness of the universalist interpretation, contributing to the ongoing debate about the balance between sovereignty and international criminal justice. They neither directly benefit nor pay, but their analysis shapes the discourse.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate international efforts to ensure accountability for the most heinous crimes (genocide, crimes against humanity, war crimes, aggression) by establishing a court of last resort with a broad mandate, preventing impunity where national systems fail.
% TRANSFER_FUNCTION: Transfers the authority to prosecute individuals for core international crimes from national jurisdictions (especially those unwilling or unable to act, or non-parties) to the International Criminal Court, along with the associated costs of investigation and prosecution borne by the accused and their states.
% ABSENT_VOICES: States that have actively withdrawn from the Rome Statute or consistently reject its jurisdiction (e.g., the US, Russia, China, Israel) are absent from the direct enforcement mechanism, though their actions are often the subject of the universalist claim. They would argue for strict adherence to state consent and national sovereignty.
% DISAPPEARANCE_RATIONALE: If the universalist mandate vanished, the ICC's ability to prosecute crimes in non-party states or without explicit consent would collapse, leading to a significant increase in impunity for core international crimes, particularly in conflict zones where national justice systems are weak or complicit. The international legal landscape for accountability would fundamentally shift.
% FOUNDING_PROBLEM: The problem of impunity for mass atrocities, where national states were either unwilling or unable to prosecute perpetrators, leading to a cycle of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: The problem of impunity remains live, as evidenced by ongoing conflicts and atrocities globally, and the continued failure of many national justice systems. Human rights organizations, UN bodies, and numerous international legal experts (outside the ICC itself) consistently corroborate the persistence of this problem and the need for international accountability mechanisms.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because this reading imposes a legal obligation and potential prosecution on non-consenting states and their nationals. Suppression (0.75) is high due to the active diplomatic and legal efforts required by the ICC and its proponents to assert and enforce this jurisdiction against sovereign resistance. The theater ratio (0.20) is low, indicating that the universalist claim is genuinely pursued, though often with significant political and practical challenges. The metrics reflect the ongoing struggle to enforce this expansive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and human rights advocates, this is a necessary and just coordination mechanism to end impunity. From the perspective of non-party states and those asserting absolute sovereignty, it is an overreach and an illegitimate extraction of sovereign authority. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC, victims of core crimes, and human rights advocates are beneficiaries (d near 0.0-0.2) as they gain an avenue for justice and accountability. Non-party states, accused nationals of non-party states, and states asserting absolute sovereignty are targets (d near 0.8-1.0) as they face potential prosecution and a perceived erosion of sovereignty. The universalist reading directly extracts authority and compliance from these resistant actors.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_unsc_referral_authority,
    'Does the UN Security Council''s Chapter VII authority legitimately override the consent requirements of the Rome Statute for non-party states, or is this an overreach of UNSC power?',
    'International legal precedent from future ICJ advisory opinions or state practice that either consistently accepts or rejects UNSC referrals as a basis for ICC jurisdiction over non-parties.',
    'If UNSC referrals are deemed to legitimately override consent, the universalist reading''s suppression and extractiveness are structurally higher and more robust. If deemed an overreach, the reading''s effective jurisdiction is significantly curtailed, reducing its extractiveness on non-party states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_unsc_referral_authority, conceptual, 'Ambiguity regarding the UN Security Council''s power to mandate ICC jurisdiction over non-party states.').

omega_variable(
    effectiveness_of_enforcement_without_consent,
    'How effective is the ICC''s enforcement of jurisdiction over nationals of non-party states in practice, given the lack of state cooperation and potential for political obstruction?',
    'Empirical analysis of arrest rates, successful prosecutions, and state cooperation levels for cases involving non-party nationals versus state party nationals over a 10-year period.',
    'If enforcement is consistently low or requires disproportionate effort, the universalist reading''s actual extractiveness is lower than claimed, and its theater ratio is higher, indicating a performative rather than effective mandate. If enforcement is robust, the reading''s claims are empirically vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_of_enforcement_without_consent, empirical, 'The practical enforceability of universalist jurisdiction against non-consenting states.').

omega_variable(
    natural_law_vs_positive_law_grounding,
    'Is the universalist mandate grounded in an inherent natural law principle of justice for core crimes, or is it solely a construct of positive international law, dependent on state practice and treaty interpretation?',
    'Philosophical and jurisprudential debate, potentially clarified by a shift in global consensus on the source of international criminal law''s authority.',
    'If grounded in natural law, the constraint''s legitimacy is less dependent on state consent, potentially increasing its perceived ''mountain-like'' quality for proponents. If purely positive, its persistence is more vulnerable to state resistance and withdrawal, making it more clearly a ''tangled rope'' sustained by active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_positive_law_grounding, conceptual, 'The fundamental philosophical grounding of universal jurisdiction.').


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
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'universalist reading' of the Rome Statute's jurisdiction, which asserts a broad mandate for international criminal justice. It is part of a family of constraints derived from the same kernel, with other readings emphasizing state consent or complementarity. The ε values differ significantly across these readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
