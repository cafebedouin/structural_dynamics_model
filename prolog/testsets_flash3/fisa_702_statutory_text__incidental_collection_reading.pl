% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__incidental_collection_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702: Incidental Collection and Warrantless Query of U.S. Person Data
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents the 'incidental collection' reading of FISA
 *   Section 702, which permits national security agencies to retain and query
 *   U.S. person communications collected incidentally to foreign intelligence
 *   surveillance, without a warrant. This reading is contested by other
 *   interpretations of the statute and the Fourth Amendment. The claimed type
 *   is 'tangled_rope' because it provides a coordination function for
 *   intelligence gathering while simultaneously extracting privacy from U.S.
 *   persons through warrantless access.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.7).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702: Incidental Collection and Warrantless Query of U.S. Person Data").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'e824f676-a65f-4932-b15d-6d02854c0db8').
narrative_ontology:cs_kernel_codification('e824f676-a65f-4932-b15d-6d02854c0db8', fixed_text).
narrative_ontology:cs_authority_grounding('e824f676-a65f-4932-b15d-6d02854c0db8', lineage).
narrative_ontology:cs_interpretation_layer_present('e824f676-a65f-4932-b15d-6d02854c0db8').
narrative_ontology:cs_reading_relation('e824f676-a65f-4932-b15d-6d02854c0db8', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('e824f676-a65f-4932-b15d-6d02854c0db8', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('e824f676-a65f-4932-b15d-6d02854c0db8', foundational, foreign_intelligence_purpose_justifies_warrantless_query).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_warrantless_query, holdable).
narrative_ontology:cs_axiom_grounding('e824f676-a65f-4932-b15d-6d02854c0db8', foreign_intelligence_purpose_justifies_warrantless_query, conventional).
narrative_ontology:cs_axiom('e824f676-a65f-4932-b15d-6d02854c0db8', foundational, incidental_collection_is_not_a_search).
narrative_ontology:cs_axiom_status(incidental_collection_is_not_a_search, holdable).
narrative_ontology:cs_axiom_grounding('e824f676-a65f-4932-b15d-6d02854c0db8', incidental_collection_is_not_a_search, conventional).
narrative_ontology:cs_reference_frame('e824f676-a65f-4932-b15d-6d02854c0db8', post_911_intelligence_imperative).
narrative_ontology:cs_drift_state('e824f676-a65f-4932-b15d-6d02854c0db8', contemporary_oversight_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e824f676-a65f-4932-b15d-6d02854c0db8', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_analysts).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the surveillance programs under FISA Section 702, collecting foreign intelligence. They interpret the statute to permit retention and warrantless querying of incidentally collected U.S. person communications for foreign intelligence purposes, arguing it is essential for national security. They benefit from broad access to intelligence data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Utilize the collected data, including incidentally collected U.S. person communications, to produce foreign intelligence reports. Their work is facilitated by the broad access permitted by this reading of the statute, allowing them to connect dots across vast datasets without needing individual warrants for U.S. person data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_analysts, beneficiary,
    organized, biographical, constrained, global).

% Have their communications, including those with foreign targets, collected and queried without a warrant. They have no practical means to avoid this collection, as it occurs incidentally to foreign intelligence surveillance. Their privacy and Fourth Amendment rights are diminished under this interpretation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, global).

% Actively challenge this interpretation of FISA 702, arguing it undermines Fourth Amendment protections for U.S. persons. They bear the cost of litigation, public advocacy, and legislative lobbying to restrict the scope of incidental collection and warrantless queries. Their efforts are constrained by the secrecy surrounding intelligence operations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, payer,
    organized, generational, constrained, national).

% Are tasked with overseeing FISA 702 programs. They receive classified briefings and reports, but their ability to effectively constrain or reform the program is limited by political considerations, information asymmetry, and the perceived national security imperative. They are a critical but often divided check on executive power.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables national security agencies to rapidly collect and analyze foreign intelligence by providing a framework for surveillance of non-U.S. persons abroad, without requiring individual warrants for every communication.
% TRANSFER_FUNCTION: Transfers privacy rights and Fourth Amendment protections from U.S. persons (whose communications are incidentally collected and queried without warrants) to national security agencies (who gain broad access to intelligence data).
% ABSENT_VOICES: Individual U.S. persons whose data is incidentally collected are unaware of their status and thus cannot object. Their interests are represented by civil liberties advocates, but direct participation is impossible due to the classified nature of the surveillance.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, national security agencies would face significant legal and operational hurdles in conducting foreign intelligence surveillance, potentially requiring warrants for U.S. person data or stricter minimization. This would force a reorganization of intelligence collection practices and likely lead to new legislative efforts to define the boundaries of surveillance.
% FOUNDING_PROBLEM: The need for agile foreign intelligence collection to counter terrorism and other national security threats, particularly after 9/11, which traditional warrant-based surveillance was deemed too slow to address.
% FOUNDING_PROBLEM_CORROBORATION: National security agencies consistently attest that the threat environment remains live and that FISA 702, under this interpretation, is critical. Civil liberties advocates and some legal scholars contest the necessity of this specific interpretation, arguing that less intrusive methods could achieve similar security outcomes, but acknowledge the underlying foreign intelligence imperative.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).
:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is substantial due to the significant privacy intrusion on U.S. persons. Suppression (0.7) is high because the surveillance is covert, and U.S. persons have no practical means to avoid being incidentally collected or having their data queried. Theater ratio (0.2) is low, as the intelligence collection is genuinely functional, though the justification for warrantless U.S. person queries is debated. The metrics reflect the operational reality of this interpretation, independent of the legal claims made by its proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national security agencies, this is a necessary and efficient coordination mechanism for foreign intelligence. From the perspective of U.S. persons and civil liberties advocates, it is an extractive mechanism that bypasses constitutional protections. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and foreign intelligence analysts are beneficiaries, gaining broad access to intelligence. U.S. persons whose data is incidentally collected are victims, bearing the cost of diminished privacy. Civil liberties advocates are also payers, expending resources to challenge the interpretation. Congressional oversight committees act as observers, with limited direct impact on the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_intent_ambiguity,
    'Does the statutory text of FISA Section 702, as originally enacted, genuinely permit the retention and warrantless querying of incidentally collected U.S. person communications, or is this an expansive interpretation?',
    'Legislative history review, originalist textual analysis, and judicial rulings specifically addressing the scope of ''incidental'' collection and ''backdoor searches''.',
    'If the original intent is found to be narrower, the constraint''s legitimacy would be undermined, potentially leading to legislative reform or judicial invalidation, reducing extractiveness and suppression. If the interpretation is upheld as consistent with original intent, its persistence would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_intent_ambiguity, conceptual, 'Ambiguity regarding the original legislative intent behind FISA 702''s scope.').

omega_variable(
    fourth_amendment_applicability,
    'To what extent does the Fourth Amendment''s warrant requirement apply to the querying of U.S. person communications collected under FISA 702, given the ''foreign intelligence purpose'' justification?',
    'Supreme Court ruling on the constitutionality of warrantless U.S. person queries in the FISA 702 context, or a clear legislative amendment clarifying the Fourth Amendment''s role.',
    'A ruling or amendment mandating warrants would significantly reduce extractiveness and suppression for U.S. persons, potentially reclassifying the constraint or forcing its dissolution. A ruling upholding the current practice would entrench the current level of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fourth_amendment_applicability, conceptual, 'Uncertainty regarding the constitutional limits on FISA 702''s incidental collection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_warrant_requirement).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, us_person_privacy_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the FISA Section 702 statutory text kernel. It focuses on the interpretation permitting incidental collection and warrantless queries of U.S. person data. Other readings (foreign_target_strict_reading, constitutional_floor_reading) offer alternative interpretations of the same statutory kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
