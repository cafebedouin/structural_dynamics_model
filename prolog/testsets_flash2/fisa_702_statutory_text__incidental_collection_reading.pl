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
 *   Section 702, which permits intelligence agencies to retain and query U.S.
 *   person communications collected incidentally to foreign intelligence
 *   targeting, without a warrant, for foreign intelligence purposes. This
 *   interpretation is contested, with other readings advocating for stricter
 *   minimization or a warrant requirement for U.S. person data. This specific
 *   reading is characterized by high extractiveness from U.S. persons and
 *   significant suppression of their Fourth Amendment rights, maintained by
 *   active enforcement and a broad interpretation of 'foreign intelligence
 *   purpose'.
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
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'ce27c913-4b31-4dd8-9de7-fd43f855732c').
narrative_ontology:cs_kernel_codification('ce27c913-4b31-4dd8-9de7-fd43f855732c', fixed_text).
narrative_ontology:cs_authority_grounding('ce27c913-4b31-4dd8-9de7-fd43f855732c', lineage).
narrative_ontology:cs_interpretation_layer_present('ce27c913-4b31-4dd8-9de7-fd43f855732c').
narrative_ontology:cs_reading_relation('ce27c913-4b31-4dd8-9de7-fd43f855732c', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce27c913-4b31-4dd8-9de7-fd43f855732c', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('ce27c913-4b31-4dd8-9de7-fd43f855732c', foundational, foreign_intelligence_purpose_justifies_warrantless_query).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_warrantless_query, holdable).
narrative_ontology:cs_axiom_grounding('ce27c913-4b31-4dd8-9de7-fd43f855732c', foreign_intelligence_purpose_justifies_warrantless_query, conventional).
narrative_ontology:cs_axiom('ce27c913-4b31-4dd8-9de7-fd43f855732c', foundational, incidental_collection_does_not_trigger_fourth_amendment).
narrative_ontology:cs_axiom_status(incidental_collection_does_not_trigger_fourth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('ce27c913-4b31-4dd8-9de7-fd43f855732c', incidental_collection_does_not_trigger_fourth_amendment, conventional).
narrative_ontology:cs_reference_frame('ce27c913-4b31-4dd8-9de7-fd43f855732c', post_9_11_surveillance_paradigm).
narrative_ontology:cs_drift_state('ce27c913-4b31-4dd8-9de7-fd43f855732c', contemporary_oversight_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ce27c913-4b31-4dd8-9de7-fd43f855732c', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the surveillance programs under FISA Section 702, collecting foreign intelligence. They benefit from the broad interpretation that allows retention and querying of U.S. person data without a warrant, arguing it is essential for national security. Their exit options are constrained by statutory limits and oversight.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Access the incidentally collected U.S. person data from the 702 database for domestic law enforcement purposes, often without a warrant, through 'backdoor searches'. This provides a significant investigative advantage, bypassing traditional Fourth Amendment requirements. Their exit options are constrained by legal challenges and potential legislative changes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators, beneficiary,
    institutional, biographical, constrained, national).

% Have their communications, including those with foreign targets, collected and retained without their knowledge or consent. Their data can be queried by domestic law enforcement without a warrant. They have no practical means to avoid this collection or query, as it occurs incidentally to foreign intelligence targeting.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, global).

% Actively challenge the broad interpretation of Section 702, arguing it violates Fourth Amendment rights. They bear the cost of litigation, public education, and lobbying efforts. Their exit options are limited to legal and political avenues, which are slow and resource-intensive.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, payer,
    organized, generational, constrained, national).

% Provide oversight of intelligence activities, including FISA Section 702. They receive classified briefings and can legislate changes to the statute. Their situation is one of balancing national security interests with civil liberties concerns, often with incomplete information.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% Are the primary targets of FISA Section 702 surveillance. While not U.S. persons, their communications are collected, and their interactions with U.S. persons lead to the incidental collection that is the subject of this reading. They have no legal standing or practical recourse.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_targets, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables intelligence agencies to collect vital foreign intelligence by targeting non-U.S. persons abroad, providing a mechanism for national security protection against foreign threats.
% TRANSFER_FUNCTION: Transfers privacy rights and Fourth Amendment protections from U.S. persons (whose data is incidentally collected and queried without warrants) to intelligence and domestic law enforcement agencies (who gain access to a vast database of communications for foreign intelligence and domestic investigative purposes).
% ABSENT_VOICES: The millions of U.S. persons whose communications are incidentally collected and retained, and whose data is subject to warrantless queries, are largely unaware and unrepresented in the policy debate. Their voices would demand stronger Fourth Amendment protections and a warrant requirement for any search of U.S. person data.
% DISAPPEARANCE_RATIONALE: If the statutory text permitting incidental collection and warrantless query of U.S. person data vanished overnight, intelligence agencies would face significant operational challenges in collecting foreign intelligence, and domestic law enforcement would lose a powerful investigative tool. The legal and operational landscape of U.S. surveillance would fundamentally reorganize around stricter Fourth Amendment interpretations.
% FOUNDING_PROBLEM: The need for effective foreign intelligence collection to protect national security, particularly in the context of evolving global threats, while navigating the complexities of modern communications that frequently involve U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence agencies and national security officials consistently attest that the problem of foreign threats is live and evolving, requiring robust intelligence capabilities. Civil liberties advocates and some legal scholars contest the necessity of this specific interpretation for solving the problem, arguing that less intrusive methods could achieve similar security outcomes while better protecting rights.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is substantial because U.S. persons lose significant privacy and Fourth Amendment protections without due process. Suppression (0.7) is high due to the classified nature of the program, the lack of individual notice, and the difficulty of challenging incidental collection. Theater ratio (0.2) is low, as the program is genuinely operational, though its justifications are increasingly scrutinized. The claimed type is 'tangled_rope' because it serves a genuine foreign intelligence coordination function but simultaneously extracts from U.S. persons through asymmetric application of surveillance powers.
 *
 * PERSPECTIVAL GAP:
 *   Intelligence agencies view this interpretation as a necessary tool for national security, a coordination mechanism for protecting the nation. Civil liberties advocates and affected U.S. persons view it as an extractive mechanism that erodes constitutional rights under the guise of foreign intelligence. The engine's classification will reflect this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Intelligence agencies and FBI domestic investigators are clear beneficiaries, gaining access to vast amounts of data. U.S. persons whose data is incidentally collected are the primary victims, bearing the cost of warrantless surveillance. Civil liberties advocates also bear costs through their efforts to challenge the program. Congressional oversight committees are observers, balancing competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_amendment_applicability,
    'Does the Fourth Amendment''s warrant requirement apply to the querying of incidentally collected U.S. person communications, regardless of the initial foreign intelligence purpose?',
    'Supreme Court ruling explicitly addressing the warrant requirement for U.S. person queries in the 702 database, or legislative amendment to FISA Section 702.',
    'If the Fourth Amendment is deemed to apply, this reading would be foreclosed, and the constraint would shift towards a ''snare'' (if the program persists without legal basis) or be dismantled. If not, this reading''s legal foundation is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fourth_amendment_applicability, conceptual, 'Ambiguity regarding the constitutional floor for U.S. person data access.').

omega_variable(
    foreign_intelligence_purpose_scope,
    'What is the true scope of ''foreign intelligence purpose'' as a justification for querying U.S. person data, and how often are queries primarily driven by domestic law enforcement interests?',
    'Independent audits by the Privacy and Civil Liberties Oversight Board (PCLOB) or a special master, with public reporting on the primary purpose of U.S. person queries.',
    'If a significant portion of queries are found to be primarily domestic, the ''coordination'' aspect of this tangled_rope would be revealed as cover, pushing the classification towards ''snare''. If genuinely foreign intelligence-driven, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_scope, empirical, 'Ambiguity regarding the true intent and scope of warrantless queries.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the FISA Section 702 statutory text, or an overreach that fundamentally alters the statute''s original intent?',
    'Analysis of legislative history, original intent arguments, and comparison with alternative readings (foreign_target_strict_reading, constitutional_floor_reading) by legal scholars and courts.',
    'If deemed an overreach, this reading''s legitimacy would be undermined, potentially leading to its rejection or reinterpretation. If affirmed as a valid reading, its persistence is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''incidental_collection_reading'' of the ''fisa_702_statutory_text'' kernel. Sibling readings (foreign_target_strict_reading, constitutional_floor_reading) offer alternative interpretations that would significantly alter the constraint''s beneficiary/victim structure and extractiveness.').


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
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_warrant_requirement).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the FISA Section 702 statutory text kernel. Each reading instantiates a different constraint with unique structural properties and extractiveness. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
