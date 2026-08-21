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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: FISA Section 702 Incidental Collection and Warrantless Query Authority (Incidental Collection Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'incidental collection' reading of
 *   FISA Section 702, which permits the retention and warrantless querying of
 *   U.S. person communications collected incidentally to foreign intelligence
 *   surveillance, provided a foreign intelligence purpose is asserted. This
 *   reading is highly contested, with critics arguing it creates a 'backdoor
 *   search' loophole for domestic law enforcement. The constraint is claimed
 *   as a Tangled Rope, reflecting its dual function of coordinating foreign
 *   intelligence collection while enabling significant extraction of privacy
 *   from U.S. persons.
 *
 * KEY AGENTS:
 *   - national_security_agencies: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - fbi_domestic_investigators: Secondary beneficiary (institutional/constrained)
 *   - us_persons_whose_communications_are_collected: Primary target/victim (powerless/trapped)
 *   - civil_liberties_advocates: Excluded voice (organized/constrained)
 *   - congress: Agenda_setter (institutional/mobile)
 *   - federal_courts: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.75).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection and Warrantless Query Authority (Incidental Collection Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '6909ad86-72c4-4482-89a3-61b8ad510c67').
narrative_ontology:cs_kernel_codification('6909ad86-72c4-4482-89a3-61b8ad510c67', fixed_text).
narrative_ontology:cs_authority_grounding('6909ad86-72c4-4482-89a3-61b8ad510c67', lineage).
narrative_ontology:cs_interpretation_layer_present('6909ad86-72c4-4482-89a3-61b8ad510c67').
narrative_ontology:cs_reading_relation('6909ad86-72c4-4482-89a3-61b8ad510c67', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('6909ad86-72c4-4482-89a3-61b8ad510c67', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('6909ad86-72c4-4482-89a3-61b8ad510c67', foundational, incidental_collection_is_not_a_fourth_amendment_search).
narrative_ontology:cs_axiom_status(incidental_collection_is_not_a_fourth_amendment_search, holdable).
narrative_ontology:cs_axiom_grounding('6909ad86-72c4-4482-89a3-61b8ad510c67', incidental_collection_is_not_a_fourth_amendment_search, conventional).
narrative_ontology:cs_axiom('6909ad86-72c4-4482-89a3-61b8ad510c67', foundational, foreign_intelligence_purpose_justifies_domestic_access).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_domestic_access, holdable).
narrative_ontology:cs_axiom_grounding('6909ad86-72c4-4482-89a3-61b8ad510c67', foreign_intelligence_purpose_justifies_domestic_access, conventional).
narrative_ontology:cs_reference_frame('6909ad86-72c4-4482-89a3-61b8ad510c67', post_fisa_amendments_act_2008).
narrative_ontology:cs_drift_state('6909ad86-72c4-4482-89a3-61b8ad510c67', contemporary_reauthorization_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6909ad86-72c4-4482-89a3-61b8ad510c67', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_whose_communications_are_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for collecting foreign intelligence under FISA Section 702. They interpret the statute to permit retention and warrantless querying of incidentally collected U.S. person communications when justified by a foreign intelligence purpose, benefiting from broad access to data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Access the database of incidentally collected U.S. person communications for domestic investigations without needing a probable cause warrant, under the justification of a foreign intelligence purpose. This provides a significant investigative shortcut.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators, beneficiary,
    institutional, biographical, constrained, national).

% Their communications are collected and queried without a warrant, even if they are not the target of surveillance. They bear the cost of diminished privacy and Fourth Amendment protections, with no practical means to avoid collection or challenge specific queries.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_whose_communications_are_collected, payer,
    powerless, immediate, trapped, universal).

% Actively challenge the interpretation and implementation of FISA Section 702, particularly regarding incidental collection and warrantless queries of U.S. person data. They advocate for stronger privacy protections and warrant requirements but are structurally excluded from direct control over the surveillance operations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Authorizes and periodically reauthorizes FISA Section 702. While it has the power to amend the statute, it often faces pressure from national security agencies to maintain broad authorities, leading to ongoing debates about the scope of surveillance.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress, agenda_setter,
    institutional, generational, mobile, national).

% Review legal challenges to FISA Section 702, including its constitutionality and statutory interpretation. The FISA Court oversees the program but its rulings are often classified and its scope of review is limited, leading to a complex and often opaque judicial oversight.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection of foreign intelligence by enabling national security agencies to monitor non-U.S. persons abroad, thereby protecting national security interests from external threats.
% TRANSFER_FUNCTION: Transfers privacy expectations and Fourth Amendment protections from U.S. persons (whose communications are incidentally collected) to national security agencies, in exchange for perceived national security benefits derived from foreign intelligence collection.
% ABSENT_VOICES: The U.S. persons whose communications are incidentally collected and queried are largely unaware of this surveillance and lack legal standing or practical means to object. Their voices are absent from the policy-making and oversight processes.
% DISAPPEARANCE_RATIONALE: If this authority vanished overnight, national security agencies would lose a critical tool for foreign intelligence collection, forcing a fundamental re-evaluation of surveillance methods and potentially leading to new, more restrictive legal frameworks for accessing U.S. person data, even if incidentally collected. The intelligence community's operations would significantly reorganize.
% FOUNDING_PROBLEM: The need for rapid and effective foreign intelligence collection to counter terrorism and other national security threats, particularly after 9/11, without being constrained by traditional probable cause warrant requirements for communications involving U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: National security officials and some members of Congress consistently attest to the ongoing necessity and effectiveness of this authority for protecting national security. Civil liberties groups, some legal scholars, and former intelligence officials contest its necessity in its current form, arguing that its domestic use has expanded beyond its original foreign intelligence purpose and that less intrusive alternatives exist.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is substantial (0.45) because U.S. persons' private communications are accessed without a warrant, representing a significant privacy cost. Suppression is high (0.75) due to the secrecy of surveillance, the lack of notification to affected individuals, and the limited legal avenues for challenge. Theater ratio is low (0.15) as the collection and querying are genuinely functional for intelligence purposes, though the justification for domestic use is often debated. Accessibility collapse is moderate-high (0.65) as U.S. persons have few practical ways to avoid incidental collection if communicating with foreign targets.
 *
 * PERSPECTIVAL GAP:
 *   National security agencies view this authority as a necessary and legitimate tool for protecting national security, with incidental collection being an unavoidable byproduct of foreign intelligence. From the perspective of U.S. persons and civil liberties advocates, it represents an overreach that undermines Fourth Amendment protections, effectively treating U.S. persons as targets without due process.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and FBI domestic investigators are clear beneficiaries, gaining access to vast amounts of data. U.S. persons whose communications are collected are the primary payers/victims, bearing the cost of privacy erosion. Civil liberties advocates are excluded from direct influence but bear the cost of defending rights. Congress and federal courts play complex roles as agenda-setters and observers, respectively, mediating the contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine foreign intelligence coordination function). The ongoing debate about the 'foreign intelligence purpose' justification for domestic queries highlights the tension between the original mandate and its evolving application. The high extractiveness and suppression, coupled with the contested founding problem status, suggest a risk of mandatrophy where the coordination function becomes cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_intelligence_purpose_ambiguity,
    'To what extent are warrantless queries of incidentally collected U.S. person communications genuinely justified by a foreign intelligence purpose, versus serving primarily domestic law enforcement objectives?',
    'Independent, transparent audits of query practices, including detailed reporting on the primary purpose and outcome of each U.S. person query, with declassified summaries for public review.',
    'If queries are found to predominantly serve domestic law enforcement, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function for foreign intelligence would be seen as more theatrical, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_ambiguity, empirical, 'Ambiguity regarding the true purpose of warrantless U.S. person queries.').

omega_variable(
    fourth_amendment_applicability_ambiguity,
    'Does the Fourth Amendment''s warrant requirement apply to government searches of U.S. person communications content, regardless of whether the collection originated from a foreign intelligence program?',
    'A definitive Supreme Court ruling on the constitutionality of warrantless U.S. person queries of incidentally collected data, or a legislative amendment explicitly requiring warrants for such queries.',
    'If the Fourth Amendment is deemed to apply, this reading of FISA 702 would be foreclosed, and the constraint would be reclassified as a Snare, as its core mechanism would be deemed unconstitutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_amendment_applicability_ambiguity, conceptual, 'Conceptual ambiguity regarding the Fourth Amendment''s reach in foreign intelligence surveillance.').

omega_variable(
    kernel_reading_contest,
    'Is this ''incidental collection'' reading of FISA 702 the most faithful interpretation of the statute, or do alternative readings (e.g., ''foreign target strict'' or ''constitutional floor'') better reflect legislative intent or constitutional requirements?',
    'Ongoing legislative debate, judicial review, and public discourse, with resolution depending on which interpretation gains legal or political supremacy.',
    'If a stricter reading gains ascendancy, the current constraint would be superseded, leading to a new constraint with lower extractiveness and higher privacy protections for U.S. persons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the FISA 702 kernel, contested by sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2012, 0.12).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2016, 0.14).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2020, 0.43).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2016, 0.71).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
