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
 *   human_readable: FISA Section 702: Incidental Collection & Warrantless Query Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'incidental collection' reading of
 *   FISA Section 702, which permits the retention and warrantless querying of
 *   U.S. person communications collected incidentally to foreign intelligence
 *   surveillance. This reading is highly contested, with national security
 *   agencies asserting its necessity and civil liberties groups arguing it
 *   constitutes an unconstitutional 'backdoor search' of Americans. The
 *   constraint is claimed as a 'tangled_rope' by its proponents (balancing
 *   foreign intelligence with domestic security), but its metrics reflect
 *   significant extraction and suppression from the perspective of affected
 *   U.S. persons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.75).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702: Incidental Collection & Warrantless Query Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '0616370c-9b96-49e8-9873-6ca19493342d').
narrative_ontology:cs_kernel_codification('0616370c-9b96-49e8-9873-6ca19493342d', fixed_text).
narrative_ontology:cs_authority_grounding('0616370c-9b96-49e8-9873-6ca19493342d', lineage).
narrative_ontology:cs_interpretation_layer_present('0616370c-9b96-49e8-9873-6ca19493342d').
narrative_ontology:cs_reading_relation('0616370c-9b96-49e8-9873-6ca19493342d', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('0616370c-9b96-49e8-9873-6ca19493342d', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('0616370c-9b96-49e8-9873-6ca19493342d', foundational, foreign_intelligence_purpose_justifies_incidental_collection).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_incidental_collection, holdable).
narrative_ontology:cs_axiom_grounding('0616370c-9b96-49e8-9873-6ca19493342d', foreign_intelligence_purpose_justifies_incidental_collection, instrumental).
narrative_ontology:cs_axiom('0616370c-9b96-49e8-9873-6ca19493342d', foundational, minimization_procedures_satisfy_fourth_amendment).
narrative_ontology:cs_axiom_status(minimization_procedures_satisfy_fourth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('0616370c-9b96-49e8-9873-6ca19493342d', minimization_procedures_satisfy_fourth_amendment, conventional).
narrative_ontology:cs_reference_frame('0616370c-9b96-49e8-9873-6ca19493342d', post_9_11_intelligence_framework).
narrative_ontology:cs_drift_state('0616370c-9b96-49e8-9873-6ca19493342d', contemporary_oversight_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0616370c-9b96-49e8-9873-6ca19493342d', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the surveillance programs, collect foreign intelligence, and define 'foreign intelligence purpose' to justify retention and querying of incidentally collected U.S. person data. They benefit from broad access to intelligence without traditional warrant requirements.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Access the Section 702 database to query U.S. person communications for domestic law enforcement purposes, often without a warrant, under the justification of a foreign intelligence purpose. This provides a 'backdoor' to intelligence relevant to domestic investigations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators, beneficiary,
    institutional, biographical, constrained, national).

% Have their communications, including content, collected and retained without a warrant if they communicate with a foreign target. They are unaware of this collection and have no means to opt out or prevent it, bearing the cost of privacy erosion.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, immediate, trapped, universal).

% Actively challenge the legality and constitutionality of this interpretation, arguing it undermines Fourth Amendment rights. They are excluded from the operational decision-making and judicial review processes that uphold this reading.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Provide legislative oversight of intelligence activities, including Section 702. They receive classified briefings and can propose amendments or reforms, but often face resistance from intelligence agencies and political divisions.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% Reviews and approves the procedures for Section 702 collection and minimization, effectively sanctioning the retention and querying of U.S. person data under this reading. Its proceedings are secret, limiting public and adversarial challenge.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, federal_intelligence_surveillance_court, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, national_security_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection of foreign intelligence with the need for domestic security, allowing intelligence agencies to rapidly access information relevant to national security threats, including those with a domestic nexus.
% TRANSFER_FUNCTION: Transfers privacy rights and Fourth Amendment protections of U.S. persons to the operational flexibility and intelligence gathering capabilities of national security and domestic law enforcement agencies, enabling warrantless access to their communications.
% ABSENT_VOICES: The millions of U.S. persons whose communications are incidentally collected are unaware of this surveillance and cannot object. Their voices are absent from the legal and policy debates that shape this constraint.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, national security agencies would lose a significant source of intelligence, particularly for threats with a foreign-domestic overlap. FBI domestic investigations would be forced to seek traditional warrants for U.S. person data, fundamentally altering intelligence and law enforcement practices and requiring new legal frameworks.
% FOUNDING_PROBLEM: The need for agile intelligence collection to counter evolving foreign threats, particularly terrorism, after 9/11, which traditional warrant requirements were deemed too slow or ill-suited to address.
% FOUNDING_PROBLEM_CORROBORATION: National security officials and some members of Congress consistently attest to the ongoing necessity of Section 702 for countering terrorism and other foreign threats. Civil liberties groups and some legal scholars, however, argue that while foreign threats are live, this specific interpretation of 702 is an overreach and that the founding problem could be addressed with more privacy-protective measures; their arguments are supported by independent legal analysis and oversight reports.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate-to-high, reflecting the significant privacy cost borne by U.S. persons whose data is accessed without a warrant. Suppression (0.75) is high because U.S. persons have no knowledge or recourse regarding incidental collection, and the legal framework actively prevents challenges. Theater ratio (0.40) is moderate, as there is a genuine foreign intelligence function, but the 'minimization' procedures often serve to legitimize domestic access rather than strictly protect U.S. person data. Accessibility collapse (0.80) is high for U.S. persons, as they cannot avoid communicating with foreign targets and thus cannot opt out of incidental collection. Resistance (0.50) is moderate, driven by civil liberties groups and some legislative efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national security agencies, this reading is a necessary coordination mechanism for protecting national security. From the perspective of U.S. persons and civil liberties advocates, it is a highly extractive mechanism that bypasses constitutional protections. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and FBI domestic investigators are clear beneficiaries, gaining access to intelligence and investigative leads. U.S. persons whose communications are incidentally collected are the primary targets, bearing the cost of privacy erosion and warrantless surveillance. Civil liberties advocates are also victims, as their efforts to protect constitutional rights are undermined by this interpretation. Congressional oversight committees and the FISC act as observers and administrators, respectively, with their own institutional constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (foreign intelligence collection) is still live, but this reading allows for significant 'function creep' into domestic law enforcement. The high extractiveness and suppression, coupled with the contested status of the founding problem's solution, suggest that while coordination exists, it is heavily skewed towards extraction. The 'foreign intelligence purpose' justification acts as a cover for broader domestic access, preventing a clear resolution of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine interpretation of FISA Section 702, or an overreach that fundamentally alters the statute''s intent?',
    'Supreme Court ruling on the constitutionality of warrantless U.S. person queries, or legislative amendment explicitly prohibiting such queries.',
    'If ruled an overreach, the constraint''s legitimacy would collapse, forcing a re-evaluation of all related intelligence practices. If upheld, this reading would be further entrenched, increasing its effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the fidelity of this reading to the original statutory and constitutional intent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by U.S. persons primarily structural (lack of legal recourse, secrecy) or internalized (lack of awareness, perceived futility of resistance)?',
    'Public awareness campaigns and legal challenges: if resistance increases significantly with awareness, the internalized component was larger; if structural barriers remain insurmountable, it is primarily structural.',
    'If internalized, effective suppression is higher than measured, as individuals carry the suppression with them. If purely structural, legal reforms could more directly alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for U.S. persons.').

omega_variable(
    foreign_intelligence_purpose_drift,
    'To what extent is the ''foreign intelligence purpose'' justification genuinely primary for U.S. person queries, versus serving as a pretext for domestic law enforcement access?',
    'Independent audits of FBI query practices, disaggregated data on the foreign vs. domestic nexus of queries, and judicial review of specific cases where foreign intelligence justification is challenged.',
    'If found to be primarily a pretext, the constraint''s ''coordination'' function would be exposed as cover, reclassifying it closer to a pure snare. If genuinely primary, the tangled_rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_drift, empirical, 'Whether the stated foreign intelligence purpose is the true driver of U.S. person data access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2010, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(fisa_tr_t2013, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2016, 0.36).
narrative_ontology:measurement(fisa_tr_t2019, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(fisa_tr_t2022, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2022, 0.39).
narrative_ontology:measurement(fisa_tr_t2025, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2010, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(fisa_be_t2013, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(fisa_be_t2019, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2019, 0.43).
narrative_ontology:measurement(fisa_be_t2022, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2022, 0.44).
narrative_ontology:measurement(fisa_be_t2025, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2010, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(fisa_su_t2013, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2013, 0.68).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(fisa_su_t2019, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(fisa_su_t2022, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement(fisa_su_t2025, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_interpretation).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, domestic_surveillance_authorities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
