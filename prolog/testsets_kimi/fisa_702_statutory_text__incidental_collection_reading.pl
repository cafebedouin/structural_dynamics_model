% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: FISA 702 Incidental Collection and Warrantless Query Authority
 *   domain: legal/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint instantiates the incidental_collection_reading of the
 *   contested kernel fisa_702_statutory_text. The FISA Section 702 statute
 *   authorizes the targeting of non-U.S. persons reasonably believed to be
 *   located abroad for foreign intelligence purposes. Under this reading, the
 *   statute permits the government to retain and conduct warrantless queries
 *   of communications incidentally collected from U.S. persons, provided the
 *   query is justified by a foreign intelligence purpose. The structural
 *   result is that U.S. persons become a victim class: their data enters
 *   intelligence databases without individualized suspicion, and domestic
 *   agencies query that data without obtaining a probable cause warrant. The
 *   Fourth Amendment warrant requirement is displaced by administrative
 *   minimization procedures. The intelligence community and the FBI are the
 *   primary beneficiaries. This is not pure extraction because the foreign
 *   intelligence targeting function is genuine; it is not pure coordination
 *   because the backdoor search authority extracts constitutional protections
 *   from a rights-holding population that cannot exit.
 *
 * KEY AGENTS:
 *   - intelligence_community: Primary agenda-setter (institutional/global) â designs targeting and retention policies, justifies the regime under national security
 *   - fbi_domestic_users: Primary beneficiary (institutional/national) â queries the database for domestic investigations without warrants
 *   - us_persons_incidental: Primary target (powerless/national) â bear the privacy and rights cost without notice or standing
 *   - fisc_oversight: Analytical observer (institutional/national) â reviews procedures but does not adjudicate individual queries
 *   - civil_liberties_advocates: Excluded voice (organized/national) â object but lack standing and access to proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.68).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection and Warrantless Query Authority").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "legal/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6').
narrative_ontology:cs_kernel_codification('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', fixed_text).
narrative_ontology:cs_authority_grounding('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', extraction).
narrative_ontology:cs_interpretation_layer_present('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6').
narrative_ontology:cs_reading_relation('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', foundational, warrantless_query_permitted_for_foreign_intelligence).
narrative_ontology:cs_axiom_status(warrantless_query_permitted_for_foreign_intelligence, holdable).
narrative_ontology:cs_axiom_grounding('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', warrantless_query_permitted_for_foreign_intelligence, conventional).
narrative_ontology:cs_axiom('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', foundational, administrative_minimization_satisfies_fourth_amendment).
narrative_ontology:cs_axiom_status(administrative_minimization_satisfies_fourth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', administrative_minimization_satisfies_fourth_amendment, instrumental).
narrative_ontology:cs_reference_frame('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', foreign_intelligence_targeting_purity).
narrative_ontology:cs_drift_state('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', post_backdoor_search_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e54ef6dc-e8d7-431e-b0b5-7cb10bc8e0b6', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_users).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidental).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_exception).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the foreign targeting collection framework, sets minimization procedures subject to FISC approval, and justifies the retention and warrantless query of incidentally collected U.S. person communications as necessary for national security.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter,
    institutional, generational, constrained, global).

% Accesses the Section 702 database to conduct queries on U.S. person identifiers in support of domestic investigations and national security matters without obtaining a probable cause warrant.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_users, beneficiary,
    institutional, biographical, constrained, national).

% Communications content and metadata are incidentally collected due to contact with foreign targets, retained in government databases, and subjected to later warrantless queries by domestic intelligence and law enforcement; they receive no notice, lack standing to challenge in most cases, and have no practical ability to avoid collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidental, payer,
    powerless, biographical, trapped, national).

% Reviews targeting and minimization procedures in ex parte proceedings, issues annual certifications, and occasionally raises concerns about querying practices but does not typically review or approve individual U.S. person queries.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisc_oversight, observer,
    institutional, generational, analytical, national).

% Represent the privacy and Fourth Amendment interests of U.S. persons but are excluded from FISC proceedings, lack effective mechanisms to challenge individual queries, and face standing barriers that prevent direct adversarial testing of the statutory framework.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables targeted collection of foreign intelligence on non-U.S. persons reasonably believed to be located abroad by using U.S. electronic communications service providers, solving the operational problem that overseas targets communicate through domestic infrastructure.
% TRANSFER_FUNCTION: Moves U.S. person communications content and metadata from private custody into government intelligence databases, then transfers search access to domestic law enforcement and foreign intelligence analysts without an individualized probable cause warrant.
% ABSENT_VOICES: U.S. persons whose data is actually queried are not notified and lack standing; criminal defendants rarely receive notice of Section 702-derived evidence; civil liberties advocates are structurally excluded from ex parte FISC proceedings.
% DISAPPEARANCE_RATIONALE: If the authority to retain and conduct warrantless queries on incidentally collected U.S. person communications disappeared, the FBI would lose a major investigative database, foreign intelligence collection would need to be restructured around narrower targeting or warrant-based querying for U.S. persons, and the boundary between foreign intelligence and domestic surveillance would shift substantially.
% FOUNDING_PROBLEM: Foreign intelligence targets located outside the United States routinely communicate through U.S.-based internet and telephone infrastructure, creating a collection gap because traditional FISA warrants target specific domestic physical spaces and persons rather than overseas electronic communications transiting U.S. networks.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and congressional intelligence committees attest that the foreign targeting gap remains live and that warrant-based collection is impractical for overseas targets. Civil liberties organizations, some FISC amici, and independent post-Snowden review panels corroborate that the program's scale and the routine use of backdoor searches now exceed the original foreign intelligence mandate.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45) reflects the moderate-high cost imposed on U.S. persons: the displacement of the warrant requirement is a genuine extraction of a procedural protection. Suppression (0.68) captures the structural barriers to resistance â state secrets privilege, lack of notice, ex parte FISC proceedings, and congressional gridlock on reform. Theater_ratio (0.40) reflects that minimization procedures and compliance training perform a protective function that is systematically bypassed by the scale of downstream querying. Accessibility_collapse (0.75) is high because practical alternatives (end-to-end encryption, avoiding foreign contact) are extreme or ineffective against upstream collection. Resistance (0.50) reflects persistent but so-far unsuccessful litigation and legislative opposition. The measurement series tracks the gradual normalization of backdoor searches from 2008 to 2024.
 *
 * PERSPECTIVAL GAP:
 *   The intelligence community seat experiences this constraint as a necessary foreign intelligence coordination mechanism with manageable privacy tradeoffs. The U.S. person seat experiences the identical structure as the erosion of Fourth Amendment protections through a statutory backdoor. The FBI seat experiences it as an investigative resource. The engine computes these divergences from the same structural data: low directionality for beneficiaries with institutional exit options, high directionality for powerless trapped targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (intelligence_community, fbi_domestic_users) have constrained or institutional exit and sit at low directionality: the constraint subsidizes their operational capacity. Victims (us_persons_incidental) are powerless and trapped, yielding high directionality: the constraint extracts constitutional protections. The FISC observer seat carries analytical exit and near-neutral directionality. Civil liberties advocates are excluded and carry moderate directionality but do not directly pay the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â foreign intelligence targets abroad using U.S. infrastructure â remains live. However, the solution has drifted: the scale of incidental collection and the routine use of backdoor searches for domestic law enforcement have transformed the constraint from a foreign intelligence scalpel into a domestic surveillance repository. The mandate has not fully atrophied because the foreign function is still real, but the domestic extraction component now drives institutional persistence. This prevents misclassification as a pure snare (there is real coordination) and as a pure rope (there is asymmetric extraction of rights).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Does the incidental collection reading represent the operative legal meaning of the FISA 702 text, or does the foreign target strict reading more accurately capture the statutory intent and historical legislative bargain?',
    'Comparative statutory history analysis, legislative history review, and judicial adoption rates of each reading in FISC and appellate opinions.',
    'If the strict reading is correct, the current query practice is ultra vires and the constraint collapses toward a snare; if this reading is correct, the extraction is statutorily authorized and the classification remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Which statutory reading is legally authoritative').

omega_variable(
    constitutional_floor_irruption,
    'Will the constitutional floor reading eventually override the statutory framework through Supreme Court review, or will the foreign intelligence exception to the Fourth Amendment remain entrenched?',
    'Awaiting Supreme Court ruling on the Fourth Amendment status of incidental collection queries, or legislative repeal of the warrantless query authority.',
    'If the constitutional floor reading prevails, the constraint would need to be restructured as a rope or scaffold with warrant requirements, collapsing extractiveness; if it does not, the current tangled rope persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_floor_irruption, empirical, 'Whether Fourth Amendment warrant requirement will displace statutory authority').

omega_variable(
    domestic_query_foreign_intelligence_boundary,
    'Is the FBI''s querying of Section 702 data for domestic investigations genuinely limited to foreign intelligence purposes, or has the operational boundary dissolved into general domestic surveillance and law enforcement?',
    'Inspector General audits of FBI query justifications, statistical disclosure of query purposes, and adversarial testing in criminal proceedings where 702-derived evidence is used.',
    'If the boundary has dissolved, the coordination function is cover for domestic law enforcement extraction and the constraint shifts toward snare; if the boundary holds, the foreign intelligence coordination function remains genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_query_foreign_intelligence_boundary, empirical, 'Whether domestic querying stays within foreign intelligence scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fisa_tr_t2, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(fisa_tr_t14, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 16, 0.4).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fisa_be_t2, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2, 0.27).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(fisa_be_t14, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 14, 0.43).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 16, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fisa_su_t2, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(fisa_su_t14, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 14, 0.64).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 16, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel fisa_702_statutory_text. The kernel decomposes into three structurally distinct claims: a constitutional floor reading (Fourth Amendment warrant requirement), a foreign target strict reading (statutory minimization and inaccessibility), and this incidental collection reading (retention and warrantless query permitted). Each reading has a different victim set, epsilon, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
