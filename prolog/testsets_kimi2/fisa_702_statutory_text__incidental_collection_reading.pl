% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: FISA 702 Incidental Collection Backdoor Query Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint instantiates the incidental_collection_reading of the
 *   FISA 702 statutory text kernel. It treats the foreign-targeting statute
 *   as permitting the retention and warrantless query of incidentally
 *   collected U.S. person communications whenever a foreign intelligence
 *   purpose is asserted, including by the FBI for domestic investigations.
 *   The reading effectively displaces the Fourth Amendment warrant
 *   requirement with administrative minimization procedures, converting a
 *   foreign-intelligence coordination mechanism into a domestic surveillance
 *   bypass. The kernel is contested by the constitutional_floor_reading
 *   (which asserts an independent Fourth Amendment warrant requirement) and
 *   the foreign_target_strict_reading (which construes statutory language to
 *   prohibit domestic access to incidental data).
 *
 * KEY AGENTS:
 *   - Intelligence community (agenda setter / beneficiary): Operates the collection and querying apparatus under the statutory reading.
 *   - FBI domestic investigators (beneficiary): Access 702 data for domestic investigations without warrants.
 *   - U.S. persons communications (payer): Bear the cost of warrantless surveillance without exit or notice.
 *   - Civil liberties advocates (observer): Challenge the practice through litigation and public advocacy.
 *   - Defense bar (excluded): Would demand warrants and suppression but are excluded from classified procedures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection Backdoor Query Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '246811ba-9073-404a-899a-5e797e52c56e').
narrative_ontology:cs_kernel_codification('246811ba-9073-404a-899a-5e797e52c56e', fixed_text).
narrative_ontology:cs_authority_grounding('246811ba-9073-404a-899a-5e797e52c56e', lineage).
narrative_ontology:cs_interpretation_layer_present('246811ba-9073-404a-899a-5e797e52c56e').
narrative_ontology:cs_reading_relation('246811ba-9073-404a-899a-5e797e52c56e', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('246811ba-9073-404a-899a-5e797e52c56e', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('246811ba-9073-404a-899a-5e797e52c56e', foundational, foreign_intelligence_purpose_supersedes_warrant).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_supersedes_warrant, holdable).
narrative_ontology:cs_axiom_grounding('246811ba-9073-404a-899a-5e797e52c56e', foreign_intelligence_purpose_supersedes_warrant, conventional).
narrative_ontology:cs_axiom('246811ba-9073-404a-899a-5e797e52c56e', foundational, administrative_minimization_satisfies_reasonableness).
narrative_ontology:cs_axiom_status(administrative_minimization_satisfies_reasonableness, holdable).
narrative_ontology:cs_axiom_grounding('246811ba-9073-404a-899a-5e797e52c56e', administrative_minimization_satisfies_reasonableness, conventional).
narrative_ontology:cs_reference_frame('246811ba-9073-404a-899a-5e797e52c56e', foreign_intelligence_primacy_framework).
narrative_ontology:cs_drift_state('246811ba-9073-404a-899a-5e797e52c56e', contemporary_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('246811ba-9073-404a-899a-5e797e52c56e', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_communications).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_exception).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates Section 702 collection and the querying infrastructure, sets minimization procedures, and advocates for the statutory reading that permits retention and warrantless query of incidentally collected U.S. person communications so long as a foreign intelligence purpose is asserted.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, beneficiary).

% Access the 702 database for domestic investigations without obtaining individualized warrants, relying on administrative queries justified by foreign intelligence purpose to obtain U.S. person communications content and metadata.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigators, beneficiary,
    institutional, biographical, constrained, national).

% U.S. persons whose communications are incidentally collected and subsequently queried without a warrant; they cannot prevent collection, are not notified of queries, lack standing in many cases, and have no practical exit from the surveillance architecture.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_communications, payer,
    powerless, immediate, trapped, national).

% Litigate and publicly advocate against backdoor searches, arguing that warrantless queries of U.S. person communications violate the Fourth Amendment; they observe and resist but do not control the statutory interpretation or querying procedures.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Would argue for suppression of evidence derived from unconstitutional queries and demand individualized warrants; structurally excluded from classified querying procedures and minimization rulemaking.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, defense_bar, excluded,
    organized, biographical, constrained, national).

narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes overseas foreign intelligence collection under statutory authority, providing a unified legal framework for targeting non-U.S. persons located abroad and consolidating signals intelligence in one statutory regime.
% TRANSFER_FUNCTION: Moves U.S. person communications content and metadata from private citizens to intelligence and federal law enforcement agencies without individualized probable cause warrants, under the administrative rubric of foreign intelligence purpose.
% ABSENT_VOICES: Criminal defense attorneys and the U.S. persons whose communications are actually queried are excluded from the administrative procedure design; they would demand individualized warrants, notice, and suppression remedies if present in the rulemaking and querying rooms.
% DISAPPEARANCE_RATIONALE: If the statutory authority for warrantless backdoor queries of U.S. person communications vanished, the FBI and intelligence agencies would lose a major source of domestic investigative leads, queries would require probable cause warrants, and the architecture of foreign-intelligence-driven domestic surveillance would contract sharply.
% FOUNDING_PROBLEM: Overseas intelligence collection after 2001 faced statutory gaps and technological fragmentation; the original FISA framework was seen as ill-suited to the speed, volume, and transnational routing of modern digital communications.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence community and congressional intelligence committees attest the foreign threat remains live. Civil liberties advocates, some FISC opinions, and the Privacy and Civil Liberties Oversight Board attest the founding problem has been subsumed by mission creep, documenting tension between the foreign targeting mandate and routine domestic law enforcement access.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.45 because the constraint systematically transfers privacy and informational autonomy from U.S. persons to state agencies without individualized judicial process. Suppression is 0.72 because the arrangement depends on state secrecy, standing barriers, and classification to prevent judicial remedy and public exit. Theater ratio is 0.55: minimization procedures perform compliance while backdoor searches routinely serve domestic investigative ends. Accessibility collapse is 0.65 because, once communications enter the 702 architecture, the warrant alternative collapses into an administrative query form. Resistance is 0.45 due to active litigation and some congressional oversight, partially neutralized by secrecy doctrines.
 *
 * PERSPECTIVAL GAP:
 *   The intelligence community and FBI experience the constraint as necessary coordination for national security; from their seats, the constraint solves a foreign intelligence collection problem with acceptable administrative safeguards. U.S. persons experience it as extraction of Fourth Amendment rights without consent, notice, or exit. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The intelligence community and FBI are structural beneficiaries: they collect the informational gains and control the enforcement machinery, placing their directionality near the subsidy end. U.S. persons are structural victims: they bear the privacy cost, lack exit, and face active suppression of alternatives, placing their directionality near the full-target end. Civil liberties advocates and the defense bar sit at analytical or constrained positions but do not reorient the core extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve a foreign intelligence coordination problem (transnational digital collection after 9/11). Under this reading, it has accumulated a domestic law enforcement extraction function that exceeds the original foreign-targeting mandate. The mandatrophy risk lies in using the foreign intelligence rationale to shield routine domestic surveillance: the founding problem remains cited as live while the operational reality has drifted toward general domestic investigative infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'Is this constraint a faithful statutory reading or an interpretive expansion that exceeds the kernel''s textual boundaries?',
    'Comparative textual analysis of the foreign-targeting language against agency query logs and FISC opinions; judicial review of whether ''incidental'' collection encompasses systematic backdoor searches.',
    'If the reading exceeds statutory text, the constraint reverts toward a snare or more extractive tangled rope; if textually grounded, the extraction is better described as a coordination cost of statutory foreign intelligence authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Ambiguity about whether incidental collection reading is statutory or interpretive overreach.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Fourth Amendment alternatives structural (state secrecy, standing barriers, classification) or internalized (U.S. persons'' privacy expectations administratively redefined)?',
    'Post-disclosure behavioral surveys and litigation rates: if U.S. persons alter communications behavior or file suit when informed, suppression is partly structural; if indifference persists, internalized redefinition is dominant.',
    'If internalized, effective suppression exceeds the structural measure because the target population carries the constraint even after potential legal victories or statutory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of Fourth Amendment rights.').

omega_variable(
    minimization_procedure_efficacy,
    'Do minimization procedures actually constrain query practices, or do they function as theatrical compliance that masks routine domestic access?',
    'Audited query logs, compliance reviews, and FISC disclosures documenting the volume, justification, and domestic investigative use of U.S. person queries.',
    'If minimization is theatrical, theater_ratio rises and the constraint edges toward snare; if genuinely constraining, the coordination function is stronger and the rope component is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_procedure_efficacy, empirical, 'Efficacy of minimization procedures as genuine constraint or theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the FISA 702 statutory text kernel. It is structurally linked to sibling readings that interpret the same statutory language with opposing beneficiary/victim structures and constitutional status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
