% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 — Foreign Target Strict Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This story instantiates the foreign-target strict reading of the Section
 *   702 kernel: the statute's 'foreign target' language is read as a genuine,
 *   binding limit that excludes U.S. persons from the collection structure
 *   except through individualized warrants, requires deletion-grade
 *   minimization of incidental U.S. person data, and categorically forbids
 *   FBI queries of the 702 database for domestic criminal purposes. Under
 *   this reading the constraint functions largely as a Rope — a targeted
 *   collection mechanism against non-U.S. persons abroad, who lack Fourth
 *   Amendment standing, coordinated through certification and FISC review,
 *   with U.S. persons structurally walled off rather than incidentally
 *   captured. This is a distinct constraint from the
 *   incidental_collection_reading (which permits warrantless query of
 *   incidental U.S. person data) and the constitutional_floor_reading (which
 *   asserts the Fourth Amendment independently requires a warrant regardless
 *   of statutory text) — each of those readings has its own ε and its own
 *   victim set, and is authored as its own story linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.22).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 — Foreign Target Strict Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '64038855-a7e2-4383-b7ee-3f009a37b06e').
narrative_ontology:cs_kernel_codification('64038855-a7e2-4383-b7ee-3f009a37b06e', fixed_text).
narrative_ontology:cs_authority_grounding('64038855-a7e2-4383-b7ee-3f009a37b06e', lineage).
narrative_ontology:cs_interpretation_layer_present('64038855-a7e2-4383-b7ee-3f009a37b06e').
narrative_ontology:cs_reading_relation('64038855-a7e2-4383-b7ee-3f009a37b06e', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('64038855-a7e2-4383-b7ee-3f009a37b06e', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('64038855-a7e2-4383-b7ee-3f009a37b06e', foundational, statutory_text_binds_executive_query_practice).
narrative_ontology:cs_axiom_status(statutory_text_binds_executive_query_practice, holdable).
narrative_ontology:cs_axiom_grounding('64038855-a7e2-4383-b7ee-3f009a37b06e', statutory_text_binds_executive_query_practice, conventional).
narrative_ontology:cs_axiom('64038855-a7e2-4383-b7ee-3f009a37b06e', foundational, minimization_requires_deletion_not_mere_access_gating).
narrative_ontology:cs_axiom_status(minimization_requires_deletion_not_mere_access_gating, holdable).
narrative_ontology:cs_axiom_grounding('64038855-a7e2-4383-b7ee-3f009a37b06e', minimization_requires_deletion_not_mere_access_gating, conventional).
narrative_ontology:cs_axiom('64038855-a7e2-4383-b7ee-3f009a37b06e', secondary, non_us_persons_abroad_lack_fourth_amendment_standing).
narrative_ontology:cs_axiom_status(non_us_persons_abroad_lack_fourth_amendment_standing, holdable).
narrative_ontology:cs_axiom_grounding('64038855-a7e2-4383-b7ee-3f009a37b06e', non_us_persons_abroad_lack_fourth_amendment_standing, deontological).
narrative_ontology:cs_reference_frame('64038855-a7e2-4383-b7ee-3f009a37b06e', textualist_foreign_target_limitation).
narrative_ontology:cs_drift_state('64038855-a7e2-4383-b7ee-3f009a37b06e', post_2011_backdoor_search_disclosures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64038855-a7e2-4383-b7ee-3f009a37b06e', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_analysts).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets_of_legitimate_interest).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_under_strict_reading).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, incidentally_collected_us_persons_under_lax_enforcement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_compatibility_of_targeted_foreign_collection).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_text_as_binding_limit_on_executive_surveillance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer targeting and minimization procedures under FISC oversight. Under this reading, they must certify that both the sender and the primary investigative interest are non-U.S. persons located abroad, and must apply minimization procedures that delete rather than merely restrict access to incidentally collected U.S. person communications. They control the actual query and retention infrastructure and could, in practice, drift from the strict reading absent enforcement.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, nsa_fbi_collection_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Non-U.S. persons abroad who are the actual, lawful subjects of foreign intelligence collection. They have no standing to object domestically and no exit from being targeted, but under this reading the statute confines that targeting to a defined, bounded category rather than permitting general surveillance of any communications touching U.S. infrastructure.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets_of_legitimate_interest, beneficiary,
    powerless, immediate, trapped, global).

% Ordinary U.S. persons whose communications might incidentally touch a targeted foreign account. Under the strict reading, such incidental data must be minimized as deletion, not indexed for later query, and is categorically inaccessible for domestic law-enforcement purposes absent an individualized warrant. They retain Fourth Amendment protection functionally intact because the statute is read to exclude them from the extraction structure entirely.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_under_strict_reading, beneficiary,
    moderate, biographical, mobile, national).

% The same population as above, but under conditions where agencies interpret or apply the statute more loosely than the strict reading requires (compliance failures, backdoor queries later found to be minimization violations). Where the strict reading is not actually enforced, this same population is transformed into the victim class this reading is designed to prevent from existing.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, incidentally_collected_us_persons_under_lax_enforcement, payer,
    powerless, immediate, trapped, national).

% Reviews certifications and minimization procedures for compliance with the strict foreign-target reading. Issues compliance opinions when agencies deviate, but operates largely ex parte and in secret, limiting the extent to which its oversight functions as a genuine external check versus a validating formality.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, agenda_setter).

% Argue that the strict reading, however textually correct, is not the reading actually implemented in practice — that agencies systematically query incidental U.S. person data. They have no standing to appear before the FISC and can only advocate through Congress, litigation (frequently dismissed for lack of standing), or public compliance reports released years after the fact.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Individuals prosecuted using leads or evidence that may trace back to Section 702 collection. Under the strict reading, this pathway should not exist at all, since domestic-purpose queries are categorically prohibited; when it does occur, defendants are frequently barred from learning whether 702 data was the origin of the investigation, foreclosing their ability to challenge the search.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, domestic_criminal_defendants, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a legally bounded, targeted mechanism for collecting foreign intelligence communications relevant to national security, without requiring a warrant for surveillance of non-U.S. persons abroad who have no Fourth Amendment claim, while textually walling off U.S. persons from the collection structure.
% TRANSFER_FUNCTION: Moves foreign-intelligence-relevant communications data from targeted non-U.S. persons abroad to the intelligence community; under the strict reading, moves nothing systematically from U.S. persons, since their incidental data is to be deleted rather than retained or queried.
% ABSENT_VOICES: Civil liberties advocates and domestic criminal defendants would object that the strict reading is a legal fiction unmatched by operational practice; they are excluded from FISC proceedings (ex parte, classified) and often lack standing or notice to challenge collection or its downstream use in court.
% DISAPPEARANCE_RATIONALE: If the strict foreign-target reading were abandoned in favor of a looser interpretation, the population of protected U.S. persons would shrink and the incidental-collection victim class would grow; whether this constitutes 'the world rearranging' is disputed because the intelligence community argues operational continuity would be unaffected (they claim they already comply with the strict standard), while civil liberties advocates argue enforcement gaps mean this reading is already partially fictional in practice.
% FOUNDING_PROBLEM: Post-9/11 and post-2008 FISA Amendments Act reforms sought to authorize efficient foreign intelligence collection on non-U.S. persons using U.S. communications infrastructure, while preserving Fourth Amendment protection for U.S. persons who might be incidentally swept in.
% FOUNDING_PROBLEM_CORROBORATION: The Justice Department and ODNI attest the strict reading is operative and enforced through minimization procedures and FISC review. The Privacy and Civil Liberties Oversight Board (PCLOB), independent of the agencies being overseen, and repeated FISC compliance opinions documenting backdoor-search violations, corroborate that the strict reading is frequently not what happens in practice — supporting the contested status rather than a clean resolution in either direction.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15) because, under this reading, U.S. persons are excluded from the target set by statutory design and their incidental data is to be deleted rather than retained or searched — there is no systematic transfer from a U.S. person victim class under a faithfully implemented strict reading. Suppression is moderate-low (0.22) reflecting the genuine secrecy and classification barriers around minimization procedures and FISC opinions, which limit outside verification even when the substantive standard is followed. Theater ratio (0.30) captures that a meaningful share of compliance activity (FISC review, PCLOB reporting, DOJ certifications) is real but increasingly performs adherence to a standard whose actual operational fidelity is contested — hence the rising trajectory over the interval as compliance-violation reports accumulated post-2011.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-U.S. persons abroad who are legitimate foreign intelligence targets are treated as beneficiaries of the bounded structure (better than unbounded dragnet collection would be, though they bear the actual surveillance burden with no U.S. legal recourse — this is inherent to the foreign-intelligence function itself, not an artifact of this reading). U.S. persons are the central beneficiary class of this specific reading: the strict textual interpretation is what keeps them out of the victim set. The only declared victim group — incidentally collected U.S. persons under lax enforcement — exists precisely at the seam where the strict reading is not actually the operative practice; this population is the measurable gap between the claimed reading and enforcement reality, which is the crux of the kernel dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient, warrant-free collection against non-U.S. persons while protecting U.S. persons) remains live in the sense that the underlying national security interest persists; but PCLOB and repeated FISC compliance opinions documenting backdoor searches corroborate, from outside the intelligence community itself, that the strict reading's protective function has been intermittently 'dead' in practice even while textually 'live.' This is why founding_problem_status is authored as contested rather than resolved in either direction — the classification depends on whether one measures the statute's text or its documented enforcement history, and those two measurements diverge, which is itself evidence for why this natural-language concept ('the 702 foreign-target rule') needed decomposition into separate kernel readings rather than a single averaged constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_reading_enforcement_fidelity,
    'Is the foreign-target strict reading actually the operative practice inside NSA and FBI query procedures, or is it a textually correct but operationally under-enforced standard?',
    'Declassified FISC compliance opinions, PCLOB audits, and DOJ Inspector General reports documenting the rate and nature of backdoor-search violations relative to total query volume over the interval.',
    'If enforcement fidelity is high, this reading''s low ε (0.15) is descriptively accurate and the constraint functions close to a genuine Rope. If enforcement fidelity is low, the victim population (incidentally_collected_us_persons_under_lax_enforcement) is not a residual edge case but the modal outcome, and the true operative constraint is closer to the incidental_collection_reading sibling despite the statutory text supporting this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reading_enforcement_fidelity, empirical, 'Whether the strict statutory reading is the reading actually implemented in agency practice.').

omega_variable(
    minimization_as_deletion_vs_access_restriction,
    'Does ''minimization'' under this reading mean actual deletion of incidental U.S. person data, or merely restricted-access retention (i.e., data kept but query-gated)?',
    'Textual and regulatory analysis of NSA/FBI minimization procedures as approved by FISC, cross-checked against technical audits of actual data retention practices and retention-period disclosures.',
    'If minimization functions as retention-with-access-restriction rather than deletion, the strict reading''s exclusion of U.S. persons from the victim set is weaker than authored here, and this story''s ε would need revision upward — or the constraint reclassified as the incidental_collection_reading in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_as_deletion_vs_access_restriction, empirical, 'Whether minimization procedures in practice match the strict reading''s deletion-based theory.').

omega_variable(
    kernel_framing_alternative_institutional_incentive,
    'Is the strict reading the correct legal interpretation of the statute, or is it the reading favored by oversight bodies because it is the most defensible position to publicly assert, independent of operational reality?',
    'Compare congressional testimony and public DOJ/ODNI statements (which consistently assert the strict reading) against internal compliance memoranda and FISC opinion language (which sometimes describe departures from it) to see whether the public-facing framing and internal operative framing diverge.',
    'If the strict reading is primarily an institutional legitimation narrative rather than the actual operative standard, this story''s classification (Rope) would need to be read alongside the incidental_collection_reading as the more descriptively accurate account of practice, even though this story remains the textually correct kernel reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_institutional_incentive, conceptual, 'Whether the strict reading is chosen for legal correctness or institutional defensibility, and what that implies for classification under this framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2011, 0.22).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2008, 0.1).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2011, 0.12).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2014, 0.13).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2017, 0.14).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2008, 0.18).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2011, 0.19).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2014, 0.2).
narrative_ontology:measurement(fisa_su_t2017, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2017, 0.21).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fisa_702_statutory_text kernel. foreign_target_strict_reading (this story) authors the lowest ε (~0.15), treating U.S. persons as excluded from the collection structure by statutory design. incidental_collection_reading authors a higher ε reflecting a victim class populated by design rather than enforcement failure. constitutional_floor_reading treats the entire statutory question as subordinate to an independent Fourth Amendment warrant requirement, which would foreclose the incidental_collection_reading's premise that a foreign-intelligence purpose alone can justify a warrantless query of U.S. person content. All three stories are linked so that contamination or purity-degradation analysis on one can propagate to the others; none of the three should be read as 'the' 702 constraint in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
