% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 Foreign Target Rule (Strict Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a strict reading of FISA Section 702, where
 *   collection is limited to communications involving non-U.S. persons abroad
 *   as both sender and primary investigative interest. It mandates rigorous
 *   minimization of incidentally collected U.S. person data, prohibiting its
 *   use for domestic law enforcement without a warrant. This reading aims to
 *   uphold Fourth Amendment protections for U.S. persons while enabling
 *   foreign intelligence collection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.2).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 Foreign Target Rule (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '14e1418c-36f0-477b-9d48-8e7e6930fb46').
narrative_ontology:cs_kernel_codification('14e1418c-36f0-477b-9d48-8e7e6930fb46', fixed_text).
narrative_ontology:cs_authority_grounding('14e1418c-36f0-477b-9d48-8e7e6930fb46', lineage).
narrative_ontology:cs_interpretation_layer_present('14e1418c-36f0-477b-9d48-8e7e6930fb46').
narrative_ontology:cs_reading_relation('14e1418c-36f0-477b-9d48-8e7e6930fb46', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('14e1418c-36f0-477b-9d48-8e7e6930fb46', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('14e1418c-36f0-477b-9d48-8e7e6930fb46', foundational, statutory_text_governs_collection_scope).
narrative_ontology:cs_axiom_status(statutory_text_governs_collection_scope, holdable).
narrative_ontology:cs_axiom_grounding('14e1418c-36f0-477b-9d48-8e7e6930fb46', statutory_text_governs_collection_scope, conventional).
narrative_ontology:cs_axiom('14e1418c-36f0-477b-9d48-8e7e6930fb46', foundational, minimization_as_deletion_or_strict_access_prohibition).
narrative_ontology:cs_axiom_status(minimization_as_deletion_or_strict_access_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('14e1418c-36f0-477b-9d48-8e7e6930fb46', minimization_as_deletion_or_strict_access_prohibition, deontological).
narrative_ontology:cs_reference_frame('14e1418c-36f0-477b-9d48-8e7e6930fb46', original_statutory_intent_and_fourth_amendment_balance).
narrative_ontology:cs_drift_state('14e1418c-36f0-477b-9d48-8e7e6930fb46', contemporary_practice_and_judicial_interpretations, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('14e1418c-36f0-477b-9d48-8e7e6930fb46', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, national_security_officials).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect foreign intelligence information from non-U.S. persons abroad, operating under the statutory framework. This reading emphasizes their mandate to protect national security while adhering to strict minimization rules for U.S. person data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the intelligence gathered under Section 702, which is critical for counterterrorism and counterproliferation efforts. This reading ensures the utility of the program while maintaining legal safeguards.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, national_security_officials, beneficiary,
    institutional, biographical, constrained, national).

% Are the primary targets of Section 702 collection, as their communications are deemed to have foreign intelligence value. Under this strict reading, their data is collected, but U.S. person data is rigorously protected.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad, payer,
    powerless, immediate, trapped, global).

% Are protected by the strict minimization and domestic use prohibitions in this reading, ensuring their communications are not subject to warrantless searches for domestic law enforcement purposes, even if incidentally collected.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, generational, mobile, national).

% Oversee the Section 702 program, ensuring compliance with statutory requirements. This reading aligns with their role in upholding strict interpretations of surveillance law.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisc_judges, observer,
    institutional, generational, analytical, national).

% Monitor the implementation of Section 702, advocating for interpretations that maximize privacy protections for U.S. persons and minimize the scope of collection. This strict reading largely aligns with their goals for U.S. person data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection of vital foreign intelligence information by intelligence agencies, ensuring that national security interests are met while establishing clear boundaries for the protection of U.S. person data.
% TRANSFER_FUNCTION: Transfers foreign intelligence information from non-U.S. persons abroad to U.S. intelligence agencies, while strictly minimizing and restricting access to any incidentally collected U.S. person data.
% ABSENT_VOICES: Foreign governments and international human rights organizations, who would argue for broader privacy protections for non-U.S. persons, are largely excluded from the domestic U.S. legal and policy debate surrounding Section 702.
% DISAPPEARANCE_RATIONALE: If this strict reading of Section 702 vanished, intelligence agencies would either face severe restrictions on foreign intelligence collection or operate with significantly fewer safeguards for U.S. person data, leading to a major reorganization of surveillance policy and practice.
% FOUNDING_PROBLEM: The need to collect foreign intelligence from non-U.S. persons located outside the United States without requiring individualized warrants, while simultaneously protecting the Fourth Amendment rights of U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence agencies and national security officials attest that the problem of collecting foreign intelligence remains live. Civil liberties advocates corroborate the need for robust U.S. person protections, supporting the 'live' status of the problem from a rights perspective.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because this reading prioritizes U.S. person protections, treating any incidental collection as a cost to be minimized rather than a resource to be exploited. Suppression is low (0.2) as U.S. persons retain strong legal avenues to challenge improper collection. Theater ratio is low (0.1) because the minimization and domestic use prohibitions are taken seriously and are not merely performative. The metrics reflect a genuine attempt at coordination with strong rights safeguards.
 *
 * PERSPECTIVAL GAP:
 *   While intelligence agencies benefit from the foreign intelligence, this reading imposes significant constraints on their use of incidentally collected U.S. person data, creating a tension between their collection mandate and rights protection. Civil liberties advocates would see this reading as a necessary, though still imperfect, safeguard.
 *
 * DIRECTIONALITY LOGIC:
 *   Intelligence agencies and national security officials are beneficiaries, gaining critical foreign intelligence. Non-U.S. persons abroad are payers, as their communications are collected. U.S. persons are beneficiaries, as their data is protected from warrantless domestic searches. This reading ensures a low directionality for U.S. persons, reflecting their protected status.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_minimization_effectiveness,
    'How effective are the minimization procedures in practice at preventing U.S. person data from being accessed or used for domestic purposes?',
    'Independent audits by the Privacy and Civil Liberties Oversight Board (PCLOB) or a special master, with full access to collection and query logs.',
    'If minimization is found to be ineffective, the actual extractiveness for U.S. persons would be significantly higher, potentially reclassifying the constraint as a Tangled Rope or Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_minimization_effectiveness, empirical, 'Empirical effectiveness of minimization procedures for U.S. person data.').

omega_variable(
    scope_of_foreign_intelligence_purpose,
    'How broadly is ''foreign intelligence purpose'' interpreted by intelligence agencies, and does this interpretation inadvertently expand collection to include U.S. persons?',
    'Judicial review by the FISA Court of specific targeting and minimization procedures, with public release of declassified opinions.',
    'A broad interpretation could lead to more U.S. person data being incidentally collected and retained, increasing extractiveness and suppression for U.S. persons, pushing the constraint towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_foreign_intelligence_purpose, conceptual, 'Interpretation of ''foreign intelligence purpose'' and its impact on U.S. person data.').

omega_variable(
    statutory_vs_constitutional_floor,
    'Is the statutory language of Section 702, even under a strict reading, sufficient to meet the Fourth Amendment''s constitutional floor for U.S. person privacy?',
    'Supreme Court ruling on the constitutionality of Section 702''s U.S. person protections, particularly regarding incidental collection and database queries.',
    'If found insufficient, the entire statutory framework would be challenged, potentially requiring individualized warrants for any U.S. person data, fundamentally altering the constraint''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_vs_constitutional_floor, conceptual, 'Whether statutory protections align with constitutional requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
