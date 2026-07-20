% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign-Target Strict Statutory Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story models the foreign_target_strict_reading of the
 *   fisa_702_statutory_text kernel. Under this reading, the statutory text
 *   imposes a hard boundary: collection is permissible only where the target
 *   is a non-U.S. person abroad, incidental U.S. person data must be
 *   minimized through deletion rather than mere access restriction, and the
 *   FBI may not query the resulting database for domestic criminal purposes
 *   absent an individualized warrant. The reading is presented by its
 *   proponents as a privacy-protective coordination device that enables
 *   foreign intelligence while preserving Fourth Amendment boundaries.
 *
 * KEY AGENTS:
 *   - us_persons: Primary beneficiary (organized/mobile) â structurally shielded from warrantless collection by the foreign-target wall and minimization rules.
 *   - intelligence_community: Primary payer (institutional/constrained) â bears the operational burden of targeting restrictions and deletion minimization.
 *   - fbi_domestic_intelligence: Secondary payer (institutional/constrained) â categorically prohibited from using 702 data for domestic criminal queries without a warrant.
 *   - congress: Agenda-setter (institutional/analytical) â enacts and reauthorizes the statutory scheme.
 *   - privacy_civil_liberties_advocates: Observer (organized/analytical) â monitors compliance and argues for strict construction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.25).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign-Target Strict Statutory Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '165ee127-6466-46a5-b699-c800885f33bd').
narrative_ontology:cs_kernel_codification('165ee127-6466-46a5-b699-c800885f33bd', formalized).
narrative_ontology:cs_authority_grounding('165ee127-6466-46a5-b699-c800885f33bd', lineage).
narrative_ontology:cs_interpretation_layer_present('165ee127-6466-46a5-b699-c800885f33bd').
narrative_ontology:cs_reading_relation('165ee127-6466-46a5-b699-c800885f33bd', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('165ee127-6466-46a5-b699-c800885f33bd', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('165ee127-6466-46a5-b699-c800885f33bd', foundational, foreign_target_statutory_exclusivity).
narrative_ontology:cs_axiom_status(foreign_target_statutory_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('165ee127-6466-46a5-b699-c800885f33bd', foreign_target_statutory_exclusivity, conventional).
narrative_ontology:cs_axiom('165ee127-6466-46a5-b699-c800885f33bd', foundational, minimization_by_deletion_not_restriction).
narrative_ontology:cs_axiom_status(minimization_by_deletion_not_restriction, holdable).
narrative_ontology:cs_axiom_grounding('165ee127-6466-46a5-b699-c800885f33bd', minimization_by_deletion_not_restriction, conventional).
narrative_ontology:cs_reference_frame('165ee127-6466-46a5-b699-c800885f33bd', foreign_target_privacy_wall).
narrative_ontology:cs_drift_state('165ee127-6466-46a5-b699-c800885f33bd', contemporary_query_practice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('165ee127-6466-46a5-b699-c800885f33bd', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_intelligence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communications content and metadata receive statutory shielding under the foreign-target limitation; incidental collection must be minimized through deletion; any domestic access requires an individualized warrant. They do not choose to participate in the constraint but are structurally protected by it.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, biographical, mobile, national).

% Conducts foreign intelligence collection under certifications approved by the FISC. Must ensure targeting is limited to non-U.S. persons located abroad, implement minimization procedures that delete incidental U.S. person data, and refrain from domestic-law queries of the resulting repository.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community, payer,
    institutional, generational, constrained, national).

% Prohibited from querying the 702-acquired database for ordinary domestic criminal investigations. To access U.S. person content, must obtain a probable-cause warrant from a federal court, functionally treating the statutory wall as a hard barrier.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_intelligence, payer,
    institutional, biographical, constrained, national).

% Enacted the foreign-target language and periodically reauthorizes the surveillance framework. Sets the statutory definitions of targeting, minimization, and query limitations that the intelligence community and courts must follow.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Monitor FISC opinions, compliance reports, and reauthorization debates to argue that the strict reading be maintained; publish analysis documenting drift toward broader retention or query practices.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, privacy_civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables targeted foreign intelligence collection against non-U.S. persons overseas while preserving a statutory wall that prevents the same mechanism from becoming a general domestic surveillance tool.
% TRANSFER_FUNCTION: Transfers surveillance authority from an unrestricted domestic/foreign boundary to a strictly foreign-target domain, while transferring privacy protection to U.S. persons via minimization and warrant requirements.
% ABSENT_VOICES: Foreign targets of collection lack U.S. standing and are not represented in FISC proceedings; U.S. persons whose data is incidentally collected are typically unaware and unrepresented; independent technical compliance auditors outside the IC are rarely part of the oversight conversation.
% DISAPPEARANCE_RATIONALE: If the foreign-target limitation and minimization mandate disappeared overnight, the statutory boundary between foreign intelligence and domestic law enforcement surveillance would collapse. The IC could retain and query incidentally collected U.S. person data without individualized warrants, and the FBI would lose the categorical prohibition on domestic-crime queries.
% FOUNDING_PROBLEM: How to authorize necessary foreign intelligence surveillance directed at non-U.S. persons abroad without creating a general warrantless domestic surveillance apparatus that captures U.S. person communications.
% FOUNDING_PROBLEM_CORROBORATION: The Privacy and Civil Liberties Oversight Board and civil liberties organizations attest that the threat of domestic overreach persists and that compliance violations demonstrate drift. The intelligence community and DOJ attest the foreign-target framework remains necessary for national security. Congressional reauthorization debates and declassified FISC opinions documenting non-compliance corroborate the tension from outside the pure beneficiary set.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is low (0.15 at interval end) because the strict reading structurally excludes U.S. persons from the victim set; the constraint operates as a privacy shield rather than an extraction mechanism. Suppression is moderate-low (0.25) because the statute is enforced through legal compliance and FISC oversight rather than coercion against rights-holders. Theater ratio is low (0.12) because the statutory language has a direct operational function in targeting certifications and minimization procedures, with limited performative maintenance. Accessibility collapse is low (0.20) because individualized warrants and alternative collection authorities remain accessible if the foreign-target channel were removed. Resistance is moderate (0.35) because the intelligence community and some legislators consistently press for broader retention and query authority.
 *
 * PERSPECTIVAL GAP:
 *   The us_persons seat should compute as a net beneficiary: the constraint subsidizes their privacy position by imposing targeting and warrant costs on the government. The intelligence_community and fbi_domestic_intelligence seats should compute as payers: they experience the constraint as a compliance burden that limits operational scope. The divergence is structural, not perspectival in a subjective sense â the same legal text governs both seats, but its directionalities are opposite.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress sets the agenda and derives no direct extraction from the constraint. The intelligence community and FBI are structural targets (d toward 1.0) because the constraint actively extracts operational flexibility from them through targeting limitations and query prohibitions. U.S. persons are structural beneficiaries (d toward 0.0) because the constraint subsidizes their privacy by deleting incidental data and requiring warrants. Privacy advocates are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve the problem of foreign intelligence collection without domestic overreach. Its mandate is contested â evidenced by repeated reauthorization fights and documented compliance violations â but under the strict reading it has not atrophied into a domestic surveillance tool. The metric profile (low theater, low extraction) supports the conclusion that it is still functioning as designed rather than persisting as a piton. Should the theater ratio and extractiveness rise significantly while the sunset/reevaluation mechanisms fail, it would drift toward tangled_rope or snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the FISA 702 statutory text correctly read as a strict foreign-target wall with deletion minimization, or does the same text permit the retention and query of incidentally collected U.S. person data?',
    'Judicial consensus from the FISC and appellate courts, or legislative amendment that clarifies minimization as deletion and domestic-query prohibition.',
    'If resolved toward the strict reading, the constraint remains a low-extraction rope protecting U.S. persons. If resolved toward incidental collection, U.S. persons shift from beneficiaries to victims and the constraint reclassifies toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between strict and incidental readings of the same statutory kernel.').

omega_variable(
    minimization_deletion_vs_access_restriction,
    'Does the strict reading''s minimization requirement functionally require physical deletion of incidental U.S. person data, or does access restriction with audit logging satisfy the statutory standard?',
    'FISC compliance opinions or legislative text explicitly defining minimization as deletion.',
    'If access restriction suffices, effective extraction on U.S. persons rises because data persists and remains queryable under narrower conditions, shifting the beneficiary/victim boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_deletion_vs_access_restriction, empirical, 'Whether minimization means deletion or access control.').

omega_variable(
    foreign_target_boundary_leakage,
    'Is the foreign-target boundary stable in practice given globally distributed communications infrastructure and multi-hop collection?',
    'Independent technical audit of targeting procedures, geolocation accuracy, and upstream collection scope.',
    'If the boundary systematically leaks U.S. person data into foreign-target collection, the effective victim set expands despite the statutory text, raising extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_boundary_leakage, empirical, 'Empirical stability of the foreign-target boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fisa_tr_t2, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2, 0.06).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 4, 0.06).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 6, 0.07).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(fisa_tr_t14, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 14, 0.11).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 16, 0.12).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fisa_be_t2, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2, 0.09).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 4, 0.1).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 6, 0.1).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(fisa_be_t14, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 14, 0.14).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 16, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fisa_su_t2, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2, 0.21).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 6, 0.23).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement(fisa_su_t14, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 14, 0.27).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 16, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
