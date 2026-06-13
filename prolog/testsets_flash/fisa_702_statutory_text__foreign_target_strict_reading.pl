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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 Foreign Target Strict Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a 'foreign target strict reading' of FISA
 *   Section 702, which limits surveillance to non-U.S. persons abroad and
 *   mandates minimization of incidentally collected U.S. person data. Under
 *   this reading, U.S. persons are not targets, and their data is protected
 *   from domestic use without a warrant. This interpretation aims to uphold
 *   Fourth Amendment protections while allowing foreign intelligence
 *   collection. The low extractiveness and suppression reflect the intent of
 *   this strict reading, where U.S. persons are largely outside the
 *   constraint's extractive scope.
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
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 Foreign Target Strict Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '1b5f0bc7-e247-425d-b586-1cbae461060b').
narrative_ontology:cs_kernel_codification('1b5f0bc7-e247-425d-b586-1cbae461060b', fixed_text).
narrative_ontology:cs_authority_grounding('1b5f0bc7-e247-425d-b586-1cbae461060b', lineage).
narrative_ontology:cs_interpretation_layer_present('1b5f0bc7-e247-425d-b586-1cbae461060b').
narrative_ontology:cs_reading_relation('1b5f0bc7-e247-425d-b586-1cbae461060b', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('1b5f0bc7-e247-425d-b586-1cbae461060b', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('1b5f0bc7-e247-425d-b586-1cbae461060b', foundational, foreign_target_principle).
narrative_ontology:cs_axiom_status(foreign_target_principle, holdable).
narrative_ontology:cs_axiom_grounding('1b5f0bc7-e247-425d-b586-1cbae461060b', foreign_target_principle, conventional).
narrative_ontology:cs_axiom('1b5f0bc7-e247-425d-b586-1cbae461060b', foundational, minimization_as_deletion_or_inaccessibility).
narrative_ontology:cs_axiom_status(minimization_as_deletion_or_inaccessibility, holdable).
narrative_ontology:cs_axiom_grounding('1b5f0bc7-e247-425d-b586-1cbae461060b', minimization_as_deletion_or_inaccessibility, conventional).
narrative_ontology:cs_reference_frame('1b5f0bc7-e247-425d-b586-1cbae461060b', statutory_text_as_limiting_principle).
narrative_ontology:cs_drift_state('1b5f0bc7-e247-425d-b586-1cbae461060b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1b5f0bc7-e247-425d-b586-1cbae461060b', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_abroad).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_domestic).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_protections).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, privacy_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the FISA Section 702 program, targeting non-U.S. persons abroad for foreign intelligence purposes. Under this strict reading, they are constrained by minimization requirements for U.S. person data and prohibitions on domestic use without a warrant.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from intelligence collection that protects national security, while being protected from being targeted by the program. Their communications are not the primary investigative interest, and incidental data is minimized.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_abroad, beneficiary,
    moderate, biographical, mobile, global).

% Are protected by the strict interpretation that prohibits the use of incidentally collected data for domestic law enforcement without a warrant. Their Fourth Amendment rights are upheld under this reading.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_domestic, beneficiary,
    organized, generational, mobile, national).

% Enacts and reauthorizes FISA Section 702, defining its statutory limits. This strict reading reflects a particular legislative intent to balance national security with civil liberties.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret the statutory language and review the legality of the 702 program. This strict reading is one possible judicial interpretation that emphasizes constitutional protections.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, federal_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates foreign intelligence collection against non-U.S. persons abroad to protect national security, while simultaneously coordinating the protection of U.S. persons' Fourth Amendment rights by limiting the scope and use of collected data.
% TRANSFER_FUNCTION: Transfers foreign intelligence information to national security agencies, while transferring privacy protections to U.S. persons by restricting the collection and use of their data.
% ABSENT_VOICES: Under this strict reading, there are no 'absent voices' among U.S. persons, as their rights are explicitly protected. However, proponents of broader surveillance powers might argue that the strictures impede necessary intelligence collection.
% DISAPPEARANCE_RATIONALE: If this strict reading of FISA 702 disappeared, national security agencies would likely expand collection and use of U.S. person data, leading to significant civil liberties concerns and a reorganization of surveillance practices and legal challenges.
% FOUNDING_PROBLEM: The founding problem was the need to collect foreign intelligence from non-U.S. persons abroad in the digital age, without requiring individualized warrants for every communication, while simultaneously safeguarding the Fourth Amendment rights of U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: National security agencies attest the problem is live due to ongoing foreign threats. Civil liberties advocates and some members of Congress corroborate the need for robust protections for U.S. persons, supporting the 'live' status of the balancing act.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).

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
 *   Extractiveness is low (0.15) because, under this strict reading, the constraint is designed to avoid targeting U.S. persons and to minimize any incidental collection, thus limiting the burden on rights-holders. Suppression is also low (0.2) as the constraint's operation is intended to be narrowly tailored and not to coerce or restrict U.S. persons. Theater ratio is low (0.1) as the stated purpose of foreign intelligence collection is genuinely pursued, and minimization is taken seriously. The metrics reflect the ideal operation of this strict interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Under this strict reading, there is minimal perspectival gap for U.S. persons, as the constraint is designed to protect their rights. However, other readings (e.g., 'incidental collection reading') would introduce a significant gap, where U.S. persons would experience the constraint as extractive. The analytical observer's view aligns with this strict reading's intent.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies are beneficiaries (d near 0.0) as they gain foreign intelligence. U.S. persons abroad are also beneficiaries (d near 0.0) as the constraint protects them from being targeted while enabling intelligence that could protect them. Under this strict reading, there are no direct victims among U.S. persons, as their data is minimized and protected from domestic use without a warrant.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''foreign target strict reading'' of FISA Section 702, or is it an ''incidental collection reading'' or ''constitutional floor reading''?',
    'Judicial precedent from the Supreme Court or legislative amendment explicitly clarifying the scope of ''foreign target'' and ''minimization'' requirements.',
    'If reclassified as ''incidental collection reading'', extractiveness and suppression for U.S. persons would increase significantly. If reclassified as ''constitutional floor reading'', the entire 702 framework would be subject to individualized warrant requirements for U.S. person data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the interpretation of FISA Section 702 statutory text.').

omega_variable(
    minimization_effectiveness,
    'Is the minimization requirement (inaccessibility for domestic purposes) effectively implemented, or does it permit de facto domestic access?',
    'Independent audits of intelligence agency databases and query logs, with public reporting on compliance and access patterns.',
    'If minimization is found to be ineffective, the constraint''s actual extractiveness and suppression for U.S. persons would be higher than stated, potentially reclassifying it as a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_effectiveness, empirical, 'Effectiveness of minimization procedures for U.S. person data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the FISA Section 702 statutory text kernel, each representing a distinct interpretation of its scope and impact on U.S. persons. This 'foreign target strict reading' emphasizes minimization and non-targeting of U.S. persons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
