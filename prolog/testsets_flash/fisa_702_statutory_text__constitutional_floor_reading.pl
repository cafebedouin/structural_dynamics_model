% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: FISA Section 702: Constitutional Floor Reading (Warrant Requirement for US Person Queries)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a specific reading of FISA Section 702,
 *   asserting that the Fourth Amendment's probable cause warrant requirement
 *   applies to any government search of U.S. person communications content,
 *   regardless of how that content was initially collected under 702
 *   authority. This interpretation reframes 702 queries as domestic searches,
 *   triggering a constitutional floor that supersedes statutory
 *   interpretations that might permit warrantless access. The extractiveness
 *   (0.25) reflects the operational friction and speed/secrecy preferences of
 *   the executive branch, which would be 'extracted' by this constitutional
 *   requirement. Suppression (0.15) is low because this reading aims to
 *   reduce, not increase, government suppressive power over individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.15).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "FISA Section 702: Constitutional Floor Reading (Warrant Requirement for US Person Queries)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'b8b0f761-fafd-4621-802c-07477612db07').
narrative_ontology:cs_kernel_codification('b8b0f761-fafd-4621-802c-07477612db07', fixed_text).
narrative_ontology:cs_authority_grounding('b8b0f761-fafd-4621-802c-07477612db07', lineage).
narrative_ontology:cs_interpretation_layer_present('b8b0f761-fafd-4621-802c-07477612db07').
narrative_ontology:cs_reading_relation('b8b0f761-fafd-4621-802c-07477612db07', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('b8b0f761-fafd-4621-802c-07477612db07', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('b8b0f761-fafd-4621-802c-07477612db07', foundational, fourth_amendment_applies_to_us_person_searches).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_us_person_searches, holdable).
narrative_ontology:cs_axiom_grounding('b8b0f761-fafd-4621-802c-07477612db07', fourth_amendment_applies_to_us_person_searches, deontological).
narrative_ontology:cs_axiom('b8b0f761-fafd-4621-802c-07477612db07', foundational, warrant_required_for_content_searches).
narrative_ontology:cs_axiom_status(warrant_required_for_content_searches, holdable).
narrative_ontology:cs_axiom_grounding('b8b0f761-fafd-4621-802c-07477612db07', warrant_required_for_content_searches, deontological).
narrative_ontology:cs_reference_frame('b8b0f761-fafd-4621-802c-07477612db07', fourth_amendment_warrant_framework).
narrative_ontology:cs_drift_state('b8b0f761-fafd-4621-802c-07477612db07', contemporary_fisa_702_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8b0f761-fafd-4621-802c-07477612db07', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_rights).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the government's legitimate foreign intelligence needs with the constitutional rights of U.S. persons by requiring judicial review (warrants) for queries of U.S. person data, ensuring both national security and civil liberties are considered.
% TRANSFER_FUNCTION: Transfers a degree of operational flexibility and speed from intelligence agencies to U.S. persons, who gain enhanced privacy and Fourth Amendment protections. It also transfers oversight authority to the FISA Court for individualized probable cause review.
% ABSENT_VOICES: The voices of U.S. persons whose communications are incidentally collected and queried without warrants are often absent from the initial authorization process. This reading aims to give those voices a constitutional mechanism for protection.
% DISAPPEARANCE_RATIONALE: If this constitutional floor disappeared, intelligence agencies would likely revert to broader warrantless access to U.S. person data under 702, leading to a significant erosion of Fourth Amendment protections and a reorganization of surveillance practices.
% FOUNDING_PROBLEM: The original problem was balancing national security needs with individual privacy rights in the context of foreign intelligence surveillance, particularly concerning the incidental collection of U.S. person data.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, constitutional scholars, and dissenting judges consistently attest that the founding problem of balancing surveillance with rights remains live and unresolved under current statutory interpretations. Their analysis, independent of intelligence agencies, corroborates the ongoing nature of this tension.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness and suppression reflect the nature of a 'constitutional floor' reading: it aims to limit government power, not to create an extractive mechanism. The 'rope' classification is chosen because it coordinates the government's intelligence needs with individual constitutional rights, albeit with a cost to executive efficiency. The resistance is high (0.8) because this reading is actively contested by intelligence agencies and the executive branch, who view it as an undue burden on national security operations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of US persons and civil liberties advocates, this constraint is a necessary protection (beneficiary seat). From the perspective of intelligence agencies and the executive branch, it is an impediment to effective foreign intelligence collection (victim seat). The FISA Court, as an agenda-setter, would experience it as a shift in its oversight responsibilities.
 *
 * DIRECTIONALITY LOGIC:
 *   US persons are full beneficiaries (d=0.0) as their Fourth Amendment rights are protected. Intelligence agencies and the executive branch are targets (d=1.0) as their operational flexibility is curtailed. Civil liberties advocates are also beneficiaries, while the FISA Court's role shifts to a more robust oversight function, making it a moderate beneficiary of this reading's clarity.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by ensuring that the original constitutional mandate (Fourth Amendment protections) remains 'live' and applicable even as surveillance technology and statutory frameworks evolve. It directly addresses the risk of the 702 statute's foreign intelligence mandate being used as cover for domestic surveillance without adequate constitutional safeguards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine constitutional floor, or a policy preference framed as constitutional necessity?',
    'Supreme Court ruling explicitly adopting this interpretation, or a constitutional amendment.',
    'If a genuine constitutional floor, it would be a Mountain; if a policy preference, it would be a Rope or Tangled Rope depending on enforcement and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''constitutional_floor_reading'' of the ''fisa_702_statutory_text'' kernel. Sibling readings (''incidental_collection_reading'', ''foreign_target_strict_reading'') would treat 702 queries as less constrained by the Fourth Amendment, leading to higher extractiveness and lower suppression for intelligence agencies.').

omega_variable(
    executive_branch_compliance_cost,
    'What is the actual cost (in terms of intelligence collection efficiency and national security risk) of implementing a probable cause warrant requirement for US person queries of 702 data?',
    'Declassified intelligence community impact assessments, or independent expert analysis of operational changes in jurisdictions with similar requirements.',
    'If costs are genuinely prohibitive, it strengthens the ''extraction'' argument for the executive branch''s resistance; if costs are manageable, it weakens the argument against this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_branch_compliance_cost, empirical, 'Quantifying the operational impact of a warrant requirement on intelligence operations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(fisa_tr_t30, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(fisa_be_t30, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(fisa_su_t30, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the FISA Section 702 statutory text kernel, each representing a distinct interpretation of its constitutional and statutory limits. This reading emphasizes Fourth Amendment protections for US persons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
