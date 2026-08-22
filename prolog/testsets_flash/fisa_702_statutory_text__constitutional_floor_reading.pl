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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Probable Cause Warrant for 702 Queries (Constitutional Floor Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a 'constitutional floor' reading of the Fourth
 *   Amendment as applied to Section 702 of FISA. It asserts that regardless
 *   of statutory language or foreign intelligence purpose, any government
 *   search of U.S. person communications content requires a probable cause
 *   warrant. This reading reframes 702 queries from a foreign intelligence
 *   statute into a criminal procedure question, emphasizing individual rights
 *   over executive operational efficiency. The claimed type is 'mountain'
 *   because it posits an unchangeable constitutional requirement, even though
 *   its application to 702 is contested.
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
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Probable Cause Warrant for 702 Queries (Constitutional Floor Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).
domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'aebf89bd-5bfc-4406-ac5a-f7cdde49a02e').
narrative_ontology:cs_kernel_codification('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', fixed_text).
narrative_ontology:cs_authority_grounding('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', lineage).
narrative_ontology:cs_interpretation_layer_present('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e').
narrative_ontology:cs_reading_relation('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', foundational, fourth_amendment_warrant_requirement_absolute).
narrative_ontology:cs_axiom_status(fourth_amendment_warrant_requirement_absolute, holdable).
narrative_ontology:cs_axiom_grounding('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', fourth_amendment_warrant_requirement_absolute, deontological).
narrative_ontology:cs_axiom('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', foundational, us_person_communications_content_protected).
narrative_ontology:cs_axiom_status(us_person_communications_content_protected, holdable).
narrative_ontology:cs_axiom_grounding('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', us_person_communications_content_protected, deontological).
narrative_ontology:cs_reference_frame('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', original_fourth_amendment_intent).
narrative_ontology:cs_drift_state('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', contemporary_surveillance_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('aebf89bd-5bfc-4406-ac5a-f7cdde49a02e', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons_privacy_advocates).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_intelligence_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the robust application of Fourth Amendment protections to all government surveillance activities, particularly those affecting U.S. persons. They benefit from a reading that mandates warrants for 702 queries, seeing it as a necessary check on executive power.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons_privacy_advocates, beneficiary,
    organized, generational, analytical, national).

% Work to ensure government surveillance adheres to constitutional limits. They view this reading as essential for protecting fundamental rights against overreach, aligning with their mission to defend individual freedoms.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, beneficiary,
    organized, generational, analytical, national).

% Bear the costs of this reading through increased operational friction, delays in intelligence gathering, and the administrative burden of obtaining individualized warrants for queries of U.S. person data. They argue it impedes their ability to protect national security.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Would be responsible for reviewing and approving probable cause warrants for 702 queries under this reading. Their workload and oversight responsibilities would significantly increase, shifting their role from programmatic review to individualized judicial scrutiny.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Are the ultimate beneficiaries of this reading, as it would provide them with greater constitutional protection against warrantless government surveillance of their communications content. Their ability to influence the constraint is diffuse and indirect.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between national security interests and individual privacy rights by establishing a clear judicial check (warrant requirement) on government surveillance of U.S. persons' communications content.
% TRANSFER_FUNCTION: Transfers a degree of operational flexibility and speed from executive intelligence agencies to the judiciary, in exchange for enhanced privacy protections for U.S. persons.
% ABSENT_VOICES: The general public, often unaware of the specifics of surveillance programs, would likely support stronger privacy protections if fully informed of the scope of warrantless searches. Their absence from direct policy debates allows for less restrictive interpretations to persist.
% DISAPPEARANCE_RATIONALE: If this constitutional floor vanished, executive intelligence agencies would likely expand warrantless access to U.S. person communications, leading to a significant erosion of privacy rights and a shift in the balance of power towards the executive branch. The legal and operational landscape of surveillance would fundamentally reorganize.
% FOUNDING_PROBLEM: The Fourth Amendment was established to prevent arbitrary government searches and seizures, ensuring that individuals' privacy and security are protected against unchecked state power.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties advocates, and historical analyses consistently corroborate the founding problem's live status, citing ongoing tensions between government surveillance capabilities and individual rights. Whistleblower disclosures and court challenges further attest to the persistent nature of this problem.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) reflects the 'cost' to executive agencies in terms of speed and secrecy, as they would need to obtain warrants. This is a low extraction because constitutional compliance is not 'extraction' in the typical sense, but rather a necessary cost of a free society. Suppression (0.15) is low because the constraint is a constitutional limit, not an actively enforced mechanism against resistance. Theater ratio (0.05) is minimal as the constitutional requirement is fundamental. Accessibility collapse (0.85) is high because, from this reading's perspective, there are no legitimate alternatives to the warrant requirement for U.S. person searches. Resistance (0.7) is high because executive agencies actively resist this interpretation, preferring broader authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intelligence agencies, this reading imposes an undue burden on national security operations. From the perspective of civil liberties advocates, it is a fundamental safeguard. The engine's classification will highlight this divergence, showing a 'mountain' from the perspective of rights-holders, but a 'snare' or 'tangled_rope' from the perspective of agencies whose operational flexibility is curtailed.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons, privacy advocates, and civil liberties organizations are beneficiaries, as this reading enhances their constitutional protections. Executive intelligence agencies are payers, as they bear the operational costs of compliance. The FISA Court acts as an agenda-setter, as it would be the body responsible for enforcing this warrant requirement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by reasserting the original constitutional mandate of the Fourth Amendment against evolving surveillance technologies. It argues that the core problem of unchecked government power remains live, and the constitutional solution (warrants) must adapt to new forms of search. It challenges the idea that national security exigencies automatically override fundamental rights, thus preventing the constitutional floor from becoming a 'piton' of performative compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_statutory_authority,
    'Is the Fourth Amendment''s warrant requirement an absolute constitutional floor for U.S. person communications content, or can statutory frameworks like FISA Section 702 create exceptions based on foreign intelligence purpose?',
    'Supreme Court ruling directly addressing the constitutionality of warrantless 702 queries of U.S. person data, or a constitutional amendment clarifying surveillance powers.',
    'If the constitutional floor is affirmed, this reading''s classification as a ''mountain'' is strengthened. If statutory exceptions are upheld, the constraint would be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of U.S. persons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_statutory_authority, conceptual, 'Ambiguity regarding the supremacy of constitutional rights over statutory national security frameworks.').

omega_variable(
    operational_cost_vs_constitutional_value,
    'What is the true operational cost (in terms of intelligence gaps or delays) of implementing a probable cause warrant requirement for 702 queries, and how does this cost weigh against the constitutional value of privacy?',
    'Independent, transparent analysis by a non-partisan body (e.g., GAO, CBO) of the impact of warrant requirements on intelligence operations, coupled with public debate on the societal value of privacy.',
    'If costs are demonstrably low, it strengthens the argument for this reading. If costs are severe and demonstrably tied to national security failures, it could lead to public pressure for a less restrictive interpretation, potentially shifting the constraint towards a ''tangled_rope'' or ''snare'' from the perspective of national security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_cost_vs_constitutional_value, empirical, 'Balancing operational efficiency with constitutional rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t1978, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(fisa_tr_t1990, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(fisa_tr_t2001, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2001, 0.05).
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(fisa_tr_t2013, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t1978, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 1978, 0.25).
narrative_ontology:measurement(fisa_be_t1990, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(fisa_be_t2001, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2001, 0.25).
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.25).
narrative_ontology:measurement(fisa_be_t2013, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2013, 0.25).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t1978, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(fisa_su_t1990, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(fisa_su_t2001, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2001, 0.15).
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.15).
narrative_ontology:measurement(fisa_su_t2013, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2013, 0.15).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the FISA Section 702 statutory text kernel. This 'constitutional_floor_reading' asserts a warrant requirement for U.S. person queries, directly influencing the interpretation and operational scope of the 'incidental_collection_reading' and 'foreign_target_strict_reading' by setting a higher constitutional bar.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
