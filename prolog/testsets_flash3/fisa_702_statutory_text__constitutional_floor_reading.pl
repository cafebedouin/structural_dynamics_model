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
    narrative_ontology:constraint_vindicates/2,
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
 *   query of U.S. person communications content constitutes a search
 *   requiring a probable cause warrant. This reframes 702 queries from a
 *   foreign intelligence statute issue to a criminal procedure question,
 *   demanding individualized judicial review by the FISA Court. The
 *   extractiveness (0.25) reflects the 'cost' to executive agencies in terms
 *   of reduced operational flexibility and increased compliance burdens,
 *   rather than extraction from individuals, as the constraint aims to
 *   protect individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.1).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.1).
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
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f').
narrative_ontology:cs_kernel_codification('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', fixed_text).
narrative_ontology:cs_authority_grounding('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', lineage).
narrative_ontology:cs_interpretation_layer_present('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f').
narrative_ontology:cs_reading_relation('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', foundational, fourth_amendment_warrant_requirement_absolute).
narrative_ontology:cs_axiom_status(fourth_amendment_warrant_requirement_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', fourth_amendment_warrant_requirement_absolute, deontological).
narrative_ontology:cs_axiom('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', foundational, us_person_communications_content_is_private).
narrative_ontology:cs_axiom_status(us_person_communications_content_is_private, holdable).
narrative_ontology:cs_axiom_grounding('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', us_person_communications_content_is_private, deontological).
narrative_ontology:cs_reference_frame('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', fourth_amendment_original_intent).
narrative_ontology:cs_drift_state('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', contemporary_surveillance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e24642aa-2ac5-4fb9-97cc-4c7b1ec0a32f', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_agencies).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individual_privacy_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As subjects of government surveillance, U.S. persons benefit from the protection of a probable cause warrant requirement, ensuring their communications content is not searched without judicial oversight. Their ability to exit surveillance is non-existent.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, generational, trapped, national).

% These agencies (e.g., NSA, FBI) bear the cost of increased procedural hurdles and reduced operational speed/secrecy due to the warrant requirement. They prefer the flexibility of warrantless queries for foreign intelligence purposes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_agencies, payer,
    institutional, biographical, constrained, global).

% The FISA Court would be responsible for conducting individualized probable cause review for 702 queries, shifting its role from programmatic oversight to specific warrant adjudication. This increases its workload and judicial scrutiny.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Congress oversees FISA and the Fourth Amendment. This reading would compel legislative action to align statutory language with constitutional requirements, potentially leading to significant reforms or reauthorization debates.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).

% These groups advocate for stronger privacy protections and constitutional compliance in surveillance. This reading vindicates their core arguments and strengthens their position in policy debates.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that government surveillance of U.S. person communications content adheres to the constitutional standard of probable cause, coordinating the balance between national security and individual privacy rights through judicial review.
% TRANSFER_FUNCTION: Transfers a degree of operational flexibility and speed from executive branch agencies to the judiciary, in exchange for enhanced constitutional compliance and protection of individual privacy.
% ABSENT_VOICES: The voices of foreign intelligence targets are absent, as this reading primarily focuses on U.S. person protections. Their perspective on the scope and impact of surveillance is not directly addressed by this specific constraint.
% DISAPPEARANCE_RATIONALE: If this constitutional interpretation vanished, executive agencies would likely revert to broader warrantless surveillance practices, leading to a significant erosion of Fourth Amendment protections for U.S. persons and a fundamental shift in the balance of power between branches of government regarding surveillance.
% FOUNDING_PROBLEM: The Fourth Amendment was established to prevent arbitrary government searches and seizures, ensuring that individuals are secure in their persons, houses, papers, and effects against unreasonable searches and seizures.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of balancing government power with individual rights remains live, as evidenced by ongoing debates, litigation, and legislative efforts concerning surveillance. Civil liberties organizations, legal scholars, and dissenting judicial opinions consistently corroborate the persistence of this fundamental tension.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low because this reading primarily aims to protect individuals from unwarranted government intrusion, rather than extract from them. The 'cost' is borne by the executive in terms of procedural overhead. Suppression is low (0.1) because the constraint is a constitutional limit, not an actively enforced mechanism to suppress alternatives for individuals. Resistance is high (0.7) from executive agencies who prefer less stringent oversight. Accessibility collapse is high (0.85) because the Fourth Amendment is a foundational limit on government power, making alternatives to its requirements largely inaccessible.
 *
 * PERSPECTIVAL GAP:
 *   Executive branch agencies would experience this as a significant constraint on their operational capabilities, viewing it as an impediment to national security. U.S. persons and civil liberties advocates would view it as a necessary protection and a vindication of fundamental rights. The FISA Court would see its role fundamentally altered, with increased judicial responsibility.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are the primary beneficiaries, as the constraint directly protects their constitutional rights. Executive agencies are the payers, bearing the costs of compliance and reduced operational freedom. The FISA Court acts as an agenda-setter, adjudicating the warrant requirements. Civil liberties advocates are also beneficiaries, as their advocacy aligns with this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of executive operational preferences as a 'natural' or 'necessary' aspect of national security. By asserting a constitutional floor, it challenges the notion that warrantless surveillance is an inherent feature of foreign intelligence gathering, thereby resisting mandatrophy where the original mandate (national security) is used to justify an overreach of power (warrantless domestic searches).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_statutory_authority,
    'Is the Fourth Amendment''s warrant requirement a non-negotiable constitutional floor for U.S. person communications content, or can statutory frameworks like FISA Section 702 provide alternative, constitutionally permissible frameworks for surveillance?',
    'Supreme Court ruling on the constitutionality of Section 702''s U.S. person query practices, or a constitutional amendment clarifying surveillance powers.',
    'If the Supreme Court upholds this reading, it would fundamentally alter surveillance practices. If it defers to statutory frameworks, the current system of warrantless queries would largely persist, albeit with potential statutory modifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_statutory_authority, conceptual, 'Ambiguity regarding the supremacy of constitutional warrant requirements over statutory foreign intelligence authorizations.').

omega_variable(
    operational_impact_of_warrants,
    'What would be the actual operational impact (e.g., delays, missed intelligence, resource burden) on national security agencies if a probable cause warrant were required for every U.S. person communications content query under Section 702?',
    'Empirical study by an independent oversight body (e.g., GAO, PCLOB) assessing the practical implications of implementing a warrant requirement, including case studies and resource analysis.',
    'If the operational impact is demonstrably severe, it could lead to political pressure to find alternative constitutional interpretations or legislative solutions. If the impact is manageable, it strengthens the argument for constitutional compliance without undue burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_impact_of_warrants, empirical, 'Uncertainty about the practical feasibility and consequences of a warrant requirement for 702 queries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2016, 0.23).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2020, 0.24).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.08).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2012, 0.09).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2016, 0.09).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the FISA Section 702 statutory text kernel, asserting a constitutional floor for U.S. person communications content searches. It directly challenges the legitimacy of the 'incidental collection' and 'foreign target strict' readings by asserting a higher constitutional standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
