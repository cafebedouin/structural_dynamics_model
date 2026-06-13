% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Race-Conscious Substantive Equality)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which holds that race-conscious measures are not merely
 *   permissible but sometimes required to address historical group
 *   subordination and achieve substantive equality. It is a Scaffold because
 *   it is intended to be temporary, with a sunset clause tied to the
 *   achievement of its remedial goals. The constraint is actively enforced by
 *   courts and implemented by institutions, leading to identifiable
 *   beneficiaries (historically subordinated groups) and payers (individuals
 *   from non-preferred groups who may be disadvantaged by remedial policies).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.45).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection: Remedial Reading (Race-Conscious Substantive Equality)").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '6f73bba0-cf8b-4a32-a9d1-b70762345d71').
narrative_ontology:cs_kernel_codification('6f73bba0-cf8b-4a32-a9d1-b70762345d71', fixed_text).
narrative_ontology:cs_authority_grounding('6f73bba0-cf8b-4a32-a9d1-b70762345d71', lineage).
narrative_ontology:cs_interpretation_layer_present('6f73bba0-cf8b-4a32-a9d1-b70762345d71').
narrative_ontology:cs_reading_relation('6f73bba0-cf8b-4a32-a9d1-b70762345d71', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('6f73bba0-cf8b-4a32-a9d1-b70762345d71', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('6f73bba0-cf8b-4a32-a9d1-b70762345d71', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('6f73bba0-cf8b-4a32-a9d1-b70762345d71', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('6f73bba0-cf8b-4a32-a9d1-b70762345d71', foundational, race_conscious_remedies_necessary).
narrative_ontology:cs_axiom_status(race_conscious_remedies_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6f73bba0-cf8b-4a32-a9d1-b70762345d71', race_conscious_remedies_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('6f73bba0-cf8b-4a32-a9d1-b70762345d71', post_civil_rights_act_remedial_jurisprudence).
narrative_ontology:cs_drift_state('6f73bba0-cf8b-4a32-a9d1-b70762345d71', contemporary_supreme_court_shift, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6f73bba0-cf8b-4a32-a9d1-b70762345d71', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause to mandate race-conscious measures to remedy the present effects of past discrimination, aiming for substantive equality. This interpretation requires active judicial oversight and enforcement of remedial policies, with a declared sunset when the effects of discrimination are overcome.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, supreme_court_majority_remedial_reading, agenda_setter,
    institutional, generational, constrained, national).

% Are the intended beneficiaries of race-conscious remedial policies, designed to overcome systemic disadvantages and achieve substantive equality. Their access to opportunities and resources is enhanced by these policies, but the benefits are contingent on the legal and political viability of the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% May experience direct costs (e.g., denial of admission, employment, or contracts) due to race-conscious remedial policies, even if they bear no personal responsibility for past discrimination. Their claims are often framed as individual rights violations, creating legal challenges to the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups, payer,
    moderate, biographical, constrained, local).

% Are tasked with designing and implementing race-conscious remedial programs in areas like admissions, scholarships, and faculty hiring. They navigate complex legal requirements and public scrutiny, balancing the mandate for remediation with concerns about reverse discrimination. Their operational autonomy is constrained by judicial interpretations.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Actively champion and defend the remedial reading, viewing it as essential for achieving racial justice and dismantling systemic inequality. They provide legal and political support for race-conscious policies and challenge efforts to dismantle them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue that any racial classification, even for remedial purposes, violates the Equal Protection Clause by treating individuals differently based on race. They advocate for a strictly colorblind interpretation and actively litigate against race-conscious policies, seeking their abolition.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates governmental and institutional efforts to address and remedy the ongoing effects of historical racial discrimination, ensuring that policies are aligned with the goal of achieving substantive equality for historically subordinated groups.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from individual members of non-preferred groups (who may be denied access in favor of beneficiaries) to members of historically subordinated racial groups, as a means of rectifying past and present systemic inequalities.
% ABSENT_VOICES: The 'colorblind' advocates, who argue that any racial classification is inherently discriminatory, are structurally excluded from the premise of this reading, which views race-conscious measures as necessary for equality. Their arguments are treated as fundamentally opposed to the remedial goal.
% DISAPPEARANCE_RATIONALE: If this remedial reading of the Equal Protection Clause vanished, all race-conscious policies aimed at substantive equality would immediately become unconstitutional. Institutions would cease such programs, and the legal landscape for addressing historical discrimination would fundamentally shift, likely exacerbating existing inequalities.
% FOUNDING_PROBLEM: The Equal Protection Clause was adopted to ensure legal equality for formerly enslaved people, but its application has been contested regarding whether it mandates or forbids race-conscious measures to overcome the persistent effects of historical group subordination.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars (outside the immediate beneficiaries of remedial policies) corroborate that the problem of racial inequality persists, and that the debate over the Equal Protection Clause's role in addressing it remains central to American constitutional law. Social science data on disparities also corroborates the ongoing problem.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) reflects the significant re-allocation of opportunities and resources mandated by remedial policies, impacting individual members of non-preferred groups. Suppression (0.45) is moderate, as there is active resistance and legal challenge to these policies, but institutions are compelled to implement them. Theater ratio (0.15) is low, indicating that the policies are genuinely aimed at their stated remedial goals, though their effectiveness and necessity are highly contested. Accessibility collapse (0.3) is low because alternative legal interpretations and policy approaches are actively pursued. Resistance (0.75) is high, reflecting ongoing legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and civil rights advocates, this is a necessary, albeit temporary, mechanism for justice. From the perspective of individual members of non-preferred groups and colorblind advocates, it is an unjust, discriminatory imposition. The engine's per-seat classification will reflect these divergent experiences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups are clear beneficiaries (d=0.0-0.2), as the constraint aims to improve their societal position. Individual members of non-preferred groups are targets (d=0.8-1.0), as they bear the direct costs of remedial policies. The Supreme Court majority (remedial reading) and civil rights advocates are beneficiaries (d=0.0-0.2) of this interpretation's persistence. Educational institutions are agenda-setters (d=0.4-0.6), implementing the policies but also facing resistance. Colorblind advocates are excluded (d=1.0), as their core premise is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_condition_ambiguity,
    'When are the ''effects of past discrimination'' sufficiently remedied to trigger the sunset clause?',
    'Empirical metrics of racial disparities (e.g., educational attainment, wealth, health outcomes) reaching parity, or a judicial declaration based on a comprehensive societal assessment.',
    'If the sunset condition is perpetually deferred, the Scaffold could drift into a Tangled Rope or Snare, as the ''temporary'' justification becomes a permanent mechanism for resource allocation. If prematurely triggered, it could leave substantive inequalities unaddressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_condition_ambiguity, conceptual, 'Ambiguity in defining the conditions for the constraint''s termination.').

omega_variable(
    individual_vs_group_rights,
    'Does the Equal Protection Clause primarily protect individual rights against racial discrimination, or does it permit/require group-conscious remedies to achieve group-level equality?',
    'A definitive Supreme Court ruling that explicitly resolves the tension between individual and group-based conceptions of equality under the Equal Protection Clause.',
    'If individual rights are prioritized absolutely, the remedial reading would be foreclosed, and race-conscious policies would be deemed unconstitutional. If group-level remedies are affirmed as constitutional, the remedial reading would be strengthened, potentially at the expense of individual claims of reverse discrimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_group_rights, conceptual, 'Fundamental conceptual disagreement over the nature of equality protected by the Equal Protection Clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1960, equal_protection_clause__remedial_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(equa_tr_t1980, equal_protection_clause__remedial_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_clause__remedial_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_clause__remedial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1960, equal_protection_clause__remedial_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(equa_be_t1980, equal_protection_clause__remedial_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(equa_be_t2000, equal_protection_clause__remedial_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(equa_be_t2024, equal_protection_clause__remedial_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1960, equal_protection_clause__remedial_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(equa_su_t1980, equal_protection_clause__remedial_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(equa_su_t2000, equal_protection_clause__remedial_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(equa_su_t2024, equal_protection_clause__remedial_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This is one of three primary readings of the Equal Protection Clause kernel, each representing a distinct constraint. This remedial reading directly influences and is influenced by the colorblind and diversity readings through ongoing legal and political contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
