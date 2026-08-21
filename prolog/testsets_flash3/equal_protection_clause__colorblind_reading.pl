% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause (Colorblind Reading)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause, which holds that the government is forbidden from
 *   making any racial classifications, treating all individuals as
 *   rights-bearers independent of group membership. This reading asserts that
 *   the clause mandates formal equality and that any race-conscious policy,
 *   even if intended to be benign or remedial, is unconstitutional. It is
 *   presented as a Mountain due to its claim of being a foundational,
 *   immutable principle of constitutional law, with negligible extraction
 *   from individuals (who are all beneficiaries of non-discrimination) and
 *   minimal active enforcement beyond judicial review. The metrics reflect
 *   this claim of naturalness and low extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.05).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.1).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '8a94a409-37d3-4185-854d-2cb05bf38ef1').
narrative_ontology:cs_kernel_codification('8a94a409-37d3-4185-854d-2cb05bf38ef1', fixed_text).
narrative_ontology:cs_authority_grounding('8a94a409-37d3-4185-854d-2cb05bf38ef1', lineage).
narrative_ontology:cs_interpretation_layer_present('8a94a409-37d3-4185-854d-2cb05bf38ef1').
narrative_ontology:cs_reading_relation('8a94a409-37d3-4185-854d-2cb05bf38ef1', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('8a94a409-37d3-4185-854d-2cb05bf38ef1', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('8a94a409-37d3-4185-854d-2cb05bf38ef1', foundational, racial_classifications_are_inherently_suspect).
narrative_ontology:cs_axiom_status(racial_classifications_are_inherently_suspect, holdable).
narrative_ontology:cs_axiom_grounding('8a94a409-37d3-4185-854d-2cb05bf38ef1', racial_classifications_are_inherently_suspect, deontological).
narrative_ontology:cs_axiom('8a94a409-37d3-4185-854d-2cb05bf38ef1', foundational, individual_rights_are_paramount_over_group_remedies).
narrative_ontology:cs_axiom_status(individual_rights_are_paramount_over_group_remedies, holdable).
narrative_ontology:cs_axiom_grounding('8a94a409-37d3-4185-854d-2cb05bf38ef1', individual_rights_are_paramount_over_group_remedies, deontological).
narrative_ontology:cs_reference_frame('8a94a409-37d3-4185-854d-2cb05bf38ef1', post_civil_rights_era_formal_equality).
narrative_ontology:cs_drift_state('8a94a409-37d3-4185-854d-2cb05bf38ef1', contemporary_judicial_interpretations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8a94a409-37d3-4185-854d-2cb05bf38ef1', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_regardless_of_race).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, proponents_of_race_conscious_policies).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As rights-bearers, all individuals benefit from the guarantee that the government will not classify them by race, ensuring formal equality before the law. Their identity as citizens is paramount, making exit from this framework unthinkable.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals_regardless_of_race, beneficiary,
    powerless, generational, identity_locked, national).

% Governmental bodies (legislatures, agencies, courts) are constrained from enacting or enforcing any racial classifications, even those intended to be benign or remedial. They must operate under the principle of colorblindness, which limits their policy options.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, governmental_entities, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for policies that consider race (e.g., affirmative action, targeted remediation) find their efforts blocked or severely limited by this reading of the Equal Protection Clause. They bear the cost of being unable to implement their preferred policy solutions.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, proponents_of_race_conscious_policies, payer,
    organized, biographical, constrained, national).

% The ultimate arbiter of constitutional meaning, the Supreme Court interprets and applies the Equal Protection Clause. Justices aligned with this reading view it as a foundational principle of individual liberty and formal equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal standard for governmental non-discrimination, ensuring that all individuals are treated equally by the state regardless of race, thereby coordinating expectations for state action.
% TRANSFER_FUNCTION: Transfers the burden of non-discrimination onto governmental entities, requiring them to forgo race-conscious policies, and transfers the benefit of formal equality to all individuals.
% ABSENT_VOICES: Those who believe that substantive equality requires race-conscious policies are present in the public discourse but are structurally excluded from the interpretation of the clause that this reading represents. Their arguments for group-based remedies are deemed incompatible with the individual-rights focus.
% DISAPPEARANCE_RATIONALE: If this colorblind reading vanished, governmental entities would immediately face pressure to implement race-conscious policies, and the legal landscape regarding affirmative action, reparations, and targeted programs would fundamentally shift, leading to a complete reorganization of anti-discrimination law.
% FOUNDING_PROBLEM: The original Equal Protection Clause was enacted to ensure legal equality for newly freed slaves, preventing states from enacting discriminatory laws based on race.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and historical records corroborate that the founding problem of racial discrimination remains live, though its contemporary manifestations and appropriate remedies are fiercely debated. This reading asserts that the original intent was to forbid all racial distinctions.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_clause__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.1) reflect the view that this is a fundamental principle of justice, not a coercive mechanism. It 'extracts' only the ability of government to classify by race, which is seen as a benefit to all individuals. The high accessibility_collapse (0.9) indicates that, from this perspective, there are no legitimate alternatives to colorblindness in governmental action. Resistance (0.15) is low because, within this reading's framework, opposition is seen as a misunderstanding of fundamental constitutional principles, not a legitimate challenge to an extractive structure. The theater_ratio is 0.0 because the constraint is understood as purely functional, with no performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   While this reading presents itself as a Mountain, other readings (remedial, diversity) would classify it differently, seeing it as a Snare or Tangled Rope that extracts from historically disadvantaged groups by preventing necessary remedies. The divergence is precisely what the kernel framework is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   All individuals are beneficiaries (d=0.0) as they are protected from racial classification. Governmental entities are agenda-setters (d=0.5) as they must administer policies consistent with colorblindness. Proponents of race-conscious policies are payers (d=1.0) as their policy goals are constrained. The Supreme Court is an observer (d=0.5) in its role as interpreter.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the mandate of colorblindness is timeless and universally applicable, thus it cannot suffer from mandatrophy. The founding problem of racial discrimination is considered 'live,' but the solution is seen as strictly individualistic and formally equal, making any deviation from colorblindness a violation, not a necessary adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the principle of colorblindness a genuine natural law (a Mountain) or a constructed legal interpretation that benefits identifiable agents (a potential False Summit)?',
    'Analysis of historical context and judicial philosophy: if the principle''s application has demonstrably shifted to protect new beneficiaries or suppress new forms of resistance, it suggests a constructed nature. If it has remained invariant across changing social contexts, it supports naturalness.',
    'If found to be a constructed principle, the classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the active enforcement and identifiable beneficiaries/victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity between a natural legal principle and a constructed judicial doctrine.').

omega_variable(
    individual_vs_group_rights,
    'Does the Equal Protection Clause primarily protect individual rights, or does it also encompass group rights and remedies for historical group-based harms?',
    'Further judicial rulings and legislative action clarifying the scope of ''person'' and ''equal protection'' in relation to group identity and historical disadvantage.',
    'If group rights are recognized, this colorblind reading would be seen as actively suppressing legitimate claims, increasing its measured extractiveness and suppression, potentially reclassifying it as a Snare or Tangled Rope from the perspective of historically disadvantaged groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_group_rights, conceptual, 'The fundamental conceptual disagreement over the referent of ''equal protection''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__colorblind_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__colorblind_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__colorblind_reading, theater_ratio, 30, 0.0).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__colorblind_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__colorblind_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement(equa_tr_t60, equal_protection_clause__colorblind_reading, theater_ratio, 60, 0.0).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__colorblind_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__colorblind_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__colorblind_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__colorblind_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__colorblind_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(equa_be_t60, equal_protection_clause__colorblind_reading, base_extractiveness, 60, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__colorblind_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__colorblind_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__colorblind_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__colorblind_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__colorblind_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(equa_su_t60, equal_protection_clause__colorblind_reading, suppression_requirement, 60, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, racial_gerrymandering_prohibitions).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading represents a different structural claim about the clause's function and impact, leading to different classifications and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
