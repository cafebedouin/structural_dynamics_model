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
 *   human_readable: Equal Protection Clause: Colorblind Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause, which holds that the government is forbidden from
 *   making any racial classifications, treating individuals as rights-bearers
 *   independent of group membership. It is presented as a Mountain due to its
 *   proponents' view of it as a fundamental, immutable principle of
 *   constitutional law. The low extractiveness reflects the formal
 *   application of a rule that, by its own lights, extracts nothing from
 *   individuals but rather protects them. The beneficiaries are all
 *   individuals, conceptually, as their rights are protected from racial
 *   classification. The victims are those who advocate for or implement
 *   race-conscious policies.
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
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause: Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '62cf7969-59be-4570-b2cd-c283e9ea4010').
narrative_ontology:cs_kernel_codification('62cf7969-59be-4570-b2cd-c283e9ea4010', fixed_text).
narrative_ontology:cs_authority_grounding('62cf7969-59be-4570-b2cd-c283e9ea4010', lineage).
narrative_ontology:cs_interpretation_layer_present('62cf7969-59be-4570-b2cd-c283e9ea4010').
narrative_ontology:cs_reading_relation('62cf7969-59be-4570-b2cd-c283e9ea4010', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('62cf7969-59be-4570-b2cd-c283e9ea4010', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('62cf7969-59be-4570-b2cd-c283e9ea4010', foundational, racial_classifications_inherently_suspect).
narrative_ontology:cs_axiom_status(racial_classifications_inherently_suspect, holdable).
narrative_ontology:cs_axiom_grounding('62cf7969-59be-4570-b2cd-c283e9ea4010', racial_classifications_inherently_suspect, deontological).
narrative_ontology:cs_axiom('62cf7969-59be-4570-b2cd-c283e9ea4010', foundational, individual_rights_transcend_group_identity).
narrative_ontology:cs_axiom_status(individual_rights_transcend_group_identity, holdable).
narrative_ontology:cs_axiom_grounding('62cf7969-59be-4570-b2cd-c283e9ea4010', individual_rights_transcend_group_identity, deontological).
narrative_ontology:cs_reference_frame('62cf7969-59be-4570-b2cd-c283e9ea4010', post_civil_rights_colorblind_ideal).
narrative_ontology:cs_drift_state('62cf7969-59be-4570-b2cd-c283e9ea4010', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62cf7969-59be-4570-b2cd-c283e9ea4010', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, race_conscious_policy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, state_and_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle that government cannot classify by race, ensuring equal treatment under the law regardless of group identity. This is a foundational right, not a choice.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals_as_rights_bearers, beneficiary,
    powerless, generational, identity_locked, national).

% Interprets and enforces the Equal Protection Clause to prohibit all governmental racial classifications, viewing individuals as the sole rights-bearers. Their institutional role is to uphold this interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court_justices_colorblind_bloc, agenda_setter,
    institutional, generational, identity_locked, national).

% Bear the cost of being unable to implement policies that use racial classifications, even for remedial or diversity purposes. Their efforts to achieve substantive equality through race-conscious means are blocked by this reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, race_conscious_policy_advocates, payer,
    organized, generational, constrained, national).

% Are constrained in their ability to design and implement policies that consider race, even to address historical discrimination or promote diversity. They must navigate legal challenges based on this reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, state_and_local_governments, payer,
    institutional, biographical, constrained, local).

% Analyze and articulate the philosophical and legal foundations of the colorblind reading, contributing to its intellectual persistence and influence on judicial interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, legal_scholars_colorblind_school, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal standard for governmental non-discrimination based on race, simplifying legal compliance by prohibiting all racial classifications and ensuring a common understanding of individual rights.
% TRANSFER_FUNCTION: Transfers the burden of non-discrimination from specific racial groups to the government itself, ensuring that no individual is treated differently by the state on account of race. It also transfers the cost of foregone race-conscious policies to advocates of such policies.
% ABSENT_VOICES: Advocates for race-conscious policies (e.g., those supporting affirmative action or reparations) are present in public discourse but are structurally excluded from the interpretive framework of this reading, which deems their policy goals unconstitutional.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape regarding race and government action would fundamentally shift. Race-conscious policies would likely proliferate, and the concept of individual rights independent of group identity in this context would be significantly diminished, leading to a major reorganization of constitutional law and public policy.
% FOUNDING_PROBLEM: The problem of governmental racial discrimination and the need to ensure equal protection of the laws for all citizens, particularly in the wake of slavery and Jim Crow.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court's consistent jurisprudence and ongoing public debate attest to the enduring nature of the problem of racial discrimination, even as the interpretation of 'equal protection' remains contested. Legal scholars from various schools corroborate the historical context and ongoing relevance of the clause.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.05) and suppression (0.1) reflect the view that this reading is a natural, self-evident principle of justice, requiring minimal active enforcement beyond judicial review. Its 'mountain' classification stems from the belief that it represents an unchangeable constitutional truth. The high accessibility collapse (0.9) indicates that, within this framework, alternatives to colorblindness are seen as logically foreclosed. Resistance (0.05) is low because, from this perspective, the principle is widely accepted as correct, with 'resistance' coming only from those seeking to subvert it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the colorblind reading's proponents, this is a fundamental, non-extractive principle. From the perspective of those advocating for race-conscious policies, this reading itself is highly extractive, as it prevents remedies for historical and ongoing systemic inequalities. This divergence is captured by the 'victims' declaration and the omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   All individuals, as abstract rights-bearers, are the beneficiaries (d=0.0) as the constraint protects them from racial classification. Supreme Court justices who adhere to this reading are agenda-setters (d=0.0-0.15) as they interpret and enforce it. Advocates for race-conscious policies and state/local governments seeking to implement them are payers/targets (d=0.8-1.0) as their policy options are restricted. Legal scholars supporting this view are observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the colorblind principle a genuine natural law of justice, or a constructed legal interpretation that benefits identifiable agents (e.g., those who benefit from the status quo)?',
    'Philosophical analysis of the nature of rights and justice, combined with empirical analysis of the distributional effects of ''colorblind'' policies.',
    'If a constructed principle, the constraint''s ''mountain'' classification would be re-evaluated, potentially shifting to a ''tangled_rope'' or ''snare'' if significant extraction is identified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity between a natural law and a constructed legal principle.').

omega_variable(
    substantive_vs_formal_equality,
    'Does the colorblind reading achieve substantive equality, or only formal equality, potentially perpetuating existing inequalities?',
    'Empirical studies on the long-term societal outcomes (e.g., educational attainment, wealth gaps, representation) of colorblind policies versus race-conscious alternatives.',
    'If it only achieves formal equality while perpetuating substantive inequality, its ''beneficiary'' status for ''all_individuals'' would be challenged, and its effective extractiveness for certain groups would be re-evaluated upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_vs_formal_equality, empirical, 'Whether formal colorblindness leads to substantive equality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal precedent, judicial enforcement) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-judicial-reversal policy trajectory: if race-conscious policies remain difficult to implement even after legal barriers are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them after legal changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
