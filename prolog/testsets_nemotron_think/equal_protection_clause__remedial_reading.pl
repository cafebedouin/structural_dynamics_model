% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Race-Conscious Remediation of Historical Subordination
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause holds that the
 *   Constitution not only permits but requires race-conscious government
 *   action to remedy the ongoing effects of historical group subordination.
 *   This reading powered Reconstruction, the civil rights movement's
 *   legislative victories, and the affirmative action jurisprudence from the
 *   1960s through the 2000s. It claims a coordination function (dismantling
 *   caste, achieving substantive equality) and carries an extraction function
 *   (non-preferred individuals bear the cost of racial preferences). The
 *   constraint is structurally temporary — it carries a sunset logic:
 *   remediation ends when its work is done. The current Court majority has
 *   effectively rejected this reading, but it remains a live position in
 *   constitutional discourse and continues to shape some state and local
 *   policies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.45).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Reading: Race-Conscious Remediation of Historical Subordination").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '3c0468cd-9b17-4c57-94ec-325797ebdb56').
narrative_ontology:cs_kernel_codification('3c0468cd-9b17-4c57-94ec-325797ebdb56', formalized).
narrative_ontology:cs_authority_grounding('3c0468cd-9b17-4c57-94ec-325797ebdb56', lineage).
narrative_ontology:cs_interpretation_layer_present('3c0468cd-9b17-4c57-94ec-325797ebdb56').
narrative_ontology:cs_reading_relation('3c0468cd-9b17-4c57-94ec-325797ebdb56', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('3c0468cd-9b17-4c57-94ec-325797ebdb56', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('3c0468cd-9b17-4c57-94ec-325797ebdb56', foundational, historical_subordination_requires_remediation).
narrative_ontology:cs_axiom_status(historical_subordination_requires_remediation, holdable).
narrative_ontology:cs_axiom_grounding('3c0468cd-9b17-4c57-94ec-325797ebdb56', historical_subordination_requires_remediation, deontological).
narrative_ontology:cs_axiom('3c0468cd-9b17-4c57-94ec-325797ebdb56', foundational, substantive_equality_requires_group_remedy).
narrative_ontology:cs_axiom_status(substantive_equality_requires_group_remedy, holdable).
narrative_ontology:cs_axiom_grounding('3c0468cd-9b17-4c57-94ec-325797ebdb56', substantive_equality_requires_group_remedy, deontological).
narrative_ontology:cs_reference_frame('3c0468cd-9b17-4c57-94ec-325797ebdb56', reconstruction_remedial_framework).
narrative_ontology:cs_drift_state('3c0468cd-9b17-4c57-94ec-325797ebdb56', contemporary_colorblind_doctrine, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c0468cd-9b17-4c57-94ec-325797ebdb56', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, government_institutions).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, corrective_justice_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups that were subjected to slavery, segregation, and ongoing systemic discrimination. They receive the benefit of race-conscious admissions, hiring, and contracting programs designed to remediate historical subordination. Their exit from the constraint's benefit structure is constrained because the remediation is tied to group membership and cannot be individually opted into or out of; the constraint's sunset would remove the remedial benefits they currently receive.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% Individual applicants (primarily white and Asian-American students in education contexts; white and male workers in employment contexts) who are disadvantaged by race-conscious allocation of scarce positions. They bear the cost of the remediation through reduced admission/hiring probabilities. Their exit is constrained because the constraint operates at the point of competitive selection for major life opportunities (elite universities, public employment) where alternatives are limited and the cost of avoiding the constraint (foregoing the opportunity) is high.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_group_members, payer,
    moderate, biographical, constrained, national).

% Universities, employers, and government agencies that design and administer race-conscious remediation programs. They set the specific parameters of the constraint (which groups count, what preferences apply, when sunset triggers). They benefit institutionally from the legitimacy and federal funding that compliance brings, and from the diversity that remediation produces. Their exit is arbitrage-grade: they can modify or abandon programs within legal boundaries, and some have done so preemptively.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, government_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, government_institutions, beneficiary).

% Federal courts (especially the Supreme Court) that define the constitutional boundaries of the remedial reading. They determine whether a given program qualifies as valid remediation, what evidence of past discrimination suffices, and when remediation is complete. They do not directly collect benefits or pay costs but structurally determine the constraint's scope and persistence. Their exit is analytical: they interpret the constraint but are not subject to its allocation effects.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Constitutional scholars, litigators, and organizations (e.g., Students for Fair Admissions, Pacific Legal Foundation) who argue that equal protection forbids all racial classifications. They are structurally excluded from the remedial framework's internal logic — the remedial reading treats their objection as irrelevant to the remediation mandate. They are trapped in the sense that they must litigate within a doctrinal structure that has already accepted the remedial premise, and their preferred reading (colorblind) has been rejected by the framework's authoritative interpreters.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, generational, trapped, national).

% Academic observers who analyze the constraint's operation, legitimacy, and effects across readings. They neither collect benefits nor pay costs directly. Their analytical seat allows them to see the full structure: the remedial reading's coordination function (addressing historical subordination), its extraction (costs to non-preferred individuals), its sunset logic, and its contested status among sibling readings.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Remediates the ongoing effects of historical group subordination (slavery, Jim Crow, exclusionary immigration laws, redlining) by allocating opportunities to historically marginalized groups until substantive equality is achieved. Solves the coordination problem of how a polity transitions from a caste system to genuine equal citizenship without leaving the subordinated groups permanently behind.
% TRANSFER_FUNCTION: Moves admission slots, hiring positions, government contracts, and legislative representation from non-preferred group members (who would receive them under race-neutral competition) to historically marginalized racial minorities, as the mechanism of remediation.
% ABSENT_VOICES: Colorblind advocates are structurally excluded from the remedial framework — they would object that any racial classification violates equal protection, but the remedial reading treats this objection as conceptually confused (confusing the classification that subordinates with the classification that remediates). Non-preferred group members who support remediation on solidarity grounds are also absent from the victim seat; the constraint's logic assigns them to the payer seat regardless of their political views.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, race-conscious affirmative action programs would lose their constitutional foundation. Universities and employers would immediately shift to race-neutral policies (or be forced to by litigation). The allocation of opportunities would rearrange significantly: historically marginalized groups would lose remedial access; non-preferred group members would gain the positions previously allocated to remediation. The polity's approach to historical injustice would shift from structural remediation to either colorblind formal equality or diversity-based justifications.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to secure equal citizenship for formerly enslaved persons, but formal equality failed to dismantle the caste system. The remedial reading was built to solve the problem that 'equal protection of the laws' required more than colorblindness — it required affirmative dismantling of the badges and incidents of slavery and the subsequent caste regime.
% FOUNDING_PROBLEM_CORROBORATION: The remedial reading's founding problem is attested by the Congressional framers of the Fourteenth Amendment (Thaddeus Stevens, Charles Sumner), Reconstruction-era legislation (Freedmen's Bureau Acts, Civil Rights Act of 1866), and the NAACP Legal Defense Fund's litigation strategy culminating in Brown v. Board. The colorblind reading's proponents (including Justice Harlan's Plessy dissent and modern originalists) contest whether the Amendment's text supports group-based remediation rather than individual colorblindness. The diversity reading's proponents (Justice Powell in Bakke, Justice O'Connor in Grutter) accept the founding problem but locate the constitutional justification in forward-looking educational benefits rather than backward-looking remediation. No consensus exists outside the remedial reading's own beneficiaries and their institutional allies.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the substantial transfer of opportunities from non-preferred to preferred groups under race-conscious programs. Suppression (0.45) is moderate: the constraint operates through law and institutional policy rather than direct coercion, but non-preferred individuals have no effective exit from the allocation system (elite universities, public employment) and colorblind advocates are structurally excluded from the framework. Theater ratio (0.22) is low: the remedial programs perform real allocation work, though some diversity-rationalized programs may be theatrical substitutions for remedial ones. Accessibility collapse (0.35) is moderate: alternatives (race-neutral policies, class-based affirmative action) exist but are treated as insufficient by the reading's logic. Resistance (0.58) is high: sustained political, legal, and intellectual opposition from colorblind and diversity readings.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the constraint is genuine coordination: it solves the founding problem of caste. From the payer seat, it is extraction: they bear costs for wrongs they did not commit. From the agenda_setter seat (institutions), it is a managed transition with institutional benefits. The engine computes these divergences from the structural data; the claimed type (scaffold) reflects the reading's own self-understanding as temporary remediation.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized minorities are beneficiaries (d near 0.0) — the constraint subsidizes their access. Non-preferred group members are payers (d near 1.0) — the constraint extracts their competitive position. Government institutions are agenda_setters with secondary beneficiary role (d ~ 0.2) — they administer and gain institutional legitimacy. Courts are agenda_setters (d ~ 0.5) — they define the constraint but don't directly collect or pay. Colorblind advocates are excluded (trapped, d undefined) — they bear the cost of living under a framework that rejects their constitutional vision. Constitutional scholars are observers (analytical, d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (dismantling caste) is contested as live vs. dead. If dead, the constraint's persistence would be mandatrophy — a remedial scaffold that outlived its function. The colorblind reading argues the founding problem is dead (formal equality achieved); the remedial reading argues it is live (substantive equality not achieved); the diversity reading sidesteps by offering a different justification. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals that the constraint's legitimacy depends on a factual/historical dispute the engine cannot resolve — exactly the structure mandatrophy detection is designed to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criterion,
    'What constitutes completion of remediation such that the sunset clause triggers? Is it statistical parity, elimination of structural disparities, or a political determination?',
    'Judicial articulation of a clear endpoint standard, or legislative sunset provisions with measurable benchmarks. Historical analysis of whether any remedial program has ever voluntarily declared its work complete.',
    'If no coherent completion criterion exists, the scaffold becomes a piton (permanent extraction without coordination function). If a criterion exists but is never met, the constraint persists as extraction. If a criterion exists and is met, the constraint sunsets as designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_completion_criterion, conceptual, 'Whether the remedial reading''s sunset logic is operationalizable or aspirational.').

omega_variable(
    group_vs_individual_remediation_boundary,
    'Does the remedial reading require group-proportional outcomes (which extracts from non-preferred individuals) or merely the removal of barriers (which would not)?',
    'Doctrinal analysis of whether strict scrutiny''s ''narrow tailoring'' requirement permits quotas/goals or only barrier removal. Empirical study of what actual remedial programs do.',
    'If the reading requires group-proportional outcomes, extraction is structural and high. If it only requires barrier removal, extraction drops and the constraint may be a rope. The diversity reading''s ''critical mass'' language blurs this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_vs_individual_remediation_boundary, conceptual, 'Whether remediation structurally requires racial proportionality (extraction) or barrier removal (coordination).').

omega_variable(
    historical_causation_attribution,
    'Can the current disparities of specific groups be causally attributed to the specific historical injustices the remedial reading targets (slavery, Jim Crow), as opposed to other factors?',
    'Interdisciplinary research (sociology, economics, history) on the causal pathways from historical subordination to present disparities, controlling for immigration, class, family structure, and other variables.',
    'If attribution fails for some groups currently benefiting, the remedial reading over-includes beneficiaries and its extraction becomes less justified. If attribution holds broadly, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_causation_attribution, empirical, 'Causal link between targeted historical injustices and present disparities for current beneficiary groups.').

omega_variable(
    remedial_reading_kernel_relation,
    'Does the remedial reading foreclose the colorblind reading within a single constitutional framework, or do they merely coexist as competing interpretations?',
    'Logical analysis of whether a constitutional theory can simultaneously hold that (a) equal protection requires race-conscious remediation and (b) equal protection forbids all racial classifications. Examination of whether any justice or scholar has held a hybrid position.',
    'If forecloses, the kernel contains a genuine logical contradiction between readings. If coexists_with, the kernel supports pluralism. The diversity reading''s relation to both is also relevant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_reading_kernel_relation, conceptual, 'Structural relationship between remedial and colorblind readings of the equal protection kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1868, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eprr_tr_t1868, equal_protection_clause__remedial_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(eprr_tr_t1896, equal_protection_clause__remedial_reading, theater_ratio, 1896, 0.85).
narrative_ontology:measurement(eprr_tr_t1954, equal_protection_clause__remedial_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(eprr_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(eprr_tr_t2003, equal_protection_clause__remedial_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(eprr_tr_t2016, equal_protection_clause__remedial_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(eprr_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.22).
narrative_ontology:measurement(eprr_tr_t2025, equal_protection_clause__remedial_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(eprr_be_t1868, equal_protection_clause__remedial_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(eprr_be_t1896, equal_protection_clause__remedial_reading, base_extractiveness, 1896, 0.05).
narrative_ontology:measurement(eprr_be_t1954, equal_protection_clause__remedial_reading, base_extractiveness, 1954, 0.25).
narrative_ontology:measurement(eprr_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(eprr_be_t2003, equal_protection_clause__remedial_reading, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(eprr_be_t2016, equal_protection_clause__remedial_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(eprr_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(eprr_be_t2025, equal_protection_clause__remedial_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eprr_su_t1868, equal_protection_clause__remedial_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(eprr_su_t1896, equal_protection_clause__remedial_reading, suppression_requirement, 1896, 0.9).
narrative_ontology:measurement(eprr_su_t1954, equal_protection_clause__remedial_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(eprr_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(eprr_su_t2003, equal_protection_clause__remedial_reading, suppression_requirement, 2003, 0.45).
narrative_ontology:measurement(eprr_su_t2016, equal_protection_clause__remedial_reading, suppression_requirement, 2016, 0.45).
narrative_ontology:measurement(eprr_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.45).
narrative_ontology:measurement(eprr_su_t2025, equal_protection_clause__remedial_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, affirmative_action_university_admissions).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, voting_rights_act_section_2).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, disparate_impact_doctrine).

% DUAL FORMULATION NOTE:
% Part of the equal_protection_clause constraint family. This reading (remedial) decomposes the kernel's contested meaning into a backward-looking corrective justice mandate. The colorblind_reading decomposes it as a formal colorblindness command. The diversity_reading decomposes it as a forward-looking educational benefit permission. The three readings have different ε values (remedial: high; diversity: moderate; colorblind: near-zero), different beneficiary/victim structures, and different temporal logics (remedial: sunset; diversity: stable; colorblind: permanent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__remedial_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_clause__remedial_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
