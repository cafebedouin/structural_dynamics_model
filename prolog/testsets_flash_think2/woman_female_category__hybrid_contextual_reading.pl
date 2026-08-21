% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Definition of Woman/Female
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint describes a proposed or implemented framework for
 *   defining 'woman' or 'female' that varies by context: biological sex is
 *   prioritized for medical, sports, and safety contexts, while gender
 *   identity is prioritized for social and legal recognition. It is presented
 *   as a compromise solution to reduce conflict between competing universal
 *   definitions. The framework requires active enforcement by institutional
 *   actors to apply the correct definition in each domain.
 *
 * KEY AGENTS:
 *   - institutional_actors_seeking_conflict_minimization: Agenda-setter/Beneficiary (institutional/constrained)
 *   - transgender_individuals_in_sex_based_contexts: Payer/Victim (moderate/identity_locked)
 *   - sex_based_rights_advocates_in_gender_identity_contexts: Payer/Victim (organized/constrained)
 *   - medical_professionals: Beneficiary/Agenda-setter (institutional/constrained)
 *   - sports_organizations: Beneficiary/Agenda-setter (institutional/constrained)
 *   - legal_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.45).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Definition of Woman/Female").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'e5b2d26b-ad10-4624-afb4-55c7621f44d7').
narrative_ontology:cs_kernel_codification('e5b2d26b-ad10-4624-afb4-55c7621f44d7', formalized).
narrative_ontology:cs_authority_grounding('e5b2d26b-ad10-4624-afb4-55c7621f44d7', expertise).
narrative_ontology:cs_interpretation_layer_present('e5b2d26b-ad10-4624-afb4-55c7621f44d7').
narrative_ontology:cs_reading_relation('e5b2d26b-ad10-4624-afb4-55c7621f44d7', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5b2d26b-ad10-4624-afb4-55c7621f44d7', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e5b2d26b-ad10-4624-afb4-55c7621f44d7', foundational, contextual_definitions_optimize_outcomes).
narrative_ontology:cs_axiom_status(contextual_definitions_optimize_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('e5b2d26b-ad10-4624-afb4-55c7621f44d7', contextual_definitions_optimize_outcomes, instrumental).
narrative_ontology:cs_axiom('e5b2d26b-ad10-4624-afb4-55c7621f44d7', foundational, harm_reduction_principle).
narrative_ontology:cs_axiom_status(harm_reduction_principle, holdable).
narrative_ontology:cs_axiom_grounding('e5b2d26b-ad10-4624-afb4-55c7621f44d7', harm_reduction_principle, deontological).
narrative_ontology:cs_reference_frame('e5b2d26b-ad10-4624-afb4-55c7621f44d7', contextual_proportionality_framework).
narrative_ontology:cs_drift_state('e5b2d26b-ad10-4624-afb4-55c7621f44d7', contemporary_policy_debates, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e5b2d26b-ad10-4624-afb4-55c7621f44d7', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, medical_professionals).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_organizations).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, transgender_individuals_in_sex_based_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates_in_gender_identity_contexts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors (e.g., policymakers, legal systems, human rights bodies) seek to establish a workable framework that reduces direct conflict and provides functional clarity across diverse contexts. They benefit from reduced litigation and social friction, but bear the administrative burden of implementing contextual rules.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, beneficiary).

% Individuals whose gender identity is 'woman' but whose biological sex is male, who are subject to sex-based definitions in contexts like sports or medical care. They experience extraction when their gender identity is subordinated to biological sex in these contexts, leading to exclusion or misgendering.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, transgender_individuals_in_sex_based_contexts, payer,
    moderate, biographical, identity_locked, global).

% Advocates (often cisgender women) who prioritize biological sex as the primary determinant of 'woman' or 'female' for all contexts, particularly for single-sex spaces or data collection. They experience extraction when gender identity is prioritized over biological sex in social or legal recognition contexts, perceiving it as an erosion of sex-based protections.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates_in_gender_identity_contexts, payer,
    organized, generational, constrained, global).

% Benefit from clarity in applying biological sex definitions for medical diagnosis, treatment, and research, where sex-linked biological differences are clinically relevant. They contribute expertise to define appropriate contexts for sex-based definitions.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, medical_professionals, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, medical_professionals, agenda_setter).

% Benefit from a framework that allows them to apply biological sex definitions to ensure fair competition and safety in sports, particularly in categories where sex-linked physiological differences are significant. They contribute to policy development for their specific domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_organizations, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sports_organizations, agenda_setter).

% Analyze the legal implications, coherence, and practical application of the hybrid contextual framework. They provide critical commentary and propose refinements without directly participating in the enforcement or experiencing the direct extraction.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for navigating conflicting definitions of 'woman' and 'female' across various social, legal, medical, and sporting domains, aiming to reduce direct conflict and provide functional clarity by applying different definitions contextually.
% TRANSFER_FUNCTION: Transfers the burden of contextual adaptation and occasional subordination of preferred definitions to both transgender individuals (in sex-based contexts) and sex-based rights advocates (in gender-identity contexts), while transferring a degree of conflict-reduction and policy stability to institutional actors.
% ABSENT_VOICES: Those who advocate for a single, universal definition (either purely sex-based or purely gender-identity-based) would object to the inherent compromise and perceived inconsistency of this hybrid approach. They are often present in public discourse but are structurally excluded from the 'compromise' framing of this constraint.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the existing conflicts over definitions would intensify, leading to greater legal ambiguity, social friction, and policy paralysis in all affected domains. Institutions would lack a guiding principle, and both groups would face universal subordination of their preferred definition in many contexts, leading to increased harm and litigation.
% FOUNDING_PROBLEM: The intractable conflict arising from universal application of either a purely sex-based or purely gender-identity-based definition of 'woman' or 'female' across all contexts, leading to perceived harms and rights violations for different groups and policy paralysis for institutions.
% FOUNDING_PROBLEM_CORROBORATION: Legal bodies, human rights organizations, and academic researchers from diverse fields (sociology, law, bioethics) outside the immediate advocacy groups corroborate the ongoing nature of this conflict and the need for a framework to manage it. Legislative hearings and policy reports also attest to the problem's persistence.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).
:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate conflicting interests and provide functional clarity (beneficiaries: institutional actors, medical/sports bodies), but it does so by imposing asymmetric costs on both transgender individuals and sex-based rights advocates when their preferred definition is subordinated in specific contexts (victims). The extractiveness (0.45) reflects the ongoing friction and perceived harms from these contextual applications. Suppression (0.55) is moderate, as neither universal definition is fully suppressed, but their application is constrained. Resistance (0.6) is high because both affected groups actively challenge the framework's boundaries and applications. Theater ratio is low (0.2) as the framework is a genuine attempt at policy, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   Institutional actors and professionals (medical, sports) experience this as a beneficial coordination mechanism that brings order to a chaotic debate. In contrast, transgender individuals and sex-based rights advocates experience it as an extractive compromise, where their fundamental claims are selectively invalidated depending on the context. The engine's per-seat classification will reflect this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors, medical professionals, and sports organizations are beneficiaries (low d) as they gain clarity and reduced conflict. Transgender individuals and sex-based rights advocates are targets (high d) as they bear the costs of contextual subordination. Legal scholars are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the hybrid approach as a pure Rope (ignoring the extraction from both groups) or a pure Snare (ignoring the genuine coordination function for institutional actors). It highlights that the 'compromise' itself generates costs for the parties it seeks to coordinate, indicating a Tangled Rope structure rather than a benign solution. The 'live' status of the founding problem suggests it is not a Piton, as the core conflict it addresses remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_bias_in_contextual_application,
    'Does the practical application of the hybrid contextual framework implicitly favor one reading (sex-based or gender-identity-based) over the other, despite its stated intent of balance?',
    'Empirical analysis of policy outcomes, resource allocation, and lived experiences across various contexts over time. If one group consistently experiences greater exclusion or harm, it suggests an implicit bias.',
    'If an implicit bias is found, the effective extractiveness for the disfavored group is higher than currently measured, and the constraint may lean closer to a Snare for that group, despite its stated coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_bias_in_contextual_application, empirical, 'Whether the contextual application maintains true balance or has an implicit bias.').

omega_variable(
    administrative_burden_vs_conflict_reduction,
    'Does the complexity of applying different definitions in different contexts create more administrative burden, confusion, and new forms of conflict than it resolves?',
    'Longitudinal studies of institutional implementation, legal challenges, and public understanding. If administrative costs and new conflicts outweigh resolved ones, the framework''s coordination function is degraded.',
    'If the burden outweighs the benefit, the constraint''s coordination function is less effective, increasing its effective extractiveness and potentially shifting it towards a Piton if the original mandate is overwhelmed by complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_burden_vs_conflict_reduction, empirical, 'Assessing the net benefit of contextual complexity.').

omega_variable(
    coordination_vs_institutional_inertia,
    'Is the institutional pursuit of ''conflict minimization'' a genuine coordination effort, or a cover for institutional inertia and a reluctance to take a definitive stance, thereby offloading the burden onto affected groups?',
    'Analysis of institutional decision-making processes, transparency in policy justifications, and comparison with jurisdictions that have adopted more definitive stances. If the primary driver is avoiding difficult decisions, it suggests inertia.',
    'If driven by inertia, the coordination function is weaker, and the constraint''s extractiveness is higher, as the institutional ''benefit'' is primarily avoiding responsibility rather than solving a problem. This would push it closer to a Snare or a Piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_institutional_inertia, conceptual, 'Distinguishing genuine coordination from institutional avoidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__hybrid_contextual_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(woma_tr_t2017, woman_female_category__hybrid_contextual_reading, theater_ratio, 2017, 0.16).
narrative_ontology:measurement(woma_tr_t2019, woman_female_category__hybrid_contextual_reading, theater_ratio, 2019, 0.17).
narrative_ontology:measurement(woma_tr_t2021, woman_female_category__hybrid_contextual_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(woma_tr_t2023, woman_female_category__hybrid_contextual_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement(woma_tr_t2025, woman_female_category__hybrid_contextual_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t2015, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(woma_be_t2017, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2017, 0.38).
narrative_ontology:measurement(woma_be_t2019, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement(woma_be_t2021, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2021, 0.43).
narrative_ontology:measurement(woma_be_t2023, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2023, 0.44).
narrative_ontology:measurement(woma_be_t2025, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2015, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(woma_su_t2017, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(woma_su_t2019, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2019, 0.51).
narrative_ontology:measurement(woma_su_t2021, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2021, 0.53).
narrative_ontology:measurement(woma_su_t2023, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2023, 0.54).
narrative_ontology:measurement(woma_su_t2025, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2025, 0.55).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2015, tn=2025
narrative_ontology:measurement(woma_grid_01, woman_female_category__hybrid_contextual_reading, accessibility_collapse(class), 2015, 0.4).
narrative_ontology:measurement(woma_grid_02, woman_female_category__hybrid_contextual_reading, accessibility_collapse(class), 2025, 0.6).
narrative_ontology:measurement(woma_grid_03, woman_female_category__hybrid_contextual_reading, accessibility_collapse(individual), 2015, 0.3).
narrative_ontology:measurement(woma_grid_04, woman_female_category__hybrid_contextual_reading, accessibility_collapse(individual), 2025, 0.6).
narrative_ontology:measurement(woma_grid_05, woman_female_category__hybrid_contextual_reading, accessibility_collapse(organizational), 2015, 0.4).
narrative_ontology:measurement(woma_grid_06, woman_female_category__hybrid_contextual_reading, accessibility_collapse(organizational), 2025, 0.7).
narrative_ontology:measurement(woma_grid_07, woman_female_category__hybrid_contextual_reading, accessibility_collapse(structural), 2015, 0.3).
narrative_ontology:measurement(woma_grid_08, woman_female_category__hybrid_contextual_reading, accessibility_collapse(structural), 2025, 0.6).
narrative_ontology:measurement(woma_grid_09, woman_female_category__hybrid_contextual_reading, resistance(class), 2015, 0.6).
narrative_ontology:measurement(woma_grid_10, woman_female_category__hybrid_contextual_reading, resistance(class), 2025, 0.8).
narrative_ontology:measurement(woma_grid_11, woman_female_category__hybrid_contextual_reading, resistance(individual), 2015, 0.5).
narrative_ontology:measurement(woma_grid_12, woman_female_category__hybrid_contextual_reading, resistance(individual), 2025, 0.7).
narrative_ontology:measurement(woma_grid_13, woman_female_category__hybrid_contextual_reading, resistance(organizational), 2015, 0.5).
narrative_ontology:measurement(woma_grid_14, woman_female_category__hybrid_contextual_reading, resistance(organizational), 2025, 0.7).
narrative_ontology:measurement(woma_grid_15, woman_female_category__hybrid_contextual_reading, resistance(structural), 2015, 0.4).
narrative_ontology:measurement(woma_grid_16, woman_female_category__hybrid_contextual_reading, resistance(structural), 2025, 0.7).
narrative_ontology:measurement(woma_grid_17, woman_female_category__hybrid_contextual_reading, stakes_inflation(class), 2015, 0.5).
narrative_ontology:measurement(woma_grid_18, woman_female_category__hybrid_contextual_reading, stakes_inflation(class), 2025, 0.8).
narrative_ontology:measurement(woma_grid_19, woman_female_category__hybrid_contextual_reading, stakes_inflation(individual), 2015, 0.4).
narrative_ontology:measurement(woma_grid_20, woman_female_category__hybrid_contextual_reading, stakes_inflation(individual), 2025, 0.7).
narrative_ontology:measurement(woma_grid_21, woman_female_category__hybrid_contextual_reading, stakes_inflation(organizational), 2015, 0.5).
narrative_ontology:measurement(woma_grid_22, woman_female_category__hybrid_contextual_reading, stakes_inflation(organizational), 2025, 0.8).
narrative_ontology:measurement(woma_grid_23, woman_female_category__hybrid_contextual_reading, stakes_inflation(structural), 2015, 0.4).
narrative_ontology:measurement(woma_grid_24, woman_female_category__hybrid_contextual_reading, stakes_inflation(structural), 2025, 0.7).
narrative_ontology:measurement(woma_grid_25, woman_female_category__hybrid_contextual_reading, suppression(class), 2015, 0.4).
narrative_ontology:measurement(woma_grid_26, woman_female_category__hybrid_contextual_reading, suppression(class), 2025, 0.7).
narrative_ontology:measurement(woma_grid_27, woman_female_category__hybrid_contextual_reading, suppression(individual), 2015, 0.3).
narrative_ontology:measurement(woma_grid_28, woman_female_category__hybrid_contextual_reading, suppression(individual), 2025, 0.6).
narrative_ontology:measurement(woma_grid_29, woman_female_category__hybrid_contextual_reading, suppression(organizational), 2015, 0.4).
narrative_ontology:measurement(woma_grid_30, woman_female_category__hybrid_contextual_reading, suppression(organizational), 2025, 0.7).
narrative_ontology:measurement(woma_grid_31, woman_female_category__hybrid_contextual_reading, suppression(structural), 2015, 0.3).
narrative_ontology:measurement(woma_grid_32, woman_female_category__hybrid_contextual_reading, suppression(structural), 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel. This 'hybrid_contextual_reading' attempts to reconcile the 'sex_biology_reading' and 'gender_identity_reading' by assigning different definitions to different contexts. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
