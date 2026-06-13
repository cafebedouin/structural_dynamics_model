% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint models unconditional income support from the 'freedom
 *   floor' perspective, where it functions as an enabler of autonomy,
 *   dignity, and increased labor market exit capacity. It is one reading of
 *   the broader 'income_support_commitment' kernel. This reading emphasizes
 *   the positive externalities of universal provision and the reduction of
 *   coercive pressures in the labor market, rather than focusing on work
 *   incentives or targeting efficiency. The constraint is designed to be a
 *   Rope, solving a collective action problem (funding a social safety net)
 *   with minimal extraction and suppression, as universality reduces
 *   administrative overhead and stigma.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.05).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, 'cd11f6e4-7a1c-4dc4-9793-99195fc4c530').
narrative_ontology:cs_kernel_codification('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', formalized).
narrative_ontology:cs_authority_grounding('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', lineage).
narrative_ontology:cs_interpretation_layer_present('cd11f6e4-7a1c-4dc4-9793-99195fc4c530').
narrative_ontology:cs_reading_relation('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', foundational, economic_security_as_human_right).
narrative_ontology:cs_axiom_status(economic_security_as_human_right, holdable).
narrative_ontology:cs_axiom_grounding('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', economic_security_as_human_right, deontological).
narrative_ontology:cs_axiom('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', foundational, autonomy_requires_exit_capacity).
narrative_ontology:cs_axiom_status(autonomy_requires_exit_capacity, holdable).
narrative_ontology:cs_axiom_grounding('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', autonomy_requires_exit_capacity, instrumental).
narrative_ontology:cs_reference_frame('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', universal_social_dividend).
narrative_ontology:cs_drift_state('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cd11f6e4-7a1c-4dc4-9793-99195fc4c530', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income, enabling greater autonomy, dignity, and the capacity to refuse exploitative labor. Benefits from reduced precarity and improved bargaining power in the labor market. Contributes to the tax base funding the support.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).

% Receives income support that recognizes and values unpaid care work, providing financial stability and reducing dependency on a single income earner or state-provided services. This enables greater choice in care arrangements.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, local).

% Gains a safety net that reduces the immediate pressure to accept low-wage, insecure work. This increases their bargaining power and capacity to seek better employment or pursue education/training.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, local).

% Receives financial independence that can be critical for exiting abusive relationships, which often involve economic control. The unconditional nature avoids means-testing that could expose them to further risk.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, identity_locked, local).

% Gains a stable income floor that de-risks creative pursuits and entrepreneurial ventures, fostering innovation and cultural production that might otherwise be economically unfeasible.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Faces increased pressure to offer competitive wages and working conditions as workers gain greater exit capacity from undesirable jobs. Contributes to the tax base funding the support. Benefits from a healthier, more stable workforce.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    powerful, biographical, mobile, national).

% Contributes to the tax base required to fund the universal income support. Bears the direct financial cost, but benefits from a more stable society, reduced crime, and improved public health outcomes.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Administers the universal income support program, ensuring timely and unconditional distribution. Benefits from simplified administration compared to means-tested programs, but faces political pressure regarding funding levels and economic impacts.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide commitment to a basic standard of living, ensuring all citizens have sufficient income to meet basic needs, thereby reducing poverty and increasing individual bargaining power in the labor market.
% TRANSFER_FUNCTION: Transfers a regular, unconditional income payment from the general tax base to all citizens, regardless of employment status or other income.
% ABSENT_VOICES: Those who believe that all income should be earned through labor, or that state support inevitably leads to dependency, are present in public discourse but are not structurally excluded from the system itself. Their arguments are part of the ongoing political contestation over the policy's design and funding.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, millions would immediately lose their primary or supplementary income, leading to a sharp increase in poverty, homelessness, and social instability. Labor markets would revert to pre-support power dynamics, and many individuals would lose their capacity for autonomous choices.
% FOUNDING_PROBLEM: The problem of poverty, economic precarity, and the erosion of worker bargaining power in increasingly automated and globalized labor markets, leading to widespread insecurity and social inequality.
% FOUNDING_PROBLEM_CORROBORATION: Economists, social scientists, and international organizations (e.g., UN, ILO) corroborate the ongoing problems of poverty and precarity, citing data on income inequality, gig economy growth, and the rising cost of living. Advocacy groups for workers, caregivers, and marginalized communities also provide extensive corroborating evidence.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because the primary function is to provide a baseline, not to extract from participants. Any 'extraction' is the necessary cost of funding a universal public good. Suppression is very low (0.05) because the unconditional nature removes coercive elements like means-testing or work requirements. Theater ratio is negligible (0.02) as the program's stated goal (providing a freedom floor) aligns directly with its operation. Accessibility collapse is low (0.2) because the constraint itself creates new options (exit from bad jobs, pursuit of education/arts) rather than collapsing them. Resistance is moderate (0.15) reflecting the ongoing political debate over funding and philosophical objections, but not active resistance to the operation of the program itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_citizens' and other beneficiary groups, this is a clear Rope, providing a net benefit and increasing freedom. From the perspective of 'employers' and 'taxpayers', it is a cost, but one that is broadly distributed and justified by societal benefits. The 'welfare_state_administrators' see it as a simplified, more efficient system compared to complex means-tested alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   All citizens, caregivers, precarious workers, abuse survivors, and artists/entrepreneurs are direct beneficiaries (d near 0.0) as they receive unconditional income. Employers and taxpayers are payers (d near 1.0) as they contribute to the funding, though employers also face constrained wage-setting power. Welfare state administrators are agenda-setters, managing the system for collective benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_sustainability,
    'Is the proposed funding mechanism for unconditional income support fiscally sustainable over the long term, especially in the face of demographic shifts or economic downturns?',
    'Longitudinal economic modeling and real-world pilot program data on tax base changes and economic activity.',
    'If unsustainable, the constraint''s claimed ''Rope'' status could degrade to a ''Tangled Rope'' or ''Snare'' if funding shortfalls lead to increased taxation on specific groups or cuts to other essential services.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability, empirical, 'Sustainability of funding for universal income support.').

omega_variable(
    labor_market_impact,
    'What is the actual impact of unconditional income support on labor force participation rates, wage levels, and the types of jobs people choose?',
    'Empirical studies from jurisdictions implementing universal basic income programs, comparing labor market outcomes before and after implementation.',
    'If labor force participation significantly declines or wages are suppressed by employers internalizing the benefit, the ''freedom floor'' claim could be challenged, potentially shifting the classification towards a ''Tangled Rope'' if unintended negative consequences emerge for some groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact, empirical, 'Impact on labor market dynamics and participation.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint a genuine ''freedom floor'' or is it merely a less efficient form of welfare provision, as argued by the ''targeting_efficiency_reading''?',
    'Conceptual analysis of the normative goals: if the primary goal is autonomy and dignity (this reading), then universality is a feature; if the primary goal is poverty reduction at lowest cost (targeting reading), then universality is a bug. Resolution depends on the adopted normative framework.',
    'If the ''targeting_efficiency_reading'' is adopted, the constraint''s ''Rope'' classification might be challenged, as its ''inefficiency'' could be seen as a form of diffuse extraction from taxpayers, pushing it towards a ''Tangled Rope'' or even ''Piton'' if the ''freedom'' benefits are deemed insufficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Conceptual framing of universal income support''s primary purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.02).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.02).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.02).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.02).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__freedom_floor_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'freedom_floor_reading' of the 'income_support_commitment' kernel, focusing on autonomy and dignity. It is structurally distinct from the 'dependency_trap_reading' (which emphasizes work disincentives) and the 'targeting_efficiency_reading' (which prioritizes cost-effective poverty reduction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
