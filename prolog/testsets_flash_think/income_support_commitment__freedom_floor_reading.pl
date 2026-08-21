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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.12).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '91780f2a-e9aa-4d9a-87de-3d1d17c14291').
narrative_ontology:cs_kernel_codification('91780f2a-e9aa-4d9a-87de-3d1d17c14291', formalized).
narrative_ontology:cs_authority_grounding('91780f2a-e9aa-4d9a-87de-3d1d17c14291', practice).
narrative_ontology:cs_interpretation_layer_present('91780f2a-e9aa-4d9a-87de-3d1d17c14291').
narrative_ontology:cs_reading_relation('91780f2a-e9aa-4d9a-87de-3d1d17c14291', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('91780f2a-e9aa-4d9a-87de-3d1d17c14291', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('91780f2a-e9aa-4d9a-87de-3d1d17c14291', foundational, universal_human_dignity).
narrative_ontology:cs_axiom_status(universal_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('91780f2a-e9aa-4d9a-87de-3d1d17c14291', universal_human_dignity, deontological).
narrative_ontology:cs_axiom('91780f2a-e9aa-4d9a-87de-3d1d17c14291', foundational, economic_security_as_freedom).
narrative_ontology:cs_axiom_status(economic_security_as_freedom, holdable).
narrative_ontology:cs_axiom_grounding('91780f2a-e9aa-4d9a-87de-3d1d17c14291', economic_security_as_freedom, instrumental).
narrative_ontology:cs_reference_frame('91780f2a-e9aa-4d9a-87de-3d1d17c14291', universal_social_contract).
narrative_ontology:cs_drift_state('91780f2a-e9aa-4d9a-87de-3d1d17c14291', contemporary_political_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('91780f2a-e9aa-4d9a-87de-3d1d17c14291', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
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

% Receive unconditional income, enabling them to provide care without financial precarity, validating their work and offering genuine choice in labor market participation.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, mobile, national).

% Gain a safety net that reduces the pressure to accept exploitative labor conditions, increasing their bargaining power and capacity to seek better employment or education.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, mobile, national).

% Acquire the financial means to leave abusive situations, reducing economic dependency and enabling autonomy and safety.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, mobile, local).

% Are supported in pursuing creative or innovative endeavors that may not offer immediate financial returns, fostering cultural production and economic dynamism.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from a universal floor of economic security, reducing overall societal stress, improving public health, and fostering social cohesion. The collective benefit is a more resilient and equitable society.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    moderate, generational, analytical, national).

% Contribute to the income support system through taxes. They face a labor market with increased worker bargaining power and reduced desperation, potentially leading to higher wages and improved working conditions.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    powerful, biographical, constrained, national).

% Fund the system through general taxation. While bearing a direct financial cost, they also benefit indirectly from a more stable and equitable society, reduced social costs, and potentially a more dynamic economy.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Administers the unconditional income support program, managing distribution, ensuring compliance, and adapting policy. Its role shifts from means-testing and conditionality to efficient universal distribution.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for the implementation and maintenance of unconditional income support, monitoring its effects and engaging in public discourse to shape policy and public opinion.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, advocacy_groups, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure basic material security and enable individual autonomy across diverse life circumstances, reducing poverty and precarity by providing a universal income floor.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens, unconditionally, as a right, rather than a conditional benefit.
% ABSENT_VOICES: Those who believe in strict individual responsibility for economic outcomes, or those who prioritize means-tested efficiency over universality, are often excluded from the core policy design discussions of this reading.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, millions would lose their safety net, forcing them into exploitative labor, increasing poverty, and severely reducing individual autonomy and dignity. The labor market dynamics would shift back towards greater employer power and precarity.
% FOUNDING_PROBLEM: Widespread poverty, economic precarity, and lack of bargaining power for workers, leading to exploitation, limited individual freedom, and social instability.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers, labor unions, human rights organizations, and recipients of existing social assistance programs attest to the ongoing nature of economic precarity and the need for a stronger, more universal safety net. This corroboration comes from outside the direct beneficiaries of the proposed system.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.12, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_sustainability,
    'Is the proposed tax base sufficient and politically sustainable to fund unconditional income support at a level that genuinely enables autonomy and dignity?',
    'Long-term economic modeling, public finance analysis, and sustained political consensus building.',
    'If funding is unsustainable, the system could either collapse, be reduced to an insufficient level (becoming more theatrical), or require increased extraction from other sources, potentially shifting its classification towards a Tangled Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability, empirical, 'The long-term financial and political viability of the funding mechanism.').

omega_variable(
    labor_market_impact,
    'What are the actual long-term effects of unconditional income support on labor supply, wage levels, and the nature of work?',
    'Large-scale, longitudinal randomized control trials and comparative studies across jurisdictions implementing similar policies.',
    'If it leads to significant labor withdrawal without corresponding societal benefits, the ''dependency trap'' reading gains empirical support. If it fosters innovation, reduces exploitation, and improves well-being, this ''freedom floor'' reading is strengthened. This could shift the perceived extractiveness and coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact, empirical, 'Empirical effects on labor market participation and dynamics.').

omega_variable(
    dependency_vs_autonomy_framing,
    'Is the core effect of unconditional income support to foster genuine autonomy and dignity, or to create a new form of state dependence?',
    'Qualitative sociological studies on recipient experiences, measures of self-efficacy and life satisfaction, and analysis of exit options from both the labor market and the support system itself.',
    'If the ''dependency trap'' framing gains dominance, the constraint''s perceived coordination function would diminish, and its extractiveness (from taxpayers) might be reinterpreted as a Snare, rather than a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dependency_vs_autonomy_framing, conceptual, 'The fundamental conceptual framing of the policy''s impact on individuals.').

omega_variable(
    universality_vs_targeting_efficiency,
    'Is the universal nature of the support essential for its enabling function and dignity, or would a means-tested, targeted approach be more efficient and equally effective?',
    'Comparative policy analysis of universal vs. targeted programs, focusing on administrative costs, stigma effects, and overall poverty reduction outcomes.',
    'If targeted approaches are proven equally effective without significant negative externalities, the ''targeting efficiency'' reading gains strength, potentially leading to policy adjustments that could alter the constraint''s beneficiary structure and perceived coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_vs_targeting_efficiency, conceptual, 'The conceptual debate over universal vs. targeted social policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.03).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.03).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.03).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__freedom_floor_reading, suppression_requirement, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, employer_wage_setting_power).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, social_safety_net_fragmentation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, poverty_reduction_targets).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('freedom_floor_reading') of the broader 'income_support_commitment' kernel, which also includes 'dependency_trap_reading' and 'targeting_efficiency_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
