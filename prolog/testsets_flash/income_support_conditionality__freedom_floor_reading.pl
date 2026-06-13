% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint represents the 'freedom floor' reading of unconditional
 *   income support, where the policy is understood to decommodify labor power
 *   and enhance positive freedom. It is a reading of the
 *   'income_support_conditionality' kernel. In this reading, the constraint
 *   functions as a Rope, coordinating a higher baseline for labor conditions
 *   by empowering workers to refuse coercive employment. This contrasts with
 *   other readings that might view it as a dependency trap or an employer
 *   subsidy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '7341997a-3432-4d9e-8e8e-6a7e8da3d377').
narrative_ontology:cs_kernel_codification('7341997a-3432-4d9e-8e8e-6a7e8da3d377', formalized).
narrative_ontology:cs_authority_grounding('7341997a-3432-4d9e-8e8e-6a7e8da3d377', lineage).
narrative_ontology:cs_interpretation_layer_present('7341997a-3432-4d9e-8e8e-6a7e8da3d377').
narrative_ontology:cs_reading_relation('7341997a-3432-4d9e-8e8e-6a7e8da3d377', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7341997a-3432-4d9e-8e8e-6a7e8da3d377', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('7341997a-3432-4d9e-8e8e-6a7e8da3d377', foundational, labor_power_decommodification_is_good).
narrative_ontology:cs_axiom_status(labor_power_decommodification_is_good, holdable).
narrative_ontology:cs_axiom_grounding('7341997a-3432-4d9e-8e8e-6a7e8da3d377', labor_power_decommodification_is_good, deontological).
narrative_ontology:cs_axiom('7341997a-3432-4d9e-8e8e-6a7e8da3d377', foundational, economic_security_enhances_freedom).
narrative_ontology:cs_axiom_status(economic_security_enhances_freedom, holdable).
narrative_ontology:cs_axiom_grounding('7341997a-3432-4d9e-8e8e-6a7e8da3d377', economic_security_enhances_freedom, deontological).
narrative_ontology:cs_reference_frame('7341997a-3432-4d9e-8e8e-6a7e8da3d377', universal_basic_income_ideal).
narrative_ontology:cs_drift_state('7341997a-3432-4d9e-8e8e-6a7e8da3d377', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7341997a-3432-4d9e-8e8e-6a7e8da3d377', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, caregivers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a baseline income that allows them to refuse exploitative or unsafe work, improving their bargaining power and quality of life. This shifts their exit options from 'constrained' to 'mobile' relative to the pre-support state.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from income stability that reduces the immediate pressure to accept any available work, enabling them to seek better employment or pursue education/training. Their labor power is decommodified.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, mobile, national).

% Are supported in their essential, often unpaid, work, reducing financial strain and allowing them to prioritize care responsibilities without economic coercion. This recognizes the social value of their labor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, mobile, national).

% Lose the structural power to compel workers into undesirable jobs due to economic necessity. They face increased pressure to offer competitive wages and better working conditions, effectively 'paying' for the decommodification of labor power through higher labor costs or reduced access to cheap labor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercion, payer,
    powerful, immediate, constrained, local).

% Administer and fund the unconditional income support program, managing its implementation and public perception. They bear the political and fiscal costs of the program, but also gain social stability and reduced poverty-related externalities.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Fund the program through taxes. Their situation is complex, as some may also be beneficiaries (e.g., low-wage workers who pay taxes) or benefit indirectly from reduced social costs, while others may perceive it as a net cost without direct benefit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a baseline standard of living, ensuring all citizens have a floor below which they cannot fall, thereby enabling greater individual autonomy and reducing the collective action problem of poverty.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base (taxpayers) to all eligible citizens (low-wage workers, precarious workers, caregivers), creating a universal income floor.
% ABSENT_VOICES: Advocates for a 'work-first' approach, who believe that all income should be tied to labor market participation, are often marginalized in discussions about unconditional support. They would argue that such support disincentivizes work and fosters dependency.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage and precarious workers would immediately lose their freedom to refuse coercive work, reverting to a state of economic vulnerability. Employers reliant on cheap labor would regain their structural power, and social welfare systems would face increased strain, leading to significant societal reorganization.
% FOUNDING_PROBLEM: The problem of poverty, economic insecurity, and the coercive power dynamics inherent in labor markets where workers must accept any job to survive.
% FOUNDING_PROBLEM_CORROBORATION: Economists and social policy researchers, independent of government or employer interests, corroborate that poverty and labor market coercion remain live problems, and that unconditional income support directly addresses these issues by providing a freedom floor. Worker advocacy groups also attest to the ongoing need for such support.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the primary function is to provide a benefit (income) and reduce the extraction previously imposed by coercive labor markets. Suppression is also low (0.1) as the constraint's persistence relies on its broad acceptance as a social good, not on active coercion. Theater ratio is minimal (0.05) as the program directly delivers its stated function. The metrics reflect the intended and observed effects of a well-implemented unconditional income support program from this specific 'freedom floor' perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of workers, this constraint is a clear Rope, providing a vital coordination function for their collective well-being. From the perspective of employers reliant on coercive labor, it is an extractive force that raises their labor costs and reduces their power. The engine's computation will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage, precarious workers, and caregivers are clear beneficiaries (d near 0.0) as they gain economic security and bargaining power. Employers reliant on coercive labor practices are victims (d near 1.0) as they lose access to cheap, desperate labor. Government agencies are agenda-setters, balancing fiscal costs with social benefits. Taxpayers are payers, but their directionality is complex, potentially benefiting indirectly from reduced social costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercive_work_definition,
    'What constitutes ''coercive work'' in practice, and how is the freedom to refuse it measured?',
    'Empirical studies on worker satisfaction, reported instances of refusal, and changes in labor market conditions (e.g., wage growth in low-skill sectors).',
    'If ''coercive work'' is narrowly defined or difficult to refuse even with support, the constraint''s effectiveness as a ''freedom floor'' is reduced, potentially shifting its classification towards a more neutral or even slightly extractive type if the support merely enables survival in poor conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_work_definition, empirical, 'Ambiguity in defining and measuring the ''freedom to refuse coercive work''.').

omega_variable(
    dependency_trap_vs_freedom_floor,
    'Is unconditional income support primarily creating a ''freedom floor'' or a ''dependency trap''?',
    'Longitudinal studies tracking labor force participation, skill development, and well-being outcomes for recipients compared to control groups. This would empirically distinguish the ''freedom_floor_reading'' from the ''dependency_trap_reading''.',
    'If evidence strongly supports a ''dependency trap,'' this reading''s classification as a Rope would be challenged, potentially shifting towards a Snare or Tangled Rope if the support is found to disempower rather than empower workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dependency_trap_vs_freedom_floor, empirical, 'Distinguishing between positive freedom and dependency outcomes.').

omega_variable(
    wage_suppression_vs_worker_power,
    'Does unconditional income support primarily enhance worker bargaining power or function as an employer wage subsidy?',
    'Economic analysis of wage trends in sectors employing beneficiaries, combined with studies on employer hiring practices and worker negotiation outcomes. This would empirically distinguish the ''freedom_floor_reading'' from the ''wage_subsidy_reading''.',
    'If evidence suggests it primarily acts as a wage subsidy, this reading''s classification as a Rope would be challenged, potentially shifting towards a Tangled Rope or Snare if employers are found to capture the benefits through suppressed wages.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_suppression_vs_worker_power, empirical, 'Distinguishing between worker empowerment and employer subsidy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'income_support_conditionality' kernel. This 'freedom_floor_reading' focuses on the decommodification of labor and enhanced worker autonomy, contrasting with the 'dependency_trap_reading' and 'wage_subsidy_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
