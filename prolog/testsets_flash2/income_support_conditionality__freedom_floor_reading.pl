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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom floor' reading of
 *   unconditional income support. In this reading, the policy functions as a
 *   genuine Rope, decommodifying labor and providing positive freedom. It is
 *   characterized by low extractiveness and suppression, as it aims to reduce
 *   coercive pressures rather than impose them. The primary beneficiaries are
 *   low-wage and precarious workers, while employers who rely on coercive
 *   labor practices become the 'payers' by losing their structural advantage.
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
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '6652bd32-44fd-479f-aa55-08974f591305').
narrative_ontology:cs_kernel_codification('6652bd32-44fd-479f-aa55-08974f591305', formalized).
narrative_ontology:cs_authority_grounding('6652bd32-44fd-479f-aa55-08974f591305', practice).
narrative_ontology:cs_interpretation_layer_present('6652bd32-44fd-479f-aa55-08974f591305').
narrative_ontology:cs_reading_relation('6652bd32-44fd-479f-aa55-08974f591305', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6652bd32-44fd-479f-aa55-08974f591305', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('6652bd32-44fd-479f-aa55-08974f591305', foundational, labor_power_decommodification_is_freedom).
narrative_ontology:cs_axiom_status(labor_power_decommodification_is_freedom, holdable).
narrative_ontology:cs_axiom_grounding('6652bd32-44fd-479f-aa55-08974f591305', labor_power_decommodification_is_freedom, deontological).
narrative_ontology:cs_axiom('6652bd32-44fd-479f-aa55-08974f591305', secondary, basic_income_enhances_worker_bargaining_power).
narrative_ontology:cs_axiom_status(basic_income_enhances_worker_bargaining_power, holdable).
narrative_ontology:cs_axiom_grounding('6652bd32-44fd-479f-aa55-08974f591305', basic_income_enhances_worker_bargaining_power, empirically_contingent).
narrative_ontology:cs_reference_frame('6652bd32-44fd-479f-aa55-08974f591305', post_scarcity_social_contract).
narrative_ontology:cs_drift_state('6652bd32-44fd-479f-aa55-08974f591305', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6652bd32-44fd-479f-aa55-08974f591305', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, caregivers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a baseline income that provides a floor below which they cannot fall, enabling them to refuse exploitative or unsafe work conditions without immediate destitution. This increases their bargaining power and freedom.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from the stability and reduced precarity, allowing them to seek more stable or fulfilling employment, pursue education, or engage in care work without constant fear of income loss. Their ability to exit poor jobs is enhanced.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, immediate, mobile, national).

% Are supported in their essential, often unpaid, work, recognizing its social value and reducing the pressure to enter paid employment out of necessity. This allows for greater autonomy in family and community roles.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, mobile, national).

% Lose their structural power to compel workers into undesirable jobs through the threat of destitution. They must now offer more attractive wages and conditions to retain labor, increasing their labor costs. Their ability to extract surplus value from desperate workers is diminished.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercion, payer,
    powerful, immediate, constrained, national).

% Champion the implementation and expansion of unconditional income support, framing it as a fundamental human right and a tool for social liberation. They actively work to shape policy and public opinion.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, advocacy_groups_for_basic_income, agenda_setter,
    organized, generational, analytical, national).

% Would argue against unconditional income support on grounds of fiscal unsustainability and disincentives to work, but their arguments are structurally sidelined in this reading which prioritizes freedom and decommodification over traditional economic efficiency metrics.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, fiscal_conservatives, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal agreement that no individual should fall below a basic income floor, enabling individuals to coordinate their labor decisions based on genuine preference rather than coercive necessity, and fostering a more equitable distribution of social goods.
% TRANSFER_FUNCTION: Transfers financial resources from the state (funded by general taxation, implicitly from higher earners and capital) to all citizens or residents, creating a baseline income floor.
% ABSENT_VOICES: Fiscal conservatives and employers who benefit from a coercive labor market are structurally excluded from the framing of this reading; they would argue that such a system is economically unsustainable and creates dependency, but their concerns are not central to the 'freedom floor' perspective.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, low-wage and precarious workers would immediately lose their freedom to refuse coercive work, reverting to a state of heightened precarity and reduced bargaining power. The labor market dynamics would shift back towards employer dominance, and social welfare would decline.
% FOUNDING_PROBLEM: The problem of coercive labor markets where individuals are forced to accept exploitative work due to the threat of destitution, leading to a lack of genuine freedom and dignity.
% FOUNDING_PROBLEM_CORROBORATION: Advocacy groups, labor unions, and social justice organizations attest that the problem of coercive labor persists, citing ongoing issues with low wages, poor working conditions, and lack of worker power. Academic research on labor market precarity and worker exploitation from outside the benefiting parties corroborates this.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) reflects that the system is designed to provide a net benefit to its primary beneficiaries, with minimal overhead. Suppression (0.1) is low because the constraint's purpose is to reduce, not impose, coercive pressure on workers. The theater ratio (0.05) is negligible, as the policy's stated goal of providing a freedom floor aligns directly with its operational effects. Accessibility collapse is low (0.2) because it expands, rather than restricts, options for workers. Resistance is low (0.05) from the perspective of beneficiaries, as the policy is welcomed; resistance from employers is framed as a loss of coercive power, not a 'resistance' to the constraint's coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage workers, this is a clear Rope, providing a vital safety net and increasing autonomy. From the perspective of employers reliant on coercive labor, it is a Snare that extracts their traditional power and increases their costs. The engine's classification for each seat will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage and precarious workers are full beneficiaries (d near 0.0) as the policy directly subsidizes their freedom and improves their bargaining position. Employers reliant on coercive labor are targets (d near 1.0) as the policy extracts their ability to compel cheap labor. Caregivers are also beneficiaries, as their unpaid labor is implicitly supported. Advocacy groups are agenda-setters, pushing for the policy's implementation. Fiscal conservatives are excluded, as their concerns are not central to this reading's value framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_impact_on_labor_supply,
    'Does unconditional income support significantly reduce labor supply for essential services, or does it primarily shift labor towards more fulfilling work?',
    'Longitudinal empirical studies tracking labor force participation rates and job quality changes in regions with unconditional income support programs.',
    'If labor supply for essential services significantly declines, the ''freedom floor'' reading''s positive coordination function might be offset by new coordination problems, potentially shifting its classification towards a Tangled Rope or even a Snare if the social costs outweigh individual benefits. If labor shifts to better work, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_impact_on_labor_supply, empirical, 'Uncertainty regarding the actual impact of unconditional income support on overall labor supply and the quality of work.').

omega_variable(
    fiscal_sustainability_vs_social_benefit,
    'Is the fiscal cost of unconditional income support sustainable in the long term, and how does it balance against the social benefits of increased freedom and reduced precarity?',
    'Comprehensive macroeconomic modeling and public finance analysis, alongside social impact assessments, to evaluate long-term fiscal implications and non-monetary social returns.',
    'If fiscally unsustainable, the constraint''s long-term viability is questionable, potentially leading to its collapse or a reclassification as a Scaffold (temporary support). If sustainable and benefits are high, the Rope classification is reinforced. This is a key point of contention with the ''dependency_trap_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_vs_social_benefit, preference, 'The trade-off between the fiscal burden and the social benefits of unconditional income support.').

omega_variable(
    coercion_definition_ambiguity,
    'Is ''coercive work'' defined solely by the absence of a basic income floor, or do other structural factors (e.g., discrimination, lack of skills, geographic isolation) also constitute coercion that unconditional income support alone cannot resolve?',
    'Qualitative sociological research and policy analysis examining the lived experiences of workers and the multi-faceted nature of labor market constraints beyond income.',
    'If coercion is multi-faceted, the ''freedom floor'' reading might overstate the policy''s ability to fully decommodify labor, suggesting a more limited coordination function and potentially higher residual extraction from other sources, pushing it towards a Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Ambiguity in the definition and scope of ''coercive work'' and the policy''s ability to address all forms of coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_conditionality' kernel, focusing on the decommodification of labor and creation of positive freedom. It is structurally distinct from the 'dependency_trap_reading' and 'wage_subsidy_reading' of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
