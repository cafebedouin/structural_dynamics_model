% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy: Quantitative Growth Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'd4be1b61-397f-4b05-b65f-6222780c7dbb').
narrative_ontology:cs_kernel_codification('d4be1b61-397f-4b05-b65f-6222780c7dbb', formalized).
narrative_ontology:cs_authority_grounding('d4be1b61-397f-4b05-b65f-6222780c7dbb', extraction).
narrative_ontology:cs_interpretation_layer_present('d4be1b61-397f-4b05-b65f-6222780c7dbb').
narrative_ontology:cs_reading_relation('d4be1b61-397f-4b05-b65f-6222780c7dbb', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('d4be1b61-397f-4b05-b65f-6222780c7dbb', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4be1b61-397f-4b05-b65f-6222780c7dbb', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('d4be1b61-397f-4b05-b65f-6222780c7dbb', foundational, gdp_growth_is_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_is_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('d4be1b61-397f-4b05-b65f-6222780c7dbb', gdp_growth_is_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('d4be1b61-397f-4b05-b65f-6222780c7dbb', secondary, investment_driven_development_is_optimal).
narrative_ontology:cs_axiom_status(investment_driven_development_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('d4be1b61-397f-4b05-b65f-6222780c7dbb', investment_driven_development_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('d4be1b61-397f-4b05-b65f-6222780c7dbb', post_reform_era_rapid_industrialization).
narrative_ontology:cs_drift_state('d4be1b61-397f-4b05-b65f-6222780c7dbb', contemporary_overcapacity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4be1b61-397f-4b05-b65f-6222780c7dbb', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, international_investors).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, overcapacity_industry_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority whose legitimacy is directly tied to delivering high GDP growth rates. They set national economic targets, direct investment, and enforce policies that prioritize quantitative expansion. They benefit from the stability and perceived success that high growth provides.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_leadership, agenda_setter,
    institutional, civilizational, constrained, national).

% Large state-owned and private enterprises in manufacturing and export sectors that receive preferential policies, subsidies, and access to capital to drive production and exports, directly contributing to GDP figures. They profit significantly from this growth model.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, generational, arbitrage, global).

% Officials whose career advancement and performance evaluations are heavily dependent on achieving local GDP growth targets. They actively promote investment, often at the expense of environmental or social considerations, to meet these metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, constrained, regional).

% Foreign entities that invest in the rapidly growing economy, benefiting from high returns, access to large markets, and often favorable regulatory environments designed to attract capital for growth. They can easily shift capital if growth falters.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, international_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Groups and individuals who bear the costs of environmental degradation (pollution, resource depletion) resulting from unchecked industrial expansion. Their concerns are often marginalized or suppressed in favor of economic growth targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_advocates, payer,
    powerless, generational, trapped, national).

% Workers in sectors that experience boom-and-bust cycles due to over-investment driven by growth targets. They face job insecurity, wage stagnation, and difficult transitions when industries are restructured or capacity is cut, bearing the human cost of inefficient growth.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overcapacity_industry_workers, payer,
    moderate, biographical, constrained, local).

% Will inherit the long-term environmental and social costs of current growth-at-all-costs policies, including climate change impacts, depleted natural resources, and accumulated debt. They have no voice in current policy decisions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Technocrats and policy experts tasked with designing and implementing economic strategies to achieve GDP growth targets. While they may understand the limitations of raw growth, their mandate is to deliver it, and their careers depend on demonstrating success within this framework.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, economic_planners, agenda_setter,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, state_leadership).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national economic activity, investment, and policy towards achieving specific, high GDP growth rates, thereby ensuring economic stability, job creation, and national strength.
% TRANSFER_FUNCTION: Transfers resources, policy priority, and political capital towards investment-heavy, export-oriented industries and infrastructure projects, often at the expense of environmental protection, social welfare, and balanced regional development. It moves legitimacy from other sources to the state based on economic performance.
% ABSENT_VOICES: Environmental groups, advocates for sustainable development, and communities directly impacted by pollution or forced displacement are often excluded from policy-making. Their perspectives, which would challenge the primacy of raw GDP growth, are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If the legitimacy derived from quantitative GDP growth vanished overnight, the entire political-economic system would face a profound crisis. State leadership would lose its primary justification for power, investment flows would halt, and local officials would lose their performance metrics. This would necessitate a complete overhaul of governance, economic planning, and societal values, leading to widespread instability and reorganization.
% FOUNDING_PROBLEM: To rapidly industrialize, lift large populations out of poverty, and establish national economic strength and international standing in a competitive global environment, particularly after periods of underdevelopment or conflict.
% FOUNDING_PROBLEM_CORROBORATION: State leadership and official media consistently assert that the founding problem (e.g., poverty, national strength) is still live and requires continued high growth. However, independent economists, environmental organizations, and social critics argue that the initial problem has been largely addressed, and the continued singular focus on raw growth now generates new, severe problems (e.g., inequality, environmental crisis, overcapacity), corroborating the 'dead' status for the original problem while acknowledging new challenges.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''quantitative_growth_reading'' of the ''performance_legitimacy'' kernel, or does it conflate with other readings?',
    'Comparison with other generated readings of the ''performance_legitimacy'' kernel and expert review of the specific policy mechanisms described.',
    'If conflated, the classification may be inaccurate, and the structural relationships to sibling readings would be misidentified. A clear distinction ensures accurate kernel-level analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensures this story is a pure reading of the specified kernel.').

omega_variable(
    growth_quality_vs_quantity_ambiguity,
    'To what extent is the state''s focus on raw GDP growth a genuine proxy for societal well-being, versus a convenient, easily measurable metric that masks underlying issues?',
    'Longitudinal studies correlating GDP growth with metrics of social welfare, environmental health, and citizen satisfaction, independent of official reporting.',
    'If raw GDP is a poor proxy, the constraint''s coordination function is weaker than claimed, and its extractiveness is higher, pushing it closer to a Snare. If it remains a strong proxy, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_quality_vs_quantity_ambiguity, empirical, 'Assesses the validity of GDP as a measure of societal benefit.').

omega_variable(
    environmental_cost_internalization,
    'Are the environmental costs of this growth model being adequately internalized and accounted for in economic planning, or are they externalized to the public and future generations?',
    'Implementation and enforcement of robust environmental accounting standards, carbon pricing, and pollution taxes that reflect true social costs.',
    'If costs are externalized, the true extractiveness of the constraint is higher than measured, and the ''payer'' role of environmental advocates is amplified. Internalization would reduce effective extraction and potentially shift the constraint towards a more balanced Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_cost_internalization, empirical, 'Examines the accounting for environmental externalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1980, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(perf_tr_t1990, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t1980, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(perf_be_t1990, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1980, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(perf_su_t1990, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, export_dependency).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, state_owned_enterprise_subsidies).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'quantitative_growth_reading' of the 'performance_legitimacy' kernel. It is structurally distinct from the 'qualitative_development_reading', 'techno_nationalist_reading', and 'livelihood_security_reading' due to differing primary metrics and beneficiary structures, but all are linked as part of the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
