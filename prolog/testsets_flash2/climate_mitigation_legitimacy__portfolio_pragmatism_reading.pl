% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Decarbonization Portfolio
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio pragmatism' reading of climate
 *   mitigation legitimacy, asserting that optimal decarbonization requires a
 *   technology-neutral approach, including both nuclear and renewables. It is
 *   a response to the urgent need for climate action, seeking to avoid
 *   ideological lock-in to specific technologies. The constraint functions as
 *   a 'rope' by coordinating diverse energy stakeholders around a flexible,
 *   outcome-oriented strategy, minimizing extraction by not privileging any
 *   single technology or industry a priori.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.25).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.3).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Decarbonization Portfolio").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '5b1260d9-a2bd-4cd2-a054-1504401bc9dc').
narrative_ontology:cs_kernel_codification('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', distributed).
narrative_ontology:cs_authority_grounding('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', expertise).
narrative_ontology:cs_interpretation_layer_present('5b1260d9-a2bd-4cd2-a054-1504401bc9dc').
narrative_ontology:cs_reading_relation('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', foundational, decarbonization_requires_all_available_low_carbon_options).
narrative_ontology:cs_axiom_status(decarbonization_requires_all_available_low_carbon_options, holdable).
narrative_ontology:cs_axiom_grounding('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', decarbonization_requires_all_available_low_carbon_options, instrumental).
narrative_ontology:cs_axiom('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', secondary, regional_optimization_is_key_to_cost_effectiveness).
narrative_ontology:cs_axiom_status(regional_optimization_is_key_to_cost_effectiveness, holdable).
narrative_ontology:cs_axiom_grounding('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', regional_optimization_is_key_to_cost_effectiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', pragmatic_decarbonization_consensus).
narrative_ontology:cs_drift_state('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', contemporary_energy_transition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5b1260d9-a2bd-4cd2-a054-1504401bc9dc', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_system_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements energy policies that consider all low-carbon technologies (nuclear, solar, wind, hydro, geothermal) based on cost, reliability, and regional suitability, without ideological preference. Benefits from a flexible mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_system_planners, agenda_setter,
    institutional, generational, constrained, national).

% Supports policies that prioritize effective decarbonization outcomes over specific technology preferences, seeing a diverse portfolio as the most pragmatic path to climate goals. Benefits from policy flexibility.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_advocates, beneficiary,
    organized, generational, mobile, global).

% Benefits from inclusion in decarbonization plans, allowing for new projects and continued operation of existing plants. Faces high capital costs and regulatory hurdles, but this reading provides a pathway for investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, global).

% Benefits from continued deployment of solar, wind, and other renewable projects, but accepts that nuclear may also play a role. Seeks policy stability and market access.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_developers, beneficiary,
    powerful, biographical, mobile, global).

% Faces pressure to decarbonize and transition away from fossil fuels, incurring costs for stranded assets or new investments in low-carbon alternatives. This reading accelerates their decline.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents, payer,
    institutional, biographical, constrained, global).

% Argues that any large-scale energy expansion, regardless of source, is unsustainable and that decarbonization must primarily come from demand reduction. Their perspective is largely outside the mainstream policy debate this constraint addresses.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_advocates, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse energy stakeholders and technologies towards the common goal of rapid and effective decarbonization by allowing for flexible, regionally optimized energy portfolios.
% TRANSFER_FUNCTION: Transfers investment and policy support to a broad range of low-carbon technologies, potentially shifting capital away from fossil fuels and towards both nuclear and renewables, based on pragmatic assessments.
% ABSENT_VOICES: Degrowth advocates, who would argue that the focus on technology portfolios misses the fundamental need for demand reduction and systemic change, are largely excluded from the policy-making process that this constraint informs.
% DISAPPEARANCE_RATIONALE: If the principle of technology-neutral portfolio pragmatism vanished, energy policy would likely fragment into ideologically driven camps (e.g., 'renewables-only' vs. 'nuclear-only'), leading to less efficient and slower decarbonization, and potentially higher costs or reliability issues. Investment signals would become less clear, and the overall climate mitigation effort would suffer.
% FOUNDING_PROBLEM: The urgent need for deep decarbonization of global energy systems while maintaining reliability and economic stability, in the face of diverse regional energy needs and technological maturity.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies (IEA, IPCC), intergovernmental climate bodies, and a broad consensus among energy economists and engineers attest to the ongoing urgency of decarbonization and the need for pragmatic, flexible approaches. This corroboration comes from outside specific technology lobbies.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is low because this reading aims for efficiency and flexibility, minimizing rents to any single technology. Suppression (0.3) is also low, as it primarily involves coordinating existing actors rather than coercing new behaviors or suppressing alternatives. Theater ratio (0.1) is minimal, reflecting a genuine focus on practical outcomes. The time series shows slight fluctuations but overall stability, indicating a relatively consistent application of this pragmatic approach over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of energy system planners and climate advocates, this is a highly effective and fair approach. However, from the perspective of 'renewables-only' or 'nuclear-only' advocates (represented by sibling readings), this approach might be seen as compromising or inefficient, leading to different perceived levels of 'extraction' or 'suppression' of their preferred path. The engine's per-seat classification would capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Energy system planners and climate mitigation advocates are beneficiaries, as this reading aligns with their goals of flexible, effective decarbonization. The nuclear and renewable industries are also beneficiaries, as they are included in the portfolio. Fossil fuel incumbents are payers, as this reading accelerates their phase-out. Degrowth advocates are excluded, as their fundamental premise of demand reduction is not central to this technology-focused portfolio approach.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by maintaining flexibility and focusing on outcomes. Its mandate is to achieve decarbonization efficiently. If it were to become overly prescriptive or favor one technology without justification, it would drift towards a 'snare' or 'tangled_rope' by extracting rents for a specific industry. The current low extractiveness and suppression suggest it is fulfilling its coordination mandate without significant capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_technology_neutrality,
    'Is the ''technology-neutral'' approach genuinely neutral, or does it implicitly favor certain technologies (e.g., those with established lobbying power or existing infrastructure)?',
    'Detailed analysis of policy implementation, funding allocations, and regulatory hurdles across different technologies over time, comparing stated neutrality with actual outcomes and investment patterns.',
    'If implicit bias is found, the effective extractiveness would be higher for disfavored technologies, and the constraint might reclassify towards a ''tangled_rope'' or ''snare'' for those specific actors, as the coordination story would be revealed as partial cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_technology_neutrality, empirical, 'Assesses whether the claimed technology neutrality holds in practice.').

omega_variable(
    long_term_cost_optimization,
    'Does a technology-neutral portfolio truly lead to the lowest long-term system costs and fastest decarbonization, or would a more focused approach (e.g., ''renewables-only'') be superior under certain future conditions?',
    'Long-term energy system modeling with updated cost projections, technological advancements, and climate targets, comparing diverse portfolio outcomes against more specialized strategies.',
    'If a more focused approach is demonstrably superior, the ''portfolio pragmatism'' reading''s legitimacy could erode, leading to increased resistance and potential reclassification as a ''piton'' if maintained by inertia, or a ''snare'' if powerful actors benefit from its sub-optimality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_cost_optimization, empirical, 'Evaluates the long-term efficacy and cost-effectiveness of the portfolio approach.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is the ''portfolio_pragmatism_reading'' of the ''climate_mitigation_legitimacy'' kernel. How distinct are its structural implications from sibling readings like ''baseload_necessity_reading'' or ''renewable_primacy_reading''?',
    'Comparative analysis of policy outcomes and investment patterns in regions adopting different primary readings. If policy outcomes converge despite different stated readings, the structural distinction is weaker than claimed.',
    'If structural implications are not sufficiently distinct, the kernel decomposition might be over-specified, suggesting that the different readings are more ''conceptual'' than ''empirical'' in their divergence, potentially leading to a re-evaluation of the kernel''s boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Documents the structural distinctiveness of this kernel reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2025, 0.09).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2035, 0.09).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2040, 0.09).
narrative_ontology:measurement(clim_tr_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2045, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2020, 0.2).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2025, 0.22).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2030, 0.25).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2035, 0.24).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2040, 0.23).
narrative_ontology:measurement(clim_be_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2045, 0.24).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2050, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2030, 0.3).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2035, 0.29).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2040, 0.28).
narrative_ontology:measurement(clim_su_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2045, 0.29).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2050, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('portfolio_pragmatism_reading') of the 'climate_mitigation_legitimacy' kernel. Its ε value reflects the pragmatic, technology-neutral approach, which differs significantly from the more prescriptive 'baseload_necessity_reading' (higher extractiveness for renewables) or 'renewable_primacy_reading' (higher extractiveness for nuclear) and the demand-focused 'degrowth_sufficiency_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
