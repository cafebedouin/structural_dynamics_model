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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Optimal Decarbonization Requires Technology-Neutral Portfolio (Portfolio Pragmatism Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio pragmatism' reading of climate
 *   mitigation legitimacy, asserting that optimal decarbonization requires a
 *   technology-neutral approach, including both nuclear and renewables. It
 *   emphasizes flexibility, cost-effectiveness, and reliability over
 *   ideological purity. This reading is one of several competing
 *   interpretations of how to legitimately achieve climate goals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.3).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.2).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Optimal Decarbonization Requires Technology-Neutral Portfolio (Portfolio Pragmatism Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '81c0ded4-8af0-44ed-830e-410fddd9ab8a').
narrative_ontology:cs_kernel_codification('81c0ded4-8af0-44ed-830e-410fddd9ab8a', distributed).
narrative_ontology:cs_authority_grounding('81c0ded4-8af0-44ed-830e-410fddd9ab8a', expertise).
narrative_ontology:cs_interpretation_layer_present('81c0ded4-8af0-44ed-830e-410fddd9ab8a').
narrative_ontology:cs_reading_relation('81c0ded4-8af0-44ed-830e-410fddd9ab8a', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('81c0ded4-8af0-44ed-830e-410fddd9ab8a', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('81c0ded4-8af0-44ed-830e-410fddd9ab8a', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('81c0ded4-8af0-44ed-830e-410fddd9ab8a', foundational, technology_agnosticism_is_optimal).
narrative_ontology:cs_axiom_status(technology_agnosticism_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('81c0ded4-8af0-44ed-830e-410fddd9ab8a', technology_agnosticism_is_optimal, empirically_contingent).
narrative_ontology:cs_axiom('81c0ded4-8af0-44ed-830e-410fddd9ab8a', secondary, diversified_portfolio_reduces_risk).
narrative_ontology:cs_axiom_status(diversified_portfolio_reduces_risk, holdable).
narrative_ontology:cs_axiom_grounding('81c0ded4-8af0-44ed-830e-410fddd9ab8a', diversified_portfolio_reduces_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('81c0ded4-8af0-44ed-830e-410fddd9ab8a', evidence_based_policy_making).
narrative_ontology:cs_drift_state('81c0ded4-8af0-44ed-830e-410fddd9ab8a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('81c0ded4-8af0-44ed-830e-410fddd9ab8a', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement energy policies that prioritize cost-effectiveness and reliability in decarbonization, without ideological preference for specific technologies. They benefit from a flexible policy toolkit.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_planners, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from policies that include nuclear power as a viable option for decarbonization, ensuring continued investment and operational opportunities. Without this constraint, they face exclusion from climate portfolios.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, national).

% Benefits from policies that include renewables as a primary option for decarbonization, ensuring market access and investment. This reading supports their inclusion but does not grant them exclusive priority.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a diversified energy portfolio that aims for stable, affordable, and reliable power supply during the energy transition. They bear the costs of policy choices but also reap the benefits of successful decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_consumers, beneficiary,
    powerless, immediate, trapped, national).

% Provide scientific and economic analysis supporting a technology-neutral approach to decarbonization, emphasizing empirical evidence over ideological commitments. Their influence is through informing policy debates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_agnostic_researchers, observer,
    analytical, generational, analytical, global).

% Would object to the inclusion of nuclear power in any decarbonization portfolio due to safety, waste, and proliferation concerns. This reading's premise of technology neutrality marginalizes their position.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, anti_nuclear_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse energy technologies and policy approaches to achieve the most effective and efficient decarbonization pathway, balancing cost, reliability, and speed across different regional contexts.
% TRANSFER_FUNCTION: Directs capital investment and policy support towards a balanced mix of nuclear and renewable energy projects, shifting resources from ideologically preferred but suboptimal solutions to pragmatically chosen ones.
% ABSENT_VOICES: Advocates for exclusive reliance on either renewables or nuclear, or for degrowth strategies, are marginalized by this reading's emphasis on a balanced, technology-neutral portfolio. They would argue for their preferred, singular solution.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, energy policy would likely revert to more ideologically driven or single-technology-focused approaches, leading to less optimal, slower, or more expensive decarbonization efforts. Investment flows would shift dramatically.
% FOUNDING_PROBLEM: The challenge of achieving rapid, reliable, and cost-effective decarbonization at scale, given the limitations and strengths of individual energy technologies and the urgency of climate change.
% FOUNDING_PROBLEM_CORROBORATION: International energy agencies, intergovernmental climate bodies, and independent engineering firms consistently corroborate the ongoing challenge of balancing decarbonization goals with energy security and economic viability, supporting the need for a flexible, technology-neutral approach.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it genuinely coordinates diverse technological and policy interests towards a common goal (decarbonization) with relatively low extraction and suppression. Extraction is low (0.3) as it aims for optimal societal benefit, not rent-seeking. Suppression is also low (0.2) because it seeks to integrate, rather than exclude, viable technologies, though it does suppress purely ideological positions. Theater ratio is minimal (0.1) as its justification is directly tied to its function.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for broad coordination, those advocating for single-technology solutions (e.g., 'renewable primacy' or 'baseload necessity') or demand-side solutions ('degrowth sufficiency') will perceive this constraint as suppressing their preferred approach, even if it benefits the overall system. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate mitigation planners are agenda-setters, benefiting from a flexible policy framework. Both nuclear and renewable industries are beneficiaries, as this reading ensures their inclusion in the portfolio. Energy consumers are also beneficiaries, gaining from a more stable and cost-effective energy transition. Technology-agnostic researchers act as observers, while anti-nuclear advocates are excluded by the premise of technology neutrality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_vs_bias,
    'Is the ''technology-neutral'' approach genuinely unbiased, or does it implicitly favor certain technologies (e.g., large-scale capital projects) due to existing institutional structures?',
    'Detailed analysis of capital allocation patterns and policy incentives under this framework, comparing actual investment outcomes against stated neutrality goals.',
    'If a hidden bias is revealed, the constraint''s effective extractiveness and suppression might be higher for disfavored technologies, potentially reclassifying it as a Tangled Rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_vs_bias, empirical, 'Assessing the true neutrality of the portfolio approach.').

omega_variable(
    optimal_mix_definition,
    'How is ''optimal'' defined in this context, and whose interests does that definition primarily serve (e.g., lowest cost, fastest deployment, highest reliability, lowest social impact)?',
    'Stakeholder analysis of the criteria used for ''optimal'' and their weighting in policy decisions, alongside public discourse analysis.',
    'If ''optimal'' is found to disproportionately serve specific industrial or political interests, the constraint''s beneficiary structure would be clarified, potentially increasing its measured extractiveness for other groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_mix_definition, conceptual, 'Clarifying the definition of ''optimal'' decarbonization.').

omega_variable(
    kernel_reading_structural_delta,
    'What specific structural elements would change if a sibling reading (e.g., ''renewable primacy'') were adopted instead of ''portfolio pragmatism''?',
    'Comparative policy analysis: modeling the impact on investment, grid structure, and regulatory frameworks under different dominant readings.',
    'A shift to ''renewable primacy'' would likely increase suppression for the nuclear industry and potentially increase extractiveness for consumers if grid stability costs rise. A shift to ''baseload necessity'' would reverse these effects. This omega documents the structural delta between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural changes under alternative kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2030, 0.09).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2030, 0.28).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2040, 0.29).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2050, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2030, 0.18).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2040, 0.19).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2050, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is the 'portfolio pragmatism' reading of the 'climate mitigation legitimacy' kernel, which also includes 'baseload_necessity_reading', 'renewable_primacy_reading', and 'degrowth_sufficiency_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
