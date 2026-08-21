% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative: Opportunity Cost Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'opportunity cost' reading of the climate
 *   mitigation imperative, asserting that nuclear energy's high capital
 *   intensity and long deployment timelines make it a net-harmful diversion
 *   of resources from faster, cheaper decarbonization strategies. This
 *   framing positions nuclear as an inefficient choice for rapid climate
 *   action, leading to its exclusion from preferred policy and investment
 *   portfolios. The constraint is claimed as a Rope by its proponents (a
 *   necessary coordination for effective climate action) but operates as a
 *   Snare by diverting resources and suppressing a viable, albeit slower,
 *   energy option.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.8).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, snare).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative: Opportunity Cost Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '3d545664-34fa-487c-b6a5-534baffe6e73').
narrative_ontology:cs_kernel_codification('3d545664-34fa-487c-b6a5-534baffe6e73', implicit).
narrative_ontology:cs_authority_grounding('3d545664-34fa-487c-b6a5-534baffe6e73', expertise).
narrative_ontology:cs_reading_relation('3d545664-34fa-487c-b6a5-534baffe6e73', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d545664-34fa-487c-b6a5-534baffe6e73', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('3d545664-34fa-487c-b6a5-534baffe6e73', foundational, carbon_reduction_speed_is_paramount).
narrative_ontology:cs_axiom_status(carbon_reduction_speed_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('3d545664-34fa-487c-b6a5-534baffe6e73', carbon_reduction_speed_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('3d545664-34fa-487c-b6a5-534baffe6e73', foundational, capital_intensity_is_a_bottleneck).
narrative_ontology:cs_axiom_status(capital_intensity_is_a_bottleneck, holdable).
narrative_ontology:cs_axiom_grounding('3d545664-34fa-487c-b6a5-534baffe6e73', capital_intensity_is_a_bottleneck, empirically_contingent).
narrative_ontology:cs_reference_frame('3d545664-34fa-487c-b6a5-534baffe6e73', optimal_resource_allocation_for_rapid_decarbonization).
narrative_ontology:cs_drift_state('3d545664-34fa-487c-b6a5-534baffe6e73', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3d545664-34fa-487c-b6a5-534baffe6e73', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_action_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, long_term_energy_planners).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, cost_effectiveness_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, rapid_decarbonization_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups and individuals advocating for aggressive climate action, prioritizing solutions that deliver the fastest and most cost-effective carbon reductions. They benefit from the framing that directs resources to their preferred solutions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_action_advocates, beneficiary,
    organized, biographical, mobile, global).

% Companies and consortia developing solar, wind, and other renewable energy technologies. They directly benefit from policies and investment flows guided by the 'fastest deployment per dollar' principle, as their technologies often fit this criterion.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Companies, researchers, and workers involved in the design, construction, and operation of nuclear power plants. They bear the cost of capital diversion, negative public perception, and policy exclusion driven by the opportunity cost argument. Their long-term projects are particularly vulnerable.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Government agencies, think tanks, and utilities responsible for ensuring stable, secure, and sustainable energy supplies over decades. They may see nuclear as a valuable component for baseload power or energy independence, but face pressure to conform to the 'fastest per dollar' metric, leading to underinvestment in nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, long_term_energy_planners, payer,
    institutional, civilizational, constrained, national).

% Companies and industries reliant on fossil fuels. While not directly targeted by this constraint, they indirectly benefit from any internal conflict or slowed progress within the low-carbon energy sector, as it prolongs their market dominance. They are excluded from the core debate of low-carbon strategy.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_fuel_incumbents, excluded,
    institutional, generational, arbitrage, global).

% Independent researchers and institutions specializing in energy economics and climate policy. They provide data and models on the cost-effectiveness, deployment speed, and system integration challenges of various energy technologies, influencing the debate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, analytical_economists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate climate mitigation efforts towards the most economically efficient and rapid deployment of carbon-reducing technologies, by prioritizing solutions with the fastest deployment per dollar.
% TRANSFER_FUNCTION: Transfers capital and policy focus away from nuclear energy development towards renewable energy and other faster-deploying, lower-capital-intensity solutions, based on a cost-effectiveness metric.
% ABSENT_VOICES: Future generations (who bear the ultimate cost of delayed or inefficient mitigation, but whose long-term energy needs might include nuclear for stability) and energy security advocates (who might prioritize nuclear for grid stability or independence, but are sidelined by the cost-per-dollar framing).
% DISAPPEARANCE_RATIONALE: If this framing vanished, nuclear energy would likely see increased investment and policy support, potentially shifting the overall energy transition strategy and capital allocation significantly. The energy policy landscape would reorganize around a broader set of criteria.
% FOUNDING_PROBLEM: The urgent need for rapid, large-scale decarbonization to avert catastrophic climate change, coupled with limited capital and time, necessitating efficient resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, international bodies (e.g., IPCC), and many governments attest to the urgency of the climate crisis and the need for effective mitigation. However, the specific 'fastest per dollar' framing is primarily attested by its beneficiaries and supporting analytical economists.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because this framing effectively diverts significant capital and policy attention from nuclear, which is a viable low-carbon option, towards other technologies. This diversion represents a substantial cost to the nuclear industry and to long-term energy planning that might favor nuclear. Suppression is also high (0.75) as the constraint actively marginalizes nuclear through discursive pressure, policy exclusion, and financial disincentives, rather than through physical barriers. The theater ratio is low (0.10) because the debate is genuine and impactful, with real policy consequences, not merely performative maintenance. Resistance is very high (0.85) from nuclear advocates who contest the premise.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between those who view 'fastest deployment per dollar' as the paramount metric for climate mitigation (beneficiaries) and those who see nuclear as a necessary component of a diverse, resilient, and long-term low-carbon energy system, despite its upfront costs and timelines (victims). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and climate action advocates are structural beneficiaries, as the constraint directs resources and policy support towards their preferred solutions. The nuclear industry and long-term energy planners are targets/payers, bearing the costs of capital diversion and policy marginalization. Fossil fuel incumbents are excluded, benefiting indirectly from any internal conflict within the low-carbon sector. Analytical economists serve as observers, providing data that influences the debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'How accurately do current models quantify the opportunity cost of investing in nuclear vs. renewables for climate mitigation, considering full system costs, grid integration, and long-term storage needs?',
    'Independent, comprehensive lifecycle assessment and techno-economic analysis comparing full system costs (including grid integration, storage, and decommissioning) across different energy portfolios, validated by real-world deployment data.',
    'If the opportunity cost is found to be lower or negligible when considering full system costs, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it from a Snare to a Tangled Rope or even a Rope, as nuclear''s role would be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Accuracy of opportunity cost calculations for nuclear vs. renewables.').

omega_variable(
    mitigation_scope_definition,
    'Is ''climate mitigation'' solely defined by carbon reduction per dollar per year, or does it encompass broader goals like energy security, grid resilience, and long-term energy independence, which nuclear might uniquely provide?',
    'Policy consensus or legislative redefinition of ''climate mitigation'' to explicitly include or exclude criteria beyond immediate carbon reduction efficiency.',
    'If the scope of mitigation expands to include these broader goals, the constraint''s justification for suppressing nuclear would weaken, reducing its extractiveness and suppression. This could lead to a reclassification towards a more coordinative type (e.g., Rope or Scaffold if temporary support is needed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_scope_definition, conceptual, 'Definition of ''climate mitigation'' and its scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, nuclear_regulatory_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
