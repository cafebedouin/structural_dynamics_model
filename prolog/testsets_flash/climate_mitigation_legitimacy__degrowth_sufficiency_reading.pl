% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization Requires Demand Reduction (Degrowth Sufficiency Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of climate
 *   mitigation legitimacy, asserting that decarbonization fundamentally
 *   requires a reduction in energy demand, thereby rendering large-scale
 *   generation expansion (whether nuclear or renewable) unnecessary. This
 *   reading challenges conventional growth-oriented climate strategies and
 *   redefines the scope of necessary action. It is presented as a Snare
 *   because its implementation would suppress alternative, growth-compatible
 *   decarbonization pathways and impose significant costs on industries and
 *   consumers reliant on energy abundance, while benefiting advocates of
 *   degrowth and local resilience.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, snare).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization Requires Demand Reduction (Degrowth Sufficiency Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'd85d4445-affd-4f95-b2a9-1986267f3eaa').
narrative_ontology:cs_kernel_codification('d85d4445-affd-4f95-b2a9-1986267f3eaa', distributed).
narrative_ontology:cs_authority_grounding('d85d4445-affd-4f95-b2a9-1986267f3eaa', distributed).
narrative_ontology:cs_reading_relation('d85d4445-affd-4f95-b2a9-1986267f3eaa', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('d85d4445-affd-4f95-b2a9-1986267f3eaa', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d85d4445-affd-4f95-b2a9-1986267f3eaa', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('d85d4445-affd-4f95-b2a9-1986267f3eaa', foundational, energy_demand_reduction_is_primary).
narrative_ontology:cs_axiom_status(energy_demand_reduction_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('d85d4445-affd-4f95-b2a9-1986267f3eaa', energy_demand_reduction_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('d85d4445-affd-4f95-b2a9-1986267f3eaa', foundational, planetary_boundaries_constrain_growth).
narrative_ontology:cs_axiom_status(planetary_boundaries_constrain_growth, holdable).
narrative_ontology:cs_axiom_grounding('d85d4445-affd-4f95-b2a9-1986267f3eaa', planetary_boundaries_constrain_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('d85d4445-affd-4f95-b2a9-1986267f3eaa', sufficiency_within_planetary_limits).
narrative_ontology:cs_drift_state('d85d4445-affd-4f95-b2a9-1986267f3eaa', contemporary_policy_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d85d4445-affd-4f95-b2a9-1986267f3eaa', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and advocate for policies that prioritize demand reduction and energy sufficiency, arguing that technological solutions alone are insufficient or undesirable. They benefit from the legitimacy this reading grants to their policy proposals.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Benefit from the emphasis on local, decentralized solutions and reduced energy consumption, aligning with their goals of community self-sufficiency and reduced reliance on large-scale infrastructure. This reading provides a policy framework for their initiatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements, beneficiary,
    moderate, biographical, constrained, local).

% Faces significant opposition and reduced investment under this reading, as large-scale nuclear expansion is deemed unnecessary or counterproductive. Their business model is directly challenged, leading to project cancellations and reduced market access.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% While advocating for clean energy, they are victims of this reading's emphasis on demand reduction over generation expansion. Their plans for large-scale renewable projects and associated infrastructure face reduced political and financial support, limiting their growth potential.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers, payer,
    powerful, biographical, constrained, global).

% Their core premise of continuous economic expansion is directly challenged by this reading. They bear the cost of policy shifts that prioritize sufficiency over growth, leading to reduced investment in traditional growth-oriented sectors and a re-evaluation of economic metrics.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_advocates, payer,
    institutional, generational, identity_locked, global).

% Experience direct impacts from demand reduction policies, potentially including restrictions on energy use, higher energy prices to disincentivize consumption, or reduced access to energy-intensive goods and services. Their choices are constrained by the imperative for sufficiency.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_consumers, payer,
    powerless, immediate, trapped, national).

% Provide scientific assessments of climate change and mitigation pathways. They observe the policy debates and the effectiveness of different strategies, but do not directly benefit or pay from this specific policy framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal efforts towards decarbonization by prioritizing a specific pathway: reducing overall energy demand to align with sustainable resource limits, thereby simplifying the challenge of clean energy supply.
% TRANSFER_FUNCTION: Transfers societal resources and political capital away from large-scale energy infrastructure projects (both fossil and clean) towards demand-side management, efficiency, and local, smaller-scale solutions. It also transfers the burden of adjustment to energy consumers and growth-dependent industries.
% ABSENT_VOICES: Future generations who might benefit from abundant, clean energy if large-scale generation were pursued more aggressively; communities whose economic development relies on energy-intensive industries; and technologists whose innovations in large-scale energy production are sidelined.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the policy landscape for decarbonization would immediately shift towards prioritizing large-scale generation (renewables, nuclear, or both). Investment flows would reorient, and the political discourse would focus on supply-side solutions, fundamentally altering the trajectory of climate action and energy system development.
% FOUNDING_PROBLEM: The problem of achieving rapid and equitable decarbonization while respecting planetary boundaries, recognizing that continuous economic growth and associated energy demand make purely supply-side solutions insufficient or environmentally unsustainable.
% FOUNDING_PROBLEM_CORROBORATION: Environmental economists and ecological scientists outside the immediate degrowth advocacy groups corroborate the problem of planetary boundaries and the limits to growth. International reports on resource depletion and ecological overshoot also provide corroborating evidence.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading imposes significant costs on industries (nuclear, large-scale renewables) and consumers by limiting energy availability and economic growth. Suppression (0.70) is also high, as it actively seeks to marginalize and suppress alternative decarbonization strategies that rely on generation expansion. The theater ratio (0.20) is relatively low, as the advocates of this reading are generally sincere in their belief that demand reduction is the most effective and ethical path, with less performative justification. Accessibility collapse (0.40) is moderate, as alternative energy futures are not entirely foreclosed but are actively disincentivized. Resistance (0.75) is high due to strong opposition from industries, governments, and populations committed to economic growth and energy abundance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this constraint is a necessary 'rope' for planetary survival, coordinating human activity within ecological limits. From the perspective of the nuclear or renewable industries, it is a 'snare' that unfairly targets their solutions and extracts their potential for growth and contribution to decarbonization. The engine's classification as a Snare reflects the structural asymmetry of costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates and local resilience movements are beneficiaries, as this reading legitimizes their policy agendas and provides a framework for their activities. The nuclear industry, renewable energy developers, economic growth advocates, and energy consumers are victims, as they bear the costs of reduced investment, suppressed growth, and constrained energy access. The directionality for these groups will reflect their position as targets of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_reduction_feasibility,
    'Is large-scale demand reduction politically and socially feasible within the required timeframe without unacceptable societal costs?',
    'Empirical observation of policy implementation in various jurisdictions and societal response; detailed socio-economic modeling of transition pathways.',
    'If infeasible, the constraint''s claimed coordination function collapses, and its suppressive aspects become pure extraction without a viable path to decarbonization. This would push the classification further towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_reduction_feasibility, empirical, 'Uncertainty regarding the practical implementability and societal acceptance of radical demand reduction policies.').

omega_variable(
    technological_potential_underestimation,
    'Does this reading systematically underestimate the potential for technological innovation (e.g., advanced renewables, fusion, carbon capture) to enable decarbonization without demand reduction?',
    'Breakthroughs in energy technology that significantly alter the cost-benefit analysis of supply-side solutions; independent expert assessment of technology readiness levels and deployment potential.',
    'If technological potential is significantly underestimated, the constraint''s justification for suppressing generation expansion weakens, and its extractive nature (from industries and consumers) becomes more apparent, potentially shifting it towards a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_potential_underestimation, conceptual, 'Ambiguity regarding the role of future technological advancements in climate mitigation strategies.').

omega_variable(
    equity_impacts_of_demand_reduction,
    'Are the costs of demand reduction equitably distributed, or do they disproportionately burden vulnerable populations and developing economies?',
    'Socio-economic impact assessments of demand reduction policies across different income groups and national contexts; analysis of energy poverty metrics.',
    'If costs are inequitably distributed, the constraint''s legitimacy as a ''just transition'' pathway is undermined, reinforcing its character as an extractive mechanism for certain populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_impacts_of_demand_reduction, preference, 'Ethical and equity considerations regarding the distribution of burdens from demand reduction policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2025, 0.17).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.18).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2035, 0.19).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2040, 0.2).
narrative_ontology:measurement(clim_tr_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2045, 0.2).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.61).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2035, 0.63).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2040, 0.64).
narrative_ontology:measurement(clim_be_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2045, 0.65).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.66).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2035, 0.68).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2040, 0.69).
narrative_ontology:measurement(clim_su_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2045, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'climate_mitigation_legitimacy' kernel, each representing a distinct approach to decarbonization. This 'degrowth sufficiency' reading directly challenges the premises of the other three, which all assume some form of large-scale generation expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
