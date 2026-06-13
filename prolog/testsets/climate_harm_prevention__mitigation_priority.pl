% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Mitigation-Priority Framework: Growth-Compatible Decarbonization via Technological Transition
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response frames legitimate
 *   climate action as rapid emissions reduction achieved through
 *   technological transition within existing growth paradigms. Future
 *   generations are the primary beneficiaries (constrained warming prevents
 *   catastrophic impacts on their living conditions); present
 *   carbon-intensive sectors and workers are the primary cost-bearers
 *   (stranded assets, job displacement, energy price changes); renewable
 *   technology producers capture policy-backed market expansion;
 *   wealthy-nation electorates benefit from diffuse climate risk reduction
 *   without apparent consumption change. The constraint is actively enforced
 *   through climate finance architecture, carbon pricing mechanisms,
 *   emissions targets, and scientific legitimacy (IPCC consensus framing).
 *   The reading competes with adaptation-priority (accepts higher warming,
 *   prioritizes resilience) and degrowth (contests the growth assumption
 *   itself) as alternative framings of the same foundational climate physics
 *   problem.
 *
 * KEY AGENTS:
 *   - Future generations: primary beneficiaries; zero voice in policy; infinite time horizon
 *   - Carbon-intensive sectors: institutional payers; high power but constrained by stranded-asset risk and transition mandates
 *   - Present-generation workers in carbon sectors: organized payers with constrained exit; face displacement but promised just-transition benefits that materialize later
 *   - Renewable technology producers: institutional beneficiaries; mobile; gain policy-backed market share expansion
 *   - Climate finance architects: agenda-setters; control target-setting, allocation flows, narrative legitimacy via science consensus
 *   - Global South developing nations: powerless payers; high climate vulnerability but minimal mitigation capital; energy development pathways prescribed from outside
 *   - Adaptation and degrowth advocates: excluded from mitigation-priority legitimacy apparatus; would challenge the growth assumption and timeline feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.61).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Mitigation-Priority Framework: Growth-Compatible Decarbonization via Technological Transition").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'e79e949d-0a7a-41af-a78a-789a1f954f92').
narrative_ontology:cs_kernel_codification('e79e949d-0a7a-41af-a78a-789a1f954f92', formalized).
narrative_ontology:cs_authority_grounding('e79e949d-0a7a-41af-a78a-789a1f954f92', expertise).
narrative_ontology:cs_interpretation_layer_present('e79e949d-0a7a-41af-a78a-789a1f954f92').
narrative_ontology:cs_reading_relation('e79e949d-0a7a-41af-a78a-789a1f954f92', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e79e949d-0a7a-41af-a78a-789a1f954f92', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e79e949d-0a7a-41af-a78a-789a1f954f92', foundational, growth_compatible_decarbonization_feasible).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_feasible, holdable).
narrative_ontology:cs_axiom_grounding('e79e949d-0a7a-41af-a78a-789a1f954f92', growth_compatible_decarbonization_feasible, empirically_contingent).
narrative_ontology:cs_axiom('e79e949d-0a7a-41af-a78a-789a1f954f92', foundational, mitigation_prioritized_over_adaptation).
narrative_ontology:cs_axiom_status(mitigation_prioritized_over_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('e79e949d-0a7a-41af-a78a-789a1f954f92', mitigation_prioritized_over_adaptation, instrumental).
narrative_ontology:cs_reference_frame('e79e949d-0a7a-41af-a78a-789a1f954f92', technological_decarbonization_within_growth).
narrative_ontology:cs_drift_state('e79e949d-0a7a-41af-a78a-789a1f954f92', post_2025_transition_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e79e949d-0a7a-41af-a78a-789a1f954f92', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_technology_producers).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_finance_architects).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_generation_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, global_south_countries_lacking_capital).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.32 (2005, when mitigation was still framed as optional) to 0.68 (2050, when the framework's costs on carbon-intensive sectors and just-transition-constrained workers are fully realized). Theater rises from 0.18 to 0.42 as the proportion of climate spending that goes to net-zero narrative and green-washing (corporate net-zero commitments, carbon offset markets) grows relative to actual emissions reduction. Suppression requirement rises from 0.35 to 0.61 as the framework must actively suppress alternative readings (adaptation does not work fast enough, degrowth is politically infeasible) and suppress the voices of fossil fuel workers who reject the transition timeline offered. The measurements track the constraint's lifecycle: early phase (2005–2015) shows it establishing legitimacy and beginning enforcement; mid-phase (2015–2025) shows extraction and suppression intensifying as the real costs land; late phase (2035–2050) shows theater rising as the framework's internal contradictions become visible (just-transition promises unfunded, Global South energy inequality deepening, wealthy-nation consumption unchanged) while suppression remains necessary to prevent acknowledged alternatives from entering policy space.
 *
 * PERSPECTIVAL GAP:
 *   The single largest gap is between the climate-finance-architect seat and the carbon-intensive-sector seat. The architect reads the constraint as genuine coordination (climate stability is a global public good) with legitimate cost-shifting (emitters pay for the harm they caused/cause). The sector reads the constraint as regulatory extraction (their capital is seized via stranded-asset rules; their workers are conscripted into transition; their cost is decoupled from any private benefit). Both readings are internally consistent. The engine's directionality computation surfaces this gap: architects get d near 0.5 (they benefit from authority but coordinate a real problem); sectors get d near 0.95 (they pay and constrained their exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: d → 0.0 (full beneficiary, but trapped powerless; the constraint exists to benefit them; they cannot exit). Carbon-intensive sectors: d → 0.95 (nearly full target; institutional power mitigates the mathematical d=1.0 maximum, but they are the named payers and face enforcement). Present-generation workers: d → 0.78 (high target; they pay in job displacement, but organized labor has some collective power and secondary beneficiary role from climate risk reduction). Renewable producers: d → 0.15 (beneficiary; mobile, institutional, gain policy capture). Climate finance architects: d → 0.52 (symmetric; they coordinate the arrangement, but they also enforce it, and they benefit from the structural authority that mitigation-priority framing grants them, and they are constrained by the political need to maintain growth narrative). Global South nations: d → 0.88 (high target; powerless, trapped, constrained energy pathways, but benefits accrue to future generations of wealthy nations). Wealthy-nation electorates: d → 0.25 (net beneficiary; organized, mobile, low perceived cost under growth assumptions). Adaptation advocates: excluded — no d value because they are not seated in the constraint, only marginalized by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification on three grounds: (1) it has a live founding problem (atmospheric CO2 rising, warming trajectory dangerous) attested outside the benefiting parties (IPCC); (2) it has real coordination function (align global incentives, prevent free-rider carbon dumping, set legitimate emission targets); (3) it has a temporal justification (the problem is urgent, the solution timeline is bounded, by 2050 the founding problem is either solved or the constraint is revealed as inadequate). However, the constraint carries mandatrophy-risk signals: the theater_ratio rising from 0.18 to 0.42 indicates that a growing portion of enforcement activity is performative rather than functional (green-washing, carbon offsets with dubious additionality, net-zero pledges decoupled from actual emissions cuts). The suppression_requirement remaining elevated (0.61 in 2050) indicates that the constraint continues to require active suppression of alternative framings (adaptation, degrowth) rather than commanding voluntary consensus. The most dangerous mandatrophy vector is the growth assumption itself: if technical decarbonization proves infeasible within growth constraints, the founding-problem status flips from 'live' to 'unsolvable-under-stated-assumptions,' and the constraint would become an inert performance lacking real function — a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_empirical,
    'Can global emissions be reduced to net-zero by 2050 while maintaining or growing per-capita material throughput in wealthy nations?',
    'Post-2050 empirical observation of energy system and material consumption decoupling. Alternatively, detailed bottom-up energy-transition modeling accounting for rebound effects, cement/steel/aviation decarbonization constraints, and total life-cycle emissions.',
    'If decoupling fails and emissions and material throughput remain coupled, the mitigation-priority reading''s core assumption breaks, and degrowth reading''s necessity claim is vindicated. The constraint would shift from tangled_rope (extractive but coordinating real problem) toward snare (extraction hiding unsolvable problem). Classification would flip at the economy-wide level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_compatibility_empirical, empirical, 'Physical feasibility of growth-compatible decarbonization.').

omega_variable(
    political_economy_of_carbon_sector,
    'Can carbon-intensive sectors'' transition be achieved through carbon pricing and market incentives, or does their institutional power (lobbying, capital control, labor leverage) prevent market-driven decarbonization?',
    'Comparative historical analysis of sector transitions that succeeded (e.g., refrigerant phase-out, lead paint removal) vs. those stalled (e.g., agricultural methane). Post-2030 assessment of whether carbon-intensive sectors'' emissions reductions track pricing incentives or are merely delayed through lobbying and regulatory capture.',
    'If carbon sectors successfully block or delay transition via political economy, the enforcement requirement would rise further (suppression would need to increase) and the constraint would become purely extractive rather than coordinating. Alternatively, if market incentives prove sufficient, the extraction component is lower than authored, and the constraint is more purely rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_carbon_sector, empirical, 'Whether carbon sector transition is market-driven or politically blocked.').

omega_variable(
    just_transition_funding_credibility,
    'Will wealthy nations actually fund just-transition support at the scale necessary to prevent mass worker displacement and maintain organized labor''s political support for mitigation?',
    'Tracking of actual climate finance flows dedicated to just-transition vs. pledged amounts (currently, pledges exceed realized funding by 3–5x). Post-2030 labor-market data on displaced fossil-fuel worker employment and wage outcomes in transition programs.',
    'If just-transition funding fails, the constraint shifts from extracting ''fairly'' (via social support) to extracting brutally (via mass joblessness). The present-generation-workers seat would flip from ''constrained payer with secondary beneficiary role'' to ''victim.'' Suppression would need to intensify to prevent labor-led opposition coalition. Classification risk: constraint becomes pure snare on that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(just_transition_funding_credibility, empirical, 'Whether just-transition is funded or merely promised.').

omega_variable(
    adaptation_infeasibility_claim,
    'Is rapid emissions mitigation actually MORE feasible than massive-scale adaptation, or does the adaptation-priority reading''s claim that mitigation is politically/economically infeasible hold?',
    'Comparative cost analysis of adaptation (dike building, water infrastructure, coastal relocation, crop breeding) vs. decarbonization at various warming levels. Post-2035 political economy assessment: has mitigation actually accelerated globally, or have nations shifted resources to adaptation despite mitigation commitments?',
    'If adaptation proves MORE feasible and mitigation stalls, the mitigation-priority reading''s legitimacy collapses, and adaptation-priority reading becomes ascendant. The constraint would be reclassified as failed founding-problem-solving (mandatrophy), and the distribution of extraction would invert (wealthy nations that promised mitigation but funded adaptation would be exposed as free-riders).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_infeasibility_claim, conceptual, 'Relative feasibility of mitigation vs. adaptation strategies.').

omega_variable(
    kernel_reading_contestation,
    'Is the mitigation-priority reading a permanent legitimate reading of climate response, or is it merely a transitional compromise between adaptation and degrowth positions?',
    'Institutional and academic landscape analysis: tracking whether adaptation-priority and degrowth readings gain policy credibility post-2030 or remain marginalized. If mitigation stalls post-2035, which reading becomes the institutional fallback?',
    'If either sibling reading gains institutional credibility (shifts from excluded to seat-holding), the kernel''s structure changes and the mitigation-priority reading becomes one of three coexisting readings rather than the dominant framing. The constraint''s enforceability would depend on defending its reading against live competitors, not suppressing impossible alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether mitigation-priority reading is permanent or transitional kernel interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 2005, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__mitigation_priority, theater_ratio, 2025, 0.38).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__mitigation_priority, theater_ratio, 2035, 0.45).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__mitigation_priority, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__mitigation_priority, base_extractiveness, 2035, 0.7).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__mitigation_priority, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__mitigation_priority, suppression_requirement, 2035, 0.62).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel contains three structurally distinct constraints, each with different ε values, beneficiary/victim structures, and type classifications. This file (mitigation_priority reading) asserts rapid technological transition is feasible; the adaptation_priority reading contests mitigation feasibility and reframes legitimacy around resilience; the degrowth_reading contests the growth assumption itself. All three share the same foundational climate physics (rising CO2, dangerous warming) but derive different policy-response structures from contested assumptions about political economy and technology. Link all three via affects_constraints to enable contamination analysis: if mitigation-priority decarbonization succeeds, adaptation and degrowth readings lose policy traction; if mitigation stalls, the sibling readings gain legitimacy and the kernel's active reading shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
