% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Imperative for Climate Response
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth_reading' of the broader
 *   'climate_response_imperative' kernel. It posits that effective climate
 *   response necessitates a fundamental structural economic transformation in
 *   the Global North, involving reduced consumption, wealth redistribution,
 *   and the establishment of post-growth institutions. This is seen as
 *   essential for both robust mitigation and equitable adaptation, and as a
 *   prerequisite for enabling Global South development without replicating
 *   unsustainable patterns. The constraint is claimed as a Tangled Rope
 *   because it offers a genuine coordination function for planetary survival
 *   but demands significant, asymmetric extraction from current beneficiaries
 *   of the growth paradigm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.75).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Imperative for Climate Response").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '73f47bdf-e4ad-4f91-8b62-355ed7e7e7da').
narrative_ontology:cs_kernel_codification('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', distributed).
narrative_ontology:cs_authority_grounding('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', expertise).
narrative_ontology:cs_interpretation_layer_present('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da').
narrative_ontology:cs_reading_relation('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', foundational, planetary_boundaries_exist).
narrative_ontology:cs_axiom_status(planetary_boundaries_exist, holdable).
narrative_ontology:cs_axiom_grounding('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', planetary_boundaries_exist, empirically_contingent).
narrative_ontology:cs_axiom('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', foundational, intergenerational_equity_is_moral_imperative).
narrative_ontology:cs_axiom_status(intergenerational_equity_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', intergenerational_equity_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', ecological_limits_framework).
narrative_ontology:cs_drift_state('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', contemporary_climate_crisis, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('73f47bdf-e4ad-4f91-8b62-355ed7e7e7da', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, ecological_systems).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_global_north_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, growth_dependent_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, incumbent_political_economic_elites).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, ecological_economics_principles).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, intergenerational_equity_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to reduce consumption, accept redistribution, and transition to post-growth institutions, incurring significant lifestyle changes and economic adjustments. Their current prosperity is tied to the growth paradigm.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_global_north_populations, payer,
    powerful, biographical, constrained, global).

% Benefit from reduced climate impacts, increased adaptation support, and global redistribution of wealth and resources, enabling sustainable development and resilience. Remain vulnerable to climate change if transformation is insufficient.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Are the primary beneficiaries of successful climate mitigation and adaptation, inheriting a more stable and equitable planet. Their interests are represented by advocates and scientific consensus.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% Face existential threat from the imperative to decarbonize and reduce energy consumption. Their business model is directly contradicted by the required transformation, leading to stranded assets and loss of market share.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_industries, payer,
    institutional, immediate, trapped, global).

% Industries whose profitability and existence rely on continuous economic growth and increasing consumption. They face significant restructuring or decline under a degrowth paradigm.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_industries, payer,
    powerful, biographical, constrained, global).

% Propose, research, and advocate for the structural economic transformation, challenging incumbent paradigms and seeking to influence policy and public opinion. They aim to shift the global agenda.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, degrowth_advocates, agenda_setter,
    moderate, generational, mobile, global).

% Benefit from reduced human impact, restoration efforts, and a return to within planetary boundaries. They are passive recipients of human action and cannot exit the system.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, ecological_systems, beneficiary,
    powerless, civilizational, trapped, universal).

% Hold significant power within the existing growth-oriented economic and political systems. They would experience a loss of wealth, influence, and legitimacy under a degrowth transformation, leading to strong resistance.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, incumbent_political_economic_elites, payer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global economic activity towards ecological sustainability and intergenerational equity by reducing material throughput and redistributing wealth, ensuring a liveable planet for all.
% TRANSFER_FUNCTION: Moves wealth, resources, and consumption capacity from present-day Global North populations and growth-dependent industries to Global South populations, future generations, and ecological restoration efforts.
% ABSENT_VOICES: Growth economists, technological optimists, industries dependent on continuous growth, and segments of the Global North population resistant to lifestyle changes. They are excluded by the degrowth framing that their solutions are insufficient or harmful.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, the world would continue on a path of ecological overshoot and increasing inequality, leading to catastrophic climate breakdown, as the current economic system would continue its unsustainable growth trajectory.
% FOUNDING_PROBLEM: The ecological overshoot and climate crisis driven by unsustainable economic growth and consumption patterns, particularly in the Global North, leading to intergenerational and global injustice.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports, ecological footprint analyses, scientific consensus on planetary boundaries, and advocacy groups representing Global South and future generations corroborate the urgency and nature of the problem. This is attested by independent scientific bodies and civil society organizations, not solely by those who would benefit from the transformation.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the profound economic and social costs imposed on present-day Global North populations and growth-dependent industries. Suppression (0.75) is high due to the entrenched interests and political resistance to such radical transformation, requiring strong policy enforcement to overcome. The theater ratio is low (0.1) because this reading emphasizes genuine, deep structural change over performative or symbolic actions. Resistance is very high (0.9) as the proposed changes directly challenge powerful economic and political incumbents, as well as deeply ingrained consumer habits. The measurement series reflects an increasing urgency and required effort over time as the climate crisis intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations and Global South populations, this imperative is a necessary coordination for survival and justice. From the perspective of present-day Global North populations and growth-dependent industries, it represents a severe imposition and extraction. The engine will compute these divergent classifications based on the declared stakeholder roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations, Global South populations, and ecological systems are clear beneficiaries (low d) as they gain a more stable and equitable future. Present-day Global North populations, fossil fuel industries, and growth-dependent industries are targets (high d) due to the required reduction in consumption, wealth, and market share. Degrowth advocates act as agenda-setters, pushing for the transformation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is far from mandatrophy; its mandate is live and growing in urgency. The classification as Tangled Rope prevents mislabeling it as pure extraction by acknowledging its genuine coordination function for planetary survival, while also highlighting the asymmetric costs it imposes. It also prevents mislabeling as a simple Rope by recognizing the substantial resistance and the need for active enforcement against powerful interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_and_social_acceptance,
    'Is a degrowth transition politically and socially feasible in democratic Global North societies without widespread social unrest or authoritarian measures?',
    'Empirical observation of pilot programs, policy implementation, and public response in regions attempting degrowth-aligned policies; sociological studies on shifts in values and collective action.',
    'If infeasible, the constraint''s effective suppression and resistance would be higher, potentially leading to a reclassification towards Snare if the coordination function cannot be realized, or Piton if the efforts become purely theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_feasibility_and_social_acceptance, empirical, 'Uncertainty regarding the political and social viability of a degrowth transition.').

omega_variable(
    degrowth_vs_green_growth_efficacy,
    'Is degrowth truly more effective than ''green growth'' strategies (e.g., strong decoupling of economic growth from resource use) in achieving climate and ecological goals?',
    'Long-term comparative studies of regions pursuing different strategies, assessing actual reductions in material footprint, emissions, and biodiversity loss relative to economic activity.',
    'If green growth proves equally or more effective, the degrowth reading''s extractiveness might be overstated, and its claimed necessity for structural transformation could be challenged, potentially shifting its classification towards a less extractive type or even a conceptual Snare if its proponents suppress alternatives without clear efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_vs_green_growth_efficacy, empirical, 'Debate over the comparative efficacy of degrowth versus green growth paradigms.').

omega_variable(
    degrowth_impact_on_wellbeing,
    'How would reduced consumption and working time in the Global North affect human well-being, social cohesion, and mental health, beyond purely economic metrics?',
    'Interdisciplinary research combining economics, sociology, psychology, and public health to model and observe the non-economic impacts of degrowth policies on quality of life indicators.',
    'If well-being significantly declines, the political feasibility (and thus effective suppression/resistance) would be severely impacted. If well-being improves (e.g., through reduced stress, stronger communities), the resistance might be lower than currently estimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_impact_on_wellbeing, empirical, 'Uncertainty about the non-economic impacts of degrowth on human well-being.').

omega_variable(
    kernel_reading_degrowth_identity,
    'This constraint is the ''degrowth_reading'' of the ''climate_response_imperative'' kernel. What are the specific structural changes this reading implies compared to its siblings?',
    'Analysis of policy proposals and academic literature from each reading to identify core structural assumptions and their implications for economic organization, resource allocation, and social norms.',
    'The ''mitigation_priority_reading'' would emphasize technological solutions and market mechanisms, potentially reducing the victim set for Global North populations but increasing reliance on unproven technologies. The ''adaptation_priority_reading'' would focus on resilience in vulnerable regions, potentially deferring mitigation efforts and increasing long-term climate risks. This omega documents the distinct structural implications of the degrowth framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_degrowth_identity, conceptual, 'Documents the specific structural implications of the degrowth reading within the climate response kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_response_imperative__degrowth_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2025, climate_response_imperative__degrowth_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_response_imperative__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2035, climate_response_imperative__degrowth_reading, theater_ratio, 2035, 0.1).
narrative_ontology:measurement(clim_tr_t2040, climate_response_imperative__degrowth_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2045, climate_response_imperative__degrowth_reading, theater_ratio, 2045, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_response_imperative__degrowth_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_response_imperative__degrowth_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(clim_be_t2025, climate_response_imperative__degrowth_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement(clim_be_t2030, climate_response_imperative__degrowth_reading, base_extractiveness, 2030, 0.81).
narrative_ontology:measurement(clim_be_t2035, climate_response_imperative__degrowth_reading, base_extractiveness, 2035, 0.83).
narrative_ontology:measurement(clim_be_t2040, climate_response_imperative__degrowth_reading, base_extractiveness, 2040, 0.84).
narrative_ontology:measurement(clim_be_t2045, climate_response_imperative__degrowth_reading, base_extractiveness, 2045, 0.85).
narrative_ontology:measurement(clim_be_t2050, climate_response_imperative__degrowth_reading, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_response_imperative__degrowth_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2025, climate_response_imperative__degrowth_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(clim_su_t2030, climate_response_imperative__degrowth_reading, suppression_requirement, 2030, 0.71).
narrative_ontology:measurement(clim_su_t2035, climate_response_imperative__degrowth_reading, suppression_requirement, 2035, 0.73).
narrative_ontology:measurement(clim_su_t2040, climate_response_imperative__degrowth_reading, suppression_requirement, 2040, 0.74).
narrative_ontology:measurement(clim_su_t2045, climate_response_imperative__degrowth_reading, suppression_requirement, 2045, 0.75).
narrative_ontology:measurement(clim_su_t2050, climate_response_imperative__degrowth_reading, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, renewable_energy_subsidies).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, international_climate_agreements).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_response_imperative' kernel, each representing a distinct structural approach to climate action. This 'degrowth_reading' emphasizes structural economic transformation and redistribution, contrasting with mitigation-focused and adaptation-focused siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
