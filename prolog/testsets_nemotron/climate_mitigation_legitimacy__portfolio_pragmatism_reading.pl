% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Technology-Neutral Portfolio Pragmatism in Climate Mitigation Policy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The portfolio pragmatism reading asserts that optimal decarbonization
 *   requires a technology-neutral mix of nuclear and renewables, with the
 *   optimal balance varying by region. This is one reading of the contested
 *   kernel 'climate_mitigation_legitimacy' — the question of what gives a
 *   decarbonization pathway legitimate authority. The sibling readings are:
 *   baseload_necessity_reading (nuclear is indispensable),
 *   renewable_primacy_reading (renewables+storage suffice), and
 *   degrowth_sufficiency_reading (demand reduction makes generation expansion
 *   unnecessary). This reading's structural delta: neither technology
 *   privileged a priori; regional variation in optimal mix; moderate capital
 *   diversification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Pragmatism in Climate Mitigation Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'c89db4ad-6e45-428d-bf8c-40f80a13f442').
narrative_ontology:cs_kernel_codification('c89db4ad-6e45-428d-bf8c-40f80a13f442', formalized).
narrative_ontology:cs_authority_grounding('c89db4ad-6e45-428d-bf8c-40f80a13f442', expertise).
narrative_ontology:cs_interpretation_layer_present('c89db4ad-6e45-428d-bf8c-40f80a13f442').
narrative_ontology:cs_reading_relation('c89db4ad-6e45-428d-bf8c-40f80a13f442', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c89db4ad-6e45-428d-bf8c-40f80a13f442', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c89db4ad-6e45-428d-bf8c-40f80a13f442', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('c89db4ad-6e45-428d-bf8c-40f80a13f442', foundational, technology_neutrality_as_optimization_principle).
narrative_ontology:cs_axiom_status(technology_neutrality_as_optimization_principle, holdable).
narrative_ontology:cs_axiom_grounding('c89db4ad-6e45-428d-bf8c-40f80a13f442', technology_neutrality_as_optimization_principle, empirically_contingent).
narrative_ontology:cs_axiom('c89db4ad-6e45-428d-bf8c-40f80a13f442', foundational, regional_resource_heterogeneity_requires_portfolio_flexibility).
narrative_ontology:cs_axiom_status(regional_resource_heterogeneity_requires_portfolio_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('c89db4ad-6e45-428d-bf8c-40f80a13f442', regional_resource_heterogeneity_requires_portfolio_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('c89db4ad-6e45-428d-bf8c-40f80a13f442', integrated_assessment_model_optimality).
narrative_ontology:cs_drift_state('c89db4ad-6e45-428d-bf8c-40f80a13f442', post_ar6_iea_nze_2023, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('c89db4ad-6e45-428d-bf8c-40f80a13f442', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, system_integrators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_allocators_locked_into_suboptimal_mix).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, system_level_optimization_over_technology_preference).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, regional_resource_heterogeneity_requires_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manage transmission and distribution systems; must balance reliability, cost, and decarbonization mandates. They set procurement rules and interconnection standards that shape the technology mix. Their institutional mandate forces technology-agnostic planning, but they face political pressure from all technology camps.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, regional).

% Supplies dispatchable low-carbon generation. Benefits from portfolio mandates that prevent renewable-only policies from excluding nuclear. High capital intensity and long lead times make exit costly; they are locked into the existing regulatory and supply chain structure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Deploy wind, solar, and storage. Benefit from technology-neutral policies that don't privilege nuclear, but also from portfolio approaches that value their output. Capital can shift between technologies and geographies relatively easily compared to nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers, beneficiary,
    organized, biographical, mobile, global).

% Design and operate hybrid systems combining generation, storage, and demand response. They profit from the complexity of multi-technology optimization. Their business model depends on the portfolio approach being the dominant paradigm.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, system_integrators, beneficiary,
    moderate, biographical, mobile, regional).

% Bear the system costs of decarbonization through electricity bills. Have no meaningful exit from the grid. The portfolio approach may lower total system cost compared to single-technology mandates, but cost allocation mechanisms are opaque and they have no leverage over technology choices.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers, payer,
    powerless, immediate, trapped, local).

% Organizations and policy entrepreneurs committed to either renewable-only or nuclear-only pathways. They invest political capital and reputation in a single-technology thesis. The portfolio consensus marginalizes their advocacy; they pay opportunity costs in influence and funding. Exit means abandoning their core identity and coalition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_advocates, excluded).

% Large institutional investors and development banks with existing portfolios tilted toward one technology. Portfolio mandates force rebalancing, creating stranded asset risk or missed returns. They have capital mobility but face fiduciary and regulatory constraints that slow reallocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_allocators_locked_into_suboptimal_mix, payer,
    powerful, biographical, constrained, global).

% Model system-level decarbonization pathways. Their analyses generally support technology-neutral portfolios as cost-minimizing, but they note that real-world implementation is distorted by political economy. They observe the constraint from outside the distributional conflict.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the system-level optimization problem: how to achieve deep decarbonization at lowest total system cost while maintaining reliability, given heterogeneous regional resources (wind, solar, hydro, geothermal, uranium), varying grid infrastructure, and different demand profiles. The coordination function is genuine — no single technology can serve all contexts optimally.
% TRANSFER_FUNCTION: Moves capital and policy support from single-technology deployment toward diversified portfolios. Transfers risk from ratepayers (who bear cost of overbuilding any single technology) to developers and investors (who must meet portfolio standards). Transfers political legitimacy from technology-specific advocacy coalitions to system-integration experts.
% ABSENT_VOICES: Communities hosting energy infrastructure (nuclear waste sites, mining for renewables, transmission corridors) are structurally excluded from portfolio-level decisions. Indigenous groups affected by uranium mining or large-scale renewable deployment. Future generations who bear long-lived waste and climate risk. These voices would object to specific siting and waste outcomes that the portfolio abstraction renders invisible.
% DISAPPEARANCE_RATIONALE: If the portfolio pragmatism constraint vanished, policy would likely polarize into renewable-only mandates (in jurisdictions with strong renewable lobbies) or nuclear-only pushes (in state-led energy systems). The technology-neutral planning frameworks (integrated resource plans, capacity markets with technology-neutral eligibility) would be replaced by technology-specific carve-outs, changing investment flows, grid planning, and international technology cooperation.
% FOUNDING_PROBLEM: Early climate policy assumed either renewables-alone or nuclear-alone could decarbonize. Both proved inadequate: renewables face intermittency and land-use limits at high penetration; nuclear faces cost, schedule, and social license barriers. The portfolio approach emerged from integrated assessment modeling showing least-cost pathways require both.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) Chapter 6 and 7: multiple modeled pathways show nuclear and renewables both expanding in cost-optimal portfolios. IEA Net Zero by 2050 roadmap: nuclear capacity doubles, renewables scale 4x. These are multi-institutional assessments not captured by any single technology lobby. However, the 'moderate capital diversification' claim is contested — some integrated assessment models show near-zero nuclear in cost-optimal paths under optimistic storage costs.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) is moderate: the constraint extracts compliance costs from single-technology advocates and capital allocators, but distributes benefits across multiple industries and ratepayers via system cost reduction. Suppression (0.35) reflects active enforcement through planning requirements, capacity market rules, and subsidy eligibility — but alternatives (single-technology policies) are not fully suppressed, they remain live political options. Theater ratio (0.28) captures the growing gap between 'technology-neutral' rhetoric and the reality that policy frameworks still embed technology-specific subsidies (production tax credits, nuclear loan guarantees) that distort the portfolio. Accessibility collapse (0.45) is moderate: single-technology pathways remain intellectually and politically available, but the institutional momentum favors portfolios. Resistance (0.55) is significant: renewable-only and nuclear-only coalitions actively contest portfolio mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the grid operator seat, this is a genuine coordination problem solved by portfolio planning — the constraint is experienced as rope. From the ratepayer seat, it's extraction via opaque cost allocation — snare. From single-technology advocates, it's suppression of their preferred pathway — tangled rope or snare depending on their power. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the authoring seat's judgment that the coordination function is real but asymmetrically extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators and system integrators sit near the beneficiary end (d ~ 0.2-0.3): they gain authority and revenue from managing complexity. Nuclear and renewable industries are beneficiaries but with asymmetric extraction — nuclear is more locked in (constrained exit) while renewables are more mobile. Ratepayers are full targets (d ~ 0.9): trapped, bearing costs with no exit. Single-technology advocates are targets (d ~ 0.7): they pay political opportunity costs. Capital allocators are intermediate (d ~ 0.5): powerful but constrained by existing portfolios. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single-technology inadequacy) remains live per IPCC/IEA assessments. However, the 'moderate capital diversification' claim is weakening: as renewable+storage costs fall, the marginal value of nuclear in portfolios declines in many regions. The constraint risks mandatrophy if portfolio mandates persist after the diversification rationale erodes. Currently the mandate is still resolving toward its function, not yet atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the technology-neutral portfolio requirement a genuine system-level coordination necessity, or does it function as a political compromise that extracts from ratepayers to subsidize both nuclear and renewable industries?',
    'Counterfactual modeling: compare total system cost under technology-neutral optimization vs. technology-specific mandates across regions. If neutral optimization consistently lowers cost, coordination function dominates. If costs are similar but political coalitions are maintained, extraction dominates.',
    'If coordination-dominant, the constraint is a genuine tangled rope. If extraction-dominant, it is a snare disguised as coordination. The classification shifts the mandated remedy: coordination failures need better optimization; extraction failures need subsidy reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the portfolio constraint''s coordination function is structurally necessary or politically constructed.').

omega_variable(
    reading_relation_baseload_necessity,
    'Does the portfolio pragmatism reading structurally foreclose the baseload necessity reading, or do they coexist as competing but compatible framings?',
    'Analyze whether a single policy framework can simultaneously hold ''neither technology privileged a priori'' and ''nuclear is indispensable for baseload''. If baseload necessity implies nuclear privilege, foreclosure holds. If baseload can be met by non-nuclear dispatchables (hydro, geothermal, long-duration storage), coexistence holds.',
    'If forecloses, the kernel has a genuine logical schism. If coexists_with, the readings are political positions within a shared framework. This affects whether the kernel can be resolved by evidence or only by power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_baseload_necessity, conceptual, 'Structural relationship between portfolio pragmatism and baseload necessity readings of the climate mitigation legitimacy kernel.').

omega_variable(
    reading_relation_renewable_primacy,
    'Does the portfolio pragmatism reading structurally foreclose the renewable primacy reading?',
    'Analyze whether ''neither technology privileged a priori'' is logically compatible with ''renewables+storage can achieve full decarbonization faster and cheaper''. If renewable primacy is true as an empirical claim, portfolio pragmatism''s neutrality becomes a normative overlay on a settled empirical question — potential foreclosure. If renewable primacy is contested, coexistence holds.',
    'Determines whether the renewable primacy reading is a live empirical challenge to portfolio neutrality or a competing normative frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_renewable_primacy, empirical, 'Structural relationship between portfolio pragmatism and renewable primacy readings.').

omega_variable(
    reading_relation_degrowth_sufficiency,
    'Does the portfolio pragmatism reading foreclose the degrowth sufficiency reading?',
    'Portfolio pragmatism assumes generation expansion is necessary; degrowth sufficiency assumes it is not. These are contradictory premises about the scale of the problem. Test: can a framework simultaneously optimize a generation portfolio AND treat demand reduction as the primary lever? If yes, coexistence. If the premises are mutually exclusive, forecloses.',
    'If forecloses, the kernel contains a fundamental scale disagreement. If coexists_with, portfolio and sufficiency are complementary policy levers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relation_degrowth_sufficiency, conceptual, 'Structural relationship between portfolio pragmatism and degrowth sufficiency readings.').

omega_variable(
    regional_variation_authenticity,
    'Is the claimed ''regional variation in optimal mix'' a genuine structural feature of the constraint, or a rhetorical device to accommodate political heterogeneity?',
    'Compare modeled optimal portfolios across regions (using consistent assumptions) with actual policy outcomes. If policy tracks modeled optima, variation is structural. If policy diverges systematically toward local industrial preferences, variation is rhetorical.',
    'If rhetorical, the constraint''s coordination function is weaker than claimed — it is a political settlement masquerading as optimization. This would increase effective extraction for ratepayers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_variation_authenticity, empirical, 'Whether regional portfolio variation reflects genuine resource heterogeneity or political capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2019, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2023, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2027, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2027, 0.27).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2031, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2031, 0.28).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tr_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2035, 0.28).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2019, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2019, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2023, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2027, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2027, 0.4).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2031, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2031, 0.41).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_be_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2035, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2019, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2019, 0.28).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2023, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2023, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2027, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2027, 0.34).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2031, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2031, 0.35).
narrative_ontology:measurement(climate_mitigation_legitimacy__portfolio_pragmatism_reading_su_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2035, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel. The four readings decompose the kernel's legitimacy claim into structurally distinct constraints with different ε values, beneficiary/victim structures, and types. The portfolio pragmatism reading has moderate ε (0.42) because it coordinates multiple industries while extracting from single-technology advocates and ratepayers. The baseload necessity reading likely has higher ε (nuclear industry capture). The renewable primacy reading has moderate ε (renewable industry capture). The degrowth sufficiency reading has low ε (few concentrated beneficiaries) but high suppression (demand reduction mandates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
