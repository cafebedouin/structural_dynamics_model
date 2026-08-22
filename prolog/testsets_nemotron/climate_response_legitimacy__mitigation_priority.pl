% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Legitimate Climate Response Prioritizes Mitigation Through Innovation and Carbon Pricing
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response legitimacy holds that
 *   emissions reduction through carbon pricing and technological innovation
 *   is the legitimate, feasible, and primary pathway — preserving economic
 *   growth while decoupling it from emissions. This reading became dominant
 *   after Rio (1992) and was cemented by the Kyoto market mechanisms and
 *   Paris Agreement's NDC architecture. It coordinates global action around a
 *   shared mitigation trajectory but extracts asymmetrically: future
 *   generations bear existential risk if decoupling fails; vulnerable
 *   populations bear regressive transition costs without guaranteed benefits;
 *   fossil workers and global south development aspirations are subordinated
 *   to the pace and shape of the technological transition. The constraint
 *   requires active enforcement (carbon pricing, border adjustments, subsidy
 *   regimes, planning reforms) and its theater ratio rises as the gap between
 *   pledged trajectories and implemented policies widens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.48).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.35).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.48).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Legitimate Climate Response Prioritizes Mitigation Through Innovation and Carbon Pricing").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede').
narrative_ontology:cs_kernel_codification('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', formalized).
narrative_ontology:cs_authority_grounding('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', lineage).
narrative_ontology:cs_interpretation_layer_present('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede').
narrative_ontology:cs_reading_relation('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', foundational, growth_decoupling_achievable_via_innovation).
narrative_ontology:cs_axiom_status(growth_decoupling_achievable_via_innovation, holdable).
narrative_ontology:cs_axiom_grounding('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', growth_decoupling_achievable_via_innovation, empirically_contingent).
narrative_ontology:cs_axiom('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', foundational, carbon_pricing_as_sufficient_coordination_mechanism).
narrative_ontology:cs_axiom_status(carbon_pricing_as_sufficient_coordination_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', carbon_pricing_as_sufficient_coordination_mechanism, instrumental).
narrative_ontology:cs_axiom('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', secondary, technological_substitution_over_demand_reduction).
narrative_ontology:cs_axiom_status(technological_substitution_over_demand_reduction, holdable).
narrative_ontology:cs_axiom_grounding('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', technological_substitution_over_demand_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', rio_kyoto_paris_mitigation_architecture).
narrative_ontology:cs_drift_state('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', post_paris_implementation_gap, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3bf0cfbb-eebd-4fd0-ae9c-62b47f637ede', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_pricing_architects).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, clean_technology_capital).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_generation_consumers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, fossil_fuel_incumbents_transitioning).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations_if_decoupling_fails).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, vulnerable_populations_locked_out_of_transition).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_fuel_workers_without_transition_pathways).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_development_aspirations_constrained).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, current_generation_consumers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_fuel_incumbents_transitioning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer carbon pricing mechanisms (taxes, cap-and-trade, border adjustments). They set the stringency trajectory and allocation rules. Their authority derives from international agreements and national legislation. They can move between institutions (World Bank, IMF, national treasuries, consultancies) and capture value through advisory roles.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_pricing_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Venture capital, green bonds, sovereign wealth funds, and corporate R&D budgets directed at renewable energy, batteries, hydrogen, carbon capture, and efficiency. They capture returns from policy-driven demand creation. Capital is fungible and can exit to other sectors if policy support wanes, though stranded asset risk creates pressure to maintain the policy regime.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, clean_technology_capital, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from continued energy access, mobility, and consumption patterns enabled by technological substitution rather than demand reduction. Bear carbon price passthrough costs and transition surcharges. Exit is constrained by infrastructure lock-in (housing, transport, grid) and the collective-action nature of lifestyle change — individual exit does not change the system.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, current_generation_consumers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, current_generation_consumers, payer).

% Oil majors, utilities, and state-owned enterprises rebranding as 'energy companies' — capturing transition subsidies, carbon credit markets, and hydrogen/CCS funding while managing decline of core assets. They pay stranded asset write-downs and compliance costs but shape the transition pace through lobbying and investment choices. Exit from fossil core is constrained by shareholder expectations and asset specificity.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_fuel_incumbents_transitioning, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, fossil_fuel_incumbents_transitioning, payer).

% Inherit the climate outcome of the mitigation-priority bet. If technological decoupling fails to achieve deep emissions cuts in time, they face unmanageable warming, irreversible ecosystem loss, and adaptation costs exceeding global GDP. They have no voice in current decisions, no exit from the planetary system, and no ability to retroactively change the trajectory chosen today.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations_if_decoupling_fails, payer,
    powerless, civilizational, trapped, universal).

% Low-income households, informal workers, and marginalized communities who face carbon price regressivity without access to clean alternatives (retrofitted housing, public transit, affordable EVs). They pay the transition costs through higher energy/food prices but cannot capture the benefits (subsidies, tax credits, green jobs) due to credit constraints, tenure insecurity, and spatial mismatch. Exit from vulnerability requires structural redistribution the current framework treats as optional.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, vulnerable_populations_locked_out_of_transition, payer,
    powerless, biographical, trapped, global).

% Workers in coal, oil, gas, and dependent communities where green job creation lags behind fossil job losses. Skills mismatch, geographic immobility, pension dependence, and community ties constrain exit. Just transition rhetoric exists but funding and implementation are consistently inadequate relative to the pace of phase-out. They bear concentrated costs of a transition whose benefits are diffuse.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_fuel_workers_without_transition_pathways, payer,
    moderate, biographical, constrained, regional).

% Developing nations facing carbon budget exhaustion before achieving energy access and industrialization. Carbon pricing and border adjustments (CBAM) raise their cost of capital and export competitiveness. Technology transfer and finance promises (Article 9, $100B/year) are chronically underdelivered. They are asked to leapfrog a fossil stage the wealthy world never leapfrogged, with insufficient support. Exit from the constraint requires rewriting global trade and finance rules they do not control.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_development_aspirations_constrained, payer,
    moderate, generational, constrained, global).

% IPCC and national academies producing consensus assessments that frame the mitigation pathway space. Their scenarios (SSP1-1.9, SSP1-2.6) embed the mitigation-priority reading's assumptions: large-scale CDR, rapid electrification, continued growth. They do not set policy but define the legitimate option space. Their authority rests on scientific credibility, which creates pressure to not undermine the paradigm that funds them.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_science_assessment_bodies, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reduction by aligning investment, innovation, and consumption around a carbon price signal and technology deployment targets, avoiding the tragedy of the commons in atmospheric absorption capacity.
% TRANSFER_FUNCTION: Moves transition costs from future generations (avoided climate damages) to current emitters and consumers via carbon prices; moves subsidies and guaranteed markets from public treasuries to clean technology capital; moves stranded asset risk from fossil incumbents to workers, communities, and balance sheets via managed phase-out.
% ABSENT_VOICES: Future generations (by definition), climate-displaced peoples not yet displaced, species and ecosystems with no standing, and global south negotiators who accept mitigation-priority framing because adaptation finance is the only finance on offer — they would demand reparative finance and technology sovereignty if not locked into the current bargaining structure.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished, carbon pricing regimes would collapse, clean tech investment would lose policy certainty, fossil assets would reprice upward, and the global climate governance architecture (Paris Agreement, NDCs, Article 6) would lose its coordinating logic. The world would reorganize around either unmanaged climate chaos or a radically different coordination framework (adaptation-priority or degrowth).
% FOUNDING_PROBLEM: How to achieve deep emissions reductions without triggering economic collapse or political backlash, given that fossil energy underpins the entire modern economic system and no agreed alternative existed at scale when the framework was codified (Rio 1992, Kyoto 1997, Paris 2015).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UNFCCC secretariat, IEA, and OECD as still live — emissions are still rising, decoupling is not yet absolute at global scale. Critics from the adaptation-priority and degrowth readings (Global South negotiators, climate justice movements, ecological economists) attest the problem has been mischaracterized: the real problem is distributional and biophysical, not merely technological, and the mitigation-priority framing serves to delay confronting that reality. No disinterested party attests the founding problem is solved.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48 at 2025) reflects that the constraint transfers substantial costs to those with least voice (future generations, vulnerable populations) while beneficiaries (clean tech capital, carbon pricing architects, transitioning incumbents) capture concentrated gains. Suppression (0.35) is moderate — the constraint operates more through incentive architecture and infrastructure lock-in than overt coercion, but border adjustments and subsidy conditionality are hardening enforcement. Theater ratio (0.42) is elevated and rising: net-zero pledges, carbon neutrality certifications, and ESG frameworks increasingly perform alignment while emissions trajectories diverge. Accessibility collapse (0.55) reflects that alternative framings (adaptation-priority, degrowth) are marginalized in official discourse and finance channels. Resistance (0.58) captures the growing pushback from climate justice movements, global south negotiating blocs, and communities facing transition burdens.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (carbon pricing architects), the constraint is a genuine coordination achievement: it has mobilized trillions in clean investment, established a global carbon price signal, and bent the emissions curve. From the victim seats (future generations, vulnerable populations), the same structure operates as a bet with their survival as collateral — the coordination function is real but the extraction is existential. The engine computes this seat divergence from the structural data; the claimed type (tangled_rope) reflects the author's judgment that both coordination and asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Carbon pricing architects and clean tech capital are structural beneficiaries (d near 0.1-0.2): they design and profit from the mechanism, with arbitrage-grade exit. Current consumers and transitioning incumbents sit near symmetric (d ~0.4-0.5): they gain coordination benefits but bear transition costs, with constrained exit. Future generations, vulnerable populations, fossil workers without pathways, and constrained global south development are structural targets (d near 0.8-0.95): they bear existential or severe costs with trapped or highly constrained exit. Climate science bodies are analytical observers (d=0.5 by construction). The gradient from beneficiary to victim maps onto power and exit differentials — the engine will compute effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (coordinate global mitigation) remains live but the extraction profile has worsened as the technological bet has proven harder than assumed (CDR scale-up delays, hard-to-abate sector stubbornness, rebound effects). The constraint avoids mandatrophy classification because the founding problem (how to reduce emissions without collapse) is still contested as live — but the rising theater ratio and extractiveness trajectory suggest the coordination function is being hollowed out while the extraction function intensifies. This is the tangled_rope zone: genuine coordination with accumulating asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_empirical,
    'Is absolute decoupling of global GDP from emissions achievable at the speed and scale required for 1.5°C/2°C pathways, or is the mitigation-priority reading betting on a biophysical impossibility?',
    'Empirical test: monitor absolute decoupling rates in major economies against IPCC pathway requirements. If global emissions do not peak before 2025 and fall ~7%/year thereafter, the bet fails and future generations enter the victim set decisively.',
    'If decoupling is biophysically infeasible at required rates, the constraint''s extractiveness toward future generations is not a risk but a certainty — reclassifying from tangled_rope toward snare for the future-generations seat. The coordination function would be revealed as a cover story for intergenerational extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility_empirical, empirical, 'Whether the core technological promise of the mitigation-priority reading is empirically viable.').

omega_variable(
    cdr_scale_up_risk,
    'Do the mitigation pathways (SSP1-1.9, SSP1-2.6) depend on carbon dioxide removal (CDR) deployment at scales that are physically, energetically, and socially plausible, or is CDR a phantom technology that makes the mitigation-priority reading appear feasible?',
    'Track CDR deployment (BECCS, DACCS, enhanced weathering) against pathway requirements. Assess land, water, energy, and justice constraints at gigatonne scale. If the gap persists, the reading''s feasibility rests on an unvalidated assumption.',
    'If CDR at required scale is implausible, the mitigation-priority reading''s coordination function is structurally dependent on a false premise — the constraint extracts from future generations by pretending a technological backstop exists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_scale_up_risk, empirical, 'Whether CDR dependence invalidates the mitigation-priority reading''s feasibility claim.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the mitigation-priority reading foreclose the adaptation_priority and degrowth_transformation readings within a single commitment framework, or do all three coexist as live positions held by different parties?',
    'Analyze whether any party (state, movement, institution) holds more than one reading simultaneously without contradiction. If the IPCC/UNFCCC framework structurally excludes adaptation-priority or degrowth framings from legitimate option space, foreclosure operates. If they merely marginalize them, coexistence operates.',
    'If foreclosure: the mitigation-priority reading''s authority depends on suppressing alternative framings — this is extraction via epistemic closure. If coexistence: the three readings are genuinely competing in a pluralistic discourse, and the constraint''s legitimacy is contestable but not structurally exclusionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Structural relationship between this reading and its sibling readings in the climate_response_legitimacy kernel.').

omega_variable(
    just_transition_implementation_gap,
    'Is the persistent gap between just transition rhetoric and implementation a feature (extraction from workers/communities is necessary for transition speed) or a bug (fixable with political will)?',
    'Compare transition funding allocated vs. needed across jurisdictions. Track fossil worker reemployment rates, community revitalization outcomes, and global south finance flows. If the gap is systematic across political systems, it is structural.',
    'If structural: the mitigation-priority reading''s extraction from fossil workers and global south is not incidental but necessary to its political economy — the constraint is more snare-like than tangled_rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_implementation_gap, empirical, 'Whether the just transition gap is structural or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 1992, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_legitimacy__mitigation_priority, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(clim_tr_t1997, climate_response_legitimacy__mitigation_priority, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(clim_tr_t2005, climate_response_legitimacy__mitigation_priority, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(clim_tr_t2009, climate_response_legitimacy__mitigation_priority, theater_ratio, 2009, 0.32).
narrative_ontology:measurement(clim_tr_t2015, climate_response_legitimacy__mitigation_priority, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(clim_tr_t2021, climate_response_legitimacy__mitigation_priority, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(clim_tr_t2025, climate_response_legitimacy__mitigation_priority, theater_ratio, 2025, 0.42).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__mitigation_priority, theater_ratio, 2030, 0.52).
narrative_ontology:measurement(clim_tr_t2035, climate_response_legitimacy__mitigation_priority, theater_ratio, 2035, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(clim_be_t1997, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1997, 0.22).
narrative_ontology:measurement(clim_be_t2005, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(clim_be_t2009, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(clim_be_t2015, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(clim_be_t2021, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(clim_be_t2025, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2030, 0.55).
narrative_ontology:measurement(clim_be_t2035, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2035, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1992, 0.12).
narrative_ontology:measurement(clim_su_t1997, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1997, 0.18).
narrative_ontology:measurement(clim_su_t2005, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2005, 0.22).
narrative_ontology:measurement(clim_su_t2009, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2009, 0.28).
narrative_ontology:measurement(clim_su_t2015, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(clim_su_t2021, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2021, 0.35).
narrative_ontology:measurement(clim_su_t2025, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2025, 0.35).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2030, 0.45).
narrative_ontology:measurement(clim_su_t2035, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2035, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, carbon_budget_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, cdr_deployment_governance).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, global_carbon_pricing_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel. The sibling readings (adaptation_priority, degrowth_transformation) instantiate different constraints with different beneficiary/victim structures and extractiveness profiles. The mitigation_priority reading has lower base extractiveness at t0 (1992) but rising trajectory as the technological bet's risks materialize; adaptation_priority likely has higher initial extractiveness (accepting warming locks in damages) but flatter trajectory; degrowth_transformation likely has high initial extractiveness (from current wealthy-nation consumers) but declining trajectory if transformation succeeds. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, powerful, 0.35).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, organized, 0.45).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, moderate, 0.75).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
