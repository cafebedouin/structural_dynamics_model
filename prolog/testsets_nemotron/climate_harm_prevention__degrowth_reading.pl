% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction in Global North as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth_reading of the
 *   climate_harm_prevention kernel. It asserts that legitimate climate
 *   response requires planned economic contraction in the Global North
 *   because mitigation within the growth framework is physically and
 *   politically impossible. The constraint coordinates a just descent of
 *   high-consumption economies while transferring atmospheric space to the
 *   Global South and future generations. It is a tangled rope: genuine
 *   coordination function (solving the carbon budget collective action
 *   problem) fused with asymmetric extraction (Global North consumers,
 *   workers, and asset holders bear concentrated costs). Active enforcement
 *   would be required: carbon rationing, production caps, border adjustments,
 *   capital controls, and just-transition guarantees — all resisted by
 *   powerful incumbents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.55).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction in Global North as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '1aba0b24-f6b6-468a-92a3-0e244fb42f21').
narrative_ontology:cs_kernel_codification('1aba0b24-f6b6-468a-92a3-0e244fb42f21', distributed).
narrative_ontology:cs_authority_grounding('1aba0b24-f6b6-468a-92a3-0e244fb42f21', extraction).
narrative_ontology:cs_reading_relation('1aba0b24-f6b6-468a-92a3-0e244fb42f21', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('1aba0b24-f6b6-468a-92a3-0e244fb42f21', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('1aba0b24-f6b6-468a-92a3-0e244fb42f21', foundational, absolute_decoupling_physically_impossible_at_required_rates).
narrative_ontology:cs_axiom_status(absolute_decoupling_physically_impossible_at_required_rates, holdable).
narrative_ontology:cs_axiom_grounding('1aba0b24-f6b6-468a-92a3-0e244fb42f21', absolute_decoupling_physically_impossible_at_required_rates, empirically_contingent).
narrative_ontology:cs_axiom('1aba0b24-f6b6-468a-92a3-0e244fb42f21', foundational, intergenerational_justice_requires_north_contraction).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_north_contraction, holdable).
narrative_ontology:cs_axiom_grounding('1aba0b24-f6b6-468a-92a3-0e244fb42f21', intergenerational_justice_requires_north_contraction, deontological).
narrative_ontology:cs_reference_frame('1aba0b24-f6b6-468a-92a3-0e244fb42f21', growth_paradigm_as_climate_policy_boundary).
narrative_ontology:cs_drift_state('1aba0b24-f6b6-468a-92a3-0e244fb42f21', post_paris_agreement_implementation_gap, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1aba0b24-f6b6-468a-92a3-0e244fb42f21', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, biodiversity_ecosystems).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_labor_dependent_on_high_carbon_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_asset_holders_in_fossil_intensive_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_governments).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, physical_limits_constrain_social_possibility).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, intergenerational_justice_requires_sacrifice_of_present_consumption).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, climate_stabilization_is_incompatible_with_perpetual_growth_in_high_consumption_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the least historical responsibility for emissions but face the most severe climate impacts. Benefit from a global contraction that reduces emissions pressure and creates atmospheric space. Exit options are constrained by structural dependence on global trade, finance, and technology flows dominated by Global North.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    moderate, biographical, constrained, global).

% Inherit the climate system shaped by present decisions. Benefit maximally from avoided catastrophic warming. Have zero exit options and no voice in present arrangements — structurally trapped in the consequences of current constraint choices.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Non-human living systems that stabilize planetary conditions. Benefit from reduced habitat destruction, pollution, and climate disruption. No agency, no exit, no voice — pure structural beneficiary of contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, biodiversity_ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, biodiversity_ecosystems).

% High per-capita consumers in wealthy nations whose lifestyle (meat-heavy diets, frequent flying, large dwellings, car dependence, fast fashion, digital intensity) drives disproportionate emissions. Would bear direct lifestyle contraction: reduced material throughput, energy descent, behavioral limits. Exit is constrained by infrastructure lock-in, cultural norms, and the political unpopularity of contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    powerful, immediate, constrained, global).

% Workers in fossil extraction, heavy industry, aviation, automotive, industrial agriculture, and associated supply chains. Face job loss, community dissolution, identity rupture. Organized through unions with political leverage but constrained by sector-specific human capital and geographic immobility. Exit requires retraining, relocation, and social protection that are politically contested.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_labor_dependent_on_high_carbon_industries, payer,
    organized, biographical, constrained, national).

% Owners of stranded-asset-risk capital: fossil reserves, combustion infrastructure, high-carbon real estate, equity in carbon-intensive firms. Bear capital devaluation under contraction. High exit mobility via portfolio reallocation, political lobbying, and jurisdictional arbitrage — but face collective action problem: individual exit accelerates the devaluation they fear.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_asset_holders_in_fossil_intensive_sectors, payer,
    institutional, biographical, mobile, global).

% Sovereign authorities that would need to plan, legislate, and enforce contraction: carbon rationing, production caps, trade restructuring, social guarantees. Face electoral punishment, capital flight risk, geopolitical competitiveness pressure, and legitimacy crises. Constrained by treaty obligations (WTO, investment agreements), voter preferences, and the growth-dependence of state revenue and employment.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_governments, payer).

% Epistemic authority assessing carbon budgets, mitigation pathways, and physical feasibility. Provides the evidence base that contraction is physically necessary within tight timelines. No material stake in the constraint's distributional outcomes; exit is analytical (revising models).
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_science_ipcc, observer,
    analytical, civilizational, analytical, universal).

% Would demand contraction in the North as climate justice but are excluded from setting the agenda in Northern capitals. Their leverage is moral authority, bloc coordination (G77, AOSIS), and the physical reality of impacts — but they do not write the laws that would implement contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_governments, excluded,
    moderate, biographical, constrained, national).

% Organized political voice of future generations in the present. Demands contraction as intergenerational justice. Identity-locked: their political self-concept is constituted through this demand; exit would dissolve their organizational rationale. Excluded from formal decision-making despite being the constituency most affected.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, youth_climate_movements, excluded,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a rapid, equitable descent of material throughput in high-consumption economies to remain within a shrinking carbon budget, while preventing chaotic collapse and guaranteeing universal basic services. Solves the collective action problem of who cuts first and deepest by assigning contraction to those with historical responsibility and capacity.
% TRANSFER_FUNCTION: Transfers atmospheric space (remaining carbon budget) and ecological carrying capacity from Global North present consumption to Global South development needs and future generations' survival. Transfers economic security from high-carbon sector workers and asset holders to a just-transition fund (theoretically). Transfers political legitimacy from growth-dependent governments to contraction-implementing authorities.
% ABSENT_VOICES: Global South governments and youth movements are formally excluded from Northern legislative agendas. Non-human nature has no voice. The global poor within the Global North (who would bear regressive impacts of contraction without adequate social protection) are often absent from degrowth policy design.
% DISAPPEARANCE_RATIONALE: If the degrowth constraint vanished overnight, emissions would continue on a growth-compatible trajectory, carbon budgets would be exceeded, and the physical climate system would reorganize toward 3°C+ warming. The political economy would rearrange around unmanaged climate breakdown rather than managed contraction — a different, more catastrophic rearrangement.
% FOUNDING_PROBLEM: The founding problem is the biophysical impossibility of decoupling GDP growth from material throughput and emissions at the speed and depth required by carbon budgets, combined with the political impossibility of Global North governments voluntarily accepting contraction within electoral cycles. The arrangement was built (conceptually) to solve the double bind: physics demands contraction, politics refuses it.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) states that 'demand-side measures and new ways of end-use service provision can reduce global GHG emissions in end-use sectors by 40–70% by 2050' and that 'absolute reductions in GDP' are not modeled in mainstream pathways. Independent ecological economists (Hickel, Kallis, Jackson, Parrique) corroborate that no empirical evidence exists for absolute decoupling at required rates. The Global North policy establishment (OECD, IEA, national governments) contests this, asserting green growth is feasible — no corroboration from outside the beneficiary set of the growth paradigm.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint demands a massive transfer of material welfare from Global North present to Global South/future beneficiaries. Suppression (0.55) is moderate-high because the constraint's persistence depends on overcoming fierce political resistance from institutional power (governments, capital, organized labor in high-carbon sectors). Theater ratio (0.38) reflects that much 'climate policy' performs green growth while emissions rise — the gap between declared ambition and material reality grows over time. Accessibility collapse (0.62) is elevated because once the carbon budget logic is accepted, alternatives (green growth, techno-optimism) collapse as physically implausible. Resistance (0.71) is high because the constraint attacks the core of the growth-dependent political settlement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Global North consumers, workers, asset holders) experience this as a snare: extraction without consent, enforced by a state they don't control. The beneficiary seats (Global South, future generations) experience it as a rope: genuine coordination solving an existential collective action problem. The agenda_setter seat (Global North governments) experiences it as a piton: a constraint they must administer but which destroys their electoral coalition. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations, future generations, and ecosystems are structural beneficiaries (d near 0.0) — they receive atmospheric space and avoided catastrophe. Global North consumers, high-carbon workers, and fossil asset holders are structural targets (d near 1.0) — they bear the contraction. Global North governments are agenda_setters but also payers (dual role): they must implement the constraint that threatens their legitimacy. Climate science is analytical observer (d=0.5). Global South governments and youth movements are excluded — their structural position would be beneficiary-adjacent but they lack agenda-setting power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by making the coordination function explicit: without planned contraction, the carbon budget collective action problem has no solution — unmanaged collapse is the alternative. The extraction is not gratuitous; it is the price of coordination. But the extraction is asymmetric and concentrated, requiring active enforcement. This is why tangled_rope, not rope or snare alone. Mandatrophy is unresolved: the founding problem (physics vs. politics double bind) is live, but the arrangement to solve it has not been built — only theorized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_decoupling_feasibility,
    'Is absolute decoupling of GDP from material throughput and emissions at the rate required by 1.5°C/2°C carbon budgets physically possible?',
    'Empirical tracking of decoupling rates in high-income economies vs. required rates from carbon budgets; integrated assessment model sensitivity to decoupling assumptions.',
    'If absolute decoupling at required rates is physically possible, the degrowth_reading''s core premise fails and the constraint''s coordination function dissolves — it becomes a snare (extraction without coordination necessity). If impossible, the coordination function is genuine and the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'Physical feasibility of green growth at climate-relevant speed and depth.').

omega_variable(
    political_feasibility_of_contraction,
    'Can Global North governments implement planned contraction without losing democratic legitimacy or being replaced by anti-contraction forces?',
    'Historical analysis of austerity politics, wartime mobilization analogs, and emerging climate policy backlash (e.g., yellow vests, farmer protests, EV mandate resistance).',
    'If contraction is politically impossible, the constraint is a snare (extraction without viable coordination mechanism) or a piton (theorized but never implemented). If feasible under crisis conditions, the tangled_rope coordination function may activate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_contraction, conceptual, 'Whether the constraint''s enforcement mechanism can ever be politically instantiated.').

omega_variable(
    just_transition_credibility,
    'Would a just transition actually protect Global North high-carbon workers and vulnerable consumers, or is ''just transition'' a performative cover for extraction?',
    'Ex post analysis of transition policies in coal regions (Germany, Spain, Poland, US Appalachia); modeling of universal basic services funding under contraction.',
    'If just transition is credible, the extraction from workers/consumers is partially compensated — the tangled_rope''s asymmetric extraction is moderated. If not, the constraint extracts from the vulnerable without redress — shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_credibility, empirical, 'Whether the constraint''s burden-sharing mechanism is real or theatrical.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this degrowth_reading structurally relate to the mitigation_priority and adaptation_priority readings of the climate_harm_prevention kernel?',
    'Structural mapping of each reading''s ε, beneficiary/victim sets, and founding problem to identify foreclosure, coexistence, or influence relations.',
    'If degrowth_reading forecloses mitigation_priority (physical impossibility of green growth), the kernel has a genuine logical split. If they coexist, the kernel hosts a live political contest. The engine uses this to compute cross-reading contamination and foreclosure dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committee frame: this constraint is one reading of climate_harm_prevention kernel; sibling readings are mitigation_priority and adaptation_priority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__degrowth_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__degrowth_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__degrowth_reading, theater_ratio, 2025, 0.29).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__degrowth_reading, theater_ratio, 2030, 0.34).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__degrowth_reading, theater_ratio, 2035, 0.36).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__degrowth_reading, theater_ratio, 2040, 0.37).
narrative_ontology:measurement(clim_tr_t2045, climate_harm_prevention__degrowth_reading, theater_ratio, 2045, 0.37).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__degrowth_reading, theater_ratio, 2050, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__degrowth_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__degrowth_reading, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__degrowth_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__degrowth_reading, base_extractiveness, 2030, 0.63).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__degrowth_reading, base_extractiveness, 2035, 0.66).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__degrowth_reading, base_extractiveness, 2040, 0.67).
narrative_ontology:measurement(clim_be_t2045, climate_harm_prevention__degrowth_reading, base_extractiveness, 2045, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__degrowth_reading, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__degrowth_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__degrowth_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__degrowth_reading, suppression_requirement, 2025, 0.47).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__degrowth_reading, suppression_requirement, 2030, 0.51).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__degrowth_reading, suppression_requirement, 2035, 0.53).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__degrowth_reading, suppression_requirement, 2040, 0.54).
narrative_ontology:measurement(clim_su_t2045, climate_harm_prevention__degrowth_reading, suppression_requirement, 2045, 0.55).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__degrowth_reading, suppression_requirement, 2050, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, global_carbon_budget_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, just_transition_governance).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, international_climate_finance_obligations).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth_reading of the climate_harm_prevention kernel. It differs structurally from mitigation_priority (which claims growth-compatible mitigation is feasible, yielding lower extractiveness ~0.35, beneficiaries = green tech sectors, victims = fossil incumbents) and adaptation_priority (which accepts higher warming, yielding different victim/beneficiary sets: present Global North consumers benefit from avoided contraction costs, future generations and Global South bear adaptation burdens). The ε values differ because the physical referents differ: degrowth_reading assesses extraction against the standing growth paradigm; mitigation_priority assesses against a hypothetical green growth trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, institutional, 0.65).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, organized, 0.75).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, powerful, 0.85).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, moderate, 0.25).
constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
