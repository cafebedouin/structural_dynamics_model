% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Structural Transformation for Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth_transformation reading of climate_response_action demands a
 *   deliberate, democratic contraction of material and energy throughput in
 *   wealthy economies to create ecological space for universal sufficiency
 *   provisioning. It rejects GDP growth as an organizing principle, treats
 *   technological substitution as necessary but insufficient, and centers
 *   redistribution (North→South, present wealthy→future generations,
 *   capital→labor) as the core climate policy lever. This reading faces
 *   intense political feasibility barriers because it threatens the asset
 *   values, institutional mandates, and identity structures of the most
 *   powerful actors in the global economy. The constraint is claimed as
 *   tangled_rope because it performs genuine coordination (solving the
 *   ecological allocation problem markets cannot) while simultaneously
 *   extracting from entrenched interests who wield structural power to
 *   suppress it — the coordination and extraction are inseparable in the same
 *   institutional structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.52).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Structural Transformation for Climate Response").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '6065203e-4984-4ced-8871-2e2545f41f70').
narrative_ontology:cs_kernel_codification('6065203e-4984-4ced-8871-2e2545f41f70', distributed).
narrative_ontology:cs_authority_grounding('6065203e-4984-4ced-8871-2e2545f41f70', distributed).
narrative_ontology:cs_reading_relation('6065203e-4984-4ced-8871-2e2545f41f70', climate_response_action__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('6065203e-4984-4ced-8871-2e2545f41f70', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('6065203e-4984-4ced-8871-2e2545f41f70', foundational, growth_imperative_incompatible_with_planetary_boundaries).
narrative_ontology:cs_axiom_status(growth_imperative_incompatible_with_planetary_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('6065203e-4984-4ced-8871-2e2545f41f70', growth_imperative_incompatible_with_planetary_boundaries, empirically_contingent).
narrative_ontology:cs_axiom('6065203e-4984-4ced-8871-2e2545f41f70', foundational, ecological_space_allocation_requires_deliberate_contraction_not_decoupling).
narrative_ontology:cs_axiom_status(ecological_space_allocation_requires_deliberate_contraction_not_decoupling, holdable).
narrative_ontology:cs_axiom_grounding('6065203e-4984-4ced-8871-2e2545f41f70', ecological_space_allocation_requires_deliberate_contraction_not_decoupling, empirically_contingent).
narrative_ontology:cs_axiom('6065203e-4984-4ced-8871-2e2545f41f70', secondary, global_north_consumption_reduction_is_necessary_for_global_south_development).
narrative_ontology:cs_axiom_status(global_north_consumption_reduction_is_necessary_for_global_south_development, holdable).
narrative_ontology:cs_axiom_grounding('6065203e-4984-4ced-8871-2e2545f41f70', global_north_consumption_reduction_is_necessary_for_global_south_development, deontological).
narrative_ontology:cs_reference_frame('6065203e-4984-4ced-8871-2e2545f41f70', post_paris_agreement_fragmented_governance).
narrative_ontology:cs_drift_state('6065203e-4984-4ced-8871-2e2545f41f70', post_ipcc_ar6_wg3_insufficient_mitigation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6065203e-4984-4ced-8871-2e2545f41f70', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_development_rights_holders).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, low_income_workers_high_emission_sectors).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_global_north_households).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_capital_asset_holders).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, growth_dependent_financial_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, climate_policy_establishment).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, low_income_workers_high_emission_sectors).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundaries_hard_constraint).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, sufficiency_over_efficiency_principle).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, intergenerational_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim atmospheric space and development rights currently occupied by Global North overconsumption. Would gain policy space for universal basic services, energy access, and sovereign development pathways if degrowth redistribution materializes. Exit is constrained by existing trade, debt, and intellectual property regimes that lock in extractive relationships.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_rights_holders, beneficiary,
    organized, generational, constrained, global).

% Bear the accumulated climate damages of present emissions without political representation. The constraint's success means a habitable planet; its failure means escalating catastrophes. No exit possible — they are the constitutive trapped constituency of intergenerational ethics.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Currently dependent on high-emission industries for wages. Would benefit from just transition programs (working time reduction, universal basic services, democratic firm ownership) but face immediate income disruption during restructuring. Their exit options are constrained by skill specificity, geographic immobility, and absence of alternative livelihood infrastructure.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, low_income_workers_high_emission_sectors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, low_income_workers_high_emission_sectors, payer).

% Top 10% global income earners whose lifestyle emissions drive overshoot. Would face absolute consumption reduction (not just decarbonization) — less air travel, smaller dwellings, reduced meat/dairy, constrained discretionary spending. Exit is mobile: they can deflect policy pressure through capital mobility, political capture, and consumption displacement to service economies.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, high_consumption_global_north_households, payer,
    powerful, biographical, mobile, global).

% Owners of stranded asset risk: fossil reserves, extraction infrastructure, combustion-dependent capital stock. Face deliberate devaluation through supply-side restriction and demand contraction. Exit via arbitrage: asset shifting to jurisdictions with weaker climate policy, financial engineering to socialize losses, and political lobbying to delay enforcement.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_capital_asset_holders, payer,
    institutional, biographical, arbitrage, global).

% Central banks, pension funds, insurance companies, and commercial banks whose balance sheets and mandate structures require perpetual GDP growth for debt service, asset valuation, and solvency. Degrowth implies managed debt restructuring, monetary architecture redesign, and fiduciary duty reform. Exit is constrained by systemic interlocking — no single institution can exit the growth imperative alone.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_financial_institutions, payer,
    institutional, generational, constrained, global).

% IPCC, UNFCCC secretariat, national climate ministries, and allied NGOs whose institutional legitimacy and funding flows are built on the mitigation_priority framing (technological substitution, carbon markets, green growth). They administer the current constraint regime and capture status/rents from it, but face legitimacy erosion as emissions trajectories diverge from targets.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_policy_establishment, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, climate_policy_establishment, beneficiary).

% Social movements, ecological economists, and political ecologies advocating structural transformation. Their professional and activist identities are fused to this reading — exit would mean abandoning the core coherence of their life-project. They set the alternative agenda but lack institutional leverage to implement it.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_movements_and_scholars, agenda_setter,
    organized, generational, identity_locked, global).

% Formally present in UNFCCC but structurally excluded from setting the global mitigation agenda — their demand for development space and climate finance is subordinated to Global North net-zero timelines and carbon market architectures. Would object to both mitigation_priority (insufficient transfer) and adaptation_priority (accepts damages as given) if their developmental sovereignty were centered.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_governments, excluded,
    organized, generational, constrained, global).

% Provide the biophysical boundary conditions (carbon budgets, tipping points, planetary boundaries) that all three readings reference but interpret differently. Their analytical seat is structurally independent — they do not collect rents or bear policy costs, but their credibility is contested by all sides.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, earth_system_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a deliberate, democratic contraction of material throughput in wealthy economies to create ecological space for sufficiency provisioning globally — solving the allocation problem that markets and growth-based governance cannot: how to live well within planetary boundaries without relying on speculative decoupling or carbon removal.
% TRANSFER_FUNCTION: Moves ecological budget (atmospheric space, biocapacity, mineral throughput) from high-consumption Global North populations and fossil capital to Global South development needs and future generations' survival requirements. Transfers political power from growth-dependent institutions to democratic sufficiency governance. Transfers time from wage labor to care/reproductive/ecological work via working time reduction.
% ABSENT_VOICES: Global South governments and movements demanding climate reparations and development sovereignty are formally in the room (UNFCCC) but structurally excluded from agenda-setting — their veto over false solutions is not recognized. Indigenous peoples guarding 80% of remaining biodiversity are consulted but not empowered. The global poor who never consented to the growth bargain have no seat.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation constraint vanished overnight, the default trajectory is mitigation_priority's green growth techno-fix (continued overshoot, reliance on BECCS/DAC, Global North consumption protected) or adaptation_priority's managed retreat (accepting 3°C+ with fortress adaptation). The world does not stay the same — the constraint is the only active proposal that reallocates ecological space justly rather than managing scarcity unjustly.
% FOUNDING_PROBLEM: The post-WWII global economic order organized around GDP growth as the universal metric of progress, locking in fossil-fueled throughput expansion, colonial resource extraction, and the externalization of ecological costs to the Global South and future generations. Climate breakdown is the planetary boundary violation of this specific growth imperative — not a generic 'market failure' but the metabolic rift of capitalism's growth requirement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) Earth system science (Rockström et al. planetary boundaries, IPCC carbon budgets) showing growth-driven throughput exceeds regenerative capacity; (2) Global South governments' consistent positions at UNFCCC (CBDR-RC, climate finance, loss and damage) attesting the growth model denies their development rights; (3) Ecological economics literature (Daly, Georgescu-Roegen, Jackson, Hickel, Kallis) demonstrating growth-decoupling is empirically insufficient at required speed/scale; (4) Historical scholarship (Malm, Hornborg, Moore) documenting the fossil-growth-colonialism nexus. No corroboration from growth-dependent institutions — their dissent is the extraction signal.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the magnitude of resource/wealth transfer required: absolute consumption reduction for the global top 10%, stranded asset devaluation for fossil capital, monetary architecture redesign for financial institutions. Suppression (0.52) is moderate — the constraint is not yet actively enforced at scale (it is a counter-hegemonic proposal), but the structural power of growth-dependent institutions creates de facto suppression through agenda exclusion, funding denial, and epistemic marginalization. Theater ratio (0.28) is low-moderate: the degrowth movement's internal practices (conferences, publications, local experiments) are mostly functional, but some performative signaling exists. Accessibility collapse (0.42) is moderate — alternatives (green growth, techno-optimism) remain cognitively accessible but are empirically weakening. Resistance (0.71) is high: the constraint meets fierce opposition from all growth-dependent power centers.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is extreme: from the Global South/future generations seat, this is a rope (genuine coordination solving an existential allocation problem); from the fossil capital/financial institution seat, it is a snare (pure extraction threatening asset values); from the degrowth advocate seat, it is a scaffold (transitional coordination toward a post-growth steady state); from the climate establishment seat, it is a piton (degraded remnant of 1970s limits-to-growth discourse, maintained theatrically). The engine computes this from the structural data — the authored claim (tangled_rope) captures the tension at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South development rights holders and future generations are structural beneficiaries (d ≈ 0.15–0.25) — the constraint subsidizes their ecological space and survival prospects. Low-income workers in high-emission sectors are dual-positioned: beneficiaries of just transition (d ≈ 0.35) but payers of transition disruption (d ≈ 0.65). High-consumption Global North households are primary payers (d ≈ 0.85) — absolute consumption loss with mobile exit. Fossil capital and financial institutions are payers with arbitrage/constrained exit (d ≈ 0.75–0.80). Climate policy establishment are agenda_setters who benefit from the status quo (d ≈ 0.30) but face legitimacy erosion. Degrowth advocates are identity-locked agenda_setters (d ≈ 0.20) — their exit is existentially costly. Earth system scientists are analytical observers (d ≈ 0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (growth imperative as metabolic rift) is live and worsening — planetary boundaries are being transgressed faster, not slower. The mitigation_priority reading (green growth) has had 30+ years to demonstrate decoupling at scale and has failed; its mandate has atrophied into piton/theater. The degrowth_transformation reading has never been institutionally implemented — it has no mandate to atrophy. Its mandatrophy question is inverted: not 'has its function atrophied?' but 'will it ever get a mandate before the window closes?' The constraint prevents mislabeling by making the coordination function (ecological space allocation) and extraction function (redistribution from powerful) structurally explicit — they are the same operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_threshold,
    'What minimum political coalition could enact degrowth transformation, and is it achievable within the carbon budget timeframe?',
    'Historical analysis of rapid socioeconomic transformations (wartime mobilization, post-colonial reconstruction, neoliberal counterrevolution) to identify coalition structures and tempo; agent-based modeling of policy diffusion under cascading climate impacts.',
    'If feasible coalition exists, the constraint is a scaffold (transitional coordination); if not, it remains a contested tangled_rope with high suppression and low implementation probability — possibly reclassifying as a snare (extraction cover for continued inaction) if co-opted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_threshold, empirical, 'Whether the constraint''s coordination function can overcome the structural power of its victims').

omega_variable(
    sufficiency_provisioning_capacity,
    'Can universal basic services, working time reduction, and democratic firm ownership actually deliver well-being at radically lower throughput, or does sufficiency provisioning require a minimum throughput floor that conflicts with planetary boundaries?',
    'Biophysical modeling of material/energy requirements for decent living standards (DLS) at global scale; empirical studies of existing low-throughput high-wellbeing societies (Costa Rica, Kerala, Cuba pre-Special Period); thermodynamic analysis of service provisioning systems.',
    'If sufficiency requires throughput above boundaries, the constraint''s coordination function fails — it becomes a snare (false promise). If sufficiency is achievable within boundaries, the coordination function is genuine and the extraction from high-consumption populations is the price of justice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_provisioning_capacity, empirical, 'Whether the constraint''s promised coordination outcome is biophysically realizable').

omega_variable(
    reading_foreclosure_boundary,
    'Does the degrowth_transformation reading logically foreclose the mitigation_priority reading within a single policy framework, or can they coexist as sequential phases (degrowth in North, green growth in South)?',
    'Formal analysis of policy package compatibility: can carbon pricing + technology deployment + growth metrics coexist with throughput caps + working time reduction + sufficiency metrics in one governance system? Historical study of policy layering vs. regime replacement.',
    'If forecloses, the kernel has a genuine structural split — the readings cannot be reconciled in one framework (cs_structure.reading_relations = forecloses). If coexists_with, the kernel permits hybrid trajectories (current UNFCCC ambiguity). If influences, degrowth framing shifts the Overton window for mitigation_policy without replacing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Structural relationship between this reading and the dominant mitigation_priority sibling').

omega_variable(
    identity_lock_mechanism_degrowth_advocates,
    'Is the identity_locked exit of degrowth advocates professional (career path dependence), relational (movement membership), ideological (worldview fusion), or institutional (NGO/academic position)?',
    'Sociological study of ecological economics/degrowth field: career trajectories, funding dependencies, organizational affiliations, and self-narratives of key actors. Compare with identity_lock mechanisms in other advocacy fields.',
    'If primarily professional/ideological, the lock is brittle — paradigm shift could release it. If relational/institutional, the lock is structural — the movement''s survival depends on the constraint''s persistence. Affects whether the constraint''s agenda_setter seat is genuinely independent or self-reproducing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_degrowth_advocates, empirical, 'Mechanism binding degrowth advocates to this reading''s persistence').

omega_variable(
    kernel_framing_underdetermination,
    'Is the climate_response_action kernel best framed as (a) a policy choice among technological options, (b) a constitutional-order question about the growth imperative, or (c) a civilizational survival threshold?',
    'Comparative analysis of how each framing structures the stakeholder set, the metrics, and the classification outcome. Test whether reframing changes the constraint''s type (e.g., framing (a) makes mitigation_priority a rope; framing (b) makes degrowth a tangled_rope; framing (c) makes all readings scaffolds).',
    'If framing (b) or (c) is structurally correct, the mitigation_priority reading is a false summit (mountain claim with beneficiaries). The kernel''s codification type in cs_structure depends on this framing choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is framed as technical, constitutional, or existential — changes all downstream classifications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 1992, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_degrowth_tr_t1992, climate_response_action__degrowth_transformation, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(climate_degrowth_tr_t2000, climate_response_action__degrowth_transformation, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(climate_degrowth_tr_t2009, climate_response_action__degrowth_transformation, theater_ratio, 2009, 0.18).
narrative_ontology:measurement(climate_degrowth_tr_t2015, climate_response_action__degrowth_transformation, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(climate_degrowth_tr_t2021, climate_response_action__degrowth_transformation, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(climate_degrowth_tr_t2030, climate_response_action__degrowth_transformation, theater_ratio, 2030, 0.27).
narrative_ontology:measurement(climate_degrowth_tr_t2040, climate_response_action__degrowth_transformation, theater_ratio, 2040, 0.28).
narrative_ontology:measurement(climate_degrowth_tr_t2050, climate_response_action__degrowth_transformation, theater_ratio, 2050, 0.28).

% Extraction over time
narrative_ontology:measurement(climate_degrowth_be_t1992, climate_response_action__degrowth_transformation, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(climate_degrowth_be_t2000, climate_response_action__degrowth_transformation, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(climate_degrowth_be_t2009, climate_response_action__degrowth_transformation, base_extractiveness, 2009, 0.31).
narrative_ontology:measurement(climate_degrowth_be_t2015, climate_response_action__degrowth_transformation, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(climate_degrowth_be_t2021, climate_response_action__degrowth_transformation, base_extractiveness, 2021, 0.54).
narrative_ontology:measurement(climate_degrowth_be_t2030, climate_response_action__degrowth_transformation, base_extractiveness, 2030, 0.61).
narrative_ontology:measurement(climate_degrowth_be_t2040, climate_response_action__degrowth_transformation, base_extractiveness, 2040, 0.65).
narrative_ontology:measurement(climate_degrowth_be_t2050, climate_response_action__degrowth_transformation, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_degrowth_su_t1992, climate_response_action__degrowth_transformation, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement(climate_degrowth_su_t2000, climate_response_action__degrowth_transformation, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(climate_degrowth_su_t2009, climate_response_action__degrowth_transformation, suppression_requirement, 2009, 0.33).
narrative_ontology:measurement(climate_degrowth_su_t2015, climate_response_action__degrowth_transformation, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(climate_degrowth_su_t2021, climate_response_action__degrowth_transformation, suppression_requirement, 2021, 0.48).
narrative_ontology:measurement(climate_degrowth_su_t2030, climate_response_action__degrowth_transformation, suppression_requirement, 2030, 0.52).
narrative_ontology:measurement(climate_degrowth_su_t2040, climate_response_action__degrowth_transformation, suppression_requirement, 2040, 0.52).
narrative_ontology:measurement(climate_degrowth_su_t2050, climate_response_action__degrowth_transformation, suppression_requirement, 2050, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, global_financial_architecture_growth_imperative).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, intellectual_property_trade_regime).

% DUAL FORMULATION NOTE:
% This constraint (degrowth_transformation) and its siblings (mitigation_priority, adaptation_priority) form the climate_response_action constraint family. All three share the kernel_id 'climate_response_action' but instantiate different constraints with different ε, beneficiaries, victims, and types. mitigation_priority claims rope (coordination via markets/tech) but has high extractiveness from Global South/future generations — likely false_summit_mountain or tangled_rope. adaptation_priority claims scaffold (transitional resilience) but lacks sunset clause and has extraction from vulnerable populations — likely snare. This reading claims tangled_rope with explicit redistribution. The ε values diverge because each reading's referent (the standing arrangement it contests) is structurally different: mitigation_priority contests 'insufficient green investment'; degrowth contests 'growth imperative as metabolic rift'; adaptation_priority contests 'unpreparedness for locked-in warming'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, institutional, 0.35).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, organized, 0.25).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
