% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Carbon-Neutral Portfolio Optimization Imperative (Nuclear-Necessary Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   Climate mitigation requires rapid decarbonization of electricity grids.
 *   The portfolio-optimization reading holds that decarbonization requires
 *   maximizing ALL low-carbon sources simultaneously—renewables, nuclear,
 *   hydro, storage, demand response—because no single source can provide both
 *   intermittency-free baseload AND achieve deep decarbonization fast enough.
 *   Under this reading, nuclear receives policy support (extended licenses,
 *   capacity subsidies, grid dispatch priority) justified as operationally
 *   necessary for reliable low-carbon grids. This reading instantiates a
 *   tangled-rope structure: genuine coordination function (align
 *   physics/engineering/economic constraints) AND asymmetric extraction
 *   (nuclear operators and utilities benefit; fossil producers and coal
 *   communities bear stranded-asset costs). The constraint's persistence
 *   depends on active enforcement: grid operators must prioritize nuclear
 *   dispatch, regulators must approve capacity payments, policymakers must
 *   prevent fossil-fuel revival. This is ONE reading of the contested kernel
 *   'climate_mitigation_imperative'; sibling readings (opportunity-cost,
 *   systems-transition) offer structurally different framings that would
 *   produce different beneficiary/victim allocations and different
 *   classifications.
 *
 * KEY AGENTS:
 *   - nuclear_operators: Structural beneficiary under this reading (receive subsidy and guaranteed dispatch). Power=institutional, exit=arbitrage (can redeploy to other jurisdictions or technologies if home-market support erodes).
 *   - fossil_fuel_producers: Structural victim (reserves become unmarketable, investment stranded). Power=powerful, exit=constrained (core asset class is written down globally by the carbon imperative).
 *   - coal_mining_regions: Structural victim bearing distributed, long-tail costs (employment collapse, community deprivation). Power=moderate→powerless (geographic concentration, skill specificity, identity-lock). Exit=trapped (relocation and retraining are theoretically available but practically identity-locked by generational occupation).
 *   - grid_operators: Agenda-setter (enforce the dispatch rules and interconnection standards that instantiate the reading). Power=institutional, exit=analytical (operate under government mandate but can propose alternative dispatch algorithms).
 *   - systems_transition_advocates: Excluded from the constraint's framing (their decentralization reading would foreclose the nuclear-necessary premise). Power=moderate, exit=constrained (can lobby and propose alternative scenarios but lack formal voice in dispatch authority).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Carbon-Neutral Portfolio Optimization Imperative (Nuclear-Necessary Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'c09d17b1-7f5e-493d-aafe-cdf08dfc1f52').
narrative_ontology:cs_kernel_codification('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', formalized).
narrative_ontology:cs_authority_grounding('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', distributed).
narrative_ontology:cs_reading_relation('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', foundational, baseload_necessity_for_decarbonization).
narrative_ontology:cs_axiom_status(baseload_necessity_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', baseload_necessity_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', foundational, nuclear_low_carbon_technology).
narrative_ontology:cs_axiom_status(nuclear_low_carbon_technology, holdable).
narrative_ontology:cs_axiom_grounding('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', nuclear_low_carbon_technology, empirically_contingent).
narrative_ontology:cs_axiom('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', secondary, all_sources_maximization_imperative).
narrative_ontology:cs_axiom_status(all_sources_maximization_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', all_sources_maximization_imperative, instrumental).
narrative_ontology:cs_reference_frame('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', rapid_decarbonization_via_diversified_low_carbon_portfolio).
narrative_ontology:cs_drift_state('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', contemporary_storage_technology_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c09d17b1-7f5e-493d-aafe-cdf08dfc1f52', '2026-06-19T14:23:45Z').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, large_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, industrial_heat_users).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, coal_mining_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, natural_gas_infrastructure_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, large_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, low_income_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, decarbonization_physics_constraint).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_reliability_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing or planned nuclear plants. Under portfolio-optimization framing, receive policy support (extended license periods, subsidized capacity auctions, grid priority scheduling) justified as necessary to maximize low-carbon baseload. Collect rents from guaranteed dispatch and capacity payments. Can shift investment to other jurisdictions or technologies if home-market support erodes, but nuclear-specific infrastructure creates lock-in.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Operate both fossil and nuclear fleets. Portfolio-optimization framing allows them to retire coal capacity (stranded assets, rising carbon costs) while capturing nuclear rents and hedging intermittency of renewable sources. They bear compliance costs for retiring fossil capacity but offset through nuclear revenue. Their scale and regulated-utility status (cost-plus pass-through) lets them diffuse stranded-asset losses across ratepayers.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, large_utilities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, large_utilities, payer).

% Energy-intensive manufacturers (steel, cement, chemicals) seeking carbon-compliant heat sources for process temperatures. Under portfolio-optimization reading, they benefit from nuclear-supplied industrial heat and steam (via district systems or dedicated plants) as a zero-carbon alternative to natural gas or coal, enabling them to claim carbon neutrality without asset stranding or offshoring.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, industrial_heat_users, beneficiary,
    powerful, biographical, mobile, global).

% Coal and natural-gas producers face stranded reserves and falling demand under carbon-intensity-based portfolio mandates. The constraint allocates carbon budget to renewables and nuclear, which implicitly excludes fossil fuels. They cannot exit: their core asset class (fossil reserves) becomes unmarketable. Litigation and lobbying are their primary exits, both expensive and containable by regulators.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers, payer,
    powerful, generational, constrained, global).

% Communities structurally dependent on coal extraction and coal-fired plant employment. Portfolio-optimization framing accelerates coal plant closures, eliminating primary employment without guaranteed retraining or transition support. Labor force specificity (coal skills do not transfer easily) and geographic concentration (mines are not mobile) create economic collapse risk. Formal exit options (relocation, retraining) are theoretically available but practically identity-locked: mining is generational family occupation and cultural identity.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, coal_mining_regions, payer,
    moderate, biographical, trapped, local).

% Financiers and operators of natural-gas pipeline networks, LNG terminals, and peaking plants. Portfolio-optimization reading treats gas infrastructure as transitional but does not mandate its elimination outright (gas can serve as balancing fuel during transition). However, the imperative to maximize low-carbon sources de-prioritizes gas in resource allocation and grid scheduling, reducing utilization and expected returns on invested capital. Exit is difficult: pipeline networks are sunk-cost infrastructure with 40-year useful life.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, natural_gas_infrastructure_investors, payer,
    powerful, biographical, constrained, national).

% Solar and wind project developers receive prioritized interconnection, transmission access, and capacity auction placement under low-carbon mandates. Portfolio-optimization reading classifies renewables as primary decarbonization source, though intermittency requires baseload stabilization—position that creates de facto demand for nuclear alongside renewable deployment. They compete for capital but are joint beneficiaries of the carbon-intensity mandate. Can relocate projects to higher-incentive jurisdictions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers, payer).

% Operate electricity transmission and distribution networks. Portfolio-optimization reading gives them an explicit mandate: balance supply reliability with carbon minimization by dispatching all available low-carbon sources (nuclear baseload, renewable variable, hydroelectric storage) in the optimal sequence. They enforce the constraint through dispatch algorithm, interconnection standards, and capacity market design. Have regulatory authority to modify dispatch rules but answer to government and public commissions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, agenda_setter,
    institutional, generational, analytical, national).

% Provide the empirical warrant for decarbonization urgency and assess technological portfolios' carbon intensity and deployment feasibility. Portfolio-optimization reading depends on their assessment that nuclear is a low-carbon source AND that grid-scale reliability requires baseload. They do not enforce the constraint but their empirical findings drive its legitimacy. Other readings dispute their empirical claims or their policy implications.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% Bear the cost of the transition through higher electricity rates (both nuclear capital costs and stranded-asset pass-throughs). Have no direct exit from grid connection (essential service). Coalition with labor unions and environmental justice groups is their primary mechanism to contest cost allocation, but institutional power is asymmetrically aligned with utilities and grid operators.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, low_income_ratepayers, payer,
    powerless, biographical, trapped, local).

% Advocate for decentralized, locally-controlled energy systems (microgrids, community solar, local storage) as the path to decarbonization. Portfolio-optimization reading excludes them by treating energy systems as inherently centralized (grid + baseload is the framing); their alternative framing—that centralized baseload perpetuates oligopoly power and prevents energy democracy—is structurally sidelined by the constraint's technology choices. They contest the reading through policy advocacy and alternative-scenario modeling but lack formal seats in capacity auctions or dispatch authority.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the energy decarbonization coordination problem: aligns investment incentives, technology selection, and dispatch sequencing under a unified low-carbon-source maximization imperative. Coordinates the physics constraint (carbon intensity) with the engineering constraint (baseload reliability) and the economic constraint (capital deployment efficiency) by declaring all three must be satisfied simultaneously.
% TRANSFER_FUNCTION: Transfers wealth from fossil-fuel asset holders and carbon-intensive infrastructure investors to nuclear operators and large utilities. Transfers compliance costs from energy companies to ratepayers (through higher rates). Transfers employment and tax base from coal-mining regions to construction and operations jobs in nuclear or renewable sectors (often geographically displaced). Transfers policy authority from markets and distributed decision-making to centralized grid operators and regulatory bodies.
% ABSENT_VOICES: Systems-transition advocates (decentralization/energy-democracy reading) are structurally excluded: the constraint's framing—that reliable decarbonization requires centralized baseload—predetermines that distributed systems are insufficient. Fossil-fuel workers and coal-dependent communities are nominally present (through labor representatives in some policy processes) but lack effective voice in technology selection decisions. Natural gas industry advocates participate in consultation but cannot contest the core assumption (low-carbon imperative) itself.
% DISAPPEARANCE_RATIONALE: If the portfolio-optimization reading and its enforcement disappeared, fossil-fuel investment would resume, nuclear plant retirements would accelerate (no policy support), renewable deployment would decelerate (no prioritization), and grid operators would dispatch based on marginal cost (coal would re-dominate baseload in most regions). Carbon emissions trajectory would steepen dramatically. Electricity price signals would shift entirely, and labor and capital currently flowing into low-carbon infrastructure would redirect to fossil fuel extraction and infrastructure.
% FOUNDING_PROBLEM: The constraint was built to solve two coupled problems: (1) unilateral physics constraint—decarbonization to 2050 requires eliminating fossil fuels from primary energy mix; (2) engineering constraint—electricity grids require reliable dispatchable baseload to manage demand variation and renewable intermittency. Portfolio-optimization reading holds that both constraints are binding and simultaneous, so all low-carbon sources must be deployed at maximum (nuclear + renewables + hydro + storage + all others) to satisfy both.
% FOUNDING_PROBLEM_CORROBORATION: Physics of carbon budget is widely corroborated: IPCC reports, peer-reviewed climate modeling, national emissions accounting. Reliability requirement for grids is corroborated by grid operators and transmission engineers. However, the claim that nuclear is NECESSARY (and not merely optional) is contested: opportunity-cost reading argues nuclear's capital intensity and deployment timeline make it net-harmful to decarbonization; systems-transition reading argues reliability can be achieved through distributed systems + storage without centralized baseload. Outside the nuclear-industry beneficiary set, major independent analyses (IRENA, IEA NEA reports) treat nuclear as one option among several, not a necessity. The 'necessary' framing is endorsed by grid operators (who benefit from centralized dispatch) and climate-policy institutions whose earlier models assumed nuclear; it is disputed by renewable-industry researchers and systems economists.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects asymmetric wealth transfer: utilities and nuclear operators capture rents through guaranteed dispatch and capacity payments; fossil producers and coal communities bear stranded-asset losses and employment collapse. The measured trajectory (0.48→0.68 over 25 years) shows extraction accumulating as coal plant retirements accelerate and nuclear rents solidify into regulatory certainty—early in the period, policy support is uncertain and fossil fuels retain market share; by year 25, coal fleet is substantially retired and nuclear dispatch priority is locked in. Suppression (0.71) reflects active enforcement: grid operators must enforce dispatch priorities, regulators must prevent fossil-fuel lobby from reversing capacity-payment rules, policymakers must maintain carbon accounting discipline. Theater (0.42) is moderate: the coordination function (baseload reliability + decarbonization) is real and material, but a significant fraction of enforcement activity (30-40% by the measurement) is devoted to defending nuclear's market position against rival cheap renewables and defending against alternative-system advocates. The time-series plateau (suppression and theater flatten after year 25) reflects maturation: early period shows rising suppression as fossil-fuel resistance intensifies and then subsides as stranded assets are written down and transition is normalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (fossil producers, coal regions) and the beneficiary seats (nuclear operators, utilities) should compute radically different constraint types from the same structural data: from a coal-mining region's seat, the constraint appears as an extractive snare enforced by distant regulators and market forces beyond local control (trapped exit, high suppression). From a nuclear operator's seat, the constraint appears as a rope—genuine coordination of physics and engineering constraints, with the operator capturing modest rents as compensation for managing baseload reliability. From grid operators' seat, it appears as rope (pure coordination problem being solved). The engine computes these per-seat divergences from power atom, exit options, and structural position. The authored claim (tangled-rope at the constraint level) represents the global structure: real coordination + asymmetric extraction. Divergence across seats is the diagnostic signal—a system whose beneficiaries see rope and whose victims see snare is exactly the structure this constraint exhibits.
 *
 * DIRECTIONALITY LOGIC:
 *   Three distinct directionality positions: (1) Nuclear operators and large utilities (d ≈ 0.1–0.2, near full beneficiary): collect rents, control dispatch authority, enjoy price certainty, can redirect capital if home-market support falters. (2) Fossil producers (d ≈ 0.9–0.95, near full target): reserves become unmarketable, infrastructure capacity is retired without compensation, exit is globally constrained by the carbon imperative itself. (3) Coal-mining regions (d ≈ 0.85–0.90, near full target): identity-locked (generational occupation, community culture fused with mining), exit to alternative employment is theoretically mobile but practically constrained by skill specificity and geographic dependency; the constraint extracts from them through employment collapse. (4) Grid operators (d ≈ 0.5, symmetric): they coordinate the constraint but do not directly collect or pay—regulatory authority is their exit, and they answer to government mandate rather than market incentives. (5) Systems-transition advocates are excluded (not a standard d calculation): the constraint's framing sidelines their alternative reading without permitting them formal voice in rebuttal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('decarbonize while maintaining grid reliability') is currently contested but not mandatrophic. The physics constraint (carbon budget) is live. The engineering constraint (reliability) is live—grids cannot yet run 100% renewable without storage at continental scales. The 'necessity' of nuclear (vs. storage + renewables at scale) is the contested element, not mandatrophic obsolescence. If storage technology matured to provide dispatchable baseload at cost parity with nuclear, the founding problem would remain live but the solution might shift: portfolio optimization would still hold (maximize all low-carbon sources) but nuclear might become optional rather than necessary. That shift would reclassify the constraint but would not make it mandatrophic. Mandatrophy would occur only if grids could be fully decarbonized reliably without any centralized baseload—a systems-transition outcome where the centralized-grid assumption itself becomes obsolete. That outcome is disputed; the policy establishment assumes centralized grids persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_necessity_vs_option,
    'Is nuclear operationally necessary for grid reliability under deep decarbonization, or is it one option among several that can be substituted by alternatives (storage, demand response, system redesign)?',
    'Natural experiments from grid operators deploying 80%+ renewable systems with storage; empirical studies of islanded and regional grids; long-term system modeling with alternative storage technologies maturing. Australia, Denmark, and some US regional grids are running real-time experiments.',
    'If nuclear is found to be substitutable (not necessary), the portfolio-optimization reading collapses into opportunity-cost reading—the constraint would then prioritize fastest-deployment capital sources (likely renewables + storage) and nuclear would shift from beneficiary to neutral/victim. If nuclear is found necessary (no viable substitute at scale), portfolio-optimization reading holds and nuclear remains beneficiary. This is the core epistemic disagreement between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_necessity_vs_option, empirical, 'Substitutability of nuclear for baseload in decarbonized grids.').

omega_variable(
    extraction_allocation_vs_coordination_cost,
    'Is the measured extraction (wealth transfer to utilities and nuclear operators) a necessary cost of the coordination function (getting all actors to deploy low-carbon simultaneously), or is it rent capture that could be constrained without impairing coordination?',
    'Regulatory experiments: some jurisdictions impose windfall-profit taxes on utilities capturing rents from carbon premium; others use auctions with price caps; others allow full cost-plus pass-through. Comparison of grid reliability and decarbonization speed across high-extraction and low-extraction jurisdictions. Economic analysis of what minimum compensation is required to sustain nuclear investment vs. what is currently being captured.',
    'If measured extraction exceeds coordination-necessary cost, the constraint is snare-contaminated (pure extraction riding genuine coordination). Policy response would be capping nuclear rents or shifting to renewable-dominant systems that capture fewer rents. If measured extraction tracks coordination cost, the constraint is cleanly tangled-rope. This determines whether the constraint is fixable (reduce extraction without losing coordination) or binary (extraction is inseparable from coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_allocation_vs_coordination_cost, empirical, 'Whether measured extraction exceeds coordination-necessary cost.').

omega_variable(
    identity_locked_transition_exit,
    'For coal-mining communities, is the measured suppression structural (economic collapse from market forces makes relocation/retraining infeasible) or internalized (workers and communities have fused identity with coal mining and reject alternative livelihoods even when offered)?',
    'Post-transition cohort analysis: track retraining take-up, migration patterns, and labor-market outcomes for workers in closed coal regions that offered comprehensive transition support vs. regions that did not. Qualitative research on identity-fusion and cultural continuity in transition outcomes.',
    'If suppression is mainly structural, policy can address it through adequate transition support, retraining, and local economic development. If suppression is mainly internalized, the transition carries long-term social costs even with generous support, and suppression scores should be understood as partially persistent post-exit. This affects judgment of whether the constraint''s human cost is addressable through compensation or is embedded in the community''s self-concept.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_transition_exit, empirical, 'Suppression mechanism in coal-community transition: structural vs. internalized.').

omega_variable(
    reading_foreclosure_boundary,
    'Does portfolio-optimization reading logically foreclose the systems-transition reading (decentralized systems), or do they represent genuinely coexisting positions where one party can adopt portfolio optimization while another adopts systems transition?',
    'Logical analysis: does claiming ''all low-carbon sources must be maximized'' entail ''centralized grids are necessary''? Or can decentralized systems also ''maximize all low-carbon sources'' by deploying local nuclear, solar, storage, and microgrids? The answer depends on whether the definition of ''low-carbon portfolio'' is technology-agnostic or implicitly assumes centralized dispatch. If technology-agnostic, readings coexist; if dispatch-dependent, portfolio-optimization forecloses systems-transition within a single framework.',
    'If readings coexist, both are live policy options and can be held by different parties without logical contradiction—the contest is empirical and political. If portfolio-optimization forecloses systems-transition, the latter reading is not coherent within portfolio-optimization''s framework and the contest is foundational (about what ''maximization'' and ''reliability'' mean). This determines whether policy compromise is possible (yes, if coexisting; no, if foreclosing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical foreclosure relationship between portfolio-optimization and systems-transition readings.').

omega_variable(
    carbon_intensity_measurement_ambiguity,
    'Is ''low-carbon'' defined by direct operational emissions, or lifecycle emissions including construction, decommissioning, and supply-chain embodied carbon? Does the definition change the ranking of nuclear vs. alternatives?',
    'Lifecycle assessment studies comparing nuclear (high construction carbon, zero operational) vs. renewables (lower construction carbon, zero operational) vs. gas (operational-dominant carbon). If lifecycle definitions shift nuclear above or below alternatives in the ranking, the beneficiary/victim structure changes.',
    'If lifecycle emissions show nuclear as higher than renewables per unit energy, the portfolio-optimization reading''s beneficiary structure inverts: renewables become primary beneficiary and nuclear becomes payer (competing for limited capital). This would reclassify the constraint and shift political dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_intensity_measurement_ambiguity, empirical, 'Definition of ''low-carbon'' and its effect on technology ranking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(clim_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(clim_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_stranded_asset_regime).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, coal_community_economic_collapse).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, utility_rate_design_cost_recovery).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'climate_mitigation_imperative'. Sibling readings are authored as separate constraint stories: 'climate_mitigation_imperative__opportunity_cost_reading' (nuclear as capital-inefficient option, renewables + storage as primary beneficiary) and 'climate_mitigation_imperative__systems_transition_reading' (decentralized systems as alternative framing that forecloses centralized nuclear necessity claim). All three readings share the referent (decarbonization of energy systems) but differ in what ε values are extracted from that referent, what beneficiary/victim sets exist, and what classification results. The three stories form a constraint family linked by kernel identity and reading relations. Portfolio-optimization reading represents the current regulatory consensus in most OECD countries; opportunity-cost reading is ascendant among climate economists and renewable-focused analysts; systems-transition reading is held by energy-democracy and environmental-justice coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerless, 0.88).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
