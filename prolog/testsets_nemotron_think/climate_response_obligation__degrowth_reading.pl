% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Climate Response: Material Throughput Reduction Within Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of the climate response obligation asserts that
 *   staying within planetary boundaries requires absolute reduction of
 *   material-energy throughput in the Global North, not merely
 *   decarbonization of a growing economy. It frames the constraint as a
 *   coordination mechanism for biophysical survival and global justice: the
 *   North contracts so the South can develop, and both converge at a
 *   sufficient but not excessive level. Capital accumulation is identified as
 *   the structural driver of throughput expansion — the constraint targets
 *   the growth imperative itself, not just its carbon intensity. This reading
 *   has moved from academic ecology (1972 Limits to Growth) through marginal
 *   policy discourse to increasing visibility in IPCC demand-side chapters
 *   and European policy debates, but remains excluded from mainstream
 *   governance.
 *
 * KEY AGENTS:
 *   - planetary_systems: Primary beneficiary (powerless/trapped/universal) — receives reduced extraction pressure
 *   - future_generations: Primary beneficiary (powerless/trapped/universal) — inherits habitability
 *   - global_north_consumers: Primary target (organized/constrained/global) — bears lifestyle reduction
 *   - capital_owners: Primary target (institutional/arbitrage/global) — bears accumulation constraint
 *   - high_carbon_industries: Primary target (powerful/constrained/global) — bears asset stranding
 *   - growth_dependent_labor: Secondary target (moderate/constrained/global) — bears transition cost
 *   - global_south_development_space: Conditional beneficiary (moderate/constrained/global) — gains only if North reduces first
 *   - degrowth_policymakers: Agenda setter (institutional/mobile/national) — administers the constraint
 *   - climate_justice_movements: Beneficiary/observer (organized/mobile/global) — monitors justice provisions
 *   - mainstream_economists: Excluded (institutional/analytical/global) — paradigmatically incommensurate
 *   - ipcc_unfccc_apparatus: Observer (institutional/analytical/global) — legitimates competing reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.75).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.78).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Climate Response: Material Throughput Reduction Within Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'b9d74498-e2ca-47fb-8563-fdb75c5a43e7').
narrative_ontology:cs_kernel_codification('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', distributed).
narrative_ontology:cs_authority_grounding('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', distributed).
narrative_ontology:cs_reading_relation('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_axiom('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', foundational, sufficiency_over_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', sufficiency_over_efficiency, deontological).
narrative_ontology:cs_axiom('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', foundational, material_throughput_must_decline_absolutely).
narrative_ontology:cs_axiom_status(material_throughput_must_decline_absolutely, holdable).
narrative_ontology:cs_axiom_grounding('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', material_throughput_must_decline_absolutely, empirically_contingent).
narrative_ontology:cs_axiom('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', secondary, global_north_contracts_first_for_global_south_space).
narrative_ontology:cs_axiom_status(global_north_contracts_first_for_global_south_space, holdable).
narrative_ontology:cs_axiom_grounding('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', global_north_contracts_first_for_global_south_space, deontological).
narrative_ontology:cs_reference_frame('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', biophysical_planetary_boundaries).
narrative_ontology:cs_drift_state('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', post_paris_agreement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b9d74498-e2ca-47fb-8563-fdb75c5a43e7', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_development_space).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, capital_owners).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, high_carbon_industries).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_dependent_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, climate_justice_movements).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_over_efficiency_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, intergenerational_justice_requires_throughput_reduction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The biosphere and earth system processes that receive reduced extraction pressure when material throughput declines. They have no voice, no exit, and no agency — they simply register the physical consequences of the constraint's operation or absence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% All people not yet born who inherit a habitable planet if throughput reduction succeeds. They cannot participate in today's decisions, cannot exit the consequences, and bear the full cost of constraint failure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_generations).

% Countries and populations in the Global South whose development space opens only if the Global North reduces throughput first. They are conditional beneficiaries — their benefit depends on a prior extraction from Northern consumers and capital. Their exit is constrained by global economic structure and historical emissions inequality.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_space, beneficiary,
    moderate, generational, constrained, global).

% Populations in wealthy nations whose material lifestyles (high energy use, meat consumption, frequent flying, large dwellings, disposable goods) are the primary target of throughput reduction. They have political voice and market power but face identity-locked consumption patterns — lifestyle is fused with self-concept, social status, and cultural scripts. Exit means downward mobility in a status hierarchy they cannot individually opt out of.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    organized, biographical, constrained, global).

% Owners of capital whose returns depend on continuous expansion of commodity production and energy throughput. They can move capital across borders and sectors (arbitrage-grade exit) but the constraint targets the accumulation logic itself — no sector fully escapes if growth is capped. They deploy political influence to shape policy, fund think tanks, and capture regulatory processes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, capital_owners, payer,
    institutional, biographical, arbitrage, global).

% Fossil fuel extraction, heavy industry, industrial agriculture, aviation, shipping — sectors whose business models require high material-energy throughput. They have concentrated political power, infrastructure lock-in, and workforce dependencies. Exit means stranded assets and community collapse; they fight constraint enforcement through lobbying, disinformation, and regulatory capture.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, high_carbon_industries, payer,
    powerful, biographical, constrained, global).

% Workers in high-throughput sectors and their communities whose wages, pensions, and public services depend on growth-linked tax bases. They did not choose the growth dependency but bear its collapse. Exit is constrained by skills specificity, geographic immobility, and the absence of just transition infrastructure. They are often pitted against climate policy by capital owners who control the narrative.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_dependent_labor, payer,
    moderate, biographical, constrained, global).

% Political actors, ministers, and legislative bodies attempting to implement throughput caps, resource rationing, work-time reduction, and decommodification. They hold formal authority but depend on unstable coalitions. Their exit is mobile — they can leave office — but the constraint persists only if institutionalized beyond electoral cycles.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% Transnational movements demanding equity in the transition: contraction and convergence, climate reparations, Global South policy space. They benefit from the constraint's justice provisions but also monitor its enforcement. They have organizational capacity and can shift tactics (mobile exit) but are excluded from formal decision-making.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, climate_justice_movements, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, climate_justice_movements, observer).

% The dominant economic profession whose models treat growth as endogenous and non-negotiable, who advise finance ministries and central banks. They would object to throughput reduction as unnecessary (decoupling) or catastrophic (welfare loss). They are excluded from the degrowth framing not by force but by paradigmatic incommensurability — their analytical tools cannot represent the constraint's logic.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mainstream_economists, excluded,
    institutional, biographical, analytical, global).

% The formal international climate governance structure that produces mitigation pathways, carbon budgets, and policy guidance. It treats degrowth as a 'demand-side measure' among others, not as the central logic. It observes the constraint from within a growth-compatible framework and legitimates the mitigation_priority reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, ipcc_unfccc_apparatus, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human material throughput within the safe operating space of planetary boundaries (climate, biosphere integrity, biogeochemical flows, land-system change, etc.) so that the earth system remains in a Holocene-like state capable of supporting civilization.
% TRANSFER_FUNCTION: Moves material throughput entitlement from Global North high-consumption populations and capital accumulation processes to: (a) planetary regeneration capacity, (b) Global South development space, (c) future generations' habitability. The transfer is enforced through caps, rations, bans, and decommodification — not markets.
% ABSENT_VOICES: The global poor who aspire to North-like consumption but are told the budget is spent; indigenous peoples whose land-based low-throughput lifeways are disrupted by both extraction AND green transition mining; non-human species who have no representation in any human governance; the unemployed and precarious who are offered 'green jobs' that may not materialize in a contracting economy.
% DISAPPEARANCE_RATIONALE: If the throughput reduction constraint vanished overnight, Global North consumption would continue expanding until biophysical limits forced chaotic collapse; Global South development would remain blocked by Northern overconsumption; capital accumulation would accelerate ecological overshoot. The world would rearrange through crisis rather than design — the constraint is what prevents the rearrangement from being catastrophic.
% FOUNDING_PROBLEM: The biophysical reality that infinite material growth on a finite planet is impossible, and that the Global North has already exceeded its fair share of the global carbon budget and material footprint. The arrangement was built (conceptually, by ecological economists and movements) to solve the dual crisis of ecological overshoot and global inequality by recognizing that Northern overconsumption is the driver of both.
% FOUNDING_PROBLEM_CORROBORATION: Ecological footprint data (Global Footprint Network), planetary boundaries science (Rockström et al., updated 2023), IPCC AR6 WGIII Chapter 2 on demand-side mitigation, UNEP Emissions Gap Reports showing mitigation shortfall, and Global South negotiating positions (G77, LDCs, AOSIS) at UNFCCC all corroborate that the problem — overshoot plus inequality — is live and worsening. No credible source outside the degrowth-benefiting coalition disputes the biophysical premise; the dispute is over the political feasibility and justice of the response.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint demands absolute throughput reduction from the world's most powerful consumers and capital — it extracts the material basis of their current welfare and profit. Suppression (0.78) is high because enforcement requires rationing, bans, work-time limits, and decommodification against fierce resistance from capital, consumers, and growth-dependent states. Theater ratio (0.25) is low because degrowth advocates genuinely seek biophysical compliance, not performative gestures — but rises slightly over time as 'green growth' rhetoric absorbs degrowth language without its substance. Accessibility collapse (0.82) is high because once planetary boundaries are accepted as hard, the alternative (continued growth) becomes physically impossible, not just politically difficult. Resistance (0.85) is very high because the constraint threatens the core logic of capitalism and the lifestyle identity of the global professional-managerial class. All metrics tracked on a shared 1972-2050 grid from Limits to Growth through mid-century.
 *
 * PERSPECTIVAL GAP:
 *   From the planetary systems / future generations seat (powerless, trapped), the constraint is a mountain — biophysical law that admits no negotiation. From the Global South development seat (moderate, constrained), it is a scaffold — temporary Northern contraction enabling Southern development, with a sunset when convergence is reached. From the Global North consumer seat (organized, identity-locked), it is a snare — coercive lifestyle reduction enforced by elites who exempt themselves. From the capital owner seat (institutional, arbitrage), it is a tangled rope — they accept carbon pricing (coordination) but fight throughput caps (extraction). From the degrowth policymaker seat (institutional, mobile), it is a rope — genuine coordination for survival. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the reading's own view that it is both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are structural beneficiaries with d near 0 (full beneficiary) — they receive reduced extraction pressure but have zero power or exit. Global South development space is a conditional beneficiary (d ~0.2-0.3) — benefit only materializes if North reduces first; exit is constrained by global inequality. Global North consumers are targets with d ~0.8-0.9 — identity-locked consumption patterns, constrained exit, organized power but used against the constraint. Capital owners are targets with d ~0.7 — high power and arbitrage exit, but the constraint targets their accumulation logic directly. High-carbon industries are targets with d ~0.85 — powerful but exit-constrained by asset specificity. Growth-dependent labor are targets with d ~0.75 — moderate power, constrained exit, bearing transition costs they did not choose. Degrowth policymakers are agenda setters with d ~0.1 — they administer and benefit institutionally from the constraint's existence. Climate justice movements are beneficiaries/observers with d ~0.2. Mainstream economists and IPCC apparatus are observers with d=0.5 (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading prevents mislabeling in two directions: (1) It resists calling the constraint pure extraction (snare) by naming the genuine coordination function — planetary survival and Global South justice are real, not cover stories. The victims are not arbitrary; they are the agents whose throughput exceeds the safe budget. (2) It resists calling it pure coordination (rope) by naming the asymmetric extraction — the North must contract, the South's development is conditional, labor bears transition costs. The mandate (planetary survival) has not atrophied; it has intensified. The constraint is not a piton — theater is low and rising only because 'degrowth' language is being co-opted by green growth advocates. Mandatrophy is not resolved; the founding problem is live and worsening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the climate_response_obligation kernel a single commitment with multiple readings, or are these three distinct obligations that only share a label?',
    'Trace the textual and institutional genealogy: does UNFCCC Article 2 (''stabilization of greenhouse gas concentrations'') structurally entail throughput reduction, or only net-zero emissions? If the latter, the kernel is ambiguous (distributed codification) and the readings are genuinely divergent, not competing interpretations of a fixed text.',
    'If the kernel is distributed/ambiguous, the three readings are not foreclosing each other — they are distinct constraints linked only by a shared label. The engine should treat them as a constraint family with weak coupling. If the kernel is formalized (Paris temperature goal), then degrowth and mitigation_priority may foreclose within a single carbon budget framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings share a single kernel or merely a label.').

omega_variable(
    throughput_vs_emissions_boundary,
    'Is material throughput reduction structurally necessary for staying within planetary boundaries, or is rapid decarbonization of energy sufficient (allowing continued material growth)?',
    'Empirical test: can global material footprint decline while GDP grows, at the rate and scale required by the carbon budget? Historical data (1990-2020) shows no absolute decoupling at global level; only relative decoupling. Future test: monitor EU/US material footprint trajectories under Green Deal / IRA policies.',
    'If absolute decoupling is empirically impossible at required speed/scale, the mitigation_priority reading''s coordination function fails — its ε rises, it becomes a snare (false coordination). The degrowth reading''s ε is validated. If decoupling succeeds, degrowth''s extraction is unnecessary — its ε falls, it becomes a snare (unjustified extraction). This is the core empirical omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(throughput_vs_emissions_boundary, empirical, 'Whether the coordination function (planetary survival) requires throughput reduction or only emissions reduction.').

omega_variable(
    north_first_conditional_justice,
    'Does the Global South development space benefit actually materialize if the North reduces throughput, or is it a theoretical promise that evaporates under geopolitical reality?',
    'Track climate finance flows, technology transfer, and policy space concessions in UNFCCC negotiations conditional on Northern mitigation ambition. Historical test: did Kyoto/Paris Northern commitments unlock Southern development? Current test: Loss and Damage fund operationalization, Article 6.4 mechanisms, BRICS development trajectories.',
    'If the conditional benefit is illusory, the global_south_development_space beneficiary is a vindicated_proposition, not a real actor — the constraint extracts from North consumers without delivering Southern justice. The reading becomes a snare for Northern consumers with no coordination payoff. If the benefit materializes, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_first_conditional_justice, empirical, 'Whether the justice coordination function (North contracts → South develops) is empirically realizable.').

omega_variable(
    suppression_mechanism_identity_lock,
    'Is the high suppression measured for global_north_consumers structural (policy enforcement, rationing) or internalized (identity-fused consumption patterns that resist change even without policy)?',
    'Compare suppression in jurisdictions with vs. without degrowth policies (e.g., work-time reduction trials, carbon rationing proposals, advertising bans). If resistance persists identically without enforcement, the suppression is internalized. Post-policy surveys on ''willingness to reduce'' vs. actual behavior.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than structural enforcement alone — the target carries the constraint internally. This strengthens the snare reading from the consumer seat. If suppression is primarily structural, enforcement design determines the constraint''s viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock, empirical, 'Structural vs. internalized suppression mechanism for identity-locked Northern consumers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1972, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1972, climate_response_obligation__degrowth_reading, theater_ratio, 1972, 0.05).
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__degrowth_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__degrowth_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__degrowth_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__degrowth_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__degrowth_reading, theater_ratio, 2030, 0.25).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__degrowth_reading, theater_ratio, 2040, 0.28).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t1972, climate_response_obligation__degrowth_reading, base_extractiveness, 1972, 0.15).
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__degrowth_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__degrowth_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__degrowth_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__degrowth_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__degrowth_reading, base_extractiveness, 2030, 0.72).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__degrowth_reading, base_extractiveness, 2040, 0.78).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1972, climate_response_obligation__degrowth_reading, suppression_requirement, 1972, 0.1).
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__degrowth_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__degrowth_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__degrowth_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__degrowth_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__degrowth_reading, suppression_requirement, 2030, 0.78).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__degrowth_reading, suppression_requirement, 2040, 0.82).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_obligation kernel. The mitigation_priority reading (rapid decarbonization with growth) and adaptation_priority reading (resilience over prevention) are sibling constraints. All three share the kernel's legitimating authority (UNFCCC, Paris Agreement, IPCC) but instantiate different ε values, different beneficiary/victim structures, and different enforcement logics. The degrowth reading has the highest ε (0.75) because it targets throughput itself; mitigation_priority has lower ε (~0.4-0.5) because it targets carbon intensity; adaptation_priority has variable ε depending on who bears adaptation costs. They form a constraint family linked by shared kernel authority and competing for the same policy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, organized, 0.85).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, institutional, 0.7).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, powerful, 0.85).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
