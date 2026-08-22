% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response: Emissions Reduction via Technological Transition
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation-priority reading of the
 *   contested climate harm prevention kernel. The reading asserts that
 *   legitimate climate response must prioritize greenhouse gas emissions
 *   reduction via technological transition (renewable energy,
 *   electrification, efficiency) operating within the growth
 *   framework—assuming that economic growth and decarbonization are
 *   compatible. Future generations and climate-vulnerable nations are the
 *   primary beneficiaries of avoided warming; present fossil-fuel-dependent
 *   industries and workers bear concentrated transition costs. The constraint
 *   is CLAIMED as tangled_rope (genuine coordination function on emissions
 *   reduction + asymmetric extraction of costs) while the authored metrics
 *   describe a moderately extractive, actively enforced arrangement. The
 *   engine will compute whether per-seat classifications diverge from this
 *   claim; divergence is the measurement we seek. This reading coexists with
 *   adaptation-priority and degrowth readings, which contest the same
 *   founding problem but disagree on mitigation feasibility and the growth
 *   assumption.
 *
 * KEY AGENTS:
 *   - Future generations: Powerless beneficiaries; inherit the climate state but have zero voice in present policy.
 *   - Low-lying nations (Marshall Islands, Bangladesh, small island states): Organized beneficiaries facing existential territorial risk; trapped exit; high political visibility but limited enforcement power.
 *   - Renewable energy sector: Institutional beneficiary; economically aligned with the constraint; grows as transition investment flows.
 *   - Carbon-intensive industries (coal, oil, gas, cement, steel): Institutional payers; face stranded assets, compliance costs, forced retooling; constrained exit via regulatory arbitrage.
 *   - Fossil fuel workers (miners, oil rig workers, power plant operators): Moderate-power payers with identity-locked exit; experience concentrated immediate costs while climate benefits are diffuse.
 *   - High-consumption present populations (Global North): Organized payers; face rising energy costs, carbon taxation, consumption restrictions; constrained but not trapped exit.
 *   - Global North governments and multilateral institutions: Agenda-setters; design emissions targets, allocate investment, enforce via regulation and carbon pricing.
 *   - Climate scientists: Observers; provide empirical grounding for the reading and its founding problem.
 *   - Adaptation-priority advocates: Excluded; their core claim (mitigation infeasible) is treated as empirically refuted.
 *   - Degrowth advocates: Excluded; their core claim (growth-compatible decarbonization impossible) directly contests this reading's foundational axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Climate Response: Emissions Reduction via Technological Transition").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '46f96560-325c-4be3-b7bc-4b93c2fb8d3c').
narrative_ontology:cs_kernel_codification('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', distributed).
narrative_ontology:cs_authority_grounding('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', distributed).
narrative_ontology:cs_reading_relation('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', foundational, growth_compatible_decarbonization).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', growth_compatible_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', secondary, mitigation_technologically_feasible).
narrative_ontology:cs_axiom_status(mitigation_technologically_feasible, holdable).
narrative_ontology:cs_axiom_grounding('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', mitigation_technologically_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', pre_industrial_climate_state).
narrative_ontology:cs_drift_state('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', contemporary_anthropocene, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('46f96560-325c-4be3-b7bc-4b93c2fb8d3c', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, low_lying_nations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, high_consumption_present_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, fossil_fuel_workers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, decarbonization_technologically_feasible).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, growth_compatible_decarbonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the primary benefit of emissions reduction: a climate system that has not crossed irreversible tipping points, lower warming trajectories, and intact agricultural and coastal systems. Their material interests are entirely dependent on present-generation emission choices; they have no voice in the policy formation and no exit from the inherited climate state.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face existential risk from sea-level rise and increased storm intensity under high-warming scenarios; mitigation offers preservation of territorial sovereignty and economic viability. Their exit options are severely constrained—physical relocation is forced migration, not choice. They lobby for aggressive emissions targets but lack enforcement mechanisms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, low_lying_nations, beneficiary,
    organized, generational, trapped, global).

% Grows economically and politically as decarbonization targets drive investment, subsidies, and regulatory preference. Their business model is aligned with the constraint's operation. They fund research, lobby for stronger targets, and benefit from technological transition rents.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_sector, beneficiary,
    institutional, biographical, mobile, global).

% Face mandated emissions reductions, stranded assets, regulatory compliance costs, and forced technology transition. Coal, oil, and high-emission manufacturing face value destruction on balance sheets and must retool production processes or exit markets. Their alternatives are absorbing the transition cost, relocating to looser jurisdictions (regulatory arbitrage), or litigation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    institutional, biographical, constrained, global).

% Experience the most concentrated transition costs: job losses in coal mining, oil refining, gas extraction. Their professional identity, community structure, pension systems, and local economies are built on fossil fuel infrastructure. Retraining programs and transition support exist but are often inadequate relative to wage losses and place-attachment. They also inherit climate benefits but experience them as diffuse while costs are immediate and acute.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_workers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, fossil_fuel_workers, beneficiary).

% In high-income nations, face rising energy prices, carbon taxation, consumption restrictions, and behavioral demands (diet change, transport electrification, building retrofits). The constraint's enforcement manifests as higher costs on carbon-intensive goods and services. Their exit is partial: they can migrate to lower-regulation jurisdictions, reduce consumption, or lobby against the constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, high_consumption_present_populations, payer,
    organized, biographical, constrained, national).

% National governments, multilateral institutions, and regulatory bodies set and enforce emissions targets, allocate transition investment, design carbon pricing, and oversee the technological transition. They adjudicate the constraint's scope, pace, and distribution of costs. They justify it as protecting future interests while claiming the transition is growth-compatible.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_north_governments, agenda_setter,
    institutional, generational, mobile, national).

% Provide the empirical grounding for the mitigation-priority reading: evidence of warming trajectories, tipping-point thresholds, technological feasibility of decarbonization, and cost-of-inaction calculations. Their work is repeatedly cited to justify the constraint and to defend it against both adaptation-priority and degrowth alternatives.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    institutional, generational, analytical, global).

% Argue that mitigation targets are politically unachievable and economically irrational, and that resources should be redirected to climate resilience and managed adaptation. They are excluded from the consensus justifying the mitigation-priority reading because their core claim (mitigation infeasibility) is treated as empirically refuted by the reading. Their voice enters only as an alternative reading, not as a party to be coordinated.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_priority_advocates, excluded,
    organized, biographical, constrained, global).

% Argue that decarbonization within the growth framework is physically impossible and that the mitigation-priority reading's core axiom (growth-compatible decarbonization) is false. They claim the constraint is extractive cover for continued consumption by the Global North while deferring collapse. They are excluded from the consensus because their core claim directly contests the reading's foundational axiom.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared global commitment to emissions reduction targets, allocates transition investment across sectors and regions, coordinates technological development and knowledge-sharing for renewable infrastructure, and synchronizes national climate policies to prevent competitive regulatory arbitrage (where carbon-intensive industries migrate to low-regulation jurisdictions).
% TRANSFER_FUNCTION: Transfers emissions reduction costs from present high-consumption populations and carbon-intensive industries to future generations (as a reduction in their inherited climate liability) and from Global North fossil fuel workers and regions dependent on carbon industries to renewable energy sector workers and regions building clean infrastructure. It also transfers rents: transition investment flows to renewable energy companies, EV manufacturers, and grid modernization contractors.
% ABSENT_VOICES: Adaptation-priority advocates (who would argue mitigation targets are unachievable and resources should redirect to resilience) and degrowth advocates (who would argue growth-compatible decarbonization is physically impossible) are excluded from consensus by the reading's foundational axioms, which treat their core claims as empirically refuted or normatively illegitimate. Workers in fossil fuel extraction and high-emission manufacturing have limited voice in policy design despite bearing concentrated costs.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority constraint vanished overnight, emissions reduction targets would collapse, investment in renewable infrastructure would halt or reverse, carbon prices would disappear, and fossil fuel industries would experience a massive asset revaluation and return to previous extraction rates. The global economy would immediately revert to higher-emissions trajectories. Future climate outcomes would be substantially worse; the winners (fossil fuel sectors, high-consumption present populations) would reorganize around renewed extraction; the losers (future generations, climate-vulnerable nations) would face locked-in warming.
% FOUNDING_PROBLEM: The climate system exhibits tipping-point dynamics and accumulating radiative forcing from anthropogenic greenhouse gas emissions. Without deliberate emissions reduction, warming will exceed adaptation capacity in many regions, causing ecosystem collapse, food system instability, mass migration, and civilizational disruption. The founding problem is: how to prevent crossing irreversible thresholds while maintaining economic growth and development.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and the IPCC attest the founding problem remains live: warming is accelerating, tipping points are approaching, and technological feasibility of decarbonization is established but political/economic will is insufficient. Adaptation advocates contest that mitigation at the scale required is politically and economically achievable; degrowth advocates contest that it is physically achievable within the growth framework. Independent economic modeling from outside the fossil fuel and renewable energy sectors supports that 1.5–2°C targets require rapid decarbonization, but dispute whether this is compatible with growth.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68 by interval end) because the constraint transfers substantial costs to present carbon-intensive sectors and workers while dispersing benefits across future generations and a global beneficiary set. The founding problem (climate tipping points) is real, but the reading's proposed solution assumes decarbonization within growth—a factual claim that degrowth critics dispute and that adaptation advocates treat as politically infeasible. Suppression is moderate (0.62) because the constraint requires active enforcement: carbon pricing to change incentives, regulation to exclude high-emission alternatives, and continuous political defense against adaptation-priority and degrowth alternatives that would collapse or redirect the constraint. Theater is moderate (0.41): the coordination function (emissions reduction targets, technology development, investment allocation) is real and functionally necessary; the theater component reflects greenwashing (where compliance is performative rather than functional), delayed implementation timelines, and the gap between stated targets and achieved emissions reductions. The measurement series show extractiveness and suppression increasing from t0 to t15 (as the transition accelerates and enforcement machinery hardens), then plateauing—modeling a transition that becomes entrenched and resistant to reversal. Theater-ratio rises with suppression, as the gap between stated targets and actual implementation becomes more visible and must be rhetorically defended. All three metrics share the single authorized time grid (t ∈ {0,5,10,15,20,25,30}) so that temporal analysis has coherent data.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should compute very differently from the seats of fossil-fuel-dependent industries, workers, and adaptation advocates vs. the seats of renewable energy beneficiaries, climate scientists, and future-generation advocates. From the industry/worker/adaptation seats, the arrangement looks like coercive cost-transfer justified by a disputed factual claim (that growth-compatible decarbonization is feasible). From the beneficiary/scientist seats, it looks like genuine coordination solving a real collective problem. The engine computes this divergence from power levels, exit options, and beneficiary/victim declarations: carbon-intensive industries have institutional power but constrained exit; future generations have powerless status but maximally trapped exit; fossil-fuel workers have moderate power but identity-locked exit, which should produce different effective directionality than pure powerlessness. Agenda-setters (Global North governments) should compute differently still—they hold the enforcement machinery and the agenda, making their directionality asymmetric relative to payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: d near 1.0 (full target of extraction—they bear the climate liability if mitigation fails, but receive the primary benefit if it succeeds; however, they have zero voice and zero exit, so structurally they are maximal targets despite being intended beneficiaries). Low-lying nations: d around 0.7-0.8 (trapped beneficiaries; they benefit from mitigation but bear costs of inaction, and their exit is physical migration—not a choice). Renewable energy sector: d near 0.0-0.1 (full beneficiary; grows economically with the constraint; mobile exit). Carbon-intensive industries: d near 1.0 (full targets; costs imposed without consent; constrained exit via regulatory arbitrage). Fossil-fuel workers: d around 0.85-0.95 (targets, but with partial identity-lock that creates higher effective extraction than pure economic analysis suggests). High-consumption populations: d around 0.6-0.7 (mixed; they bear visible costs but also inherit climate benefits; constrained but not trapped exit). Global North governments: d around 0.4-0.5 (symmetric; they collect political legitimacy and investment opportunity but also bear enforcement costs and political blow-back). Adaptation and degrowth advocates: excluded, so d not computed; they would occupy different constraint spaces if their readings were instantiated separately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate tipping points) remains live and contested—the mitigation-priority reading ASSERTS that the problem is solvable via technological transition within growth, while adaptation and degrowth readings DISPUTE this claim. Mandatrophy (where the founding problem dies but the constraint persists) is NOT yet resolved because the dispute is open and the founding problem's status is genuinely contested. However, if technological decarbonization reaches maturity (t~30+) and deployment slows despite cost improvements, or if warming crosses stated tipping points despite mitigation efforts, the founding problem's status would shift from 'live' to 'dead' while the constraint persists as a zombie—that would resolve mandatrophy toward entrenched institutional inertia. The current measurement trajectory (extractiveness and suppression plateauing post-t15) is consistent with either successful stabilization or premature institutional lock-in; measurement at t30+ would disambiguate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_boundary,
    'Is greenhouse-gas emissions reduction compatible with continued economic growth (measured as GDP expansion), or does the physical constraint on carbon intensity require planned contraction?',
    'Decoupling hypothesis test: track total GDP growth vs. total emissions reduction over a 10–15 year interval; if emissions fall while GDP grows, the hypothesis is supported; if emissions fall only when GDP contracts, the reading''s core axiom fails. Independent carbon accounting to prevent carbon accounting method variation from confounding the test.',
    'If incompatible, the reading collapses into the degrowth reading, and the constraint''s legitimacy shifts from coordination-with-asymmetric-costs to coercive-contraction-dressed-as-transition. If compatible, the reading''s foundational axiom holds and the constraint''s classification as tangled_rope (real coordination + asymmetric extraction) is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_boundary, empirical, 'Whether decarbonization can occur within growth framework').

omega_variable(
    mitigation_feasibility_vs_political_will,
    'Is the constraint''s slow implementation (theater_ratio rising, target timelines repeatedly delayed) due to technological barriers, political economy barriers, or the reading''s claim being empirically false?',
    'Counterfactual analysis: survey what would change if political will were maximal (no industry lobbying, no regulatory capture, no election cycles). If barriers remain high, technology is the constraint; if barriers mostly evaporate, politics is the real constraint and adaptation-priority advocates'' claim (mitigation infeasible) gains credibility.',
    'If political barriers are dominant, the reading remains live but suppression and theater increase. If technological barriers dominate, the reading''s factual claim (feasibility) fails and classification shifts toward snare (pure extraction with unachievable coordination framing). If both are substantial, the constraint is hybrid extraction under a contested feasibility claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_feasibility_vs_political_will, empirical, 'Whether implementation delays are due to technical or political constraints').

omega_variable(
    intergenerational_power_asymmetry,
    'How much of the constraint''s extractiveness derives from the fact that future generations have zero political power and no exit, making them maximally capturable for cost-transfer regardless of whether the coordination function is real?',
    'Structural analysis: decompose extractiveness into (a) coordination overhead (genuine cost of decarbonization), (b) rent capture (renewable energy windfall profits, transition winner capture), and (c) intergenerational power arbitrage (using future generations'' powerlessness to justify present sacrifice). If (c) is substantial, the constraint is more extractive than the coordination function alone justifies.',
    'If (c) is substantial, the reading becomes a cover for intergenerational rent transfer, and the classification shifts from tangled_rope toward snare. If (c) is minor, the reading''s asymmetry is justifiable as the price of decarbonization necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_power_asymmetry, conceptual, 'Whether future-generation powerlessness is structurally exploited').

omega_variable(
    fossil_fuel_worker_exit_structure,
    'Is the measured identity-locked exit status for fossil-fuel workers structural (their skills and community have no alternative use) or performative (skills are transferable but identity investment makes exit psychologically costly)?',
    'Post-exit trajectory study: track workers who exit coal/oil industries and measure whether suppression persists (internalized) or dissolves (structural). If persists, identity-lock is real and substantive; if dissolves, lock is performative and could be broken by strong enough policy support.',
    'If structural, fossil-fuel workers are genuinely trapped and the constraint''s extraction from them is maximal. If performative, suppression is internalized but could be reversed, and the extraction is conditional on ideology rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_fuel_worker_exit_structure, empirical, 'Whether fossil-fuel worker identity-lock is structural or performative').

omega_variable(
    reading_contest_resolution,
    'Which reading (mitigation_priority, adaptation_priority, or degrowth) will be endorsed by empirical evidence and political institutions over the next 15 years? Will one reading foreclose the others, or will they continue coexisting?',
    'Observational time-series: track deployed mitigation capacity, emissions trajectories, warming paths, adaptation investment, and policy emphasis shifts. If emissions reduction tracks to 1.5–2°C target timelines, mitigation_priority is vindicated. If mitigation stalls and adaptation spending rises sharply, adaptation_priority gains institutional weight. If GDP growth decouples sharply from any reasonable carbon budget, degrowth advocates'' claim is vindicated.',
    'This is the ultimate falsifiability test for the reading. If the reading''s empirical predictions fail (warming continues, emissions don''t decouple, growth doesn''t accommodate decarbonization), the entire justification structure for the constraint collapses and the classification flips from coordination to pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_resolution, empirical, 'Which kernel reading is empirically supported over 15-year time horizon').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__mitigation_priority, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__mitigation_priority, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__mitigation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__mitigation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__mitigation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__mitigation_priority, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__mitigation_priority, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__mitigation_priority, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__mitigation_priority, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__mitigation_priority, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__mitigation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__mitigation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__mitigation_priority, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__mitigation_priority, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__mitigation_priority, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__mitigation_priority, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__mitigation_priority, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__mitigation_priority, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(clim_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.22).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, renewable_energy_subsidy_architecture).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, fossil_fuel_phase_out_mandate).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested climate_harm_prevention kernel. The mitigation_priority reading instantiates the normative claim that emissions reduction via technological transition within growth is both feasible and legitimate. Two sibling readings decompose the same founding problem (climate tipping points) but propose different solutions: adaptation_priority assumes mitigation is politically infeasible and redirects to resilience; degrowth_reading contests the growth assumption itself. These readings are NOT the same constraint viewed from different angles—they have different ε values, different beneficiary/victim structures, and different empirical predictions. Each should be authored separately. This reading influences the others by controlling investment flows and institutional endorsement: if mitigation-priority constraints are successfully enforced, capital and political will are diverted from adaptation and degrowth funding paths.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
