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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response (Growth-Compatible Decarbonization)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate harm prevention asserts that
 *   legitimate climate response centers on emissions reduction via rapid
 *   technological transition to renewable energy, electrification, and
 *   efficiency, all achieved within a continued growth framework. This
 *   reading dominates international climate governance (UNFCCC, Paris
 *   Agreement, net-zero pledges by governments and corporations). Future
 *   generations are cast as the primary beneficiaries (avoided climate
 *   damages); present-generation carbon-intensive sectors and low-income
 *   workers bear transition costs. The constraint is a tangled_rope: it
 *   coordinates genuine technological transition (a real collective-action
 *   problem in energy system restructuring) while asymmetrically extracting
 *   from fossil fuel workers, stranded-asset holders, and present low-income
 *   consumers. The core axiom — that growth and decarbonization are
 *   compatible via technology — is contested by adaptation and degrowth
 *   readings but remains operationalized in policy and capital flows.
 *
 * KEY AGENTS:
 *   - Future generations: powerless, civilizational time horizon, trapped exit — cast as beneficiaries but cannot participate in present decisions; represented through advocacy coalitions
 *   - Climate advocates and environmental NGOs: organized, institutional power, mobile exit — set the mitigation-priority agenda; frame the constraint's legitimacy
 *   - Green technology sector: institutional, biographical horizon, arbitrage exit — gain market access and value from the constraint's persistence
 *   - Carbon-intensive industries: powerful, biographical horizon, constrained exit — face stranded assets and regulatory pressure; resist the constraint
 *   - Fossil fuel workers: moderate power, biographical horizon, identity-locked exit — displaced by transition; constrained bargaining power
 *   - Low-income present consumers: powerless, biographical horizon, trapped exit — bear regressive cost distribution; benefit deferred
 *   - Political center coalitions: institutional, biographical horizon, constrained exit — operationalize and enforce the constraint
 *   - Adaptation advocates: organized, excluded from policy-setting by the constraint's dominance
 *   - Degrowth theorists: moderate power, excluded from mainstream governance by the core axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.54).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Climate Response (Growth-Compatible Decarbonization)").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '6ff8503e-11ad-47d5-ae87-d79007347382').
narrative_ontology:cs_kernel_codification('6ff8503e-11ad-47d5-ae87-d79007347382', fixed_text).
narrative_ontology:cs_authority_grounding('6ff8503e-11ad-47d5-ae87-d79007347382', lineage).
narrative_ontology:cs_interpretation_layer_present('6ff8503e-11ad-47d5-ae87-d79007347382').
narrative_ontology:cs_reading_relation('6ff8503e-11ad-47d5-ae87-d79007347382', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('6ff8503e-11ad-47d5-ae87-d79007347382', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('6ff8503e-11ad-47d5-ae87-d79007347382', foundational, growth_decarbonization_compatibility).
narrative_ontology:cs_axiom_status(growth_decarbonization_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('6ff8503e-11ad-47d5-ae87-d79007347382', growth_decarbonization_compatibility, empirically_contingent).
narrative_ontology:cs_axiom('6ff8503e-11ad-47d5-ae87-d79007347382', foundational, technological_transition_feasibility).
narrative_ontology:cs_axiom_status(technological_transition_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('6ff8503e-11ad-47d5-ae87-d79007347382', technological_transition_feasibility, empirically_contingent).
narrative_ontology:cs_axiom('6ff8503e-11ad-47d5-ae87-d79007347382', secondary, future_generations_primary_beneficiary).
narrative_ontology:cs_axiom_status(future_generations_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('6ff8503e-11ad-47d5-ae87-d79007347382', future_generations_primary_beneficiary, deontological).
narrative_ontology:cs_reference_frame('6ff8503e-11ad-47d5-ae87-d79007347382', paris_agreement_mitigation_framework).
narrative_ontology:cs_drift_state('6ff8503e-11ad-47d5-ae87-d79007347382', contemporary_empirical_feasibility_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6ff8503e-11ad-47d5-ae87-d79007347382', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, low_income_present_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, low_income_present_consumers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_transition_feasibility).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, growth_decarbonization_compatibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in present climate policy decisions but will experience the consequences of either aggressive mitigation (lower damages) or continued inaction (locked into warming trajectory). Represented through climate science, climate advocacy coalitions, and intergenerational ethics frameworks in present discourse. Their interests are inferred rather than directly expressed; they have no bargaining power.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__mitigation_priority, future_generations).

% Frame and promote the mitigation-priority diagnosis: rapid emissions reduction via technological transition is necessary to prevent catastrophic future climate damages and is economically feasible within growth frameworks. Shape international climate governance (UNFCCC, Paris Agreement, national climate laws), scientific consensus (IPCC synthesis), and capital flows (ESG standards, green bonds). Benefit from institutional legitimacy, funding alignment, and policy adoption of their framing.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_advocates_environmental_ngos, agenda_setter,
    organized, generational, mobile, global).

% Solar, wind, battery, carbon capture, electric vehicle, grid modernization, and related technology companies expand rapidly under the mitigation-priority policy regime. Receive government subsidies, preferential procurement, regulatory mandates favoring their products, and investment capital fleeing fossil fuel assets. Accumulate market share and shareholder value as energy infrastructure transitions toward renewable sources. Can exit the constraint by diversifying into other technology markets if renewable saturation occurs.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Oil, gas, coal extraction; coal and gas power generation; cement; steel; petrochemicals; and related carbon-intensive sectors face stranded assets (reserves that become economically unrecoverable), operating restrictions (emission caps, carbon pricing), and pressure to invest in decarbonization or shrink. Cannot easily exit through relocation or market substitution because their infrastructure is geographically fixed and their business models are fundamentally dependent on fossil fuels. Must adapt through acquisition by green technology companies, managed decline, or political resistance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Coal miners, oil rig workers, refinery operators, power plant workers, and related employment faces contraction as fossil fuel operations decline. Job training and retraining programs exist but are often inadequate in capacity or placement success. The communities built around extraction are economically hollowed out. Professional identity, place-based social structure, and family occupation histories are fused to extraction work, making career exit psychologically difficult independent of economic opportunity. Bargaining power is fragmented by geography and industry structure; political influence is declining relative to climate advocates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_workers, payer,
    moderate, biographical, identity_locked, regional).

% Bear immediate, concentrated costs of the energy transition through higher electricity prices (grid modernization, renewable subsidy pass-through), transportation costs (fuel prices, EV transition infrastructure cost), and inflation from green infrastructure investment. Have minimal capacity to absorb cost shocks due to low income and high energy intensity of their consumption. Also benefit from avoided future climate damages, but the benefit is deferred (decades), probabilistic (depends on mitigation success), and diffuse across all future people. Exit options are trapped: cannot change residence, energy source, or transportation mode without substantial sacrifice.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, low_income_present_consumers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, low_income_present_consumers, beneficiary).

% National governments and center-left to center-right political coalitions operationalize the mitigation-priority constraint through carbon pricing, regulatory emission standards, technology mandates, infrastructure investment (renewable power, grid, public transit, building retrofit), and international climate agreements. Must balance mitigation urgency against political feasibility; maintain public support while imposing regressive costs on voter base; enforce the constraint against industry lobbying and grassroots resistance. Exit is constrained by electoral cycles and institutional lock-in: once climate legislation passes, reversal is difficult (voters expect continuation, investor expectations shift).
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, political_center_governing_coalitions, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for prioritizing near-term climate resilience building, managed retreat from vulnerable areas, improved disaster response infrastructure, and agricultural adaptation as the primary response to climate change. Argue that aggressive emissions reduction is politically infeasible in time and that adaptation spending yields faster, more direct benefits to vulnerable populations. Are structurally excluded from mainstream international climate governance (UNFCCC prioritizes mitigation) and funding allocation (most climate finance flows to renewable energy, not adaptation). Their presence in climate negotiations is symbolic; their policy recommendations are marginalized.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_advocates_practical_climate_response, excluded,
    organized, generational, constrained, global).

% Argue that physical limits to material throughput and empirical decoupling rates make growth-compatible decarbonization impossible; contend that legitimate climate response requires planned economic contraction (degrowth) in the Global North, with equity transfers to the Global South to enable development. Are institutionally excluded from mainstream climate policy by the mitigation-priority constraint's core axiom (growth and decarbonization are compatible). Maintain their position in academic, activist, and social movement spaces; lack access to international negotiations and government policy-setting. Their exclusion is not accidental but structural: the constraint's authority depends on maintaining growth-compatibility as a non-negotiable premise.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_theorists_activists, excluded,
    moderate, generational, mobile, global).

% Conduct empirical research on climate physics, emission pathways, technological feasibility, and mitigation costs. Synthesize findings through institutions like the IPCC and national academies. Provide epistemic authority for the mitigation-priority constraint by assessing that emissions reduction is necessary and that specific technological pathways (renewable energy scaling, efficiency, electrification, carbon capture) are technically feasible. Their role is observational and epistemic rather than agenda-setting; they testify to constraints and possibilities but do not set political priorities. Increasing scrutiny on decoupling claims and feasibility assumptions may shift their assessed confidence in the constraint's core axioms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists_consensus_institutions, observer,
    institutional, generational, analytical, global).

% Will experience the heaviest climate impacts (drought, flooding, sea-level rise, agricultural failure) despite having generated minimal cumulative emissions. Have minimal bargaining power in international climate negotiations despite bearing the largest consequences. Are excluded from meaningful seats in policy-setting; their interests are represented by humanitarian advocates and climate justice NGOs, not by their own political power. The mitigation-priority constraint assumes their interests (future damages prevented) are primary beneficiaries, but they are not consulted on whether the constraint's distribution of present costs is acceptable.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_south_vulnerable_populations, excluded,
    powerless, generational, trapped, global).

% Union and labor organizations represent workers across industries, including both fossil fuel and green technology sectors. Advocate for 'just transition' policies (job training, wage support, pension protection for displaced workers) and for inclusion in climate and energy policy-setting. Are excluded from primary agenda-setting in international climate governance, though increasingly present in national-level negotiations. Push back against uncompensated transition costs and demand active policy investment in worker retraining and community resilience. Exit is constrained by labor dependence on whatever economy remains; cannot readily shift sectoral focus.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_north_labor_movements, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, green_technology_sector).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global alignment on the necessary magnitude of emissions reduction, the technological pathways for achieving it, the capital flows to support renewable energy and efficiency infrastructure, and the labor reallocation from fossil fuel to clean-energy sectors. Solves the collective-action problem of energy system transition: individual firms or nations cannot decarbonize alone without competitive disadvantage; the constraint establishes coordinated expectations and shared investments.
% TRANSFER_FUNCTION: Moves capital value from stranded fossil fuel assets to renewable energy and technology companies. Moves present consumer expenditure (via carbon pricing, grid modernization cost, green infrastructure investment) from workers and low-income households to green technology providers and capital owners. Postpones climate damages (externalized to future generations and disproportionately to the Global South) through present-generation investment in mitigation. Moves employment from fossil fuel sectors to green technology and infrastructure sectors, with unequal reallocation across geographies and skill levels.
% ABSENT_VOICES: Workers in fossil fuel sectors lack effective seats in international climate policy negotiations; adaptation advocates and degrowth theorists are structurally excluded from agenda-setting; Global South populations and vulnerable communities who will bear the heaviest climate impacts are marginalized in decision-making despite having the largest stake. Future generations are cited as primary beneficiaries but cannot participate in present deliberation.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority constraint and its enforcement vanished, capital currently flowing to renewable energy and grid modernization would reverse direction toward fossil fuel infrastructure expansion; regulatory emission standards would relax or disappear; carbon pricing would collapse; international climate agreements would lose enforcement; green technology investment would decline sharply. The political coalition supporting present transition costs would lose its legitimating frame. Energy systems would reorganize around fossil fuel dependence, cumulative warming would accelerate toward higher damage scenarios, and the distributional conflicts (stranded assets, worker displacement, regressive consumer costs) would be resolved in favor of incumbent carbon-intensive industries.
% FOUNDING_PROBLEM: Atmospheric carbon concentration from fossil fuel combustion and land use change is rising exponentially, creating risk of climate system bifurcations, ecological tipping points, and civilization-scale disruption (agricultural failure, sea-level rise, mass displacement, infrastructure collapse). This externality is not priced into market signals; fossil fuel extraction internalizes extraction costs and profits but externalizes climate damages to all affected populations and to future generations. Voluntary corporate and individual action is insufficient to prevent accumulation; coordinated policy intervention is necessary.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists (IPCC, NASA, national academies) attest that atmospheric CO2 is rising and that current warming trajectory (without strong mitigation) exceeds targets for limiting dangerous climate change. Carbon-intensive industries and some economic analysts contest the urgency and feasibility of rapid decarbonization, but do not deny the underlying physical accumulation of atmospheric CO2. Adaptation advocates attest the founding problem is live but argue the mitigation-priority response misallocates resources relative to adaptation. Degrowth theorists attest the founding problem is live and that the mitigation-priority response is insufficient and misdirected. Global South vulnerable populations attest that climate impacts are already occurring in their regions and that mitigation urgency is real, though distribution of response burden is unfair.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) and rising through the interval, then plateaus. Early extractiveness (0.48) reflects coordination value: the constraint solved a genuine problem (aligning capital and policy around decarbonization). Rising extractiveness (0.48→0.69 at t=30) reflects accumulating cost asymmetry: low-income consumers absorb regressive energy cost increases while green technology owners capture value gains; fossil fuel workers absorb identity-locked displacement while climate advocates gain institutional legitimacy. The plateau at t=30 suggests the constraint reaches an equilibrium of enforced stability — further extraction requires increased suppression (which rises with t), but additional extraction yields diminish as the low-hanging technological gains are exhausted and marginal transition costs rise. Theater ratio rises from 0.22 to 0.44, indicating growing performative activity: corporate net-zero pledges with weak interim targets, offset schemes with questionable additionality, and 'greenwashing' by fossil fuel companies increase relative to actual emissions reductions. Suppression requirement rises steadily (0.38→0.55) as the constraint must increasingly active-enforce against both industry resistance (capital flight from carbon-price regimes) and bottom-up defection (political backlash to regressive costs, labor resistance in fossil fuel regions). Resistance is high (0.71) from the outset because the constraint produces concentrated losses (for workers, industries, low-income consumers) and diffuse gains (future generations, technology investors); this asymmetry drives persistent resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute different types from the same structural data. From the agenda-setter seat (climate advocates, political center), the constraint computes as tangled_rope with substantial coordination value offsetting the extraction; the founding problem is live and the response is legitimate. From the payer seats (workers, industries, low-income consumers), the same constraint computes closer to snare: the coordination function is secondary to the extraction; the founding problem (future climate damages) is real but too distant and uncertain to justify present sacrifice by powerless agents. The engine computation per-seat will surface this divergence. The authored claim is tangled_rope because the coordination function (aligning capital and policy toward decarbonization) is genuine and necessary; but the metrics reflect that the extraction component dominates the distribution network.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations (cast as beneficiary, but non-agent placeholder): high d toward target end (d≈0.9) because they cannot exit, have no present bargaining power, and bear the deferred consequences of present inaction. Their interests are inferred by advocates, not directly expressed. Climate advocates and environmental NGOs: low d (d≈0.2) — they set the agenda, gain institutional legitimacy, benefit from funding and policy priority aligned with their framing; moderate exit (mobile to other advocacy causes, but career-locked to environmentalism). Green technology sector: low d (d≈0.15) — institutional power, arbitrage exit to other markets if renewable technology saturates, direct capture of subsidy value. Carbon-intensive industries: high d (d≈0.8) — constrained exit (existing assets are stranded, market access is policy-regulated), powerful but in defensive position against the constraint's enforcement. Fossil fuel workers: highest d (d≈0.95) — powerless, identity-locked exit (place, profession, community are constituted through extraction work), concentrated immediate costs. Low-income consumers: high d (d≈0.75) — powerless, trapped exit, regressive cost impact. Political center: moderate-high d (d≈0.55) — institutional power but constrained by need to maintain political coalition; must balance mitigation urgency against present-voter opposition to regressive costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (future climate damages are accelerating), but the mandatrophy test asks: does the constraint persist because it solves the founding problem, or despite the founding problem shifting? Theater ratio rising from 0.22 to 0.44 indicates growing performative activity relative to functional emissions reduction. Corporate net-zero pledges proliferate with weak interim targets; offset schemes lack additionality; regulatory standards are watered down by industry lobbying; and the constraint increasingly requires active suppression (enforcement) to prevent capital flight and political defection. This pattern suggests emerging mandatrophy: the constraint's original mandate (align capital and policy toward actual emissions reduction) is giving way to a secondary mandate (maintain the appearance of climate action while protecting profitability and growth). The theater ratio rising toward the ceiling while actual emissions reductions plateau (or decelerate) is the diagnostic signal. The base_properties.mandatrophy_resolved field is left unset, indicating the story does not declare resolution; the rising theater ratio and suppression requirement in measurements provide the temporal evidence for T17 abductive triggers (mountain_extraction_accumulation, theater_drift_threshold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decarbonization_compatibility,
    'Is technological decarbonization physically and economically compatible with continued global growth, or does sustained growth require material throughput that decouples from emissions reductions only marginally?',
    'Long-term empirical tracking of global GDP growth, energy consumption, material extraction, and absolute emissions; comparison of decoupling rates to the 2.3%+ annual decarbonization required for 1.5°C pathways; physical audit of renewable energy capacity scaling relative to total energy demand growth.',
    'If full decoupling is not achievable at required speed, the mitigation-priority reading''s core axiom (growth + decarbonization) fails, and the constraint collapses into de facto degrowth or adaptation-priority. The authority grounding shifts from technological feasibility to political necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decarbonization_compatibility, empirical, 'Whether growth and decarbonization are physically decoupled or economically and thermodynamically coupled.').

omega_variable(
    cost_asymmetry_legitimacy,
    'Is the regressive distribution of present transition costs (borne by low-income, fossil fuel dependent populations) justified by the deferred, probabilistic benefit to future generations?',
    'Normative deliberation across seats; empirical measurement of cost incidence by income decile and regional dependence on carbon-intensive industries; comparison of cost-benefit ratios with intergenerational discount rates; political representation of present-generation losers in decision-making.',
    'If the asymmetry is not legitimated, the constraint loses its Tangled Rope coordination framing and appears as snare (concentrated extraction from present payers). Political coalitions supporting the constraint would fracture, and enforcement would require higher suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_asymmetry_legitimacy, preference, 'Whether present sacrifice to prevent future harm is ethically and politically justified when present populations bear concentrated costs.').

omega_variable(
    adaptation_mitigation_tradeoff,
    'At what warming threshold do residual climate damages exceed the cost and disruption of aggressive mitigation, making adaptation the cost-minimizing response even if mitigation is theoretically feasible?',
    'Integrated assessment models comparing mitigation pathways (carbon price, energy transition cost) with adaptation pathways (infrastructure resilience, disaster response, managed relocation); empirical tracking of actual warming trajectory and adaptation capacity; political negotiation of acceptable residual risk.',
    'If the threshold is crossed before mitigation reaches decarbonization targets, the adaptation_priority reading becomes structurally dominant, and the mitigation-priority constraint loses its legitimating logic. Political coalitions shift toward adaptation funding and away from mitigation mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_tradeoff, empirical, 'The cost-minimizing climate response path given actual decarbonization speed and residual warming damages.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.54 at interval end) primarily structural (economic barriers to capital flight, regulatory sanctions for non-compliance) or internalized (fossil fuel worker identification with extraction, low-income consumer belief that transition costs are inevitable and uncontestable)?',
    'Post-regulation trajectory: if suppression persists after policy enforcement is relaxed (e.g., carbon pricing is lifted), the suppression is internalized; if it collapses, the suppression is structural. Qualitative evidence from worker interviews, consumer surveys, and capital reallocation patterns.',
    'If suppression is primarily internalized, exit from the constraint is psychologically harder for target populations than structural barriers suggest; effective extraction is higher. If structural, policy reform can quickly dissolve the constraint''s hold. The distinction affects long-term stability and the possibility of reversing the constraint''s enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the constraint''s suppressive force is external coercion or internalized compliance.').

omega_variable(
    identity_lock_fossil_fuel_workers,
    'For workers in fossil fuel extraction and power generation, is identity-locked exit the accurate characterization, or can career and geographical mobility be enabled through training, investment, and community restructuring?',
    'Tracking of worker outcomes across jurisdictions with different transition support programs; comparison of communities with active retraining/diversification investment vs. those with minimal support; measurement of psychological/social costs of exit relative to economic opportunity.',
    'If identity-lock is accurate and persistent, the constraint extracts from workers with no real exit, and classification shifts toward snare. If exit can be enabled through policy investment, the constraint remains Tangled Rope with distributional unfairness but genuine coordination. This determines whether the constraint requires fundamental restructuring or targeted support policy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_fossil_fuel_workers, empirical, 'Whether fossil fuel workers'' identity fusion to extraction work is an irreducible structural property or a function of inadequate transition support.').

omega_variable(
    vindicated_propositions_falsifiability,
    'Are the vindicated propositions (technological_transition_feasibility, growth_decarbonization_compatibility) empirically testable and falsifiable, or are they maintained as normative commitments regardless of empirical drift?',
    'Tracking of scientific and economic literature testing the propositions; measurement of actual decarbonization rates and technology scaling vs. modeled requirements; institutional responses if empirical evidence challenges the propositions.',
    'If the propositions prove empirically false but remain institutionally vindicated, the constraint''s authority grounding shifts from expertise to extraction (the benefiting institutions defend the propositions regardless of evidence). This would reclassify the constraint from tangled_rope toward snare and indicate mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vindicated_propositions_falsifiability, empirical, 'Whether the constraint''s legitimating propositions remain open to falsification or are protected from empirical challenge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__mitigation_priority, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__mitigation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__mitigation_priority, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__mitigation_priority, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__mitigation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__mitigation_priority, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__mitigation_priority, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__mitigation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__mitigation_priority, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__mitigation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__mitigation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__mitigation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__mitigation_priority, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(clim_be_t30, observed).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__mitigation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__mitigation_priority, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__mitigation_priority, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__mitigation_priority, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__mitigation_priority, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__mitigation_priority, suppression_requirement, 25, 0.53).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__mitigation_priority, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__mitigation_priority, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, energy_transition_labor_displacement).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, carbon_pricing_regressive_distribution).

% DUAL FORMULATION NOTE:
% This constraint is one reading (mitigation_priority) of the contested climate_harm_prevention kernel. The sibling readings (adaptation_priority, degrowth_reading) are separate constraint stories with distinct ε values, beneficiary/victim structures, and temporal trajectories. They are linked via network edges to model the constraint-family relationship and the possibility that upstream empirical drift or political shift could make sibling readings structurally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
