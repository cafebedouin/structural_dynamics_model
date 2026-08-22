% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Regime (2°C Limit via Carbon Markets, Innovation, and Growth Compatibility)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate-response kernel is the
 *   operative international regime: a formalized commitment to limiting
 *   warming below 2°C, pursued through emissions reductions priced and traded
 *   via carbon markets, accelerated by technological innovation policy, and
 *   explicitly reconciled with continued GDP growth. This story instantiates
 *   that reading only; the adaptation-first and degrowth readings are
 *   separate constraints in the same kernel family. The standing arrangement
 *   under contest is the existing mitigation-first architecture (UNFCCC/Paris
 *   temperature goal, NDC cycle, Article 6 markets, green industrial policy),
 *   and epsilon is authored for that arrangement as this reading's own lights
 *   assess it — including its internal equity mechanisms (common but
 *   differentiated responsibilities, climate finance), which persistently
 *   underdeliver against their own stated terms. The claim/metric gap is
 *   deliberate: the regime self-describes as pure coordination on a global
 *   externality (a rope), while the authored structural data show both
 *   genuine coordination and asymmetric extraction running through the same
 *   structures — the engine measures that divergence; do not reconcile the
 *   claim to the metrics.
 *
 * KEY AGENTS:
 *   - innovation_capacity_nations: primary beneficiary (institutional/arbitrage) — capture the transition value chain, standards rents, and carbon market revenue
 *   - carbon_market_intermediaries: secondary beneficiary (organized/arbitrage) — collect fees on market machinery regardless of credit integrity
 *   - green_technology_industries: beneficiary (organized/mobile) — demand created by mandates, subsidy, and procurement
 *   - unf_climate_regime: agenda_setter (institutional/constrained) — administers the temperature goal, market rules, and stocktake
 *   - high_emitting_sector_workers: primary payer (moderate/trapped) — concentrated regional transition costs
 *   - global_south_vulnerable_populations: payer (powerless/trapped) — absorb deferred adaptation costs and residual impacts
 *   - future_generations: payer and excluded (powerless/trapped) — inherit the residual with no seat and no exit
 *   - fossil_dependent_developing_nations: payer (moderate/constrained) — stranded assets and leapfrog costs
 *   - fossil_fuel_incumbents: payer (institutional/arbitrage) — bear stranding but hedge by acquiring the constraint's subsidized machinery
 *   - climate_vulnerable_island_states: excluded (organized/trapped) — procedural voice, subordinated survival claims
 *   - degrowth_climate_justice_movements: excluded (organized/constrained) — outside the frame their critique targets
 *   - integrated_assessment_modeling_community: observer (institutional/analytical) — defines feasibility inside the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.55).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Mitigation-Priority Climate Regime (2°C Limit via Carbon Markets, Innovation, and Growth Compatibility)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '1a4c58bc-2383-493a-bf6b-f79cd147bc4e').
narrative_ontology:cs_kernel_codification('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', formalized).
narrative_ontology:cs_authority_grounding('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', expertise).
narrative_ontology:cs_interpretation_layer_present('1a4c58bc-2383-493a-bf6b-f79cd147bc4e').
narrative_ontology:cs_reading_relation('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', climate_response_action__adaptation_priority, forecloses).
narrative_ontology:cs_reading_relation('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', foundational, temperature_rise_is_limitable_below_two_degrees).
narrative_ontology:cs_axiom_status(temperature_rise_is_limitable_below_two_degrees, holdable).
narrative_ontology:cs_axiom_grounding('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', temperature_rise_is_limitable_below_two_degrees, empirically_contingent).
narrative_ontology:cs_axiom('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', foundational, climate_response_must_preserve_gdp_growth).
narrative_ontology:cs_axiom_status(climate_response_must_preserve_gdp_growth, holdable).
narrative_ontology:cs_axiom_grounding('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', climate_response_must_preserve_gdp_growth, instrumental).
narrative_ontology:cs_reference_frame('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', growth_compatible_cost_optimal_mitigation).
narrative_ontology:cs_drift_state('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', post_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a4c58bc-2383-493a-bf6b-f79cd147bc4e', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_capacity_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, green_technology_industries).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sector_workers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_dependent_developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, green_growth_decoupling_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, cost_optimal_abatement_pathways).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, carbon_pricing_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty architecture: convenes the COP process, maintains the temperature goal and transparency framework, supervises carbon market rules under Article 6, and runs the global stocktake. Its institutional identity is fused with the mitigation-first frame remaining the organizing principle; consensus rules mean the agenda moves only as fast as innovation-capacity nations and large emitters permit. It cannot exit its role without dissolving the coordination it exists to provide.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, unf_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Export the constraint's required technology: solar, wind, batteries, electric vehicles, heat pumps, and prospectively carbon removal. They write industrial policy that routes subsidy and procurement to domestic firms, shape standards and intellectual property regimes, and capture carbon market revenue through project development and financial services. They also bear real abatement costs at home, but the transition's value chain concentrates in their industries and treasuries; their diversified economies let them reposition if the frame shifts.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_capacity_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Operate the market machinery: registries, verification bodies, offset developers, trading desks, and exchanges that take fees on every credit issued and traded. Revenue scales with transaction volume regardless of whether credits represent real abatement. If a given market or standard collapses under integrity scandal, they can re-register under a new scheme or jurisdiction.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, immediate, arbitrage, global).

% Manufacturers and project developers whose demand is created by the constraint: mandates, subsidies, carbon prices, and procurement targets convert the temperature goal into orders. Capital is mobile across jurisdictions and relocates to wherever subsidy regimes are most favorable; their commercial interest tracks the continuation of the mitigation frame specifically, not climate policy generally.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, green_technology_industries, beneficiary,
    organized, biographical, mobile, global).

% Coal miners, combustion-engine autoworkers, and heavy-industry labor concentrated in specific regions bear the transition costs the constraint concentrates on high-emitting sectors: plant closures, skills that do not transfer, and regional tax-base collapse. Retraining programs arrive slower than closures; moving means leaving family networks and depreciated housing. Their exposure is to the speed and sequencing of the constraint, which they do not set.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_sector_workers, payer,
    moderate, biographical, trapped, regional).

% States whose fiscal base and development strategy are built on hydrocarbon exports face stranding of the very assets their development finance was collateralized against, while being asked to leapfrog directly to imported technology they do not manufacture. Debt distress limits their room to absorb transition costs; their leverage in the regime is procedural rather than material.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_dependent_developing_nations, payer,
    moderate, generational, constrained, national).

% Communities in low-lying, drought-exposed, and heat-stressed regions absorb the residual impacts the constraint's deferral leaves unaddressed: the adaptation costs the mitigation-first frame pushes into the future arrive in their villages first. Adaptation finance promised in exchange has repeatedly underdelivered against assessed need. Migration is the exit, and it is the one receiving states are progressively closing.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_vulnerable_populations, payer,
    powerless, generational, trapped, continental).

% Hold no seat in any negotiation yet inherit the constraint's residual: whatever warming the 2°C-and-overshoot pathway leaves, whatever removal capacity fails to materialize, and whatever adaptation debt accumulates. Their interests enter only through advocacy proxies such as youth litigation and ombudsman proposals, none of which carry agenda power. There is no exit because there is no elsewhere in time.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, future_generations, excluded).

% Producers and refiners face demand erosion and stranded reserves under the constraint, and pay through carbon prices and disclosure regimes. Their response is hedging: acquiring carbon-removal and offsets businesses the constraint subsidizes, funding transition narratives that slow sequencing, and repositioning as broader energy companies. They bear real costs but retain the capital to shape how the costs are distributed.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_fuel_incumbents, payer,
    institutional, biographical, arbitrage, global).

% A coalition of low-lying states whose existential exposure gives them moral standing and procedural voice — they forced the 1.5°C aspiration into the Paris text — but not agenda power: the 2°C architecture, the market mechanisms, and the growth-compatibility premise were set without their consent, and the compensation their survival arguably requires is treated as discretionary finance rather than obligation.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_vulnerable_island_states, excluded,
    organized, civilizational, trapped, regional).

% Movements arguing that the constraint's growth-compatibility premise is itself the problem: that sufficiency, redistribution, and reduced material throughput would cut emissions faster and more justly than technology substitution. They are outside the policy conversation their critique targets — heard in protests and side-events, absent from negotiation mandates and from the model scenarios that define what counts as feasible.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Produces the cost-optimal pathways that define what feasibility means inside the frame: carbon budgets, abatement cost curves, and the heavy reliance on future carbon removal that keeps 2°C arithmetically reachable while near-term cuts stay shallow. Their assumptions discipline the policy conversation; their models are where the growth-compatibility premise is technically enforced.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, integrated_assessment_modeling_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, innovation_capacity_nations).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine global externality problem: no single nation internalizes climate damages, so coordinated emission-reduction commitments, common measurement and verification, a shared temperature target, carbon pricing, and pooled innovation investment align decentralized action in a way unilateral action cannot.
% TRANSFER_FUNCTION: Moves abatement costs onto current high-emitting sectors and their workforces; moves carbon market revenue, subsidy flows, standards rents, and intellectual property income to innovation-capacity nations and market intermediaries; moves development headroom from fossil-dependent developing nations (constrained growth paths, stranded collateral) to technology exporters; and moves residual climate risk — the damages left unaddressed by the deferral — onto future generations and vulnerable populations in the Global South.
% ABSENT_VOICES: Future generations have no seat at all; Global South vulnerable populations hold procedural voice without agenda power; degrowth and climate-justice movements are structurally outside the frame their critique targets; island states' survival claims were subordinated in the 2°C compromise. The unanimity of the policy consensus rests partly on these seats never having been in the room where feasibility was defined.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the NDC architecture, compliance-grade carbon markets, green industrial policy, and the entire innovation pipeline calibrated to it would lose their organizing frame; abatement efforts would fragment into unilateral measures, carbon markets would collapse to voluntary niches, and the burden-sharing contest the arrangement currently defers would reopen immediately among states.
% FOUNDING_PROBLEM: Built to solve the global collective-action failure on the greenhouse-gas externality after the Kyoto era's top-down binding allocation collapsed: how to coordinate emission cuts across sovereign states without requiring binding burden-sharing or sacrificing economic growth — answered with a temperature target, market mechanisms, and innovation substitution.
% FOUNDING_PROBLEM_CORROBORATION: The externality problem remains live by testimony outside the benefiting parties: IPCC assessment reports and atmospheric CO2 records document continued emissions growth against the target trajectory, and reinsurance industry loss data plus UNEP adaptation-gap accounting document the accumulating residual. Innovation-capacity nations also attest the problem is live, but the corroborating sources above sit outside the beneficiary set; no source outside any party disputes that the externality itself persists.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 because the arrangement genuinely coordinates a real externality while running regressive transfers through the same structures: market rents and value-chain capture flow to innovation-capacity seats, while transition costs concentrate on trapped workforces and residual risk defers to unrepresented parties. Suppression is 0.55 and is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation (only extractiveness is): it combines legally binding market participation and disclosure regimes with the discursive foreclosure of rival framings from feasibility definitions. Theater is 0.45: the pledge wave of the early 2020s produced commitments unbacked by policy, and a large share of nature-based offset cohorts have failed integrity re-analysis, but real abatement policy (renewable deployment, EU and comparable regulatory backfill) does function. Accessibility collapse is low (0.35) because the sibling readings remain visibly live — alternatives to the frame have not collapsed. Resistance is 0.55: fossil incumbents, petrostates, and justice movements contest sequencing and burden-sharing without breaking the regime. The measurement series run on one shared time grid (1997–2026) so every tracked metric is authored at every examined point. The theater series shows a COP-cycle oscillation superimposed on its rise — ambition spikes at stocktake and pledge moments, then drifts — and that oscillation is partly the extraction mechanism itself (pledge cycles generate reputational credit without forcing compliance, an intermittent-reinforcement structure). The 2023-to-2026 theater decline reflects integrity reform (offset core-principles regimes, Article 6.4 rules) and policy backfill, not a completed correction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the agenda-setter's seat the arrangement is the coordination it administers and cannot survive without; from the innovation-capacity and intermediary seats it is a value chain they profit from and can reposition within; from the trapped payer seats — regional transition workforces, Global South populations, fossil-dependent states — the same structures operate as extraction whose sequencing they do not control. Same-level dynamics matter: fossil-dependent developing nations and innovation-capacity nations are both states at nominally similar standing, but differentiated exit options (constrained versus arbitrage) and opposite positions (payer versus beneficiary) split their computed seats. Island states experience the coordination rhetoric as covering an existential exposure the 2°C compromise priced in. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (innovation_capacity_nations, carbon_market_intermediaries, green_technology_industries) derive low directionality — the arrangement subsidizes them, and their arbitrage-grade or mobile exit places them nearest the beneficiary end. Declared victims derive high directionality, modulated by exit: future_generations and global_south_vulnerable_populations (powerless, trapped) sit nearest the full-target end; high_emitting_sector_workers (trapped, regionally concentrated) are near-full targets on the transition-cost channel; fossil_dependent_developing_nations (constrained exit) are strong targets with partial damping. Fossil_fuel_incumbents are payers whose arbitrage exit damps effective extraction — they bear costs but can acquire positions inside the constraint's subsidized machinery. The unf_climate_regime as agenda-setter sits low: it collects institutional persistence rather than rents. Scope amplification applies at the global scope of the market machinery, where verification of credit integrity is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the unpriced global externality) is live, so the arrangement is not mandatrophy-resolved; the classification as tangled_rope is what prevents mislabeling in both directions. Reading the regime as a pure rope — its own self-description — would erase the named victims: the workforces bearing concentrated transition costs, the populations absorbing deferred adaptation, and the generations holding no seat. Reading it as a snare would erase the genuine coordination that any climate response requires and that persists independently of the market machinery's integrity. The tangled classification holds both facts in one structure. The forward risk is component-level piton drift: if credit integrity continues to fail, the market component becomes performance (theater rising past functional coordination) administered by an agenda-setter whose cost to fix it exceeds what it bears — the fixing-cost data here (prohibitive) and the diffuse-benefit structure of reform make that drift the live failure mode to monitor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_removal_scalability,
    'Will carbon dioxide removal scale to the multi-gigatonne levels the 2°C pathway''s residual-emissions arithmetic requires, at costs compatible with the growth-compatibility premise?',
    'Deployment and cost-curve tracking of BECCS and direct air capture against integrated-assessment requirement levels; comparison of announced versus operating capacity and realized removal volumes.',
    'If removal fails to scale, residual emissions have no sink: the constraint must either tighten sharply on current actors (deeper near-term cuts, raising extraction on present payers) or breach the 2°C limit (dissolving this reading''s coordination claim and shifting salience to the adaptation and degrowth siblings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_removal_scalability, empirical, 'Feasibility of the carbon-removal assumption the pathway arithmetic depends on.').

omega_variable(
    gdp_decoupling_rate,
    'Can GDP decouple from emissions at the sustained rates the growth-compatible 2°C pathway requires, or is the decoupling assumption functioning as a promise rather than an observation?',
    'Compare historical absolute-decoupling episodes and their durations against required pathway rates; decompose observed decoupling into efficiency, structural shift, and offshoring components.',
    'If required decoupling rates exceed anything durably observed, the growth axiom and the temperature axiom collide inside this reading — forcing either open abandonment of one axiom (frame collapse toward the degrowth sibling) or silent target breach (rising theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gdp_decoupling_rate, empirical, 'Whether the growth-compatibility premise survives contact with required decoupling rates.').

omega_variable(
    carbon_market_credit_integrity,
    'What fraction of issued carbon credits represent additional, permanent abatement rather than counterfactual, non-permanent, or double-counted activity?',
    'Independent offset integrity audits and academic re-analysis of credit cohorts, reconciled against registry issuance and retirement data.',
    'A high ghost-credit share converts the market component from coordination into extraction with performative cover: extraction and theater rise at the market seats and the arrangement drifts toward snare-flavored operation at those seats while retaining genuine coordination elsewhere in the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_market_credit_integrity, empirical, 'Integrity of the market mechanism the coordination claim partially rests on.').

omega_variable(
    intergenerational_representation_mechanism,
    'Can future generations'' interests acquire enforcement capacity inside a constraint whose compliance runs on present electoral and market cycles?',
    'Track institutional experiments: youth climate litigation outcomes, future-generations ombudsman proposals, and constitutional environmental-rights provisions, assessed for whether they acquire agenda power or remain advisory.',
    'If no mechanism acquires teeth, the deferral is structurally locked — the future_generations seat remains permanently powerless and the transfer function''s intergenerational component is unfixable within this frame; if one succeeds, the victim set gains agenda power and the extraction profile shifts materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_representation_mechanism, conceptual, 'Whether the constraint''s most exposed victim seat can ever be represented within it.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel climate_response_action — would instantiating the adaptation_priority or degrowth_transformation reading restructure the beneficiary and victim sets and the epsilon profile entirely?',
    'Author the sibling stories and compare computed seat classifications: adaptation_priority makes under-protected present-day vulnerable populations the central victims and resilience investors the beneficiaries; degrowth_transformation inverts the growth axiom and makes high-throughput consumers the payers.',
    'The classification of this arrangement as tangled_rope does not transfer to the siblings; the kernel contest is over which arrangement is the referent, and each reading yields a different constraint with its own epsilon — cross-reading epsilon comparisons are invalid by the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story''s identity as the mitigation_priority reading and what siblings would change.').

omega_variable(
    adaptation_deferral_quantification,
    'How large is the residual climate damage this pathway defers onto vulnerable regions relative to the adaptation investment the adaptation_priority reading would front-load?',
    'Adaptation finance gap accounting (UNEP Adaptation Gap series) against residual-damage projections under observed mitigation trajectories rather than pledged ones.',
    'Quantifies the transfer function''s regressive core: a large deferral gap supports high effective extraction at the global_south seat and strengthens the reading that the coordination benefit is purchased partly with the welfare of parties who never agreed to the trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deferral_quantification, empirical, 'Magnitude of the costs the mitigation-first frame shifts onto unprotected regions.').

omega_variable(
    alternative_frame_suppression_ambiguity,
    'Is the foreclosure of degrowth and sufficiency framings from policy feasibility a structural suppression (agenda control, model discipline, negotiation mandates) or an internalized one (policymakers unable to conceive alternatives)?',
    'Compare feasibility claims across institutional contexts: where sufficiency and demand-side policies have actually been enacted, test whether the prior impossibility claims held.',
    'If suppression is substantially internalized, it persists even after agenda control loosens and the suppression metric understates the constraint''s stickiness; if structural, agenda reform would collapse it quickly — the two cases imply different reform leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_frame_suppression_ambiguity, conceptual, 'Structural versus internalized mechanism behind the exclusion of rival climate framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 1997, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1997, climate_response_action__mitigation_priority, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(clim_tr_t2005, climate_response_action__mitigation_priority, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(clim_tr_t2009, climate_response_action__mitigation_priority, theater_ratio, 2009, 0.3).
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__mitigation_priority, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(clim_tr_t2020, climate_response_action__mitigation_priority, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(clim_tr_t2023, climate_response_action__mitigation_priority, theater_ratio, 2023, 0.52).
narrative_ontology:measurement(clim_tr_t2026, climate_response_action__mitigation_priority, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t1997, climate_response_action__mitigation_priority, base_extractiveness, 1997, 0.4).
narrative_ontology:measurement(clim_be_t2005, climate_response_action__mitigation_priority, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(clim_be_t2009, climate_response_action__mitigation_priority, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(clim_be_t2015, climate_response_action__mitigation_priority, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(clim_be_t2020, climate_response_action__mitigation_priority, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(clim_be_t2023, climate_response_action__mitigation_priority, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(clim_be_t2026, climate_response_action__mitigation_priority, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1997, climate_response_action__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(clim_su_t2005, climate_response_action__mitigation_priority, suppression_requirement, 2005, 0.36).
narrative_ontology:measurement(clim_su_t2009, climate_response_action__mitigation_priority, suppression_requirement, 2009, 0.41).
narrative_ontology:measurement(clim_su_t2015, climate_response_action__mitigation_priority, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(clim_su_t2020, climate_response_action__mitigation_priority, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(clim_su_t2023, climate_response_action__mitigation_priority, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(clim_su_t2026, climate_response_action__mitigation_priority, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' decomposes into three structurally distinct constraint stories under the epsilon-invariance principle. This story (mitigation_priority) authors the emissions-reduction-first arrangement: coordination through markets and innovation, extraction through regressive cost concentration and intergenerational deferral. The adaptation_priority sibling authors a resilience-investment arrangement with a different victim set (under-protected present populations) and different beneficiaries. The degrowth_transformation sibling authors a throughput-reduction arrangement that treats the growth-compatibility premise itself as the extraction. The upstream reading (this one) is the dominant regime whose deferral creates the conditions — accumulating residual impacts and the adaptation finance gap — that raise the adaptation sibling's salience and the degrowth sibling's critique; the network edges encode that downstream pressure. Each story carries its own epsilon over its own standing arrangement; the family link is structural, not a shared measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
