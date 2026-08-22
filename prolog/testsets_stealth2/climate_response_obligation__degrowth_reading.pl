% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Reading of the Climate Response Obligation — Absolute Throughput Reduction within Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the climate_response_obligation
 *   kernel: the degrowth_reading, which holds that the obligation's operative
 *   content is absolute reduction of material throughput — sufficiency
 *   prioritized over efficiency — so that human economies fit inside
 *   planetary boundaries. EPSILON REFERENT: the standing arrangement under
 *   contest for this story is the throughput-reduction obligation AS THIS
 *   READING HOLDS THE KERNEL TO REQUIRE, assessed by the reading's own lights
 *   — including its own acknowledgment that Northern lifestyles bear absolute
 *   reduction and that Southern development space hangs on Northern-first
 *   sequencing. It is NOT the current growth economy (a different constraint,
 *   addressed by the sibling readings' files) and NOT a frictionless
 *   post-degrowth steady state (the endorsed-alternative prohibition). The
 *   reading is demanding by its own account: it imposes real costs on
 *   identifiable seats while purchasing a genuine collective good. KEY AGENTS
 *   (by structural relationship): - planetary_systems: ultimate beneficiary
 *   seat (non-agent) — climate, nutrient cycles, biodiversity receive reduced
 *   pressure; cannot act or advocate - future_generations: principal
 *   beneficiary (powerless/trapped) — inherit a stabilized Earth system;
 *   represented only by proxy - global_south_populations: conditional
 *   beneficiary with latent payer exposure (moderate/constrained) —
 *   development space protected only if the North contracts first -
 *   global_north_consumption_class: primary payer (powerful/identity_locked)
 *   — absolute lifestyle reduction demanded; consumption fused with identity
 *   and status - carbon_intensive_industries: institutional payer
 *   (institutional/constrained) — stranded assets, contracted output -
 *   high_throughput_sector_workers: excluded voice (organized/trapped) —
 *   livelihoods slated for contraction, outside agenda-setting rooms -
 *   national_governments_growth_dependent: agenda setter with payer exposure
 *   (institutional/constrained) — must enforce caps while fiscally dependent
 *   on growth - international_climate_regime_bodies: agenda setter
 *   (institutional/generational) — administer fair-share arithmetic and
 *   verification - earth_system_science_community: analytical observer
 *   (moderate/analytical) — quantifies boundary transgressions, holds no
 *   enforcement power
 *
 * KEY AGENTS:
 *   - planetary_systems: ultimate beneficiary seat (non-agent, universal scope) — receives reduced extraction pressure; registers outcomes but cannot act
 *   - future_generations: principal beneficiary (powerless/trapped/civilizational) — bear none of the adjustment costs, collect the entire payoff, speak only through proxies
 *   - global_south_populations: conditional beneficiary with secondary payer exposure (moderate/constrained/global) — fair-share floors if the North moves first; frozen near subsistence if it does not
 *   - global_north_consumption_class: primary payer (powerful/identity_locked/continental) — absolute consumption reduction demanded; exit reads as downward social mobility
 *   - carbon_intensive_industries: institutional payer (institutional/constrained/global) — contraction strands reserves, fleets, and plant
 *   - high_throughput_sector_workers: excluded voice (organized/trapped/regional) — sequencing demands arrive late in venues they do not control
 *   - national_governments_growth_dependent: agenda setter with secondary payer exposure (institutional/constrained/national) — enforcement erodes the fiscal base that funds enforcement
 *   - international_climate_regime_bodies: agenda setter (institutional/generational/global) — compute entitlements, verify compliance, hold no police power
 *   - earth_system_science_community: analytical observer (moderate/analytical/global) — supply the factual substrate every reading argues from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.62).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.58).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Reading of the Climate Response Obligation — Absolute Throughput Reduction within Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '16c16fe7-6bdb-4224-a68a-f06881034ee1').
narrative_ontology:cs_kernel_codification('16c16fe7-6bdb-4224-a68a-f06881034ee1', distributed).
narrative_ontology:cs_authority_grounding('16c16fe7-6bdb-4224-a68a-f06881034ee1', expertise).
narrative_ontology:cs_interpretation_layer_present('16c16fe7-6bdb-4224-a68a-f06881034ee1').
narrative_ontology:cs_reading_relation('16c16fe7-6bdb-4224-a68a-f06881034ee1', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('16c16fe7-6bdb-4224-a68a-f06881034ee1', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('16c16fe7-6bdb-4224-a68a-f06881034ee1', foundational, aggregate_throughput_reduction_required).
narrative_ontology:cs_axiom_status(aggregate_throughput_reduction_required, holdable).
narrative_ontology:cs_axiom_grounding('16c16fe7-6bdb-4224-a68a-f06881034ee1', aggregate_throughput_reduction_required, empirically_contingent).
narrative_ontology:cs_axiom('16c16fe7-6bdb-4224-a68a-f06881034ee1', foundational, sufficiency_precedes_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_precedes_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('16c16fe7-6bdb-4224-a68a-f06881034ee1', sufficiency_precedes_efficiency, deontological).
narrative_ontology:cs_reference_frame('16c16fe7-6bdb-4224-a68a-f06881034ee1', steady_state_within_planetary_boundaries).
narrative_ontology:cs_drift_state('16c16fe7-6bdb-4224-a68a-f06881034ee1', contemporary_overshoot_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('16c16fe7-6bdb-4224-a68a-f06881034ee1', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumption_class).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, carbon_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, national_governments_growth_dependent).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, limits_to_growth_thesis).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, steady_state_economics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The climate system, nitrogen and phosphorus cycles, freshwater stocks, and biodiversity that the obligation is calibrated to protect. Reduced material throughput lowers the pressure placed on them. They register the outcome — stabilization or further transgression — but cannot act, negotiate, advocate, or relocate; every effect on them arrives through what others decide.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% People not yet born who would inherit a stabilized Earth system if aggregate throughput falls in time. They contribute nothing to present decisions and bear none of the adjustment costs; they cannot vote, litigate, or exit. Their entire position is carried by proxy advocates, constitutional argument, and the discount rates the living choose.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% The majority of humanity, whose material consumption remains far below Northern levels. Under the fair-share allocation this reading proposes, their development space is protected precisely because the North contracts first: they gain atmospheric headroom and transfer-funded infrastructure. The same allocation turns punitive if the North delays — caps applied without Northern-first sequencing would freeze their consumption near subsistence while historical emitters keep their accumulated stock. Their exit options are narrow, since migration corridors and export markets are governed by the same Northern policies the obligation reshapes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_populations, payer).

% High-income households in North America, Europe, Japan, and Australia whose mobility, housing, diets, and air travel account for a disproportionate share of global material footprint. The obligation demands absolute reduction of their consumption — smaller homes, fewer flights, plant-rich diets, longer product lives — not merely cleaner consumption. Their social positions, professional networks, and self-presentations are built around these patterns; stepping out of them reads socially as downward mobility, a cost that money does not measure and compensation schemes do not reach.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumption_class, payer,
    powerful, biographical, identity_locked, continental).

% Fossil fuel producers, airlines, cement and steel makers, industrial agriculture, and fast-fashion chains whose business volumes are the throughput the obligation targets. Contraction strands reserves, fleets, and plant; pivoting to low-throughput lines writes off most of their installed capital. They retain formidable lobbying, litigation, and public-relations capacity, which they deploy against the obligation's adoption and against its stringency where adopted.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, carbon_intensive_industries, payer,
    institutional, biographical, constrained, global).

% Coal miners, combustion-engine and refinery workers, industrial farmers, and logistics employees whose livelihoods are slated for contraction under any serious throughput reduction. Unions give them collective voice, but the sufficiency agenda is drafted in ministries, citizens' assemblies, and treaty venues where their sequencing demands — retraining guarantees, regional transition funds, slower timetables — arrive late or symbolically. Their skills, homes, and mortgages are tied to places whose employment base the obligation shrinks.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, high_throughput_sector_workers, excluded,
    organized, biographical, trapped, regional).

% Elected governments that would legislate and enforce the caps, rationing, and standards the obligation requires. Their tax bases, pension funding, employment statistics, and borrowing capacity all scale with GDP, so every year of enforced contraction squeezes the fiscal machinery that pays for enforcement itself. Electoral cycles reward delivering prosperity now; the obligation's payoff accrues on horizons beyond any sitting parliament's tenure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, national_governments_growth_dependent, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, national_governments_growth_dependent, payer).

% Treaty secretariats, scientific assessment structures, and proposed fair-share commissions that would compute national throughput entitlements, verify compliance, and adjudicate disputes. They hold no police power; their leverage is transparency, benchmarking, and the reputational cost of defection. Their continuity depends on the regime remaining legitimate simultaneously in the eyes of Northern payers and Southern claimants — two audiences whose demands diverge.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, international_climate_regime_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Climate scientists, Earth-system researchers, and assessment bodies such as the IPCC that quantify boundary transgressions and publish remaining carbon and material budgets. They supply the factual substrate every reading of the obligation argues from, hold no enforcement power, and absorb sustained political attack whenever their findings imply costs for powerful seats.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, earth_system_science_community, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Planetary sinks and sources are unowned commons: no single actor's restraint stabilizes them, and each actor's incentive is to let others cut first. The obligation sets an aggregate material budget consistent with boundary stability and coordinates simultaneous, verifiable throughput reduction across all parties, with fair-share entitlements specifying who reduces how much.
% TRANSFER_FUNCTION: Moves consumption capacity and throughput entitlements from the present to the future (headroom reserved rather than spent) and from high-consuming Northern populations to under-consuming Southern populations (fair-share floors); moves capital out of volume-dependent sectors; and displaces political attention from efficiency (more output per unit) to sufficiency (deciding how much output is enough).
% ABSENT_VOICES: Future generations are structurally absent and speak only through proxies. High-throughput sector workers and the regions dependent on them are consulted after allocation formulas are set. Most Northern sufficiency proposals assume Southern consent to the fair-share arithmetic rather than negotiating it; and the constituencies for continued growth — consumers and industry — are present mainly as objects of the obligation, not as co-authors of it.
% DISAPPEARANCE_RATIONALE: Proponents hold that everything depends on it: without absolute throughput reduction, boundary transgression proceeds, and the rearrangement arrives later as forced adaptation, climate-driven migration, and abandoned coastlines. Opponents hold that nothing depends on it: markets substitute, efficiency compounds, and the obligation's disappearance merely removes one advocacy program from a crowded field. Because the obligation is not yet embodied in standing institutions, its overnight loss changes little immediately — which side of that dispute is correct is precisely what the sibling readings contest.
% FOUNDING_PROBLEM: Aggregate human material throughput began exceeding planetary regeneration and absorption capacities — flagged systematically in the 1972 Limits to Growth analysis and quantified per-boundary from 2009 onward — while efficiency gains and relative decoupling proved too slow or partial to close the gap, implying that total throughput itself must contract, beginning with the highest consumers.
% FOUNDING_PROBLEM_CORROBORATION: Earth-system assessment bodies outside the degrowth movement corroborate the problem: IPCC working-group reports on emissions and consumption gaps, the Stockholm Resilience Centre's planetary-boundary updates showing six of nine boundaries transgressed, and UNEP material-flow accounting. These sources attest the overshoot problem, not the sufficiency remedy — the claim that throughput reduction outranks decarbonization and resilience as a response is contested by the sibling readings and is corroborated by no body outside this reading's own tradition.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the obligation imposes real, targeted costs — absolute Northern consumption reduction, stranded industrial capital, contested Southern development space — but those costs purchase a genuine collective good rather than any seat's private revenue, so epsilon sits well below snare range while far above rope-range coordination overhead. Suppression 0.58: persistence requires binding caps, rationing or cap-and-share infrastructure, advertising restrictions, and the deliberate ruling-out of the grow-your-way-out alternative that the reading's own premise forecloses; consumer exit is additionally blocked by identity fusion rather than law. Theater 0.20: the program's content is mostly functional (budgets, allocations, verification), though sustainability rhetoric increasingly circulates as corporate branding without throughput cuts — a monitored drift vector. Accessibility_collapse 0.60: the boundary facts close the ignore-it option nearly completely, but allocation design space (who reduces, how fast, with what transfers) remains genuinely open, so collapse sits below mountain range. Resistance 0.75: a blocking coalition of affluent consumers, carbon-intensive industry, and growth-dependent fiscal states actively contests adoption and stringency. MEASUREMENTS run on one shared grid (t=0..53, corresponding to 1972 Limits-to-Growth publication through 2025): base_extractiveness rises as the reading hardens from advisory warning to binding obligation with named payers; suppression_requirement rises as voluntary approaches repeatedly fail and the implied coercion of any sufficient pathway grows; theater_ratio rises as sufficiency language is co-opted by greenwashing. All three series are monotonic — no oscillation, hence no intermittent-reinforcement dynamic to document. SUPPRESSION NOTE: suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled (by directionality and spatial scope) in the engine's computation. COALITION NOTE: the two payer seats plus growth-dependent governments constitute a plausible blocking coalition; the analysis treats their alignment as contingent (industry and consumers defect from each other under rationing designs that exempt basics) rather than as an immutable barrier.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme and structural. From the global_north_consumption_class seat the obligation lands as imposed sacrifice with an identity price — the engine should compute a heavily extractive experience approaching the snare end for that seat. From the future_generations seat it is pure subsidy: costs zero, payoff total. From the global_south_populations seat it is conditional — protective allocation if Northern-first sequencing holds, punitive cap if it does not; the same structure computes oppositely across that seat's two phases. From the national_governments seat the obligation is a coordination duty entangled with fiscal self-harm: enforcing it erodes the growth-dependent revenue that pays for enforcing it. From the international_regime seat it is an arithmetic and legitimacy problem without police power. There is no seat from which the arrangement reads as symmetric — the asymmetry between who decides, who reduces, and who collects is the point of the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (planetary_systems, future_generations, global_south_populations) derive low directionality for those seats — damped effective extraction, with planetary_systems and future_generations nearest the full-beneficiary end (trapped exit, powerless power, universal scope). Victim declarations (global_north_consumption_class, carbon_intensive_industries) derive high directionality — amplified effective extraction — with the consumption class pushed toward the full-target end by identity_locked exit: trapped-or-locked targets sit nearer full target than mobile ones. NO DIRECTIONALITY OVERRIDES are authored, deliberately: the South's conditional position (beneficiary if the North moves first, target otherwise) is invisible to the derivation chain, but the override surface is keyed by power atom, and an override at 'moderate' would misfire onto the other moderate-power seat (earth_system_science_community). The conditionality is therefore routed to the south_conditional_directionality omega instead of a blunt per-power-atom correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against SNARE mislabeling: the obligation imposes heavy, asymmetric costs, but the costs purchase a real collective good — boundary stability — and no named seat captures the gains as revenue; a snare verdict would erase the genuine coordination function that motivates the reading. Against ROPE mislabeling: the asymmetry is structural, not incidental — who reduces is fixed by historical responsibility and capability, enforcement is required to hold the allocation against the blocking coalition, and alternatives (grow-your-way-out) are suppressed by the reading's own premise — so a pure-coordination verdict would erase the extraction the reading itself acknowledges. MANDATE STATUS: the founding problem (overshoot) is live and worsening, so there is no mandate decay to resolve; mandatrophy_resolved is not declared. The lifecycle risk this story monitors is different: if adoption fails and sufficiency language persists as branding, theater_ratio climbs while throughput does not fall — the Goodhart drift by which a demanding obligation degenerates into performed virtue. The theater_ratio series is the early-warning instrument for that trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the operative content of the climate_response_obligation kernel aggregate throughput reduction (this reading), rapid decarbonization within continued growth (mitigation_priority), or resilience investment under committed warming (adaptation_priority)?',
    'Not resolvable by data alone: each reading rests on a different foundational premise. Adherence shifts with empirical inputs — demonstrated absolute decoupling strengthens mitigation_priority; confirmed boundary transgression along decarbonization-only pathways strengthens this reading; warming locked past thresholds strengthens adaptation_priority.',
    'Sibling readings instantiate different constraints with different victim sets and epsilon values: this reading places Northern consumption patterns in the victim set and treats capital accumulation itself as the extractive mechanism; mitigation_priority keeps growth intact and targets carbon intensity; adaptation_priority relocates cost onto exposed populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which premise the kernel''s obligation reduces to — one reading of three.').

omega_variable(
    absolute_decoupling_possibility,
    'Can GDP grow while absolute material throughput and consumption-based emissions fall at the rate planetary boundaries require?',
    'Global consumption-based material footprint and emission accounting over coming decades; sector-level dematerialization audits; systematic comparison of achieved versus required decoupling rates.',
    'If strong absolute decoupling at the required pace is demonstrated, this reading''s foundational empirical axiom fails, epsilon drops toward coordination-cost levels, and the constraint collapses toward mitigation_priority; if not, the throughput-reduction obligation stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_possibility, empirical, 'Empirical hinge on which the reading''s foundational axiom turns.').

omega_variable(
    south_conditional_directionality,
    'Does the Global South experience the throughput-reduction obligation as protected development space (beneficiary) or as capped development imposed without Northern-first contraction (target)?',
    'Implementation pilots with fair-share allocation and resource transfers: observe whether Northern absolute reductions precede Southern caps and whether finance and technology transfers actually materialize.',
    'If Northern-first sequencing fails, the South''s effective directionality flips toward full target, the asymmetry deepens, and the arrangement drifts from tangled_rope toward snare; honored sequencing keeps the South on the beneficiary side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(south_conditional_directionality, empirical, 'Conditionality of the South''s seat under the fair-share allocation.').

omega_variable(
    consumer_identity_fusion_depth,
    'How much of Northern resistance to consumption reduction is interest-based (income, convenience) versus identity-fused (consumption constituting selfhood and status)?',
    'Longitudinal study of communities undergoing mandated consumption reduction (rationing trials, car-free urbanization, frequent-flyer levies): track persistence of opposition after material compensation is provided.',
    'If resistance is identity-fused, suppression requirements exceed what legal enforcement alone supplies — enforcement must reach cultural norms, and the scalar suppression measure understates the coercion a binding regime would need; classification drifts toward higher-suppression profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_identity_fusion_depth, empirical, 'Interest versus identity composition of payer-seat resistance.').

omega_variable(
    democratic_enforcement_feasibility,
    'Can binding throughput caps be enforced through democratic institutions without authoritarian ratchet or permanent emergency powers?',
    'Comparative study of cap-and-share, carbon rationing, and sufficiency-legislation pilots: durability of enforcement, civil-liberties incidence, reversal rates after electoral turnover.',
    'If democratic enforcement proves unstable, the choice collapses to unenforced aspiration (a piton-theater trajectory in which sufficiency language persists without throughput cuts) or coercive enforcement (suppression escalation); either outcome reshapes the classification away from the authored tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_enforcement_feasibility, conceptual, 'Enforcement-mode uncertainty for a binding sufficiency regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__degrowth_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__degrowth_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(clim_tr_t53, climate_response_obligation__degrowth_reading, theater_ratio, 53, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__degrowth_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__degrowth_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(clim_be_t53, climate_response_obligation__degrowth_reading, base_extractiveness, 53, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__degrowth_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__degrowth_reading, suppression_requirement, 50, 0.51).
narrative_ontology:measurement(clim_su_t53, climate_response_obligation__degrowth_reading, suppression_requirement, 53, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'climate response obligation' covers three structurally distinct claims that cannot share one epsilon. This file instantiates the degrowth_reading — the obligation as absolute throughput reduction with sufficiency priority; its epsilon is authored for that obligation as it would bind, by this reading's own lights. Sibling files instantiate mitigation_priority (obligation as rapid decarbonization within growth; victim set defined by carbon intensity, not throughput volume) and adaptation_priority (obligation as resilience investment under committed warming; beneficiary set centered on exposed populations). Upstream substrate: Earth-system boundary science feeds all three readings; this reading additionally cites decoupling-failure evidence as its grounding, making it downstream of the same empirical record the mitigation reading draws on while reaching the opposite prescription. All family members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
