% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Climate Response Obligation — Mitigation Priority Reading (Standing Fossil Arrangement Under Contest)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation_priority reading of the kernel
 *   climate_response_obligation: the claim that intergenerational justice
 *   requires minimizing warming and that the present generation is obligated
 *   to rapid decarbonization. Per the ε-referent rule for kernel-reading
 *   stories, ε is authored for the standing arrangement under contest — the
 *   fossil-fueled global energy economy and its political defense, which
 *   continues to externalize climate costs onto parties with no vote, no
 *   market presence, and no exit — assessed by this reading's own lights. The
 *   reading assesses that arrangement as heavily cost-shifting: genuine
 *   energy coordination for billions, with the climate bill transferred to
 *   the future and to populations that emitted almost nothing. The
 *   decarbonized arrangement this reading advocates is NOT the referent and
 *   receives no ε here. Sibling readings (adaptation_priority,
 *   degrowth_reading) are separate constraints linked via
 *   network.affects_constraints; their ε profiles differ because their lights
 *   differ — adaptation_priority treats warming as a fixed condition and
 *   authors the standing arrangement by resilience lights (lower measured
 *   cost-shifting), degrowth_reading authors it with throughput as the
 *   contested axis. Interval 0–35 maps approximately to 1990–2025, from the
 *   IPCC First Assessment Report to the present implementation-gap era. KEY
 *   AGENTS (by structural relationship): - future_generations: primary target
 *   (powerless/trapped) — absorb uncompensated warming damages; no vote, no
 *   market presence, no exit - climate_vulnerable_global_south: primary
 *   target (organized/trapped) — minimal historical emissions, maximal
 *   exposure - low_lying_island_states: existential-exposure target
 *   (organized/trapped) — territory itself is the loss -
 *   fossil_fuel_producers: primary beneficiary and agenda_setter
 *   (institutional/arbitrage) — set the extraction pace, collect the margin
 *   the externalization makes possible - fossil_capital_owners: beneficiary
 *   (powerful/arbitrage) with prospective stranded-asset exposure under this
 *   reading's transition - global_north_governments: agenda_setter
 *   (institutional/constrained) administering the arrangement and bearing
 *   this reading's allocated mitigation burden -
 *   global_north_energy_consumers: beneficiary/payer hybrid
 *   (organized/constrained) — subsidized energy now, transition costs later -
 *   carbon_intensive_workers: standing-arrangement dependents facing stranded
 *   livelihoods (organized/constrained) -
 *   energy_poor_global_south_households: double-exposed (powerless/trapped) —
 *   failed by provision and by externalization alike -
 *   youth_climate_litigants: partially enfranchised payers litigating as
 *   proxy voice for the absent - nonhuman_natural_systems: non-agent
 *   cost-bearer (kept for completeness; excluded from derivation) -
 *   ipcc_and_climate_science: analytical observer — sees the full
 *   carbon-budget structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.85).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.72).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Response Obligation — Mitigation Priority Reading (Standing Fossil Arrangement Under Contest)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '4b6e994f-8035-4334-a7b5-ed0a126251b0').
narrative_ontology:cs_kernel_codification('4b6e994f-8035-4334-a7b5-ed0a126251b0', fixed_text).
narrative_ontology:cs_authority_grounding('4b6e994f-8035-4334-a7b5-ed0a126251b0', lineage).
narrative_ontology:cs_interpretation_layer_present('4b6e994f-8035-4334-a7b5-ed0a126251b0').
narrative_ontology:cs_reading_relation('4b6e994f-8035-4334-a7b5-ed0a126251b0', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('4b6e994f-8035-4334-a7b5-ed0a126251b0', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('4b6e994f-8035-4334-a7b5-ed0a126251b0', foundational, intergenerational_justice_requires_minimal_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_minimal_warming, holdable).
narrative_ontology:cs_axiom_grounding('4b6e994f-8035-4334-a7b5-ed0a126251b0', intergenerational_justice_requires_minimal_warming, deontological).
narrative_ontology:cs_axiom('4b6e994f-8035-4334-a7b5-ed0a126251b0', foundational, rapid_decarbonization_still_prevents_worst_outcomes).
narrative_ontology:cs_axiom_status(rapid_decarbonization_still_prevents_worst_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('4b6e994f-8035-4334-a7b5-ed0a126251b0', rapid_decarbonization_still_prevents_worst_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('4b6e994f-8035-4334-a7b5-ed0a126251b0', secondary, historical_emissions_create_asymmetric_duties).
narrative_ontology:cs_axiom_status(historical_emissions_create_asymmetric_duties, holdable).
narrative_ontology:cs_axiom_grounding('4b6e994f-8035-4334-a7b5-ed0a126251b0', historical_emissions_create_asymmetric_duties, deontological).
narrative_ontology:cs_reference_frame('4b6e994f-8035-4334-a7b5-ed0a126251b0', prevention_first_treaty_framework).
narrative_ontology:cs_drift_state('4b6e994f-8035-4334-a7b5-ed0a126251b0', contemporary_implementation_gap, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b6e994f-8035-4334-a7b5-ed0a126251b0', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, fossil_capital_owners).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_north_energy_consumers).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, climate_vulnerable_global_south).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, low_lying_island_states).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, energy_poor_global_south_households).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, nonhuman_natural_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital_owners).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_governments).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_energy_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, youth_climate_litigants).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, precautionary_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, cbdr_rc_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and sell the coal, oil, and gas whose combustion the treaty layer seeks to limit. Set the pace of the standing arrangement through exploration, permitting pressure, production levels, and lobbying, and defend it through subsidy advocacy and litigation against mitigation policy. Collect the revenue stream that continued combustion generates; reserve valuations price in the arrangement holding. Exit is arbitrage-grade: portfolios, markets, and jurisdictions can be rebalanced, and demand contraction in one region can be outlasted by shifting sales to growing markets.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_producers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold equity and debt in carbon-intensive assets whose returns depend on the arrangement persisting. Collect dividends, coupons, and asset appreciation without operating the system. Under this reading's advocated rapid transition, a large share of those assets strand — reserves unburnable, plants retired early — making this seat simultaneously a collector under the standing arrangement and a designated cost-bearer under the transition it would impose. Exit is mobile in principle: capital can rotate into renewables and other sectors, though repositioning at scale carries valuation losses.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, fossil_capital_owners, payer).

% Administer the arrangement's public half: fossil subsidies, leasing and permitting, energy regulation, and the treaty pledges layered on top. Collect fiscal flows from fossil activity and deliver cheap energy that constituents price into their living standards. This reading allocates them the disproportionate mitigation burden — historical emissions, fiscal capacity, and CBDR-RC logic — so the same seat owes the transition's largest costs. Exit is constrained: they answer to electorates living now, while the largest beneficiaries of their compliance do not yet exist.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_governments, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_governments, payer).

% Receive subsidized, reliable fossil energy priced below its climate cost, and consume at per-capita rates the climate system cannot generalize. Bear diffuse climate damages and would bear concentrated transition costs — retrofit, fuel switching, price increases — under this reading's allocation. Exit is constrained: energy is not optional, and infrastructure choices (housing, transport, heating) were made under the arrangement's prices.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_energy_consumers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_energy_consumers, payer).

% Will inherit the warming, sea-level rise, ecosystem losses, and infrastructure lock-in that the present generation's combustion produces, having received none of its benefits and had no vote in its issuance. They cannot exit the climate system, cannot renegotiate its terms, and appear in present decisions only through proxies — courts, commissioners, and the discount rates the present chooses. Their claim is the center of this reading: the arrangement transfers costs to them at a scale no other seat approaches.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Populations and states with minimal historical emissions and maximal exposure: drought belts, monsoon-fed agriculture, heat-exposed cities. Bear damages they did little to cause, with the least adaptive capacity. Organized as negotiating blocs (G77, African Group), they hold formal seats in treaty forums but are out-weighted in finance and agenda power. Exit is trapped: geography, livelihoods, and statehood are not portable.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_global_south, payer,
    organized, generational, trapped, global).

% Small island states for whom the arrangement's costs are existential: territory, freshwater, and statehood itself sit inside the flooding zone the standing trajectory implies. Organized (AOSIS) and morally central to the treaty's most ambitious goals, they command little material leverage. No exit exists at any price — the asset at risk cannot be relocated.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, low_lying_island_states, payer,
    organized, generational, trapped, regional).

% Households without reliable energy access — the founding problem the arrangement claims to answer, unresolved in their case. They are failed twice over: by provision that never reached them and by climate damages arriving from emissions they did not produce. This reading's burden allocation is designed to protect their development space; whether it does is an open incidence question. Exit is trapped: poverty forecloses the options wealthier seats take for granted.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, energy_poor_global_south_households, payer,
    powerless, immediate, trapped, regional).

% Coal miners, oil and gas workers, combustion-engine and heavy-industry labor: their livelihoods are built into the standing arrangement and end with it. They collect wages from continued operation today and face stranded skills and communities under the rapid transition this reading requires — the concrete face of the current generation bearing transition costs. Organized through unions with real political weight; exit is constrained by geography, skill specificity, and age.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_workers, beneficiary,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, carbon_intensive_workers, payer).

% Young cohorts who will live through the decades the standing trajectory degrades, suing states and corporations as proxy voice for those not yet born (Juliana, Held v. Montana, the KlimaSeniorinnen line). They bear the arrangement's costs with partial enfranchisement: they can vote and litigate but do not set the agenda. Exit is constrained — the future they litigate about is where they will live.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, youth_climate_litigants, payer,
    moderate, biographical, constrained, global).

% Non-agent entity kept for narrative completeness and excluded from beneficiary/victim derivation and directionality: ecosystems, ice sheets, and ocean chemistry bear the arrangement's largest absolute costs and hold no standing of any kind — no vote, no market presence, no proxy except what science speaks for them. Listed because a cost accounting that omits them misstates the arrangement's scale.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, nonhuman_natural_systems, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__mitigation_priority, nonhuman_natural_systems).

% The assessment machinery that quantifies the carbon budgets, warming trajectories, and damage functions the whole contest runs on. Neither collects from the arrangement nor bears its costs; its seat is analytical — it sees the full structure that each seated party sees partially, and its findings are the common evidentiary ground the readings dispute the interpretation of.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, ipcc_and_climate_science, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, fossil_fuel_producers).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing arrangement coordinates energy provision at civilizational scale: dense infrastructure networks, fuel supply chains, capital markets, and regulatory regimes deliver reliable energy to eight billion people. The treaty layer (UNFCCC/Paris) coordinates a response to a global externality no single actor can abate alone, through NDC ratchet cycles, stocktakes, and finance transfers.
% TRANSFER_FUNCTION: Moves the climate-stabilization costs of combustion off current beneficiaries — producers' margins, shareholders' returns, consumers' prices — and onto parties without market presence or vote: future generations absorb warming damages, the Global South absorbs exposure it did not cause, and nonhuman systems absorb the remainder. Explicit and implicit fossil subsidies (roughly $1.3T explicit, ~$7T including implicit support per IMF methodology) move public funds toward fossil energy each year.
% ABSENT_VOICES: Future generations are the paradigmatic absent voice — no vote, no market presence, represented only through proxy litigation, ombudsperson proposals, and discount-rate choices made by the present. Nonhuman nature has no standing at all. Climate-vulnerable Global South populations are formally seated in treaty forums but structurally out-weighed in finance and agenda power. Their absence is the mechanism this reading's justice claim identifies: unanimity among the seated is purchased by excluding the seats that would object.
% DISAPPEARANCE_RATIONALE: The parties dispute it. Mitigation-priority parties hold that arrangements depend on the obligation: without it, emission trajectories lock in roughly 3°C, and the seats with no exit — future generations, island states, ecosystems — bear irreversible losses. Adaptation-priority parties hold the world would rearrange little, because they judge resilience investment to dominate prevention at the margin; degrowth parties hold the obligation as institutionalized (efficiency, offsets, pledges) is already theater that throughput limits would replace. The disagreement is precisely the kernel contest, so the verdict is contested rather than resolvable from this seat.
% FOUNDING_PROBLEM: The fossil arrangement was built to solve industrial energy scarcity — dense, reliable, cheap energy for industrialization and modern life. The treaty layer was built to solve the free-rider problem of a global externality — coordinating abatement that no single state rationally undertakes alone.
% FOUNDING_PROBLEM_CORROBORATION: The energy-scarcity founding problem is attested from outside the beneficiary set by energy-poor Global South households and by IEA/World Bank energy-access data — populations the arrangement fails who nonetheless attest the need it answers. The externality-coordination problem is attested by IPCC assessment literature and atmospheric physics, which sit outside the fossil beneficiary set. No party outside the mitigation coalition attests that intergenerational justice requires this reading's specific burden allocation; that element is corroborated only within the reading's own normative tradition (philosophy of future generations; court reasoning in KlimaSeniorinnen and Held) — which this corroboration statement records as signal rather than defect.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, contested).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85 at interval end) because the standing arrangement's defining operation, on this reading's assessment, is the uncompensated transfer of climate-stabilization costs to seats that cannot refuse — the transfer is not a side effect but the arrangement's margin. Suppression (0.72) is authored as a raw structural property and is deliberately NOT scaled by power or scope; the engine owns that arithmetic. The 0.72 describes the enforcement machinery itself: fossil subsidy regimes, infrastructure lock-in, lobbying and litigation against mitigation policy, and the structural disenfranchisement of future generations. Theater_ratio (0.55) captures the pledge-delivery gap: net-zero pledges, offset schemes, and voluntary commitments that perform response while emission trajectories persist. Accessibility_collapse (0.45) is moderate: alternatives have not collapsed — renewables and electrification are scaling — but systemic exit from fossil energy is blocked by infrastructure and capital-stock lifetimes. Resistance (0.6) reflects a real counter-mobilization: climate litigation, youth movements, divestment, Global South finance diplomacy. The measurement series run on ONE shared time grid (points 0,5,10,15,20,25,30,35) with every tracked metric authored at every point. Suppression_requirement is tracked because the narrative specifically traces an enforcement ratchet — the fossil defense apparatus intensified as mitigation pressure grew — not a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute different types from the same structure. From future_generations and climate_vulnerable_global_south, the standing arrangement is enforced cost-shifting with no exit — the purest target position available. From fossil_fuel_producers and fossil_capital_owners, the same arrangement is a legitimate coordination structure they built and defend. Global_north_governments straddle: they administer the arrangement, collect its fiscal flows, and would bear its transition costs — their computed type should oscillate with which cost stream is salient. Youth_climate_litigants see the structure only through proxy instruments, so their seat measures the proxy's fidelity as much as the constraint itself. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (producers, capital owners, Global North consumers, carbon-intensive workers) drive those seats toward the beneficiary end; victim declarations (future generations, vulnerable Global South, island states, energy-poor households, nonhuman systems) drive those seats toward the full-target end, amplified by trapped exit — future_generations is the limit case: zero exit, zero voice, civilizational horizon. The structural delta's 'future generations as primary beneficiaries' names their position under the honored obligation; under the ε-referent (the standing arrangement) the same seat is the primary victim — one structural fact viewed from the two sides of the contest. No directionality overrides are authored: the role declarations plus exit options already differentiate every seat the derivation needs to distinguish, and the override mechanism is keyed by power atom, which cannot separate this story's heterogeneous organized class (vulnerable-South blocs, Global North consumers, workers) without mis-correcting one of them. The two agenda_setter seats fall to derived handling; their hybrid positions — producers collect while setting pace; governments administer while owing transition costs — are documented here and in stakeholder situations rather than forced through a per-atom override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Calling the standing arrangement a snare would erase the genuine coordination function — billions depend on fossil energy provision, and the reading's own justice allocation (Global North pays disproportionately) presupposes a functioning energy system to transition. Calling it a rope would erase the externalization — the arrangement's persistence depends on active enforcement precisely because its costs land on seats that cannot push back. Mandatrophy risk sits in the theater series: the treaty layer's pledge machinery (NDCs, net-zero pledges, offsets) is drifting toward performance of response rather than response — the degraded-endpoint risk for the obligation while the underlying arrangement persists. The R5 interview feeds the mismatch check: founding problem live + contested disappearance verdict flags the right question (does the obligation still bind, or has its institutional form gone inert?) without pre-empting it. The receipt-surface cell (named capturer + cheap-to-fix-relative-to-benefit) says the block is political capture, not economic impossibility — avoided damages outweigh transition costs by a wide margin in every serious accounting — which is exactly the distinction the receipt surface exists to keep visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel climate_response_obligation — the mitigation_priority reading. What would the sibling readings (adaptation_priority, degrowth_reading) change structurally, and where exactly is the disagreement located?',
    'No data resolves a committer structure; resolution is framing choice. The disagreement is located in two structural elements: (a) whether warming is a preventable variable (this reading) or a fixed condition to be endured (adaptation_priority), and (b) whether the response lever is emissions minimization within growing economies (this reading) or material throughput reduction (degrowth_reading).',
    'Under adaptation_priority, future_generations leave the primary-beneficiary/victim center — warming is treated as fixed, so the victim set shifts to currently-exposed populations and adaptation finance becomes the transfer. Under degrowth_reading, current overconsumers join the target set and technology-reliant mitigation loses its vindicated status. This story''s ε (0.85 for the standing fossil arrangement) is authored only under this reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the climate-response kernel; siblings would restructure the beneficiary/victim sets.').

omega_variable(
    warming_preventability_frontier,
    'Is the reading''s foundational empirical premise still live — can rapid decarbonization still minimize warming to near-1.5°C outcomes, or has the carbon budget been exhausted enough that the obligation''s object shifts to overshoot and carbon-dioxide removal?',
    'Carbon-budget accounting (Global Carbon Budget, IPCC assessment cycle), observed warming trajectory, and CDR scalability assessments (engineered removal cost and volume versus residual emissions).',
    'If 1.5°C is foreclosed, the reading must re-found on overshoot-and-return, which enlarges the victim set (overshoot decades bear unavoidable damages) and makes the obligation partially compensatory rather than purely preventive — changing ε''s composition and the transition''s urgency profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_preventability_frontier, empirical, 'Whether the preventability premise of the mitigation reading remains empirically live at the 1.5°C level.').

omega_variable(
    transition_burden_incidence,
    'Does the transition burden actually land where this reading allocates it — on the Global North and fossil capital — or does it leak onto energy-poor households and Global South development through regressive carbon pricing, supply-chain pass-through, and under-delivered climate finance?',
    'Distributional incidence analysis of carbon pricing and transition policy; climate-finance delivery audits against the NCQG; energy-poverty tracking under decarbonization scenarios.',
    'If burdens leak to the poor, the advocated transition itself becomes cost-shifting toward the reading''s own justice constituency — the constraint family risks a tangled_rope-to-snare drift inside the remedy, and the CBDR-RC allocation fails its own test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_burden_incidence, empirical, 'Whether mitigation burdens follow the reading''s justice allocation or regress onto the energy-poor.').

omega_variable(
    stranded_asset_response_mode,
    'Fossil capital is beneficiary of the standing arrangement and prospective victim via stranded assets under this reading''s transition. Does that dual position generate obstruction (political delay to defer stranding) or accommodation (managed wind-down seeking compensation)?',
    'Incumbent investment and lobbying patterns under tightening policy, divestment and litigation behavior, and asset revaluation trajectories as policy credibility shifts.',
    'Obstruction confirms the enforcement-ratchet reading of the suppression series (suppression rises as stranding nears); accommodation would flatten the suppression trajectory and open a negotiated transition path this story does not model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_response_mode, empirical, 'Whether stranded-asset exposure drives fossil-capital obstruction or accommodation.').

omega_variable(
    proxy_representation_adequacy,
    'Future generations enter only through proxies — courts, ombudsperson proposals, discount-rate ethics. Is proxy representation a legitimate stand-in for the absent, or a structurally inadequate substitute that guarantees systematic under-weighting of the very interests this reading centers?',
    'Comparative analysis of proxy mechanisms: litigation outcomes versus scientific damage assessments, discount-rate sensitivity of policy cost-benefit analysis, and performance of institutional experiments (future-generations commissioners, ombudsperson offices).',
    'If proxies systematically under-weight, the standing arrangement''s effective cost-shifting onto the future is higher than any measured ε — the metric floor is set by the proxy''s fidelity; if proxies are adequate, current measurement is unbiased.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_adequacy, conceptual, 'Whether proxy representation of future generations is adequate or structurally under-weighs them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_priority_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t0, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t5, climate_response_obligation__mitigation_priority, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t5, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t10, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t15, climate_response_obligation__mitigation_priority, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t15, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t20, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t25, climate_response_obligation__mitigation_priority, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t25, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.53).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t30, observed).
narrative_ontology:measurement(climate_mitigation_priority_tr_t35, climate_response_obligation__mitigation_priority, theater_ratio, 35, 0.55).
narrative_ontology:measurement_basis(climate_mitigation_priority_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_priority_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t0, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t5, climate_response_obligation__mitigation_priority, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t5, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t10, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t15, climate_response_obligation__mitigation_priority, base_extractiveness, 15, 0.73).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t15, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t20, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t25, climate_response_obligation__mitigation_priority, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t25, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.83).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t30, observed).
narrative_ontology:measurement(climate_mitigation_priority_be_t35, climate_response_obligation__mitigation_priority, base_extractiveness, 35, 0.85).
narrative_ontology:measurement_basis(climate_mitigation_priority_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_priority_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t0, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t5, climate_response_obligation__mitigation_priority, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t5, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t10, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t15, climate_response_obligation__mitigation_priority, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t15, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t20, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t25, climate_response_obligation__mitigation_priority, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t25, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t30, observed).
narrative_ontology:measurement(climate_mitigation_priority_su_t35, climate_response_obligation__mitigation_priority, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(climate_mitigation_priority_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response obligation' covers three structurally distinct readings of one kernel (UNFCCC/Paris text). This story instantiates the mitigation_priority reading only; its ε is authored for the standing fossil arrangement as this reading assesses it (high cost-shifting onto the voiceless). The adaptation_priority sibling authors ε for the same standing arrangement by resilience lights (warming as fixed condition; lower measured cost-shifting, different victim set). The degrowth_reading sibling authors ε with material throughput as the contested axis (current overconsumption as target class). The readings are linked, not merged: each has its own ε, its own beneficiary/victim structure, and its own classification, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
