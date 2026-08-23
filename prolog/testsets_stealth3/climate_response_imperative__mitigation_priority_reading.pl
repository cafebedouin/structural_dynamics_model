% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Climate Response Regime (Innovation-and-Markets Reading)
 *   domain: political economy / climate governance / intergenerational justice
 *
 * SUMMARY:
 *   The standing arrangement under contest is three decades of climate
 *   governance institutionalized around one answer to the climate-response
 *   kernel: emissions reduction pursued chiefly through technological
 *   innovation and market mechanisms (negotiated targets, carbon pricing,
 *   offset markets, subsidized clean-tech deployment), with adaptation
 *   treated as a residual funding line and reliance on future carbon removal
 *   embedded in mainstream net-zero pathways. The arrangement achieves
 *   genuine coordination at scale (common accounting, comparable targets,
 *   trillions steered toward decarbonization) while its incidence is sharply
 *   asymmetric: finance flows and policy attention concentrate in Global
 *   North innovation sectors and intermediaries, adaptation deficits
 *   accumulate in exposed Southern regions, and deferred costs compound
 *   silently for future generations. This file instantiates the
 *   mitigation_priority_reading of the climate_response_imperative kernel;
 *   the sibling readings (adaptation_priority_reading, degrowth_reading)
 *   instantiate different constraints with different victim sets and are
 *   linked via network edges. Claim and metrics are independent authored
 *   facts: claimed_type is tangled_rope from structural judgment (real
 *   coordination function plus asymmetric incidence plus active enforcement);
 *   the metrics are authored from the descriptive record without tuning
 *   toward any predicted engine output.
 *
 * KEY AGENTS:
 *   - - g20_emitter_governments: agenda-setting bloc (institutional/constrained) — writes the mitigation-first rules and controls fund boards
 *   - - global_north_cleantech_industries: primary beneficiary (powerful/mobile) — collects mandated, subsidized demand
 *   - - carbon_market_intermediaries: fee-collecting beneficiary (organized/arbitrage) — revenue scales with volume, not atmospheric outcome
 *   - - green_finance_asset_managers: beneficiary (institutional/arbitrage) — gathers assets under the decarbonization theme
 *   - - fossil_incumbents_with_cdr_stakes: dual-positioned beneficiary (powerful/mobile) — purchases delay via removal promises while paying compliance friction
 *   - - future_generations: silent payer (powerless/trapped/universal scope) — inherits deferred mitigation and deferred adaptation costs
 *   - - vulnerable_global_south_populations: present-day payer (powerless/trapped) — absorbs rising impacts on loan-financed mitigation terms
 *   - - frontline_coastal_communities: irreversibly exposed payer (powerless/trapped) — loses place before any removal benefit arrives
 *   - - least_developed_countries_bloc: organized payer with marginal voice (organized/constrained) — wins language, waits on disbursement
 *   - - climate_justice_movements: excluded advocate (organized/constrained) — observer status, no decision rights
 *   - - ipcc_assessment_body: analytical observer (institutional/analytical) — documents the gap between pledges and pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.66).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Climate Response Regime (Innovation-and-Markets Reading)").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "political economy / climate governance / intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '0a4f6886-8ad6-485b-ba22-2339f6272624').
narrative_ontology:cs_kernel_codification('0a4f6886-8ad6-485b-ba22-2339f6272624', formalized).
narrative_ontology:cs_authority_grounding('0a4f6886-8ad6-485b-ba22-2339f6272624', expertise).
narrative_ontology:cs_interpretation_layer_present('0a4f6886-8ad6-485b-ba22-2339f6272624').
narrative_ontology:cs_reading_relation('0a4f6886-8ad6-485b-ba22-2339f6272624', climate_response_imperative__adaptation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('0a4f6886-8ad6-485b-ba22-2339f6272624', climate_response_imperative__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('0a4f6886-8ad6-485b-ba22-2339f6272624', foundational, market_price_signals_sufficient_for_decarbonization).
narrative_ontology:cs_axiom_status(market_price_signals_sufficient_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('0a4f6886-8ad6-485b-ba22-2339f6272624', market_price_signals_sufficient_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('0a4f6886-8ad6-485b-ba22-2339f6272624', foundational, adaptation_is_residual_not_primary).
narrative_ontology:cs_axiom_status(adaptation_is_residual_not_primary, holdable).
narrative_ontology:cs_axiom_grounding('0a4f6886-8ad6-485b-ba22-2339f6272624', adaptation_is_residual_not_primary, instrumental).
narrative_ontology:cs_axiom('0a4f6886-8ad6-485b-ba22-2339f6272624', secondary, technology_curves_will_close_the_remaining_gap).
narrative_ontology:cs_axiom_status(technology_curves_will_close_the_remaining_gap, holdable).
narrative_ontology:cs_axiom_grounding('0a4f6886-8ad6-485b-ba22-2339f6272624', technology_curves_will_close_the_remaining_gap, empirically_contingent).
narrative_ontology:cs_reference_frame('0a4f6886-8ad6-485b-ba22-2339f6272624', innovation_market_sufficiency_baseline).
narrative_ontology:cs_drift_state('0a4f6886-8ad6-485b-ba22-2339f6272624', post_global_stocktake_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a4f6886-8ad6-485b-ba22-2339f6272624', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_cleantech_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, green_finance_asset_managers).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, fossil_incumbents_with_cdr_stakes).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, vulnerable_global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, frontline_coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, fossil_incumbents_with_cdr_stakes).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, least_developed_countries_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author the national pledges and finance commitments that define the regime's shape. Their stated targets center on emissions cuts and technology deployment; adaptation appears as a secondary funding line. Domestic electoral cycles reward visible industrial policy such as factories, jobs, and export credits over payments for damages abroad. They control the boards of the major multilateral climate funds and can block consensus at the annual conferences.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, g20_emitter_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Manufacture solar panels, batteries, heat pumps, and electric vehicles whose demand is guaranteed by mandates, tax credits, and procurement rules written into the pledges. Public subsidy de-risks their capital expenditure, and the innovation-centered framing positions them as the engine of the solution. Their capital can move between jurisdictions competing to host production.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_cleantech_industries, beneficiary,
    powerful, biographical, mobile, global).

% Operate registries, verification services, brokerages, and exchanges that take a fee on every credit issued or traded. Revenue scales with transaction volume whether or not the underlying reductions are additional. When methodologies are tightened they redesign their products rather than shrink.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, immediate, arbitrage, global).

% Package decarbonization-themed funds and stewardship services sold on the promise that portfolio alignment drives real-world emissions cuts. Management fees accrue on assets gathered under the theme. They vote in standard-setter consultations and benefit when disclosure rules make their product category mandatory.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, green_finance_asset_managers, beneficiary,
    institutional, biographical, arbitrage, global).

% Produce oil, gas, and coal while investing selectively in capture-and-storage pilots and offset portfolios. The prospect of future large-scale removal technologies extends their social license and financing access for continued production today, and they also collect subsidies for the pilot projects themselves. They bear some compliance friction under carbon pricing but can relocate production to friendlier jurisdictions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, fossil_incumbents_with_cdr_stakes, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, fossil_incumbents_with_cdr_stakes, payer).

% Will inherit whatever warming is locked in by today's deferred ambition plus the unpaid bill for protections not built now. They hold no seat in any negotiating room, cannot trade away their exposure, and cannot exit the atmosphere they will occupy. Their interests enter the arrangement only through discount rates chosen by others.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Live in regions where drought, flood, and heat mortality are already rising. Finance reaching them skews toward loans for emission-cutting projects rather than grants for sea walls, early-warning systems, or resilient agriculture. Migration is possible but destroys livelihood and community; staying means absorbing intensifying damage.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, vulnerable_global_south_populations, payer,
    powerless, generational, trapped, global).

% Occupy deltas, small islands, and low-lying shorelines where sea-level rise and storm surge are converting land permanently. Adaptation funding arrives late, in fractions of assessed need, often as debt. Relocation means losing place-based livelihoods, burial grounds, and social fabric; the promised benefits of future removal technologies arrive, if ever, after the water does.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, frontline_coastal_communities, payer,
    powerless, biographical, trapped, regional).

% Negotiate as a coordinated group of roughly forty-six states with minimal individual leverage. They have won rhetorical concessions such as a loss-and-damage fund and adaptation-goal language that translate slowly into disbursements. Their technical delegations are small next to those of major emitters, and dependence on concessional finance limits how hard they can push.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, least_developed_countries_bloc, payer,
    organized, generational, constrained, continental).

% Organize protests, litigation, and divestment campaigns arguing for reparative finance, consumption reduction, and adaptation-first spending. They hold observer accreditation at the conferences but no decision rights; their proposals enter the process only if a party adopts them. Their leverage is disruption and moral framing rather than votes.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Produces the periodic assessments that all parties cite. Its scenarios document the widening gap between pledged pathways and measured emissions, the shortfall in adaptation finance, and the dependence of low-overshoot pathways on removal scales never yet demonstrated. It holds no enforcement power; its influence runs through legitimacy alone.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ipcc_assessment_body, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_cleantech_industries).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the sovereign free-rider problem in emissions reduction: common carbon accounting makes national efforts comparable, negotiated targets create reciprocal expectations, carbon prices and subsidy regimes steer private capital toward low-carbon technology at scale, and pledge-review cycles generate reputational pressure without world government.
% TRANSFER_FUNCTION: Moves capitalized finance, subsidy streams, and policy attention toward Global North clean-technology industries, carbon-market intermediaries, and themed asset management; moves the costs of un-prevented damage onto exposed Southern regions now and onto future generations later, via deferred adaptation spending and reliance on removal technologies not yet demonstrated at scale.
% ABSENT_VOICES: Future generations are absent from every seat and every forum. Frontline coastal communities appear only through accredited NGOs. Advocates of adaptation-first and consumption-reduction framings hold observer status without decision rights, and Southern publics are represented by governing elites whose incentives diverge from theirs.
% DISAPPEARANCE_RATIONALE: Carbon markets, net-zero-aligned finance, and the innovation-subsidy complex would lose their coordinating frame within months and capital would repricing climate exposure chaotically; adaptation finance would not automatically rise to fill the gap. The sibling readings dispute whether the rearrangement would help or harm, but the rearrangement itself is certain: a large share of the world's decarbonization investment, accounting infrastructure, and diplomatic effort is organized around this arrangement.
% FOUNDING_PROBLEM: After Kyoto, how to induce sovereign states to cut emissions without coercive enforcement. The design answer was to make mitigation investable: targets plus markets plus technology cost curves, with adaptation treated as a local residual responsibility.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and the independent environmental-economics literature corroborate that the free-rider problem remains real and unsolved, with an emissions gap persisting across three decades of pledges. No corroborating source outside the benefiting parties attests that market-innovation mechanisms specifically remain sufficient: successive UNEP Emissions Gap reports and the Stern-review lineage document the shortfall, while the sufficiency claim is maintained principally by the arrangement's beneficiaries and administering parties.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: substantial but riding on real coordination. Tracked climate finance has run roughly nine-to-one mitigation-to-adaptation across the interval; offset markets have repeatedly certified non-additional reductions; and net-zero pathways lean on removal scales never demonstrated, which converts today's under-delivery into tomorrow's locked warming. It is not maximal because physical decarbonization is genuinely occurring — solar and battery cost curves are real achievements of this architecture. Suppression 0.62: the arrangement's persistence depends on actively maintaining its frame — MRV and accounting rules, Article 6 supervision, and increasingly border-adjustment mechanics enforce participation, while discursive closure ('there is no alternative' policy realism) marginalizes sibling readings. Roughly sixty percent of the suppression is structural (finance architecture, consensus rules, gatekept fora) and forty percent internalized (professional identity fusion: market instruments define what counts as a serious proposal). Theater_ratio 0.53: pledge announcements, offset portfolios, and themed funds are a growing performative layer over real deployment; the crossover past 0.5 around 2021 marks the net-zero pledge wave. Accessibility_collapse 0.42: the sibling readings remain visible and live — alternatives have not collapsed — but exiting the dominant frame carries career and institutional cost. Resistance 0.60: sustained movement pressure, litigation, and the LDC bloc's loss-and-damage wins demonstrate real coalition power among otherwise weakly positioned states; without that coalition, resistance would sit nearer 0.35. The measurement series run on one shared eight-point grid (all three metrics at every point, 1997-2025). The suppression_requirement series is authored deliberately: enforcement capacity visibly hardened over the interval (CDM verification, Paris transparency framework, Article 6 finalization at Glasgow, CBAM phasing in), which is an enforcement-ratchet trajectory, not a static picture. Measurement points are anchored at post-conference milestones to avoid sampling one phase of the COP negotiation cycle; the underlying trend is monotonic, and the visible pledge-disappointment oscillation around each COP is a calendar artifact of the process, not an intermittent-reinforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute very differently. From future_generations, vulnerable_global_south_populations, and frontline_coastal_communities, the arrangement operates as enforced deferral: coordination happens on someone else's timetable, paid for in their exposure. From the cleantech, intermediary, and asset-manager seats, the same structure is an opportunity lattice they did not build but profit within. The agenda_setter seat experiences managed consensus — difficult, legitimate, slow. A sharp same-power divergence sits between fossil_incumbents_with_cdr_stakes and global_north_cleantech_industries: both powerful and mobile, but incumbents use removal promises to purchase optionality while absorbing compliance friction, whereas cleantech collects unconditional subsidized demand — structurally similar agents, materially different relationships to the constraint. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: cleantech industries, intermediaries, and asset managers hold mobile or arbitrage exits and collect directly from the arrangement's operation, placing them near the beneficiary pole. Fossil incumbents derive low-but-not-zero d — their secondary payer position (compliance costs under carbon pricing) pulls them slightly off the pure-beneficiary end, which the derivation captures without an override. Victim declarations map to high directionality: future_generations combine powerless power, trapped exit, and universal spatial scope, so the engine both places them at the full-target end and amplifies their effective extraction through scope; vulnerable populations and coastal communities are trapped with regional-to-global exposure. The LDC bloc derives high d moderated slightly by its organized bargaining wins. The IPCC observer is analytically neutral. No directionality_overrides are authored: the derivation chain from roles, power, and exit options reproduces the true relationships, and a per-power-atom override could not separate the two powerful beneficiary seats from each other without misstating one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereign free-riding on emissions cuts — remains live per every independent assessment, so this arrangement is not mandatrophy-resolved and should not be flagged as such. The classification prevents two opposite errors. Reading the arrangement as pure extraction (a snare mislabel) would erase the real coordination achievement: common accounting, reciprocal target expectations, and cost-curve-driven deployment have delivered emission reductions no alternative framework has yet demonstrated at scale. Reading it as mere coordination cost (a rope mislabel) would erase the asymmetric incidence: the adaptation deficit, the loan-not-grant finance skew, and the intergenerational transfer embedded in CDR reliance are not overhead of coordination but its distributional product. The theater_ratio trajectory (0.22 to 0.53) tracks the growth of the pledge layer and is the leading indicator of atrophy risk: if pledge performance continues substituting for delivery, the arrangement drifts toward the degraded pole while its beneficiaries continue collecting — the monitoring question this story leaves open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_displacement_risk,
    'Which reading of the climate_response_imperative kernel actually governs resource allocation, and could the mitigation_priority_reading be displaced through finance-rule changes rather than treaty-text revision?',
    'Track the mitigation-to-adaptation split in committed and disbursed climate finance across successive conference finance decisions; displacement is signaled when the adaptation share approaches parity without any formal revision of the kernel''s primacy language.',
    'If the adaptation_priority_reading displaces this one, the victim set shifts — present-day Northern consumers begin bearing direct resilience costs while the intergenerational transfer shrinks — and this constraint''s epsilon would be re-authored from a different seat over the same referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_displacement_risk, conceptual, 'Whether this reading remains the operative one or is displaced by a sibling through finance-channel change.').

omega_variable(
    cdr_scalability_gap,
    'Can engineered and nature-based carbon removal scale to the multi-gigatonne levels embedded in the net-zero pathways this reading relies on?',
    'Deployment and cost-curve data through 2035: installed removal capacity, pipeline conversion rates, and realized prices versus pathway assumptions.',
    'If removal undershoots, deferred mitigation becomes permanently locked warming — the intergenerational transfer hardens from contingent to irreversible, and the arrangement''s computed classification shifts toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability_gap, empirical, 'Reliability of the removal backstop the reading''s pathways assume.').

omega_variable(
    intergenerational_discount_rate_choice,
    'What discount rate legitimately governs the weight given to future generations'' losses in this reading''s cost-benefit framing?',
    'Normative-economic deliberation and the revealed rates in official social-cost-of-carbon revisions; no purely empirical resolution exists.',
    'At near-zero discounting the deferred-cost transfer dominates the ledger and effective extraction rises sharply; at commercial rates it nearly vanishes — the classification of the same arrangement swings on this parameter, which is a value choice the reading has made implicitly rather than defended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_choice, preference, 'Value parameter controlling how heavily the victim set weighs in the reading''s own accounting.').

omega_variable(
    climate_finance_additionality,
    'Is finance counted as climate mitigation genuinely additional, or reclassified development assistance?',
    'Creditor-reporting audits comparing pre-pledge and post-pledge aid allocations to the same recipients and sectors, isolating relabeled flows.',
    'If flows are largely rebranded, measured extraction understates reality: beneficiary seats collect twice (subsidy plus diverted aid) while victim seats receive less than the recorded figures suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_finance_additionality, empirical, 'Additionality of the finance flows the reading counts as its response.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the marginalization of alternative responses structural (finance architecture, consensus rules, gatekept fora) or internalized (policy-community conviction that market instruments are the only serious tools)?',
    'Counterfactual probe: when access rules opened, as in the loss-and-damage fund design process, did alternative-framed proposals gain traction, or were they re-translated into market-compatible instruments upon entry?',
    'If internalized, opening the architecture will not release the suppressed alternatives — suppression would persist after structural reform, and the measured suppression understates the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Location of the mechanism keeping sibling readings marginal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_priority_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2001, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2009, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2009, 0.38).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2013, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2013, 0.41).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2017, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2021, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2021, 0.5).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2025, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2025, 0.53).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_priority_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.46).
narrative_ontology:measurement(climate_mitigation_priority_be_t2001, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(climate_mitigation_priority_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(climate_mitigation_priority_be_t2009, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2009, 0.56).
narrative_ontology:measurement(climate_mitigation_priority_be_t2013, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2013, 0.59).
narrative_ontology:measurement(climate_mitigation_priority_be_t2017, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(climate_mitigation_priority_be_t2021, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement(climate_mitigation_priority_be_t2025, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2025, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_priority_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.34).
narrative_ontology:measurement(climate_mitigation_priority_su_t2001, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2001, 0.39).
narrative_ontology:measurement(climate_mitigation_priority_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.43).
narrative_ontology:measurement(climate_mitigation_priority_su_t2009, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2009, 0.47).
narrative_ontology:measurement(climate_mitigation_priority_su_t2013, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2013, 0.51).
narrative_ontology:measurement(climate_mitigation_priority_su_t2017, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(climate_mitigation_priority_su_t2021, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2021, 0.59).
narrative_ontology:measurement(climate_mitigation_priority_su_t2025, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: the colloquial label 'the climate response' conflates three structurally distinct constraints sharing one kernel. This file authors the mitigation_priority_reading (vehicle: markets plus innovation; adaptation residual; victims include future generations via deferred costs and removal reliance). adaptation_priority_reading authors the resilience-first arrangement with a different beneficiary/victim geometry (present-day exposed regions become the coordination focus; Northern consumers bear direct costs). degrowth_reading authors the structural-transformation arrangement (Northern consumption itself becomes the object of constraint). Upstream/downstream structure: this reading currently dominates finance allocation and shapes both siblings' operating environments through budget lines — influence runs through money, not logic. Logical relations among readings are carried separately in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
