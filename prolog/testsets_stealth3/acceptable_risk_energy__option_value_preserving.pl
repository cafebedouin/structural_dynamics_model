% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Acceptable-Risk Doctrine for Energy Pathways
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel; the kernel is
 *   the acceptable-risk question in energy-system design, and this reading
 *   holds that acceptable risk is whatever preserves decision flexibility
 *   across multiple pathways under deep uncertainty. The standing arrangement
 *   under contest, which is the sole epsilon referent here, is the
 *   multi-pathway hedging apparatus itself: reliability mandates, capacity
 *   market payments, and subsidy programs that keep firm nuclear and
 *   dispatchable fossil viable, enforced by blocking both extremes (nuclear
 *   phase-out campaigns and rapid fossil retirement mandates). The sibling
 *   readings are separate constraint files with their own epsilon values,
 *   beneficiary structures, and classifications; their structural deltas are
 *   recorded in the omega variables and kernel_context, not folded into this
 *   classification or averaged into its metrics. Claim and metrics are
 *   authored independently: the claimed type is tangled_rope on structural
 *   grounds (a genuine hedging function, identifiable payers, and active
 *   enforcement), while the metrics describe the arrangement as it has
 *   actually operated over the interval.
 *
 * KEY AGENTS:
 *   - energy_regulators_and_planners: agenda setter (institutional/constrained) — administers the portfolio doctrine and its enforcement machinery
 *   - nuclear_plant_operators: primary beneficiary (institutional/constrained) — collects subsidy and capacity transfers as explicit line items
 *   - fossil_fuel_producers: secondary beneficiary (powerful/arbitrage) — retains domestic market access; exit via export redirection
 *   - regional_transmission_operators: institutional administrator-beneficiary (institutional/trapped) — runs the capacity markets that pay firm units
 *   - electricity_ratepayers: primary payer (powerless/trapped) — pays the option premium through retail bills
 *   - fossil_pollution_exposed_communities and climate_exposed_populations: cost-bearers (powerless/trapped) — bear continued-operation harms at local and global scope
 *   - renewable_energy_developers: dual-positioned payer-beneficiary (organized/mobile) — buildout proceeds but the hedge raises their cost of capital
 *   - premature_closure_exposed_regions: payer and cautionary case (organized/trapped) — their documented losses anchor the doctrine's justification
 *   - future_generations: excluded absent voice (powerless/trapped) — inherit whichever error the hedge fails to prevent
 *   - energy_system_analysts: analytical observer (analytical/analytical) — computes the expected-value alternative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.66).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.66).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Acceptable-Risk Doctrine for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, 'f7cb71aa-01e3-4d9d-947a-92092c624291').
narrative_ontology:cs_kernel_codification('f7cb71aa-01e3-4d9d-947a-92092c624291', distributed).
narrative_ontology:cs_authority_grounding('f7cb71aa-01e3-4d9d-947a-92092c624291', distributed).
narrative_ontology:cs_reading_relation('f7cb71aa-01e3-4d9d-947a-92092c624291', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('f7cb71aa-01e3-4d9d-947a-92092c624291', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('f7cb71aa-01e3-4d9d-947a-92092c624291', foundational, option_value_dominates_under_deep_uncertainty).
narrative_ontology:cs_axiom_status(option_value_dominates_under_deep_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('f7cb71aa-01e3-4d9d-947a-92092c624291', option_value_dominates_under_deep_uncertainty, instrumental).
narrative_ontology:cs_axiom('f7cb71aa-01e3-4d9d-947a-92092c624291', foundational, premature_closure_costs_are_systematically_undercounted).
narrative_ontology:cs_axiom_status(premature_closure_costs_are_systematically_undercounted, holdable).
narrative_ontology:cs_axiom_grounding('f7cb71aa-01e3-4d9d-947a-92092c624291', premature_closure_costs_are_systematically_undercounted, empirically_contingent).
narrative_ontology:cs_reference_frame('f7cb71aa-01e3-4d9d-947a-92092c624291', diversified_multi_pathway_baseline).
narrative_ontology:cs_drift_state('f7cb71aa-01e3-4d9d-947a-92092c624291', contemporary_storage_cost_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f7cb71aa-01e3-4d9d-947a-92092c624291', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_plant_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_host_communities).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, regional_transmission_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_regulators_and_planners).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, fossil_pollution_exposed_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_energy_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, premature_closure_exposed_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, electricity_ratepayers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, renewable_energy_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, nuclear_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, future_generations).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, real_options_decision_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, robust_decisionmaking_under_deep_uncertainty).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, energy_security_diversification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the portfolio: set reliability mandates, administer subsidy programs that keep merchant nuclear plants solvent, and design capacity market rules that compensate dispatchable fossil units for standing by. Their proceedings weigh phase-out petitions against fuel-diversity and reliability arguments, and their authority is constituted by managing exactly this tradeoff. They cannot exit the mandate without dissolving their own function; agency continuity and career standing ride on the framework they administer.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_regulators_and_planners, agenda_setter,
    institutional, generational, constrained, national).

% Operate wholesale markets and design capacity products that pay firm nuclear and fossil units for availability. Value supply diversity in reliability modeling; collect administrative fees and institutional standing from running the multi-pathway market design. They cannot exit: they are constituted by the market framework they administer.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, regional_transmission_operators, beneficiary,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, regional_transmission_operators, agenda_setter).

% Operate merchant reactors whose economics depend on zero-emission-credit payments, production tax credits, and capacity payments layered on top of energy revenue. They collect these transfers as explicit line items. Sunk capital and decommissioning obligations make exit identical to shutdown, so their commercial strategy is keeping the pathway open.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_plant_operators, beneficiary,
    institutional, biographical, constrained, national).

% Host plants that anchor local tax bases and skilled employment. They benefit from continued operation and organize to lobby for subsidy continuation; they also carry accident risk and the full local weight of any eventual shutdown. The plant is the local economy, so moving is not a live option for most residents.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_host_communities, beneficiary,
    organized, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, nuclear_host_communities, payer).

% Produce coal and gas whose domestic generation demand persists because the pathway stays open. They fund fuel-security and reliability advocacy. If domestic demand erodes they can redirect output to export markets, so exit is cheap for them relative to the communities attached to the plants that burn their fuel.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers, beneficiary,
    powerful, biographical, arbitrage, global).

% Pay reliability riders, capacity charges, and subsidy surcharges embedded in retail bills. They receive firm supply and some price stability in return. They cannot choose their utility's portfolio or leave the grid; the premium arrives whether or not they would have bought the hedge.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, electricity_ratepayers, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, electricity_ratepayers, beneficiary).

% Live downwind and downstream of plants whose continued dispatch is justified by portfolio diversity. They bear particulate, ozone, and water burdens that a committed retirement schedule would have retired sooner. Moving away is possible at the cost of homes, jobs, and social networks.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_pollution_exposed_communities, payer,
    powerless, biographical, constrained, local).

% Bear the cumulative emissions of delayed decarbonization: stronger storms, heat extremes, sea-level rise, crop stress. They hold no seat in capacity proceedings in any jurisdiction and have no exit from the climate system.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_exposed_populations, payer,
    powerless, generational, trapped, global).

% Benefit from buildout mandates and tax credits, but bear the hedging's costs: interconnection queues crowded by legacy capacity, policy whipsaw between pathway commitments, and a higher cost of capital because no jurisdiction will promise the destination. Their capital can move to jurisdictions that do commit.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_energy_developers, payer,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, renewable_energy_developers, beneficiary).

% Regions that lost firm-capacity plants on schedules set by economics or accident rather than planned transition, with shutdowns followed by coal backfill, import dependence, and workforce dispersal. Their documented losses are the cautionary case the doctrine cites for keeping pathways open; under the current arrangement they remain exposed wherever subsidies lapse before replacement capacity exists.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, premature_closure_exposed_regions, payer,
    organized, biographical, trapped, regional).

% Inherit whichever error the hedge fails to prevent: the emissions of continued fossil operation, the costs of plants subsidized past their economics, or the scarcity of a pathway closed too early. They are present in no proceeding; every decision in this arrangement is taken on their behalf.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, future_generations, excluded).

% Compute mortality-per-TWh rankings, levelized costs, and option-premium valuations; publish both the expected-value case that the hedge is overpriced and the tail-risk case that it underweights catastrophe. They hold testimony seats, not decision seats.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_system_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, nuclear_plant_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of irreversible commitment under deep uncertainty: no single actor can preserve fuel diversity, firm capacity, and a skilled nuclear workforce alone, and once a pathway's supply chains and workforces dissolve they cannot be rebuilt on demand. The arrangement coordinates maintenance of multiple generation pathways so decision-makers can re-weight the portfolio as evidence about costs, climate damages, and geopolitics arrives.
% TRANSFER_FUNCTION: Moves money from electricity ratepayers (subsidy surcharges, capacity charges) to operators of firm nuclear and fossil capacity, and moves present-day health and climate costs from continued fossil operation onto pollution-exposed communities and climate-exposed populations; the counterpart flow is preserved optionality, meaning firm capacity, fuel diversity, and workforce continuity held for future decision-makers.
% ABSENT_VOICES: Future generations hold no seat in any proceeding. Fossil pollution-exposed communities appear only as limited-standing intervenors. Expected-value analysts enter as evidence rather than as a decision rule; committed-decarbonization advocates are present but outvoted by the reliability frame. The apparent consensus behind prudence arises partly because the seats that would object to paying the premium are diffuse and unrepresented.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, jurisdictions would commit: subsidy programs would lapse and merchant reactors would retire on economic schedules; capacity markets would stop paying fossil units to stand by; phase-out mandates would face no reliability counterweight. The system would reorganize around committed pathways, some expected-value and some tail-risk, with stranded assets, workforce dislocation, and a scramble to rebuild firm capacity wherever the committed pathway undersupplies it.
% FOUNDING_PROBLEM: Energy systems built on confident mid-century forecasting suffered expensive lock-in, including over-ordered reactors, oil-shock exposure, and stranded coal; the later corrective, premature exit, produced its own documented harms: post-shutdown coal backfill, import dependence, and workforce dispersal. The arrangement was built to prevent both error types by refusing early commitment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: decision-theoretic and real-options literature independently values flexibility under deep uncertainty; post-Fukushima energy reviews in Germany and Japan documented the costs of rapid exit; international scenario practice keeps multiple pathways partly on these grounds. Corroboration is broad for the problem and contested for the remedy: expected-value analysts outside the benefiting parties argue on the public record that the option premium now exceeds the hedge's value.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66: the arrangement transfers real money (subsidy surcharges, capacity payments) and externalizes real harms (continued fossil dispatch), but the flows are bounded and partly reciprocal, since ratepayers receive firm supply and the hedge insures against documented lock-in and premature-exit harms. Suppression is 0.52: enforcement is regulatory rather than coercive, meaning phase-out petitions denied, capacity rules rewritten, subsidy programs defended, and it suppresses both extremes moderately rather than either side totally. Theater is 0.30: reliability and fuel-security arguments are partly genuine, since grids do need firm capacity during transition, and partly incumbent cover; the theater series peaks at the 2017 resilience docket, which was widely assessed outside the benefiting parties as a coal bailout, and partially recedes as support becomes statutory. Accessibility collapse is 0.40: alternatives remain partly accessible, with one major jurisdiction exiting nuclear outright, several grids pursuing high-renewables commitments, and expected-value frameworks informing some regulators, but within a given jurisdiction the subsidy and capacity machinery, once embedded in statute and tariff, is expensive to unwind. Resistance is 0.58: the arrangement is attacked from at least three directions at once, which keeps enforcement active. The suppression series is authored because the story genuinely tracks enforcement-capacity change: it built up through 2017 and then partially institutionalized, declining slightly as support became routine statute rather than contested docket. All series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the regulator and operator seats the arrangement is prudent portfolio management they personally maintain and staff. From the ratepayer and pollution-exposed seats it is a premium billed to people who never chose it. From the analyst seat it is a mispriced option whose premium can now be compared against storage cost curves. From the premature-closure regions it is vindication of their loss. Same structure, different types per seat: the engine derives this divergence from the declared roles, exits, and scopes, and the divergence is the measurement, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the operator, producer, host-community, transmission-operator, and regulator seats at the low-d end: the arrangement subsidizes them, and constrained or trapped exits deepen their structural stake in its persistence. Victim declarations place the payer seats at the high-d end: trapped ratepayers and trapped climate-exposed populations sit nearest the full-target end, with the constrained pollution-exposed communities slightly below them. Dual-positioned seats land mid-range: renewable developers combine payer costs with beneficiary buildout and mobile exit, and ratepayers combine the surcharge with firm supply. Scope amplifies the global seat: the climate-exposed populations' costs are the hardest to verify and the most diffuse, so their effective burden scales up relative to the local pollution seat carrying a similar declared role. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents both mislabels. Reading the arrangement as pure coordination, which is the doctrine's own self-description as prudent insurance, would erase the identifiable payers: ratepayers billed for the premium, communities breathing the externality, populations inheriting the emissions. Reading it as pure extraction, which is the climate-advocate framing of a fossil-and-nuclear bailout, would erase the genuine function: firm capacity during transition, workforce continuity that cannot be rebuilt on demand, and insurance against documented lock-in and premature-exit harms. The founding problem, irreversible commitment under deep uncertainty, remains live, so the mandate is not atrophied: the live founding-problem status and the world_rearranges disappearance verdict agree, and no zombie flag is expected. The drift to watch is different: if storage cost curves and climate urgency keep narrowing the uncertainty, the hedging mandate could atrophy into pure incumbent maintenance, with theater rising while the coordination function flattens. The theater_ratio series is the tripwire; it peaked in 2017 and has partially receded, which currently reads as function persisting rather than performance replacing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the acceptable_risk_energy kernel; what exactly would change structurally if a sibling reading displaced it, and where is the disagreement located?',
    'Comparative structural analysis across the three reading-files: track which seats change role (payers under catastrophic_tail_dominant include closed-plant workforces; payers under expected_value_dominant shrink to residual subsidy recipients), which enforcement machinery persists, and which victim-ledger entries survive translation between readings.',
    'Under catastrophic_tail_dominant, both nuclear and fossil face mandated closure and the victim set shifts to closure-exposed workforces and regions; under expected_value_dominant, coal exits on mortality accounting while nuclear persists and subsidy flows shrink. This arrangement''s epsilon is valid only for its own reading; cross-reading epsilon comparison is a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of three readings of the acceptable-risk kernel; disagreement located in the deep-uncertainty decision rule and in the victim ledger.').

omega_variable(
    depth_of_uncertainty_empirical_status,
    'Is the uncertainty governing energy pathways still deep enough to defeat probabilistic decision rules, or have storage cost curves, climate sensitivity estimates, and technology learning rates narrowed the distributions enough that expected-value commitment is now decision-relevant?',
    'Backtest option-preservation against committed-pathway strategies over 1990-2024 using realized costs, damages, and technology trajectories; forward-test storage deployment data against the hedge''s annual premium.',
    'If uncertainty has narrowed, the option premium is overpriced, the coordination function decays toward incumbent maintenance, and the expected_value_dominant sibling gains structural ground; if genuinely deep, the premium reads as coordination cost and the extraction metrics overstate the arrangement''s burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_of_uncertainty_empirical_status, empirical, 'Whether deep uncertainty, the reading''s foundational premise, still holds empirically.').

omega_variable(
    option_premium_incidence,
    'Who actually bears the option premium: do subsidy surcharges and capacity charges fall proportionally or regressively across customer classes, and where do the continued-operation pollution burdens concentrate?',
    'Rate-impact disaggregation by customer class in zero-emission-credit and capacity-market jurisdictions, mapped against pollution exposure and income data.',
    'Regressive incidence raises effective extraction for the payer seats and pushes the arrangement toward the extraction-dominant end of the hybrid range; proportional incidence supports the insurance framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_premium_incidence, empirical, 'Distribution of the hedge''s costs across ratepayer classes and exposure communities.').

omega_variable(
    closure_harm_ledger_symmetry,
    'Does the reading''s victim ledger apply a common metric across both error types, counting opportunity costs of premature closure (coal backfill, workforce dispersal, import dependence) on par with continued-operation harms (pollution, emissions, subsidy), or does it selectively weight closure harms to justify the hedge?',
    'Full harm-ledger reconstruction for both error types over the interval under a single metric (monetized health burdens plus distributional weights), audited by parties outside the benefiting set.',
    'If the ledger is asymmetric, part of the coordination function is cover, effective extraction rises toward the extraction-dominant end, and the tangled_rope claim weakens toward snare; if symmetric, the hedging is honest insurance and the reading''s claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_harm_ledger_symmetry, conceptual, 'Whether the doctrine''s signature victim accounting is symmetric or selectively weighted.').

omega_variable(
    firm_capacity_substitutability,
    'Is firm dispatchable capacity during the transition genuinely necessary at the scale the arrangement maintains, or do storage, transmission, and demand response substitute at lower total cost than the hedge''s premium?',
    'Engineering and market studies of high-renewables systems with storage buildout; compare reliability outcomes and total system cost against capacity-payment counterfactuals.',
    'If substitutable, the enforcement machinery defends incumbents rather than reliability and the justification for active enforcement collapses; if not substitutable at transition scale, the arrangement''s core coordination claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_substitutability, empirical, 'Whether the reliability function the arrangement defends is substitutable at scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__option_value_preserving, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(acce_tr_t8, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t21, acceptable_risk_energy__option_value_preserving, theater_ratio, 21, 0.28).
narrative_ontology:measurement_basis(acce_tr_t21, observed).
narrative_ontology:measurement(acce_tr_t27, acceptable_risk_energy__option_value_preserving, theater_ratio, 27, 0.36).
narrative_ontology:measurement_basis(acce_tr_t27, observed).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__option_value_preserving, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(acce_tr_t32, observed).
narrative_ontology:measurement(acce_tr_t34, acceptable_risk_energy__option_value_preserving, theater_ratio, 34, 0.3).
narrative_ontology:measurement_basis(acce_tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__option_value_preserving, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(acce_be_t8, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t21, acceptable_risk_energy__option_value_preserving, base_extractiveness, 21, 0.55).
narrative_ontology:measurement_basis(acce_be_t21, observed).
narrative_ontology:measurement(acce_be_t27, acceptable_risk_energy__option_value_preserving, base_extractiveness, 27, 0.6).
narrative_ontology:measurement_basis(acce_be_t27, observed).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__option_value_preserving, base_extractiveness, 32, 0.64).
narrative_ontology:measurement_basis(acce_be_t32, observed).
narrative_ontology:measurement(acce_be_t34, acceptable_risk_energy__option_value_preserving, base_extractiveness, 34, 0.66).
narrative_ontology:measurement_basis(acce_be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__option_value_preserving, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(acce_su_t8, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t21, acceptable_risk_energy__option_value_preserving, suppression_requirement, 21, 0.5).
narrative_ontology:measurement_basis(acce_su_t21, observed).
narrative_ontology:measurement(acce_su_t27, acceptable_risk_energy__option_value_preserving, suppression_requirement, 27, 0.56).
narrative_ontology:measurement_basis(acce_su_t27, observed).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__option_value_preserving, suppression_requirement, 32, 0.53).
narrative_ontology:measurement_basis(acce_su_t32, observed).
narrative_ontology:measurement(acce_su_t34, acceptable_risk_energy__option_value_preserving, suppression_requirement, 34, 0.52).
narrative_ontology:measurement_basis(acce_su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% The colloquial label 'acceptable risk in energy policy' covers three structurally distinct arrangements per the epsilon-invariance principle, so it decomposes into a three-story constraint family: the option-value-preserving arrangement (this file, with both pathways kept viable and the premium paid by ratepayers and exposed populations), the catastrophic-tail arrangement (mandated closure of both nuclear and fossil, with the victim set shifting to closure-exposed workforces and regions), and the expected-value arrangement (mortality-per-TWh commitment, with coal exiting and nuclear persisting on reduced subsidy). Each story carries its own epsilon, beneficiaries, and victims; measuring acceptable risk by one reading's observables yields a different epsilon than measuring it by another's, which is the signal that these are different constraints sharing a label rather than one constraint with a measurement parameter. The files are linked through affects_constraints because each reading's operation changes the resource and legitimacy environment of the others; this reading's subsidy and capacity machinery materially changes what the sibling arrangements would inherit if adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
