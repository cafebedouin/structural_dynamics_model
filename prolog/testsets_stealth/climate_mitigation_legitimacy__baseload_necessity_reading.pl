% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Doctrine in Decarbonization Pathway Governance
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The claim that reliable decarbonization requires dispatchable baseload
 *   power that renewables cannot provide at scale operates in energy
 *   governance as a legitimacy gate: it determines which portfolios count as
 *   serious, which technologies receive capacity payments and credits, and
 *   which are demoted as insufficient. This story instantiates the
 *   baseload_necessity_reading of the climate_mitigation_legitimacy kernel.
 *   The referent for epsilon is the standing arrangement under contest - the
 *   existing resource-adequacy, accreditation, and procurement regime that
 *   privileges firm dispatchable capacity - assessed by this reading's own
 *   lights. Because this reading affirms the necessity claim, it grants that
 *   most firm-capacity cost purchases real insurance; the epsilon it authors
 *   (0.52) reflects the excesses it cannot deny (cost overruns, gold-plating,
 *   fossil free-riding on the same doctrine) rather than the arrangement a
 *   hostile reading would score. Sibling readings share this referent and
 *   author different epsilon values over it; they are separate constraint
 *   files linked through the network block. The claim/metric independence
 *   rule is honored: claimed_type is stated from structure (a genuine
 *   coordination core joined to an enforced asymmetry), and the metrics are
 *   authored descriptively without tuning toward any predicted engine
 *   verdict.
 *
 * KEY AGENTS:
 *   - incumbent_utility_holdcos: agenda setter and principal receipt-holder (institutional/constrained) - administers procurement, earns returns on firm assets, writes the reliability studies
 *   - nuclear_operators_and_vendors: primary beneficiary (institutional/constrained) - collects capacity payments and credits as classified-necessary infrastructure
 *   - gas_generation_industry: secondary beneficiary (powerful/mobile) - rides the adequacy doctrine to extend the thermal market
 *   - renewable_storage_developers: primary target among competitors (organized/constrained) - discounted accreditation raises their delivered cost
 *   - electricity_ratepayers: diffuse payer (powerless/trapped) - funds capacity payments, overruns, and stranded-asset risk
 *   - demand_flexibility_aggregators: excluded competitor (moderate/trapped) - adequacy methods undervalue their product
 *   - grid_reliability_regulators: enforcement administrator (institutional/constrained) - sets adequacy standards and accredits capacity
 *   - energy_system_analysts: analytical observer (institutional/analytical) - sees the full structure, holds no procurement authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Doctrine in Decarbonization Pathway Governance").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '887d8422-5281-457a-844b-b9f3c583733d').
narrative_ontology:cs_kernel_codification('887d8422-5281-457a-844b-b9f3c583733d', distributed).
narrative_ontology:cs_authority_grounding('887d8422-5281-457a-844b-b9f3c583733d', expertise).
narrative_ontology:cs_interpretation_layer_present('887d8422-5281-457a-844b-b9f3c583733d').
narrative_ontology:cs_reading_relation('887d8422-5281-457a-844b-b9f3c583733d', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('887d8422-5281-457a-844b-b9f3c583733d', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('887d8422-5281-457a-844b-b9f3c583733d', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('887d8422-5281-457a-844b-b9f3c583733d', foundational, firm_dispatchable_capacity_nonsubstitutable_at_scale).
narrative_ontology:cs_axiom_status(firm_dispatchable_capacity_nonsubstitutable_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('887d8422-5281-457a-844b-b9f3c583733d', firm_dispatchable_capacity_nonsubstitutable_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('887d8422-5281-457a-844b-b9f3c583733d', foundational, reliability_precedence_over_cost_in_pathway_choice).
narrative_ontology:cs_axiom_status(reliability_precedence_over_cost_in_pathway_choice, holdable).
narrative_ontology:cs_axiom_grounding('887d8422-5281-457a-844b-b9f3c583733d', reliability_precedence_over_cost_in_pathway_choice, deontological).
narrative_ontology:cs_reference_frame('887d8422-5281-457a-844b-b9f3c583733d', firm_capacity_anchored_adequacy).
narrative_ontology:cs_drift_state('887d8422-5281-457a-844b-b9f3c583733d', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('887d8422-5281-457a-844b-b9f3c583733d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_operators_and_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utility_holdcos).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, gas_generation_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_storage_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, demand_flexibility_aggregators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own most of the thermal fleet, file the integrated resource plans, and propose the procurements that state commissions approve. Earn authorized returns on capital they build and recover fuel and operating costs from customers. They commission the reliability studies that justify new firm capacity and bear the reputational blame for outages, which makes them conservative about retiring thermal units. Franchise obligations limit exit, but they can route costs into customer rates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utility_holdcos, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utility_holdcos, beneficiary).

% Operate the existing reactor fleet and sell new plant designs, components, fuel, and services. Collect capacity payments, production tax credits, and regulated cost recovery tied to their units being procured as essential firm supply. Their revenue case depends on remaining classified as indispensable; early retirement strands decades of sunk capital, so they press for long license renewals and favorable accreditation treatment. Exit means writing off assets with very long service lives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_operators_and_vendors, beneficiary,
    institutional, generational, constrained, global).

% Builds and operates combined-cycle and peaking plants whose capacity payments and running hours follow from reliability requirements. Every adequacy study concluding that variable-output resources need firm backup enlarges its addressable market. Turbines are faster to build than reactors and can sometimes be converted or relocated, so its capital position is far less sunk than the nuclear fleet's.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, gas_generation_industry, beneficiary,
    powerful, biographical, mobile, continental).

% Develop wind, solar, and battery projects and sell output under contracts and market rules benchmarked to firm-capacity standards. Accreditation methods discount their contribution during stress hours, so they must overbuild or pair with storage to win procurements, raising their delivered cost relative to thermal bids. Capital committed to specific projects cannot move; the development pipeline can be redirected to other jurisdictions only slowly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_storage_developers, payer,
    organized, biographical, constrained, global).

% Pay the bills that fund capacity payments, plant construction, and cost overruns, and absorb the retail price effects of every portfolio decision. They have no direct seat in resource-adequacy proceedings except through consumer advocates of varying strength, and leaving the grid is impractical for households. Outage blame lands on whoever holds the reliability portfolio, which shapes what they can be told they must pay for.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers, payer,
    powerless, biographical, trapped, regional).

% Bundle controllable load, distributed batteries, and curtailable industrial demand into products that shave peak hours. Adequacy frameworks measure them with methods designed for large generators, so their certified contribution stays small and their revenue thin despite demonstrated field performance. Their business case depends on rule changes made in proceedings dominated by the incumbent owners and operators of thermal assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, demand_flexibility_aggregators, excluded,
    moderate, immediate, trapped, national).

% Set resource-adequacy standards, accredit capacity, approve integrated resource plans, and enforce reliability mandates. Their statutory duty is continuity of supply, and their professional risk concentrates on visible failures rather than on overspending, so their determinations weight firm capacity heavily. They can rewrite accreditation rules but face litigation, legislative pushback, and outage blame when they do.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Produce capacity-expansion models, loss-of-load studies, and scenario literature at universities, national laboratories, and international agencies. They publish the evidence both camps cite, referee the substitutability question, and hold no procurement authority; their influence runs through which numbers decision-makers choose to quote.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, energy_system_analysts, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utility_holdcos).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Electricity systems must balance supply and demand continuously under weather-correlated uncertainty, including multi-day events of low wind, low sun, and extreme temperature that variable-output resources and short-duration storage cover poorly. The arrangement coordinates long-lived capital investment around that insurance function and gives planners a shared adequacy metric for comparing portfolios.
% TRANSFER_FUNCTION: Moves ratepayer revenue and public subsidy toward owners of dispatchable thermal and nuclear assets through capacity payments, regulated returns, and tax credits; moves procurement share and accredited capacity value away from variable renewables, storage, and demand-side resources; transfers cost-overrun and stranded-asset risk to customers and taxpayers.
% ABSENT_VOICES: Demand-flexibility aggregators and storage-first developers participate in markets but are structurally absent from adequacy rule-making, where accreditation methodology is set by and for thermal incumbents. Future ratepayers who will inherit the costs of long-lived assets have no seat at all. Consumer advocates represent present ratepayers unevenly across jurisdictions.
% DISAPPEARANCE_RATIONALE: If the necessity doctrine vanished overnight, capacity market constructs, integrated resource plan conventions, nuclear subsidy justifications, and fossil life-extension filings would lose their organizing principle simultaneously. Procurement would rebalance toward accredited portfolios of renewables, storage, and flexibility; stranded-asset litigation would begin immediately; and the nuclear order book would collapse to the few projects already under construction.
% FOUNDING_PROBLEM: Twentieth-century resource adequacy was built for thermal-dominated grids: variable renewables were negligible, storage was uneconomic, and the planning question was how much steam-turbine capacity to hold in reserve. The doctrine formalized that answer into accreditation rules, reserve margins, and procurement categories.
% FOUNDING_PROBLEM_CORROBORATION: Independent system-operator loss-of-load studies and the peer-reviewed capacity-expansion literature corroborate that the underlying adequacy problem is real and remains live under deep decarbonization. International scenario bodies attest that decarbonized portfolios retain a firm-resource need while disputing that it must take traditional baseload form. For the narrow claim that only conventional baseload suffices, corroboration comes almost entirely from parties inside the benefiting set; no fully external source attests that narrow version, and that absence is itself signal.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52: by this reading's own lights the bulk of firm-capacity expenditure buys genuine reliability insurance, but the reading concedes overruns, rate-base padding, and gas plants sheltered by the same doctrine - a real excess it cannot explain away. Suppression is 0.58: alternatives are not banned, but accreditation methodology and procurement design systematically demote them, and demotion is enforced through mandatory reliability standards, capacity market constructs, and integrated resource plan approval. Theater ratio is 0.30: most reliability analysis is functional engineering, but a growing share of adequacy discourse functions rhetorically to shield thermal fleets from retirement pressure. Accessibility collapse is 0.42: once the doctrine is understood, alternatives remain visible and partially viable - renewables keep winning procurements on cost - so alternatives have not collapsed the way a natural limit would collapse them. Resistance is 0.66: the doctrine meets sustained, well-funded contest from renewable and storage industries, climate advocates, and some commissions, which is what a defended construct rather than a natural law looks like. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) with all three tracked metrics authored at every point; all series rise modestly, reflecting enforcement infrastructure that hardened over the interval (post-blackout mandatory standards, capacity market construction, tightening accreditation) and a doctrine increasingly deployed against competing resources. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change, not merely extraction drift. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine through directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats should compute divergent types from identical structural data. From the incumbent utility and regulator positions the arrangement is a reliability regime they administer in good faith under asymmetric blame: failure is visible and career-ending, overspending is diffuse and forgivable, so their seat computes coordination-dominant. From the ratepayer and developer positions the same structure operates as enforced cost allocation with demoted alternatives. Institutional identity reinforces the administrator side: organizations constituted around the mission of keeping the lights on experience skepticism about firm capacity as a threat to identity, not merely to revenue, which is why their exit is constrained even where legal authority to change course exists. Ratepayers are individually powerless but not without coalition potential - organized consumer advocacy has shifted accreditation dockets in several jurisdictions - which caps how far their effective extraction can ratchet before political feedback engages. The analyst seat observes the divergence without occupying either side of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: nuclear operators and vendors and the gas industry sit near the beneficiary pole (d near 0), with the gas industry slightly less subsidized by the doctrine than the nuclear fleet because its capital is less sunk and its product competes more directly with the alternatives being demoted. Incumbent utility holdcos combine agenda-setting with receipt-collection, placing them deepest at the beneficiary end - they both administer the arrangement and accrue its gains, which is why the receipt surface names them. Victim declarations drive the opposite pole: ratepayers (trapped, powerless) sit nearest the full-target end, with renewable_storage_developers close behind (constrained exit, organized enough to litigate and lobby). Demand-flexibility aggregators are declared victims despite their excluded role: the accreditation methods are the mechanism through which they pay. Regulators derive near-symmetric positioning through the fallback chain - they neither collect the gains nor bear the costs directly - and the analyst seat is analytical by construction. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - assuring resource adequacy in a thermal-era grid - is contested rather than dead: the insurance need is corroborated by external engineering literature, while the narrow claim that only conventional baseload meets it is corroborated almost exclusively inside the benefiting set. The status-times-verdict pairing (contested, world_rearranges) correctly avoids the zombie flag: this is not an arrangement outliving a vanished function but an arrangement whose function is real and whose scope is disputed. The tangled_rope classification does the mandatrophy work in both directions: a pure-snare reading would license discarding genuine reliability planning along with the rent collection, and a pure-rope reading would excuse the enforced asymmetry, the capital lock-in, and the demotion of alternatives as mere coordination overhead. Holding both faces of the structure open keeps the substitutability question - the actual crux - empirically live instead of settling it by category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the baseload_necessity_reading of the climate_mitigation_legitimacy kernel. How would the sibling readings (renewable_primacy_reading, portfolio_pragmatism_reading, degrowth_sufficiency_reading) restructure the beneficiary/victim sets and epsilon for the same standing pro-firm-capacity policy regime?',
    'Generate the three sibling stories over the identical referent (the existing resource-adequacy and procurement regime) and compare computed per-seat classifications; the disagreement is located in the substitutability proposition and in the reliability-versus-cost precedence, not in the referent.',
    'The renewable_primacy reading would raise epsilon sharply and enlarge the victim set on the same referent; portfolio_pragmatism would compress extraction toward the coordination floor; degrowth_sufficiency would relocate the dispute from supply composition to demand levels. Classification of the regime is therefore reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a four-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    firm_capacity_substitutability,
    'Can long-duration storage, demand flexibility, transmission expansion, and firm low-carbon generators collectively substitute for thermal baseload at full-decarbonization scale, including through multi-day low-wind low-solar events?',
    'Effective-load-carrying-capacity accreditation studies, multi-day storage cost trajectories, dark-doldrums event analysis, and realized performance of high-renewables portfolios during recorded stress events.',
    'If substitutable, the doctrine''s demotion of alternatives is exclusion without engineering warrant and effective extraction rises toward the snare boundary; if not substitutable at acceptable cost, a substantial share of the arrangement''s cost is genuine insurance and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_substitutability, empirical, 'The technical crux on which the necessity claim stands or falls.').

omega_variable(
    rent_insurance_composition,
    'What share of the arrangement''s cost above a technology-neutral procurement counterfactual is incumbent rent collection versus genuine reliability insurance value?',
    'Counterfactual capacity-expansion modeling with and without firm-capacity preference, paired with forensic comparison of procurement outcomes and rate-base growth in comparable jurisdictions.',
    'An insurance-dominated composition supports the tangled-rope reading with a modest excess-extraction tail; a rent-dominated composition pushes the same structure toward snare and motivates accreditation reform rather than doctrinal preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_insurance_composition, empirical, 'Decomposition of measured cost into coordination content and captured surplus.').

omega_variable(
    sunk_capital_ratchet,
    'Does the doctrine persist because ongoing engineering analysis supports it, or because sunk capital in forty-to-eighty-year assets makes reversal appear irresponsible to the administrators who would have to reverse it?',
    'Cross-jurisdictional comparison of doctrine strength against thermal-legacy intensity: if adherence tracks the size of incumbent rate bases rather than grid physical characteristics, the ratchet dominates; new-build grids with minimal thermal legacy provide the cleanest contrast.',
    'Ratchet dominance predicts the justification decays as assets turn over and the arrangement drifts toward inertial maintenance; analysis dominance predicts stable persistence independent of asset age.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunk_capital_ratchet, empirical, 'Whether persistence is evidential or balance-sheet-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 18, 0.49).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate over whether climate mitigation needs baseload decomposes, per the epsilon-invariance principle, into four readings of the climate_mitigation_legitimacy kernel, each a separate constraint story with its own epsilon, beneficiary/victim structure, and classification over the shared referent of the existing pro-firm-capacity policy regime. This file instantiates baseload_necessity_reading (epsilon 0.52 by its own lights). The renewable_primacy_reading shares the referent and authors substantially higher epsilon with an enlarged victim set; portfolio_pragmatism_reading compresses extraction toward the coordination floor; degrowth_sufficiency_reading relocates the contest to the demand axis. Upstream-downstream structure: the baseload necessity claim is cited as engineering warrant inside the portfolio and renewable debates, so this reading exerts structural pressure on its siblings' operating environments. All four files carry mutual links in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
