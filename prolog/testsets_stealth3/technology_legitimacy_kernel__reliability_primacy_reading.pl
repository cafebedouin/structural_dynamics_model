% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Legitimacy Gate for Climate Technologies
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates one specific legitimacy criterion operating in
 *   climate and energy governance: a technology counts as a legitimate
 *   mitigation tool if and only if it provides dispatchable, baseload-capable
 *   generation. The criterion does real work — grids genuinely need firm
 *   capability — but its biconditional form converts a system-level adequacy
 *   property into a per-technology admission test, and that conversion
 *   channels subsidy eligibility, capacity revenues, and policy standing
 *   toward incumbent firm generators while imposing storage-bundling costs
 *   and accreditation haircuts on entrants and diffuse reliability premiums
 *   on ratepayers. The claim and the metrics are authored independently: the
 *   constraint is CLAIMED as tangled_rope because the story's analysis holds
 *   that a genuine coordination function and asymmetric extraction ride the
 *   same structure, while each metric records what is descriptively true of
 *   the arrangement's operation; the engine computes per-seat classifications
 *   from the structural data and any divergence from this claim is the datum,
 *   not an error. The epsilon referent is the standing arrangement — the gate
 *   as it actually operates in accreditation proceedings, subsidy design, and
 *   policy discourse — assessed by this reading's own lights, which count
 *   genuine reliability service as legitimate coordination cost and therefore
 *   temper measured extraction below what a seat hostile to the criterion
 *   would likely author.
 *
 * KEY AGENTS:
 *   - - regional_grid_regulators: Agenda setter (institutional/constrained) — accredits capacity and approves plans under firm-capability criteria; administrative authority rides on the framework
 *   - - nuclear_power_operators: Primary beneficiary (organized/identity_locked) — passes the gate unconditionally; collects credit eligibility and capacity revenue; commercially and professionally fused with the baseload frame
 *   - - gas_generation_owners: Secondary beneficiary (powerful/constrained) — uses the gate defensively to defer retirement of sunk dispatchable assets and collect capacity payments
 *   - - ratepayers: Primary target (powerless/trapped) — bear capacity charges, zero-emission-credit surcharges, and storage-procurement riders through retail rates
 *   - - wind_solar_developers: Net target with incidental benefit (powerful/mobile) — pay storage-bundling and accreditation costs yet gain storage demand and can migrate across jurisdictions
 *   - - battery_storage_vendors: Incidental beneficiary (organized/arbitrage) — sell the compliance good the gate mandates across global markets
 *   - - demand_response_providers: Excluded voice (organized/constrained) — functionally dispatchable load classified as non-generation, absorbing discounted capacity value with no formal seat
 *   - - power_systems_engineering_community: Analytical observer (analytical/analytical) — documents that flexibility, storage, and interconnection can deliver historically baseload-attributed services
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.64).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.54).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Legitimacy Gate for Climate Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '21b1e7ca-1fdd-428f-adc7-20be0b0edba9').
narrative_ontology:cs_kernel_codification('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', formalized).
narrative_ontology:cs_authority_grounding('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', expertise).
narrative_ontology:cs_interpretation_layer_present('21b1e7ca-1fdd-428f-adc7-20be0b0edba9').
narrative_ontology:cs_reading_relation('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_reading_relation('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', foundational, grid_stability_requires_dispatchable_generation).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_generation, holdable).
narrative_ontology:cs_axiom_grounding('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', grid_stability_requires_dispatchable_generation, empirically_contingent).
narrative_ontology:cs_axiom('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', secondary, legitimacy_attaches_to_delivered_firm_energy).
narrative_ontology:cs_axiom_status(legitimacy_attaches_to_delivered_firm_energy, holdable).
narrative_ontology:cs_axiom_grounding('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', legitimacy_attaches_to_delivered_firm_energy, instrumental).
narrative_ontology:cs_reference_frame('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', firm_capacity_reserve_margin_framework).
narrative_ontology:cs_drift_state('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', contemporary_flexibility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21b1e7ca-1fdd-428f-adc7-20be0b0edba9', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_owners).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_vendors).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, demand_response_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, baseload_plant_paradigm).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, reserve_margin_adequacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accredit capacity resources, approve integrated resource plans, and run resource-adequacy proceedings using firm-capacity criteria. Their administrative authority and staffing rest on the adequacy framework the gate expresses; abandoning the planning category would require statutory redesign and expose them to outage-blame politics, so their realistic exit is incremental criteria revision rather than departure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, regional_grid_regulators, agenda_setter,
    institutional, generational, constrained, regional).

% Operate high-capacity-factor plants that pass the gate unconditionally. Collect eligibility for clean-electricity credits, federal loan guarantees, and capacity revenues that are framed as reliability compensation. Their commercial case and professional self-understanding are built around baseload economics, so leaving the frame would mean dismantling the narrative their asset values and workforce identities depend on.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_operators, beneficiary,
    organized, generational, identity_locked, national).

% Market their fleets as dispatchable firming capacity and use the gate to defend units against retirement, collecting capacity payments and reliability-must-run designations. Their benefit is largely defensive — deferred stranding of sunk assets — and their fuel-supply contracts and turbine fleets give them little room to pivot away from the frame that keeps those assets monetized.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_owners, beneficiary,
    powerful, biographical, constrained, national).

% Bear capacity charges, reliability premiums, and system-cost pass-throughs embedded in retail rates, including zero-emission credit surcharges and storage-procurement riders justified in reliability terms. Leaving the grid is impractical, moving service territory is costly, and the charges are diffuse enough per household that organized objection is rare.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, biographical, trapped, regional).

% Face accreditation haircuts, storage-bundling requirements, and interconnection delays when their projects are evaluated under firm-capacity criteria, raising their cost of policy standing. The same gate manufactures demand for the storage systems they increasingly co-develop, and their multi-jurisdictional project pipelines let them shift investment toward markets with friendlier accreditation treatment.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers, beneficiary).

% Sell the compliance good the gate requires: every legitimacy hurdle placed on intermittent generation translates into storage procurement. They sell into multiple national markets simultaneously and can reprice or reroute product freely, so the gate functions for them as a demand-subsidy they never had to advocate for openly.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, battery_storage_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Aggregate flexible load that can curtail on command — functionally dispatchable — but accreditation and planning frameworks classify them as non-generation, capping their counted capacity value and excluding them from the legitimacy conversation about what counts as a real resource. They absorb the opportunity cost of discounted capacity revenue while having no formal seat in the proceedings that set the definitions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, demand_response_providers, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, demand_response_providers, payer).

% Publishes operational studies showing that inverter-based resources, storage, transmission interconnection, and demand flexibility can deliver the services historically attributed to baseload plants, and that flexibility and ramping capability are becoming the operative planning quantities. Its findings are selectively cited by every other seat but it collects and pays nothing under the arrangement.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, power_systems_engineering_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real resource-adequacy problem: electric grids require firm capacity, frequency response, and sustained output through weather-driven lulls and demand extremes, and the gate gives planners a single accreditation standard — dispatchable capability — for steering investment toward resources that deliver those services.
% TRANSFER_FUNCTION: Moves subsidy eligibility, capacity-market revenues, permitting priority, and climate-financing access toward firm-capable portfolios; moves compliance costs — storage procurement, accreditation haircuts, interconnection delay — onto intermittent-resource developers; and moves system-cost premiums to ratepayers through retail rates.
% ABSENT_VOICES: Demand-response providers and other distribution-edge resources would object that load flexibility is dispatchability and that the generation-centric frame arbitrarily caps their counted value; low-income ratepayers would object to the regressive incidence of reliability premiums but are diffuse across jurisdictions and rarely seated; parts of the power-systems engineering community would object that the baseload category itself is an obsolete planning construct, and they are heard selectively rather than as definitional authorities.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight, capacity-accreditation regimes would have to be rebuilt around probabilistic portfolio adequacy, clean-energy subsidy eligibility would detach from firm-capability tests, firm-generator revenues would reprice against energy-only compensation, and storage-procurement obligations currently justified as reliability compliance would be renegotiated — the resource-adequacy apparatus and the flows riding on it would visibly reorganize.
% FOUNDING_PROBLEM: Mid-twentieth-century grid expansion centered on very large, capital-intensive thermal plants whose economics required high utilization against predictable demand segments; planners needed a category guaranteeing that committed plants would run enough to recover costs and that aggregate supply would meet peak load plus reserve margin. 'Baseload' emerged as that planning category, and the later question of which technologies count as legitimate climate solutions inherited the category wholesale.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability bodies and international energy assessments — none of them beneficiaries of the gate — corroborate that resource adequacy under weather-driven lulls and demand extremes remains a live engineering problem. Conversely, system-operator experience with high shares of inverter-based resources and the flexibility-focused operations literature, likewise external to the beneficiaries, attest that the specific baseload category is contested and increasingly bypassed in practice. No source outside the benefiting parties and allied trade associations attests that the biconditional form itself — legitimacy iff individual dispatchability — is necessary; the iff-conversion from a system property to a per-technology admission test is corroborated by no one outside the seat it admits.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but bounded (0.64 at interval end): capacity payments and reliability premiums partly purchase real reliability services, so not all transferred value is rent; the excess lies in the iff-conversion, which has no engineering necessity — portfolio-level adequacy does not require every admitted technology to be individually dispatchable. Suppression is moderate (0.54) and reflects active enforcement machinery: accreditation rule maintenance, integrated-resource-plan approvals, and recurring legislative attempts to reprice firm power; it is authored as a raw structural property and is deliberately NOT scaled — only extractiveness is scaled, by directionality and scope in the engine's computation. Theater rises steadily (0.18 to 0.40) as the term 'baseload' ages against operational practice that increasingly prices flexibility, ramping, and stored energy rather than plant class; a growing fraction of gate activity defends the category rather than performs adequacy work. Accessibility collapse is moderate-high (0.62): within the frame's own logic almost every alternative is redescribed as insufficient until firmed, but a genuine compliance path (storage) exists, so alternatives narrow without vanishing. Resistance is high (0.66): sustained industrial, advocacy, and engineering contestation, reinforced by deployment data that keeps eroding the gate's empirical premises. The temporal series share one grid (points 0-24 at stride 4) so every metric is authored at every examined time point; the suppression series dips at point 20 because a prominent federal repricing proposal failed and subsequent federal legislation broadened subsidy eligibility beyond firm-capable resources, temporarily relaxing enforcement before demand-side pressure revived firm-power advocacy — a contested-enforcement oscillation, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is a competent adequacy standard the regulator administers with professional diligence; from the nuclear-operator seat it is a deserved recognition of genuine firm value; from the gas-owner seat it is a survival shield for sunk assets; from the ratepayer seat it is a diffuse, uncontestable bill; from the developer seat it is a toll booth with a purchasable pass; from the excluded demand-response seat it is a definitional wall. Same nominal policy domain, radically different experienced constraints — the divergence is computed by the engine from the structural data above, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation for the three collector seats: nuclear operators (identity-locked beneficiaries, d near the subsidized end), gas owners (beneficiaries whose defensive rent still registers as subsidy), and storage vendors (arbitrage-grade beneficiaries, nearest the beneficiary extreme, since they can exit into any market while collecting everywhere). Victim declarations drive the target seats: ratepayers (trapped, near the full-target end), developers (net targets despite the storage side-payment, moderated slightly by mobility), and demand-response providers (structurally discounted participants). One explicit override is declared for the institutional power atom, held solely by the regulators: with no beneficiary or victim declaration, the regulator would fall to the per-atom canonical fallback near symmetry, but the true relationship is mild beneficiary-of-authority (d approximately 0.32) — the constraint feeds the regulator's administrative mandate, staff remit, and procedural centrality without transferring extraction receipts to it. The override corrects a blind spot of the derivation chain, which cannot see authority-subsidy for undeclared seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assuring adequacy for a thermal-dominated planning regime with inflexible, capital-heavy plants — has been substantially transformed rather than solved: the adequacy concern survives in modified form (weather-driven lulls, extreme-event resilience), while the specific solution category (plant-class baseload with reserve margins) is increasingly bypassed by flexibility-based operation. Because the status is contested rather than dead, the constraint is not declared mandatrophy-resolved; it retains a live core function entangled with a superseding practice. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the genuine adequacy coordination that ratepayer premiums partly purchase, and reading it as pure coordination would erase the iff-conversion through which incumbent seats collect rents no engineering requirement obligates anyone to pay them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the reliability-primacy reading of technology_legitimacy_kernel; sibling readings (deployability-within-carbon-budget; bounded-and-reversible-failure-modes) instantiate structurally different constraints with different epsilon values, beneficiary sets, and victim sets. What determines which reading governs legitimacy judgments in a given jurisdiction or proceeding?',
    'Comparative coding of accreditation proceedings, subsidy statutes, and legislative testimony across jurisdictions to identify which reading''s criterion is operationally decisive where, and tracking of shifts after reliability events or major deployment milestones.',
    'Under the deployability reading, the nuclear seat migrates from beneficiary toward target (long build times fail the timeline test) and storage-plus-renewables portfolios enter the beneficiary set; under the failure-mode reading, nuclear exits the beneficiary set entirely and the victim set expands to include host communities and future generations. The classification of this file is conditional on reliability primacy actually governing the arrangement it describes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the technology-legitimacy kernel is operative, and how the seat structure reshuffles across readings').

omega_variable(
    baseload_category_validity,
    'Is individually dispatchable generation a physically necessary component of deeply decarbonized grids, or an obsolete planning category that storage, transmission, demand flexibility, and geographic aggregation render unnecessary?',
    'Operational performance data from systems running at high inverter-based-resource shares through lulls and extreme events, together with production-cost modeling of counterfactual portfolios with and without committed firm capacity.',
    'If the category is physically necessary, a large fraction of the measured transfer is genuine coordination cost and the constraint settles as a durable tangled rope; if the category is obsolete, the gate''s remaining function is legacy defense and rent collection, supporting reclassification toward snare or piton as theater rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_category_validity, empirical, 'Whether the gate''s coordination core is a real physical requirement or a legacy planning artifact').

omega_variable(
    reliability_cost_incidence,
    'Do capacity payments, reliability premiums, and firm-power surcharges track the verified reliability value of the receiving resources, or do they exceed it as incumbency protection?',
    'Comparison of accredited capacity compensation against marginal effective load-carrying-capacity valuations, and audit of reliability-must-run and zero-emission-credit determinations against avoided-outage cost estimates.',
    'Compensation tracking verified adequacy value supports the coordination reading and bounds extraction near the resource-allocation floor; persistent excess above verified value establishes the rent component and sharpens the tangled-rope-to-snare gradient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_cost_incidence, empirical, 'Whether reliability compensation tracks service value or incumbency rent').

omega_variable(
    nuclear_identity_fusion,
    'Is the nuclear seat''s vigorous defense of the gate driven by its verified reliability contribution, or by identity and economic lock-in — a commercial case and professional self-concept constituted by the baseload narrative?',
    'Observe advocacy behavior when reliability contributions and commercial interests diverge: whether operators support accreditation reforms that reward verified firm value but disadvantage their legacy units, and how workforce and investor communications reframe if capacity revenues detach from plant class.',
    'If lock-in dominates, the seat''s perceived persistence is inflated by identity rather than function, and a break of the baseload identity frame would convert this seat from gate-defender to indifferent party, accelerating drift toward piton dynamics for the gate as a whole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_identity_fusion, empirical, 'Identity-lock versus functional contribution as the driver of the primary beneficiary seat''s gate defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(tech_tr_t4, observed).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(tech_tr_t12, observed).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(tech_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement_basis(tech_be_t4, observed).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(tech_be_t12, observed).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(tech_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(tech_su_t4, observed).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(tech_su_t12, observed).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(tech_su_t16, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(tech_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate climate technology' decomposes, per the epsilon-invariance principle, into at least three structurally distinct constraints corresponding to the three declared readings of technology_legitimacy_kernel: this reliability-primacy reading (gate keyed to dispatchable capability; nuclear in the beneficiary set, ratepayers and gated entrants as payers), the deployability-within-carbon-budget reading (gate keyed to speed and scale; long-build-time technologies become targets), and the bounded-failure-modes reading (gate keyed to reversibility; legacy-waste technologies become targets). Each story carries its own epsilon, stakeholders, and classification; forcing one story to cover all three would manufacture observable-dependent epsilon. This file links both siblings via network.affects_constraints because they share the kernel, compete over the same legitimacy-adjudication venues, and condition one another's resource environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
