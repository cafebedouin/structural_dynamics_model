% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Baseload Necessity Doctrine in Decarbonization Legitimacy
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The doctrine that reliable decarbonization requires dispatchable baseload
 *   power beyond what renewables can provide at scale operates across
 *   planning institutions, capacity markets, and subsidy design. Presented as
 *   grid physics, it functions as a legitimacy gate: portfolios containing
 *   firm thermal capacity pass; renewable-only proposals are marked
 *   inadequate; capital concentrates in long-lived, utility-owned assets.
 *   Underneath sits a genuine coordination core - resource adequacy under
 *   adverse weather sequences is a real collective problem - and layered on
 *   top is asymmetric extraction: subsidy streams and rate-base returns flow
 *   to incumbent owners while accreditation penalties and financing spreads
 *   fall on the alternatives the gate excludes. This file instantiates the
 *   baseload_necessity_reading of the climate_mitigation_legitimacy kernel;
 *   the renewable_primacy, portfolio_pragmatism, and degrowth_sufficiency
 *   readings are separate constraint files with their own beneficiary
 *   structures and epsilon values, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   incumbent_regulated_utilities (institutional/arbitrage) - primary
 *   beneficiary and co-agenda-setter, collects rate-base returns and can
 *   redeploy capital if the paradigm shifts; nuclear_power_industry
 *   (powerful/identity_locked) - principal beneficiary, subsidy stream
 *   premised on the necessity claim; merchant_gas_generators
 *   (powerful/mobile) - secondary beneficiary, capacity revenues preserved by
 *   the dispatchability framing; electricity_ratepayers (moderate/trapped) -
 *   primary payer, bears cost premiums and socialized overruns;
 *   renewable_energy_developers (organized/constrained) - payer, bears
 *   accreditation and legitimacy penalties; climate_vulnerable_populations
 *   (powerless/trapped) - downstream payer, bears delayed-abatement damages;
 *   grid_planning_authorities (institutional/constrained) - agenda-setter
 *   administering the adequacy standards; demand_flexibility_providers
 *   (organized/constrained) - excluded voice;
 *   independent_energy_systems_analysts (analytical/analytical) - analytical
 *   observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.6).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Doctrine in Decarbonization Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'aa5a5318-e74d-4d4d-b5fc-1fe979957481').
narrative_ontology:cs_kernel_codification('aa5a5318-e74d-4d4d-b5fc-1fe979957481', distributed).
narrative_ontology:cs_authority_grounding('aa5a5318-e74d-4d4d-b5fc-1fe979957481', extraction).
narrative_ontology:cs_interpretation_layer_present('aa5a5318-e74d-4d4d-b5fc-1fe979957481').
narrative_ontology:cs_reading_relation('aa5a5318-e74d-4d4d-b5fc-1fe979957481', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('aa5a5318-e74d-4d4d-b5fc-1fe979957481', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('aa5a5318-e74d-4d4d-b5fc-1fe979957481', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('aa5a5318-e74d-4d4d-b5fc-1fe979957481', foundational, renewables_cannot_scale_to_firm_capacity).
narrative_ontology:cs_axiom_status(renewables_cannot_scale_to_firm_capacity, holdable).
narrative_ontology:cs_axiom_grounding('aa5a5318-e74d-4d4d-b5fc-1fe979957481', renewables_cannot_scale_to_firm_capacity, empirically_contingent).
narrative_ontology:cs_axiom('aa5a5318-e74d-4d4d-b5fc-1fe979957481', foundational, decarbonization_legitimacy_requires_firm_dispatchable_capacity).
narrative_ontology:cs_axiom_status(decarbonization_legitimacy_requires_firm_dispatchable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('aa5a5318-e74d-4d4d-b5fc-1fe979957481', decarbonization_legitimacy_requires_firm_dispatchable_capacity, instrumental).
narrative_ontology:cs_reference_frame('aa5a5318-e74d-4d4d-b5fc-1fe979957481', thermal_baseload_adequacy_standard).
narrative_ontology:cs_drift_state('aa5a5318-e74d-4d4d-b5fc-1fe979957481', post_storage_cost_collapse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa5a5318-e74d-4d4d-b5fc-1fe979957481', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_regulated_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, merchant_gas_generators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, resource_adequacy_planning_paradigm).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, marginal_firm_capacity_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the resource-adequacy rules through which every proposed decarbonization portfolio must pass: capacity accreditation, reserve margin targets, loss-of-load standards, integrated resource plan approval. Their procedures treat continuous-output thermal capacity as the reference against which other resources are discounted. They answer for blackouts, not for cost, so loosening the firm-capacity requirement carries asymmetric blame risk for the officials involved.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_planning_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Own and operate the large dispatchable plants whose value the adequacy rules certify, and earn regulated returns on whatever capital sits in the rate base. They draft the integrated resource plans, fund the technical studies, and hold seats on the planning committees where adequacy assumptions are fixed. Because returns attach to capital rather than to technology, they could redeploy into renewables, storage, or transmission if the planning paradigm moved; defending the current rules is a choice, not a survival condition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_regulated_utilities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_regulated_utilities, agenda_setter).

% Reactor vendors, operators, and fuel-cycle firms whose order books, subsidy eligibility, and workforce pipelines depend on the necessity claim. Production tax credits, loan guarantees, and licensing reform arrive premised on nuclear being indispensable rather than merely useful. The profession's self-understanding as essential, physics-grade reliability is bound up with the claim remaining true, and reactor-specific capital and skills have no ready second market.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry, beneficiary,
    powerful, generational, identity_locked, global).

% Sell capacity and energy into wholesale markets; the dispatchable-backup framing protects their capacity revenues from being competed away by storage and demand response. They can repower, resell, or convert units, so their attachment to the framing is commercial rather than structural.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, merchant_gas_generators, beneficiary,
    powerful, biographical, mobile, continental).

% Pay the bills: capacity charges, surcharges recovering reactor cost overruns, and securitized bonds for early-retired plants. Household budgets and rental tenure pin most of them to the grid, and behind-the-meter generation is priced out of reach for the households bearing the largest share of the cost premiums.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_ratepayers, payer,
    moderate, biographical, trapped, national).

% Build wind, solar, and storage projects and sell into the same markets. Accreditation formulas discount their capacity, planning models assign them high uncertainty costs, and public verdicts that renewable-only portfolios are inadequate raise their financing spreads. Their assets are pinned to sites and interconnection queues, so responding to adverse rules means absorbing the penalty or leaving the segment; they still benefit from the overall climate-policy demand their products serve.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, beneficiary).

% Live where heat, flood, and storm exposure is highest and adaptive capacity is lowest. When the adequacy gate slows or raises the price of the cheapest abatement routes, emissions persist longer and the resulting damages land disproportionately on them. There is no exit from the atmosphere.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Aggregate demand response, managed charging, and virtual power plant capacity that could displace part of the firm-capacity requirement. Adequacy accounting systematically undercounts these resources, and they hold no standing seat in the planning dockets where the discount factors are set.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, demand_flexibility_providers, excluded,
    organized, biographical, constrained, national).

% University groups, national laboratories, and independent consultants running multi-year weather-conditioned capacity-expansion models. Their publications both establish that firm capacity carries real value at high penetration and document renewable-heavy portfolios clearing reliability targets. They own no assets and collect no capacity revenue.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, independent_energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_regulated_utilities).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the resource-adequacy problem for a decarbonizing grid: guaranteeing that supply meets demand through worst-case weather sequences by requiring certified firm, dispatchable capacity in every approved portfolio, giving planners and regulators a determinate reliability criterion.
% TRANSFER_FUNCTION: Moves money - production tax credits, loan guarantees, capacity payments, and rate-base returns on long-lived plant - from taxpayers and electricity customers to owners of large dispatchable generation assets; and moves legitimacy, approving firm-capacity-inclusive portfolios while marking renewable-only proposals inadequate.
% ABSENT_VOICES: Demand-flexibility providers and storage innovators outside the incumbent planning process would contest the adequacy accounting that discounts their resources; climate-vulnerable communities in high-emitting regions would contest the abatement delay the gate licenses. Both are absent from the integrated-resource-plan dockets where the assumptions are fixed.
% DISAPPEARANCE_RATIONALE: Adequacy standards, capacity-market designs, subsidy schedules, and utility capital plans all presuppose the necessity claim; overnight removal would force immediate re-accreditation of capacity, repricing of long-lived assets, renegotiation of subsidy regimes, and replanning of every portfolio awaiting approval.
% FOUNDING_PROBLEM: Mid-twentieth-century demand was inflexible and storage did not exist at grid scale; keeping the lights on required plants that ran continuously, and 'baseload' named that real operating pattern.
% FOUNDING_PROBLEM_CORROBORATION: Independent energy-systems analysts - national-laboratory and peer-reviewed capacity-expansion literature - corroborate that firm capacity retains positive value at high variable-renewable penetration, i.e. the founding reliability problem persists in transformed form. No source outside the benefiting parties attests the categorical half of the claim (that renewables cannot provide the service at scale); attestation for that half comes overwhelmingly from incumbent-utility and nuclear-affiliated engineering voices, and stating that absence is itself the signal.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.65: the doctrine's standing operation routes capital - production tax credits, loan guarantees, capacity payments, rate-base returns on long-lived plant - toward incumbent owners while imposing financing and accreditation penalties on the excluded alternatives; a real adequacy service sits underneath, so the score stops short of pure-rent territory. The referent is the doctrine's standing operation (planning standards plus subsidy architecture plus capacity-market design), not any endorsed alternative pathway. Suppression 0.60: alternatives are not outlawed but are penalized at the accreditation, planning-assumption, and financing layers; suppression is authored as a raw structural property and deliberately left unscaled - the engine applies directionality and scope scaling to extractiveness only. Theater 0.40: a substantial share of doctrinal activity - fuel-security hearings, adequacy studies commissioned to justify existing fleets - performs necessity rather than producing it, alongside genuine reliability engineering. Accessibility_collapse 0.48: renewable-plus-storage pathways remain visible and increasingly demonstrated, so accepting the doctrine narrows what counts as admissible inside planning institutions without extinguishing awareness of alternatives. Resistance 0.62: sustained contestation from the renewable industry, consumer advocates, and independent modelers meets the doctrine in every integrated-resource-plan cycle. All three tracked series share one six-point grid (t=0..15, approximately 2010-2025) so every metric is authored at every examined time point. Suppression_requirement is included because the story specifically traces enforcement intensification - accreditation reform and capacity-market redesign hardening the gate over the period - not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the utility seat the arrangement reads as prudence: someone must keep the lights on, and regulated returns on reliable plant are the agreed price. From the ratepayer and developer seats the same rules operate as a toll gate - costs socialized, alternatives penalized. From the analyst seat it is a categorical claim stretched over a real but narrower engineering result. The engine computes these per-seat differences from the power, exit, and role data. Note on coalition potential: ratepayers and renewable developers individually look weak, but consumer advocacy allied with renewable trade associations constitutes real coalition leverage inside regulatory proceedings, which is why those seats are authored moderate and organized rather than powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive directionality near the beneficiary end, ordered by exit quality: the utilities' arbitrage-grade redeployment option places them lowest; merchant gas generators' mobile exit keeps them low; the nuclear industry's identity_locked status anchors it inside the arrangement even as its fortunes ride on the necessity claim. Declared victims derive high directionality, amplified by exit trapping and scope: climate_vulnerable_populations (trapped, global scope - hardest verification, strongest amplification) sit nearest the full-target end; trapped national ratepayers next; constrained continental developers below them. Grid planning authorities are neither declared beneficiary nor victim; their seat derives from administration rather than receipt. The open question of partial capture at that seat is routed to an omega rather than a directionality override, because overrides key on the power atom and would misapply to the utilities sharing the institutional atom.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure snare would license scrapping adequacy planning wholesale, discarding a real reliability function any decarbonizing grid needs solved. Reading it as pure rope would license uncapped subsidy on the strength of the coordination story alone. The tangled_rope classification holds both halves visible: coordination (adequacy under weather-year stress is a genuine collective-action problem) and extraction (the specific gate channels rents and excludes rivals through the same structure). On the genealogy interview, founding_problem_status=contested combined with disappearance_verdict=world_rearranges produces no dead-mandate flag - correctly, because electrification and data-center load growth revive parts of the founding problem even as the categorical impossibility premise ages. The mismatch consumer should watch the theater_ratio series for proxy substitution rather than expect a zombie signature here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the climate_mitigation_legitimacy kernel; would the renewable_primacy or portfolio_pragmatism sibling readings restructure the beneficiary set and reclassify the same referent arrangement?',
    'Classify the identical referent arrangement under each sibling reading''s beneficiary/victim declarations and compare computed types; observe which planning institutions adopt which reading over successive resource-adequacy reform cycles.',
    'Under the renewable_primacy_reading, nuclear exits the beneficiary set and this doctrine''s exclusion function computes as near-pure suppression of alternatives (snare drift); under the portfolio_pragmatism_reading the necessity claim softens to optimality and the extraction diffuses across a broader portfolio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story instantiates the baseload_necessity_reading; sibling readings are separate constraints with different beneficiary sets and different epsilon.').

omega_variable(
    firm_capacity_physical_core,
    'How much of the doctrine''s content is irreducible engineering (positive marginal value of firm capacity at high variable-renewable penetration) versus constructed privilege (specific technologies, ownership forms, and the categorical impossibility claim)?',
    'Loss-of-load-probability studies across multi-decade weather-year ensembles comparing portfolios with and without each candidate firm resource at matched reliability and cost targets.',
    'If the irreducible core is small, most measured extraction is rent and the story drifts toward snare; if large, a corresponding share of effective extraction is genuine coordination cost and the rope component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_physical_core, empirical, 'Split between the doctrine''s physical regularity core and its constructed privilege layer.').

omega_variable(
    categorical_impossibility_status,
    'Can renewables plus storage, transmission, and demand flexibility actually serve a deeply decarbonized grid at scale, or does the doctrine track a real physical limit?',
    'Demonstrated high-penetration systems, long-duration storage cost curves, and published renewable-heavy capacity-expansion studies audited for hidden slack (curtailment tolerance, land use, interconnection assumptions).',
    'If renewables can serve the reliability function at acceptable cost, the doctrine''s exclusion of renewable-only pathways loses its coordination justification and the story drifts toward snare; if they cannot, the doctrine retains coordination content and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_impossibility_status, empirical, 'Empirical status of the categorical impossibility premise distinguishing this reading from its siblings.').

omega_variable(
    suppression_internalization,
    'Is the suppression of renewable-only pathways structural (accreditation rules, planning assumptions, financing gates) or internalized (investors and developers treating the ''inadequate'' verdict as settled fact)?',
    'Post-reform trajectory: if financing spreads and proposal volumes for renewable-only portfolios fail to recover after accreditation and planning rules are relaxed, the suppression is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure and outlives rule changes; classification consequences persist after the enforcement machinery is dismantled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized mechanism carrying the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t3, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(clim_tr_t3, observed).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t9, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement_basis(clim_tr_t9, observed).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(clim_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t3, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement_basis(clim_be_t3, observed).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t9, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement_basis(clim_be_t9, observed).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(clim_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t3, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement_basis(clim_su_t3, observed).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t9, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 9, 0.59).
narrative_ontology:measurement_basis(clim_su_t9, observed).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(clim_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'reliable decarbonization needs baseload' decomposes at the kernel climate_mitigation_legitimacy into four readings instantiated as separate constraint files: this baseload_necessity_reading, plus the renewable_primacy, portfolio_pragmatism, and degrowth_sufficiency readings. Each carries its own epsilon over the same referent arrangement (reading-indexed values, fixed referent): this reading admits nuclear into the beneficiary set as necessary infrastructure and marks renewable-only pathways inadequate; the sibling readings relocate nuclear to optionality or irrelevance and redistribute victims accordingly. The edges below declare the family links; the upstream/downstream asymmetry runs from this reading's institutional victories (accreditation rules, subsidy gates) into the operating environment of the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
