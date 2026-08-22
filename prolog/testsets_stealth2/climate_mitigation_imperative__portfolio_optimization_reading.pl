% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Portfolio-Optimization Reading of the Mitigation Imperative: Maximize All Low-Carbon Sources, Nuclear-Inclusive Baseload Mandate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the portfolio-based mitigation
 *   architecture: technology-neutral clean-electricity standards,
 *   carbon-intensity accounting, and a subsidy and capacity-payment apparatus
 *   premised on the claim that all low-carbon sources must be maximized and
 *   that firm (nuclear-inclusive) baseload is necessary for reliability. The
 *   arrangement performs genuine coordination — portfolio decarbonization,
 *   reliability hedging, prevention of technology-battle paralysis — while
 *   simultaneously channeling concentrated support to nuclear operators and
 *   vendors and imposing compliance costs and retirement schedules on fossil
 *   generation, with ratepayers funding the premium. KEY AGENTS (by
 *   structural relationship): nuclear_utilities_and_vendors: primary
 *   beneficiary (institutional/identity_locked) — collects subsidy and
 *   capacity revenue streams; federal_state_legislators_and_regulators:
 *   agenda setter (institutional/mobile) — writes standards, appropriates
 *   support, grants licenses; fossil_fuel_generators: primary target
 *   (powerful/constrained) — bears compliance costs and forced retirements;
 *   fossil_fuel_workers_and_communities: concentrated cost bearer
 *   (powerless/trapped); electricity_ratepayers: dual-positioned cost bearer
 *   and incidental beneficiary (moderate/constrained); wind_solar_developers:
 *   secondary beneficiary (organized/mobile); grid_operators: operational
 *   beneficiary (institutional/constrained);
 *   full_renewables_pathway_advocates: excluded voice (moderate/trapped);
 *   independent_energy_systems_analysts: analytical observer. This file
 *   instantiates ONE reading of the climate_mitigation_imperative kernel (see
 *   kernel_context); the sibling readings are separate constraint stories
 *   linked via network.affects_constraints, and nothing from them is averaged
 *   into this file's epsilon. Epsilon's referent is the standing
 *   portfolio-based arrangement as this reading's own lights assess it —
 *   moderately extractive: real coordination carrying real, asymmetrically
 *   distributed transfers. The claimed type and the metrics are authored
 *   independently: I claim tangled_rope because the structure shows both a
 *   genuine coordination function and asymmetric extraction requiring active
 *   enforcement; the metrics record what I take to be descriptively true of
 *   its operation.
 *
 * KEY AGENTS:
 *   - nuclear_utilities_and_vendors: primary beneficiary (institutional/identity_locked) — collects subsidy, credit, and capacity revenue streams justified by the all-sources imperative
 *   - federal_state_legislators_and_regulators: agenda setter (institutional/mobile) — defines compliant technologies, sets support levels, grants licenses
 *   - fossil_fuel_generators: primary target (powerful/constrained) — bears tightening compliance costs and scheduled retirements on sunk site-specific capital
 *   - fossil_fuel_workers_and_communities: concentrated cost bearer (powerless/trapped) — local economies absorb closures set elsewhere
 *   - electricity_ratepayers: dual-positioned (moderate/constrained) — fund the support premium and receive the decarbonized, reliable product
 *   - wind_solar_developers: secondary beneficiary (organized/mobile) — revenue rides on compliance eligibility; capital relocates freely
 *   - grid_operators: operational beneficiary (institutional/constrained) — procure firm capacity; favor dispatchable low-carbon units
 *   - full_renewables_pathway_advocates: excluded voice (moderate/trapped) — storage-and-flexibility-first proposals ruled out of the planning frame
 *   - independent_energy_systems_analysts: analytical observer (analytical/analytical) — model alternative portfolios without revenue dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio-Optimization Reading of the Mitigation Imperative: Maximize All Low-Carbon Sources, Nuclear-Inclusive Baseload Mandate").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'e9fba4e3-ae7e-441a-ac40-cc85daee93f5').
narrative_ontology:cs_kernel_codification('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', formalized).
narrative_ontology:cs_authority_grounding('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', expertise).
narrative_ontology:cs_interpretation_layer_present('e9fba4e3-ae7e-441a-ac40-cc85daee93f5').
narrative_ontology:cs_reading_relation('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', foundational, firm_low_carbon_capacity_is_mitigation_necessary).
narrative_ontology:cs_axiom_status(firm_low_carbon_capacity_is_mitigation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', firm_low_carbon_capacity_is_mitigation_necessary, empirically_contingent).
narrative_ontology:cs_axiom('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', foundational, carbon_intensity_neutrality_across_technologies).
narrative_ontology:cs_axiom_status(carbon_intensity_neutrality_across_technologies, holdable).
narrative_ontology:cs_axiom_grounding('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', carbon_intensity_neutrality_across_technologies, instrumental).
narrative_ontology:cs_reference_frame('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', technology_neutral_portfolio_standard).
narrative_ontology:cs_drift_state('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', contemporary_post_cost_decline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9fba4e3-ae7e-441a-ac40-cc85daee93f5', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_utilities_and_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, wind_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_generators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_workers_and_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing reactors and sell reactor designs, fuel, and maintenance services. Receive production tax credits, capacity-market revenues, and state zero-emission credit programs justified by the all-sources-maximized imperative; business plans and balance sheets are built around multi-decade operating licenses. Exiting would mean writing off licensed assets and abandoning the only market their specialized engineering workforce serves.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_utilities_and_vendors, beneficiary,
    institutional, generational, identity_locked, global).

% Write clean-electricity standards, appropriate support programs, and grant operating licenses; they decide which technologies count toward compliance and at what support level. Electoral cycles and constituent pressure shape which portfolio weights they defend, and they retain the authority to amend or repeal the standards they enacted.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, federal_state_legislators_and_regulators, agenda_setter,
    institutional, biographical, mobile, national).

% Own coal and gas fleets whose output is priced out of compliance markets as carbon-intensity standards tighten. They pay compliance costs, lose dispatch hours, and face scheduled retirement dates. Capital is sunk in site-specific plants and fuel contracts; repurposing or divesting takes years and recovers only part of book value.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_generators, payer,
    powerful, biographical, constrained, continental).

% Live in towns whose employment, tax base, and local services depend on generating stations slated for closure under the standards. Skills are plant-specific, housing wealth is local, and relocation means leaving family networks; transition assistance arrives late and partial, and the closure schedule is set in distant capitals.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_workers_and_communities, payer,
    powerless, generational, trapped, regional).

% Pay retail rates that bundle the cost of subsidy programs, capacity payments, and grid upgrades required by the portfolio buildout; they also receive cleaner air and a progressively decarbonized, reliable supply. Leaving the bundled system is realistic only for affluent households able to finance standalone generation and storage.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, beneficiary).

% Build utility-scale renewable projects whose revenue depends on standards and credits counting their output toward compliance. Capital moves freely between projects and jurisdictions, so development pipelines shift toward whichever markets currently offer the richest support.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, wind_solar_developers, beneficiary,
    organized, biographical, mobile, global).

% Run wholesale markets and reliability planning; they procure whatever firm capacity resource-adequacy rules require and favor portfolios containing dispatchable low-carbon units because those simplify balancing. They cannot relocate their service territories and must operate whatever mix policy delivers.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, generational, constrained, continental).

% Research groups and campaign organizations arguing that storage, transmission expansion, and demand flexibility can replace firm baseload entirely. Their proposals are received as engineering naivety inside planning processes that assume firm capacity, and they hold no vote in standard-setting committees; their remaining moves are publishing outside analyses and seeking intervenor status in proceedings.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, full_renewables_pathway_advocates, excluded,
    moderate, generational, trapped, global).

% Academic and consultancy modelers who run capacity-expansion and reliability studies across alternative portfolio assumptions, publish cost and adequacy comparisons, and advise both sides of the technology dispute. Their seat carries no compliance obligations and no revenue dependence on any particular technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, independent_energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_utilities_and_vendors).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of decarbonizing shared electricity systems without sacrificing reliability: coordinates investment across complementary generation technologies so that variable renewables, firm low-carbon capacity, and transmission expand together rather than competing for policy support and cannibalizing each other's revenue.
% TRANSFER_FUNCTION: Moves fiscal support, guaranteed revenue streams (tax credits, capacity payments, cost-recovery mechanisms), and regulatory accommodation from taxpayers and electricity consumers toward qualifying low-carbon generators — disproportionately toward capital-intensive firm resources such as nuclear — while imposing compliance costs and retirement schedules on fossil-fuel generation.
% ABSENT_VOICES: Full-renewables/storage-first pathway advocates are outside the portfolio consensus: their claim that flexibility and storage can substitute for firm baseload is treated inside planning processes as a category error rather than a competing hypothesis, and they sit outside the modeling teams and standard-setting committees that define which portfolios count as feasible. Fossil-fuel workers and communities bear the transition's concentrated local costs without seats at the table where closure schedules are written.
% DISAPPEARANCE_RATIONALE: If the portfolio mandate and its support apparatus vanished overnight, subsidy and capacity-revenue flows to nuclear would cease and merchant retirements would accelerate; fossil units would re-enter wherever energy-only prices clear; compliance-driven renewable buildout would stall pending new instruments; and reliability planning would reorganize around whatever mix remained. Utility financing structures, state compliance filings, and international pledge accounting all currently depend on the arrangement continuing.
% FOUNDING_PROBLEM: Early climate policy faced a sequencing dilemma: variable renewables could not yet carry grids alone, single-technology mandates repeatedly failed politically, and technology battles among low-carbon camps stalled mitigation altogether. The portfolio imperative was constructed to keep all low-carbon options funded simultaneously, hedge against any single technology failing, and prevent internecine fights from freezing decarbonization policy.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties, split across the two halves of the founding problem: IPCC assessment reports and independent continental grid-operator seasonal adequacy reviews attest that firm low-carbon capacity remains a live engineering requirement at high variable-renewable penetration, while independent levelized-cost literature attests that the support premium paid to firm nuclear exceeds any residual engineering necessity. Neither source set includes the arrangement's direct beneficiaries; the beneficiaries themselves attest only that the problem is fully live.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement blends a genuine Pigouvian component (pricing fossil carbon is corrective, not extractive) with a rent component (the nuclear-support premium exceeds what cost-effectiveness or residual engineering necessity alone would justify, and capacity-payment structures guarantee revenue regardless of merit-order outcomes). Suppression 0.52: persistence depends on binding standards, mandated retirement schedules, and compliance definitions that exclude non-qualifying pathways from credit — coercive force is real but stops short of foreclosing all alternatives, since jurisdictions retain instrument choice and portfolio weights. Theater ratio 0.32: the coordination core is functional (actual decarbonization and reliability procurement occur), but a growing share of activity is rhetorical maintenance of 'technology neutrality' — invoked selectively to defend specific support streams rather than applied symmetrically. Accessibility collapse 0.42: once the baseload-necessity premise is granted, storage-first and demand-flexibility-first alternatives collapse substantially inside official planning, yet workable alternative mixes and instruments persist in reality and in the literature. Resistance 0.60: fossil owners litigate and lobby intensively, ratepayer advocates contest cost premiums in proceedings, and parts of the environmental movement fight nuclear eligibility — the arrangement must be actively defended. Temporal design: one shared grid (t=0..30, mapping approximately 1995..2025) carries all three tracked metrics at all six points, so no metric is sampled against another's scalar. The suppression_requirement series is authored deliberately: enforcement capacity demonstrably intensified over the interval (voluntary renewable-portfolio targets hardened into binding clean-energy standards with nuclear eligibility, then into statutory credit apparatuses defended through litigation), which is an enforcement-infrastructure story, not merely an extraction shift. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the agenda-setter seat the arrangement is a hedge-rational portfolio policy it designed and can amend; from the fossil-generator seat it is a tightening squeeze on sunk capital administered by rivals' regulators; from the community seat it is a closure schedule written by strangers; from the nuclear seat it is overdue recognition of an indispensable resource; from the excluded pathway advocates' seat it is a planning orthodoxy that renders their alternative unthinkable before it is tested. The ratepayer seat splits internally: households experience the premium monthly while receiving the decarbonized product continuously. Powerlessness here is fragmented rather than coalition-proof: fossil workers and communities could in principle form just-transition coalitions with ratepayer advocates, but geography, timing, and union-vs-consumer framing have kept that coalition latent — the engine should read the powerless seats' effective power as below their nominal coalition potential. Same-level divergence: fossil generators and wind_solar_developers both hold organized-to-powerful standing, yet exit differs sharply — developer capital is mobile across jurisdictions while generator capital is welded to plant sites and fuel contracts, so identical formal standing yields different exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: nuclear_utilities_and_vendors sit nearest the beneficiary pole (d near 0.05) — concentrated receipts, identity-locked exit, generational horizon; wind_solar_developers sit low but their arbitrage-grade mobility tempers the subsidy they can capture per jurisdiction; grid_operators sit low-moderate, benefiting from simplified balancing rather than direct transfers. Victim declarations drive high directionality: fossil_fuel_workers_and_communities sit nearest the full-target pole (trapped, powerless, bearing diffuse-but-total local costs); fossil_fuel_generators sit high despite organizational power because asset specificity traps them — power without exit does not purchase relief here. Electricity_ratepayers are the deliberate dual case: listed among victims for the premium they fund, holding a secondary beneficiary role for the decarbonized reliable supply they receive; their true position sits nearer symmetric than a pure-victim reading would suggest, and the dual-role declaration encodes that so the engine does not over-amplify their chi. Scope amplification applies through the stakeholder scopes (regional to global): verification of whether support tracks genuine system value is hardest at the global vendor tier, where the largest premiums flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (technology-battle paralysis plus early variable-renewable inadequacy) is contested rather than dead: the reliability half is corroborated as live by independent adequacy assessments, while the cost-effectiveness half is corroborated as substantially solved by independent cost-curve literature. Reading status=contested against disappearance_verdict=world_rearranges produces no dead-mandate zombie flag, and theater_ratio 0.32 stays well short of performative-dominance territory — this is not a piton wearing a coordination costume. The classification guards against mislabeling in both directions: reading the arrangement as pure extraction erases the genuine collective-action function (reliability at high renewable penetration is a real engineering constraint, not a cover story), while reading it as pure coordination erases the concentrated nuclear rent capture and the foreclosure of storage-first alternatives that the same structure accomplishes. The tangled_rope verdict is reading-indexed: it holds for the portfolio_optimization_reading's instantiation and would not survive translation into the sibling readings' frames (see the kernel_reading_contest omega).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (portfolio_optimization_reading) of the climate_mitigation_imperative kernel. Does instantiating a sibling reading change the structural classification?',
    'Author and compile the sibling stories (climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading) and compare computed per-seat classifications; the cross-reading delta is the kernel''s contest made measurable.',
    'Under the opportunity_cost_reading, nuclear leaves the beneficiary set and becomes a net target — its support stream reads as misallocation and the measured extraction on the support component rises sharply. Under the systems_transition_reading, the target set expands to host communities and the coordination function itself is denied, pushing the classification toward snare-flavored. This file''s tangled_rope verdict holds only within this reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three live readings of a contested kernel; classification is reading-indexed, not topic-indexed.').

omega_variable(
    baseload_necessity_empirical_status,
    'Is firm low-carbon capacity genuinely necessary for reliability at high variable-renewable penetration, or is that necessity a transient artifact of current storage and long-duration-storage costs?',
    'Deployment and adequacy data from high-penetration grids (interconnection-level seasonal adequacy studies, South Australia and Danish-type systems, long-duration storage cost trajectories) tracked against the penetration levels at which firm capacity ceases to bind.',
    'If the necessity is transient, the foundational axiom loses its empirical grounding as storage costs fall, and the nuclear-support component reclassifies toward rent capture riding a fading engineering claim; if durable, the coordination framing strengthens and part of the measured extraction is the price of the reliability function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_status, empirical, 'Whether the reading''s foundational necessity claim is an enduring engineering fact or a cost-era artifact.').

omega_variable(
    nuclear_support_incidence,
    'Who actually captures the nuclear-support premium — operating companies, reactor vendors and their supply chains, construction labor, or financiers via guaranteed-return structures?',
    'Audit of credit, capacity-payment, and cost-recovery flows against levelized-cost gaps and contract structures across recipient tiers.',
    'Determines the receipt surface: concentration in a single vendor-operator tier supports a captured-extraction reading; diffusion across labor and supply chains supports a broader industrial-policy reading and weakens any single-seat capture verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_support_incidence, empirical, 'Incidence of the support premium across recipient tiers.').

omega_variable(
    fossil_burden_composition,
    'Is the burden on fossil generation primarily Pigouvian correction (internalizing the climate externality) or strategic exclusion that protects subsidized incumbents behind compliance definitions?',
    'Compare the fossil burden imposed by this arrangement against the burden a first-best externality price would impose at equal abatement; decompose the difference into corrective versus protective components using compliance-definition analysis.',
    'If predominantly corrective, a large share of apparent extraction is legitimate externality pricing and epsilon''s extractive component shrinks; if predominantly protective, the arrangement''s enforcement machinery is defending a transfer structure and the snare-leaning components strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_burden_composition, conceptual, 'Composition of the fossil-sector burden: externality correction versus incumbent protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the climate mitigation imperative.' The natural-language concept covers three structurally distinct claims that share a kernel (decarbonization is obligatory) but instantiate different constraints with different epsilon values, beneficiary/victim structures, and classifications: this portfolio_optimization_reading (technology-neutral carbon intensity; nuclear in the beneficiary set; tangled_rope-shaped), the opportunity_cost_reading (cost-per-dollar supremacy; nuclear as net target), and the systems_transition_reading (governance-form criterion; nuclear and centralization as targets). Each story carries its own stable epsilon per the epsilon-invariance principle; they are linked here because the upstream scientific-assessment layer of the kernel is cited as evidence by all three, so contamination propagates along these edges. This file links to both siblings; each sibling should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
