% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Velocity Primacy Test for Climate Technology Legitimacy
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   In contemporary climate and energy governance, a technology's standing as
 *   a legitimate mitigation instrument is gated by a single admission test:
 *   can it be deployed at scale within the remaining carbon budget's timeline
 *   — the 2030 emissions-milestone and 2050 net-zero horizon. The test is
 *   operative in clean-energy-standard definitions, subsidy eligibility,
 *   taxonomy classification, procurement scoring, and internationally
 *   negotiated contribution accounting. Its operation concentrates subsidy
 *   flows, permitting priority, and reputational standing on fast-deploying
 *   variable renewables and their supply chains; shifts
 *   intermittency-management and grid-upgrade burdens onto system operators
 *   and the ratepayers who fund them; and defines slow-deploying firm
 *   technologies — above all nuclear — out of eligibility regardless of
 *   lifecycle carbon performance. This file instantiates the
 *   velocity_primacy_reading of the technology_legitimacy_kernel; the
 *   reliability_primacy and precautionary readings are separate constraints
 *   linked through the network block. Per the claim/metric independence rule,
 *   claimed_type is asserted from structural analysis while the metrics are
 *   authored as descriptive measurements of the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - energy_ministries_climate_agencies: Agenda setter (institutional/identity_locked) — administers the eligibility rules and enforces the velocity test across subsidy and taxonomy instruments
 *   - solar_wind_developers: Primary beneficiary (organized/mobile) — collects the subsidy flows and portfolio credit the test channels
 *   - renewable_supply_chain_manufacturers: Beneficiary (institutional/arbitrage) — captures the equipment demand generated by velocity-governed buildouts worldwide
 *   - climate_urgency_advocacy_networks: Beneficiary (organized/identity_locked) — gains mobilizing frame and agenda relevance from the countdown
 *   - grid_operators: Primary target (institutional/trapped) — absorbs intermittency-management costs without setting the test
 *   - nuclear_power_industry: Target (organized/trapped) — defined out of eligibility by construction-time failure against the clock
 *   - electricity_ratepayers: Target with partial offsetting benefit (moderate/constrained) — pays integration surcharges while capturing some cheap-generation gains
 *   - firm_clean_energy_startups: Excluded voice (moderate/trapped) — would argue capacity-based admission but sits outside the eligibility conversation
 *   - energy_systems_analysts: Analytical observer (analytical/analytical) — models the full portfolio tradeoff the single test truncates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.62).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity Primacy Test for Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'b361832c-8496-410d-980b-f1b0a90049c4').
narrative_ontology:cs_kernel_codification('b361832c-8496-410d-980b-f1b0a90049c4', formalized).
narrative_ontology:cs_authority_grounding('b361832c-8496-410d-980b-f1b0a90049c4', expertise).
narrative_ontology:cs_interpretation_layer_present('b361832c-8496-410d-980b-f1b0a90049c4').
narrative_ontology:cs_reading_relation('b361832c-8496-410d-980b-f1b0a90049c4', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_reading_relation('b361832c-8496-410d-980b-f1b0a90049c4', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('b361832c-8496-410d-980b-f1b0a90049c4', foundational, temporal_feasibility_confers_legitimacy).
narrative_ontology:cs_axiom_status(temporal_feasibility_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b361832c-8496-410d-980b-f1b0a90049c4', temporal_feasibility_confers_legitimacy, instrumental).
narrative_ontology:cs_axiom('b361832c-8496-410d-980b-f1b0a90049c4', secondary, slow_technology_capital_is_diverted_abatement).
narrative_ontology:cs_axiom_status(slow_technology_capital_is_diverted_abatement, holdable).
narrative_ontology:cs_axiom_grounding('b361832c-8496-410d-980b-f1b0a90049c4', slow_technology_capital_is_diverted_abatement, empirically_contingent).
narrative_ontology:cs_reference_frame('b361832c-8496-410d-980b-f1b0a90049c4', budget_constrained_deployment_feasibility).
narrative_ontology:cs_drift_state('b361832c-8496-410d-980b-f1b0a90049c4', post_2022_energy_security_turn, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b361832c-8496-410d-980b-f1b0a90049c4', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_supply_chain_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_urgency_advocacy_networks).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, deployment_first_prioritization_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, lcoe_cost_decline_trajectory_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and administer the eligibility rules through which public money and regulatory credit reach mitigation technologies: clean-energy-standard definitions, taxonomy classifications, subsidy program criteria, and internationally negotiated contribution accounting. Their programmatic identity and diplomatic standing are built around the 2030/2050 milestone architecture; revisiting the admission test would mean reopening commitments negotiated over a decade. They build nothing themselves; they decide what counts.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, energy_ministries_climate_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Develop utility-scale solar and wind projects whose construction times fit comfortably inside the milestone windows. They receive the subsidy streams, tax credits, and portfolio-standard credit that the admission test channels, and they win procurement rounds designed around speed-to-build. Their pipelines and financing terms assume the current eligibility rules persist; a broadened test would admit competitors they currently out-compete on schedule rather than on lifetime output.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_developers, beneficiary,
    organized, biographical, mobile, global).

% Manufacture modules, turbines, inverters, and balance-of-system equipment. Demand for their products scales with the pace of buildout that the admission test rewards, and they sell globally into whichever jurisdictions adopt fast-deployment frameworks, so their revenue does not depend on any single country's rules. Factory capacity and inventory are tuned to high-velocity orders.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_supply_chain_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Campaign organizations and transnational coalitions whose mobilizing message is that the window is closing and only immediately deployable tools count. The urgency frame organizes their fundraising, membership recruitment, and media presence; volunteers and donors are acquired through deadline-centered appeals. Some member groups experiment with reliability- or justice-centered framings, but the dominant networks are organizationally constituted around the countdown.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_urgency_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).

% Run the transmission system and balance supply and demand in real time. As variable generation's share grows under velocity-governed procurement, they absorb rising curtailment management, ancillary-service procurement, voltage and frequency control, and transmission-upgrade coordination. They operate whatever mix the eligibility rules produce; they cannot decline the intermittency that arrives with it, and their reliability obligations continue regardless. They file resource-adequacy warnings but do not set the admission test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, generational, trapped, regional).

% Operate existing reactors and vendor new builds whose licensing and construction cycles run one to two decades — outside the milestone windows the admission test recognizes. Existing plants earn no portfolio credit under velocity-scored standards despite zero direct emissions during operation; new builds lose subsidy eligibility and political sponsorship at proposal stage. The industry's workforce, supply chain, and regulator relationships are specific to this technology and have no alternative market.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry, payer,
    organized, generational, trapped, global).

% Households and businesses paying electricity bills. They fund the system twice over: through public subsidy budgets allocated by the eligibility rules, and through tariff components that recover grid operators' integration and upgrade costs. Offsetting this, wholesale prices fall in hours of abundant variable output, and some affluent customers self-supply with rooftop solar and batteries. Most cannot leave the grid; their exposure is set by regulators they do not choose.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, beneficiary).

% Venture-backed developers of enhanced geothermal, small modular reactors, and long-duration storage whose products would qualify as low-carbon under lifecycle tests but whose commercialization timelines extend past the milestone windows. They are ineligible for the flagship subsidy programs and absent from portfolio-standard credit, so their revenue paths depend on niche procurement or export markets. They argue for capacity- and attribute-based admission but hold no seat in the eligibility conversation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, firm_clean_energy_startups, excluded,
    moderate, biographical, trapped, global).

% Modelers and researchers at universities, the IEA, and consultancies who compute least-cost decarbonization pathways under alternative admission rules. Their published work shows the tradeoffs the single-test regime truncates — system cost, reliability margins, land use, supply-chain concentration — and their scenario libraries are the raw material policymakers cite selectively. They take no side in allocation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, energy_systems_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_developers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives funders, regulators, and planners a common screening rule for allocating scarce political capital and subsidy budgets under a hard atmospheric deadline: prioritize whatever can actually be built before the budget closes, rather than adjudicating every technology's merits case-by-case.
% TRANSFER_FUNCTION: Moves subsidy flows, permitting priority, clean-energy-standard credit, and reputational standing toward fast-deploying variable renewables and their supply chains; moves intermittency-integration and grid-upgrade costs onto system operators and electricity ratepayers; withdraws eligibility and sponsorship from slow-deploying firm technologies regardless of lifecycle carbon performance.
% ABSENT_VOICES: Firm-power technologists — nuclear engineers, enhanced-geothermal and long-duration-storage developers, reliability engineers — object from outside the eligibility conversation; grid operators sit inside implementation but outside criterion-setting; future generations who inherit either a depleted budget or a brittle grid are present in no seat.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen eligibility to lifecycle-carbon and capacity-based tests: nuclear and firm renewables would regain portfolio standing and subsidy access, storage and firming procurement signals would reprice, interconnection queues would rebalance toward dispatchable resources, and the buildout's composition — not its existence — would reorganize around a multi-criteria legitimacy regime.
% FOUNDING_PROBLEM: Climate policy in the 2000s and 2010s suffered deployment paralysis: technology-neutral R&D rhetoric and perfectionist assessment let 'not yet good enough' displace 'buildable now' while the budget drained. The velocity criterion was built to force near-term deployment discipline — to make time-feasibility, not technological elegance, the admission test.
% FOUNDING_PROBLEM_CORROBORATION: The deadline problem is attested from outside the benefiting parties by carbon-cycle physics and IPCC remaining-budget accounting, which no renewable-industry actor controls; the cost side is attested by grid-operator resource-adequacy filings and reliability-engineering testimony. Corroboration that the criterion still solves the problem rather than functioning as a rent gate is weaker and contested — the strongest independent support is continued budget depletion; the weakest is the criterion's own beneficiaries' testimony.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the deadline is physically real and the prioritization function genuine, so the arrangement is not pure extraction; but it systematically shifts costs onto parties who did not set the test — grid operators absorb intermittency management they cannot decline, ratepayers fund the system through both subsidy budgets and integration tariffs, and firm low-carbon options lose eligibility at proposal stage. Suppression 0.58 is structural rather than personal: eligibility rules, taxonomy boundaries, and procurement scoring starve alternatives without banning anyone; nothing coerces persons, but capital and standing flow only through the velocity gate. Theater_ratio 0.32: the test performs real filtering work (some candidates genuinely cannot build in time), while a growing share of urgency rhetoric — deadline invocations in contexts where marginal allocations do not track the calendar — is performative. Accessibility_collapse 0.55: once the velocity frame is accepted, slow-deploying alternatives collapse out of the decision space almost completely inside it, but the sibling readings keep meta-level alternatives alive, so collapse is incomplete. Resistance 0.62: sustained institutional pushback — resource-adequacy filings, nuclear advocacy, post-2022 capacity-market redesigns, jurisdictional experimentation with firm-capacity credits. requires_active_enforcement is true: the test survives only through continuous maintenance in binding instruments; unenforced, eligibility would drift open. Measurements run on one single shared seven-point grid (t=0..12, mapped approximately 2014-2026); all three tracked series rise monotonically — extraction accumulates as exclusion compounds, enforcement machinery matured through taxonomy and eligibility codification (which is why a suppression_requirement series is authored: the story specifically traces enforcement buildout, not mere extraction drift), and theater grows as urgency rhetoric decouples from marginal decisions. Endpoint values match base_properties.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences disciplined triage: a hard atmospheric deadline forces choosing buildable-now tools over perfect-later ones, and from its own position the arrangement is coordination administered under duress. The payer seats compute differently: grid operators experience an unfunded mandate (obligations fixed, mix imposed), the nuclear industry experiences definitional foreclosure (its product disqualified by a clock, not by carbon arithmetic), and ratepayers experience a bill they did not vote on. Beneficiary seats experience earned reward for schedule performance. The victims also lack coalition potential: their preferred remedies conflict — operators want firm-capacity procurement, nuclear wants eligibility restoration, ratepayers want lower bills — so the cost-shifting persists against fragmented opposition. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: solar and wind developers (mobile exit, organized power) sit near the beneficiary end — the test subsidizes exactly their schedule advantage; supply-chain manufacturers hold arbitrage-grade exit (they sell globally into whichever regimes adopt the test) and sit nearest the beneficiary end of any seat; advocacy networks gain agenda relevance and are identity-locked to the frame. The agenda-setter is identity-locked (its programmatic identity is the milestone architecture) yet institutionally advantaged — a mixed position the engine resolves from power and exit atoms. Victims derive high directionality: grid operators are trapped (they must balance whatever mix arrives) and institutional; the nuclear industry is trapped (no alternative market for its specific capability); ratepayers are constrained payers with a partial offsetting benefit encoded as secondary_role beneficiary — cheap-generation gains against integration surcharges — leaving them partway toward symmetric. Residual ambiguity in the ratepayer net position is routed to an omega rather than a directionality override, because the override surface is keyed by power atom and would misapply to the other moderate-power seat. Suppression is authored as a raw structural property and is not scaled; extractiveness is scaled by the engine from directionality and the national-to-global scopes, which modestly amplify effective extraction through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deployment paralysis against a draining budget — is live, corroborated from outside the benefiting parties by carbon-budget accounting; the status=live x world_rearranges combination produces no zombie flag under the mismatch consumer. The prospective mandatrophy risk is structural: the arrangement's justification is entirely temporal (a closing window), yet has_sunset_clause is authored false because no eligibility framework enacts expiry provisions keyed to the milestone dates. Deadlines have already slipped historically without the criterion sunsetting; if the pattern holds past 2050, the justification lapses while rent flows persist — the classic setup for inertial maintenance. The post_deadline_persistence omega routes this trajectory question to observation rather than pre-adjudicating it. Classification consequence: today the arrangement is a tangled rope (genuine coordination function plus asymmetric extraction requiring active enforcement); the mandatrophy pathway runs through its scaffold-like transitional character toward piton dynamics only if the deadline passes without an enacted sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the technology_legitimacy_kernel: legitimacy conferred solely by deployability-at-scale within the 2030/2050 carbon-budget window. How would the reliability_primacy_reading (legitimacy iff dispatchable baseload capability) or the precautionary_reading (legitimacy iff worst-case failures bounded and reversible within a generation) restructure the beneficiary and victim sets?',
    'Comparative classification of the sibling-reading stories: whichever reading is operative in binding instruments (clean-energy-standard definitions, taxonomy rules, capacity-market design) determines which industries collect subsidy rents and which bear integration or legacy costs. Observe which criterion governs eligibility jurisdiction by jurisdiction.',
    'Under reliability primacy, nuclear enters the beneficiary set and variable renewables become the managed input; under precautionary primacy, legacy-cost-bearing technologies exit legitimacy regardless of speed. The velocity reading''s specific extraction pattern (fast-deployer rents, grid-operator cost shift) holds only while velocity is the operative test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the technology-legitimacy kernel is operative.').

omega_variable(
    deadline_physical_vs_constructed,
    'How much of the criterion''s forcing force comes from the physical remaining carbon budget, and how much from the constructed milestone years (2030/2050) and the deploy-or-it-does-not-count cutoff built around them?',
    'Compare criterion behavior under alternative target architectures: cumulative-emissions accounting without milestone years versus the current milestone-bound framework. Carbon-cycle budget arithmetic is physical; the specific target years and the admission cutoff are negotiated conventions.',
    'If the physical budget does the work, the criterion''s exclusions approach natural-constraint status and carry mountain-like immunity from revision; if the milestone years do the work, the exclusions are policy choices and the constraint is fully constructed and revisable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadline_physical_vs_constructed, conceptual, 'Whether the deadline disciplining the criterion is physical arithmetic or constructed policy.').

omega_variable(
    integration_cost_attribution,
    'How much of the intermittency-management burden on grid operators and ratepayers is intrinsic to variable-renewable physics, and how much is an artifact of the velocity criterion steering investment away from the firming and storage that would internalize those costs?',
    'Counterfactual portfolio modeling: simulate system costs under velocity-governed buildout versus a portfolio that co-procures firm capacity at each penetration level; compare realized integration-cost trajectories in high-variable-renewable jurisdictions against co-optimized benchmarks.',
    'If costs are intrinsic, the grid operators'' burden reflects physics and their victim status weakens toward managed-transition friction; if artifact, the criterion actively externalizes costs it could internalize, strengthening the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_cost_attribution, empirical, 'Attribution of intermittency costs between physics and criterion-induced underinvestment in firming.').

omega_variable(
    nuclear_exclusion_counterfactual,
    'Would nuclear power have achieved meaningful scale within the budget window absent the velocity criterion''s marginalization, or does its exclusion track genuine construction-time and cost facts?',
    'Cross-jurisdiction comparison of nuclear delivery times and costs under differing legitimacy regimes (jurisdictions that weighted firm low-carbon capacity versus velocity-governed ones), controlling for supply-chain and regulatory variables.',
    'If nuclear was deployable-but-starved, its victim status is imposed by the criterion and the extraction reading strengthens; if it was undeployable in-window regardless, the criterion merely registered a fact and nuclear''s exclusion carries little extraction content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_exclusion_counterfactual, empirical, 'Whether nuclear''s marginalization is criterion-imposed or fact-tracking.').

omega_variable(
    post_deadline_persistence,
    'When the budget window closes or the milestone years pass, does the velocity criterion sunset with its justification, or persist as institutional inertia governing a changed problem?',
    'Observe eligibility-framework behavior at and after milestone dates: whether sunset provisions are enacted, whether the criterion migrates to new deadlines, or whether it continues allocating rents after its temporal justification lapses.',
    'Persistence past the deadline would convert the arrangement''s transitional character into inertial maintenance, shifting the classification trajectory toward piton dynamics and confirming mandatrophy; orderly sunset would vindicate the coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_deadline_persistence, conceptual, 'Post-2050 trajectory of a deadline-justified criterion with no enacted sunset clause.').

omega_variable(
    urgency_frame_identity_lock,
    'Is the urgency frame (there is no time for alternatives) load-bearing for the advocacy coalition''s cohesion such that member organizations cannot evaluate the criterion independently of their organizational identity?',
    'Track coalition behavior when velocity claims and outcome claims diverge: if members defend the criterion when shown equal-emissions slower portfolios, identity fusion is operative; if they update, the frame is instrumental.',
    'Identity fusion would raise effective suppression above the structural measure (members enforce the frame internally) and entrench the criterion against evidence-driven revision; instrumental use would leave the criterion ordinarily revisable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urgency_frame_identity_lock, conceptual, 'Whether advocacy-coalition commitment to the velocity frame is identity-fused or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(velocity_primacy_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(velocity_primacy_tr_t0, observed).
narrative_ontology:measurement(velocity_primacy_tr_t2, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(velocity_primacy_tr_t2, observed).
narrative_ontology:measurement(velocity_primacy_tr_t4, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(velocity_primacy_tr_t4, observed).
narrative_ontology:measurement(velocity_primacy_tr_t6, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(velocity_primacy_tr_t6, observed).
narrative_ontology:measurement(velocity_primacy_tr_t8, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(velocity_primacy_tr_t8, observed).
narrative_ontology:measurement(velocity_primacy_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(velocity_primacy_tr_t10, observed).
narrative_ontology:measurement(velocity_primacy_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(velocity_primacy_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(velocity_primacy_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(velocity_primacy_be_t0, observed).
narrative_ontology:measurement(velocity_primacy_be_t2, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(velocity_primacy_be_t2, observed).
narrative_ontology:measurement(velocity_primacy_be_t4, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(velocity_primacy_be_t4, observed).
narrative_ontology:measurement(velocity_primacy_be_t6, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(velocity_primacy_be_t6, observed).
narrative_ontology:measurement(velocity_primacy_be_t8, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement_basis(velocity_primacy_be_t8, observed).
narrative_ontology:measurement(velocity_primacy_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(velocity_primacy_be_t10, observed).
narrative_ontology:measurement(velocity_primacy_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(velocity_primacy_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(velocity_primacy_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(velocity_primacy_su_t0, observed).
narrative_ontology:measurement(velocity_primacy_su_t2, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2, 0.44).
narrative_ontology:measurement_basis(velocity_primacy_su_t2, observed).
narrative_ontology:measurement(velocity_primacy_su_t4, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(velocity_primacy_su_t4, observed).
narrative_ontology:measurement(velocity_primacy_su_t6, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 6, 0.51).
narrative_ontology:measurement_basis(velocity_primacy_su_t6, observed).
narrative_ontology:measurement(velocity_primacy_su_t8, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(velocity_primacy_su_t8, observed).
narrative_ontology:measurement(velocity_primacy_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(velocity_primacy_su_t10, observed).
narrative_ontology:measurement(velocity_primacy_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(velocity_primacy_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'technology legitimacy for climate mitigation' decomposes into three structurally distinct constraints, one per reading of the technology_legitimacy_kernel: velocity primacy (this file), reliability primacy, and precautionary primacy. Each has its own epsilon, beneficiary/victim sets, and classification; forcing one story to span all three would violate epsilon-invariance, since measuring legitimacy by deployment speed, by dispatchability, or by failure-mode reversibility yields different extraction profiles over the same technology landscape. The velocity reading influences the reliability reading's operating environment (velocity-governed variable-renewable buildout changes the grid conditions reliability governance must manage) and coexists with the precautionary reading as rival orderings held by different factions of the same dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
