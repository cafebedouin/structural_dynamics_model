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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Velocity Primacy Reading — Deployability-within-Carbon-Budget Legitimacy Gate
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the technology-legitimacy kernel:
 *   the velocity primacy reading, under which a mitigation technology is
 *   legitimate if and only if it can be deployed at scale within the
 *   remaining carbon budget window. Per the epsilon-invariance discipline,
 *   this file contains only this reading as a clean, single-epsilon
 *   constraint; the reliability primacy and precautionary readings are
 *   separate constraints with their own epsilon values, beneficiary/victim
 *   structures, and classifications, linked through the network section. The
 *   epsilon referent is the standing arrangement under contest — the
 *   deployability screen as it actually operates across scenario modeling,
 *   green-taxonomy administration, lender policy, and national plan drafting
 *   — assessed by this reading's own evaluative lights. Because this reading
 *   endorses the criterion, its own-lights assessment still registers the
 *   arrangement's real costs: the reading regards the exclusions as
 *   substantially justified by budget arithmetic while acknowledging that the
 *   screen externalizes integration costs and forecloses firm-low-carbon
 *   options whose absence the endgame will feel. The claim/metric split is
 *   deliberate: the structural claim is tangled_rope (a genuine time-scarcity
 *   coordination function fused with asymmetric exclusion and unpriced
 *   cost-shifting), while the metrics are authored independently as
 *   descriptive truths about how the screen currently operates.
 *
 * KEY AGENTS:
 *   - - renewable_energy_industries: Primary beneficiary (organized/mobile) — product lines pass the screen by construction; gains market share and policy priority
 *   - - climate_advocacy_organizations: Secondary beneficiary (moderate/identity_locked) — strategic identity fused with the velocity pathway; polices the screen in coalition fora
 *   - - renewable_infrastructure_investors: Secondary beneficiary (powerful/arbitrage) — captures bankability advantages; exits by moving capital, not by defending the screen
 *   - - intergovernmental_scenario_bodies: Agenda setter (institutional/identity_locked) — operationalizes the screen in pathways and lead-time tables; methodological continuity is its authority
 *   - - national_energy_ministries: Agenda setter (institutional/mobile) — writes the domestic rules implementing the screen; adjusts at margins under coalition cost
 *   - - nuclear_power_industry: Primary payer (organized/constrained) — disqualified on lead time regardless of lifetime carbon performance; loses finance eligibility and policy slots
 *   - - grid_operators: Primary payer (institutional/trapped) — absorbs rising integration, reserve, and stability costs of the selected portfolio; cannot exit the balancing obligation
 *   - - electricity_ratepayers: Payer with incidental benefit (powerless/trapped) — pays bundled integration charges; no exit and no seat
 *   - - developing_country_energy_planners: Excluded voice (moderate/trapped) — needs firm financeable capacity; objections surface only bilaterally
 *   - - independent_energy_systems_analysts: Analytical observer (analytical/analytical) — publishes cross-cutting lead-time and system-cost evidence; no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity Primacy Reading — Deployability-within-Carbon-Budget Legitimacy Gate").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'fe052ec2-0483-49eb-8cf0-49eea33d2958').
narrative_ontology:cs_kernel_codification('fe052ec2-0483-49eb-8cf0-49eea33d2958', distributed).
narrative_ontology:cs_authority_grounding('fe052ec2-0483-49eb-8cf0-49eea33d2958', expertise).
narrative_ontology:cs_interpretation_layer_present('fe052ec2-0483-49eb-8cf0-49eea33d2958').
narrative_ontology:cs_reading_relation('fe052ec2-0483-49eb-8cf0-49eea33d2958', technology_legitimacy_kernel__reliability_primacy_reading, influences).
narrative_ontology:cs_reading_relation('fe052ec2-0483-49eb-8cf0-49eea33d2958', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('fe052ec2-0483-49eb-8cf0-49eea33d2958', foundational, timely_scale_deployability_legitimizes).
narrative_ontology:cs_axiom_status(timely_scale_deployability_legitimizes, holdable).
narrative_ontology:cs_axiom_grounding('fe052ec2-0483-49eb-8cf0-49eea33d2958', timely_scale_deployability_legitimizes, empirically_contingent).
narrative_ontology:cs_axiom('fe052ec2-0483-49eb-8cf0-49eea33d2958', secondary, cumulative_emissions_opportunity_cost_dominates).
narrative_ontology:cs_axiom_status(cumulative_emissions_opportunity_cost_dominates, holdable).
narrative_ontology:cs_axiom_grounding('fe052ec2-0483-49eb-8cf0-49eea33d2958', cumulative_emissions_opportunity_cost_dominates, empirically_contingent).
narrative_ontology:cs_reference_frame('fe052ec2-0483-49eb-8cf0-49eea33d2958', budget_deadline_deployability_screen).
narrative_ontology:cs_drift_state('fe052ec2-0483-49eb-8cf0-49eea33d2958', post_2022_energy_security_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe052ec2-0483-49eb-8cf0-49eea33d2958', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_infrastructure_investors).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture and deploy solar, wind, and storage at gigawatt scale. Every strengthening of the deployability screen enlarges their addressable market, subsidy access, and permitting priority, because their product lines pass the screen by construction. Exit would mean redeploying supply chains and sales organizations toward adjacent sectors, which is costly but feasible.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_industries, beneficiary,
    organized, biographical, mobile, global).

% Campaign for rapid renewables buildout and staff the coalitions that police which technologies appear in credible transition plans. The deployability criterion anchors their theory of change, their fundraising appeals, and their alliances with industry. Abandoning the criterion would dissolve a strategic identity built over two decades, so exit is unthinkable from inside the organizational self-concept even where individual analysts harbor doubts.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_organizations, beneficiary,
    moderate, generational, identity_locked, global).

% Allocate pension, sovereign, and private capital to projects whose bankability depends on policy frameworks the deployability screen shapes. The screen steers eligible-universe definitions, taxonomy labels, and lender policies toward their asset class. Capital moves across jurisdictions and asset classes quickly, so their position is protected by optionality rather than loyalty.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_infrastructure_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Produce the net-zero pathways, technology lead-time tables, and model ensembles that operationalize the deployability test for ministries, lenders, and treaty reviews. Deployment-rate screening is embedded in their published methodology; revising it would unsettle prior scenarios, invite accusations of inconsistency, and erode the authority that rests on methodological continuity. The institution has become its pathway apparatus.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, intergovernmental_scenario_bodies, agenda_setter,
    institutional, generational, identity_locked, global).

% Write portfolio standards, auction designs, green-taxonomy positions, and export-credit rules that implement the deployability criterion in domestic law. Electoral turnover lets a ministry shift criteria, but doing so carries coalition costs with advocacy partners and lenders, so most adjust at the margins rather than replace the screen.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, national_energy_ministries, agenda_setter,
    institutional, biographical, mobile, national).

% Operates the existing low-carbon fleet and develops replacement reactors whose construction lead times run ten to twenty years in most Western jurisdictions. The deployability screen disqualifies new builds regardless of lifetime emissions performance, costing the industry green-finance eligibility, policy slots in national plans, and a shrinking talent pipeline. Assets are site-specific and the workforce's skills do not transfer cleanly, so exit means writing down sunk capital and dispersing accumulated expertise.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_power_industry, payer,
    organized, biographical, constrained, global).

% Must keep the system balanced on whatever portfolio the deployability screen selects. As the variable-generation share rises, they procure reserves, manage curtailment, and maintain stability services whose costs grow faster than the screened technologies' headline prices. They cannot decline the obligation to keep supply matching demand, and the integration costs they absorb are rarely priced back into the legitimacy metric that chose the portfolio.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, trapped, continental).

% Pay bills that bundle inexpensive variable generation with mounting network, balancing, and backup charges. They receive cleaner air and falling wholesale energy prices alongside the integration surcharge, and they have no practical option to leave the grid or influence the technology screen that shapes their bill composition.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, electricity_ratepayers, beneficiary).

% Need firm, financeable capacity to power industrialization on short schedules. OECD-led scenario consensus narrows their concessional-finance menu toward variable renewables plus imported firm fuels, and their objections surface only in bilateral negotiations rather than in the fora where the screen is maintained. They would argue for technology-neutral eligibility tied to delivered firm energy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, developing_country_energy_planners, excluded,
    moderate, generational, trapped, national).

% Publish lead-time audits, system-cost decompositions, and reliability studies that cut across the screen's verdicts in both directions. They hold no enforcement power and no revenue stake; their dissent circulates into academic debate and occasionally into ministry testimony, feeding rival framings of technology legitimacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, independent_energy_systems_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_industries).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real scarcity-allocation problem: with a finite remaining carbon budget, capital, political attention, permitting bandwidth, and concessional finance must flow first to technologies that deliver cumulative emissions reductions before the budget closes. The deployability screen ranks technologies by scalable abatement within the 2030/2050 window and converts abstract temperature targets into near-term procurement queues.
% TRANSFER_FUNCTION: Moves legitimacy, capital eligibility, policy priority, and regulatory fast-tracking toward fast-deploying technologies; moves exclusion toward slow ones in the form of lost finance access, omitted policy slots, and reputational marginalization. Separately, it transfers unpriced system-integration work onto grid operators and bill-composition changes onto ratepayers.
% ABSENT_VOICES: Developing-country energy planners who need firm financeable capacity are outside the scenario fora where the screen is maintained; grid reliability engineers hold minority seats in pathway advisory groups; nuclear-experienced climate scientists publish objections that rarely enter plan annexes; future populations exposed to both residual emissions and system-failure risk have no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: If the deployability screen vanished overnight, green-taxonomy eligibility, lender policies, and national plan technology lists would reorganize around whichever competing legitimacy test each jurisdiction favored; several nuclear pipelines and firm-low-carbon programs would reopen within a planning cycle; renewable subsidy design would lose its current priority ordering; and the advocacy-finance-industry coalition built around velocity would fracture and regroup. Named parties exist and standing arrangements depend on the screen, so the world rearranges.
% FOUNDING_PROBLEM: Early-2010s climate planning leaned heavily on technologies perpetually twenty years from deployment — fusion, Generation IV reactors, large-scale capture — allowing governments to pledge deep cuts while deferring procurement indefinitely. The velocity test was forged to force transition plans to rest only on what could physically be built within the budget window, converting distant targets into near-term orders.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: intergovernmental assessment literature repeatedly documents the gap between pledged targets and implementable pathways; the historical record shows serially cancelled or decades-delayed demonstration projects across capture and advanced fission; and adversely positioned nuclear-sector analysts concede the historical deferral problem while disputing the test's current application. No major party claims the deferral problem never existed; the dispute is over whether it remains the binding constraint today.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58: the screen redirects finance eligibility, policy slots, and reputational standing away from entire technology classes on a single criterion, and it shifts unpriced integration costs onto parties who did not choose the portfolio — but the criterion answers to a real scarcity (cumulative budget arithmetic), which bounds how much of the measured burden is rent rather than coordination cost. Suppression is authored at 0.62 as a raw structural property, unscaled by power or scope: exclusion operates through taxonomy definitions, lender policies, model gatekeeping, and coalition policing rather than legal prohibition, leaving alternatives formally available but systematically disadvantaged. Theater ratio is 0.30: deployment-speed analysis is genuinely functional, but a growing share of deadline rhetoric serves fundraising and coalition mobilization beyond what deployment planning consumes. Accessibility collapse is 0.45 — once the criterion is accepted, alternatives to it collapse within the framework, yet the framework itself remains openly contested by two live sibling readings, so collapse is partial. Resistance is 0.55: sustained pushback from nuclear-aligned analysts, reliability engineers, and firm-power-seeking importers keeps the screen's margins under pressure. The temporal series run on one shared six-point grid (2015-2025, biennial) with all three tracked metrics authored at every point; trajectories rise monotonically with no oscillation, so no cyclical mechanism is claimed. Rising base_extractiveness models accumulation: as the renewables coalition consolidated, the screen hardened from heuristic to gatekeeper (green-finance eligibility fights, fleet-exclusion defaults in national plans), which is the accumulation signature worth investigating rather than tuning away.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute divergent types from identical structural data. From the renewable industry and investor seats, the screen is the coordination device that made their business case legible to lenders — a rope experienced from inside. From the nuclear industry seat, the same screen is a legitimacy bar that ignores its lifetime carbon performance — extraction experienced as categorical exclusion. From the grid operator seat, it is an unfunded mandate: others select the portfolio, the operator absorbs its physics. From the scenario-body seat, it is methodology, neither benefit nor burden. The engine computes these per-seat classifications from the structural data; this commentary explains why they diverge without adjudicating among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the renewable industry, advocacy, and investor seats toward the beneficiary end of directionality: the screen subsidizes their addressable market and bankability, and their exit postures (mobile supply chains, arbitrage capital, identity-locked mission) modulate how fully each collects. Victim declarations drive the nuclear industry and grid operators toward the target end: both bear costs imposed by the screen's operation, with the grid operator's trapped exit pushing it nearer full-target than the industry's constrained position. Ratepayers sit mid-range — they pay integration charges but also receive cheap clean generation, a genuinely dual position carried as payer with secondary beneficiary role. The scenario bodies and ministries administer the screen and collect reputational and electoral returns from it, giving them a mild beneficiary lean that the structural derivation should register through their agenda-setting relationship even though they appear in no beneficiary array. No directionality overrides are authored: the per-power-atom override mechanism is too coarse to distinguish the institutional grid operator (full-target) from the institutional scenario body (mild beneficiary) without corrupting one or the other, so the derivation chain is left to read the declared roles and exits directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deferral via perpetually-distant technology — is live: the budget is still finite and shrinking, and the temptation to pad plans with undeployable options persists. Classification as tangled_rope prevents two opposite misreadings. Reading the screen as pure rope would erase the identifiable payers: an industry excluded on lead time alone, operators absorbing unpriced integration costs, and bill-payers with no seat — asymmetries that require active enforcement (taxonomy rules, lender policies, coalition policing) to hold. Reading it as pure snare would erase the genuine coordination achievement: the screen did convert distant pledges into procurement queues and did redirect capital toward technologies that have delivered measurable cumulative abatement. The mandatrophy risk runs forward, not backward: the measurement series shows extractiveness and suppression accumulating together, the classic signature of coordination machinery accreting extraction. If intermittency costs remain unpriced back into the legitimacy metric and exclusion hardens from default to dogma, the arrangement drifts snare-ward while retaining its coordination cover; the temporal data exists precisely to date any such transition honestly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the deployability-within-budget verdict set the content of the technology-legitimacy kernel itself, or one faction''s reading — would the reliability and precautionary readings assign legitimacy to materially different technology sets?',
    'Author the sibling readings as separate constraint stories and compare verdict sets seat-by-seat; technologies whose legitimacy flips across readings (fission, capture-equipped generation, firm variable-plus-storage configurations) locate the disagreement structurally.',
    'Divergent verdict sets confirm a genuinely contested kernel in which per-reading classifications stand as authored; convergence would indicate one reading has captured the kernel and the siblings are residual rather than live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether this reading''s verdicts constitute the kernel or one faction''s instantiation of it.').

omega_variable(
    budget_timeline_hardness,
    'Is the 2030/2050 deployment window a hard physical budget constraint, or a politically constructed deadline that demand reduction, carbon removal scaling, or budget reassessment could relax?',
    'Track successive carbon-budget assessments and removal-deployment curves against announced targets, then test whether legitimacy verdicts track budget revisions or persist independently of them.',
    'If the window is soft, the screen''s exclusions lose their necessity justification and the arrangement shifts snare-ward; if hard, the exclusions carry genuine coordination weight and the measured extraction is partly the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_timeline_hardness, conceptual, 'Hardness of the timeline premise underlying the velocity test.').

omega_variable(
    intermittency_cost_attribution,
    'Are the system-integration costs borne by grid operators intrinsic to fast variable deployment, or artifacts of transmission and storage under-investment that velocity-first policy itself produced?',
    'Compare integration-cost trajectories across jurisdictions that paired deployability targets with binding grid-investment mandates against those that pursued deployment targets alone.',
    'If artifact, grid-operator burden is remediable inside the reading and effective extraction falls; if intrinsic, the reading structurally externalizes costs it does not price, and the tangled-rope reading hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_cost_attribution, empirical, 'Origin of the intermittency costs the victim set absorbs.').

omega_variable(
    self_fulfilling_deployability,
    'Does the velocity screen partly manufacture the deployment gap it cites — do financing exclusions and policy omission slow firm-low-carbon construction, validating the screen with a disadvantage it created?',
    'Compare construction timelines and financing costs for comparable firm-low-carbon projects across jurisdictions with and without green-finance eligibility, isolating the financing channel from engineering lead times.',
    'If circular, part of the measured extraction on the excluded set is manufactured incapacity rather than discovered incapacity, raising effective extraction and strengthening the payer seats'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_fulfilling_deployability, conceptual, 'Circularity between the screen''s enforcement and the deployment gaps it cites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlk_velocity_tr_t2015, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(tlk_velocity_tr_t2017, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2017, 0.21).
narrative_ontology:measurement(tlk_velocity_tr_t2019, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2019, 0.23).
narrative_ontology:measurement(tlk_velocity_tr_t2021, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement(tlk_velocity_tr_t2023, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2023, 0.28).
narrative_ontology:measurement(tlk_velocity_tr_t2025, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(tlk_velocity_be_t2015, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(tlk_velocity_be_t2017, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement(tlk_velocity_be_t2019, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(tlk_velocity_be_t2021, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(tlk_velocity_be_t2023, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2023, 0.56).
narrative_ontology:measurement(tlk_velocity_be_t2025, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tlk_velocity_su_t2015, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(tlk_velocity_su_t2017, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2017, 0.52).
narrative_ontology:measurement(tlk_velocity_su_t2019, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(tlk_velocity_su_t2021, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(tlk_velocity_su_t2023, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement(tlk_velocity_su_t2025, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial notion 'legitimate climate technology' covers three structurally distinct legitimacy gates that cannot share one epsilon. The velocity primacy reading (this file) gates on deployability within the budget window; the reliability primacy reading gates on dispatchability and baseload capability; the precautionary reading gates on bounded, reversible failure modes and legacy costs. Each yields a different beneficiary/victim structure — this reading admits renewables and excludes fission on time; the reliability reading reverses the fission verdict; the precautionary reading excludes fission on waste while tolerating slower deployment. The upstream physical premise (finite cumulative carbon budget) constrains all three and is cited by this reading as its warrant. Family members are linked through affects_constraints; contamination propagates when one reading's verdicts are imported into another's fora (for example, velocity-screened technology lists appearing in reliability-focused capacity planning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
