% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Technology Legitimacy Kernel — Reliability Primacy Reading
 *   domain: energy_policy/climate_mitigation
 *
 * SUMMARY:
 *   This constraint instantiates the reliability-primacy reading of a
 *   contested kernel in energy policy: technology legitimacy for climate
 *   mitigation. The reading defines legitimate climate technology as that
 *   which provides dispatchable, baseload-capable generation to ensure grid
 *   stability. This reading advantages nuclear operators (high capacity
 *   factors naturally meet the criterion) and grid-reliability incumbents
 *   (whose operational frameworks become the measure of legitimacy), while
 *   imposing a legitimacy tax on intermittent renewables (which must now
 *   co-pair with storage to qualify) and excluding failure-mode and
 *   deployment-velocity frameworks from the legitimacy assessment. The claim
 *   is tangled_rope (genuine coordination function—grid stability—paired with
 *   asymmetric extraction favoring incumbents); metrics reflect high
 *   extraction, substantial suppression (enforcement of the dispatchability
 *   criterion against alternative framings), and moderate theater (the
 *   grid-stability rationale is real, but an increasing share of suppression
 *   effort sustains the primacy hierarchy rather than stability itself).
 *
 * KEY AGENTS:
 *   - Nuclear operators: beneficiary, hold dispatchability naturally, gain priority
 *   - Renewable developers: payers, face legitimacy gate requiring storage co-investment
 *   - Grid operators & regulators: agenda-setters, define and enforce the dispatchability standard
 *   - Ratepayers: bear dual role (benefit from stability, pay for storage infrastructure)
 *   - Climate scientists (precautionary frame), deployment advocates (velocity frame): excluded, their axes are off-table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.71).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Technology Legitimacy Kernel — Reliability Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '2e9feda7-9bb3-4e3c-a0b9-25fced53fad6').
narrative_ontology:cs_kernel_codification('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', formalized).
narrative_ontology:cs_authority_grounding('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', extraction).
narrative_ontology:cs_interpretation_layer_present('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6').
narrative_ontology:cs_reading_relation('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', technology_legitimacy_kernel__precautionary_reading, forecloses).
narrative_ontology:cs_reading_relation('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', foundational, dispatchability_categorical_primacy).
narrative_ontology:cs_axiom_status(dispatchability_categorical_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', dispatchability_categorical_primacy, instrumental).
narrative_ontology:cs_axiom('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', secondary, worst_case_failure_modes_deferred).
narrative_ontology:cs_axiom_status(worst_case_failure_modes_deferred, holdable).
narrative_ontology:cs_axiom_grounding('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', worst_case_failure_modes_deferred, conventional).
narrative_ontology:cs_reference_frame('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', grid_stability_centered_legitimacy).
narrative_ontology:cs_drift_state('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', post_2021_blackout_urgency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e9feda7-9bb3-4e3c-a0b9-25fced53fad6', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_incumbents).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, renewable_technology_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators_and_regulators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_vendors).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, dispatchability_primacy_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, baseload_generation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate nuclear generation plants with capacity factors consistently above 90%. Under this reading's legitimacy criterion (dispatchability/baseload), nuclear plants are immediately certified as legitimate climate technology. They benefit from regulatory priority, capital availability, and protection from competition by technologies that must pay the storage-pairing cost. Their operational model is validated as the template for decarbonization. Exit is unnecessary; the reading favors their existing business.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators, beneficiary,
    institutional, generational, mobile, national).

% Develop and deploy solar photovoltaic, wind, and emerging intermittent generation technologies. These technologies are physically intermittent and do not naturally provide dispatchability. Under this reading's legitimacy gate, renewable projects must now co-pair with storage systems (batteries, thermal storage, green hydrogen) to meet the legitimacy criterion, significantly raising capital costs and project development timelines. Exit options are constrained: they can invest in storage (expensive, slower), challenge the reading in regulatory forums (slow, uncertain), or pivot to non-climate geographies where different legitimacy frames apply. The reading does not ban renewables; it taxes them.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, renewable_technology_developers, payer,
    powerful, biographical, constrained, global).

% Operate transmission and distribution networks and set reliability standards through regulatory bodies (NERC, FERC, regional RTOs). Under this reading, their existing operational frameworks, engineering disciplines, and risk management practices become the measure of technology legitimacy. They set and enforce the dispatchability/baseload standard; technologies that do not meet it must demonstrate compliance through co-investment in balancing infrastructure. Grid operators collect institutional authority (their judgment on what is 'legitimate' becomes regulatory law) and deference from technology developers. Their exit is mobile—they can shift to alternative readings if policy pressure mounts.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators_and_regulators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators_and_regulators, beneficiary).

% Consume electricity from the grid and rely on stable, continuous supply. They genuinely benefit from grid stability and reliable generation—a real coordination function. They also bear the costs: when utilities invest in storage infrastructure required by the reading's legitimacy gate, those costs are passed through rates. When utilities prioritize dispatchable generation over cost-optimal renewable deployment, ratepayers face higher electricity prices. Their exit is constrained by geography (the grid is a natural monopoly) and by the regulatory institutions that set the legitimacy frame. They are dual-positioned: they benefit from the coordination function the reading secures, but they pay the price of securing it via a specific (costly) technology mix.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs, beneficiary).

% Develop and deploy battery systems (lithium-ion, flow, thermal), compressed-air systems, green hydrogen, and other storage technologies. The reading's requirement that intermittent renewables co-pair with storage creates derived demand for their products. Their beneficiary status is secondary: they are not directly favored by the reading, but they profit from the storage-pairing requirement it imposes. Their exit is mobile—if policy shifts to velocity-primacy or precautionary readings, storage demand may decline (or, alternatively, might increase if different reasons for storage emerge). They are positioned to benefit regardless of whether reliability-primacy reading persists, as long as some form of storage requirement remains.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Argue that the legitimacy criterion for climate technology should center on the reversibility of worst-case failure modes and legacy costs. They point to nuclear waste, accident risks, and decommissioning liabilities as failure modes that cannot be reversed within a generation, and contrast them with renewable intermittency (a manageable technical problem, not an existential hazard). They would privilege precautionary_reading over reliability_primacy. They are excluded from this reading's framework because the reading brackets failure-mode analysis as secondary to operational reliability. Their voice is present in climate policy forums but not in the legitimacy-assessment institutions that enforce this reading.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists_emphasizing_reversibility, excluded,
    analytical, civilizational, analytical, global).

% Emphasize the remaining carbon budget and argue that any technology deployable at scale within the 2030/2050 window should be eligible for climate-legitimacy certification, regardless of dispatchability or worst-case risk. They cite deployment timelines: nuclear plants take 10–15 years from site selection to operation; wind and solar can be installed in 1–3 years. They would privilege velocity_primacy_reading. They are excluded from this reading because the reading treats dispatchability as non-negotiable and does not trade it off against deployment speed. Their critique is structural to the reading's core, but institutional power lies with grid operators, not velocity advocates.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, deployment_velocity_advocates, excluded,
    analytical, civilizational, analytical, global).

% Allocate climate finance and development capital based on technology legitimacy frameworks. Banks, development finance institutions, and climate funds track which technologies are certified as legitimate and direct capital accordingly. Under this reading, they are observers: they respond to the legitimacy criterion set by grid operators and regulators, but they do not set the reading itself. Their power is downstream—they amplify the reading's effects by matching capital allocation to legitimacy verdicts. If the reading shifts (to velocity or precautionary), capital flows would reorganize.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_finance_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared technical standard for assessing which generation technologies reliably contribute to grid stability. Solves the collective-action problem of preventing blackouts and cascading failures during high-penetration renewable scaling by defining a clear metric (dispatchability/baseload capability) that grid operators, utilities, and regulators can all recognize and enforce. Without such a standard, different jurisdictions and utilities would adopt incompatible reliability criteria, creating coordination failures at interconnection points and during cross-border power trading.
% TRANSFER_FUNCTION: Transfers legitimacy certification (and associated regulatory priority, capital availability, and deployment support) from intermittent renewable developers to nuclear operators and dispatchable incumbents. Ratepayers bear the cost in the form of storage-infrastructure investment required to make renewables 'legitimate' and/or higher electricity prices from prioritizing dispatchable generation. Renewable developers must co-invest in storage systems to qualify for legitimacy, raising project costs by 20–40% depending on duration requirements.
% ABSENT_VOICES: Climate scientists emphasizing failure-mode reversibility (precautionary frame) and deployment-velocity advocates are structurally excluded from the reading's framework. They participate in climate policy forums and produce technical literature, but the institutions that enforce the legitimacy standard (NERC, FERC, utilities) do not listen to their criteria—precaution and speed are off-table within this reading. Their objections are routed to alternative readings (precautionary_reading, velocity_primacy_reading), not incorporated here.
% DISAPPEARANCE_RATIONALE: If the reliability-primacy legitimacy criterion disappeared overnight, renewable deployment would accelerate (no longer requiring paired storage to be considered legitimate), storage co-investment requirements would drop, renewable LCOE curves would steepen downward, and grid operators would integrate intermittent supply at higher penetration using demand-side flexibility and expanded transmission interconnection. The energy technology portfolio would reorganize around criteria emphasizing cost and speed over dispatchability-as-primacy. Within 5 years, the generation mix would be visibly different (higher renewable %, lower dispatchable %), and investment patterns would shift accordingly.
% FOUNDING_PROBLEM: 2021–2023: rapid renewable deployment in high-penetration regions (Texas, California, parts of Australia) caused grid stress events and near-miss blackouts. Grid operators and regulators identified intermittent generation without sufficient dispatchable backup as a root cause. The founding problem is ensuring grid stability and preventing cascading blackouts during decarbonization—specifically, maintaining reserve margins and frequency response during high-renewable-penetration scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators (NERC, FERC, regional RTOs) attest the problem is live and cite specific blackout events (Texas 2021, California 2022). Renewable developers and independent analyses (NREL, academic institutions, European grid operators) attest the problem is overframed: demand-side flexibility, expanded transmission, modest storage, and aggressive demand management can maintain stability at 80%+ renewable penetration without treating dispatchability as primacy. European grids (Denmark, Spain, Portugal) have operated at 50%+ renewable penetration with fewer dispatchability-class plants than US grids, suggesting the problem is solvable via different mechanisms. The corroboration diverges sharply: incumbent grid operators (who set this reading) and independent analysts (who contest it) tell incompatible stories.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the interval (0.54→0.68) as the reading becomes institutionalized: regulators adopt dispatchability as the standard, utilities embed it in procurement criteria, and the cost of qualifying renewables (storage pairing) becomes empirically clear. Suppression stays high (0.64→0.71) and stable because maintaining the reading requires consistent exclusion of alternative frameworks—precautionary and velocity readings propose to reorganize the legitimacy assessment, so active enforcement (regulatory capture, investment prioritization) sustains the reading's dominance. Theater rises moderately (0.28→0.42) because the reading's grid-stability rationale is real (genuine coordination function), but an increasing share of regulatory effort is visibly dedicated to protecting the dispatchability hierarchy itself—not to stability that could be achieved differently. The one-grid alignment ensures every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the grid operator and nuclear-operator seats, the reading is a genuine safety/stability measure that ensures reliable supply. From the renewable-developer and ratepayer seats, the reading is a disguised protectionist gate that locks in incumbent technology and imposes unnecessary storage costs. The engine computes this perspectival gap from the stakeholder's structural positions: beneficiaries see coordination, payers see extraction. The commentary maps the gap to real institutional power—grid operators set the standard (organizational power), renewables developers must argue for variance (constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and grid-reliability incumbents are full beneficiaries: the reading certifies their technology/role as the measure of legitimacy (d ≈ 0.1–0.2, low extraction). Renewable developers are full targets: they must now clear a capability bar that requires co-investment or challenge the reading itself (d ≈ 0.85–0.95, high extraction). Ratepayers sit near symmetric but trending toward target: they benefit from stable supply (genuine coordination) but increasingly bear the cost of storage infrastructure required by the reading (d ≈ 0.55–0.65, moderate extraction). Grid operators are beneficiaries-with-power (they set the criterion and collect institutional authority as a result; d ≈ 0.15–0.25). No directionality overrides are needed; the structural derivation is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids collapsing mandatrophy because the founding problem (grid stability during rapid renewable scaling) is genuinely live, and the reading provides a real (if contestable) mechanism to solve it. The threat is not mandatrophy but competitive-reading foreclosure: if the velocity-primacy or precautionary readings gain institutional ground, this reading's legitimacy threshold becomes a zombie—defended by incumbents against alternatives rather than solving the coordination problem. The measurement series track this risk: theater is rising, which signals increasing performance-maintenance effort. An omega variable addresses the unresolved question of whether dispatchability is structurally necessary or a policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_necessity_vs_choice,
    'Is dispatchability a structurally necessary physical requirement for grid stability at high renewable penetration, or is it a policy choice that reflects incumbent preferences and engineering conservatism?',
    'Controlled experiments or natural experiments from high-renewable-penetration jurisdictions (Denmark, Costa Rica, Uruguay, California) that operate with minimal dispatchable generation—measure stability metrics (frequency deviation, blackout rates, LOLP) and compare to traditional dispatchability-heavy grids. Analysis of whether reserve margins and demand management can substitute for dispatchability.',
    'If dispatchability is necessary, the reading''s primacy is justified and the extraction reflects genuine coordination cost. If dispatchability is a choice, the reading becomes a policy tool for protecting incumbent technology; the tangled_rope classification holds but the beneficiary set is vindicated as rational rather than captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchability_necessity_vs_choice, empirical, 'Physical necessity vs. policy choice for the dispatchability criterion.').

omega_variable(
    storage_cost_trajectory,
    'Will battery and other storage technologies achieve cost parity with dispatchable generation within the 2030–2050 window, making storage-paired renewables economically equivalent to nuclear/fossil baseload?',
    'Projections from NREL, BloombergNEF, IEA storage roadmaps; periodic re-assessment of levelized cost of electricity (LCOE) for batteries + renewables vs. nuclear/gas + operating reserves.',
    'If storage costs fall below nuclear and dispatchable costs (on LCOE + balancing basis), the legitimacy gap between renewables and baseload collapses; the reading''s discrimination mechanism weakens and becomes less extractive. If storage remains expensive relative to dispatchable, the reading''s requirement for paired storage sustains the cost gap and the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(storage_cost_trajectory, empirical, 'Whether storage cost trends will eventually equalize the legitimacy burden across technologies.').

omega_variable(
    kernel_interpretability_under_climate_urgency,
    'As remaining carbon budget and climate urgency compress the timeline, does the reliability-primacy reading''s requirement for dispatchability become internally incoherent—forcing a choice between grid stability and emission targets that makes both unachievable?',
    'Modeling of energy system feasibility under 1.5°C/2°C carbon budgets with and without the dispatchability requirement; comparative analysis of technology deployment timelines and system stability across readings.',
    'If the reading becomes incoherent under climate urgency (dispatchability requirement slows decarbonization below the rate needed to meet climate targets), institutional pressure will mount to shift to velocity-primacy or hybrid readings. The reading would transition from tangled_rope toward piton (performatively maintained but abandoned by those it served).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretability_under_climate_urgency, conceptual, 'Whether the reliability-primacy axioms remain coherent under accelerating climate-urgency constraints.').

omega_variable(
    committer_frame_sibling_readings,
    'This constraint is one reading of a contested kernel; the sibling readings (velocity_primacy, precautionary) are authored as separate constraints. How do the ε values and victim sets differ across readings, and what does that divergence reveal about the kernel''s contestation structure?',
    'Compare the three constraint stories'' base_properties.extractiveness and victims[] arrays. Map the divergence to the reading-specific axioms and reference frames. Use the triplet to test whether readings are genuinely alternative commitments or surface variants of a single underlying choice.',
    'If ε values diverge widely across readings (e.g., reliability_primacy ε=0.68, velocity_primacy ε=0.35, precautionary ε=0.72), it suggests readings inhabit truly different problem spaces—not just reordering the same variables. If victim sets diverge, it reveals which parties bear costs under which reading''s authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_readings, empirical, 'Cross-reading constraint analysis: ε and victim-set divergence as markers of kernel contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(tech_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tech_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(tech_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the technology_legitimacy_kernel. The kernel is a stabilized commitment to assessing climate-technology legitimacy; different readings impose different legitimacy criteria (reliability/velocity/precaution). The three readings are NOT alternative measurements of the same constraint—they instantiate different ε values, different victim sets, and different beneficiary coalitions because they answer the legitimacy question differently. The reliability_primacy_reading treats dispatchability as the non-negotiable constraint and extracts from renewables developers and ratepayers via a storage-pairing requirement. The velocity_primacy_reading would treat deployment speed as primacy and extract from slow-to-build incumbents. The precautionary_reading would treat failure-mode reversibility as primacy and extract from long-legacy technologies like nuclear. All three are live in policy discourse; this story models one reading (reliability primacy) as a tangled_rope. The other two readings are authored as separate constraint stories, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
