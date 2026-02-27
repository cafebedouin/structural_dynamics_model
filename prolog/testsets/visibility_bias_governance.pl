% ============================================================================
% CONSTRAINT STORY: visibility_bias_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_visibility_bias_governance, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visibility_bias_governance
 *   human_readable: The Dashboard Delusion
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The dashboard delusion describes a governance constraint where
 *   policy-making organs become locked into visible metrics (stock indices,
 *   quarterly GDP, monthly unemployment, surface-level crime statistics)
 *   while systematically deprioritizing low-visibility systemic decay
 *   (infrastructure entropy, institutional capacity erosion, trust deficit,
 *   pension underfunding, deferred maintenance cascades). The constraint
 *   exhibits a tangled rope structure: high-visibility metrics do provide a
 *   coordination function (enabling consensus on short-term priorities,
 *   simplifying decision-making for time-pressed executives), but this
 *   coordination benefit is achieved through systematic extraction from
 *   distributed beneficiaries of low-visibility systemic goods. The
 *   constraint persists because incumbent decision-makers (executive,
 *   legislative, corporate) experience the dashboard as a genuine
 *   coordination mechanism that solves the acute problem of 'what to focus
 *   on?', while those bearing the cost of deferred systemic work
 *   (infrastructure workers, future generations, civil society) have no
 *   mechanism to make their interests visible within the same metric frame.
 *   The theater ratio (0.81) reflects the performative nature of governance
 *   dashboards: public displays of metric achievement become decoupled from
 *   actual institutional capacity or systemic health. Politicians celebrate
 *   declining unemployment while infrastructure atrophies. Markets celebrate
 *   GDP growth while social cohesion degrades. The constraint has degraded
 *   substantially over the 20-year interval as digital dashboards have become
 *   the primary information architecture for governance, pushing
 *   low-visibility work further into invisibility.
 *
 * KEY AGENTS:
 *   - Incumbent Decision Makers: Primary beneficiary (institutional/arbitrage) — executives and elected officials capture credit for visible metric improvements; exit via electoral arbitrage before consequences manifest
 *   - Infrastructure and Institutional Systems: Primary victim (powerless/trapped) — bear accumulated deferred maintenance, capacity erosion, and decay with no political voice; cannot exit or relocate systemic dependencies
 *   - Distributed Citizenry: Secondary victim (moderate/constrained) — experience quality-of-life degradation from infrastructure failure and institutional capacity loss; constrained by geography and political organization
 *   - Professional Bureaucrats: Mixed (moderate/constrained) — see coordination benefit from shared metrics but are incentivized to optimize visible targets rather than invisible systemic work; career advancement requires dashboard performance
 *   - Financial Market System: Institutional beneficiary (institutional/arbitrage) — prices high-visibility metrics, systematically underprices low-visibility decay; exit via arbitrage between visible and invisible assets
 *   - Reform Coalition: Organized agents (organized/constrained) — advocates for alternative dashboards (infrastructure condition indices, trust metrics, true cost accounting) that would make invisible decay visible; see sunset path through metric reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visibility_bias_governance, 0.58).
domain_priors:suppression_score(visibility_bias_governance, 0.68).
domain_priors:theater_ratio(visibility_bias_governance, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visibility_bias_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(visibility_bias_governance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(visibility_bias_governance, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(visibility_bias_governance, tangled_rope).
narrative_ontology:human_readable(visibility_bias_governance, "The Dashboard Delusion").
narrative_ontology:topic_domain(visibility_bias_governance, "political/institutional").

domain_priors:requires_active_enforcement(visibility_bias_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(visibility_bias_governance, incumbent_decision_makers).
narrative_ontology:constraint_beneficiary(visibility_bias_governance, financial_market_actors).
narrative_ontology:constraint_victim(visibility_bias_governance, future_institutional_capacity).
narrative_ontology:constraint_victim(visibility_bias_governance, distributed_citizenry).
narrative_ontology:constraint_victim(visibility_bias_governance, systemic_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFRASTRUCTURE DECAY VICTIM (SNARE) — Those bearing the cost of deferred infrastructure maintenance, pension underfunding, and trust erosion have no exit. The metrics that drive policy visibility omit their suffering. They are trapped by geography (cannot move infrastructure), time horizon (decay compounds across decades), and political organization (distributed, diffuse costs). Maximum experienced extraction.
constraint_indexing:constraint_classification(visibility_bias_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT DECISION MAKER (ROPE) — Executives and elected officials experience the constraint as coordination: high-visibility metrics enable consensus on short-term priorities. Dashboard data simplifies coalition-building. Exit via electoral arbitrage (rotate to next office before consequences manifest). Net beneficiary — extraction runs toward this agent. Theater ratio (0.81) reflects performative governance: public dashboard metrics become the official reality.
constraint_indexing:constraint_classification(visibility_bias_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROFESSIONAL BUREAUCRAT (TANGLED ROPE) — Civil servants experience both coordination (shared metrics enable inter-departmental alignment) and extraction (career advancement requires hitting visible targets; invisible systemic work goes unrewarded). Constrained exit — bound to institutional logic but aware of systemic rot not reflected in dashboards. Mixed experience.
constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Infrastructure audit advocates, fiscal transparency groups, and system integrity organizations see the dashboard delusion as a temporary institutional failure solvable through metric reform. Alternative dashboards (infrastructure condition indices, trust metrics, true cost accounting) represent a sunset mechanism. High suppression of these alternatives (incumbents resist transparency), but organized agents perceive exit path through institutional redesign.
constraint_indexing:constraint_classification(visibility_bias_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL MARKET SYSTEM (PITON) — Markets price in high-visibility indices (stock prices, unemployment, GDP growth) while systematically underpricing low-visibility decay (infrastructure entropy, institutional degradation, social cohesion loss). Market pricing of these invisible costs has atrophied — the mechanism persists through inertia and information asymmetry, not because it accurately reflects value. Theater ratio extraordinarily high (0.85+): market performance becomes ceremonial proxy for true economic health.
constraint_indexing:constraint_classification(visibility_bias_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE LIMIT VIEW (MOUNTAIN) — From a universal/civilizational context, bounded human cognition cannot track more than ~7-9 concurrent metrics effectively. Attention to high-salience data (daily stock ticker, monthly unemployment) is neurologically privileged over low-salience data (20-year infrastructure decay). This perspective frames the dashboard delusion as an immutable cognitive constraint — you cannot simultaneously monitor what you can see and what you cannot. However, the structural data contradicts the mountain classification: the constraint is not cognitive but institutional — alternative information architectures (distributed monitoring, cross-timescale dashboards) could be implemented but are actively suppressed by beneficiaries.
constraint_indexing:constraint_classification(visibility_bias_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visibility_bias_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(visibility_bias_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visibility_bias_governance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(visibility_bias_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(visibility_bias_governance, TR),
    TR >= 0.70.

:- end_tests(visibility_bias_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The incumbent group captures meaningful career and political benefits (re-election probability, budget allocation, performance bonuses) from optimizing visible metrics while deferring costs to distributed beneficiaries of systemic work. The extraction is high (0.58) because the deferral timeline is systematic — costs compound across decades while benefits accrue within electoral cycles. However, extractiveness is not higher (0.70+) because there exists a genuine coordination function: shared metrics do solve the real problem of prioritizing among competing demands. The constraint is not pure rent-seeking but a hybrid where coordination benefit is real and extraction is real. Suppression (0.68): High. Barriers to making systemic decay visible include: information asymmetry (systemic decay is genuinely harder to measure and report), institutional inertia (dashboard architecture is locked in by legacy systems), political economy (incumbent beneficiaries resist metric expansion), and attention scarcity (media and public focus are limited). Theater ratio (0.81): Very high, and rising. Dashboard metrics have become ceremonial representations of governance success disconnected from actual systemic health. Public emphasis on metric targets (unemployment down, stock market up, crime statistics down) is performative — the real work of infrastructure maintenance, institutional capacity building, and trust cultivation is invisible and unrewarded. Theater has risen from 0.62 to 0.81 over the interval as digital dashboards have become the primary communication channel for governance, strengthening the disconnect between visible metrics and invisible systemic health.
 *
 * PERSPECTIVAL GAP:
 *   The original beneficiary and infrastructure victim perspectives show maximal perspectival gap. Incumbents see rope (coordination of priorities via shared metrics) while infrastructure decay victims see snare (trapped by compounding decay with no political voice). The professional bureaucrat sees tangled rope (both coordination and extraction in career incentives). The reform coalition sees scaffold (temporary problem solvable via metric architecture change). The financial market sees piton (pricing mechanism atrophied, persists through inertia). The analytical observer risks seeing mountain (cognitive limits on attention) but structural data reveals this as false summit — institutional choices, not cognitive constraints, drive the metric hierarchy. The perspectival spread demonstrates how the same structural phenomenon (metric-driven governance) produces radically different classifications depending on the agent's structural position and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by their structural position relative to extraction flow. Incumbents with arbitrage exit options derive d ≈ 0.15-0.20 (beneficiaries experiencing low effective extraction due to exit capacity). Infrastructure/institutional beneficiaries trapped by geography and compounding decay derive d ≈ 0.92 (maximum targets experiencing high extraction). Professional bureaucrats constrained by institutional logic but aware of systemic rot derive d ≈ 0.58 (mixed experience). Organized reform agents with constrained but non-zero exit derive d ≈ 0.45 (moderate experienced extraction despite agency). Financial system with arbitrage exit derives d ≈ 0.25 (beneficiary despite systemic underpricing). The piton classification derives from the theater gate rather than high experienced extraction — market pricing mechanism has atrophied and persists through institutional inertia, not because it functions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY SITUATION: The claim that dashboard governance is a 'natural law of human cognition' (mountain perspective) is a false summit that naturalizes what is actually a contingent institutional arrangement. Bounded cognition (true limit) and bounded institutional capacity (true limit) do constrain governance, but the dashboard delusion is not an immutable consequence — it is a choice architecture where incumbents have aligned incentives to make visible metrics salient and invisible metrics inert. The constraint is a tangled rope (genuine coordination + genuine extraction), not a mountain. The mandatrophy is resolved by rejecting the naturalization and identifying the institutional choice points: who sets the dashboard metrics, what timescales are represented, which stakeholders have voice in metric design, and whether feedback loops include low-visibility decay signals. Alternative information architectures (infrastructure condition indices updated real-time, distributed monitoring with citizen feedback, cross-timescale dashboards spanning electoral and infrastructure lifecycles) exist and are technically feasible but are suppressed by incumbents who benefit from the current metric hierarchy. This is extraction, not law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visibility_threshold_identification,
    'What metric characteristics determine visibility/invisibility? Is it frequency of reporting, salience to incumbent interests, or inherent measurability?',
    'Comparative analysis of metrics that entered dashboards vs. those that did not (infrastructure condition, social trust, institutional decay). Map temporal distribution of measurements, stakeholder interest, and implementation barriers.',
    'If frequency-driven: metric reform alone can solve the problem (Scaffold confirmed). If salience-driven to incumbent interests: visibility bias is intentional extraction mechanism (Snare confirmed). If measurability-driven: technical investment can make invisible metrics visible (Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_threshold_identification, empirical, 'Which factors determine metric visibility in governance dashboards').

omega_variable(
    systemic_decay_quantifiability,
    'Can infrastructure entropy, institutional capacity decay, and trust loss be measured at sub-annual scales and aggregated into predictive indices? Or are they fundamentally resistant to quantification?',
    'Pilot implementation of infrastructure condition indices (asset lifespan/replacement cycle ratio), institutional capacity metrics (staff tenure, training investment), and trust surveys. Test whether real-time monitoring can detect decay before cascade failure.',
    'If quantifiable: the piton and mountain classifications are false — the constraint is institutional choice, not cognitive limit (Snare/Tangled Rope confirmed). If quantification fails: scaffold sunset is optimistic; alternative metrics may be inherently less salient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_decay_quantifiability, empirical, 'Whether low-visibility metrics can be measured and aggregated').

omega_variable(
    dashboard_reform_political_feasibility,
    'Does adding low-visibility metrics to official dashboards change policy outcomes? Or do decision-makers simply ignore unwelcome data?',
    'Natural experiments from jurisdictions that have implemented comprehensive infrastructure/trust dashboards. Track whether policy allocations shift after implementation. Interview decision-makers about metric use.',
    'If reform effective: Scaffold classification confirmed, sunset is real. If ignored: the extraction mechanism is not metric omission but metric suppression — even visible decay is deprioritized for incumbent benefit (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dashboard_reform_political_feasibility, empirical, 'Whether dashboard reform changes policy priorities').

omega_variable(
    election_cycle_congruence,
    'Do policy distortions track electoral cycles? If the constraint is incumbent arbitrage, we should see metric-chasing intensify before elections and infrastructure/trust work accelerate after they are safely past.',
    'Time-series analysis of policy allocations and metric emphasis relative to electoral calendar. Compare policy patterns in off-cycle budgeting vs. election-proximate cycles.',
    'Strong congruence: constraint is pure arbitrage extraction (Snare/Scaffold confirmed, with intentional sunset timing). Weak congruence: constraint is more systemic (institutional inertia rather than individual extraction, Piton/Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(election_cycle_congruence, empirical, 'Whether policy distortion tracks electoral cycles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visibility_bias_governance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vbg_tr_t0, visibility_bias_governance, theater_ratio, 0, 0.62).
narrative_ontology:measurement(vbg_tr_t10, visibility_bias_governance, theater_ratio, 10, 0.74).
narrative_ontology:measurement(vbg_tr_t20, visibility_bias_governance, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(vbg_be_t0, visibility_bias_governance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vbg_be_t10, visibility_bias_governance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vbg_be_t20, visibility_bias_governance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(visibility_bias_governance, information_standard).
narrative_ontology:affects_constraint(visibility_bias_governance, deferred_maintenance_cascade).
narrative_ontology:affects_constraint(visibility_bias_governance, institutional_capacity_erosion).
narrative_ontology:affects_constraint(visibility_bias_governance, political_attention_scarcity).

% DUAL FORMULATION NOTE:
% The dashboard delusion is upstream of multiple systemic degradation constraints. The metric architecture (which measurements are visible, reportable, governable) determines which systemic problems attract policy attention. Deferred maintenance, institutional capacity loss, and trust decay are all downstream constraints that become entrenched when their signals are suppressed in the governance dashboard. This constraint family reflects how information architecture couples to outcomes across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(visibility_bias_governance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
