% ============================================================================
% CONSTRAINT STORY: us_wind_project_ban_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_wind_project_ban_2025, []).

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
 *   constraint_id: us_wind_project_ban_2025
 *   human_readable: Executive Ban on New Wind Power Projects (2025)
 *   domain: political/economic
 *
 * SUMMARY:
 *   In 2025, an executive order bans all new onshore and offshore wind power
 *   projects within US national jurisdiction. The ban operates as a
 *   structural snare on renewable energy developers and climate mitigation
 *   actors: they are trapped by legal prohibition with no exit path, while
 *   incumbent fossil fuel producers benefit from suppressed competition. The
 *   constraint exhibits high suppression (0.72) because alternative energy
 *   pathways are mandated away and administrative remedies are blocked. Base
 *   extractiveness (0.58) reflects the asymmetric benefit to fossil fuel
 *   interests during the ban's duration, though this extraction is contingent
 *   on Congressional inaction and lacks the immutability of a natural law.
 *   The theater ratio (0.45) is moderate: the ban performs executive control
 *   and energy independence framing, but this performance is offset by
 *   practical reality (global renewable costs continue declining, US
 *   isolation increases, grid reliability concerns mount). The constraint is
 *   structurally reversible through Congressional action, executive reversal,
 *   or state-level workarounds, making it a clear snare rather than a
 *   mountain. The perspectival gap is sharp: fossil fuel beneficiaries see
 *   coordination (rope); renewable developers see pure extraction (snare);
 *   climate institutions see both coordination (treaty framework) and
 *   extraction (missed targets); the analytical observer at civilizational
 *   scale risks false-summit framing (seeing executive sovereignty as natural
 *   law rather than contingent policy).
 *
 * KEY AGENTS:
 *   - Renewable Energy Developers: Primary victim (powerless/trapped) — capital-intensive projects halted; sunk permitting costs; no legal pathway to proceed
 *   - Climate Mitigation Institutions: Primary victim (powerless/trapped) — lose primary decarbonization lever; cannot override executive mandate through normal channels
 *   - Fossil Fuel Industry: Primary beneficiary (institutional/arbitrage) — protected from renewable competition; benefits without direct enforcement burden
 *   - State Energy Regulators: Secondary actor (organized/constrained) — retain some coordination authority (grid management) but lose renewable deployment levers; constrained by federal preemption
 *   - International Climate Commitments (Paris, IEA pledges): Secondary victim (powerful/mobile) — US faces extraction pressure on emissions targets; retains treaty exit option at diplomatic cost
 *   - Congress: Structural actor (organized/mobile) — holds override authority but faces supermajority burden and political division
 *   - Global Energy Transition: Observer (institutional/arbitrage) — ban is isolated resistance; other nations accelerate renewable deployment; US loses market share in growing sector
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_wind_project_ban_2025, 0.58).
domain_priors:suppression_score(us_wind_project_ban_2025, 0.72).
domain_priors:theater_ratio(us_wind_project_ban_2025, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_wind_project_ban_2025, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_wind_project_ban_2025, snare).
narrative_ontology:human_readable(us_wind_project_ban_2025, "Executive Ban on New Wind Power Projects (2025)").
narrative_ontology:topic_domain(us_wind_project_ban_2025, "political/economic").

domain_priors:requires_active_enforcement(us_wind_project_ban_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, incumbent_energy_producers).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, renewable_energy_developers).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, climate_mitigation_actors).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, grid_decarbonization_goals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RENEWABLE ENERGY DEVELOPER (SNARE) — Trapped by executive prohibition with no legal exit path. Capital-intensive projects cannot proceed. Sunk costs in permitting and planning. High suppression: alternative energy sources are mandated away; developer cannot arbitrage to other markets within national jurisdiction.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLIMATE MITIGATION ACTORS (SNARE) — Policy institutions, NGOs, and scientific bodies committed to emissions reduction face structural extraction of their primary lever (renewable deployment). No legal exit from executive mandate. Suppression operates through institutional channels: administrative bodies cannot override; Congress faces supermajority requirements.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL INDUSTRY (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism protecting market share against renewable competition. Can arbitrage through other markets (export, foreign subsidiaries) but benefits from domestic suppression of alternatives. Low effective extraction experienced from beneficiary position.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENERGY REGULATORS (TANGLED ROPE) — Constrained by federal preemption but retain some coordination function through grid management and retail regulation. Can encourage distributed solar or hydro as partial workarounds. Significant extraction: lose renewable deployment authority. Mixed extraction-coordination: must maintain grid reliability despite reduced supply diversity.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE COMMITMENTS (TANGLED ROPE) — US treaty obligations (Paris Agreement, IEA net-zero pledges) face extraction pressure through missed renewable targets. Nations retain exit option (withdraw from treaty) but at diplomatic cost. Effective extraction is constrained by national sovereignty and treaty renegotiation capacity. Coordination function remains (treaty framework) alongside asymmetric extraction (reduced US emissions cuts).
constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL ENERGY TRANSITION (PITON) — The ban is inertial resistance to structural energy shift. Low actual extractiveness globally (other nations accelerate wind deployment). High theater ratio: the ban performs executive control and energy independence framing while global decarbonization continues. The constraint degrades over time as international renewable costs decline and US isolation increases.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — The prohibition might appear as an immutable sovereign power constraint (natural law of national authority). However, structural data contradicts this: the ban is a reversible executive order with low emergence-naturally signature, moderate suppression (surmountable through Congressional action), and significant theater (performing executive control while global facts change). This is a false summit — contingent policy misrepresented as immutable law.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_wind_project_ban_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_wind_project_ban_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_wind_project_ban_2025, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_wind_project_ban_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_wind_project_ban_2025, TR),
    TR >= 0.70.

:- end_tests(us_wind_project_ban_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ban directly suppresses $100+ billion in projected renewable investment and benefits fossil fuel incumbents through market share protection. The extraction is not total (alternative technologies, state workarounds, Congressional reversal remain possible) but is substantial and immediate. Measured at 0.42 at ban initiation (legal shock), rising to 0.58 as incumbent fossil fuel interests consolidate gains and renewable pipeline dries up. Suppression (0.72): High. Renewable developers face absolute legal prohibition — they cannot build wind projects anywhere within national jurisdiction. Administrative channels are closed (executive order); Congressional override requires supermajority; state-level alternatives are limited by federal preemption. Suppression is softened only by Congressional reversal pathway and long-term state-level adaptation options, preventing it from reaching 0.95. Theater ratio (0.45): Moderate. The ban performs executive control and energy independence framing ('American coal and natural gas') but this performance is weakened by practical reality: global wind costs are falling, US renewable industries shrink, grid reliability concerns emerge, and international market share is lost to competitors. The performance is not sustained against observable facts, keeping theater below 0.50. Claimed type: Snare. The constraint meets snare criteria: extractiveness > 0.46 (0.58), suppression > 0.60 (0.72), victims clearly identified (renewable developers, climate institutions), and no meaningful coordination function for the banned group.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates sharp perspectival disagreement. Fossil fuel beneficiaries with arbitrage options (can sell internationally, have capital mobility) perceive the ban as pure coordination (rope) — a mechanism solving the legitimate problem of managing energy transition timeline and protecting incumbent investments. Renewable energy developers trapped by the ban see snare — maximum extraction with no exit. State regulators see tangled rope — they retain some coordination authority (grid management) but lose renewable deployment tools and face extraction through federal preemption. Climate institutions see tangled rope — treaty obligations create coordination function (remain parties to Paris) but the ban extracts through missed emissions targets and reduced domestic mitigation capacity. The analytical observer at global/civilizational scale risks the false-summit perspective: seeing the executive prohibition as a natural law of national sovereignty rather than a contingent policy choice. The structural data (medium theater ratio, reversible through Congressional action, low emergence-naturally signature) reveals this as naturalization of policy, not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position. Fossil fuel producers are beneficiaries with high arbitrage (can sell domestically or export, have capital mobility) → d ≈ 0.10 → f(d) ≈ -0.05 → low experienced extractiveness (they benefit). Renewable developers are victims with zero exit options (banned entirely, cannot arbitrage) → d ≈ 0.98 → f(d) ≈ 1.40 → maximum experienced extractiveness (full target). State regulators are constrained (cannot override federal ban) but retain some agency (grid management, distributed solar incentives) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate experienced extractiveness (stuck in mixed position). International climate institutions are powerful with mobile exit (can withdraw from treaties) but choose to remain → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction. Congress is organized with high agency (supermajority override available) → d ≈ 0.45 → f(d) ≈ 0.55 → low-to-moderate constraint experience. The piton perspective (global energy transition) experiences low extractiveness because the ban is isolated — other nations accelerate renewable deployment, US loses relative position but global decarbonization continues.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: 'Is this coordination hiding as extraction, or extraction hiding as coordination?' The data supports snare classification. The ban produces no coordination benefit for the banned developers (renewable energy companies gain nothing from the policy). The beneficiary (fossil fuel industry) gains pure advantage, not coordination. The suppression (0.72) is asymmetric coercion with no offsetting coordination function for the constrained parties. This rules out tangled_rope (which requires genuine coordination alongside extraction). The state regulators' tangled_rope perspective is legitimate — they do retain coordination authority (grid reliability, distributed solar) — but the primary constraint on renewable developers is pure extraction. The false-summit risk is the analytical observer framing the ban as a mountain (natural law of executive sovereignty). The structural data contradicts this: emerges_naturally = false (it's a reversible policy choice), accessibility_collapse is low (Congress can override), resistance to override is not irreducible (will weaken over time). The mandatrophy resolves: snare from the primary victim perspective; snare from the climate institution perspective; rope from the fossil fuel beneficiary perspective; tangled_rope from state and international institutional perspectives; piton at global scale (degraded resistance to global decarbonization). No single unified classification — the presheaf over observation contexts is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coal_plant_lifecycle,
    'Will the ban extend incumbent coal plants'' operational lifespans, or will retirement economics override the wind prohibition?',
    'Empirical tracking of coal plant retirement rates and announced closures before/after ban; correlation with natural gas and renewable alternatives availability',
    'If ban extends coal lifespans by 5+ years: extractive effect is durable and high. If coal retirements proceed despite ban: extraction is limited to renewable-specific developer damage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coal_plant_lifecycle, empirical, 'Coal plant lifecycle extension vs retirement economic dynamics').

omega_variable(
    congressional_reversal_timeline,
    'What is the structural probability and timeline for Congressional action to override or modify the executive ban?',
    'Tracking of Congressional energy bills, pressure from constituent states with renewable infrastructure, future election cycles and party composition shifts',
    'If Congress acts within 2 years: snare is temporary (becomes scaffold). If ban persists 5+ years: snare is durable and classification holds. If reversed: constraint becomes degraded piton (performative control gesture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_reversal_timeline, empirical, 'Timeline for Congressional override or modification of executive ban').

omega_variable(
    state_level_workarounds,
    'Can state-level renewable incentives and distributed generation succeed in bypassing the federal ban''s suppression?',
    'Empirical monitoring of rooftop solar installation rates, state renewable energy credits, municipal microgrids, and competitive renewable energy procurement in states with strong renewable mandates',
    'If workarounds reduce suppression effect by 40%+: classification shifts toward tangled_rope (suppression weakens). If workarounds fail: suppression remains high and snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_level_workarounds, empirical, 'Effectiveness of state-level renewable energy workarounds').

omega_variable(
    energy_security_reversal_trigger,
    'Does grid vulnerability, electricity cost spikes, or energy security concerns trigger reversal rhetoric that reframes wind as strategic infrastructure?',
    'Monitoring of energy price trends, blackout events, geopolitical supply disruptions, and executive messaging shifts on renewable necessity',
    'If reversal occurs: constraint becomes piton (performative, degraded). If security concerns don''t trigger reversal: snare extractiveness remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_security_reversal_trigger, empirical, 'Energy security or grid reliability triggers for reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_wind_project_ban_2025, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(windban_tr_t0, us_wind_project_ban_2025, theater_ratio, 0, 0.38).
narrative_ontology:measurement(windban_tr_t3, us_wind_project_ban_2025, theater_ratio, 3, 0.42).
narrative_ontology:measurement(windban_tr_t6, us_wind_project_ban_2025, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(windban_be_t0, us_wind_project_ban_2025, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(windban_be_t3, us_wind_project_ban_2025, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(windban_be_t6, us_wind_project_ban_2025, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_wind_project_ban_2025, resource_allocation).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, electricity_grid_decarbonization_timeline).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, us_energy_sector_transition_capital_allocation).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, international_climate_commitment_compliance).

% DUAL FORMULATION NOTE:
% The wind ban is upstream of three downstream constraints: (1) electricity grid decarbonization timeline, which faces extraction pressure from delayed renewable deployment; (2) energy sector capital allocation, which is redirected toward fossil fuel lock-in rather than transition infrastructure; (3) international climate commitments, which face structural extraction through reduced US emissions reductions. Each downstream constraint has its own extractiveness profile; the ban amplifies extraction risk across the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_wind_project_ban_2025, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
