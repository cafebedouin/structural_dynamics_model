% ============================================================================
% CONSTRAINT STORY: us_wind_project_ban_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: political/economic/energy
 *
 * SUMMARY:
 *   The executive ban on new wind power projects (enacted 2025) creates a
 *   structural constraint between the political authority seeking fossil fuel
 *   market protection and the renewable energy sector, climate mitigation
 *   coalitions, and grid modernization advocates. The ban forecloses a
 *   substantial economic and technology pathway while redistributing capture
 *   rents to incumbent fossil fuel producers. The constraint exhibits snare
 *   characteristics: high suppression (regulatory prohibition with no
 *   legitimate exit), high base extractiveness (captures market share from
 *   renewables to fossil fuels), rising theater ratio (initial rhetoric of
 *   'energy independence' and 'manufacturing protection' masks underlying
 *   rent-protection function as justifications erode). The constraint is
 *   sustained by asymmetric institutional power: executive authority can
 *   impose bans unilaterally; renewable developers and climate coalitions
 *   cannot legally circumvent national regulatory jurisdiction, though they
 *   may pursue international projects or distributed energy substitutes. The
 *   ban demonstrates the distinction between extraction and coordination:
 *   from the fossil fuel incumbent's perspective, it is a coordination
 *   mechanism (alignment with government preference); from the developer's
 *   perspective, it is pure extraction (foreclosure of business
 *   opportunities). The analytical observer risks naturalizing the ban as an
 *   inevitable expression of national energy sovereignty, when the structural
 *   data reveals it as a contingent political choice sustained by suppression
 *   and rising performative rhetoric.
 *
 * KEY AGENTS:
 *   - Incumbent Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — gain protected market position and deferred grid transition investments; experience ban as coordination mechanism
 *   - Renewable Energy Developers (mid-scale): Primary victim (powerless/trapped) — face total foreclosure of domestic project pipeline; cannot relocate within US jurisdiction; no legal exit pathway
 *   - Climate Mitigation Coalition: Secondary victim (organized/constrained) — lose policy leverage for decarbonization pathway; constrained by polarization and regulatory suppression
 *   - Grid Operators (Climate-Aligned): Tertiary victim (powerless/constrained) — forced to pursue suboptimal and more expensive grid modernization pathways; cannot legally integrate wind
 *   - Executive Administration: Institutional beneficiary-narrator (institutional/arbitrage) — frames ban using energy independence and manufacturing rhetoric; functions as arena for fossil fuel coordination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing ban as law of national interest; engine detects false summit given structural data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_wind_project_ban_2025, 0.68).
domain_priors:suppression_score(us_wind_project_ban_2025, 0.75).
domain_priors:theater_ratio(us_wind_project_ban_2025, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_wind_project_ban_2025, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_wind_project_ban_2025, snare).
narrative_ontology:human_readable(us_wind_project_ban_2025, "Executive Ban on New Wind Power Projects (2025)").
narrative_ontology:topic_domain(us_wind_project_ban_2025, "political/economic/energy").

domain_priors:requires_active_enforcement(us_wind_project_ban_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, incumbent_fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, executive_administration).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, renewable_energy_developers).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, climate_mitigation_coalitions).
narrative_ontology:constraint_victim(us_wind_project_ban_2025, grid_modernization_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RENEWABLE ENERGY DEVELOPER (SNARE) — Faces total prohibition on project development within US territory. Cannot exit by relocating within jurisdiction; foreign projects face tariff and financing barriers. Trapped by regulatory ban with no legal pathway. d≈0.96, f(d)≈1.40, σ=1.0 → χ≈0.65. Experiences pure extraction through foreclosure of business opportunities.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLIMATE-ALIGNED GRID OPERATORS (SNARE) — Cannot legally pursue wind integration strategies within US borders. Constrained by regulatory mandate to forgo cost-effective grid modernization. Exit options (distributed solar, geothermal, battery) are secondary and more expensive. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.50. Extraction via forced suboptimal technology choices.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOSSIL FUEL PRODUCERS (ROPE) — Coordinate with executive to eliminate competing energy source. Ban secures market dominance and defers costly grid transition investments. Experiences constraint as pure coordination: alignment with government preference = protected market position. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE COALITION AND PUBLIC INTEREST GROUPS (TANGLED ROPE) — Coordination function: global climate mitigation requires US participation in low-carbon energy transition (coordination gain). Extraction mechanism: ban suppresses this coalition's policy leverage while benefiting fossil fuel incumbents who fund counter-messaging. Coalition is constrained by polarization and limited near-term exit options. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.56. Mixed: genuine coordination need but severe extraction from ban.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE ADMINISTRATION (PITON) — Ban is framed as 'energy independence' and 'manufacturing protection' but functions largely as rent-protection for incumbents. Theater ratio high: rhetoric about domestic energy production and job protection; functional reality is market foreclosure. theater_ratio=0.58 approaches piton threshold; rising toward 0.70. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04. Administration sees itself as coordinator but progressively as theatrical as functional justifications erode.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW / FALSE SUMMIT) — Risks naturalizing ban as inevitable outcome of sovereign energy policy ('every nation must choose its own energy mix'). From civilizational scale, could appear as an immutable national interest constraint. However, structural data (ε=0.68, suppression=0.75, theater=0.58) contradicts mountain classification. Engine detects false summit: the ban is contingent political choice, not natural law. Real constraint is political (snare with organized resistance), not civilizational.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.68): High. The ban transfers market share and capital flows from renewable developers to fossil fuel incumbents over a 5-10 year horizon. The extraction is measurable: prevented wind projects represent ~150-200 GW of forgone capacity, valued at $150-250 billion in lifetime generation and economic activity. The value is extracted rather than destroyed because it flows to incumbent producers (higher fossil fuel demand) and their suppliers. Not maximal (0.95) because alternative renewable pathways (solar, geothermal, distributed) offer partial substitutes and exit options. Suppression (0.75): High. Regulatory ban is near-total prohibition with no legitimate pathway to wind development within national territory. Exit options: developers can pursue foreign projects (capital controls, tariffs, financing barriers); distributed solar (expensive, time-consuming permitting); grid operators can use alternative sources (constrained by cost and availability). Suppression is not 0.90+ because some legal exits exist and enforcement burden is finite. Theater ratio (0.58, rising): Moderate-high. Ban is justified using 'energy independence,' 'manufacturing protection,' 'grid stability' rhetoric. These narratives have reduced empirical grounding as grid studies show wind integration feasibility, supply chains stabilize, and energy independence motive conflicts with continued fossil fuel imports. Theater rises over interval because initial framing (2025) has higher surface plausibility; by 2027 (time_point=12), performance gaps between rhetoric and evidence create rising gap (theater approaches piton-threshold 0.70).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Fossil fuel incumbents see coordination (alignment with government = protected market). Renewable developers see snare (total foreclosure). Climate coalition sees tangled rope (genuine coordination need for decarbonization + severe extraction from ban). Grid operators see snare (forced suboptimality). Executive sees piton (degrading theater as justifications erode). Analytical observer risks mountain (natural law frame). The perspectival gap reveals that the same structural mechanism appears benign (coordination), mixed (tangled rope), severe (snare), and even natural (mountain) depending on the observer's position and beneficiary/victim status. This is the core diagnostic function of the DR framework: to show that classification is not observer-independent but precisely indexed to structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil fuel producers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Low directionality; net beneficiaries experience ban as coordination, not extraction. Renewable developers: Victim + trapped → d≈0.96, f(d)≈1.40. Maximum directionality; cannot exit US jurisdiction or circumvent regulatory ban. d approaches 1.0. Climate coalition: Victim + constrained → d≈0.78, f(d)≈1.12. High directionality; some exit options (international advocacy, distributed energy) but major constraints (polarization, regulatory suppression). Grid operators: Victim + constrained → d≈0.85, f(d)≈1.15. High directionality; constrained to suboptimal alternatives. Executive administration: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Aligned with primary beneficiary (fossil fuels); frames ban as national coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The ban resolves mandatrophy by demonstrating how a single regulatory structure can function as extraction (snare) for some actors and coordination for others. The mandate is NOT to classify the ban as a single type, but to show that snare is the dominant structural reality: the ban forecloses a legitimate alternative energy source to protect incumbent rents. The coordination perspective (fossil fuel incumbent) is subordinate: it exists but it depends on suppressing superior alternatives. If the suppression were removed, the coordinate alignment would collapse — incumbents would not voluntarily choose higher-cost fossil fuels if renewables were available at lower cost. This reveals the snare: the coordination is artificial, maintained only by regulatory foreclose. The theater rising from 0.48 to 0.58 shows drift toward piton-degradation: the ban's functional purpose (rent protection) is increasingly divorced from its stated purpose (energy independence, manufacturing). By civilizational scale, the ban becomes visible as a contingent extraction mechanism rather than an inevitable natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_substitution_success,
    'Can US energy grid achieve decarbonization targets (50%+ by 2035) without wind, using only solar and storage, while maintaining grid stability and affordability?',
    'Grid modeling with binding renewable percentage targets; comparison of least-cost pathways with and without wind across decarbonization scenarios; empirical monitoring of grid reliability metrics and energy prices under ban conditions',
    'If yes: ban may have limited climate impact (other renewables substitute); snare classification weakens. If no: ban creates binding climate constraint; snare classification strengthens and extraction becomes civilizational-scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_success, empirical, 'Whether alternative renewables can substitute for wind in decarbonization pathway').

omega_variable(
    ban_reversibility_horizon,
    'What is the expected duration of the ban? Is it a permanent policy instrument or a temporary restriction subject to future reversal?',
    'Legislative tracking; analysis of enabling statute (executive order vs law); monitoring of political coalitions and decarbonization cost trajectories; comparison to similar historical bans and their typical durations',
    'If temporary (< 5 years): scaffold perspective gains credibility; constraint may resolve to tangled rope. If permanent: snare classification is durable; extraction mechanism persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ban_reversibility_horizon, preference, 'Duration and reversibility of the executive ban').

omega_variable(
    international_retaliation_scope,
    'Will trading partners (EU, Canada, Mexico) impose tariffs or trade restrictions on US energy imports and exports in response to climate policy inconsistency?',
    'Trade agreement monitoring; tariff schedule reviews; analysis of carbon border adjustment mechanisms; historical precedent from other US energy/climate policy reversals',
    'If extensive retaliation: extraction costs rise for fossil fuel beneficiaries; snare structure may collapse under trade pressure. If minimal retaliation: beneficiaries retain gains; snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_retaliation_scope, empirical, 'International trade response to US wind ban').

omega_variable(
    distributed_energy_adaptation,
    'Do distributed rooftop solar and community-scale geothermal projects proliferate in response to centralized wind ban, creating a decentralized renewable pathway?',
    'Empirical monitoring of distributed energy deployment rates, permitting timelines, and financing availability; comparison of pre-ban vs post-ban adoption curves; analysis of regulatory barriers to distributed alternatives',
    'If proliferation occurs: victims gain exit option; constraint shifts from snare toward tangled rope. If suppressed: snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_energy_adaptation, empirical, 'Distributed energy adaptation circumventing centralized wind ban').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_wind_project_ban_2025, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wind_ban_tr_t0, us_wind_project_ban_2025, theater_ratio, 0, 0.48).
narrative_ontology:measurement(wind_ban_tr_t6, us_wind_project_ban_2025, theater_ratio, 6, 0.54).
narrative_ontology:measurement(wind_ban_tr_t12, us_wind_project_ban_2025, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(wind_ban_be_t0, us_wind_project_ban_2025, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(wind_ban_be_t6, us_wind_project_ban_2025, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(wind_ban_be_t12, us_wind_project_ban_2025, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_wind_project_ban_2025, resource_allocation).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, grid_decarbonization_target_2035).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, trade_carbon_border_adjustment).

% DUAL FORMULATION NOTE:
% The US wind ban is downstream of fossil fuel rent-protection incentives and upstream of grid decarbonization constraints. The structural constraint (snare) manifests simultaneously as energy policy, industrial policy, and political coordination between executive and incumbent energy producers. Related constraints: grid decarbonization (faces binding constraint from ban), fossil subsidy regime (ban is complementary enforcement mechanism), trade retaliation (carbon-aware trading partners respond to policy inconsistency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_wind_project_ban_2025, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
