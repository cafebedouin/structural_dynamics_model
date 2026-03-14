% ============================================================================
% CONSTRAINT STORY: colony_autonomy_vs_earth_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colony_autonomy_vs_earth_governance, []).

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
 *   constraint_id: colony_autonomy_vs_earth_governance
 *   human_readable: Colony Autonomy vs Earth Governance
 *   domain: geopolitical/institutional
 *
 * SUMMARY:
 *   Colony-Earth governance relationships exemplify how a single constraint
 *   can instantiate across all six classification types depending on the
 *   observer's structural position. The constraint operates at the
 *   intersection of genuine coordination necessity (planetary problems
 *   require some integration) and asymmetric extraction (Earth captures
 *   resource flows and political control). This story models how colonies
 *   transition from purely extractive (Snare) arrangements toward federated
 *   autonomy (Scaffold) while Earth authority maintains the system through
 *   mixed coordination and enforcement (Tangled Rope). The measurements show
 *   extractiveness rising over 60 time units (years) as administrative
 *   complexity increases, while theater ratio remains moderate — the system
 *   retains functional coordination even as extraction accumulates. The core
 *   tension: Earth legitimates centralization through coordination necessity
 *   claims; colonies perceive primarily extraction; analytical observers risk
 *   naturalizing political dominance as immutable law.
 *
 * KEY AGENTS:
 *   - Colonial Populations: Primary victim (powerless/trapped) — lack exit options, bear resource extraction and administrative subordination
 *   - Earth Central Authority: Primary beneficiary (institutional/arbitrage) — captures resources, maintains geopolitical control, experiences constraint as coordination mechanism
 *   - Colonial Administrative Elites: Secondary beneficiary and victim (institutional/constrained) — extract from local populations while remaining subordinate to Earth authority
 *   - Colonial Regional Leadership: Mixed actor (moderate/constrained) — some delegated power and local rents, but constrained by Earth oversight
 *   - Independence Movement Coalition: Organized agents (organized/mobile) — perceive autonomy as achievable through federation, see current system as temporary
 *   - Legacy Colonial Administration: Institutional inertia (institutional/constrained) — maintains oversight ritual even as functional necessity declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political arrangements as necessary coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colony_autonomy_vs_earth_governance, 0.58).
domain_priors:suppression_score(colony_autonomy_vs_earth_governance, 0.65).
domain_priors:theater_ratio(colony_autonomy_vs_earth_governance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colony_autonomy_vs_earth_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(colony_autonomy_vs_earth_governance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(colony_autonomy_vs_earth_governance, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colony_autonomy_vs_earth_governance, tangled_rope).
narrative_ontology:human_readable(colony_autonomy_vs_earth_governance, "Colony Autonomy vs Earth Governance").
narrative_ontology:topic_domain(colony_autonomy_vs_earth_governance, "geopolitical/institutional").

domain_priors:requires_active_enforcement(colony_autonomy_vs_earth_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colony_autonomy_vs_earth_governance, earth_central_authority).
narrative_ontology:constraint_beneficiary(colony_autonomy_vs_earth_governance, colonial_administrative_elites).
narrative_ontology:constraint_victim(colony_autonomy_vs_earth_governance, colonial_populations).
narrative_ontology:constraint_victim(colony_autonomy_vs_earth_governance, resource_bearing_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIAL POPULATION (SNARE) — Trapped by legal dependency, resource monopolization, and lack of exit pathways. Cannot migrate freely, cannot establish alternative governance, cannot access resources without permission. Bears full extraction through resource extraction, labor requirements, and political subordination. No meaningful coordination benefit flows to this agent.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COLONIAL REGIONAL LEADERSHIP (TANGLED ROPE) — Constrained by Earth authority approval requirements and resource dependency, but exercises delegated administrative power and captures local rents. Benefits from coordination of local resources while bearing oversight costs and restricted autonomy. High extraction but some agency through administrative roles.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EARTH CENTRAL AUTHORITY (ROPE) — Benefits from resource flows, geopolitical control, and reduced competition from colony sovereignty. Experiences constraint as solving coordination problem: unified resource management across dispersed colonies. Low extraction experienced by beneficiary with arbitrage options.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLONIAL ADMINISTRATIVE ELITES (TANGLED ROPE) — Extract rents from local populations while remaining subordinate to Earth authority. Coordinate local resource flows and governance while bearing constraints on autonomy and policy choices. Dual position: coordinator of colonial systems, extractor from colonial populations, subordinate to Earth center. Significant asymmetric extraction embedded within coordination framework.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGACY COLONIAL ADMINISTRATION (PITON) — Governance structures inherited from initial colonization now operate largely on institutional momentum. Actual functional necessity of Earth-based authorization has declined with local institutional maturity, yet formal approval requirements persist. Theater ratio 0.48 reflects genuine coordination function mixed with vestigial oversight ritual. System persists through inertia despite reduced functional justification.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INDEPENDENCE MOVEMENT COALITION (SCAFFOLD) — Organized agents (colonial liberation movements, federated colony councils, diaspora networks) perceive autonomy constraint as a temporary coordination failure with a sunset: self-governance frameworks, intercolony compacts, and mutual defense arrangements can replace Earth-centered coordination. High suppression countered by coalition agency and visible exit pathway. Classified as scaffold with implicit sunset logic.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some centralized coordination may appear structurally necessary for planetary-scale problems: climate, pandemics, interstellar commerce, existential risk. This perspective risks naturalizing contingent political arrangements as immutable requirements for civilization-level survival. Engine flags this as false summit: the 'necessity' claim requires empirical verification against alternative decentralized coordination mechanisms.
constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colony_autonomy_vs_earth_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colony_autonomy_vs_earth_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colony_autonomy_vs_earth_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colony_autonomy_vs_earth_governance, TR),
    TR >= 0.70.

:- end_tests(colony_autonomy_vs_earth_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. Initial value reflects genuine coordination of resource flows and basic administrative integration (0.42). By year 60, extractiveness has accumulated to 0.62 as Earth authority layers on additional regulatory requirements, revenue demands, and policy constraints without proportional benefit to colonies. The trajectory reflects ratchet dynamics: new extraction mechanisms are added (carbon quotas, technology transfer requirements, security fees) but never removed. Suppression (0.65): High. Barriers to colonial autonomy include military dependency (Earth controls security apparatus), economic leverage (Earth monopolizes space trade), institutional lock-in (legal frameworks require Earth approval), and information control. Most suppression is structural rather than internalized — colonies perceive barriers as external. Theater (0.48): Moderate. The system retains genuine coordination function (resource allocation, intercolony dispute resolution, environmental regulation) but increasingly embeds performative elements (approval rituals that no longer affect decisions, regulatory committees with no actual authority). Theater is lower than pure Piton because coordination remains functionally necessary.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival spread: Snare (victim view), Tangled Rope (mixed views), Rope (beneficiary view), Scaffold (organized agent view), Piton (institutional inertia view), and Mountain (false analytical view). This range reveals the constraint is fundamentally contested — no single type captures the structural reality from all positions. The beneficiary (Earth) experiences coordination (Rope); the victim experiences extraction (Snare); the middle agent experiences both mixed (Tangled Rope). The analytical observer's Mountain classification is a false summit — it naturalizes the political arrangement by framing it as necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position. Earth authority as institutional/arbitrage beneficiary derives d ≈ 0.10 (low, negative chi). Colonial populations as powerless/trapped victims derive d ≈ 0.92 (high, maximum chi). Colonial elites as institutional/constrained actors derive d ≈ 0.55 (mixed, moderate chi). Independence movements as organized/mobile agents derive d ≈ 0.60 (above symmetric, but coalition power moderates extraction). The administrative system as institutional/constrained derives d ≈ 0.50 from its role as both beneficiary (maintains power) and victim (constrained by legacy requirements). Directionality gaps between beneficiaries and victims drive the perspectival disagreement on classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint exemplifies how mandatrophy dissolves when perspectival multiplicity is acknowledged. The question 'is this pure extraction or genuine coordination?' has different answers depending on position. Earth correctly perceives coordination function; colonies correctly perceive extraction; both are structurally accurate. The mandatrophy is not 'which type is correct?' but 'which position are we measuring from?' The classification Tangled Rope is correct at the system level precisely because extraction and coordination coexist structurally. The Snare classification from the powerless perspective is not false — it is the victim's accurate experience of a mixed system where their share of coordination benefit is near-zero. Resolution requires either: (1) accepting that the constraint has multiple valid classifications from different positions (presheaf model), or (2) decomposing the constraint into separate coordination and extraction stories with different ε values. The current story uses model (1): Tangled Rope as the system-level classification, with perspectival variance as diagnostic feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What planetary-scale coordination functions genuinely require centralized Earth authority versus can be delivered through federation of autonomous colonies?',
    'Comparative analysis of coordination mechanisms: empirical performance data on decentralized climate coordination, pandemic response, trade regulation, and conflict prevention in alternative governance structures (historical federations, city-state networks, consortium models)',
    'If threshold is high (few genuine necessities): autonomy constraint classifies as Snare for broader population. If threshold is low (extensive genuine necessities): constraint reclassifies as higher-extraction Tangled Rope with legitimate coordination component. Current ambiguity sustains both extraction and legitimate authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Extent of coordination functions requiring centralized authority').

omega_variable(
    resource_scarcity_structural,
    'Is resource scarcity that justifies Earth control an intrinsic physical constraint or a manufactured scarcity maintained through centralized monopoly?',
    'Analysis of resource access patterns: comparison of extraction efficiency under different governance models; testing of decentralized resource management protocols; assessment of whether scarcity rhetoric persists when actual abundance conditions obtain',
    'If intrinsic: suppression reflects genuine material limits, classification stable. If manufactured: suppression is enforcement mechanism for artificial scarcity, extractiveness reclassifies upward, constraint shifts toward Snare pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_scarcity_structural, empirical, 'Whether resource scarcity justifying control is structural or manufactured').

omega_variable(
    exit_option_feasibility,
    'Can colonies realistically achieve autonomy through federation, secession, or unilateral sovereignty assertion, or are exit barriers genuinely insurmountable?',
    'Scenario analysis: assessment of military, economic, technological, and diplomatic barriers to colonial exit; historical precedent analysis of successful and failed independence transitions; structural modeling of trade dependency and resource leverage',
    'If barriers are surmountable: exit_options upgrade from trapped to constrained or mobile for colonial populations; classification shifts from Snare toward Tangled Rope. If barriers are insurmountable: confirms Snare classification; suppression metric validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Feasibility of colonial exit through autonomy').

omega_variable(
    identity_lock_mechanism,
    'Do colonial populations internalize Earth governance as a natural/necessary system despite structural exit options, or are perceptions of necessity driven by material barriers?',
    'Ethnographic analysis of colonial identity narratives; comparative study of populations with different material constraints; measurement of identity fusion with Earth-governance frames versus instrumental acceptance; post-exposure analysis when material barriers are reduced',
    'If identity-locked: constraint operates partly through cognitive capture, supporting higher theater ratio and Piton perspective. If purely material: identity is instrumental, theater ratio overstated, populations perceive constraint as externally imposed Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether colonial subordination is identity-locked or structurally enforced').

omega_variable(
    federation_coordination_cost,
    'Would decentralized federation require transaction costs comparable to or higher than centralized Earth authority, offsetting autonomy gains?',
    'Comparative institutional analysis of federation overhead; empirical data on governance costs from successful federated systems (pre-colonial state networks, modern federated democracies); modeling of intercolony coordination mechanisms for public goods provision',
    'If federation costs are comparable: autonomy constraint reclassifies as Scaffold with genuine sunset to federation. If federation costs are prohibitive: coordination justification for constraint strengthens, suppression becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_coordination_cost, empirical, 'Transaction costs of federation versus centralized authority').

omega_variable(
    power_consolidation_dynamics,
    'Does centralized Earth authority prevent power consolidation or facilitate it? Could colony independence trigger more extractive warlordism or predatory regional powers?',
    'Historical analysis of post-colonial power dynamics; comparative study of security outcomes in decentralized versus centralized systems; modeling of conflict escalation patterns under different governance structures; assessment of whether current authority prevents or enables elite capture',
    'If current authority prevents worse extraction: constraint reclassifies as Tangled Rope with security coordination justified. If current authority enables elite capture: suppression mechanism is political control masquerading as security, constraint shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_consolidation_dynamics, empirical, 'Whether centralization prevents or enables power consolidation and extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colony_autonomy_vs_earth_governance, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colo_tr_t0, colony_autonomy_vs_earth_governance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(colo_tr_t30, colony_autonomy_vs_earth_governance, theater_ratio, 30, 0.45).
narrative_ontology:measurement(colo_tr_t60, colony_autonomy_vs_earth_governance, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(colo_be_t0, colony_autonomy_vs_earth_governance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(colo_be_t30, colony_autonomy_vs_earth_governance, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(colo_be_t60, colony_autonomy_vs_earth_governance, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colony_autonomy_vs_earth_governance, resource_allocation).
narrative_ontology:affects_constraint(colony_autonomy_vs_earth_governance, intercolony_trade_dependency).
narrative_ontology:affects_constraint(colony_autonomy_vs_earth_governance, resource_scarcity_enforcement).
narrative_ontology:affects_constraint(colony_autonomy_vs_earth_governance, earth_military_monopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colony_autonomy_vs_earth_governance, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
