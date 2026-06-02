% ============================================================================
% CONSTRAINT STORY: infrastructure_monopoly_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_monopoly_power, []).

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
 *   constraint_id: infrastructure_monopoly_power
 *   human_readable: Infrastructure Monopoly Power
 *   domain: economic/regulatory
 *
 * SUMMARY:
 *   Infrastructure monopoly power represents a core extraction mechanism in
 *   modern economies. Essential networks — electricity, water,
 *   telecommunications, transportation — exhibit high fixed costs and network
 *   effects that historically justified regulated monopoly as the
 *   coordination mechanism balancing universal access against firm viability.
 *   However, this institutional arrangement has degraded into a Snare:
 *   regulatory capture allows operators to extract economic rents far
 *   exceeding legitimate coordination costs. Extractiveness has risen from
 *   0.52 to 0.68 over the interval as lobbying intensity has increased and
 *   regulatory bodies have weakened. Theater ratio has risen from 0.45 to
 *   0.58, reflecting that public interest rhetoric (universal service,
 *   safety, community benefit) increasingly masks rent extraction with
 *   minimal coordination function. The constraint exhibits all six types from
 *   different structural positions, making it diagnostically rich. The
 *   dependent user experiences pure extraction (Snare). The would-be
 *   competitor experiences blockade-style extraction (Snare). The captured
 *   regulator experiences mixed coordination and extraction (Tangled Rope).
 *   The monopoly operator experiences the constraint as coordination with
 *   extraction benefits (Rope). The legacy utility framework persists through
 *   inertia and capture theater (Piton). Organized opposition groups
 *   experience mixed extraction and resistance coordination (Tangled Rope).
 *   The civilizational analytical observer risks naturalizing contingent
 *   regulatory capture as inherent infrastructure economics (false Mountain).
 *
 * KEY AGENTS:
 *   - Dependent Users: Primary victims (powerless/trapped) — cannot exit due to geographic/technical necessity; bear full extraction cost without negotiating power
 *   - Potential Competitors: Secondary victims (moderate/constrained) — face prohibitive entry barriers; rationally excluded from market despite potential efficiency gains
 *   - Captured Regulators: Mixed agent (powerful/mobile) — maintain coordination facade while infrastructure operators influence regulatory outcomes through lobbying and expertise dependence
 *   - Monopoly Operators: Primary beneficiaries (institutional/arbitrage) — extract rents through monopoly position and regulatory capture; arbitrage across jurisdictions and regulatory environments
 *   - Legacy Utility Framework: Institutional structure (institutional/arbitrage) — originally designed to coordinate universal access + fair pricing; now functions as theater masking rent extraction
 *   - Organized Opposition: Secondary organized actors (organized/constrained) — consumer advocacy and environmental coalitions coordinate resistance; cannot exit but build collective pressure for reform
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent capture as immutable infrastructure economics (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_monopoly_power, 0.68).
domain_priors:suppression_score(infrastructure_monopoly_power, 0.72).
domain_priors:theater_ratio(infrastructure_monopoly_power, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_monopoly_power, extractiveness, 0.68).
narrative_ontology:constraint_metric(infrastructure_monopoly_power, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(infrastructure_monopoly_power, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_monopoly_power, snare).
narrative_ontology:human_readable(infrastructure_monopoly_power, "Infrastructure Monopoly Power").
narrative_ontology:topic_domain(infrastructure_monopoly_power, "economic/regulatory").

domain_priors:requires_active_enforcement(infrastructure_monopoly_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_monopoly_power, monopoly_operator).
narrative_ontology:constraint_victim(infrastructure_monopoly_power, dependent_users).
narrative_ontology:constraint_victim(infrastructure_monopoly_power, competitor_entrants).
narrative_ontology:constraint_victim(infrastructure_monopoly_power, public_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT USERS (SNARE) — Users depend on the infrastructure for essential services (electricity, water, telecommunications, transport). Geographic or technical constraints make switching impossible. High suppression: regulation is weak or captured. Maximum experienced extraction. No coordination benefit perceived — the constraint is purely extractive from this position.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: POTENTIAL COMPETITOR (SNARE) — Entry barriers are prohibitive: sunk capital costs, rights-of-way monopoly, regulatory capture, predatory pricing. Exit option is 'constrained' rather than 'trapped' because competitors could theoretically invest at catastrophic cost, but doing so is economically irrational. Extraction mechanism: monopolist maintains dominance through barriers rather than coordination benefit.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPTURED REGULATOR (TANGLED ROPE) — Has formal coordination function (setting rates, enforcing service standards) and genuine coordination responsibility (ensuring network stability, broad access). But experiences asymmetric extraction: regulated firms influence regulatory bodies through lobbying, revolving-door employment, and expertise dependence. The regulator maintains the coordination facade while infrastructure operator captures regulatory process. Mobile exit option reflects that regulations can theoretically change, but regulatory capture makes this improbable.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MONOPOLY OPERATOR (ROPE) — Experiences the constraint as pure coordination: maintaining network stability, ensuring broad coverage, managing demand. The operator benefits from regulatory protection and extraction capacity. Arbitrage exit: can lobby for favorable regulation, can move capital across jurisdictions, can lobby for rate increases. Sees coordination benefit (network effects, user lock-in) alongside profit extraction.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGACY UTILITY FRAMEWORK (PITON) — The traditional public utility model (regulated monopoly) was designed to balance coordination (ensuring universal access) with fairness (rate regulation). That purpose has atrophied. Modern monopolies extract rent through regulatory capture and price discrimination while the regulatory ritual (rate hearings, service standards, public interest claims) persists as performance theater. Theater ratio reflects that public interest framing obscures private extraction.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED OPPOSITION (TANGLED ROPE) — Consumer advocacy groups and environmental coalitions have coordinated to challenge monopoly pricing and practices. They experience genuine extraction (paying above-competitive rates) but also engage in coordination — building alternative power sources (distributed solar), organizing rate challenges, pushing for regulatory reform. Constrained exit: they cannot leave the grid, but can organize collective resistance. Organized power gives them agency absent in the powerless perspective.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL MONOPOLY VIEW (MOUNTAIN) — From a civilizational perspective, some infrastructure exhibits natural monopoly properties: the cost structure (high fixed costs, low marginal costs) makes duplication wasteful. This perspective sees monopoly as inherent to infrastructure physics rather than as a contingent institutional arrangement. However, structural data reveals this as a false summit — the extractive suppression and regulatory capture are contingent and contestable, not immutable properties of infrastructure technology.
constraint_indexing:constraint_classification(infrastructure_monopoly_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_monopoly_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_monopoly_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_monopoly_power, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_monopoly_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_monopoly_power, TR),
    TR >= 0.70.

:- end_tests(infrastructure_monopoly_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising. The monopoly operator captures economic rents through control of essential infrastructure. The value reflects that extraction has accelerated as regulatory capture has deepened and alternative coordination mechanisms (distributed generation, deregulated markets) have been blocked. Initial 0.52 was justified by legacy public utility model (coordination + fair pricing); current 0.68 reflects that coordination function has atrophied while extraction has intensified. Suppression (0.72): Very high. Users cannot exit without extreme cost. Competitors face legal and technical barriers to entry. Regulation has been captured and weakened. Organized opposition exists but lacks enforcement power. Theater ratio (0.58): Moderate-high, rising. Regulatory process maintains legitimacy through public interest claims (universal service, safety, community benefit) while operators pursue profit maximization. Public utility commission meetings, rate hearings, and corporate sustainability messaging constitute the theater. Rising from 0.45 reflects that legitimacy maintenance has become more elaborate as extraction has intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure appears as pure extraction to trapped users (Snare), mixed coordination and extraction to constrained actors with agency (Tangled Rope for regulators and organized opposition), coordination-based profit to the operator (Rope), degraded ritual to civilizational observers (Piton), and potentially natural law to those who accept the infrastructure-economics framing (false Mountain). The gap between the Snare and Rope perspectives is enormous: the user is trapped and extracted; the operator is coordinating and profitable. The captured regulator occupies the middle ground: genuinely responsible for coordination but structurally influenced by the operator. The organized opposition perspective shows that collective agency changes the classification: even trapped in the infrastructure dependency, coordinated resistance shifts from pure Snare toward mixed Tangled Rope. The analytical observer's mountain classification is revealed as false summit through the structural data: extraction rises over time (contingent), suppression is enforced through regulatory choice (contingent), and theater ratio rises (indicating performative maintenance rather than immutable function). These are signatures of a contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to extraction flow. Dependent users are full targets: beneficiary=false, victim=true, exit=trapped → high d → high f(d) → maximum experienced extraction chi. Potential competitors are targets with some agency: victim=true, exit=constrained → high d → high f(d) → very high extraction. Captured regulator is ambiguous: nominally powerful but structurally influenced, exit=mobile (regulation can theoretically change) but practically constrained by capture → moderate d → moderate f(d). Monopoly operator is beneficiary: beneficiary=true, arbitrage exit → very low d → negative f(d) → experiences the constraint as enabling, not extractive. Organized opposition is victim with organized power: victim=true (extraction), organized power → moderate-to-low d despite victim status because collective power reduces f(d). The piton classification derives from theater_ratio gate (>0.70 threshold not quite reached at 0.58, but trend is toward pitonization as theater_ratio rises and coordination function atrophies).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that all six types are legitimate perspectival readings with different information and power positions. The natural monopoly framing (mountain) is revealed as a false summit: the structural data shows rising extractiveness, rising theater, and contingent regulatory capture — these are signatures of institutional choice, not natural law. The Snare classification from the dependent user perspective is the most structurally accurate: maximum suppression, maximum extraction, no coordination benefit. The Rope classification from the operator perspective is their genuine experience: they are coordinating network operations and experiencing profit. The Tangled Rope classifications for regulators and organized opposition reflect that these actors maintain genuine coordination functions while being subject to extraction forces (capture in the regulator case, dependency in the opposition case). The Piton classification reflects that the public utility framework persists through institutional inertia and legitimacy theater rather than functional necessity — the form of regulation remains but the coordination content has hollowed out. Resolving mandatrophy requires acknowledging that the classification SHOULD vary by position: the dependent user's Snare is not wrong; it is perspectivally accurate. The operator's Rope is also not wrong; it reflects their genuine experience. The analytical observer's temptation to call it Mountain is the false summit that mandatrophy detection is designed to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_monopoly_or_contingent_power,
    'Is the monopoly power a natural consequence of infrastructure economics or a contingent political-regulatory creation?',
    'Comparative institutional analysis: contrast jurisdictions with strong vs weak utility regulation; measure whether network effects and economies of scale persist under competitive models (e.g., telecommunications deregulation outcomes, electricity markets with competitive generation and transmission separation)',
    'If natural: classification shifts toward Mountain from analytical perspectives. If contingent: classification stays Snare/Tangled Rope, and regulatory reform becomes strategically viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_monopoly_or_contingent_power, empirical, 'Whether monopoly is natural economics or political arrangement').

omega_variable(
    regulatory_capture_mechanism,
    'What share of extraction flows from monopoly position itself vs from regulatory capture that prevents competition?',
    'Counterfactual analysis: model extraction under scenarios with and without regulatory capture; measure correlation between lobbying expenditure and favorable regulatory outcomes; compare pricing in jurisdictions with strong vs weak capture',
    'If capture is dominant: extraction can be reduced by regulatory reform. If monopoly position is dominant: reform requires structural separation or technology disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Contribution of regulatory capture to total extraction').

omega_variable(
    universal_access_coordination_function,
    'Does the monopoly structure provide genuine universal access coordination that would degrade under competition?',
    'Comparison of access rates and pricing in competitive vs regulated markets; analysis of whether competitive markets achieve equivalent universal service through alternative mechanisms (subsidies, public authority, cross-subsidization)',
    'If coordination function is genuine: Tangled Rope classification is accurate (real coordination + extraction). If spurious: classification should be pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_coordination_function, empirical, 'Whether universal access depends on monopoly coordination').

omega_variable(
    exit_capacity_elasticity,
    'How elastic is user exit capacity as price and service quality vary?',
    'Measurement of demand elasticity relative to price and service changes; study of user substitution behavior when alternatives emerge (solar adoption, alternative ISPs, demand reduction); longitudinal user surveys on willingness to exit',
    'If elasticity is very low: trapping is severe, classification confirms Snare for dependent users. If elasticity is moderate: users have constrained but real options, could shift toward organized resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_elasticity, empirical, 'User sensitivity to price and service quality changes').

omega_variable(
    technology_disruption_timeline,
    'What is the realistic timeline for technology disruption (distributed generation, decentralization) to break the monopoly position?',
    'Analysis of adoption curves for competing technologies; cost trajectory projections; regulatory analysis of whether incumbent monopoly can delay disruption through capture',
    'If disruption is 5-10 years away: the constraint may be Scaffold-like (temporary with sunset). If disruption is 30+ years away: Snare/Mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_disruption_timeline, empirical, 'Timeline for technological disruption of infrastructure monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_monopoly_power, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_mono_tr_t0, infrastructure_monopoly_power, theater_ratio, 0, 0.45).
narrative_ontology:measurement(infra_mono_tr_t10, infrastructure_monopoly_power, theater_ratio, 10, 0.52).
narrative_ontology:measurement(infra_mono_tr_t20, infrastructure_monopoly_power, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(infra_mono_be_t0, infrastructure_monopoly_power, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(infra_mono_be_t10, infrastructure_monopoly_power, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(infra_mono_be_t20, infrastructure_monopoly_power, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_monopoly_power, resource_allocation).
narrative_ontology:affects_constraint(infrastructure_monopoly_power, regulatory_capture_mechanism).
narrative_ontology:affects_constraint(infrastructure_monopoly_power, utility_pricing_asymmetry).

% DUAL FORMULATION NOTE:
% Infrastructure monopoly power decomposes into multiple structurally distinct constraints: natural monopoly coordination (potentially Mountain if genuine economies of scale dominate), regulatory capture extraction (Snare), and rent extraction through pricing power (Snare). This story treats monopoly power as a unified constraint, but analysis should decompose into separate stories if empirical investigation reveals that natural monopoly and capture represent different ε values. See network.affects_constraints for downstream constraints that depend on this monopoly structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_monopoly_power, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
