% ============================================================================
% CONSTRAINT STORY: matching_markets_general
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_matching_markets_general, []).

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
 *   constraint_id: matching_markets_general
 *   human_readable: Matching Markets (Non-Commodity Exchange)
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Matching markets—labor markets, dating platforms, housing rentals, organ
 *   allocation—create a structural tension between coordination and
 *   extraction. Unlike commodity markets where price aggregates preferences
 *   into a single clearing mechanism, matching markets require algorithms to
 *   approximate mutual preference and orchestrate bilateral outcomes. This
 *   creates an asymmetric structural position: platforms control both the
 *   information (what preferences are revealed and visible to whom) and the
 *   algorithm (how preferences map to matches). The constraint exhibits all
 *   six DR types depending on the observer's structural relationship to the
 *   platform. For agents excluded by the algorithm or locked into a platform
 *   with poor matches, the market is a snare with no exit. For matched
 *   participants, it is a mixed rope-snare hybrid where coordination benefits
 *   are offset by information asymmetry and switching costs. For platform
 *   operators, it is pure coordination—the matching function is their value
 *   proposition. For regulators, matching market oversight is largely
 *   theater: rules written for price-clearing commodity markets don't fit
 *   thick preference structures, and algorithm transparency remains
 *   voluntary. For standards advocates, the constraint is a temporary
 *   scaffolding being replaced by portability protocols. For the
 *   civilizational observer, mutual preference matching is an irreducible
 *   feature—but this risks naturalizing what is actually a design choice:
 *   whether the platform is open, transparent, and interoperable or closed,
 *   proprietary, and lock-in prone.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — control algorithm, capture information rents, benefit from network effects and lock-in
 *   - Excluded/Low-Signal Agents: Primary victim (powerless/trapped) — limited choice set, no transparency into matching algorithm, cannot access alternative markets
 *   - Matched Participants: Secondary victim (moderate/constrained) — benefit from liquidity but constrained by lock-in, information asymmetry about match quality relative to alternatives
 *   - Regulators: Institutional actor (institutional/constrained) — mandates are performative; cannot enforce real algorithm transparency or interoperability without platform cooperation
 *   - Open Standards Coalition: Organized agents (organized/constrained) — portability protocols, API standardization efforts to reduce lock-in and enable switching
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform dominance as structural necessity rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_markets_general, 0.38).
domain_priors:suppression_score(matching_markets_general, 0.52).
domain_priors:theater_ratio(matching_markets_general, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_markets_general, extractiveness, 0.38).
narrative_ontology:constraint_metric(matching_markets_general, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(matching_markets_general, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(matching_markets_general, tangled_rope).
narrative_ontology:human_readable(matching_markets_general, "Matching Markets (Non-Commodity Exchange)").
narrative_ontology:topic_domain(matching_markets_general, "economic/social/technological").

domain_priors:requires_active_enforcement(matching_markets_general).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(matching_markets_general, platform_operators).
narrative_ontology:constraint_beneficiary(matching_markets_general, high_preference_agents).
narrative_ontology:constraint_victim(matching_markets_general, low_preference_agents).
narrative_ontology:constraint_victim(matching_markets_general, market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED AGENT (SNARE) — Agents with low preference signals or limited access cannot exit; face structural extraction through limited choice set and asymmetric information about alternatives. No transparent substitute markets. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(matching_markets_general, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MATCHED PARTICIPANT (TANGLED ROPE) — Participants benefit from liquidity and coordination function (finds compatible counterpart) but constrained by algorithm transparency limits and lock-in effects. Extraction occurs through information asymmetry about match quality. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(matching_markets_general, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from network effects and data advantages. Experiences constraint as pure coordination problem: solving matching is their core function. Can arbitrage between different market designs. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.04. Negative effective extraction = net beneficiary through coordination rent.
constraint_indexing:constraint_classification(matching_markets_general, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (PITON) — Regulatory frameworks (antitrust, data protection, disclosure) for matching markets are largely performative or degraded: rules written for commodity markets don't fit thick preference structures; enforcement is theater because regulators lack algorithmic transparency. theater_ratio=0.58 approaches piton threshold. Constrained by inability to mandate real-time algorithm inspection. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.39.
constraint_indexing:constraint_classification(matching_markets_general, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN STANDARDS COALITION (SCAFFOLD) — Organized efforts (portable matching algorithms, API standardization, preference portability protocols) aim to reduce lock-in and create alternative matching pathways. Temporary support for transition from closed platforms to decentralized/interoperable designs. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.21. Low extraction because coalition has exit mechanism (portability) and sunset timeline (standards adoption).
constraint_indexing:constraint_classification(matching_markets_general, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational scope, mutual preference matching is an irreducible feature of thick markets: cannot reduce to price alone; some version of preference aggregation is structurally necessary. However, this naturalizes what is actually a design choice: centralized vs decentralized, opaque vs transparent, proprietary vs portable. The mountain framing risks conflating necessity (matching is needed) with current contingency (closed platforms dominate).
constraint_indexing:constraint_classification(matching_markets_general, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_markets_general_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(matching_markets_general, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets_general, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(matching_markets_general, TR),
    TR >= 0.70.

:- end_tests(matching_markets_general_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Matching markets combine genuine coordination function (finding compatible counterparts is hard and platforms solve this) with meaningful extraction (information asymmetry about preferences, lock-in, and algorithm opacity). The constraint is not pure coordination (ε would be ≤0.05) because platforms can and do extract rents through opaque ranking and preference revelation. But extractiveness is not severe (ε<0.46) because the coordination function is real and participants do benefit from liquidity and match quality. The trajectory from 0.22 to 0.38 reflects increasing sophistication of extraction: early platforms were relatively transparent about matching; as markets matured and competition concentrated, algorithmic opacity and behavioral targeting increased. Suppression (0.52): Moderate-high. Significant barriers include information asymmetry (users don't know how the algorithm works), lock-in costs (rebuilding profiles on new platforms), network effects (better matches require larger user base), and lack of transparent substitutes. But suppression is not total—multi-platform participation is feasible in some domains (dating, labor) though costly. Theater ratio (0.58): Moderate. Regulatory frameworks for matching markets are increasingly performative as they apply commodity-market rules to thick-preference structures. Data protection compliance (GDPR) and algorithmic transparency mandates (AI Act) are well-intentioned but have limited enforcement reach into proprietary algorithms. The theater has increased as platforms have become more algorithmically complex and regulators' ability to inspect or enforce has become more limited.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of perspectival disagreement. Platform operators see pure coordination (Rope) because matching is their value function and they have arbitrage options (enter new markets, change algorithms). Excluded agents see pure extraction (Snare) because they have no exit and no transparency. Matched participants see mixed coordination-extraction (Tangled Rope) because the platform both solved their matching problem and constrained their options. Regulators see their own degraded function (Piton) because oversight rules don't fit the constraint's structure. Standards advocates see a temporary problem with a sunset (Scaffold) because portability protocols are technically feasible and adoption is increasing. The civilizational observer risks seeing an immutable natural law (Mountain)—mutual preference matching requires centralized coordination—but this conflates structural necessity (matching is needed) with current design (closed platforms dominate). The perspectival gap is 100% real: no single type is 'correct'; the system's truth is the presheaf of observations.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary through information control and network effects. Excluded agents: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction because no transparency, no alternatives, no exit. Matched participants: Victim + constrained (but with some benefit) → d≈0.68, f(d)≈1.05. Significant extraction (lock-in, info asymmetry) but offset by genuine coordination benefit. Regulators: Divided (mandating transparency but constrained by technical opacity) → d≈0.50, f(d)≈0.65. Piton classification emerges from theater gate, not directionality alone. Standards coalition: Organized + constrained (but with exit path visible) → d≈0.42, f(d)≈0.42. Low effective extraction because coalition has agency and sunset mechanism is real. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival—risks naturalizing design choice.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH ARCHITECTURAL DECOMPOSITION: This constraint resolves mandatrophy by showing that the classification depends critically on platform architecture. A closed, opaque matching platform (high information asymmetry, high switching costs, proprietary algorithm) classifies as Tangled Rope or Snare. An open, transparent, interoperable platform (low information asymmetry, low switching costs, portable profiles) classifies as Rope. The constraint is not ambiguous about type—it is ambiguous about architecture. The mandatrophy is resolved by recognizing that matching markets as a category can instantiate multiple types depending on design. The current extractiveness (0.38) reflects the actual empirical distribution of platform architectures in 2026: mostly closed (extraction-prone) with growing open alternatives (coordination-dominant). As open standards adoption increases, the empirical ε for the category should decrease toward 0.20 (coordination-dominant), revealing that the extraction was contingent design, not structural necessity. The measurement trajectory (0.22→0.38) shows extraction growing during the market concentration phase; the open standards coalition perspective predicts this trajectory will reverse if portability protocols succeed (scaffold sunset mechanism). The mandatrophy is not 'which type is correct?' but 'which architecture trajectory dominates?' The analytical observer's mountain is a false summit if it naturalizes current closed-platform dominance as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_measurability_threshold,
    'How much of an agent''s preference can be captured in algorithmic form without inducing strategic behavior or misrepresentation?',
    'Longitudinal studies comparing stated vs revealed preferences on matching platforms; analysis of strategic distortion when preference signals affect matching outcomes',
    'If threshold high: matching is near-transparent (Rope from more perspectives). If threshold low: significant information extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_measurability_threshold, empirical, 'Measurability limit of preferences in algorithmic matching').

omega_variable(
    platform_interoperability_feasibility,
    'Can portable preference profiles and interoperable matching protocols actually reduce lock-in without degrading match quality?',
    'Technical feasibility studies; comparison of match quality in centralized vs distributed matching systems; measurement of user switching costs with vs without portability',
    'If feasible: scaffold perspective confirmed — decentralized sunset is real. If infeasible: lock-in is structural necessity, not extractive design choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_interoperability_feasibility, empirical, 'Whether portable profiles reduce lock-in while maintaining match quality').

omega_variable(
    thick_preference_coordination_necessity,
    'Does mutual preference matching fundamentally require centralized coordination, or can it be achieved through decentralized/market mechanisms?',
    'Comparison of historical peer-to-peer matching mechanisms (personal networks, matchmakers, bulletin boards) with platform algorithms; analysis of failure modes in decentralized matching',
    'If decentralized viable: extraction is platform design choice (Tangled Rope confirmed). If centralization necessary: current platform dominance is coordination equilibrium (Rope more legitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thick_preference_coordination_necessity, conceptual, 'Necessity of centralized coordination in thick markets').

omega_variable(
    multi_platform_simultaneous_matching,
    'Can an agent simultaneously participate in multiple matching platforms without incurring prohibitive search/coordination costs?',
    'Empirical study of multi-platform users in dating, labor, housing markets; measurement of effort required to maintain consistent profiles and preferences across platforms',
    'If feasible: exit options are mobile (lower d, lower χ). If prohibitive: exit options are trapped/constrained (higher d, higher χ).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multi_platform_simultaneous_matching, empirical, 'Multi-platform participation feasibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(matching_markets_general, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(match_tr_t0, matching_markets_general, theater_ratio, 0, 0.35).
narrative_ontology:measurement(match_tr_t10, matching_markets_general, theater_ratio, 10, 0.52).
narrative_ontology:measurement(match_tr_t20, matching_markets_general, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(match_be_t0, matching_markets_general, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(match_be_t10, matching_markets_general, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(match_be_t20, matching_markets_general, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(matching_markets_general, resource_allocation).
narrative_ontology:affects_constraint(matching_markets_general, algorithm_opacity_verification).
narrative_ontology:affects_constraint(matching_markets_general, platform_network_effects).
narrative_ontology:affects_constraint(matching_markets_general, preference_aggregation_authenticity).

% DUAL FORMULATION NOTE:
% Matching markets decompose into at least three distinct structural constraints: (1) the matching algorithm's inherent coordination complexity (resource_allocation problem), (2) the information asymmetry and opacity of platform algorithms (verification/transparency constraint), (3) the network effects and lock-in that prevent exit (switching_cost constraint). This story captures the aggregate constraint; upstream constraints have their own ε values reflecting the empirical status of specific architectural choices. Platform architecture (open vs closed, transparent vs opaque) mediates the relative contribution of each component to total extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(matching_markets_general, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
