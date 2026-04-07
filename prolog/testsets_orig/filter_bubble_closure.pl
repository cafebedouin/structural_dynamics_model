% ============================================================================
% CONSTRAINT STORY: filter_bubble_closure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_filter_bubble_closure, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: filter_bubble_closure
 *   human_readable: Filter Bubble Closure in Algorithmic Content Systems
 *   domain: media/information_systems/political_economy
 *
 * SUMMARY:
 *   Filter bubble closure in algorithmic content systems creates a constraint
 *   where platform-optimized information routing systematically suppresses
 *   exposure to disconfirming content, alternative viewpoints, and
 *   marginalized creators. The constraint exhibits structural hybridity: it
 *   is genuinely coordinative (solves the problem of routing content to
 *   receptive audiences, reducing information exploration costs) while
 *   simultaneously extractive (concentrates attention and advertising value
 *   toward high-engagement clusters, suppressing epistemic diversity). The
 *   extractiveness has increased over the measurement interval as algorithms
 *   have become more sophisticated and the economic dependence on engagement
 *   metrics has deepened. Theater ratio tracks algorithmic sorting's
 *   performance of 'personalization' and 'relevance' while the actual
 *   function (content gatekeeping) increasingly serves platform financial
 *   interests rather than user information quality. The constraint's
 *   eight-perspective structure reveals the fundamental tension:
 *   beneficiaries (platforms, advertisers, optimizers) experience
 *   coordination; powerless agents (epistemic commons, marginalized creators)
 *   experience extraction; organized alternatives (decentralized platforms)
 *   see a temporary problem with a sunset; and analytical observers risk
 *   naturalizing a contingent design choice as a law of human cognition.
 *
 * KEY AGENTS:
 *   - Platform Owners and Engagement Optimizers: Primary beneficiary (institutional/arbitrage) — capture advertising value and user attention concentration; experience constraint as coordination mechanism
 *   - Advertisers and Marketers: Secondary beneficiary (powerful/mobile) — benefit from targeted reach and reduced advertising waste; face complexity costs and regulatory risk
 *   - Epistemic Commons: Primary victim (powerless/trapped) — information diversity systematically suppressed; bears extraction with no voice or exit option
 *   - Marginalized Content Creators: Primary victim (powerless/trapped) — face algorithmic suppression outside engagement-optimized clusters; depend on platforms with no meaningful exit
 *   - Information Researchers and Journalists: Secondary victim (moderate/constrained) — benefit from relevant content routing; suffer from reduced source diversity and filter-bubble dependency
 *   - Legacy News Institutions: Institutional actor (institutional/constrained) — maintain editorial standards theatrically while ceding algorithmic gatekeeping to platforms; see own constraint as piton
 *   - Decentralized Information Alternatives: Organized agents (organized/constrained) — building escape routes through RSS, federation, open protocols; see filter bubbles as temporary platform pathology
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing algorithmic sorting as inherent cognitive necessity rather than contingent platform design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(filter_bubble_closure, 0.58).
domain_priors:suppression_score(filter_bubble_closure, 0.65).
domain_priors:theater_ratio(filter_bubble_closure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(filter_bubble_closure, extractiveness, 0.58).
narrative_ontology:constraint_metric(filter_bubble_closure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(filter_bubble_closure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(filter_bubble_closure, tangled_rope).
narrative_ontology:human_readable(filter_bubble_closure, "Filter Bubble Closure in Algorithmic Content Systems").
narrative_ontology:topic_domain(filter_bubble_closure, "media/information_systems/political_economy").

domain_priors:requires_active_enforcement(filter_bubble_closure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(filter_bubble_closure, platform_owners).
narrative_ontology:constraint_beneficiary(filter_bubble_closure, advertisers).
narrative_ontology:constraint_beneficiary(filter_bubble_closure, engagement_optimizers).
narrative_ontology:constraint_victim(filter_bubble_closure, content_diversity).
narrative_ontology:constraint_victim(filter_bubble_closure, epistemic_commons).
narrative_ontology:constraint_victim(filter_bubble_closure, marginalized_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The information ecosystem bears extraction with no exit or voice. Filter bubbles systematically suppress exposure to disconfirming information, marginalizing diverse viewpoints and alternative narratives. The commons cannot organize or appeal; it has no structure for agency. Maximum extraction without mitigation.
constraint_indexing:constraint_classification(filter_bubble_closure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED CONTENT CREATORS (SNARE) — Creators outside algorithmic preference clusters face structural barriers to discoverability. Algorithmic sorting by engagement preference systematically suppresses minority viewpoints, niche communities, and low-engagement-rate content. Creators have no exit from the platform ecosystem (dependency on reach) and no meaningful avenue to escape algorithmic suppression. The constraint is enforced invisibly through ranking mechanisms rather than explicit prohibition.
constraint_indexing:constraint_classification(filter_bubble_closure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INFORMATION RESEARCHERS AND JOURNALISTS (TANGLED ROPE) — These agents benefit from algorithmic sorting (locating relevant information quickly) while bearing extraction costs (suppression of alternative sources, reduced access to information outside their filter bubble, dependence on platform metrics). Career incentives align with viral content, not comprehensiveness. High cost to exit (audience dependency) but some agency in narrative framing.
constraint_indexing:constraint_classification(filter_bubble_closure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OWNERS AND ENGAGEMENT OPTIMIZERS (ROPE) — Experience the filter bubble as a pure coordination solution: algorithmic sorting routes content to maximally receptive audiences, reducing exploration cost and optimizing engagement metrics. The constraint is functionally beneficial — engagement rates, advertising CPM, and user retention all improve. Beneficiaries have arbitrage options (can adjust algorithms, A/B test, pivot to alternative metrics) and experience the mechanism as coordination, not extraction.
constraint_indexing:constraint_classification(filter_bubble_closure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY NEWS INSTITUTIONS (PITON) — Traditional media has internalized filter-bubble logic through editorial metrics and audience-targeting strategies. The constraint persists as theatrical performance of 'personalization' and 'relevance' while the primary function (information distribution) has been captured by platform algorithms. Legacy institutions maintain editorial standards ritually while ceding algorithmic gatekeeping to platforms. Theater ratio high, functional independence low.
constraint_indexing:constraint_classification(filter_bubble_closure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE AND DECENTRALIZED INFORMATION ALTERNATIVES (SCAFFOLD) — Organized coalitions (RSS feeds, open protocols, mastodon instances, wikis, independent news platforms) view filter bubbles as temporary platform-dependent pathologies. These actors are building alternative information architectures with explicit filter-bubble resistance: transparent algorithms, user control over sorting, non-engagement-based ranking. The scaffold is real — some users have migrated to alternatives — but adoption remains constrained by network effects and switching costs. Sunset clause: as decentralized platforms mature and federation gains scale, the platform-dependent filter bubble loses structural necessity.
constraint_indexing:constraint_classification(filter_bubble_closure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CORPORATE ADVERTISERS AND MARKETERS (TANGLED ROPE) — Benefit from filter bubbles (targeted reach, reduced advertising waste, higher conversion rates) while bearing coordination costs (complexity of microtargeting, ethical liability, regulatory scrutiny). Can exit by shifting budgets but mobile options are constrained (Google/Meta duopoly captures majority ad spend). Experience extraction as a coordination necessity with asymmetric benefit distribution toward platforms.
constraint_indexing:constraint_classification(filter_bubble_closure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of information filtering is inherent to any system that must prioritize among infinite possible inputs. Cognitive bandwidth is finite; some sorting mechanism is necessary. This perspective naturalizes algorithmic sorting as a mathematical necessity. However, the structural data contradicts this: filter-bubble enforcement via engagement metrics is a design choice, not a law of nature. The engine will flag this as a false summit, revealing that 'information is too abundant to process without filtering' naturalizes what is actually a contingent choice about WHO DECIDES the sorting criteria.
constraint_indexing:constraint_classification(filter_bubble_closure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(filter_bubble_closure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(filter_bubble_closure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(filter_bubble_closure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(filter_bubble_closure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(filter_bubble_closure, TR),
    TR >= 0.70.

:- end_tests(filter_bubble_closure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits genuine coordination function (content routing to receptive audiences reduces exploration cost and improves user experience) but increasingly serves platform profit maximization at the expense of epistemic diversity. The trajectory from 0.35 to 0.58 reflects accumulating algorithmic sophistication and economic dependence on engagement metrics. The value exceeds 0.46, triggering omega variables and measurement requirements, because the primary extraction mechanism (suppression of non-engagement-optimized content) now dominates the coordination function. Suppression (0.65): High. Barriers to access disconfirming information include: algorithmic ranking that deprioritizes low-engagement content, network effects that concentrate users in high-engagement clusters, economic incentives that reward platform participation over platform exit, and cognitive capture through repeated exposure to homophilous content. These barriers are not total (some users deliberately seek alternative sources) but are substantial and systematically structured. Theater ratio (0.68): High-moderate. The theatrical component is significant: platforms present algorithmic sorting as 'personalization' and 'relevance' while the actual function is engagement optimization and attention extraction. The performance of neutrality and user-centering masks extraction. Theater ratio increase from 0.50 to 0.68 reflects deepening gap between algorithmic rhetoric (giving you what you want) and function (capturing your attention for advertiser value).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a single set of base properties. Platform owners and optimizers perceive pure coordination (Rope) — they are solving the legitimate problem of routing content efficiently. Marginalized creators perceive pure extraction (Snare) — algorithmic suppression with no alternatives. Moderate agents perceive hybridity (Tangled Rope) — they benefit from relevance routing and suffer from filter-bubble imprisonment. The epistemic commons perceives pure extraction (Snare) — the information environment is degraded with no self-healing mechanism. Legacy institutions perceive degradation of their own role (Piton) — editorial standards persist theatrically while platforms control actual gatekeeping. Decentralized alternatives perceive a temporary institutional arrangement (Scaffold) — the constraint exists only because incumbent platforms have network advantages; alternatives can build exit routes. The analytical observer risks perceiving immutable law (Mountain) — but the engine flags this as naturalization of what is actually a platform business model choice. This perspectival range is structurally justified: different power levels, exit options, and beneficiary/victim positions genuinely experience the same constraint differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective derive from structural position within the constraint. Platform owners and optimizers as beneficiaries with arbitrage options experience low d (~0.10-0.15), producing negative χ. Marginalized creators as trapped victims experience high d (~0.90), producing maximum χ. Moderate agents (researchers, journalists) with constrained exit experience moderate-high d (~0.60-0.70). The analytical observer at civilizational scope with analytical exit experiences d ~0.72 but faces a false-summit detection signal: the natural-law framing naturalizes what the structural data reveals as a contingent design choice. The directionality pipeline computes d from power atom + exit options + beneficiary/victim status; no overrides needed because the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as Tangled Rope because it meets all three gates: (1) active enforcement (algorithmic ranking is continuously maintained), (2) genuine beneficiaries (platforms, advertisers), and (3) clear victims (marginal creators, epistemic commons). The mandatrophy is resolved through perspectival decomposition: the Rope perspectives (platform owners' pure coordination) and Snare perspectives (powerless agents' pure extraction) are both correct descriptions of the constraint from their respective vantage points. The constraint is not mislabeled as either pure coordination or pure extraction — it genuinely coordinates information routing while extracting attention and diversity. The mandatrophy resolves by acknowledging that the primary function has become extraction-maximization (engagement metrics) subordinating the coordination function (content discovery). At t=0, the coordination function dominated (extractiveness 0.35, theater 0.50); by t=15, extraction dominates (extractiveness 0.58, theater 0.68). The constraint is crossing from hybrid-with-dominant-coordination into hybrid-with-dominant-extraction. If this trajectory continues, classification will drift toward Snare (extractiveness approaching 0.66+, suppression high). The scaffold perspective (decentralized alternatives) offers a structural escape route if federation achieves sufficient scale; without it, the constraint will solidify as pure extraction over generational timescale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_vs_structural_bubble,
    'Is the filter bubble caused by algorithmic recommendation design or by structural properties of human preference and social networks that algorithms merely implement?',
    'Comparative analysis of algorithmic vs non-algorithmic information systems; measurement of bubble formation on platforms with no recommendation algorithm; cross-platform studies controlling for algorithm versus user selection behavior',
    'If algorithmic: filter bubble is contingent and removable (Scaffold/Rope classification rises). If structural: filter bubble is inherent to information systems (Mountain classification rises). If both: classification depends on the degree of algorithmic amplification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_vs_structural_bubble, empirical, 'Whether bubbles are caused by algorithms or by structural human preferences').

omega_variable(
    engagement_metric_necessity,
    'Is engagement-rate optimization a necessary business model for platform sustainability or a chosen metric that could be replaced by alternative revenue models?',
    'Historical analysis of platform economics; identification of revenue models not dependent on engagement metrics; measurement of sustainability under different ranking systems',
    'If necessary: engagement-driven filter bubbles are inherent to platform economics (Tangled Rope/Snare). If contingent: alternative metrics (time-well-spent, epistemic diversity) are structurally viable (Scaffold/Rope classification rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_metric_necessity, empirical, 'Whether engagement optimization is economically necessary').

omega_variable(
    user_agency_in_bubble_formation,
    'To what extent do filter bubbles reflect algorithmic imposition versus user preferences for homophilous content and echo chambers?',
    'A/B testing with algorithm-disabled content feeds; user studies on preference for diverse vs homogeneous information; historical analysis of information-seeking behavior pre-algorithm',
    'If predominantly user-driven: suppression metric (responsibility) should decrease; victims classification uncertain. If predominantly algorithm-driven: suppression metric sustained; victims classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_in_bubble_formation, empirical, 'Degree of user agency versus algorithmic imposition in bubble formation').

omega_variable(
    decentralization_scalability,
    'Can decentralized/open-protocol information systems achieve sufficient user adoption and feature parity with centralized platforms to constitute a viable sunset for the filter-bubble constraint?',
    'Longitudinal tracking of federation adoption (Mastodon, Bluesky, RSS revival); measurement of network effects and switching costs; comparative feature analysis with dominant platforms',
    'If viable: Scaffold perspective is structural and sunset is real (Extract extractiveness declining, theater ratio rising). If not viable: Scaffold is aspirational and decentralization is a false exit option.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_scalability, empirical, 'Whether decentralized platforms can compete with incumbent centralized platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(filter_bubble_closure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbc_tr_t0, filter_bubble_closure, theater_ratio, 0, 0.5).
narrative_ontology:measurement(fbc_tr_t5, filter_bubble_closure, theater_ratio, 5, 0.62).
narrative_ontology:measurement(fbc_tr_t10, filter_bubble_closure, theater_ratio, 10, 0.68).
narrative_ontology:measurement(fbc_tr_t15, filter_bubble_closure, theater_ratio, 15, 0.75).

% Extraction over time
narrative_ontology:measurement(fbc_be_t0, filter_bubble_closure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fbc_be_t5, filter_bubble_closure, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fbc_be_t10, filter_bubble_closure, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fbc_be_t15, filter_bubble_closure, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(filter_bubble_closure, information_standard).
narrative_ontology:boltzmann_floor_override(filter_bubble_closure, 0.12).
narrative_ontology:affects_constraint(filter_bubble_closure, algorithmic_recommendation_systems).
narrative_ontology:affects_constraint(filter_bubble_closure, attention_economy_extraction).
narrative_ontology:affects_constraint(filter_bubble_closure, platform_network_effects).
narrative_ontology:affects_constraint(filter_bubble_closure, epistemic_polarization_dynamics).

% DUAL FORMULATION NOTE:
% Filter bubble closure is downstream of platform business model decisions (engagement metric optimization) but represents a distinct structural constraint. Upstream constraints (attention_economy_extraction, algorithmic_recommendation_systems) have their own extractiveness values reflecting the fundamental design choices; filter_bubble_closure models the user-facing manifestation of these upstream constraints. The constraint family decomposes into: (1) platform_engagement_metric_optimization (ε=0.55, Tangled Rope, upstream driver), (2) filter_bubble_closure (ε=0.58, Tangled Rope, direct user impact), and (3) epistemic_polarization_dynamics (ε=0.72, Snare, generational consequence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(filter_bubble_closure, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
