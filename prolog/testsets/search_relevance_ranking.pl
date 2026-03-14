% ============================================================================
% CONSTRAINT STORY: search_relevance_ranking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_search_relevance_ranking, []).

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
 *   constraint_id: search_relevance_ranking
 *   human_readable: Search Relevance Ranking Algorithms
 *   domain: information_technology/platform_economy
 *
 * SUMMARY:
 *   Search relevance ranking represents a global infrastructure constraint
 *   that coordinates information access while simultaneously extracting value
 *   from both information seekers and content creators. The ranking
 *   algorithm's purported function — connecting users with relevant
 *   information — is genuine, but the constraint exhibits classical tangled
 *   rope architecture: real coordination (users find relevant content,
 *   creators reach audience, platform aggregates supply and demand) layered
 *   with asymmetric extraction (platform captures attention and advertising
 *   revenue, low-resource creators face visibility barriers, users are
 *   subject to algorithmic changes they cannot control or understand). The
 *   constraint has degraded significantly over two decades: early search
 *   engines (PageRank era) were closer to pure coordination; modern search
 *   has accumulated engagement optimization, advertising integration, and
 *   behavioral tracking such that the ranking system now functions as much as
 *   an extraction mechanism as a coordination mechanism. Theater ratio has
 *   risen from 0.35 to 0.75 as the gap has widened between the 'finding
 *   relevant results' narrative and the actual optimization target
 *   (engagement, ad-adjacent positioning, retention).
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — no viable alternatives for internet information access; face algorithmic opacity and unilateral ranking changes
 *   - Low-Resource Content Creators: Primary victims (powerless/trapped) — depend on search for audience reach but cannot influence rankings; face total suppression (no collective action capacity)
 *   - Search Platform Operator: Primary beneficiary (institutional/arbitrage) — controls ranking algorithm; captures data, attention, and advertising revenue; can unilaterally change criteria
 *   - High-Bidding Advertisers: Secondary beneficiary (powerful/arbitrage) — benefit from ranking influence through ads; have exit optionality through alternative platforms
 *   - Content Creator Ecosystem: Secondary victim/moderate agent (moderate/constrained) — benefits from search distribution but bears extraction through visibility asymmetry; some collective agency through SEO practices
 *   - Regulatory Coalition: Organized opponent (organized/constrained) — coordinating through antitrust and digital services regulation; constrained by network effects and platform dominance
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent ranking design as inevitable outcome of the 'relevance problem'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(search_relevance_ranking, 0.58).
domain_priors:suppression_score(search_relevance_ranking, 0.65).
domain_priors:theater_ratio(search_relevance_ranking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(search_relevance_ranking, extractiveness, 0.58).
narrative_ontology:constraint_metric(search_relevance_ranking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(search_relevance_ranking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(search_relevance_ranking, tangled_rope).
narrative_ontology:human_readable(search_relevance_ranking, "Search Relevance Ranking Algorithms").
narrative_ontology:topic_domain(search_relevance_ranking, "information_technology/platform_economy").

domain_priors:requires_active_enforcement(search_relevance_ranking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(search_relevance_ranking, search_platform_operator).
narrative_ontology:constraint_beneficiary(search_relevance_ranking, high_bidding_advertisers).
narrative_ontology:constraint_victim(search_relevance_ranking, end_users).
narrative_ontology:constraint_victim(search_relevance_ranking, low_resource_content_creators).
narrative_ontology:constraint_victim(search_relevance_ranking, query_intent_alignment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped in search ecosystem with no viable exit. Users cannot access the internet's information without passing through ranking algorithms; no alternatives provide comparable comprehensiveness. Suppression is extreme: algorithm changes go unannounced, ranking criteria are opaque, and users have zero visibility into why results changed. Maximum experienced extraction from the perspective of information access as a fundamental need.
constraint_indexing:constraint_classification(search_relevance_ranking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-RESOURCE CONTENT CREATORS (SNARE) — Trapped between the requirement to reach audience through search (no alternative distribution channel) and inability to influence ranking algorithms. Algorithm changes unilaterally alter their visibility; SEO costs are barriers to entry; demotions happen without notice or recourse. Suppression is total — creators cannot organize collective action against algorithm changes, and exit (creating off-platform audience) requires resources they lack.
constraint_indexing:constraint_classification(search_relevance_ranking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR ECOSYSTEM (TANGLED ROPE) — At the collective level with longer time horizon, content creators benefit from search distribution (genuine coordination function: reaching audience at scale) but bear extraction through visibility asymmetry and algorithm gambling. Some agency exists — collective SEO practices, community standards, reverse-engineering ranking signals — but exit remains costly (building platform, investing in alternative distribution). Mixed extraction and coordination.
constraint_indexing:constraint_classification(search_relevance_ranking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SEARCH PLATFORM OPERATOR (ROPE) — Experiences ranking system as coordination mechanism: organizing information to serve user queries is the platform's core function. The operator has exit optionality through design choices (can change ranking criteria, migrate to new algorithms) and derives primary benefit from comprehensive index and user engagement. Extraction runs toward this agent — they capture data, attention, and advertising revenue. Classification as Rope reflects genuine coordination function alongside extraction.
constraint_indexing:constraint_classification(search_relevance_ranking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-BIDDING ADVERTISERS (TANGLED ROPE) — Powerful agents with substantial arbitrage options (can advertise through multiple platforms, can create owned distribution). Coordinate through search ads to reach intent-rich users (genuine coordination function). Also benefit from extraction — rankings are subtly influenced by ad-adjacent positioning, and ranking opacity prevents competitors from challenging positions. High extraction effectiveness but significant agency and exit optionality. Require active enforcement of payment mechanisms and bidding rules.
constraint_indexing:constraint_classification(search_relevance_ranking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PAGERANK LEGITIMACY NARRATIVE (PITON) — The original PageRank algorithm (link-based relevance as proxy for authority) provided a mathematical justification for ranking. Modern search has layered on thousands of ML-based signals, behavioral tracking, and intent inference. The PageRank narrative persists — 'our algorithm finds the most relevant results' — but is substantially theater. Ranking is now optimized for engagement, ad-adjacent positioning, and platform retention. Theater ratio (0.68) reflects the gap between the mathematical legitimacy story and actual black-box optimization. The narrative is maintained through institutional inertia; the original justification is degraded.
constraint_indexing:constraint_classification(search_relevance_ranking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY AND COMPETING PLATFORM COALITION (TANGLED ROPE) — Organized agents (regulators, competing platforms, search-adjacent services) see ranking algorithms as extractive mechanisms that concentrate power. Coordinating through regulatory frameworks (EU Digital Services Act, potential antitrust action) and alternative platforms. Benefit from search's coordination function (some use search infrastructure themselves) but bear extraction through market concentration. Constrained exit (can build alternatives but face network effects). Require active enforcement of regulatory frameworks.
constraint_indexing:constraint_classification(search_relevance_ranking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a civilizational/universal perspective, perfect relevance ranking is computationally and epistemologically impossible: query ambiguity is irreducible, user intent is unknowable from search strings alone, and relevance is observer-dependent. This perspective sees the ranking problem as an inherent limit to information access systems. However, this naturalization obscures the contingent institutional choice: the platform deliberately avoids user-side customization and query clarification mechanisms that would reduce ambiguity. The 'impossibility' is partly engineered to justify opacity.
constraint_indexing:constraint_classification(search_relevance_ranking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(search_relevance_ranking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(search_relevance_ranking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(search_relevance_ranking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(search_relevance_ranking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(search_relevance_ranking, TR),
    TR >= 0.70.

:- end_tests(search_relevance_ranking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The platform captures surplus from information discovery that could theoretically accrue to users (through saved search time, higher satisfaction) or creators (through direct audience access). Measurement shows extraction increasing over time — early search (0.28) was closer to coordination; modern search (0.58) is substantially extractive through engagement optimization and ad integration. The extraction is not total (users do find information, creators do reach some audience) but is significant and growing. Suppression (0.65): High. Multiple reinforcing mechanisms prevent exit and reduce information flows: algorithmic opacity (users cannot understand or challenge ranking changes), network effects (no alternative provides comparable coverage), SEO barriers (creators must invest in gaming), and behavioral tracking (platform extracts information to refine targeting). Individual exit is impossible; collective action is suppressed through information asymmetry. Theater ratio (0.68): High. The 'finding relevant results' narrative masks engagement optimization, advertising influence, and retention mechanics. The PageRank legitimacy story (mathematical rigor in ranking) is maintained but degraded — modern ranking is black-box ML trained on engagement rather than authority-based link analysis. The narrative serves as justification for opacity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the powerless user (Snare) and the platform operator (Rope) reveals the extraction mechanism. Both perspectives are correct: the system genuinely coordinates information access (Rope function) AND genuinely extracts from users and creators (Snare function). The gap is not a measurement error — it reflects that the constraint simultaneously performs coordination and extraction, with the extraction hidden behind the coordination narrative. The piton classification (PageRank legitimacy degraded) reveals how the theater increases over time: as actual ranking optimization drifts further from the 'relevant results' principle toward engagement and ad optimization, the narrative maintenance effort intensifies. The mountain classification risk (relevance is inherently impossible) is a false summit: it naturalizes the architectural choice (platform-controlled, opaque ranking) when alternatives exist (user-controlled ranking with transparent signals, federated discovery systems). The constraint is tangled rope, not mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. End users and low-resource creators are full targets (d ≈ 0.95) — they bear extraction costs without benefiting; trapped exit yields high f(d) → high χ. The platform is a beneficiary with arbitrage (d ≈ 0.05) — captures surplus; arbitrage exit yields low f(d) → low/negative χ. High-bidding advertisers are beneficiaries with arbitrage (d ≈ 0.25) — benefit from ad reach and subtle ranking preferences; arbitrage exit yields f(d) ≈ 0.02. The regulatory coalition are victims with constrained exit (d ≈ 0.70) — cannot fully exit search's dominance, but have some agency through regulation. The constraint is global scope (σ=1.2), which amplifies χ across all agents — the scale of concentration amplifies both coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is UNRESOLVED in this constraint and represents a critical policy question. The core tension: Search ranking simultaneously performs essential coordination (connecting information seekers with information sources) and essential extraction (concentrating visibility asymmetrically, capturing attention surplus, enforcing behavioral tracking). No single classification captures the full structure. The platform-controlled architecture makes extraction inherent to the coordination function — the platform can only coordinate by controlling visibility, which creates the extraction mechanism. Resolving mandatrophy requires either: (a) decomposing into separate constraints (coordination function vs extraction function), or (b) accepting tangled rope as the stable classification and regulating the asymmetric extraction without destroying the coordination function. The regulatory coalition's approach is (b): enforce transparency, enable user control, establish content creator rights, without migrating to pure coordination (which would require decentralized search — architecturally feasible but network-effect-hamstrung). The unresolved mandatrophy is itself the policy crux: what degree of extraction is acceptable as the price of the coordination function?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relevance_intent_alignment,
    'Are ranking algorithms optimizing for user-perceived relevance or for platform-measured engagement metrics?',
    'User satisfaction surveys vs engagement metrics; analysis of ranking changes that increase engagement but reduce relevance assessments; A/B tests with user feedback loop',
    'If optimizing for relevance: constraint is Rope from user perspective. If optimizing for engagement: constraint is Snare from user perspective; extraction is hidden behind the relevance narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relevance_intent_alignment, empirical, 'Whether ranking optimizes relevance or engagement').

omega_variable(
    algorithmic_opacity_necessity,
    'Is ranking algorithm opacity necessary for spam prevention and quality, or is it strategic opacity masking extraction?',
    'Comparison with open-ranking systems (academic search, library systems); impact analysis of transparency on spam; historical analysis of algorithm disclosure and subsequent gaming',
    'If necessary: opacity is coordination cost (suppression is justified). If strategic: opacity is primary extraction mechanism (suppression is engineered, not inherent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether opacity is technical necessity or strategic choice').

omega_variable(
    alternative_discovery_mechanisms,
    'Could user-controlled ranking (filtering, re-ranking, personalization controls) replace platform-controlled ranking without loss of discovery effectiveness?',
    'Empirical performance of user-customizable ranking systems; user satisfaction with transparency; content creator reach in decentralized discovery systems',
    'If effective: tangled rope could become rope for users + scaffold for creators (sunset to user agency). If ineffective: mountain classification partially justified; current architecture is least-bad solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_discovery_mechanisms, empirical, 'Whether user-controlled ranking can replace platform ranking').

omega_variable(
    advertising_signal_contamination,
    'To what degree do paid advertising signals influence organic ranking, either directly or through behavioral feedback loops?',
    'Analysis of ranking changes at ad-insertion boundaries; A/B tests with ad budget variations; measurement of engagement-amplification for high-bidders',
    'If substantial: snare classification strengthened; ranking is extraction mechanism wrapped in coordination narrative. If minimal: tangled rope classification validated; coordination function is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advertising_signal_contamination, empirical, 'Whether advertising influences organic ranking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(search_relevance_ranking, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srch_tr_t0, search_relevance_ranking, theater_ratio, 0, 0.35).
narrative_ontology:measurement(srch_tr_t5, search_relevance_ranking, theater_ratio, 5, 0.52).
narrative_ontology:measurement(srch_tr_t10, search_relevance_ranking, theater_ratio, 10, 0.68).
narrative_ontology:measurement(srch_tr_t15, search_relevance_ranking, theater_ratio, 15, 0.75).

% Extraction over time
narrative_ontology:measurement(srch_be_t0, search_relevance_ranking, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(srch_be_t5, search_relevance_ranking, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(srch_be_t10, search_relevance_ranking, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(srch_be_t15, search_relevance_ranking, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(search_relevance_ranking, information_standard).
narrative_ontology:affects_constraint(search_relevance_ranking, advertising_marketplace_extraction).
narrative_ontology:affects_constraint(search_relevance_ranking, content_creator_dependency).
narrative_ontology:affects_constraint(search_relevance_ranking, behavioral_data_asymmetry).

% DUAL FORMULATION NOTE:
% Search ranking decomposes into at least two structurally distinct constraints: (1) relevance_ranking_coordination (ε ≈ 0.25, Rope) — the core function of matching queries to relevant results, which is genuine coordination; (2) visibility_concentration_extraction (ε ≈ 0.72, Snare) — the asymmetric allocation of visibility and the suppression of low-resource creators. These are linked constraints; the platform's control of relevance ranking enables the visibility concentration. This story focuses on the tangled combination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(search_relevance_ranking, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
