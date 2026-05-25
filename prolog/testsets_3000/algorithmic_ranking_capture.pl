% ============================================================================
% CONSTRAINT STORY: algorithmic_ranking_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_ranking_capture, []).

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
 *   constraint_id: algorithmic_ranking_capture
 *   human_readable: Algorithmic Ranking Capture in Information Markets
 *   domain: platform_economics/information_systems
 *
 * SUMMARY:
 *   Algorithmic ranking capture describes the institutional mechanism through
 *   which platforms controlling information ranking extract value from
 *   content creators and users by optimizing ranking algorithms for metrics
 *   (engagement, time-on-site, monetization) misaligned with user search
 *   intent or information quality. The constraint exhibits the characteristic
 *   structure of a tangled rope: genuine coordination function (ranking
 *   solves information discovery at scale) paired with asymmetric extraction
 *   (optimization objectives concentrate value toward platform and
 *   well-resourced producers). The constraint operates at institutional
 *   rather than interpersonal scale, with clearly differentiated structural
 *   positions: algorithm owners benefit through engagement-driven revenue;
 *   well-resourced content producers can afford SEO capture; organic users
 *   receive coordination benefit alongside extraction; marginal creators are
 *   trapped in visibility dependency; the information commons bears costs of
 *   degraded ranking integrity. The measurements show extractiveness rising
 *   from 0.35 to 0.58 and theater from 0.42 to 0.68 over 15 years, reflecting
 *   increasing optimization sophistication layered atop growing regulatory
 *   theater (transparency requirements, algorithm audits) that does not
 *   change underlying capture incentives.
 *
 * KEY AGENTS:
 *   - Algorithm Owners (Ranking Platforms): Institutional/arbitrage beneficiaries — control ranking formula, monetize user attention, set optimization objectives. Full exit flexibility; perceive constraint as coordination mechanism.
 *   - Marginal Content Creators: Powerless/trapped victims — depend on platform ranking for visibility; cannot exit without abandoning distribution. No bargaining power; no transparency into ranking changes.
 *   - Well-Resourced Content Producers: Organized/constrained actors — can hire SEO experts, invest in ranking optimization, diversify distribution. Mixed experience: benefit from coordination and ranking reach, constrained by optimization costs.
 *   - Organic Search Users: Moderate/constrained — receive genuine coordination benefit (ranking solves discovery problem), but also extraction (rankings optimized for engagement/attention capture, not intent satisfaction). Switching costs from network effects and active search cost.
 *   - Information Commons Integrity: Powerless/trapped abstract collective — misinformation and filter bubbles degrade information quality when ranking optimizes engagement over accuracy. No self-correction mechanism.
 *   - Regulatory Bodies: Powerful/mobile — implement transparency requirements and ranking audits, but create theater without changing optimization incentives (piton perspective).
 *   - Federated/Decentralized Ranking Initiatives: Organized/constrained — alternative coordination models with distributed governance. See algorithmic capture as temporary institutional arrangement with sunset through protocol-level ranking.
 *   - Analytical Observer: Analytical/analytical — risks naturalizing capture as inherent to information abundance (false mountain). Framing drives classification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_ranking_capture, 0.58).
domain_priors:suppression_score(algorithmic_ranking_capture, 0.65).
domain_priors:theater_ratio(algorithmic_ranking_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_ranking_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_ranking_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_ranking_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_ranking_capture, tangled_rope).
narrative_ontology:human_readable(algorithmic_ranking_capture, "Algorithmic Ranking Capture in Information Markets").
narrative_ontology:topic_domain(algorithmic_ranking_capture, "platform_economics/information_systems").

domain_priors:requires_active_enforcement(algorithmic_ranking_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_ranking_capture, ranking_algorithm_owners).
narrative_ontology:constraint_beneficiary(algorithmic_ranking_capture, high_capital_content_producers).
narrative_ontology:constraint_victim(algorithmic_ranking_capture, organic_search_users).
narrative_ontology:constraint_victim(algorithmic_ranking_capture, marginal_content_creators).
narrative_ontology:constraint_victim(algorithmic_ranking_capture, information_commons_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL CONTENT CREATOR (SNARE) — Small creators cannot exit algorithmic dependency without abandoning distribution channels. Trapped by lack of alternative ranking systems. Experiences full extraction: algorithmic changes destroy visibility with no recourse or transparency. No coordination function perceived — pure rent extraction.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMATION COMMONS INTEGRITY (SNARE) — Abstract collective good that bears costs of captured algorithms: misinformation persistence, filter bubbles, discovery failure. Cannot exit or organize. Maximum extraction — no self-correction mechanism when ranking is optimized for engagement rather than truth or diversity.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIC SEARCH USER (TANGLED ROPE) — Constrained by switching costs and network effects but receives coordination benefit: algorithmic ranking solves the information discovery problem at scale. Also bears extraction: rankings optimized for engagement/monetization rather than relevance. Mixed experience — genuine problem solved alongside asymmetric optimization.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALGORITHM OWNER / RANKING PLATFORM (ROPE) — Benefits from coordination function: ranking at scale genuinely solves discovery problem. Perceives the system as fair coordination mechanism. Has arbitrage capacity: can change ranking formula, licensing, or business model. Net beneficiary with full strategic flexibility.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY BODY (PITON) — Rules and transparency requirements (EU Transparency Regulation, DMA Article 38) create a performative compliance theater without changing ranking incentives. Regulators see the constraint as degraded — enforcement mechanisms exist but are circumvented through complexity and regulatory capture. Theater ratio high: compliance theater masks continued capture.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FEDERATED SEARCH AND DECENTRALIZED RANKING INITIATIVES (SCAFFOLD) — Organized alternatives (protocol-level ranking, community-curated feeds, decentralized search) see algorithmic ranking capture as a temporary institutional arrangement with a sunset. Low effective extraction because this perspective has agency and sees structural exit paths. Theater: new systems avoid some ranking theater by distributing governance.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: WELL-RESOURCED CONTENT PRODUCER (TANGLED ROPE) — Large media firms, publishers, platforms can hire ranking optimization experts and have bargaining power. Experience tangled rope: algorithmic ranking solves distribution coordination, but also enables SEO capture and pay-for-ranking extraction. Constrained by resource requirements for optimization, but have exits through owned distribution and diversification.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — At civilizational scale, ranking captures the inherent problem of information abundance: some mechanism must filter infinite content to finite human attention. This perspective risks naturalizing capture as 'inherent to ranking' — an immutable property of information markets. Engine false summit detector: the capture is contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(algorithmic_ranking_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_ranking_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_ranking_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_ranking_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_ranking_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_ranking_capture, TR),
    TR >= 0.70.

:- end_tests(algorithmic_ranking_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The algorithm owner extracts through two channels: (1) capturing user attention and monetizing through advertising/data, and (2) setting ranking objectives that disadvantage organic discovery in favor of algorithmic amplification. The extraction is significant but not total — genuine coordination value exists (ranking does solve discovery), so extraction is not 0.85+ (snare territory). Measuring from the marginal creator perspective, extractiveness is near-maximum (0.95 trapped vulnerability). Measuring from the platform perspective, it approaches coordination (0.05). The base properties represent the average across all structural positions, weighted toward user-level experience. Suppression (0.65): High. Marginal creators face structural barriers: platform dependency (no alternative ranking systems at comparable scale), opacity of ranking algorithms (cannot understand or predict changes), and asymmetric power (platform can change ranking without warning or negotiation). Users face weaker but real suppression: switching cost through network effects and active search burden. Well-resourced producers have lower suppression (capital to hire optimization expertise, bargaining power). Theater ratio (0.68): High and rising. Regulatory compliance (EU Transparency Regulation, DMA Article 38) creates performative audit and transparency theater: algorithms must be explainable, but explainability does not prevent capture. Ranking explanations mask optimization for engagement. SEO industry itself increasingly theatrical — ranking factor forums, algorithm update theater, pseudo-scientific optimization rituals. Measurement trajectory: Theater rose from 0.42 to 0.68 as regulations imposed transparency theater and SEO optimization became more ritualistic.
 *
 * PERSPECTIVAL GAP:
 *   The gap between algorithm owner (rope) and marginal creator (snare) perspectives is structural, not observational. Both are measuring the same constraint; they occupy different positions in the extraction flow. The algorithm owner controls what gets ranked and receives the revenue; the marginal creator receives no communication about algorithm changes and no appeal process. The coordinate gap (rope vs snare) reveals the asymmetry. Organized producers (tangled rope) occupy the middle: they benefit from ranking reach (coordination) but must invest in optimization (extraction cost). Their gap from the platform is smaller — they have exits (diversification, owned distribution) and bargaining power. Users (tangled rope) see coordination (finding things works) and extraction (engagement optimization) simultaneously. The federated ranking perspective (scaffold) claims the entire current system is temporary — alternative structures with lower theater (0.45 vs 0.68) are emerging. The regulatory perspective (piton) reveals that transparency mandates create compliance theater without changing optimization incentives: algorithm documentation exists, but rank optimization for engagement continues. The analytical false mountain perspective shows the risk of naturalizing contingent arrangements: 'ranking must optimize engagement because information is abundant' — this masks the institutional choice to monetize user attention.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values flow from structural position and power: Platform owners benefit (d ≈ 0.05, beneficiary with arbitrage) → low f(d) → negative or near-zero χ. Well-resourced producers benefit but face optimization costs (d ≈ 0.30, mixed beneficiary/victim with constrained exit) → low f(d) → moderate χ. Organic users receive coordination benefit but face attention extraction (d ≈ 0.50, balanced) → f(d) ≈ 0.65 → moderate χ. Marginal creators face maximum extraction (d ≈ 0.92, victim with trapped exit) → f(d) ≈ 1.38 → high χ. Information commons faces maximum extraction with no exit (d ≈ 0.98) → f(d) ≈ 1.42 → near-maximum χ. Global scope (σ = 1.2) amplifies χ by 20% — capture reaches every information market. Suppression is not scaled, remaining 0.65 across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE SATISFIED: (1) Genuine coordination function: algorithmic ranking solves the real problem of discovering relevant content at scale. Without ranking, information markets fail (users cannot find anything). This coordination function is structural, not pretext. (2) Asymmetric extraction: platform captures engagement-driven revenue; well-resourced producers can optimize; marginal creators face visibility dependency; users bear attention extraction; information commons bears integrity costs. The extraction is not symmetrical — it concentrates toward platform and away from marginal creators. (3) Active enforcement required: maintaining capture requires continuous optimization, algorithm update theater, and suppression of transparent ranking. If enforcement ceased (open algorithm, user-customizable ranking), capture would degrade. The constraint is NOT a rope (pure coordination) because extraction is substantial and concentrated. It is NOT a snare (pure extraction) because genuine coordination value exists and users receive discovery benefit. It IS a tangled rope: both mechanisms are present, structurally interlocked. MANDATROPHY DRIVER: The risk in classification is mischaracterizing engagement optimization as 'optimization for relevance' — framing capture as service. The tangled rope gate prevents this by requiring both explicit coordination function (acknowledged) and explicit extraction (acknowledged). The piton perspective (regulatory theater) is separately classified to show that compliance mechanisms do not change the underlying tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ranking_capture_vs_algorithmic_inevitability,
    'Is algorithmic ranking capture inherent to any ranking system, or contingent on specific business model incentives and opacity?',
    'Comparative analysis of ranking systems with different ownership structures, transparency levels, and monetization models. Test whether transparent ranking algorithms with non-engagement-optimized objectives exhibit capture.',
    'If inherent: mountain classification justified. If contingent: capture is institutional choice, tangled_rope classification correct, and regulatory intervention has structural possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ranking_capture_vs_algorithmic_inevitability, empirical, 'Whether ranking capture is inherent or contingent on business model').

omega_variable(
    organic_discovery_alternative_viability,
    'Can federated, protocol-level, or community-curated ranking systems achieve sufficient scale and diversity without reintroducing capture at a different layer?',
    'Longitudinal performance tracking of federated search initiatives; comparison of capture mechanisms (centralized algorithmic vs distributed governance failure modes); adoption rate analysis.',
    'If viable: scaffold sunset is real structural possibility, reducing perceived extractiveness for organized actors. If not: alternatives are aspirational, and users remain trapped — snare classification holds, extraction is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_discovery_alternative_viability, empirical, 'Viability of alternative ranking systems to displace centralized capture').

omega_variable(
    optimization_objective_observability,
    'What portion of algorithmic ranking capture operates through observable SEO vulnerability vs opacity of ranking objectives and training data?',
    'Audit of ranking formula transparency; comparison of capture success rates for: known ranking factors (exploitable optimization), unknown objectives (pure opacity), and randomized components. Test whether transparency alone reduces capture.',
    'If observable: transparency regulation (EU DMA) is structurally effective and suppression can decrease. If opaque: transparency theater masks continued capture — piton classification validated, regulation is performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_objective_observability, empirical, 'Observable vs opaque components of ranking capture vulnerability').

omega_variable(
    user_exit_cost_measurement,
    'What are the true switching costs for users when ranking quality degrades: active search cost, information quality loss, or network effect lock-in?',
    'Behavioral analysis of search switching when ranking quality declines; user satisfaction vs exit correlation; competitive switching rate analysis during ranking degradation events.',
    'If switching cost is active search (low structural trap): constrained exit option justified. If network effect dominant (high structural trap): trapped exit more accurate, suppression increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_exit_cost_measurement, empirical, 'User switching costs and structural barriers to exit').

omega_variable(
    engagement_optimization_alignment_with_user_intent,
    'At what proportion do engagement-optimized ranking objectives align with user search intent vs systematically diverge toward addiction and attention capture?',
    'Large-scale user intent analysis; correlation of ranking position with search intent satisfaction; measurement of filter bubble effects; off-platform outcome tracking (decision quality, diversity exposure).',
    'If high alignment: ranking is genuine coordination with side effects, tangled_rope classification holds. If systematic divergence: ranking is extraction mechanism masked as coordination, snare classification more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_optimization_alignment_with_user_intent, empirical, 'Alignment of engagement optimization with user intent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_ranking_capture, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algrank_tr_t0, algorithmic_ranking_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algrank_tr_t5, algorithmic_ranking_capture, theater_ratio, 5, 0.58).
narrative_ontology:measurement(algrank_tr_t10, algorithmic_ranking_capture, theater_ratio, 10, 0.68).
narrative_ontology:measurement(algrank_tr_t15, algorithmic_ranking_capture, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(algrank_be_t0, algorithmic_ranking_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algrank_be_t5, algorithmic_ranking_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algrank_be_t10, algorithmic_ranking_capture, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(algrank_be_t15, algorithmic_ranking_capture, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_ranking_capture, information_standard).
narrative_ontology:affects_constraint(algorithmic_ranking_capture, search_engine_optimization_race).
narrative_ontology:affects_constraint(algorithmic_ranking_capture, engagement_driven_feed_capture).
narrative_ontology:affects_constraint(algorithmic_ranking_capture, platform_recommendation_lock_in).

% DUAL FORMULATION NOTE:
% Algorithmic ranking capture decomposes into structurally distinct constraints by domain: search engine optimization in search markets has different extraction mechanisms than feed ranking in social platforms, which differs from recommendation systems in commerce. Each domain has its own constraint story with different ε values reflecting domain-specific coordination necessity (search = 0.60, feed = 0.48, recommendation = 0.65). The present story represents an abstract model applicable across all domains. Domain-specific stories should link back via affects_constraints to show how domain-level capture instantiates the general mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_ranking_capture, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
