% ============================================================================
% CONSTRAINT STORY: google_ai_search_overview
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_google_ai_search_overview, []).

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
 *   constraint_id: google_ai_search_overview
 *   human_readable: Google's AI-Powered Search Summary Layer (Gemini)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Google's AI Overviews represent a structural shift in search result
 *   presentation that creates tension between three competing logics: (1)
 *   improved user experience through AI summarization, (2) reduced referral
 *   traffic and revenue for content creators, and (3) consolidated
 *   information gatekeeping by a dominant platform. The constraint exhibits
 *   characteristics of all six DR types depending on the observer's
 *   structural position. For individual content creators, it is a pure
 *   extraction mechanism (Snare) — they cannot exit Google's index and have
 *   no compensation. For mid-market publishers, it is extractive but with
 *   partial agency (Snare to Tangled Rope range). For Google itself, it is
 *   coordination (Rope) — they are optimizing their own ecosystem. For
 *   regulatory bodies, it presents as a mixed extraction-coordination problem
 *   (Tangled Rope) with unclear enforcement mechanisms. For alternative
 *   search platforms (DuckDuckGo, Perplexity, ChatGPT), it is a temporary
 *   problem with a sunrise (Scaffold) — distributed AI search is
 *   materializing as a genuine exit path. For the creators' response layer
 *   (robots.txt, nosnippet tags), it is a degraded mitigation ritual (Piton)
 *   largely powerless against algorithmic override. From a civilizational
 *   perspective, the constraint risks appearing as inevitable technological
 *   evolution (Mountain), but the base metrics reveal it as a contingent
 *   institutional arrangement rather than a law of nature.
 *
 * KEY AGENTS:
 *   - Content creators (powerless/trapped): Individual bloggers, journalists, domain experts whose work feeds AI overviews without attribution control or compensation
 *   - Mid-market publishers (moderate/constrained): News sites, review aggregators, publishing houses losing 18-64% of referral traffic; capital-intensive to exit
 *   - Google (institutional/arbitrage): Benefits from extended dwell time, increased ad impressions, reduced traffic leakage; captures user attention value
 *   - Large tech competitors (powerful/arbitrage): Meta, Apple, Microsoft with partial exit (own search products) but also trapped in Google's SERP for discoverability
 *   - Regulatory authorities (organized/constrained): FTC, EU DMA, UK CMA; benefit from compliance reporting but constrained by technical capacity and industry capture
 *   - Alternative search ecosystems (organized/mobile): DuckDuckGo, Perplexity, specialized semantic search; represent genuine exit path with 10%+ market share threshold
 *   - Creator mitigation layer (moderate/constrained): robots.txt, nosnippet tags, legal threats; performative strategies overridable by algorithmic change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(google_ai_search_overview, 0.58).
domain_priors:suppression_score(google_ai_search_overview, 0.68).
domain_priors:theater_ratio(google_ai_search_overview, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(google_ai_search_overview, extractiveness, 0.58).
narrative_ontology:constraint_metric(google_ai_search_overview, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(google_ai_search_overview, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(google_ai_search_overview, tangled_rope).
narrative_ontology:human_readable(google_ai_search_overview, "Google's AI-Powered Search Summary Layer (Gemini)").
narrative_ontology:topic_domain(google_ai_search_overview, "technological/economic").

domain_priors:requires_active_enforcement(google_ai_search_overview).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(google_ai_search_overview, google_advertising_revenue).
narrative_ontology:constraint_beneficiary(google_ai_search_overview, google_user_engagement_metrics).
narrative_ontology:constraint_victim(google_ai_search_overview, organic_content_creators).
narrative_ontology:constraint_victim(google_ai_search_overview, search_referral_traffic).
narrative_ontology:constraint_victim(google_ai_search_overview, third_party_information_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Powerless individual bloggers, journalists, product reviewers, and domain experts cannot exit Google's search index. Their content feeds AI overviews without compensation or attribution control. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.96. High effective extraction masked by 'improved search experience' framing.
constraint_indexing:constraint_classification(google_ai_search_overview, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET PUBLISHER (SNARE) — News sites, review aggregators, and mid-size publishing depend on referral traffic from Google Search. AI overviews reduce click-through rates 18-64% (varying by query type). Exit is theoretically possible (invest in direct audience) but capital-intensive. d≈0.80, f(d)≈1.20, σ=1.2 → χ≈0.85. Extraction without meaningful coordination benefit.
constraint_indexing:constraint_classification(google_ai_search_overview, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GOOGLE ADVERTISING BUSINESS (ROPE) — Benefits from extended SERP dwell time, increased ad impressions per session, and reduced traffic leakage to competitors. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary. Experiences constraint as coordination: managing its own ecosystem to maximize monetization.
constraint_indexing:constraint_classification(google_ai_search_overview, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TECH COMPETITORS (TANGLED ROPE) — Have partial exit (own search products, ecosystem lock-in), but also trapped in Google's SERP for discoverability. Apple News, Meta's search investments, and Microsoft's Bing gain leverage if users abandon Google. d≈0.45, f(d)≈0.42, σ=1.2 → χ≈0.30. Mixed: they experience coordination incentives (better SERP) alongside extraction (traffic control, ranking manipulation).
constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITIES (TANGLED ROPE) — Benefit from Google's compliance reporting and anti-monopoly enforcement; constrained by limited technical capacity and industry capture. AI overviews are both a coordination problem (should search results be transparent?) and an extraction mechanism (Google controls what information users see first). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Regulatory framing masks power imbalance.
constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE SEARCH ECOSYSTEMS (SCAFFOLD) — DuckDuckGo, Perplexity AI, ChatGPT, specialized semantic search, federated search. These represent a temporary coordination solution with a sunset: if alternative aggregators achieve 10%+ search market share, the AI overview extraction mechanism loses force. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.22. Low effective extraction because exit options are materializing.
constraint_indexing:constraint_classification(google_ai_search_overview, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: MITIGATION THEATER (PITON) — robots.txt exclusion, 'nosnippet' tags, legal threats, and API terms of service. These are largely performative: Google can and has overridden them for perceived user benefit. Theater_ratio=0.62 reflects that creators engage in compliance theater (blocking AI scraping) knowing Google can remove content from overviews via algorithm change alone. The mitigation strategies persist through institutional inertia, not because they are effective.
constraint_indexing:constraint_classification(google_ai_search_overview, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TECHNOLOGICAL INEVITABILITY (MOUNTAIN) — From a long-term view, generative AI summarization of search results is a natural evolution of information aggregation. Some compression of source attribution is inevitable when moving from link-based to summary-based interfaces. However, base properties (ε=0.58, suppression=0.68) contradict the mountain gate — this is NOT an immutable technological law. The extraction is contingent on Google's market dominance and legal immunity from copyright liability. The engine marks this as a false summit.
constraint_indexing:constraint_classification(google_ai_search_overview, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(google_ai_search_overview_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(google_ai_search_overview, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(google_ai_search_overview, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(google_ai_search_overview, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(google_ai_search_overview, TR),
    TR >= 0.70.

:- end_tests(google_ai_search_overview_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. Google captures extended SERP dwell time, increased ad impressions per session, and preserved traffic that would otherwise leak to third-party summarization services. The extraction is not maximal because alternative summarization (ChatGPT, Perplexity) creates competitive pressure and users retain agency in query formulation. The value increased from 0.35 to 0.58 over 24 months as rollout expanded and traffic impact became measurable (18-64% CTR reduction depending on query category). Suppression (0.68): High. Creators face significant barriers to independent distribution: requires direct audience investment (capital-intensive), SEO reliance persists (no true exit), algorithmic visibility depends on Google's favor, and robots.txt/nosnippet are easily overridden by terms-of-service changes. Legal remedies (copyright claims) face uncertain outcomes. However, suppression is not total — some creators gain visibility in overviews, some users follow source links, and alternative search is materializing. Theater ratio (0.62): Moderate-high. Reflects that Google's 'improved user experience' framing masks extraction logic, creator mitigation strategies (robots.txt) are performative theater, and regulatory response is largely advisory rather than enforcement. The ratio reflects Goodhart drift: the public narrative (better search results) diverges from structural reality (consolidated gatekeeping).
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests radically differently across structural positions. Content creators experience Snare (pure extraction with no exit). Google experiences Rope (coordination within their ecosystem). Publishers experience Tangled Rope (mixed coordination benefit + extraction cost). Competitors experience Tangled Rope (both beneficiary and victim depending on query type). Regulators experience Tangled Rope with enforcement theater. Alternative search platforms experience Scaffold (temporary problem with visible sunset). Creators' mitigation layer experiences Piton (degraded ritual). The civilizational observer risks Mountain (technological inevitability), but the metrics reveal this as false — the constraint is contingent on Google's market dominance and legal immunity, not on technological laws. The perspectival gap is widest between Google (Rope) and content creators (Snare) — they observe the same structural phenomenon but classify it at opposite poles.
 *
 * DIRECTIONALITY LOGIC:
 *   Content creators: Victim + trapped → d≈0.92, f(d)≈1.39. Maximal extraction. Publishers: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction. Google: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary, negative effective extraction. Competitors: Mixed + arbitrage → d≈0.45, f(d)≈0.42. Moderate effective extraction; they have some agency. Regulators: Victim (of industry capture) + constrained → d≈0.50, f(d)≈0.65. Moderate extraction. Alternative search: Beneficiary (from Google's fragmentation) + mobile → d≈0.25, f(d)≈0.15. Low effective extraction; exit pathway materializing. Creator mitigation: Victim + constrained → d≈0.70, f(d)≈1.02. Moderate extraction (theater masks ineffectiveness).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as tangled rope because it satisfies all three gates: (1) beneficiaries declared (google_advertising_revenue, google_user_engagement_metrics), (2) victims declared (organic_content_creators, search_referral_traffic), and (3) active enforcement required (algorithmic ranking, search index control, legal immunity via fair use). The coordination function is genuine: AI overviews improve user experience by reducing search fragmentation and summarization time. But the extraction is also genuine: creators bear costs (lost traffic, unpaid content use) while Google captures value. The mandatrophy is resolved by recognizing that the same constraint solves a coordination problem (better search UX) while extracting value (traffic consolidation). This is not a false coordination claim or a false extraction claim — both logics are structurally real. The constraint stabilizes as tangled_rope because neither logic dominates. If copyright liability were imposed, extraction would degrade and the constraint would shift to Scaffold. If regulatory intervention separated indexing from summarization, enforcement costs would increase and the constraint might shift to Snare (enforcement overhead exceeding extraction benefit). If alternative search achieved 10%+ market share, suppression would decrease and the constraint might degrade to Rope (coordination without extraction). The current classification reflects the present-day balance between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_liability_threshold,
    'Will courts rule that AI-generated summaries constitute fair use, or will they require licensing/compensation for source content?',
    'Litigation outcomes (class actions by publishers, Copyright Office guidance, appellate precedent); regulatory action (EU AI Act, Digital Markets Act enforcement)',
    'If fair use prevails: constraint remains as tangled rope (coordination + extraction). If licensing required: AI overviews become economically unviable and sunset (scaffold converts to resolved). Classification flips from tangled_rope to scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_liability_threshold, empirical, 'Legal threshold for AI summary copyright liability').

omega_variable(
    user_satisfaction_inflection,
    'At what accuracy/hallucination threshold do users prefer AI summaries vs. traditional link-based results?',
    'User behavior studies (click-through rates, dwell time, return rates); error tracking of AI overview inaccuracy; competitive switching to alternatives',
    'If users demand high accuracy: extraction mechanism weakens (lower suppression, lower χ). If users accept lower accuracy: extraction mechanism strengthens (higher suppression, maintained χ). Affects whether constraint stabilizes as tangled_rope or degrades to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_satisfaction_inflection, empirical, 'User satisfaction inflection for AI-generated summaries').

omega_variable(
    regulatory_breakthrough_probability,
    'Will EU DMA, UK CMA, or FTC force structural separation of search index from summarization layer?',
    'Regulatory enforcement actions; mandated interoperability (forced access to Google index for competitors); mandatory disclosure of AI training data sources',
    'If regulatory breakthrough occurs: constraint becomes scaffold with enforcement sunset (10-15 year timeline). If regulatory capture continues: constraint degrades to piton (enforcement theater). Classification potentially shifts from tangled_rope to scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_breakthrough_probability, conceptual, 'Probability of regulatory structural intervention').

omega_variable(
    ai_overview_quality_ceiling,
    'Is real-time AI summarization fundamentally limited by computational cost and latency, creating a practical capacity ceiling below 100% search coverage?',
    'Technical analysis of model inference costs; coverage rollout timeline; comparison to actual deployment breadth 2 years post-launch',
    'If quality ceiling exists: only 30-50% of queries get AI overviews, reducing suppression and extraction effect. If no ceiling: full coverage achieved, extraction mechanism maximized. Affects suppression value and whether constraint stabilizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ai_overview_quality_ceiling, empirical, 'Technical ceiling on AI overview coverage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(google_ai_search_overview, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaso_tr_t0, google_ai_search_overview, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gaso_tr_t12, google_ai_search_overview, theater_ratio, 12, 0.56).
narrative_ontology:measurement(gaso_tr_t24, google_ai_search_overview, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(gaso_be_t0, google_ai_search_overview, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gaso_be_t12, google_ai_search_overview, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(gaso_be_t24, google_ai_search_overview, base_extractiveness, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(google_ai_search_overview, information_standard).
narrative_ontology:affects_constraint(google_ai_search_overview, search_ranking_gatekeeping).
narrative_ontology:affects_constraint(google_ai_search_overview, content_attribution_and_licensing).
narrative_ontology:affects_constraint(google_ai_search_overview, platform_ecosystem_lock_in).

% DUAL FORMULATION NOTE:
% AI Overviews can be decomposed into two structurally distinct constraints: (1) search result summarization (ε≈0.35, information standard, mostly coordination) and (2) traffic redirection and referral suppression (ε≈0.72, gatekeeping mechanism, mostly extraction). The integrated AI Overview constraint (ε=0.58) is their hybrid. These are linked: summarization technology enables traffic suppression, but suppression is not inherent to summarization. The story focuses on the integrated version (as Google presents it) while acknowledging the decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(google_ai_search_overview, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
