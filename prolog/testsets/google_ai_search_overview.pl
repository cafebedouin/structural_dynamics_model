% ============================================================================
% CONSTRAINT STORY: google_ai_search_overview
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Google's integration of generative AI summaries ('AI Overviews') at the
 *   top of search results creates a structural constraint that combines
 *   coordination benefits (improved user experience) with asymmetric
 *   extraction (traffic redirection away from content creators). The
 *   constraint operates by converting search users' attention from organic
 *   result links to AI-generated summaries powered by aggregated content.
 *   Small content creators have no viable exit; publishers have constrained
 *   options (SEO optimization, legal challenge); Google has full arbitrage;
 *   competing platforms face mobile but constrained alternatives; regulators
 *   have organized response with sunset logic; the civilizational analytical
 *   view risks naturalizing what is contingent institutional design. The
 *   theater_ratio (0.48) reflects that the mechanism is not highly
 *   performative — the summarization is genuinely functional (users report
 *   satisfaction) — but the extraction mechanism remains opaque to many
 *   affected parties (they discover traffic loss rather than choosing
 *   participation). Over the 12-month interval, extractiveness increased from
 *   0.35 to 0.58 as AI summary placement became more prominent and content
 *   creators reported measurable traffic decline.
 *
 * KEY AGENTS:
 *   - Small Content Creators (Blogs, Niche Publishers): Primary victims (powerless/trapped) — no mechanism to opt-out or reduce summary inclusion; traffic loss is uncompensated
 *   - Large Publishers (News Organizations, Media Companies): Secondary victims (moderate/constrained) — have some SEO leverage and legal recourse but face significant traffic redirection
 *   - Google Search Division: Primary beneficiary (institutional/arbitrage) — captures user attention and advertising inventory; experiences constraint as coordination feature
 *   - Competing Platforms (Microsoft Bing, OpenAI ChatGPT, Meta AI Search): Secondary beneficiary (powerful/mobile) — face pressure to adopt similar summarization but retain exit options
 *   - Regulatory Coalition (EU DMA, FTC, Publisher Lawsuits): Organized responders (organized/constrained) — pursuing opt-out mechanisms and attribution requirements with 3-7 year regulatory timeline
 *   - Search Users: Implicit beneficiaries (powerful/mobile) — experience improved summary convenience; bear cost indirectly through reduced content diversity and creator incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(google_ai_search_overview, 0.58).
domain_priors:suppression_score(google_ai_search_overview, 0.62).
domain_priors:theater_ratio(google_ai_search_overview, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(google_ai_search_overview, extractiveness, 0.58).
narrative_ontology:constraint_metric(google_ai_search_overview, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(google_ai_search_overview, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(google_ai_search_overview, tangled_rope).
narrative_ontology:human_readable(google_ai_search_overview, "Google's AI-Powered Search Summary Layer (Gemini)").
narrative_ontology:topic_domain(google_ai_search_overview, "technological/economic").

domain_priors:requires_active_enforcement(google_ai_search_overview).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(google_ai_search_overview, google_search_monetization).
narrative_ontology:constraint_beneficiary(google_ai_search_overview, user_convenience_perception).
narrative_ontology:constraint_victim(google_ai_search_overview, content_creators_traffic).
narrative_ontology:constraint_victim(google_ai_search_overview, content_creator_monetization).
narrative_ontology:constraint_victim(google_ai_search_overview, search_result_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Small publishers, bloggers, and content sites have no viable exit. Their traffic flows directly to Google's AI-generated summary instead of their pages. No mechanism to opt-out; search visibility is non-negotiable for discovery. Maximum experienced extraction with minimal suppression opacity — the mechanism is transparent but coercive.
constraint_indexing:constraint_classification(google_ai_search_overview, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOPHISTICATED PUBLISHER (TANGLED ROPE) — Larger publishers (news organizations, media companies) have constrained options: they can attempt SEO optimization to get featured in summaries, or they can pursue legal/regulatory challenges. Benefits from some AI summary citations (backlinks, attribution); bears extraction through traffic loss. Both extraction and coordination present — mixed experience.
constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOOGLE SEARCH DIVISION (ROPE) — Experiences the constraint as pure coordination: aggregating content into summaries improves user satisfaction metrics, increases session duration, and maintains search dominance. Benefits from user lock-in and advertising inventory growth. Arbitrage exit available (can always revert summarization, shift to other products). Net beneficiary.
constraint_indexing:constraint_classification(google_ai_search_overview, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING TECHNOLOGY PLATFORM (TANGLED ROPE) — Alternative search engines and AI assistants face mixed dynamics: extraction through Google's dominance forcing them to compete on summarization quality, but also coordination opportunity to build differentiated search products. Mobile exit available (users can switch to Bing, ChatGPT). Significant agency but constrained by network effects.
constraint_indexing:constraint_classification(google_ai_search_overview, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY/ANTITRUST COALITION (SCAFFOLD) — EU Digital Markets Act, FTC investigations, and publisher lawsuits (New York Times et al.) represent organized response with sunset logic. If regulatory action forces Google to modify AI summary placement, attribution, or opt-out mechanisms, the extraction mechanism degrades. Enforcement is constrained but improving; sunset estimated 3-7 years as regulations mature.
constraint_indexing:constraint_classification(google_ai_search_overview, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — From a civilizational view, search result aggregation might appear as an immutable feature of how information systems scale: larger indices require summarization for usability. However, the structural data contradicts this. The constraint is contingent on (a) Google's market dominance, (b) lack of regulatory guardrails, (c) absence of attribution mechanisms that would redirect traffic. None of these are natural laws.
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
    constraint_indexing:constraint_classification(google_ai_search_overview, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(google_ai_search_overview, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(google_ai_search_overview_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. Google captures significant economic value (advertising inventory, user engagement, data signal) that flows directly from content creator traffic redirection. The extraction is not maximal because (a) attribution links provide some return traffic, (b) regulatory pressure is rising, (c) users can still click through to full pages if summary is insufficient. Suppression (0.62): Moderate-high. The mechanism is transparently visible (users see AI Overview box), but content creators have limited alternatives: search visibility is non-negotiable for discovery, and no formal opt-out mechanism existed at deployment. The transparency reduces suppression below pure coercion, but the lack of meaningful exit increases it above coordination-only systems. Theater ratio (0.48): Moderate. The summarization mechanism is genuinely functional — users do perceive increased convenience, session duration increases, satisfaction metrics improve — but some performative elements exist (the 'AI' branding may create perceived neutrality that masks extraction; the mechanism wasn't transparently negotiated with content creators as a trade-off).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Google sees Rope: they solve the real coordination problem of summarizing massive result sets into digestible information. Small creators see Snare: they lose traffic with no compensation or exit. Publishers see Tangled Rope: they gain some attribution benefit but lose significant traffic. Competitors see constrained Tangled Rope: they face pressure to compete on summarization but retain platform mobility. Regulators see Scaffold: the extraction mechanism can be constrained by mandatory opt-out, attribution expansion, and traffic-sharing requirements over a 3-7 year horizon. The analytical observer risks false-summit Mountain (aggregation is inherent to scale) but the structural data reveals contingency: with attribution, opt-out, or traffic-sharing, the constraint would shift classifications entirely. This perspectival gap is the core diagnostic signature — the same mechanism appears coordinating from one view, extractive from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Small creators (powerless, trapped) derive d = 0.95 (full victims with no exit), producing high f(d) = 1.42 and high experienced extraction. Publishers (moderate, constrained) derive d = 0.65-0.75 (mostly victims with some leverage), producing moderate f(d) = 0.95-1.15. Google (institutional, arbitrage) derives d = 0.05 (full beneficiary with complete exit optionality), producing negative f(d) = -0.12 and negative experienced extraction (they benefit). The directive flow is asymmetric: Google benefits, creators lose, with regulatory constraints pushing the system toward more symmetric terms. No directionality overrides needed — the derivation chain captures the structural reality accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between genuine coordination (summarization improves user experience, which is real) and extraction (traffic redirection to Google's owned properties, which is real). The Tangled Rope classification captures both: (1) Beneficiary group (Google) experiences real coordination benefit, (2) Victim group (content creators) bears real extraction cost, (3) Active enforcement (Google's indexing, ranking algorithm, summary placement) maintains the asymmetry, (4) No legal framework requires revenue-sharing despite the extraction. The classification prevents mischaracterization as 'pure coordination' (which would ignore creator harm) or 'pure extraction' (which would ignore genuine summarization benefit). The mandatrophy is resolved by the mixed institutional perspective (Perspective 4) and the regulatory scaffold (Perspective 5) — organized actors can see the constraint clearly and are building countervailing power through regulatory frameworks that could restore more symmetric terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_mechanism_sufficiency,
    'Does source attribution in AI summaries constitute adequate compensation for traffic loss to content creators?',
    'Traffic analysis pre/post attribution implementation; measurement of click-through rates from summary attribution links; comparison with organic search traffic baseline',
    'If effective: constraint shifts toward Rope (coordination with residual attribution benefit). If ineffective: remains Snare for powerless creators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_mechanism_sufficiency, empirical, 'Whether summary attribution provides sufficient traffic compensation').

omega_variable(
    regulatory_opt_out_enforceability,
    'Can regulators enforce meaningful opt-out mechanisms for content creators without fragmenting the search experience or reducing Google''s competitive moat?',
    'Implementation analysis of EU Digital Markets Act requirements; measurement of opt-out take-up rates; assessment of technical feasibility for per-domain summary suppression',
    'If enforceable: scaffold sunset mechanism becomes real, constraint classification shifts toward temporary (3-5 year horizon). If not enforceable: regulatory threats remain performative, constraint persists in current form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_opt_out_enforceability, empirical, 'Enforceability of regulatory opt-out requirements').

omega_variable(
    alternative_discovery_viability,
    'Do alternative discovery mechanisms (social platforms, AI assistants, RSS aggregators, direct search) provide viable substitutes for Google search-driven traffic?',
    'Traffic diversification analysis for content creators over 2-3 year window; measurement of percentage creators achieving >20% traffic from non-Google sources; correlation with adoption of alternative platforms',
    'If viable alternatives exist: creator exit is less trapped (d shifts lower), classification changes toward Tangled Rope for broader creator base. If Google remains sole viable source: trap persists, Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_discovery_viability, empirical, 'Viability of alternative content discovery channels').

omega_variable(
    summary_quality_bias,
    'Do AI summaries systematically favor certain content sources (established publishers, SEO-optimized sites, Google-owned properties) over others, creating hidden extraction beyond traffic redirection?',
    'Analysis of summary source attribution patterns; measurement of inclusion rates by publisher size, SEO sophistication, and Google ownership; detection of systematic content filtering',
    'If bias is systematic: extraction is higher than traffic loss alone suggests (chi increases). If summaries are source-agnostic: constraint is pure traffic redirection (simpler mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(summary_quality_bias, empirical, 'Whether AI summaries exhibit systematic source bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(google_ai_search_overview, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaso_tr_t0, google_ai_search_overview, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gaso_tr_t6, google_ai_search_overview, theater_ratio, 6, 0.4).
narrative_ontology:measurement(gaso_tr_t12, google_ai_search_overview, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(gaso_be_t0, google_ai_search_overview, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gaso_be_t6, google_ai_search_overview, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(gaso_be_t12, google_ai_search_overview, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(google_ai_search_overview, information_standard).
narrative_ontology:affects_constraint(google_ai_search_overview, search_result_ranking_opacity).
narrative_ontology:affects_constraint(google_ai_search_overview, content_creator_algorithmic_dependency).
narrative_ontology:affects_constraint(google_ai_search_overview, large_language_model_training_attribution).

% DUAL FORMULATION NOTE:
% This constraint is structurally distinct from general search ranking dynamics. AI Overviews represent a specific mechanism (generative summarization with placement priority) that creates a new extraction vector distinct from traditional ranking. The network links reflect upstream dependencies (ranking opacity enables summary placement) and downstream consequences (training data attribution and creator dependency both depend on summary extraction levels).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
