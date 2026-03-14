% ============================================================================
% CONSTRAINT STORY: algorithmic_recommendation_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_recommendation_opacity, []).

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
 *   constraint_id: algorithmic_recommendation_opacity
 *   human_readable: Algorithmic Recommendation Opacity and User Exploitation
 *   domain: digital_platforms/information_asymmetry
 *
 * SUMMARY:
 *   Algorithmic recommendation systems on major platforms (YouTube, TikTok,
 *   Instagram, Twitter/X, Netflix) create a structural asymmetry: platforms
 *   know exactly how they rank content and influence user exposure, while
 *   users cannot see, understand, or contest algorithmic decisions. This
 *   opacity enables extraction of user attention (toward
 *   engagement-maximizing rather than user-preference-aligned content),
 *   suppression of alternative content distribution pathways, and lock-in
 *   through network effects. The constraint exhibits different character from
 *   each structural position: platform operators experience it as
 *   coordination (solving content abundance), trapped users experience it as
 *   extraction with no exit, regulatory bodies experience it as degraded
 *   theater (compliance without functional oversight), and emerging
 *   decentralized alternatives experience it as a temporary problem with a
 *   sunset clause. The extractiveness trajectory shows steady increase from
 *   0.35 to 0.58 over 15 years as platforms optimize engagement metrics and
 *   user switching costs accumulate. Theater ratio increases from 0.40 to
 *   0.70 as transparency theater (algorithmic auditing, explainability
 *   documentation, regulatory compliance) becomes more elaborate while actual
 *   algorithmic decision-making becomes more opaque.
 *
 * KEY AGENTS:
 *   - Users/Algorithmic Subjects: Primary victims (powerless/trapped) — face information asymmetry and network lock-in; cannot predict or influence recommendations; bear full extraction cost
 *   - Independent Content Creators: Secondary victims (moderate/constrained) — depend on platform distribution but face opaque visibility ranking; benefit from reach but constrained by algorithmic gatekeeping
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — design and control opacity to maximize engagement and data value; full exit optionality; capture attention asymmetrically
 *   - Attention Merchants/Advertisers: Secondary beneficiaries (institutional/arbitrage) — benefit from platform's ability to deliver algorithmically-sorted high-engagement audiences; profit from user attention extraction
 *   - Regulatory Bodies: Institutional actor (institutional/mobile) — mandated to oversee algorithmic fairness and transparency but lack technical capacity and enforcement power; maintain performative compliance theater
 *   - Decentralized Platform Developers: Organized agents (organized/constrained) — building interoperability standards and decentralized alternatives; see opacity as solvable through architecture design; constrained by network effects inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_recommendation_opacity, 0.58).
domain_priors:suppression_score(algorithmic_recommendation_opacity, 0.72).
domain_priors:theater_ratio(algorithmic_recommendation_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_recommendation_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_recommendation_opacity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_recommendation_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_recommendation_opacity, tangled_rope).
narrative_ontology:human_readable(algorithmic_recommendation_opacity, "Algorithmic Recommendation Opacity and User Exploitation").
narrative_ontology:topic_domain(algorithmic_recommendation_opacity, "digital_platforms/information_asymmetry").

domain_priors:requires_active_enforcement(algorithmic_recommendation_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_recommendation_opacity, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_recommendation_opacity, attention_merchants).
narrative_ontology:constraint_victim(algorithmic_recommendation_opacity, user_autonomy).
narrative_ontology:constraint_victim(algorithmic_recommendation_opacity, content_creators).
narrative_ontology:constraint_victim(algorithmic_recommendation_opacity, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ALGORITHMIC SUBJECT (SNARE) — Users cannot see how recommendations are generated, cannot predict what content will be shown, and face prohibitive switching costs (social graph lock-in, data portability barriers, network effects). Trapped by information asymmetry and structural lock-in. Maximum extraction: attention directed toward high-engagement content regardless of user preference or epistemic value. No meaningful exit option.
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT CONTENT CREATORS (TANGLED ROPE) — Benefit from access to audience distribution via recommendation algorithms, yet face opacity about visibility factors. Cannot predict or influence ranking. High cost to exit (lose audience). Also contribute to platform lock-in through reliance. Mixed coordination (algorithm distributes content) and extraction (visibility is algorithmically gated and opaque).
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience opacity as pure coordination mechanism: algorithmic curation solves the problem of content abundance and user attention allocation. The opacity is functional — it enables A/B testing, personalization, and engagement optimization. Net beneficiary with full exit optionality (can change algorithms, monetization models, business strategy). Extraction flows toward this agent.
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY ACCOUNTABILITY APPARATUS (PITON) — Transparency mandates (EU Digital Services Act, algorithmic auditing requirements) are largely performative theater. Compliance reports and algorithmic explainability documentation exist but remain opaque to regulators. The apparatus persists due to institutional mandate and public pressure, but functional oversight capacity has atrophied. High theater (compliance rituals), low actual extraction reduction. Maintained through regulatory inertia.
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEROPERABILITY AND DECENTRALIZED ALTERNATIVES (SCAFFOLD) — Organized efforts (ActivityPub, Bluesky, decentralized social networks) represent a temporary coordination solution with an explicit sunset: if interoperability standards mature, users can port data across platforms, breaking network lock-in. The constraint becomes obsolete when exit becomes genuinely mobile. Coordinated agents with agency and a visibility path. High suppression now, declining as alternatives mature.
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — Risk of classification as mountain: viewing algorithmic opacity as an immutable consequence of information theory and computational complexity. Recommendation algorithms operate under computational constraints; perfect transparency would require exposing proprietary optimization logic and training data. From this view, opacity is inherent to algorithmic systems — a natural law rather than an extractive mechanism. However, the structural data contradicts this. Opacity could be reduced substantially (explanation layers, ranking factor disclosure, algorithmic auditing) without eliminating computational optimization. The mountain framing naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_recommendation_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_recommendation_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_recommendation_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_recommendation_opacity, TR),
    TR >= 0.70.

:- end_tests(algorithmic_recommendation_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract attention value through opaque ranking that prioritizes engagement over user preference. However, extraction is not maximal (0.72+) because platforms do provide real coordination benefit (content discovery, personalization) — the opacity enables them to extract a fraction of the value created. The constraint contains genuine coordination alongside asymmetric extraction. Suppression (0.72): High. Users cannot exit due to network effects (friends and content are on the platform), data portability barriers (cannot transfer social graph), and switching costs (replication of followers/subscriptions). Regulatory oversight is suppressed through technical opacity and regulatory capture (platforms can claim complexity justifies non-disclosure). Theater ratio (0.65): Moderate-high. Transparency compliance theater is substantial: algorithmic auditing reports, explainability dashboards, regulatory testimonies about algorithmic fairness. But the core extraction mechanism (opaque ranking) remains unchanged. The theater gives appearance of oversight without reducing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator sees this as pure coordination (Rope) — algorithms solve the technical problem of content abundance and audience discovery. The trapped user sees it as pure extraction (Snare) — attention is directed algorithmically regardless of preference, with no exit option. Content creators see mixed coordination and extraction (Tangled Rope) — they benefit from algorithmic distribution but are gatekept by opaque ranking. Regulatory bodies see degraded theater (Piton) — compliance mechanisms exist but lack enforcement teeth. Decentralized platform builders see a temporary problem with a sunset (Scaffold) — interoperability standards can replace the network lock-in within a generation. The civilizational analytical observer risks seeing opacity as immutable (Mountain) — inherent to information asymmetry and algorithmic complexity — but the structural data shows opacity is a contingent choice, not a law of nature. Platforms *could* disclose ranking factors, publish explainability layers, and submit to algorithmic auditing without destroying core functionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators benefit from opacity and have full exit optionality (can change algorithms, pivot to new models) — they derive low directionality (d ≈ 0.15), producing negative effective extraction. Users cannot see or predict algorithms and face prohibitive switching costs — they derive high directionality (d ≈ 0.90), producing maximum effective extraction. Content creators have moderate exit options (can move to decentralized platforms but lose audience) — they derive moderate directionality (d ≈ 0.65). Regulatory bodies are organizationally mobilized but lack enforcement capacity — they derive moderate directionality (d ≈ 0.55). The decentralized alternative builders have organized agency and a structural exit path — they derive low-moderate directionality (d ≈ 0.40).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely a Tangled Rope (mixed coordination and extraction) rather than pure Snare or pure Rope. Algorithmic recommendation systems do provide real coordination benefit — they solve the content abundance problem and enable personalized discovery. But they also enable substantial extraction — opaque ranking concentrates attention asymmetrically and locks users into network effects. The constraint requires active enforcement (platform investment in recommendation systems) and produces both coordination (content discovery) and extraction (attention capture). The perspectival gap is diagnostic: if all perspectives classified as Snare, the constraint would be pure extraction and the 'coordination benefit' framing would be false. Instead, the platform operator genuinely experiences coordination, the user genuinely experiences extraction, and the analytical observer risks naturalizing what is actually a designed system choice. The scaffold perspective (decentralized alternatives with sunset logic) is crucial: it shows the opacity is solvable architecturally, not inherent to algorithms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'What level of algorithmic explanation is technically necessary for functionality vs. unnecessary and extraction-enabling?',
    'Comparative analysis of platforms with varying transparency levels (Twitter''s algorithm vs Bluesky''s public algorithm); measurement of engagement metrics and user satisfaction across transparency tiers',
    'If threshold is low (10-20% explanation suffices): most opacity is extractive overhead and suppression is inflated. If threshold is high (70%+ explanation needed): opacity is partially justified by technical necessity. Classification would shift from Snare toward Tangled Rope if threshold is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Technical necessity threshold for algorithmic opacity').

omega_variable(
    lock_in_mechanism_primacy,
    'Is user entrapment primarily from information asymmetry (cannot understand/predict algorithms) or from structural lock-in (social graph, data portability barriers)?',
    'A/B testing with high-transparency algorithms on competing platforms; measurement of switching rates when exit barriers are reduced but opacity remains; user surveys distinguishing comprehension complaints from switching friction',
    'If primarily information asymmetry: addressing opacity directly reduces perceived extraction. If primarily structural lock-in: transparency alone provides limited relief; interoperability is required. Changes perceived time to exit and suppression intensity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lock_in_mechanism_primacy, empirical, 'Whether lock-in is driven by opacity or by structural network effects').

omega_variable(
    engagement_maximization_beneficiary_alignment,
    'Does algorithmic engagement maximization actually serve user interests (discovering genuinely preferred content) or primarily serve advertiser/platform interests (extracting attention)?',
    'Longitudinal studies of user satisfaction and stated preferences vs algorithmic recommendations; measurement of user-reported discovery satisfaction; comparison of engagement-optimized algorithms with explicit user-preference-aligned algorithms',
    'If user-aligned: engagement maximization is coordination (Rope). If advertiser-aligned: engagement maximization is extraction mechanism (Snare/Tangled Rope). This distinction determines whether the constraint is genuine coordination with opacity cost or pure extraction theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_maximization_beneficiary_alignment, empirical, 'Whether engagement optimization serves user or platform interests').

omega_variable(
    interoperability_sunset_viability,
    'Can decentralized interoperability standards (ActivityPub, Bluesky) actually reduce recommendation opacity lock-in or do they reproduce the same extraction mechanisms in distributed form?',
    'Implementation analysis of decentralized recommendation mechanisms; longitudinal study of user migration patterns and adoption rates; measurement of algorithmic transparency in federated systems; analysis of whether opacity recurs at federation coordination layer',
    'If viable: Scaffold classification is structurally sound — sunset is real and extraction will decline. If not viable: Scaffold is aspirational, not structural. Constraint may be permanent Snare with organized resistance that does not succeed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_sunset_viability, empirical, 'Whether decentralized alternatives can solve algorithmic opacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_recommendation_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_rec_tr_t0, algorithmic_recommendation_opacity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(algo_rec_tr_t5, algorithmic_recommendation_opacity, theater_ratio, 5, 0.55).
narrative_ontology:measurement(algo_rec_tr_t10, algorithmic_recommendation_opacity, theater_ratio, 10, 0.65).
narrative_ontology:measurement(algo_rec_tr_t15, algorithmic_recommendation_opacity, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(algo_rec_be_t0, algorithmic_recommendation_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algo_rec_be_t5, algorithmic_recommendation_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algo_rec_be_t10, algorithmic_recommendation_opacity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(algo_rec_be_t15, algorithmic_recommendation_opacity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_recommendation_opacity, information_standard).
narrative_ontology:affects_constraint(algorithmic_recommendation_opacity, social_graph_lock_in).
narrative_ontology:affects_constraint(algorithmic_recommendation_opacity, data_portability_barriers).
narrative_ontology:affects_constraint(algorithmic_recommendation_opacity, engagement_metric_maximization).
narrative_ontology:affects_constraint(algorithmic_recommendation_opacity, filter_bubble_polarization).

% DUAL FORMULATION NOTE:
% Algorithmic recommendation opacity decomposes into multiple structurally distinct constraints: (1) recommendation_ranking_opacity (ε≈0.58, Tangled Rope) — the core constraint of this story, (2) social_graph_lock_in (ε≈0.72, Snare) — network effects trapping users, (3) data_portability_barriers (ε≈0.68, Snare) — technical/legal barriers to user data export, (4) engagement_metric_maximization (ε≈0.65, Snare) — optimization for engagement rather than user preference. Each has different ε and different solution pathways. This story focuses on recommendation ranking opacity; decomposed constraints address structural lock-in and metric misalignment separately. All linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_recommendation_opacity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
