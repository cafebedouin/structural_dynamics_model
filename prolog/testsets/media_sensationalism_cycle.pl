% ============================================================================
% CONSTRAINT STORY: media_sensationalism_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_sensationalism_cycle, []).

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
 *   constraint_id: media_sensationalism_cycle
 *   human_readable: Media Sensationalism Cycle
 *   domain: media/information_systems/political_economy
 *
 * SUMMARY:
 *   The media sensationalism cycle is a structural constraint operating at
 *   the intersection of attention economics, algorithmic amplification, and
 *   business model incentives. It creates a coordination problem (infinite
 *   content competing for finite attention) that has been systematized into
 *   an extraction apparatus (engagement algorithms maximizing
 *   outrage-inducing content to maximize advertising impressions and platform
 *   capture). The constraint exhibits Tangled Rope structure: genuine
 *   coordination function (matching content to available audience attention)
 *   coexists with systematic asymmetric extraction (value captured by
 *   platforms and media outlets, costs borne by news consumers and democratic
 *   deliberation). The cycle is sustained by suppression mechanisms operating
 *   at multiple levels: algorithmic (alternative content filtered out),
 *   structural (platform incumbent power), and internalized (consumers
 *   internalize the framing that sensationalism equals importance). The
 *   theater ratio (0.68) reflects the gap between the performative functions
 *   claimed by legacy media (editorial oversight, fact-checking, narrative
 *   authority) and their actual epistemic influence (displaced by
 *   algorithms). The extractiveness has increased over the measurement
 *   interval (0.35 → 0.58) as algorithmic amplification has intensified and
 *   traditional editorial functions have atrophied. Alternative pathways
 *   exist (nonprofit journalism, subscription models, community media,
 *   algorithm transparency advocacy) but face incumbent platform power.
 *
 * KEY AGENTS:
 *   - News Consumers: Primary victims (powerless/trapped) — attention captured, epistemic quality degraded, no meaningful exit from engagement cycle
 *   - Democratic Deliberation: Primary victim (powerless/trapped) — abstract collective good; fragmented attention and manufactured urgency degrade deliberative capacity
 *   - Media Outlets: Primary beneficiaries (institutional/arbitrage) — capture advertising revenue through engagement amplification; can switch business models if needed
 *   - Advertising Platforms: Primary beneficiaries (powerful/arbitrage) — control algorithmic amplification and ad-matching; direct beneficiaries with maximum agency
 *   - Conscientious Journalists: Secondary victims (moderate/constrained) — forced to package content for algorithmic amplification while genuinely coordinating public accountability function
 *   - Political/Corporate Elites: Mixed (powerful/mobile) — both experience sensationalism as coordination problem and use it to capture narrative attention
 *   - Alternative Media Movements: Organized agents (organized/constrained) — building exit pathways but face incumbent platform barriers
 *   - Legacy Editorial Standards: Institutional actor (institutional/constrained) — persist as theater while actual epistemic function displaced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_sensationalism_cycle, 0.58).
domain_priors:suppression_score(media_sensationalism_cycle, 0.65).
domain_priors:theater_ratio(media_sensationalism_cycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_sensationalism_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_sensationalism_cycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(media_sensationalism_cycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_sensationalism_cycle, tangled_rope).
narrative_ontology:human_readable(media_sensationalism_cycle, "Media Sensationalism Cycle").
narrative_ontology:topic_domain(media_sensationalism_cycle, "media/information_systems/political_economy").

domain_priors:requires_active_enforcement(media_sensationalism_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_sensationalism_cycle, media_outlets).
narrative_ontology:constraint_beneficiary(media_sensationalism_cycle, advertising_platforms).
narrative_ontology:constraint_beneficiary(media_sensationalism_cycle, engagement_maximizers).
narrative_ontology:constraint_victim(media_sensationalism_cycle, news_consumers).
narrative_ontology:constraint_victim(media_sensationalism_cycle, democratic_deliberation).
narrative_ontology:constraint_victim(media_sensationalism_cycle, factual_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWS CONSUMER (SNARE) — Trapped in the sensationalism cycle. Algorithmic feeds amplify outrage-inducing content; switching platforms offers no escape (identical incentives across ecosystem). Cannot distinguish manufactured urgency from genuine events. Bears full cognitive/emotional extraction — attention captured, trust degraded, epistemic quality compromised. No meaningful exit.
constraint_indexing:constraint_classification(media_sensationalism_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC DELIBERATION (SNARE) — Abstract collective good that cannot organize or exit. Sensationalism fragments attention, replaces reasoning with reaction, and corrodes the epistemic commons. Generational timescale reveals structural damage: entire cohorts grow up with degraded information ecosystems. Maximum extraction with zero agency.
constraint_indexing:constraint_classification(media_sensationalism_cycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSCIENTIOUS JOURNALIST (TANGLED ROPE) — Constrained by platform algorithms and editorial pressure for engagement metrics. Also genuinely coordinates: investigative journalism serves coordination function (hold power accountable, inform public). But forced to package stories for algorithmic amplification. Mixed experience — real public service alongside extraction of labor toward engagement maximization.
constraint_indexing:constraint_classification(media_sensationalism_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEDIA OUTLET LEADERSHIP (ROPE) — Institutional beneficiary with arbitrage options (can switch business models, invest in subscriptions, build alternative revenue). Experiences the sensationalism constraint as pure coordination: engagement algorithms solve the collective action problem of competing for attention in infinite-content environment. Net beneficiary — extraction flows toward this group.
constraint_indexing:constraint_classification(media_sensationalism_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISING PLATFORM OPERATORS (ROPE) — Powerful institutional actor with multiple exit options. Sensationalism is pure coordination mechanism: maximizing time-on-platform maximizes ad impressions; engagement algorithms solve the matching problem between content and audience. Direct beneficiary. Zero effective extraction because this agent controls the constraint architecture.
constraint_indexing:constraint_classification(media_sensationalism_cycle, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY EDITORIAL STANDARDS (PITON) — Traditional fact-checking, editorial review, and narrative framing practices persist as theater: they are maintained through institutional inertia while their actual verification function is displaced by algorithmic amplification. Newsrooms still staff editors and fact-checkers, but these functions have atrophied — algorithms, not editors, determine what reaches audiences. High theater ratio reflects the gap between the ritual of editorial oversight and its actual epistemic influence.
constraint_indexing:constraint_classification(media_sensationalism_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ALTERNATIVE MEDIA MOVEMENTS (SCAFFOLD) — Organized agents (nonprofit journalism, fact-checking organizations, community media, subscription models) are building alternative verification and distribution pathways outside the sensationalism cycle. See the constraint as temporary — solvable through: decentralized platforms, norms-based curation, literacy programs, regulation. Constrained by platform incumbent power but with agency and visible exit path. Sunset timescale: 10-20 years if regulatory/market pressure shifts incentive structures.
constraint_indexing:constraint_classification(media_sensationalism_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: POLITICAL & CORPORATE ELITES (TANGLED ROPE) — Powerful agents with mobile options (can hire PR teams, buy favorable coverage, fund propaganda outlets). Experience sensationalism as both coordination and extraction: the same cycle that distracts the public is available for elite capture (weaponized outrage, managed scandals). Mixed experience — genuine coordination problem of competing for narrative attention alongside extraction of media infrastructure for preferential access.
constraint_indexing:constraint_classification(media_sensationalism_cycle, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, sensationalism is a structural coordination problem (infinite content, finite attention) with asymmetric extraction (algorithms capture value from engagement, outlets monetize attention). Genuine coordination function (matching content to audience) exists alongside systematic extraction (attention harvesting, trust degradation, deliberative quality collapse). Classification reflects the hybrid nature: coordination mechanism corrupted into extraction apparatus.
constraint_indexing:constraint_classification(media_sensationalism_cycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_sensationalism_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_sensationalism_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_sensationalism_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_sensationalism_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_sensationalism_cycle, TR),
    TR >= 0.70.

:- end_tests(media_sensationalism_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, with upward trajectory. The sensationalism cycle extracts attention, trust, and deliberative quality from consumers and the public sphere, while platforms and outlets capture advertising value. The metric reflects genuine coordination (matching infinite content to finite attention) corrupted into extraction apparatus (outrage amplification). Suppression (0.65): High. Barriers include platform algorithmic control (alternative content suppressed), incumbent platform power (switching is costly due to network effects and exclusive distribution deals), and cognitive capture (consumers internalize sensationalism-as-importance frame). Some suppression is structural (genuine platform monopoly in attention markets); some is internalized (trust in media outlets degraded, but consumers lack frameworks to imagine alternative epistemic sources). Theater ratio (0.68): High and increasing. Traditional editorial functions (fact-checking, narrative curation, source verification) persist as institutional practice but have atrophied in actual epistemic influence — algorithms, not editors, determine what reaches audiences. The ritual is maintained through legacy media institution inertia.
 *
 * PERSPECTIVAL GAP:
 *   The gap between consumer (Snare) and platform (Rope) perspectives reveals the core structural dynamics. Consumers perceive the constraint as extractive and inescapable; platforms perceive it as coordination function that solves attention allocation. This gap is NOT a difference in how well each agent understands the mechanism — it is a structural difference in their relationship to it. The mechanism that coordinates infinite content to finite attention (an actual coordination problem) has been architected to extract value from engagement toward advertising platforms. Both perspectives are correct given their positions: the constraint IS a coordination mechanism AND an extraction apparatus. The Tangled Rope classification captures this hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position: Who benefits? Who bears costs? What are actual exit options? Media outlets and platforms are beneficiaries with arbitrage exit options (can shift to subscriptions, diversify revenue, fund alternative platforms) — they experience low or negative effective extraction. News consumers are victims with trapped exit options (algorithms everywhere, network effects prevent switching) — they experience high extraction. Journalists are mixed (moderate power, constrained exit) — they benefit from coordination functions but are extracted from to maximize engagement. The conscientious journalist's extraction comes from forced repackaging of substantive stories for algorithmic amplification, requiring labor to frame important-but-unsexy content as sensational. Democratic deliberation has no exit and cannot organize — maximum experienced extraction. The platform operators' institutional power and arbitrage options place them at the beneficiary end of the directionality spectrum; trapped consumers place them at the victim end.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that sensationalism serves dual functions: (1) coordination of attention allocation in infinite-content environments (genuine problem), and (2) extraction of attention, trust, and deliberative quality toward platforms and outlets (genuine extraction). The constraint cannot be simply dissolved — doing so would leave the underlying coordination problem unsolved. But it also cannot be naturalized as inevitable — alternative models (subscription, nonprofit, community media, algorithm transparency) have demonstrated viability at smaller scale. The scaffold perspective identifies the actual resolution path: transitional support for alternative platforms with eventual sunset of the sensationalism cycle as market/regulatory conditions shift. The mandatrophy reveals that previous analyses conflated the coordination problem with the extraction apparatus, or treated extraction as necessary overhead for solving coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_scarcity_inherence,
    'Is sensationalism inherent to finite attention spans or contingent to advertising-driven business models?',
    'Comparison of non-commercialized or subscription-funded media organizations with advertising-funded peers; analysis of content sensationalism across different revenue models and market structures',
    'If inherent: sensationalism is a coordination problem (Rope from all perspectives). If contingent: sensationalism is extractive apparatus (Snare becomes dominant). If mixed: Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_scarcity_inherence, empirical, 'Whether sensationalism is inherent to attention economics or contingent to business model').

omega_variable(
    algorithmic_amplification_intentionality,
    'Do engagement algorithms maximize sensationalism intentionally (designed extraction) or as unintended byproduct of engagement maximization?',
    'Internal algorithmic audits; documentation of platform design decisions; A/B testing records showing engagement optimization vs. content quality tradeoffs; expert analysis of algorithm architecture',
    'If designed: deliberate Snare by platforms (pure extraction mechanism). If unintended byproduct: tragic commons problem (Tangled Rope). If intentional but justified as necessary: Rope (coordination via engagement matching).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_amplification_intentionality, empirical, 'Whether sensationalism amplification is designed or unintended').

omega_variable(
    alternative_platform_viability,
    'Can alternative media platforms (nonprofit, subscription, algorithm-lite) sustain quality journalism at scale?',
    'Economic analysis of subscription models, nonprofit journalism funding, community media sustainability; comparison of content quality and audience reach across models; 5-10 year longitudinal tracking of alternative platform growth',
    'If viable: scaffold sunset is real (alternative exit exists). If unviable: trapped agents have no actual exit path, snare classification confirmed. If partially viable: scaffold for some agents, snare for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative media platforms can scale sustainably').

omega_variable(
    consumer_agency_in_attention_capture,
    'Do news consumers actively prefer sensational content or are they manipulated into engagement?',
    'Behavioral studies with choice architecture experiments; user interviews about content preferences; A/B testing showing preference for sensational vs. substantive content when attention constraints are relaxed; analysis of user behavior after algorithm changes',
    'If active preference: consumers see sensationalism as coordination benefit (Rope). If manipulated: consumers are trapped (Snare). If both: genuine coordination with predatory design (Tangled Rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_agency_in_attention_capture, empirical, 'Degree of consumer agency in sensational content consumption').

omega_variable(
    suppression_mechanism_origin,
    'Is suppression of alternative information pathways structural (platform monopoly) or internalized (cognitive capture)?',
    'Analysis of actual access to alternative news sources; measurement of awareness/usage of independent media; content recommendation analysis; geographic variation in suppression across different market structures',
    'If structural: exit barriers are material (trapped/constrained). If internalized: cognitive capture via identity/trust (identity_locked exit option becomes relevant). If both: suppression is multiplicative (harder to escape).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Structural vs. internalized suppression of alternative media').

omega_variable(
    regulatory_intervention_design,
    'Can regulation address sensationalism without enabling censorship or regulatory capture?',
    'Analysis of existing regulatory approaches (media ownership rules, public broadcasting, fact-checking mandates, algorithmic transparency); international comparison of regulatory models; impact assessment of different interventions on content quality and platform behavior',
    'If viable: regulation-based scaffold becomes real (sunset path exists). If not: constraint may become permanent or shift form. If badly designed: regulation becomes its own extraction mechanism (new Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_design, preference, 'Viability of regulation to address sensationalism without creating new constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_sensationalism_cycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mediasens_tr_t0, media_sensationalism_cycle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mediasens_tr_t5, media_sensationalism_cycle, theater_ratio, 5, 0.55).
narrative_ontology:measurement(mediasens_tr_t10, media_sensationalism_cycle, theater_ratio, 10, 0.68).
narrative_ontology:measurement(mediasens_tr_t2, media_sensationalism_cycle, theater_ratio, 2, 0.48).
narrative_ontology:measurement(mediasens_tr_t7, media_sensationalism_cycle, theater_ratio, 7, 0.62).

% Extraction over time
narrative_ontology:measurement(mediasens_be_t0, media_sensationalism_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mediasens_be_t5, media_sensationalism_cycle, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mediasens_be_t10, media_sensationalism_cycle, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mediasens_be_t2, media_sensationalism_cycle, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(mediasens_be_t7, media_sensationalism_cycle, base_extractiveness, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_sensationalism_cycle, resource_allocation).
narrative_ontology:boltzmann_floor_override(media_sensationalism_cycle, 0.18).
narrative_ontology:affects_constraint(media_sensationalism_cycle, platform_algorithmic_amplification).
narrative_ontology:affects_constraint(media_sensationalism_cycle, advertising_attention_market).
narrative_ontology:affects_constraint(media_sensationalism_cycle, journalistic_epistemic_authority).
narrative_ontology:affects_constraint(media_sensationalism_cycle, public_trust_degradation).

% DUAL FORMULATION NOTE:
% The media sensationalism cycle decomposes into structurally distinct constraints: (1) attention scarcity coordination (genuine problem), (2) algorithmic engagement amplification (extraction mechanism), (3) advertising value capture (distribution of benefits), (4) editorial standard atrophy (institutional piton). This story represents the unified constraint at the system level; downstream constraints track specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(media_sensationalism_cycle, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
