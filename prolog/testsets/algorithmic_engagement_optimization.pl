% ============================================================================
% CONSTRAINT STORY: algorithmic_engagement_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_engagement_optimization, []).

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
 *   constraint_id: algorithmic_engagement_optimization
 *   human_readable: Algorithmic Engagement Optimization in Social Media Platforms
 *   domain: technology/platform_governance/behavioral_economics
 *
 * SUMMARY:
 *   Algorithmic engagement optimization in social media platforms represents
 *   a foundational extraction constraint in the digital age. Platforms
 *   present engagement optimization as a necessary technical solution to
 *   information distribution at scale: with billions of users and trillions
 *   of potential posts, some algorithmic curation is unavoidable. However,
 *   the specific choice to optimize for engagement metrics (time spent,
 *   shares, likes, comments) rather than alternative targets (user wellbeing,
 *   epistemic quality, attention security) is a contingent design decision
 *   driven by the advertising-based monetization model. This constraint
 *   exhibits complex perspectival structure across all six types: individual
 *   users experience it as a Snare (network lock-in, no practical exit);
 *   content creators experience mixed coordination and extraction (Tangled
 *   Rope); advertisers experience pure coordination (Rope); regulators
 *   experience mixed dynamics with capture risk (Tangled Rope); the attention
 *   commons is identity-locked within the constraint itself (Snare);
 *   governance mechanisms are substantially performative (Piton); and
 *   analytical observers risk naturalizing a policy choice as an immutable
 *   law of information (Mountain). The extractiveness has accelerated over
 *   time (from 0.35 to 0.62 over 9 years), driven by increasingly
 *   sophisticated behavioral targeting, while the theater ratio has also
 *   increased (0.35 to 0.68), reflecting the platform's escalating governance
 *   performance to manage legitimacy pressure from regulators and civil
 *   society.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — structurally locked by network effects; no practical alternatives despite theoretical availability
 *   - Content Creators: Secondary victims/partial beneficiaries (moderate/constrained) — benefit from algorithmic distribution but extracted through reach suppression and platform control
 *   - Advertising Networks: Primary beneficiaries (institutional/arbitrage) — gain efficient targeting; extraction runs in their favor
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture attention and data; revenue model depends on engagement optimization
 *   - Social Cohesion and Epistemic Commons: Structural victims (powerless/identity_locked) — abstract collective good captured by the optimization function itself
 *   - Regulatory Coalition: Organized agents (organized/constrained) — attempt coordination oversight but face capture and asymmetric information
 *   - Platform Governance Systems: Institutional performers (institutional/arbitrage) — maintain compliance theater; actual control mechanisms are attenuated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_engagement_optimization, 0.62).
domain_priors:suppression_score(algorithmic_engagement_optimization, 0.58).
domain_priors:theater_ratio(algorithmic_engagement_optimization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_engagement_optimization, extractiveness, 0.62).
narrative_ontology:constraint_metric(algorithmic_engagement_optimization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(algorithmic_engagement_optimization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_engagement_optimization, tangled_rope).
narrative_ontology:human_readable(algorithmic_engagement_optimization, "Algorithmic Engagement Optimization in Social Media Platforms").
narrative_ontology:topic_domain(algorithmic_engagement_optimization, "technology/platform_governance/behavioral_economics").

domain_priors:requires_active_enforcement(algorithmic_engagement_optimization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_engagement_optimization, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_engagement_optimization, advertising_networks).
narrative_ontology:constraint_victim(algorithmic_engagement_optimization, user_autonomy).
narrative_ontology:constraint_victim(algorithmic_engagement_optimization, social_cohesion).
narrative_ontology:constraint_victim(algorithmic_engagement_optimization, mental_health_commons).
narrative_ontology:constraint_victim(algorithmic_engagement_optimization, attention_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Users are structurally trapped by network effects and social dependency; alternatives offer no genuine escape (Mastodon has no audience, Signal lacks their social graph). The algorithm extracts attention and behavioral data with minimal coordination benefit. Users perceive the trap as immutable: 'everyone is on this platform, I have no choice.' Maximum suppression; the alternatives are theoretically available but practically unusable due to network lock-in.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Creators experience genuine coordination: the algorithm distributes their content to audiences, enabling reach they could not achieve independently. But the same algorithm also extracts through recommendation opacity and reach suppression for non-optimized content. Creators are constrained by career dependency (many earn primary income through platform monetization) but have some exit capacity (diversifying platforms, direct audience). Mixed experience: real coordination function coupled with asymmetric extraction through algorithmic prioritization.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADVERTISING NETWORK (ROPE) — Advertisers experience the algorithm as pure coordination: it solves the targeting problem (reaching relevant audiences) and the efficiency problem (maximizing conversion per impression). The extraction runs in the advertiser's favor — they benefit from better-than-alternative targeting. This is the perspective from which the constraint appears as successful coordination, not extraction.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Regulators (EU Digital Services Act, UK Online Safety Bill, FTC) face genuine coordination challenges: algorithmic systems are opaque, multi-stakeholder governance is required, and coordination benefits are real (shared safety standards reduce race-to-the-bottom dynamics). But regulators are also targets of extraction: compliance theater, lobbying capture, and regulatory arbitrage allow platforms to appear compliant while maintaining core extraction mechanisms. Organized agents with constrained exit (must regulate, cannot opt out) experience mixed rope and snare dynamics.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL COHESION / ATTENTION COMMONS (SNARE, identity_locked) — The abstract collective good (shared epistemic reality, distributed attention capacity, social trust) cannot exit and cannot organize. It is constituted through the platforms themselves — the commons exists as a property of the network, making exit literally unthinkable for distributed agents. The constraint extracts attention to engagement metrics at the cost of social fragmentation, polarization, and reduced epistemic reliability. The binding is cognitive: agents internalize the platform's framing ('engagement is value,' 'viral is important') making the constraint appear self-evident rather than imposed. No external alternatives exist because the commons has been captured by the optimization function itself.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM GOVERNANCE (PITON) — Content moderation, transparency reports, community standards enforcement, and algorithmic audit boards are substantially performative. Platforms maintain elaborate governance rituals (appeal processes, policy documentation, external advisory boards) that create appearance of accountability while core engagement optimization proceeds unaltered. Theater ratio (0.68) reflects that the governance apparatus persists through institutional inertia (expected by regulators, users, and stakeholders) but core function (protecting attention security and cohesion) has atrophied. The real coordination occurs at the engagement optimization layer, not the governance layer.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, algorithmic optimization appears as an immutable constraint of information at scale: any system distributing content to billions of people must use algorithmic filtering (no human moderator can review billions of posts), and any algorithm is vulnerable to gaming toward engagement metrics (Goodhart's law: 'when a measure becomes a target, it ceases to be a good measure'). This perspective risks naturalizing what is actually a contingent design choice. The extraction mechanism (maximizing engagement) is presented as inevitable, but alternative optimization targets (user wellbeing, epistemic quality, attention security) are technically feasible — the mountain classification conceals a policy choice.
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — A regional/biographical analytical perspective sees the constraint as primarily a coordination mechanism: platforms have solved the genuine problem of information discovery and social connection at scale. From this view, engagement optimization is a necessary coordination tool, not an extraction mechanism. The extractive aspects are side effects, not primary function. This perspective emphasizes what the algorithm enables (connection, reach, discoverability) rather than what it extracts (attention, data, autonomy). The perspectival gap between the civilizational natural law and the biographical coordination views reveals the mandate: which aspects of the constraint are inevitable, and which are design choices?
constraint_indexing:constraint_classification(algorithmic_engagement_optimization, rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_engagement_optimization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_engagement_optimization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_engagement_optimization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_engagement_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_engagement_optimization, TR),
    TR >= 0.70.

:- end_tests(algorithmic_engagement_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising. The algorithm extracts user attention, behavioral data, and autonomy through engagement optimization. Users experience loss of agency (content selection is determined by platform ranking, not user choice), loss of privacy (behavioral tracking for targeting), and loss of epistemic control (exposure to engagement-optimized content rather than information of actual interest). The extractiveness is not total (users can still post, view, and communicate) but substantial — the primary value flows to platform operators, not users. The trajectory from 0.35 to 0.62 over 9 years reflects increasing sophistication of behavioral targeting and deepening integration of engagement metrics into platform architecture. Suppression (0.58): Moderate-high and structural. Network lock-in is the primary suppression mechanism: users cannot leave without losing access to their social graph, and alternative platforms offer no comparative advantage because their user bases are small. Data portability and interoperability would reduce suppression, but platforms actively resist these (regulatory compliance is theater; actual technical barriers persist). Secondary suppression operates through cognitive capture: users have internalized engagement-as-value framing, making exit feel impossible even for structurally mobile agents. Theater ratio (0.68): High and rising. Platform governance systems (content moderation, transparency reports, algorithmic audit boards, community standards) are substantially performative. The apparatus creates appearance of accountability and user protection while core optimization proceeds unaltered. The ratio has increased over time as regulatory pressure has forced platforms to elaborate governance infrastructure without changing extraction mechanisms. The Piton classification is accurate: the governance system is degraded, maintained through institutional inertia and regulatory expectation rather than genuine function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Individual users (trapped, powerless) experience pure Snare — extraction with no coordination benefit. Content creators (constrained, moderate) experience Tangled Rope — real coordination (reach) coupled with extraction (algorithmic control). Advertisers (institutional, arbitrage) experience Rope — successful coordination for their objectives. The open question is whether the constraint's primary function is coordination (information distribution at scale) or extraction (attention capture for advertising value). The analytrical observer's mountain is a false summit: the naturalization of engagement optimization as inevitable masks the contingency of the monetization model. The civilization/universal perspective risks saying 'algorithmic filtering is necessary for information at scale, therefore engagement optimization is necessary,' but this conflates the technical necessity of curation with the policy choice of engagement metrics. Alternative curation functions exist (recency, quality, serendipity, user-specified preferences) and are technically feasible. The perspectival gap reveals the mandate: identify which aspects are truly constrained by information physics (some curation is necessary) and which are contingent policy choices (engagement metrics are chosen, not forced).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Individual users: victims of network lock-in with no exit capacity → high d (0.92+) → high experienced extraction (f(d) ≈ 1.35). Content creators: mixed — beneficiaries of reach but victims of algorithmic control, constrained exit → moderate d (0.58) → moderate experienced extraction. Advertisers: pure beneficiaries with high exit capacity (can shift budgets) → low d (0.18) → low/negative experienced extraction (f(d) ≈ -0.02). Regulators: organized agents with constrained exit (must regulate, cannot opt out) and mixed victim/beneficiary status (regulation is their function, but they are also captured) → moderate d (0.55). The social commons: cannot exit, cannot organize, trapped within the constraint structure → very high d (0.95+). Platform operators: beneficiaries with arbitrage exit → very low d (0.08) → strong negative experienced extraction. The directionality derivation produces substantial gaps: beneficiaries experience the constraint as coordination (low χ from low d), while victims experience it as extraction (high χ from high d). This gap is the entire analytical content of the Tangled Rope classification — the constraint is simultaneously coordination for beneficiaries and extraction for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint exhibits genuine ambiguity between classification types, not from observer error but from structural contingency. At ε = 0.62, the constraint is above the Snare floor (0.46) and below the Mountain ceiling (0.25 incompatible), placing it in high-extractiveness territory. The classification depends on observable choice: (1) Measure from the advertising efficiency perspective: the constraint solves a genuine coordination problem (targeting), and extractiveness drops to 0.20-0.30 (Rope). (2) Measure from the user autonomy perspective: the constraint is pure extraction, and extractiveness rises to 0.75+ (Snare). (3) Measure from the social cohesion perspective: the constraint extracts epistemic commons at 0.62 with some coordination value for platform operators and creators (Tangled Rope). The mandatrophy is NOT resolvable by better measurement — it reflects that the constraint serves incompatible functions (coordination for some, extraction for others) simultaneously. The resolution is perspectival: declare which objectives are primary (user autonomy? platform efficiency? social cohesion?), and the classification follows. The engine cannot adjudicate this normative choice. For now, Tangled Rope is the most accurate classification because it acknowledges both the coordination function (genuine, measurable, beneficial for creators and advertisers) and the extraction function (genuine, asymmetric, costly for users and commons). The unresolved mandatrophy indicates that policy intervention should target the objectifive mismatch: either (a) change the optimization target to user wellbeing (makes it more clearly Tangled Rope or Rope), or (b) accept that maximizing engagement extracts from users and commons, and design compensation or exit mechanisms accordingly (keeps it Snare from victims' perspective).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimization_target_substitution,
    'Is engagement optimization a necessary feature of information distribution at scale, or a contingent design choice driven by advertising-based monetization?',
    'Comparison of platforms with different revenue models: Wikipedia (no engagement optimization), TikTok (maximum engagement optimization), Bluesky (algorithmic choice), Signal (no algorithm). Measurement of social outcomes and user autonomy across model types.',
    'If necessary: constraint is closer to Mountain (information law); extraction is coordination cost. If contingent: constraint is Snare (design-driven extraction); platforms chose to optimize for engagement because it maximizes advertising value, not because alternatives are impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_target_substitution, empirical, 'Whether engagement optimization is necessary or a contingent monetization choice').

omega_variable(
    identity_lock_cognitive_scope,
    'How much of user suppression is structural (network lock-in, data portability barriers) vs. internalized (users have adopted the platform''s framing of engagement value and have identity-fused with their digital presence)?',
    'Longitudinal studies of users who exit vs. attempt to exit; analysis of stated reasons for platform dependence (structural barriers vs. identity/FOMO); measurement of suppression persistence after users leave (do they feel liberated or do they carry the suppression frame with them?).',
    'If primarily structural: suppression can be reduced by interoperability and portability. If primarily internalized: users carry the extraction mechanism with them; regulation must address cognitive capture, not just technical barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cognitive_scope, empirical, 'Proportion of suppression that is structural vs. internalized identity lock').

omega_variable(
    attention_commons_irreversibility,
    'Is the fragmentation of shared epistemic reality (dissolution of common news/reality baseline) reversible if engagement optimization is eliminated, or has the network already bifurcated beyond repair?',
    'Measurement of epistemic cohesion metrics (shared information exposure, consensus on basic facts) pre- and post-major algorithmic changes (EU DMA enforcement, TikTok algorithm restrictions). Longitudinal tracking of polarization metrics across simulated vs. real recommendation changes.',
    'If reversible: constraint is Tangled Rope (can be reformed). If irreversible: constraint has caused permanent damage to the commons; classification should reflect that the social cost is now path-dependent and no longer contingent on the constraint''s continuation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attention_commons_irreversibility, empirical, 'Whether attention commons fragmentation is reversible').

omega_variable(
    regulatory_capture_feedback,
    'Are regulatory mechanisms (Digital Services Act, Online Safety Bill) being incorporated into the extraction logic itself, such that compliance theater becomes a new coordination cost layer rather than a control mechanism?',
    'Analysis of platform compliance responses to major regulations; measurement of whether compliance costs are passed to users (slower platforms, reduced features) or absorbed by platforms; tracking of enforcement action effectiveness (do investigations reduce extraction, or do they only require theater).',
    'If captured: regulatory layer becomes part of the Tangled Rope, not a control mechanism. Extraction persists but now includes compliance costs. If effective: regulatory mechanisms can force optimization target shift from engagement to user wellbeing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback, empirical, 'Whether regulations are being incorporated into extraction logic').

omega_variable(
    alternative_algorithm_feasibility,
    'Are alternative optimization targets (user wellbeing, epistemic quality, attention security) technically feasible with the same data and computational resources, or do they face fundamental information barriers?',
    'Implementation experiments: test alternative ranking functions on production traffic; measure user satisfaction, engagement, and social outcomes. Comparison with research implementations (well-being ranking, serendipity ranking, epistemic quality ranking).',
    'If feasible: constraint is pure policy choice (Snare becomes most accurate classification). If infeasible: engagement optimization is constrained by information barriers (constraint has Mountain aspects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_algorithm_feasibility, empirical, 'Technical feasibility of alternative algorithmic optimization targets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_engagement_optimization, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algeng_tr_t0, algorithmic_engagement_optimization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algeng_tr_t3, algorithmic_engagement_optimization, theater_ratio, 3, 0.48).
narrative_ontology:measurement(algeng_tr_t6, algorithmic_engagement_optimization, theater_ratio, 6, 0.62).
narrative_ontology:measurement(algeng_tr_t9, algorithmic_engagement_optimization, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(algeng_be_t0, algorithmic_engagement_optimization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algeng_be_t3, algorithmic_engagement_optimization, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(algeng_be_t6, algorithmic_engagement_optimization, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(algeng_be_t9, algorithmic_engagement_optimization, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_engagement_optimization, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_engagement_optimization, attention_scarcity_market).
narrative_ontology:affects_constraint(algorithmic_engagement_optimization, data_extraction_surveillance).
narrative_ontology:affects_constraint(algorithmic_engagement_optimization, polarization_amplification_cycle).

% DUAL FORMULATION NOTE:
% Algorithmic engagement optimization is a constraint family linking three structurally distinct constraints: (1) attention resource allocation (platform must distribute limited user attention; engagement optimization is one allocation rule), (2) behavioral data extraction (optimization requires behavioral tracking, which extracts data), (3) content ranking and discoverability (what users see is determined by algorithms; ranking for engagement has downstream effects on information landscape). Each has distinct ε values and beneficiary/victim structure. This story focuses on the primary extraction mechanism (engagement optimization itself); see network links for downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_engagement_optimization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
