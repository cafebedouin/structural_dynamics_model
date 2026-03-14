% ============================================================================
% CONSTRAINT STORY: subscriber_attention_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscriber_attention_commons, []).

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
 *   constraint_id: subscriber_attention_commons
 *   human_readable: Subscriber Attention Commons Extraction
 *   domain: digital_media/attention_economy
 *
 * SUMMARY:
 *   The subscriber attention commons creates a structural extraction
 *   mechanism where digital platforms organize subscriber time and behavioral
 *   data for monetization through advertising and creator paywalls, while
 *   simultaneously providing genuine coordination benefits through content
 *   discovery and community formation. The constraint exhibits a perspectival
 *   range from pure coordination (platform view) through hybrid
 *   extraction-coordination (creator and moderate user view) to pure
 *   extraction (trapped subscriber and attention commons victim view) to
 *   degraded theater (notification systems) to aspirational sunset
 *   (regulatory coalition). Extractiveness has increased over the interval
 *   (0.35 → 0.62) as platforms have intensified engagement optimization
 *   through dark patterns, autoplay, algorithmic feeds, and notification
 *   systems. Theater ratio has risen from 0.38 to 0.55, reflecting that an
 *   increasing proportion of notification and recommendation value serves
 *   platform engagement metrics rather than user welfare. The constraint is
 *   foundational to the attention economy and exhibits the key characteristic
 *   of tangled rope: genuine coordination function (connecting creators to
 *   audiences, helping users discover content) embedded within asymmetric
 *   extraction (time harvesting, behavioral data collection, paywall
 *   gatekeeping).
 *
 * KEY AGENTS:
 *   - Subscribers: Primary victim (powerless/trapped) — bear extraction through attention capture, algorithmic sorting lock-in, behavioral data harvesting, and limited transparency about mechanisms
 *   - Attention Commons: Secondary victim (abstract collective, unable to organize) — the shared epistemic and social benefit of authentic content discovery and community connection, degraded by optimization for engagement metrics
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture value through advertising, data monetization, and rent extraction from creators and subscribers
 *   - Premium Creators: Secondary beneficiary (powerful/mobile) — gain distribution and subscriber revenue infrastructure; also co-extract through paywalled content and tiered access
 *   - Attention-Aware Users: Moderate agent (moderate/constrained) — constrained by switching costs and social embeddedness; experience both genuine coordination benefit and extraction
 *   - Regulation Coalition: Organized agents (organized/constrained) — EU regulators (DSA), consumer advocates, open-source platforms (ActivityPub ecosystem) — building interoperability and transparency mandates with regulatory sunset logic
 *   - Notification Theater: Institutional mechanism (institutional/arbitrage) — notification systems, autoplay, and algorithmic feeds that persist through inertia and opacity despite atrophied primary function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscriber_attention_commons, 0.62).
domain_priors:suppression_score(subscriber_attention_commons, 0.58).
domain_priors:theater_ratio(subscriber_attention_commons, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscriber_attention_commons, extractiveness, 0.62).
narrative_ontology:constraint_metric(subscriber_attention_commons, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(subscriber_attention_commons, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscriber_attention_commons, tangled_rope).
narrative_ontology:human_readable(subscriber_attention_commons, "Subscriber Attention Commons Extraction").
narrative_ontology:topic_domain(subscriber_attention_commons, "digital_media/attention_economy").

domain_priors:requires_active_enforcement(subscriber_attention_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscriber_attention_commons, platform_operators).
narrative_ontology:constraint_beneficiary(subscriber_attention_commons, premium_content_creators).
narrative_ontology:constraint_victim(subscriber_attention_commons, subscribers).
narrative_ontology:constraint_victim(subscriber_attention_commons, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSCRIBER (SNARE) — Trapped by subscription lock-in, algorithmic sorting that makes exit costly (loss of curated feeds, habit dependency, social integration), and minimal transparency about extraction mechanisms. Bears the cost of unlimited attention capture through notification systems, autoplay, recommendation feeds optimized for engagement rather than user welfare. No meaningful exit option without abandoning social ties and content access.
constraint_indexing:constraint_classification(subscriber_attention_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ATTENTION-AWARE USER (TANGLED ROPE) — Constrained by switching costs and social embeddedness but capable of partial exit (reducing usage, account dormancy, or multi-platform presence). Experiences genuine coordination benefit (discovering content, connecting with communities) alongside extraction (time harvesting, behavioral data collection, dark patterns). Exit is costly but possible at a price.
constraint_indexing:constraint_classification(subscriber_attention_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from subscriber attention lock-in through advertising and data monetization. Experiences the constraint as a coordination mechanism: organizing subscriber attention enables content discovery, social connection, and creator monetization. Net beneficiary — extraction runs toward this agent. Can arbitrage between different subscriber segments and alternative monetization mechanisms.
constraint_indexing:constraint_classification(subscriber_attention_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PREMIUM CREATOR (TANGLED ROPE) — Powerful institutional agent with audience mobility options. Benefits from subscriber platform distribution and monetization infrastructure. Also extracts from subscribers through paywalled content, Patreon mechanics, or exclusive access gates. Genuine coordination function (content distribution) with asymmetric extraction (premium tier pricing, subscriber attention hoarding). Mobile exit option but costly (audience rebuilding).
constraint_indexing:constraint_classification(subscriber_attention_commons, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATION COALITION (SCAFFOLD) — Organized agents (regulators, consumer advocates, open-source platforms) see subscriber attention extraction as a temporary coordination failure solvable through design mandates: opt-in notifications, algorithmic transparency, data rights, interoperability requirements. These represent sunset clauses — as regulatory frameworks mature (DSA in EU, potential US privacy legislation), the extraction mechanism loses force. Low effective extraction from this perspective because the coalition has agency and sees an exit path through norm-setting.
constraint_indexing:constraint_classification(subscriber_attention_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NOTIFICATION THEATER (PITON) — Notification systems, autoplay defaults, and recommendation algorithms are largely performative from a user-welfare perspective. They ostensibly help users discover valuable content but are primarily tuned to maximize time-on-platform and click metrics. The primary function (content discovery) has atrophied; the mechanism persists through institutional inertia and opacity. Theater ratio reflects that 70% of notification value is engagement optimization rather than genuine user-requested notification. Platforms maintain the ritual because alternatives haven't fully replaced them.
constraint_indexing:constraint_classification(subscriber_attention_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ATTENTION SCARCITY VIEW (MOUNTAIN) — From a civilizational perspective, some attention allocation is inherent to digital coordination: human attention is finite, and any system organizing information must triage. This perspective sees subscriber attention extraction as an immutable natural law of information abundance. However, the structural data contradicts this mountain classification — the base metrics reveal that extraction is contingent on platform architecture choices (notification defaults, algorithmic ranking, paywall design) and institutional power asymmetries, not attention scarcity itself. The false summit reveals naturalization of design choices as inevitable laws.
constraint_indexing:constraint_classification(subscriber_attention_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscriber_attention_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscriber_attention_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscriber_attention_commons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscriber_attention_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(subscriber_attention_commons, TR),
    TR >= 0.70.

:- end_tests(subscriber_attention_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. Subscribers lose time (bounded by 24 hours/day but concentrated through dark patterns), behavioral data (proprietary algorithmic models), and epistemic clarity (authentic interest discovery degraded by engagement optimization). The extraction is substantial but not maximal (0.72+) because genuine coordination benefits (content discovery, community) remain real — the extraction is not pure rent-seeking but genuine value capture during value creation. Theater ratio (0.55): Moderate-high. Notification systems, autoplay, and recommendation algorithms are nominally tools for user content discovery but are primarily tuned to maximize time-on-platform and engagement metrics. About 45% of notification/recommendation value serves genuine user interests; 55% serves platform engagement optimization. This is lower than pure piton theater (which would be 0.70+) because some genuine coordination function remains — but substantial enough to flag as extractive theater. Suppression (0.58): Moderate-high. Barriers to exit include switching costs (social graph, habit, data lock-in), algorithmic sorting that creates habitual dependency, limited awareness of extraction mechanisms (opacity), and social integration (FOMO, community belonging). But suppression is not maximal because alternative platforms exist and some users do exit or reduce usage — the barrier is real but penetrable at a cost.
 *
 * PERSPECTIVAL GAP:
 *   The trapped subscriber perceives a snare (pure extraction, no exit); the platform operator perceives rope (coordination mechanism generating value); the regulation coalition perceives a scaffold (temporary problem solvable through interoperability mandates); the notification theater is a piton (degraded ritual sustained by inertia). The civilizational analytical observer risks classifying this as a mountain (attention scarcity is inherent to information abundance) but the structural data reveals this as false naturalization — the extraction flows from platform architecture choices (algorithmic ranking, notification defaults, paywall design) and power asymmetries, not from scarcity itself. A subscriber with full algorithmic autonomy and transparent notification controls would experience dramatically lower extraction. The perspectival gap reveals that what appears as 'natural' attention economics is actually a contingent set of design choices and regulatory gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim flow: Platform operators benefit from subscriber attention and behavioral data (arbitrage exit, institutional power, low d). Premium creators benefit from subscriber platform distribution and revenue infrastructure (mobile exit, powerful position, moderate d). Subscribers bear extraction through time, data, and epistemic clarity loss (trapped exit, powerless position, high d). Attention commons bears the abstracted loss of coordinate authentic value discovery (no exit, no power, very high d). The derivation chain produces: Subscriber d ≈ 0.93 (trapped + victim + powerless) → f(d) ≈ 1.38 → chi ≈ 0.62 × 1.38 × 1.0 ≈ 0.85 (high effective extraction). Platform operator d ≈ 0.10 (arbitrage + beneficiary + institutional) → f(d) ≈ -0.09 → chi ≈ 0.62 × (-0.09) × 1.0 ≈ -0.06 (negative — they subsidize the arrangement). Premium creator d ≈ 0.55 (mobile + mixed beneficiary/victim + powerful) → f(d) ≈ 0.75 → chi ≈ 0.62 × 0.75 × 1.2 ≈ 0.56 (moderate, scaled by continental scope of creator networks). Scope modifier σ(global) = 1.2 amplifies chi for global-scope agents (platform operators, creators), slightly dampening effective beneficiary advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids the mandatrophy trap by clearly separating the coordination function (genuine value creation through content discovery, community formation, creator distribution) from the extraction mechanism (time harvesting, behavioral data collection, paywall gatekeeping, algorithmic engagement optimization). The presence of both a coordination function and asymmetric extraction, along with active enforcement (algorithmic ranking, notification defaults, paywall systems), confirms the tangled rope classification. The constraint is not purely extractive (which would deny the real value coordination creates) nor purely coordinative (which would deny the asymmetric value capture). The scaffold perspective demonstrates that the constraint has contingent elements — regulatory interoperability and algorithmic transparency mandates could shift the ratio toward pure coordination or reduce suppression. The piton perspective correctly identifies degraded theater (notifications as engagement optimization rather than user-requested discovery). The mountain perspective is identified as a false summit — attention scarcity is real but extraction flows from architecture choices, not scarcity itself. The mandatrophy is resolved by recognizing that subscriber attention commons is a genuine tangled rope: coordination mechanisms (content discovery, community) captured within an extractive architecture (attention optimization, paywall gatekeeping).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notification_value_threshold,
    'What proportion of notification content is user-requested vs algorithmically pushed engagement optimization?',
    'A/B testing of opt-in notification systems; comparison of user satisfaction and content discovery rates between curated (user-requested) and algorithmic push notification cohorts',
    'If >80% is user-requested: notification mechanism is genuine coordination (Rope from subscriber perspective). If <40% is user-requested: notification mechanism is pure extraction theater (Snare from subscriber perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notification_value_threshold, empirical, 'Proportion of notifications driven by user request vs algorithmic optimization').

omega_variable(
    switching_cost_magnitude,
    'What is the true cost (time, social capital, data loss) for a subscriber to switch platforms or reduce platform dependency by 50%?',
    'Cohort tracking of users who exit or reduce usage; measurement of re-engagement costs, social link loss, content library access loss, and time required to rebuild comparable experience on alternative platforms',
    'If cost < 10 hours of time: exit options should be reclassified as ''mobile'' rather than ''trapped''; suppression should be reduced from 0.58 to ~0.35; constraint may reclassify as Rope or Scaffold. If cost > 50 hours: trapped classification is validated; suppression may increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_magnitude, empirical, 'True cost to subscribers of platform exit or significant usage reduction').

omega_variable(
    algorithmic_recommendation_autonomy,
    'To what extent can subscribers customize recommendation algorithms, or are algorithmic rankings opaque and non-negotiable?',
    'Audit of platform preference controls; measurement of user ability to: (1) opt out of algorithmic recommendation, (2) weight recommendation criteria, (3) access and understand ranking logic. Comparison of platforms with high vs low algorithmic transparency.',
    'If high autonomy: suppression should decrease; exit options should shift from trapped to constrained; classification may shift from Snare to Tangled Rope. If low autonomy: suppression validated; trapping mechanism is cognitive (users internalize algorithmic ranking as ''natural'' recommendations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_recommendation_autonomy, empirical, 'Subscriber autonomy over algorithmic recommendation systems').

omega_variable(
    identity_lock_mechanism,
    'Is subscriber lock-in driven by structural switching costs or by identity fusion with platform communities and creator relationships?',
    'Survey and interview data: ask exited users whether departure was driven by technical friction (time, data loss) or identity disruption (loss of community belonging, creator-follower identity). Compare stated reasons across demographics.',
    'If primarily structural: suppression is 0.58 (validated). If primarily identity-based: exit option should be reclassified as ''identity_locked''; suppression perception may be lower but actual immobility higher; classification from subscriber perspective becomes Rope (identity-locked agents at biographical horizon perceive constraints as changeable in principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether subscriber lock-in is structural or identity-based').

omega_variable(
    regulatory_interoperability_feasibility,
    'Can platform interoperability (data portability, algorithm switching, cross-platform messaging) be implemented at technical and business scale without degrading coordination function?',
    'Technical feasibility analysis of ActivityPub protocols, data portability standards; case studies of interoperable platforms (Mastodon, Bluesky); cost analysis for implementing interoperability on incumbent platforms',
    'If feasible: Scaffold perspective is validated — regulatory sunset is real structural mechanism. If infeasible: Scaffold is aspirational theater; constraint may degrade to Piton or stabilize as permanent Tangled Rope. Affects mandatrophy resolution for extractiveness > 0.70.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_interoperability_feasibility, empirical, 'Technical and business feasibility of interoperable platform architecture').

omega_variable(
    creator_revenue_dependency,
    'What proportion of premium creators depend on subscriber platform revenue for >50% of income vs diversified revenue sources?',
    'Survey of platform creators; measurement of income distribution across: platform subscriptions, direct Patreon/Substack, sponsorships, external media. Identify cohort with high platform dependency vs independent creators.',
    'If >60% dependent: creators are co-extractors trapped in platform ecosystem (reclassify creator perspective exit from ''mobile'' to ''constrained''). If <30% dependent: creators are genuinely mobile; their powerful/mobile perspective may shift classification or reduce chi. Affects directionality logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_revenue_dependency, empirical, 'Creator revenue dependency on subscriber platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscriber_attention_commons, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_tr_t0, subscriber_attention_commons, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sac_tr_t3, subscriber_attention_commons, theater_ratio, 3, 0.47).
narrative_ontology:measurement(sac_tr_t6, subscriber_attention_commons, theater_ratio, 6, 0.55).
narrative_ontology:measurement(sac_tr_t9, subscriber_attention_commons, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(sac_be_t0, subscriber_attention_commons, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sac_be_t3, subscriber_attention_commons, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sac_be_t6, subscriber_attention_commons, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(sac_be_t9, subscriber_attention_commons, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscriber_attention_commons, attachment_coordination).
narrative_ontology:boltzmann_floor_override(subscriber_attention_commons, 0.12).
narrative_ontology:affects_constraint(subscriber_attention_commons, algorithmic_ranking_fairness).
narrative_ontology:affects_constraint(subscriber_attention_commons, creator_precarity_labor).
narrative_ontology:affects_constraint(subscriber_attention_commons, data_monetization_asymmetry).

% DUAL FORMULATION NOTE:
% The subscriber attention commons is upstream of multiple structurally distinct constraints: algorithmic ranking fairness (how algorithms distribute visibility), creator precarity labor (how creator revenue dependency creates extraction), and data monetization asymmetry (how behavioral data is captured and monetized). Each downstream constraint has its own epsilon reflecting its specific empirical status and bargaining dynamics. The subscriber attention commons constrains all three through the shared attention and data flow.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(subscriber_attention_commons, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
