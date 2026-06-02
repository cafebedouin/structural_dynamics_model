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
    constraint_indexing:constraint_classification/3,
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
 *   The subscriber attention commons represents a structural constraint where
 *   digital platforms organize subscriber time, behavioral data, and social
 *   attention for monetization through advertising, creator revenue-sharing,
 *   and data licensing, while simultaneously providing genuine coordination
 *   benefits through content discovery, algorithmic recommendation, and
 *   community formation. The constraint exhibits the full range of
 *   Deferential Realism classifications depending on the observer's
 *   structural position: platform architects experience it as pure
 *   coordination (Rope); creators and moderate users experience it as hybrid
 *   coordination-extraction (Tangled Rope); trapped users dependent on
 *   platforms for employment or social connection experience it as pure
 *   extraction (Snare); regulators maintain performative oversight (Piton);
 *   organized alternatives (cooperatives, open-source platforms) experience
 *   it as temporary coordination problem with sunset mechanisms (Scaffold);
 *   and civilizational observers risk naturalizing the contingent
 *   institutional arrangement as an immutable feature of attention scarcity
 *   (Mountain, false summit). The constraint's extractiveness has grown from
 *   0.35 to 0.58 over the interval (2015-2025) as platforms have tightened
 *   algorithmic control, increased data monetization, and developed more
 *   sophisticated behavioral targeting. Suppression has risen from 0.52 to
 *   0.68 as network effects have deepened and regulatory theater has proven
 *   ineffective at reducing lock-in. Theater ratio has increased from 0.42 to
 *   0.55 as regulatory compliance (transparency reports, content moderation
 *   policies) has become more elaborate while actual user consent and control
 *   remain minimal.
 *
 * KEY AGENTS:
 *   - Trapped Subscribers: Powerless/trapped (global/biographical) — users with high platform dependency for employment, social connection, or family contact; bear full extraction cost; zero exit optionality
 *   - Precarious Creators: Moderate/constrained (global/biographical) — creators with material but not total platform income dependency; benefit from distribution and audience reach; face high exit costs but structural exit is possible
 *   - Platform Operators: Institutional/arbitrage (global/immediate) — architects of attention monetization; experience constraint as coordination; maximal exit optionality through arbitrage across jurisdictions and business models
 *   - Regulatory Authorities: Institutional/arbitrage (national/generational) — maintain oversight theater; enforce procedural compliance (policies, transparency reports) without functional constraint on platform behavior
 *   - Creator Cooperatives and Open-Source Alternatives: Organized/constrained (global/generational) — face network-effect barriers but building alternative coordination mechanisms with lower extraction; represent structural exit pathway with residual friction
 *   - Data Portability and Interoperability Mandates: Organized/constrained (national/generational) — regulatory and technical infrastructure (GDPR, DMA, ActivityPub) creating temporary scaffolding with explicit sunset (as federated standards mature, network lock-in erodes)
 *   - Analytical Observer: Analytical/analytical (universal/civilizational) — risks naturalizing platform-proprietary attention monetization as inherent to content discovery at scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscriber_attention_commons, 0.58).
domain_priors:suppression_score(subscriber_attention_commons, 0.68).
domain_priors:theater_ratio(subscriber_attention_commons, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscriber_attention_commons, extractiveness, 0.58).
narrative_ontology:constraint_metric(subscriber_attention_commons, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(subscriber_attention_commons, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscriber_attention_commons, tangled_rope).
narrative_ontology:human_readable(subscriber_attention_commons, "Subscriber Attention Commons Extraction").
narrative_ontology:topic_domain(subscriber_attention_commons, "digital_media/attention_economy").

domain_priors:requires_active_enforcement(subscriber_attention_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscriber_attention_commons, platform_operators).
narrative_ontology:constraint_beneficiary(subscriber_attention_commons, algorithmic_recommendation_infrastructure).
narrative_ontology:constraint_victim(subscriber_attention_commons, attention_commons).
narrative_ontology:constraint_victim(subscriber_attention_commons, precarious_creators).
narrative_ontology:constraint_victim(subscriber_attention_commons, trapped_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED SUBSCRIBER (SNARE) — Users who depend on platform for social connection, income verification, or employment networking face structural lock-in. Exit costs include social isolation, loss of employment leads, and reputational damage. Algorithmic feed reinforces engagement through variable reward schedules (intermittent reinforcement). Suppression is high: legal EULA barriers, network effects, and cognitive capture through algorithmic personalization. Experienced extraction is maximum — users bear full cost of attention monetization with minimal exit capacity.
constraint_indexing:constraint_classification(subscriber_attention_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRECARIOUS CREATOR (TANGLED ROPE) — Creators benefit from platform's content distribution and audience reach (coordination function) while bearing asymmetric extraction through revenue-share mechanisms and algorithmic suppression of reach. Exit is costly but possible: migration to competitor platforms, Patreon, or Substack require rebuilding audience but are structurally feasible. Creators experience both genuine coordination (algorithm surfaces their work to interested audiences) and systematic extraction (platform captures 30-50% of revenue, controls visibility).
constraint_indexing:constraint_classification(subscriber_attention_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience the constraint as pure coordination: the platform solves a genuine problem (matching creators with audience at scale). Network effects and algorithmic curation provide real coordination benefit. The extraction layer (attention monetization, behavioral data licensing) is seen as legitimate value capture for providing the service. Beneficiaries with maximal exit optionality: can arbitrage across jurisdictions, licensing models, and regulatory regimes. Effective extraction runs toward the platform, not away — they are not targets of the mechanism but architects of it.
constraint_indexing:constraint_classification(subscriber_attention_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (PITON) — Maintains oversight and enforcement rituals (content moderation standards, transparency reports, audit procedures) that are largely performative. The regulatory requirement for terms-of-service disclosure is theater: users do not read ToS, regulators cannot enforce at scale, platforms optimize for regulatory theater rather than genuine consent. The oversight mechanism persists through institutional inertia and the appearance of control rather than actual functional constraint on platform behavior. Theater ratio high: compliance is measured by procedural adherence (having policies, publishing reports) rather than behavioral outcomes.
constraint_indexing:constraint_classification(subscriber_attention_commons, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CREATOR COOPERATIVES AND ALTERNATIVES (TANGLED ROPE) — Organized creators (unions, federation networks, open-source platforms like Mastodon, PeerTube) see both genuine coordination problems to solve (audience discovery, content recommendation at scale) and extractive mechanisms they want to displace. These alternatives provide real coordination benefits (algorithmic feeds, content recommendation) while maintaining lower extraction (non-profit models, user ownership, transparent algorithms). Exit is structurally possible but faces network-effect barriers and switching costs for users. The cooperative model itself is tangled rope: genuine coordination of creator interests with residual extraction (community labor, server costs, maintenance burden).
constraint_indexing:constraint_classification(subscriber_attention_commons, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTEROPERABILITY MANDATES (SCAFFOLD) — Regulatory and technical moves toward data portability (GDPR, DMA, ActivityPub federation) create temporary coordination structures with explicit sunset clauses. These mandates reduce lock-in by allowing users to port their social graphs and behavioral history across platforms. As interoperability matures, network effects that trap users weaken. Theater ratio moderate: mandates require genuine technical compliance (API standards, data export formats) with measurable behavioral outcomes (reduced switching costs). Extraction mechanism loses force as users can credibly threaten exit.
constraint_indexing:constraint_classification(subscriber_attention_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the constraint appears as an immutable feature of information ecology: attention is scarce, so some filtering/curation mechanism is necessary, and whoever builds the curation infrastructure will capture value. The observer risks naturalizing what is a contingent institutional arrangement (platform-proprietary algorithms, attention monetization) as inherent to the coordination problem. The engine's false summit detector will identify this as naturalization of mechanisms that are actually contestable and historically contingent.
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
 *   Extractiveness (0.58): Moderate-high. The platform monetizes subscriber attention and behavioral data through advertising revenue (primary), creator revenue-sharing (secondary), and data licensing (tertiary). Base extraction reflects that platforms capture 30-70% of advertising revenue while creators share 50-55%, and users receive minimal direct compensation for attention and data. The metric reflects the asymmetry between value extracted (behavioral data, attention time) and value returned (content access, algorithmic filtering). Suppression (0.68): High. Multiple layers of suppression operate: (1) Legal/contractual — EULA terms that users do not read and cannot negotiate; (2) Technical — algorithm opacity, no algorithmic literacy requirements for exit; (3) Economic — network effects create high switching costs (social graphs, follower counts, habit formation); (4) Cognitive — algorithmic feeds employ variable reward schedules similar to slot-machine mechanics, creating engagement loops that feel voluntary but are engineered. Theater ratio (0.55): Moderate. Regulatory compliance (terms-of-service disclosure, content moderation policies, transparency reports) is substantial but substantially performative — the compliance ritual produces the appearance of user consent and platform accountability without functional changes to extraction mechanisms. The rise from 0.42 to 0.55 reflects increased regulatory theater (more elaborate policies) without functional reduction in lock-in or extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range of classification from perspectives defined by exit optionality and power asymmetry. Platform operators with arbitrage options (can arbitrage across jurisdictions, licensing models, regulatory regimes) experience the constraint as Rope — genuine coordination that produces value. Creators with constrained exit (can migrate to alternatives but at high cost) experience Tangled Rope — real coordination benefits coupled with asymmetric extraction through revenue-sharing and algorithmic suppression. Trapped users (dependent on platform for employment or social connection) experience Snare — pure extraction with minimal coordination benefit and zero exit capacity. Organized alternatives (cooperatives, federation networks) experience Tangled Rope of a different kind — solving the coordination problem (content discovery, recommendation) while maintaining lower extraction, but facing network-effect barriers that make their own model residually extractive (community labor, server costs). Regulators experience Piton — their own oversight mechanisms are degraded (compliance theater, unenforceable rules) yet persist through institutional inertia. The analytical observer risks Mountain — seeing the extraction as inherent to information scarcity — but structural data reveals this as a false summit: platform-proprietary algorithms and attention monetization are contingent choices, not immutable laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural relationship to the constraint — their power level, exit options, and role in the extraction flow. Trapped users (powerless/trapped) derive d ≈ 0.95, placing them at maximum extraction target (f(d) ≈ 1.42). Precarious creators (moderate/constrained) derive d ≈ 0.68, placing them at moderate-high extraction target (f(d) ≈ 1.00), modulated downward by genuine coordination benefits they receive (audience reach, algorithmic discovery). Platform operators (institutional/arbitrage) derive d ≈ 0.15, placing them at extraction beneficiary (f(d) ≈ -0.01), consistent with their maximal exit optionality and net benefit from the mechanism. Regulatory authorities (institutional/arbitrage) derive d ≈ 0.20, reflecting that they maintain the oversight theater but do not directly extract, and maintain maximal optionality to arbitrage across jurisdictions and enforcement strategies. Creator cooperatives (organized/constrained) derive d ≈ 0.55, placing them at moderate extraction target (f(d) ≈ 0.75), reflecting that they both suffer from the platform's network effects but are actively building structural alternatives. Scope modifier σ(global) = 1.2 amplifies all χ values (more global scope = harder verification = more hidden extraction). Effective extraction χ = ε × f(d) × σ(S) scales per the formula: for trapped users, χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (near-maximal experienced extraction); for creators, χ ≈ 0.58 × 1.00 × 1.2 ≈ 0.70 (high experienced extraction); for platforms, χ ≈ 0.58 × (-0.01) × 1.2 ≈ -0.007 (negative experienced extraction — the mechanism subsidizes them).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope (ε = 0.58, χ ranges from -0.01 to 0.99 across perspectives). The initial puzzle — 'Is this coordination or extraction?' — dissolves when perspectives are included: it is both, and the perspectival gap itself is the diagnostic signal. The constraint provides real coordination (content discovery, audience matching, network formation) that platforms and some creators genuinely benefit from. Simultaneously, it extracts real value (behavioral data, attention time, revenue-share asymmetry) that trapped users and precarious creators bear. The mandatrophy is resolved not by choosing between coordination and extraction, but by measuring the magnitude and distribution of each across different observer positions. The false summit perspective (analytical/mountain) is particularly important: the risk is that the coordination function ('platforms solve the hard problem of content discovery at scale') will be used to justify the extraction ('therefore attention monetization is natural and inevitable'). The framework separates these claims: the coordination is genuine; the extraction is contingent. Alternative platforms (cooperatives, federation networks) demonstrate that content discovery can be solved with lower extraction, though not without residual coordination costs (server maintenance, community labor, algorithmic complexity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_recommendation_necessity,
    'Is algorithmic content recommendation an inherent requirement for solving content discovery at scale, or merely the dominant privatized solution?',
    'Comparative analysis of non-algorithmic discovery mechanisms (human curation, community voting, chronological feeds, federated protocols); measurement of discovery effectiveness across models',
    'If inherent: platform extraction is coordinate to coordination (Rope justified). If contingent: extraction is pure rent-seeking (Snare from creator/user view). Classification moves along snare-tangled_rope spectrum depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_recommendation_necessity, empirical, 'Whether algorithmic recommendation is structurally necessary or contingently dominant').

omega_variable(
    attention_monetization_decoupling_feasibility,
    'Can attention-based advertising be structurally decoupled from user attention commons without destroying the coordination function (content discovery)?',
    'Analysis of non-attention-monetized platforms (Wikipedia, Mastodon, Bluesky); measurement of user engagement and discovery efficacy without ad-targeting; A/B testing of non-personalized recommendation feeds',
    'If decoupling feasible: extraction is not necessary for coordination (Tangled Rope confirmed, Scaffold pathway viable). If decoupling degrades coordination: extraction is coupled to coordination benefits (justifies Rope perspective higher than current assessment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_monetization_decoupling_feasibility, empirical, 'Whether attention monetization is necessary for effective content discovery').

omega_variable(
    network_effect_magnitude_and_portability,
    'What magnitude of network effect lock-in persists when users can port their social graphs and followers across platforms via data portability and federation standards?',
    'Longitudinal study of user switching behavior post-Mastodon/Bluesky migrations; measurement of friction in graph portability; analysis of residual lock-in from feed algorithms, community norms, and switching costs',
    'If residual lock-in remains high: trapped and constrained exit options still bind (Snare, Tangled Rope persist). If lock-in erodes: exit options upgrade to mobile (classifications shift toward Rope). Interoperability sunset becomes structural rather than aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_magnitude_and_portability, empirical, 'Magnitude of persistent network lock-in under data portability').

omega_variable(
    creator_dependency_distribution,
    'What proportion of creators have material income dependency on platform distribution (making exit structurally trapped) vs. using platform for supplementary reach (making exit constrained)?',
    'Survey of creator income distribution; analysis of platform-dependency by income quartile; measurement of income loss upon platform switching or suspension',
    'If majority trap-dependent: victim group is powerless (Snare severity increases). If majority supplementary: victim group is moderate (Tangled Rope justified). Affects directionality computation for precarious_creators group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_dependency_distribution, empirical, 'Distribution of creator income dependency on platforms').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.68) primarily structural (legal, technical, economic barriers) or substantially internalized (users accept extraction as ''natural,'' normalize algorithmic ranking, believe alternatives are inferior)?',
    'Post-exit behavior analysis: do users who leave platforms experience suppression persistence? Qualitative interviews about perceived exit barriers vs. actual technical barriers; measurement of regulatory capture in user expectations',
    'If primarily structural: suppression metric is accurate. If substantially internalized: actual suppression is higher (cognitive capture mechanisms); identity_locked exit options are undersourced in current analysis. Affects interpretation of trap vs. constraint for users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscriber_attention_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_tr_t0, subscriber_attention_commons, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sac_tr_t5, subscriber_attention_commons, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sac_tr_t10, subscriber_attention_commons, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(sac_be_t0, subscriber_attention_commons, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sac_be_t5, subscriber_attention_commons, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sac_be_t10, subscriber_attention_commons, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sac_su_t0, subscriber_attention_commons, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(sac_su_t5, subscriber_attention_commons, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(sac_su_t10, subscriber_attention_commons, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscriber_attention_commons, resource_allocation).
narrative_ontology:affects_constraint(subscriber_attention_commons, algorithmic_recommendation_opacity).
narrative_ontology:affects_constraint(subscriber_attention_commons, creator_revenue_asymmetry).
narrative_ontology:affects_constraint(subscriber_attention_commons, network_effect_lock_in).

% DUAL FORMULATION NOTE:
% The subscriber attention commons decomposes into three structurally distinct constraints: (1) algorithmic_recommendation_opacity (ε ≈ 0.35, Mountain/Piton) — the algorithmic filtering mechanism is opaque and unverifiable from user perspective; (2) creator_revenue_asymmetry (ε ≈ 0.52, Tangled Rope) — platform captures 30-70% of advertising revenue while providing genuine distribution; (3) network_effect_lock_in (ε ≈ 0.68, Snare) — social graph entrenchment creates high switching costs. These three constraints are coupled but structurally distinct. The primary attention_commons story models the aggregate effect. Decomposition enables higher-resolution analysis of which structural mechanisms matter most for each actor class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
