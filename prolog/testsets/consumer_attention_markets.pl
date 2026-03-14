% ============================================================================
% CONSTRAINT STORY: consumer_attention_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_attention_markets, []).

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
 *   constraint_id: consumer_attention_markets
 *   human_readable: Consumer Attention Capture and Behavioral Extraction
 *   domain: digital_markets/behavioral_economics/technology
 *
 * SUMMARY:
 *   Consumer attention markets represent the commodification and industrial
 *   extraction of human cognitive focus through digital platforms. The
 *   structural constraint operates through a combination of network effects,
 *   behavioral design, algorithmic amplification, and asymmetric information
 *   about data monetization. Platforms coordinate genuine utility (social
 *   connection, information discovery, community formation) while
 *   simultaneously extracting behavioral surplus (time, attention, cognitive
 *   patterns) that is monetized to advertisers and data brokers. The
 *   constraint exhibits high suppression because users face significant
 *   barriers to exit: social network lock-in, information gatekeeping, habit
 *   formation through behavioral design, and the generational lock-in of
 *   digital identity formation. The theater ratio reflects that consumer
 *   protection apparatus (privacy regulations, media literacy, consent
 *   mechanisms) has become increasingly performative — users are informed
 *   about data collection and behavioral targeting but engagement continues
 *   to accelerate, suggesting that awareness interventions are insufficient
 *   to counteract extraction mechanisms. The trajectory of both
 *   extractiveness and theater ratio over the 15-year interval shows
 *   monotonic increase: extraction mechanisms have become more sophisticated,
 *   behavioral design more refined, and identity-locking more complete, while
 *   the regulatory and awareness response has shifted toward purely symbolic
 *   protection.
 *
 * KEY AGENTS:
 *   - Attention-Captured Consumers: Primary victim (powerless/trapped) — bear full cost of attention extraction with minimal exit options; subject to behavioral design mechanisms
 *   - Network-Dependent Users: Secondary victim (moderate/constrained) — face high-cost exit due to social coordination lock-in; structurally dependent on platform presence
 *   - Platform-Native Digital Residents: Identity-locked victim (powerless/identity_locked at generational timescale) — identity formed within platform ecosystems; exit threatens self-concept
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture behavioral data surplus and advertising revenue; benefit from network effects and coordination
 *   - Advertisers: Secondary beneficiary (powerful/mobile) — receive efficient audience targeting and conversion optimization; experience platform as coordination mechanism
 *   - Behavioral Data Extractors: Tertiary beneficiary (institutional/arbitrage) — specialize in behavioral analytics, prediction, and micro-targeting; derive value from platform data access
 *   - Regulation/Awareness Apparatus: Institutional actor (organized/constrained) — traditional regulatory and educational responses experiencing degradation into performative theater
 *   - Alternative Platform Coalition: Organized agents building exit pathways (organized/mobile) — federation protocols, open-source platforms, and attention-respecting alternatives with lower extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_attention_markets, 0.68).
domain_priors:suppression_score(consumer_attention_markets, 0.72).
domain_priors:theater_ratio(consumer_attention_markets, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_attention_markets, extractiveness, 0.68).
narrative_ontology:constraint_metric(consumer_attention_markets, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(consumer_attention_markets, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_attention_markets, snare).
narrative_ontology:human_readable(consumer_attention_markets, "Consumer Attention Capture and Behavioral Extraction").
narrative_ontology:topic_domain(consumer_attention_markets, "digital_markets/behavioral_economics/technology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_attention_markets, attention_platform_operators).
narrative_ontology:constraint_beneficiary(consumer_attention_markets, behavioral_data_extractors).
narrative_ontology:constraint_beneficiary(consumer_attention_markets, attention_brokers).
narrative_ontology:constraint_victim(consumer_attention_markets, consumer_agency).
narrative_ontology:constraint_victim(consumer_attention_markets, consumer_time_autonomy).
narrative_ontology:constraint_victim(consumer_attention_markets, consumer_cognitive_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATTENTION-CAPTURED CONSUMER (SNARE) — Consumers are structurally trapped in attention markets. Exit options appear cosmetic: smartphone addiction, algorithmic recommendation loops, and infinite scroll mechanics create compulsive usage patterns. While consumers could theoretically delete accounts or reduce usage, the suppression is severe: social coordination (friends/family on platforms), information gatekeeping (news/communication now platform-mediated), and habit formation make exit practically impossible. The consumer bears the full cost of attention extraction without meaningful compensation or negotiation.
constraint_indexing:constraint_classification(consumer_attention_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE NETWORK-DEPENDENT USER (SNARE) — Users with social or professional network dependence face constrained but high-cost exit. Leaving a platform means abandoning communication channels, professional presence, or social coordination. The constraint is extractive even for moderate-power agents: they pay in time and attention while platforms monetize behavioral data. The suppression comes from network effects — the platform's value is precisely the concentration of their contacts, creating dependency that escalates extraction.
constraint_indexing:constraint_classification(consumer_attention_markets, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ATTENTION PLATFORM OPERATOR (TANGLED ROPE) — Platforms genuinely coordinate user interaction and social connection (coordination function: enabling communication, community, information discovery). Simultaneously, they extract user attention and behavioral data at scale (extraction function: time monetization, ad targeting, behavioral prediction). This is not pure extraction — the coordination function is real and valuable to users. But the extraction is asymmetric and systematic: users receive utility, but platforms receive both the utility gains AND the behavioral surplus. The operator benefits from both coordination and asymmetric extraction simultaneously.
constraint_indexing:constraint_classification(consumer_attention_markets, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ADVERTISER (ROPE) — Advertisers experience the attention market as a coordination mechanism: they gain efficient access to targeted audiences; platforms efficiently connect advertisers with consumers. This is primarily a coordination problem (supply of ad inventory, demand for ad reach, price discovery). While advertisers pay for access, they receive genuine value (customer acquisition, brand awareness). The extraction is symmetric — both parties benefit from the match. Advertisers have significant exit options (alternative channels, email, search, other platforms).
constraint_indexing:constraint_classification(consumer_attention_markets, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM-NATIVE GENERATION (SNARE / IDENTITY_LOCKED) — Digital natives whose identity formation occurs within platform ecosystems experience structurally deeper entrapment. Their identity is constituted through platform presence, social validation metrics (likes, followers), and mediated peer relationships. Exit is not merely constrained (high cost) but identity-threatening (would require becoming a different person). This cohort faces biographical-generational time scales where the platform's extraction mechanisms operate on identity formation itself, not just time allocation. The suppression is internalized: the victim carries the platform's extraction metrics (follower counts, engagement metrics) as self-concept.
constraint_indexing:constraint_classification(consumer_attention_markets, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM REGULATION APPARATUS (PITON) — Traditional media literacy campaigns, privacy regulations (GDPR, CCPA), and attention-awareness interventions are substantially performative. Users are informed about data collection but continue heavy usage. Regulations impose compliance theater (privacy policies, consent mechanisms) without structural change to extraction mechanisms. The regulatory apparatus sees its own approach as degraded — awareness interventions have not reduced engagement, consent mechanisms are pro-forma, and behavioral design continues unabated. The theater_ratio is high because the regulatory response maintains the appearance of consumer protection while extraction persists.
constraint_indexing:constraint_classification(consumer_attention_markets, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ALTERNATIVE ATTENTION ARCHITECTURE (SCAFFOLD) — Emerging alternatives (federation protocols like ActivityPub, open-source social platforms like Mastodon/Bluesky, attention-economy alternatives like Substack/newsletters, time-bounded platform access policies) represent organized exit pathways with sunset logic. These alternatives are building parallel attention infrastructure with lower extraction mechanisms (subscription-based or public-good funding rather than behavioral data monetization). This perspective sees the current attention market as a temporary coordination failure being solved by decentralized architecture and different business models. The scaffold is structural — alternatives are materially available and growing, not merely aspirational.
constraint_indexing:constraint_classification(consumer_attention_markets, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE AT CIVILIZATIONAL SCALE) — From a civilizational perspective, consumer attention markets create negative externalities and cognitive tragedy-of-the-commons structures. Individual rational decisions to use platforms accumulate into collective attention depletion and epistemic pollution. The analytical observer sees this as a structural feature of attention economies: attention is zero-sum (finite human cognitive hours), platforms optimize for capture, and the individual rational choice (use the platform) compounds into civilizational irrationality (cognitive commons degradation). However, some analytical positions risk naturalizing contingent business model choices as inherent features of digital communication.
constraint_indexing:constraint_classification(consumer_attention_markets, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_attention_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_attention_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_attention_markets, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_attention_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_attention_markets, TR),
    TR >= 0.70.

:- end_tests(consumer_attention_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platform operators systematically convert user time and behavioral data into advertising revenue and data products. The extraction is not absolute (users do receive coordination utility in terms of social connection and information access) but is substantial and asymmetric — users receive utility but platforms capture the marginal surplus. The trajectory from 0.35 to 0.68 reflects the industry evolution from early web coordination (Friendster, early Facebook) toward sophisticated behavioral extraction (algorithmic feeds, micro-targeting, engagement optimization). Suppression (0.72): Very high. Users face multiple layers of suppression: (1) Network effects create switching costs and information gatekeeping; (2) Behavioral design (infinite scroll, notification algorithms, variable reward schedules) generates compulsive usage despite conscious user desire to reduce time; (3) Social coordination (peers/family/professional contacts concentrated on platforms) makes exit loss aversive; (4) Generational identity lock for digital natives makes exit identity-threatening. The suppression is partially structural (network effects) and partially internalized (behavioral design mechanisms create compulsive patterns; platform metrics internalized as self-worth). Theater ratio (0.65): Substantial. Consumer protection apparatus (GDPR privacy policies, media literacy campaigns, attention-awareness interventions, consent mechanisms) is increasingly performative. Users are informed about behavioral targeting and data collection but continue heavy engagement. Regulations impose compliance costs on platforms (privacy policy generation, consent flows) without substantially changing extraction mechanisms. The theatrical quality increases over time as regulation becomes more detailed and visible while extraction continues unabated.
 *
 * PERSPECTIVAL GAP:
 *   The most acute gap is between the platform operator (Tangled Rope) and the trapped consumer (Snare). The operator sees coordination — providing social connection, information discovery, community formation. The consumer sees extraction — their attention is systematically captured through behavioral design and monetized without compensation. Both descriptions are structurally accurate: the coordination IS real, AND the extraction IS real. The perspectival gap reveals that Tangled Rope is the honest description — the platform genuinely coordinates AND extractively captures surplus. The secondary gap is between the platform operator and the alternative coalition (Scaffold). The operator experiences the current system as stable and beneficial. The alternative coalition experiences it as temporary and degraded, seeing the sunset approaching as federation and decentralized architecture mature. The platform-native generation's identity-locked perspective reveals a new form of suppression: their cognitive lock is not just behavioral (habit, compulsion) but constitutive (identity formed within the system). Exit for this cohort is not merely high-cost but identity-threatening, operating at deeper psychological layers than conscious preference or rational calculation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship between each agent and the extraction flow. Consumers are victims with trapped or identity_locked exit options: derived d = 0.92–0.95, producing f(d) ≈ 1.35–1.42. They experience maximum effective extraction. Platform operators are beneficiaries with arbitrage exit options: derived d = 0.05–0.15, producing f(d) ≈ -0.12 to -0.01. They experience negative effective extraction (extraction flows toward them). Advertisers are beneficiaries with mobile exit options: derived d = 0.20–0.35, producing f(d) ≈ 0.02–0.40. They experience low or slightly positive effective extraction; the constraint is primarily coordination (many alternative channels available). The platform regulation apparatus at institutional level with constrained options: derived d = 0.55–0.70 depending on capture assumptions. No directionality override needed; the derivation chain produces accurate d values from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as high-extraction Snare (ε=0.68, χ>0.66) with genuine coordination function (platform operators experience Tangled Rope). This appears to violate the mandatrophy prohibition: cannot label as Snare while acknowledging real coordination. RESOLUTION: The mandatrophy is resolved by perspectival indexing. From the CONSUMER perspective (powerless/trapped), the constraint classifies as Snare — extraction is asymmetric, suppression is severe, and the coordination utility does not justify the extraction burden from their position. From the PLATFORM OPERATOR perspective (institutional/arbitrage), the constraint classifies as Tangled Rope — there is genuine coordination (enabling communication, information discovery) coupled with asymmetric extraction. Both classifications are correct for their respective indices. The mandatrophy violation is prevented by refusing to assign a single type to the constraint independent of the observer's position. The constraint IS a snare for trapped consumers and IS a tangled rope for operators. The resolution mechanism is the presheaf structure: the constraint does not have a single type, it has a indexed family of types over the observation site. The 'true' classification is the family, not any single member.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_design_vs_user_preference,
    'How much of user engagement is genuine preference for platform content versus outcome of behavioral design (infinite scroll, notification manipulation, algorithmic amplification)?',
    'Experimental comparison: user engagement with and without behavioral design features; longitudinal tracking of engagement change following design intervention removal; cross-platform comparison of engagement with similar content but different design',
    'If design-driven (>60%): suppression is primarily mechanistic and could be reversible through architectural change. If preference-driven (>60%): extraction is lower than classified because users are willing participants receiving genuine utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_design_vs_user_preference, empirical, 'Attribution of engagement to behavioral design versus user preference').

omega_variable(
    data_monetization_asymmetry_magnitude,
    'What is the actual magnitude of value extraction from consumer behavioral data relative to the coordination utility users receive?',
    'Comparative analysis: lifetime value per user (behavioral data monetization) versus cost of platform infrastructure and service provision; behavioral economics valuation of time/attention lost to engagement mechanisms',
    'If extraction significantly exceeds coordination value: strengthens Snare classification. If extraction roughly equals coordination value: constraint shifts toward Tangled Rope or degrades toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_monetization_asymmetry_magnitude, empirical, 'Magnitude of data monetization relative to platform utility').

omega_variable(
    alternative_platform_adoption_ceiling,
    'What is the maximum adoption rate for alternative platforms (federation, open-source, attention-respecting models) given network effects and coordination lock-in?',
    'Historical analysis of platform transitions (MySpace to Facebook, proprietary social to open protocols); diffusion modeling with network effect constraints; empirical tracking of Mastodon/Bluesky adoption curves and retention rates',
    'If ceiling is high (>40% market share possible): Scaffold perspective is structural, sunset is realistic. If ceiling is low (<10%): alternative platforms are aspirational, trap remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_adoption_ceiling, empirical, 'Adoption ceiling for alternative platforms given network effects').

omega_variable(
    identity_fusion_reversibility,
    'For platform-native digital residents, how reversible is identity fusion with platform presence? What is the psychological cost of platform exit?',
    'Longitudinal psychological assessment of platform migrants; measurement of identity re-integration post-exit; comparative analysis of identity stability in high-platform vs low-platform communities',
    'If identity fusion is largely reversible: exit_options upgrade from identity_locked to constrained for generational cohort. If irreversible: suppression is deeper than estimated and manifests as internalized control mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_reversibility, empirical, 'Reversibility of identity fusion with platform presence').

omega_variable(
    regulation_mechanism_effectiveness,
    'Do attention-limiting regulations (screen time warnings, algorithm transparency, consent requirements) actually reduce extraction or merely create compliance theater?',
    'Comparison of engagement metrics pre/post regulation in same jurisdiction; analysis of repeat-user behavior after exposure to warnings; cross-jurisdictional comparison of regulation stringency versus actual engagement change',
    'If effective (>20% engagement reduction): regulatory apparatus ceases to be Piton; constraint shifts toward Scaffold. If theater (no significant change): Piton classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulation_mechanism_effectiveness, empirical, 'Effectiveness of attention-limiting regulations').

omega_variable(
    coordination_necessity_versus_extraction_coupling,
    'Is the coordination function genuinely coupled to the extraction mechanism, or could the same coordination utility be provided with lower extraction (e.g., decentralized architecture, different monetization)?',
    'Architectural analysis of platform functionality; identification of design choices that create extraction without coordination necessity; proof-of-concept implementations (ActivityPub federated platforms) showing coordination with lower extraction',
    'If coupled: Tangled Rope classification is correct — extraction is necessary cost of coordination. If decoupled: extraction is contingent design choice, constraint should reclassify toward Scaffold or degrade toward Rope as alternatives mature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_versus_extraction_coupling, conceptual, 'Whether extraction is necessarily coupled to coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_attention_markets, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, consumer_attention_markets, theater_ratio, 0, 0.35).
narrative_ontology:measurement(attn_tr_t5, consumer_attention_markets, theater_ratio, 5, 0.5).
narrative_ontology:measurement(attn_tr_t10, consumer_attention_markets, theater_ratio, 10, 0.65).
narrative_ontology:measurement(attn_tr_t15, consumer_attention_markets, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, consumer_attention_markets, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, consumer_attention_markets, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(attn_be_t10, consumer_attention_markets, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(attn_be_t15, consumer_attention_markets, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_attention_markets, resource_allocation).
narrative_ontology:affects_constraint(consumer_attention_markets, behavioral_targeting_asymmetry).
narrative_ontology:affects_constraint(consumer_attention_markets, network_effect_lock_in).
narrative_ontology:affects_constraint(consumer_attention_markets, epistemic_pollution_attention_economy).

% DUAL FORMULATION NOTE:
% Consumer attention markets decompose into three structurally distinct constraints: (1) resource_allocation coordination (matching ad inventory supply with audience demand) has low extraction; (2) behavioral targeting asymmetry (conversion of user patterns into prediction models) has high extraction; (3) network effect lock-in (social coordination concentrated on single platforms) has high suppression. The unified story captures the constraint family coupling where all three mechanisms operate simultaneously from platform architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
