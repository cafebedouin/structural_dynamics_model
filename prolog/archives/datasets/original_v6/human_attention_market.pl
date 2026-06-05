% ============================================================================
% CONSTRAINT STORY: human_attention_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_attention_market, []).

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
 *   constraint_id: human_attention_market
 *   human_readable: The Human Attention Market
 *   domain: digital_platforms/cognitive_economics
 *
 * SUMMARY:
 *   The human attention market represents a structural constraint where
 *   digital platforms coordinate audience-advertiser matching while
 *   simultaneously extracting user attention through algorithmic
 *   optimization. The constraint exhibits hybrid characteristics: genuine
 *   coordination function (platforms enable communication and content
 *   discovery that lacks efficient alternatives) coexists with significant
 *   extraction (appropriation of user attention value without equivalent
 *   compensation). The extractiveness trajectory shows acceleration over the
 *   interval as algorithmic sophistication has increased (from behavioral
 *   targeting to predictive engagement models to neuromorphic recommendation
 *   systems). Theater ratio shows equivalent growth, indicating increasing
 *   performative component as engagement metrics increasingly diverge from
 *   actual user preference. The constraint operates across multiple
 *   institutional scales: individual users (powerless/trapped or
 *   identity-locked), small creators (moderate/constrained), platforms
 *   (institutional/arbitrage), user advocacy groups (organized/constrained),
 *   and regulatory bodies (organized/constrained). Classification varies
 *   sharply across perspectives: powerless users perceive snare; moderate
 *   creators perceive tangled rope; beneficiary platforms perceive rope;
 *   organized coalitions perceive tangled rope with regulatory sunset
 *   (scaffold); and the analytical observer risks naturalizing a contingent
 *   arrangement as immutable law (false summit mountain).
 *
 * KEY AGENTS:
 *   - End Users (Powerless, Trapped/Identity-Locked): Primary victims bearing full extraction cost; structurally trapped by network effects; identity-fused users carry suppression even if they technically exit
 *   - Platform Companies (Institutional, Arbitrage): Primary beneficiaries; experience the constraint as coordination mechanism; can arbitrage globally between attention supply and advertising demand
 *   - Small Content Creators (Moderate, Constrained): Mixed position; experience genuine coordination (platform enables audience) and extraction (algorithmic manipulation of reach, revenue sharing); have some agency but constrained by algorithm dependencies
 *   - User Advocacy Groups / Digital Rights Organizations (Organized, Constrained): See both coordination and extraction clearly; organized enough to lobby for regulation but constrained by platform dominance
 *   - Regulatory Bodies (Organized, Constrained): Building alternative attention architecture (DSA, privacy regulations, interoperability mandates) with explicit sunset logic; decentralized platform maturation reduces extraction inherently
 *   - Attention Metrics Ecosystem (Institutional, Arbitrage): Maintains performative engagement metrics (likes, shares, view counts) as proxy for actual attention; real functional measurement (eye-tracking, dwell time, biometric data) underutilized publicly
 *   - Analytical Observer (Analytical, Analytical): Civilizational perspective that risks naturalizing contingent platform architecture as immutable scarcity constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_attention_market, 0.58).
domain_priors:suppression_score(human_attention_market, 0.65).
domain_priors:theater_ratio(human_attention_market, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_attention_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(human_attention_market, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(human_attention_market, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_attention_market, tangled_rope).
narrative_ontology:human_readable(human_attention_market, "The Human Attention Market").
narrative_ontology:topic_domain(human_attention_market, "digital_platforms/cognitive_economics").

domain_priors:requires_active_enforcement(human_attention_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_attention_market, platform_companies).
narrative_ontology:constraint_beneficiary(human_attention_market, advertising_networks).
narrative_ontology:constraint_victim(human_attention_market, end_users).
narrative_ontology:constraint_victim(human_attention_market, democratic_discourse).
narrative_ontology:constraint_victim(human_attention_market, human_cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ORDINARY USER (SNARE) — Structurally trapped: genuine alternatives to attention-harvesting platforms do not exist at scale. Free services extract attention in exchange for minimal coordination benefit. Exit costs are extremely high (loss of social connections, professional network, communication infrastructure). User experiences maximum extraction with minimal real alternatives.
constraint_indexing:constraint_classification(human_attention_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE IDENTITY-FUSED USER (SNARE) — Structurally mobile (could theoretically quit social media) but identity-locked: professional identity (influencers, content creators, journalists), social identity (peer group coordination via platforms), and self-presentation are constituted through platform participation. Exit would require abandoning the identity frame itself, not just the platform. Suppression is internalized — the user carries the constraint with them even if they technically leave.
constraint_indexing:constraint_classification(human_attention_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SMALL CONTENT CREATOR (TANGLED ROPE) — Experiences genuine coordination function: platforms enable audience building, monetization, and distribution that has no equivalent alternative. Also experiences significant extraction: algorithmic manipulation of reach, attention rationing, and revenue-sharing asymmetry. High suppression (algorithmic algorithm changes can destroy earnings overnight) but real agency and real benefits. Mixed extraction and coordination.
constraint_indexing:constraint_classification(human_attention_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PLATFORM COMPANY (ROPE) — Sees the attention market as pure coordination: solving the collective action problem of matching advertising buyers with audiences. Benefits from network effects (more users increase value for advertisers). Experiences minimal extraction (can arbitrage between advertising markets globally). From this perspective, the constraint is a Rope enabling voluntary participation.
constraint_indexing:constraint_classification(human_attention_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ORGANIZED USER COALITION (TANGLED ROPE) — Digital rights organizations, labor movements, and user advocacy groups perceive the constraint as hybrid: genuine need for platform coordination (users benefit from connecting) but unjust extraction (platform appropriates surplus attention value without fair compensation). Organized agency provides some exit capacity (collective bargaining, regulation advocacy) but constrained by platform dominance. This perspective sees both the coordination function and the extraction mechanism clearly.
constraint_indexing:constraint_classification(human_attention_market, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE REGULATORY FRAMEWORK (SCAFFOLD) — EU Digital Services Act, online privacy regulations, and data protection laws are building alternative attention architecture. These frameworks temporarily constrain platform extraction (consent requirements, algorithmic transparency, interoperability mandates) with explicit sunset logic: the regulatory scaffolding is designed to transition toward decentralized/federated platforms that reduce extraction inherently. Suppression declining as technical alternatives mature.
constraint_indexing:constraint_classification(human_attention_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ATTENTION METRICS THEATER (PITON) — Traditional attention measurement (likes, shares, view counts, engagement metrics) is substantially performative: metrics incentivize gaming (clickbait, algorithmic manipulation, sensationalism) rather than genuine user interest capture. The theater persists through institutional inertia despite widespread recognition of metric distortion. Platforms maintain the ritual while acknowledging internally that direct attention harvesting (biometric data, dwell time, eye-tracking) is more functionally useful. Theater ratio ≥ 0.70.
constraint_indexing:constraint_classification(human_attention_market, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, attention scarcity is a fundamental property of human cognition: there are 24 hours in a day, and the number of claims on attention always exceeds available supply. Some market for attention allocation is therefore inevitable. This perspective risks naturalizing what is actually a contingent institutional arrangement (corporate-owned platforms with algorithmic priority) as an immutable law. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(human_attention_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_attention_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_attention_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_attention_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_attention_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(human_attention_market, TR),
    TR >= 0.70.

:- end_tests(human_attention_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Platforms capture significant value from user attention without equivalent compensation. However, the extraction is not maximal (0.72+) because genuine coordination function exists: users do benefit from connecting, discovering content, and building audiences. The value extraction includes: (1) direct advertising revenue (platform captures ~$0.20-0.40 per user per day globally), (2) behavioral data (sold to third-party advertisers and data brokers), (3) intellectual property (user-generated content constitutes valuable training data for AI systems without compensation). Suppression (0.65): Significant but not total. Exit barriers include: network effects (all friends/followers on platform), switching costs (profile/archive loss), professional necessity (social media required for marketing), and psychological dependency (intermittent reinforcement via engagement metrics). However, some exit capacity exists (people do successfully leave social media; alternative platforms exist at smaller scale). Theater ratio (0.68): High and increasing. Traditional attention metrics (likes, shares, comments, view counts) are substantially performative: they incentivize engagement-maximizing behavior (sensationalism, outrage, clickbait) rather than content quality or user interest. Platforms and users optimize for metric scores rather than actual preference satisfaction. Real functional measurement (biometric attention, eye-tracking, dwell time, neuromorphic assessment of genuine interest) is collected but not publicly disclosed. The theater reflects the gap between what platforms claim to measure and what actually drives engagement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The platform company's Rope perspective claims pure coordination (connecting audiences with advertisers). The user's Snare perspective claims pure extraction (attention harvesting without compensation). The creator's Tangled Rope perspective recognizes both functions simultaneously. The organized coalition's Tangled Rope perspective includes regulatory remediation. The scaffold perspective claims the extraction is temporary and being sunset by regulatory architecture. The piton perspective claims the entire attention metrics theater is degraded ritual. The mountain perspective risks claiming attention scarcity is immutable, which the engine will flag as false summit. No single perspective is incorrect — each captures a genuine structural feature. The gap between them reveals that the constraint's classification depends entirely on the observer's structural position and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural position relative to the attention extraction flow. Beneficiary platform companies (d ≈ 0.05): arbitrage exit options mean they can move value across markets globally; they are net beneficiaries. Powerless trapped users (d ≈ 0.95): network effects eliminate exit options; they bear maximum extraction. Identity-locked users (d ≈ 0.89): structurally mobile but cognitively captured; they experience high extraction despite having technical ability to leave. Small creators (d ≈ 0.55): symmetric position — genuine benefits from platform (audience building, monetization) balanced against significant extraction (algorithmic manipulation, revenue sharing asymmetry). Organized user coalitions (d ≈ 0.60): they are targeted by extraction but have enough organized power to negotiate, creating moderate directionality. The derived f(d) values feed into chi calculation: platform companies experience negative effective extraction (they benefit); trapped users experience maximum effective extraction (1.4+ in the legacy scale); moderate creators and organized groups experience medium extraction (0.75-1.0 in legacy scale). Scope modifier σ(S) at global scale (σ=1.2) amplifies extraction calculation — the attention market operates at global scale, making verification and alternative coordination harder, which increases effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION PARADOX RESOLVED: This constraint demonstrates why mandatrophy analysis is necessary for attention markets. A naive reading would classify the constraint as Snare (high extraction, high suppression, low beneficiary coordination). However, this misses the genuine coordination function: platforms do solve a real collective action problem (how to connect billions of people and match advertiser attention to relevant audiences at scale). Alternative mechanisms for this coordination do not exist at equivalent efficiency or scale. Simultaneously, the constraint exhibits significant extraction (users are not compensated for attention value; behavioral data is commodified; intellectual property is appropriated). The tangled rope classification preserves both functions: genuine coordination (users need platforms, platforms solve real problems) AND unjust extraction (users bear costs without equivalent benefit). The mandatrophy is resolved by recognizing that the constraint can be and should be reformed (regulatory frameworks like DSA are building alternatives) without eliminating the coordination function entirely. The scaffold perspective articulates this explicitly: temporary regulatory constraints guide the transition toward decentralized platforms that retain coordination function while reducing extraction inherently. The classification moves from pure snare (implying elimination) to tangled rope + sunset (implying reformation). The false summit mountain perspective is revealed as naturalization: the 'immutability' of platform dominance is not a law of nature but a contingent institutional arrangement that regulatory and technical change can alter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_reversibility,
    'For identity-locked users, is the cognitive capture permanent or would identity reframing (prompted by social cue or deliberate therapy) enable exit?',
    'Longitudinal tracking of users who abandon social media after identity-reframing intervention (e.g., sabbatical programs, cognitive behavioral interventions); measurement of relapse rates and psychological adjustment post-exit',
    'If reversible: identity lock is a cognitively captured perspective rather than structural, and users bear some responsibility for exit. If permanent: identity lock represents a manufactured psychological dependency that persists independently of rational decision-making, implicating platform design in durable harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock enables users to exit after reframing intervention').

omega_variable(
    suppression_mechanism_locus,
    'Is user suppression (inability to exit) primarily structural (real technical/social alternatives absent) or internalized (belief in absence despite structural availability)?',
    'Comparative analysis of exit attempts by demographically similar users with varying suppression beliefs; measurement of actual exit costs vs. perceived exit costs; tracking of users who successfully defect to alternative platforms',
    'If structural: platform dominance is a genuine coordination monopoly requiring antitrust intervention. If internalized: user perception is misaligned with structural reality, and suppression is partly a belief problem addressable through information and credible alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Whether suppression is structural or internalized belief').

omega_variable(
    algorithmic_coordination_function,
    'Does algorithmic ranking genuinely solve a coordination problem (connecting users with content they want) or is it purely extractive (maximizing engagement time independent of user preference)?',
    'A/B testing with alternative ranking functions: user-preference-aligned algorithms vs. engagement-maximizing algorithms; measurement of user satisfaction, content diversity, and attention time under each; cross-platform comparison with open-source algorithms',
    'If genuinely coordinating: platforms are tangled ropes with legitimate coordination function embedded. If purely extractive: algorithmic ranking is Snare disguised as Rope via performative legitimation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_coordination_function, empirical, 'Whether algorithmic ranking solves coordination or purely extracts').

omega_variable(
    decentralized_platform_viability,
    'Can federated/decentralized platforms (Mastodon, ATProtocol-based systems, community-governed instances) achieve the scale and network-effect coordination of corporate platforms without equivalent extraction?',
    'Longitudinal economic analysis of decentralized platform operations; comparison of per-user infrastructure costs, moderation labor, revenue models, and sustainability across decentralized vs. centralized platforms; user migration patterns to alternative platforms',
    'If viable: scaffold perspective is structurally real — the extraction is a contingent choice, not an inherent feature. If unviable: the extraction may be an unavoidable coordination cost, and the constraint''s classification shifts toward legitimate Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_platform_viability, empirical, 'Whether decentralized platforms can achieve equivalent coordination without equivalent extraction').

omega_variable(
    cognitive_autonomy_quantification,
    'How much of user attention time is captured through algorithmic manipulation (dark patterns, intermittent reinforcement, social proof) vs. genuine user preference?',
    'Neuroscientific measurement of reward-center activation during platform use; comparison of self-reported attention intention vs. actual attention allocation; analysis of deleted-search recovery rates (users searching for content they intended to avoid)',
    'If manipulation dominates (>70% of attention time): the constraint is more extractive and suppressive than disclosed metrics suggest. If preference dominates: extraction may be overstated, and users have more agency than identity-lock perspective suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_autonomy_quantification, empirical, 'Degree of user attention captured through algorithmic manipulation vs. genuine preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_attention_market, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, human_attention_market, theater_ratio, 0, 0.45).
narrative_ontology:measurement(attn_tr_t5, human_attention_market, theater_ratio, 5, 0.58).
narrative_ontology:measurement(attn_tr_t10, human_attention_market, theater_ratio, 10, 0.68).
narrative_ontology:measurement(attn_tr_t15, human_attention_market, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, human_attention_market, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(attn_be_t5, human_attention_market, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(attn_be_t10, human_attention_market, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(attn_be_t15, human_attention_market, base_extractiveness, 15, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_attention_market, resource_allocation).
narrative_ontology:affects_constraint(human_attention_market, algorithmic_content_moderation).
narrative_ontology:affects_constraint(human_attention_market, social_media_regulatory_capture).
narrative_ontology:affects_constraint(human_attention_market, digital_labor_exploitation).

% DUAL FORMULATION NOTE:
% The attention market decomposes into multiple structurally distinct constraints: (1) attention_market_user_extraction (ε=0.58, Tangled Rope) — extraction of user attention value without compensation, (2) algorithmic_content_moderation (ε=0.62, Snare) — platforms enforce content policy with minimal user recourse, (3) social_media_regulatory_capture (ε=0.51, Tangled Rope) — platforms lobby against their regulation while claiming coordination functions, (4) digital_labor_exploitation (ε=0.64, Snare) — uncompensated content creator labor subsidizes platform value. Each story has distinct base extraction; they are linked via network affects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_attention_market, moderate, 0.55).
constraint_indexing:directionality_override(human_attention_market, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
