% ============================================================================
% CONSTRAINT STORY: attention_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_monopoly, []).

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
 *   constraint_id: attention_monopoly
 *   human_readable: Attention Monopoly in Digital Information Ecosystems
 *   domain: digital_platforms/cognitive_economics
 *
 * SUMMARY:
 *   Attention monopoly describes the structural capture of user cognitive
 *   capacity by a small number of digital platforms through algorithmic
 *   curation, behavioral design, and network effects. This constraint
 *   operates at the intersection of cognitive economics, regulatory policy,
 *   and platform architecture. The monopoly exhibits a perspectival gap:
 *   platform operators experience a pure coordination problem (matching
 *   creators to audiences); users experience extraction (behavioral capture
 *   with minimal exit options); content creators experience a tangled mix
 *   (platforms provide visibility but extract through algorithmic suppression
 *   and data asymmetry); regulators and decentralization movements see a
 *   temporary problem with architectural solutions (interoperability
 *   standards, transparency mandates, federated alternatives). The
 *   constraint's extractiveness has increased from 0.35 to 0.58 over 15 years
 *   as platforms have optimized attention-capture algorithms and behavioral
 *   design. Theater ratio has similarly increased from 0.42 to 0.68 as the
 *   performative aspects of algorithmic curation have expanded (trending
 *   sections, recommended feeds, algorithmic amplification metrics) relative
 *   to user-driven discovery.
 *
 * KEY AGENTS:
 *   - Platform Users (Attentional Precariat): Primary victim (powerless/trapped) — structurally dependent on platforms with high switching costs, behavioral addiction, and mandatory participation for social/economic access
 *   - Content Creators and Communities: Secondary victim (moderate/constrained) — benefit from platform reach but face algorithmic suppression, attention tax, and data asymmetry
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture advertising revenue, user data, and market power through attention extraction; experience the system as solving a coordination problem
 *   - Attention Extractors (Advertisers, Attention Farmers): Secondary beneficiary (institutional/arbitrage) — benefit from concentrated access to user attention at scale
 *   - Regulatory Bodies and Open-Source Movements: Organized actors (organized/constrained) — building alternative pathways (interoperability mandates, federated protocols, decentralized standards) with sunset logic
 *   - Legacy Media Institutions: Piton agents (institutional/arbitrage) — traditional attention coordination mechanisms (broadcast, editorial gatekeeping) persist through regulatory frameworks and advertising conventions despite functional obsolescence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent monopoly as an inevitable law of attention economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_monopoly, 0.58).
domain_priors:suppression_score(attention_monopoly, 0.62).
domain_priors:theater_ratio(attention_monopoly, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_monopoly, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attention_monopoly, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_monopoly, tangled_rope).
narrative_ontology:human_readable(attention_monopoly, "Attention Monopoly in Digital Information Ecosystems").
narrative_ontology:topic_domain(attention_monopoly, "digital_platforms/cognitive_economics").

domain_priors:requires_active_enforcement(attention_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_monopoly, attention_extractors).
narrative_ontology:constraint_beneficiary(attention_monopoly, algorithmic_intermediaries).
narrative_ontology:constraint_victim(attention_monopoly, attention_payers).
narrative_ontology:constraint_victim(attention_monopoly, alternative_content_creators).
narrative_ontology:constraint_victim(attention_monopoly, information_ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTIONAL PRECARIAT (SNARE) — Users structurally trapped in attention extraction ecosystems. Barriers to exit: switching costs (social graph, habit, FOMO), behavioral addiction by design, mandatory platform use for employment/community participation, and lack of viable alternatives. No meaningful exit option. Bears full cost of attention extraction through time loss, cognitive manipulation, and behavioral capture. Maximum experienced extraction.
constraint_indexing:constraint_classification(attention_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATORS & COMMUNITIES (TANGLED ROPE) — Face genuine coordination problem: distributed creators need visibility and audience. Platforms provide real coordination function (matching creators to audiences). But platforms also extract through algorithmic suppression, attention tax (creator needs to produce constant novelty), and asymmetric data collection. Constrained by economic dependence on platform reach. Both coordination benefit and asymmetric extraction are structural.
constraint_indexing:constraint_classification(attention_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience the constraint as pure coordination: managing user-generated content at scale, matching supply (creators) and demand (audiences), moderating information flow. All extraction flows toward this institutional actor. High arbitrage optionality: can relocate infrastructure, rebrand, or pivot business models. Perceives the constraint as solving a coordination problem they benefit from.
constraint_indexing:constraint_classification(attention_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY & DECENTRALIZATION MOVEMENTS (SCAFFOLD) — Organized agents (EU regulators, open-source alternatives, decentralized protocols) see attention monopoly as a temporary coordination failure with a sunset clause. Digital Services Act, DMA, interoperability mandates, and federated social protocols (ActivityPub, Mastodon, Bluesky) represent alternative pathways with designed sunset: as regulation increases transparency and interoperability costs, and as decentralized alternatives mature, the monopoly's extraction mechanism loses force. Suppression declining as regulatory frameworks establish.
constraint_indexing:constraint_classification(attention_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY ATTENTION MODELS (PITON) — Traditional broadcast media (TV, radio, print) once coordinated mass attention through scarcity. That coordination function has atrophied as digital platforms captured the market. But the legacy model persists through regulatory frameworks, advertising conventions, and institutional inertia. High theater: advertising metrics, Nielsen ratings, and editorial gatekeeping are largely performative in the digital era, maintained because regulatory alternatives haven't fully replaced them. Theater increased as platforms rendered legacy models obsolete.
constraint_indexing:constraint_classification(attention_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attention is scarce and concentration may be inevitable: human attention has hard limits (24 hours/day, ~4-6 hours discretionary), and network effects naturally concentrate platforms. This perspective sees the monopoly as an immutable consequence of attention economics. However, the structural data contradicts the mountain classification — the engine will detect a false summit, revealing that attention concentration is a contingent result of policy choices (algorithm design, data collection, regulatory capture) rather than a law of nature.
constraint_indexing:constraint_classification(attention_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_monopoly, TR),
    TR >= 0.70.

:- end_tests(attention_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms capture significant user time, behavioral control, and data value without proportional compensation or autonomy. The increase from 0.35 to 0.58 reflects algorithmic optimization for engagement (attention capture) as the primary revenue driver. However, extractiveness is not maximum (0.70+) because platforms do provide genuine value (connection, discovery, content access) that users voluntarily engage with — the constraint is not purely predatory. Suppression (0.62): High. Significant barriers to exit include switching costs (social graph, accumulated data, habit formation), behavioral addiction by design (notification systems, variable reward schedules, infinite scroll), mandatory platform participation for employment and community (network externality), and lack of viable alternatives. But suppression is not total — some users do migrate, and decentralized alternatives exist at smaller scale. Theater ratio (0.68): High and increasing. Algorithmic curation presents itself as neutral discovery mechanism but is substantially performative — ranking, trending, and recommendation feeds are designed to maximize engagement rather than serve user interests. The theater increased as platforms shifted from user-driven content (chronological feeds) to algorithmic selection. Traditional performance metrics (click-through rate, time-on-platform) are treated as measures of success despite misaligning with user wellbeing.
 *
 * PERSPECTIVAL GAP:
 *   The attention monopoly shows why single-perspective analysis fails. A platform operator's analysis sees efficiency and user benefit; a user's analysis sees behavioral trap and time theft; a creator's analysis sees mixed coordination and extraction; a regulator's analysis sees a solvable architectural problem. Each perspective captures real structural properties. The constraint's definition requires the presheaf over all positions: the monopoly is snare + tangled rope + rope + scaffold + piton + mountain, depending on observational context.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators have high arbitrage optionality and benefit from the constraint, yielding low d. Users have minimal exit options and bear the cost, yielding high d. This differentiates their directionality values and thus their effective extractiveness chi. The base extractiveness (0.58) is scaled by f(d) for each agent: beneficiaries experience negative chi (they are subsidized); victims experience chi > epsilon (they bear more than the average cost).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three gates: (1) Beneficiaries declared (platform operators, advertisers, attention extractors); (2) Victims declared (users, content creators, information ecosystem); (3) Active enforcement required (algorithmic curation, behavioral design, data collection infrastructure maintain the extraction). The classification prevents misreading as pure rope (mere coordination) or pure snare (mere extraction). The coordination function is real — platforms do match creators to audiences — but it is asymmetrically captured through data extraction, algorithmic control, and behavioral manipulation. Mandatrophy is resolved by showing that both the coordination and extraction are structural, not contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_addiction_mechanism,
    'Is suppression structural (switching costs, network effects, data lock-in) or behavioral (internalized addiction, neurochemical dependence)?',
    'Comparative analysis: do users who successfully migrate to alternatives experience withdrawal symptoms? Do design changes (notification reduction) correlate with engagement loss? Post-exit behavior patterns for long-term quitters.',
    'If structural: exit barriers are primarily economic/technical (can be reformed by interoperability mandates). If behavioral: exit barriers are internalized and persist even after structural barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_addiction_mechanism, empirical, 'Whether suppression is structural or behavioral addiction').

omega_variable(
    algorithmic_opacity_extractiveness,
    'How much of the extractiveness (0.58) is driven by algorithmic opacity vs. deliberate attention capture design?',
    'A/B testing: compare platforms with transparent vs opaque algorithms; audit algorithmic recommendations for attention-hoarding properties; historical analysis of design changes and engagement metrics.',
    'If opacity-driven: transparency reforms (EU DSA) could reduce extractiveness significantly. If design-driven: opacity is a symptom, not a cause — reducing it without changing incentives will produce new extraction mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_extractiveness, empirical, 'Opacity vs. design as extraction mechanism').

omega_variable(
    platform_coordination_necessity,
    'Do modern content creators genuinely need centralized platform coordination, or has decentralized coordination become sufficient?',
    'Economic viability analysis: can creators sustain audiences on federated platforms? Comparison of discovery mechanisms (algorithmic vs. social vs. search). Time-to-profitability for creators on centralized vs. decentralized platforms.',
    'If genuinely necessary: platforms'' rope classification is accurate — they solve a real coordination problem. If decentralized alternatives work: scaffold sunset is real — regulation and alternatives will succeed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_coordination_necessity, empirical, 'Whether platform coordination is genuinely necessary').

omega_variable(
    network_effect_lock_in_reversibility,
    'Are network effects that lock users into platforms reversible, or do they represent natural monopolies?',
    'Historical case studies of platform migrations and defection cascades (Myspace → Facebook, Twitter diaspora post-Musk). Network effect reversibility thresholds. Mechanism analysis: are network effects driven by switching costs (reversible) or by genuine coordination superiority (harder to reverse)?',
    'If reversible: attention monopoly is a policy-dependent Snare/Tangled Rope, not a mountain. If irreversible: natural monopoly dynamics may make structural reform impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_in_reversibility, empirical, 'Whether network effect lock-in is reversible').

omega_variable(
    regulatory_capture_risk,
    'Will regulation (DSA/DMA) reduce extraction or be captured and weaponized by incumbent platforms?',
    'Comparative analysis across jurisdictions (EU vs. China vs. US). Temporal tracking of regulatory effectiveness post-implementation. Mechanism analysis: do compliance costs disproportionately burden small competitors? Do large platforms use regulation to entrench market position?',
    'If effective: scaffold sunset is real. If captured: regulation may entrench the monopoly by raising barriers for alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether regulation will reduce or entrench extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_monopoly, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atte_tr_t0, attention_monopoly, theater_ratio, 0, 0.42).
narrative_ontology:measurement(atte_tr_t7, attention_monopoly, theater_ratio, 7, 0.55).
narrative_ontology:measurement(atte_tr_t15, attention_monopoly, theater_ratio, 15, 0.68).
narrative_ontology:measurement(atte_tr_t10, attention_monopoly, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(atte_be_t0, attention_monopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(atte_be_t7, attention_monopoly, base_extractiveness, 7, 0.47).
narrative_ontology:measurement(atte_be_t15, attention_monopoly, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(atte_be_t10, attention_monopoly, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_monopoly, resource_allocation).
narrative_ontology:affects_constraint(attention_monopoly, algorithmic_curation_opacity).
narrative_ontology:affects_constraint(attention_monopoly, behavioral_addiction_design).
narrative_ontology:affects_constraint(attention_monopoly, data_asymmetry_extraction).
narrative_ontology:affects_constraint(attention_monopoly, creator_economic_dependence).

% DUAL FORMULATION NOTE:
% Attention monopoly is a constraint family decomposable by mechanism. Base story covers the integrated monopoly structure. Downstream constraints address specific extraction mechanisms: algorithmic curation (information standard type), behavioral design (attachment coordination abuse), data asymmetry (identity coordination), and economic dependence (resource allocation). Each has distinct epsilon and classification. Links enable contamination analysis: if regulation reduces algorithmic opacity, does extractiveness decrease proportionally or do other mechanisms compensate?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_monopoly, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
