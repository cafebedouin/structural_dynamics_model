% ============================================================================
% CONSTRAINT STORY: algorithmic_information_curation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_information_curation, []).

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
 *   constraint_id: algorithmic_information_curation
 *   human_readable: Algorithmic Information Curation
 *   domain: digital_platforms/information_systems
 *
 * SUMMARY:
 *   Algorithmic information curation on digital platforms creates a
 *   structural constraint where the ranking and distribution of information
 *   is determined by engagement metrics (likes, shares, comments, dwell time)
 *   optimized for platform revenue and user retention. This constraint
 *   exhibits extraction from information consumers and marginalized creators
 *   while providing coordination benefits to platform operators and
 *   high-engagement creators. The mechanism suppresses information diversity,
 *   marginalizes non-mainstream perspectives, and extracts attention and
 *   behavioral data at scale. The constraint demonstrates perspectival
 *   plurality: passive consumers experience pure extraction (Snare);
 *   marginalized creators experience mixed coordination-extraction (Tangled
 *   Rope); high-engagement creators and platform operators experience genuine
 *   coordination (Rope); advocacy coalitions experience constrained
 *   opposition (Tangled Rope); content moderation governance appears as
 *   degraded theater (Piton); and the analytical observer risks naturalizing
 *   engagement ranking as an inevitable consequence of information scarcity
 *   (false Mountain). The extractiveness trajectory shows accumulation over
 *   time as platforms optimize their engagement metrics and as the network
 *   effects concentrate attention on high-performing content, raising
 *   barriers for marginalized creators. Theater ratio rises as platforms
 *   introduce transparency initiatives, algorithmic explainability, and
 *   content moderation governance boards that appear to address the problem
 *   while the primary extraction mechanism (engagement ranking) remains
 *   intact.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — captures advertising revenue, user growth, and engagement data; experiences constraint as positive coordination mechanism
 *   - Passive Information Consumers: Primary victim (powerless/trapped) — receives algorithmically filtered feed with no exit option; attention and behavioral data extracted; epistemic diversity suppressed
 *   - Marginalized Creators: Secondary victim (moderate/constrained) — algorithmically suppressed due to lower engagement history; faces barriers to visibility and reach; benefits from platform distribution but disadvantaged by ranking
 *   - High-Engagement Creators: Secondary beneficiary (powerful/mobile) — receives algorithmic amplification; can move to alternative platforms or monetize independently; net beneficiary despite platform restrictions
 *   - Information Consumers Collectively: Victim (powerless/trapped) — epistemic commons degraded by engagement-optimized information ranking; filter bubbles and algorithmic radicalization reduce information diversity
 *   - Organized Advocacy Coalitions: Organized victim (organized/constrained) — can pressure platforms for algorithmic modification but face regulatory and network-effect barriers; constrained opposition to extraction
 *   - Content Moderation Governance: Institutional actor (institutional/arbitrage) — maintains performative moderation policy that appears to address harm while engagement-driven ranking persists; theater mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_information_curation, 0.62).
domain_priors:suppression_score(algorithmic_information_curation, 0.68).
domain_priors:theater_ratio(algorithmic_information_curation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_information_curation, extractiveness, 0.62).
narrative_ontology:constraint_metric(algorithmic_information_curation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_information_curation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_information_curation, tangled_rope).
narrative_ontology:human_readable(algorithmic_information_curation, "Algorithmic Information Curation").
narrative_ontology:topic_domain(algorithmic_information_curation, "digital_platforms/information_systems").

domain_priors:requires_active_enforcement(algorithmic_information_curation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_information_curation, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_information_curation, high_engagement_creators).
narrative_ontology:constraint_victim(algorithmic_information_curation, information_consumers).
narrative_ontology:constraint_victim(algorithmic_information_curation, marginalized_creators).
narrative_ontology:constraint_victim(algorithmic_information_curation, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE INFORMATION CONSUMER (SNARE) — Structurally trapped within algorithmic feed systems with no exit option. Algorithms optimize for engagement rather than accuracy or diversity, extracting attention and behavioral data while suppressing alternative information sources. Consumer has no mechanism to escape the filtering mechanism or negotiate its terms. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(algorithmic_information_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED CREATOR (TANGLED ROPE) — Faces high barriers to visibility and reach. Algorithms suppress content from creators with lower engagement history, different demographics, or non-mainstream perspectives. Exit is technically possible (switch platforms, create alternative channels) but constrained by resource requirements, network effects, and opportunity costs. Mixed experience: the platform provides distribution infrastructure and potential audience, but the algorithmic ranking systematically extracts attention from their content toward high-engagement creators.
constraint_indexing:constraint_classification(algorithmic_information_curation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits directly from algorithmic curation. Engagement metrics drive advertising revenue and user retention. Experiences the constraint as coordination: algorithms solve the problem of recommendation at scale, enabling the platform to function as an intermediary. Has full arbitrage options (can modify algorithms, adjust ranking, change incentive structures). Extraction flows toward this agent; the constraint is experienced as a positive coordination mechanism that creates value.
constraint_indexing:constraint_classification(algorithmic_information_curation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-ENGAGEMENT CREATOR (ROPE) — Benefits from algorithmic amplification. Content that triggers engagement metrics receives exponential distribution. Mobile exit options: can move to alternative platforms, build independent audiences, monetize through multiple channels. Experiences the constraint as genuine coordination — the algorithm helps them reach audiences efficiently. Net beneficiary despite some platform-imposed restrictions on content.
constraint_indexing:constraint_classification(algorithmic_information_curation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED ADVOCACY COALITIONS (TANGLED ROPE) — Civil society, media literacy organizations, and transparency advocates can pressure platforms for algorithmic disclosure and modification. But exit is constrained by regulatory limitations, network effects, and the difficulty of building competing infrastructure at platform scale. Mixed experience: they have organizational capacity to challenge and negotiate (unlike passive consumers), but structural barriers prevent exit. The constraint extracts attention and influence from their counter-narratives while providing them platform access to express them.
constraint_indexing:constraint_classification(algorithmic_information_curation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION THEATER (PITON) — Platforms maintain explicit content moderation policies and oversight boards that appear to solve the curation problem independently of engagement metrics. But the primary constraint (information ranking by engagement) persists beneath the moderation layer. Moderation is performative — it addresses the most egregious harms while the fundamental extraction mechanism (engagement-driven ranking) remains intact and often unexamined. Theater ratio reflects the gap between moderation policy and actual curation dynamics.
constraint_indexing:constraint_classification(algorithmic_information_curation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information distribution at scale necessarily involves prioritization; some curation mechanism is inevitable given attention scarcity. The constraint might appear to be a natural limit: you cannot show all information to all people, so some ranking is inherent to information systems. However, this naturalizes the specific mechanism (engagement-driven algorithmic ranking) as the only solution, when alternatives exist (diversity-weighted ranking, editorial human curation, participatory governance, content networks without centralized algorithms). The engine will detect this as a false summit revealing contingent institutional choice misframed as natural law.
constraint_indexing:constraint_classification(algorithmic_information_curation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_information_curation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_information_curation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_information_curation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_information_curation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_information_curation, TR),
    TR >= 0.70.

:- end_tests(algorithmic_information_curation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The engagement-ranking mechanism extracts attention, behavioral data, and creator opportunity from users and marginalized creators to benefit platform operators and high-engagement creators. The extraction is not absolute (some coordination benefits exist in information distribution infrastructure) but substantial and asymmetric. The trajectory shows accumulation: as platforms optimize engagement metrics and grow, barriers to marginalized creators increase. Suppression (0.68): High. Engagement metrics suppress marginalized perspectives, non-profitable content, and information diversity. Users face algorithmic filtering with limited transparency or control. Creators face algorithmic gatekeeping. The suppression is structural (embedded in ranking function) and enforced through feed design and recommendation dynamics. Theater ratio (0.58): Moderate-high. Platforms deploy transparency initiatives, algorithmic explainability, and content moderation boards that appear to solve the problem. But the primary extraction mechanism (engagement ranking) persists beneath the performative layer. Moderation addresses flagrant harms but does not challenge the fundamental ranking incentive. Theater ratio rises over time as platforms invest more in governance performance relative to algorithmic modification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Platform operators see Rope (coordination mechanism that solves information distribution at scale). Passive consumers see Snare (pure extraction with no exit). Marginalized creators see Tangled Rope (genuine coordination infrastructure but asymmetric extraction via algorithmic suppression). Advocacy coalitions see Tangled Rope (can organize opposition but constrained by network effects). Content moderation apparatus sees Piton (performative governance theater maintaining ritual appearance of control while extraction persists). The analytical observer risks seeing Mountain (naturalization: 'you must rank information somehow, so engagement ranking is inevitable'). The engine detects the mountain as false summit — alternatives to engagement ranking exist (diversity weighting, human editorial, participatory governance) and are technically feasible, making engagement ranking a contingent institutional choice rather than natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations determine directionality: platform operators and high-engagement creators are beneficiaries (d ≈ 0.05–0.40, low χ); passive consumers, marginalized creators, and the epistemic commons are victims (d ≈ 0.75–0.95, high χ). The suppression metric (0.68) reflects structural barriers to alternative information pathways and algorithmic gatekeeping. The extractiveness metric (0.62) reflects asymmetric concentration of attention and data toward beneficiaries. The measurement trajectory shows accumulation: over 10 periods, extractiveness rises from 0.35 to 0.62 as platforms optimize engagement metrics and build network effects that concentrate attention. Theater ratio rises from 0.35 to 0.58 as platforms invest in transparency and governance performance without modifying ranking incentives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Is engagement-based ranking a genuine coordination mechanism (solving the problem of information distribution at scale) that justifies asymmetric extraction, or is it primarily an extraction mechanism labeled as coordination? The tangled_rope classification requires both genuine coordination function AND asymmetric extraction + active enforcement. Evidence for coordination: (1) engagement ranking does enable information distribution to billions at minimal per-user cost, (2) alternative curation methods (human editorial, participatory governance) have not scaled to comparable user bases, (3) some creators benefit from algorithmic amplification. Evidence for pure extraction: (1) engagement metrics optimize for attention capture and behavior modification, not information accuracy or diversity, (2) the same ranking mechanism that distributes information also concentrates attention on high-profit content, (3) marginalized creators and diverse perspectives are systematically suppressed. Resolution hinges on omega variable 'mandatrophy_engagement_coordination_boundary': if engagement ranking produces measurably worse information outcomes (accuracy, diversity, reliability) than alternatives, it fails the coordination gate and should classify as Snare. If it produces comparable outcomes, Tangled Rope holds. Current evidence suggests the former — engagement ranking produces greater filter bubbles and misinformation spread than diversity-weighted or human-curated alternatives — indicating the constraint may be better classified as Snare with coordination justification as false cover story. However, the tangled_rope classification is retained pending empirical resolution because platforms do provide real distribution infrastructure and because some genuine coordination function exists alongside the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_metric_definition,
    'What constitutes ''engagement'' in the algorithmic ranking function, and how does this definition determine which information is amplified?',
    'Reverse-engineering platform ranking algorithms through user studies, leaked documentation, or regulatory disclosure. Cross-platform comparison of engagement metrics (likes, shares, dwell time, comments) and their correlation with information accuracy/diversity.',
    'If engagement proxies for accuracy: constraint classifies as lower-extraction coordination (Rope from more perspectives). If engagement actively selects for sensationalism/divisiveness: constraint classifies as higher-extraction snare (Snare confirmed from consumer perspective). Definition determines the constraint''s extractiveness baseline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_metric_definition, empirical, 'Whether engagement metrics align with accuracy, diversity, or divisiveness').

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does disclosure of algorithmic ranking criteria (when it occurs) actually enable users/creators to navigate or contest the system, or does transparency remain performative without corresponding power to modify the algorithm?',
    'Study of platform transparency reports: measure whether disclosed criteria enable predictability; user experiment on whether transparency changes behavior; comparative analysis of platforms with/without transparency mandates (EU vs US).',
    'If transparency enables contestation: suppression value drops, exit_options improve for moderate/organized agents. If transparency is ritual without power: theater_ratio rises, constraint remains snare/tangled_rope despite disclosure performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency enables meaningful agency').

omega_variable(
    alternative_curation_feasibility,
    'Can algorithmic curation be replaced by human editorial curation, participatory governance, or diversity-weighted algorithms without catastrophic content moderation failure or system-wide coordination breakdown?',
    'Analysis of non-algorithmic or alternative-algorithmic information systems (Mastodon federated governance, community moderation on volunteer platforms, Wikipedia editorial processes, academic journal peer review). Measurement of content diversity, accuracy, and user satisfaction metrics.',
    'If alternatives are feasible: the mountain perspective is false — curation mechanism is contingent, not natural law. Constraint reclassifies as human-agency-dependent (Rope or Tangled Rope). If alternatives fail: mountain may be partially correct — engagement ranking may be approximating an unavoidable optimization constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_curation_feasibility, empirical, 'Whether non-algorithmic curation mechanisms can scale').

omega_variable(
    suppression_mechanism_layering,
    'Is suppression of marginalized voices primarily a direct result of engagement-ranking algorithms, or is it amplified by downstream effects (filter bubbles, algorithmic radicalization, network homophily) that the engagement metric itself does not directly cause?',
    'Intervention study: modify engagement metrics on experimental platform instances while holding other variables constant; measure changes in content diversity and marginalized creator reach. Causal pathway analysis of suppression (direct vs indirect).',
    'If direct: engagement ranking is the primary extraction mechanism; suppression value reflects algorithmic design. If indirect: engagement ranking enables suppression through network dynamics; treating the algorithm as the constraint misses the true lever (recommender system design, not engagement metric itself). May require constraint decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_layering, empirical, 'Whether suppression is direct algorithmic effect or network cascade').

omega_variable(
    mandatrophy_engagement_coordination_boundary,
    'Does engagement-based ranking perform a genuine coordination function (solving information distribution at scale) that makes it Rope, or is engagement optimization primarily an extraction mechanism (concentrating attention on profitable content) that merely labels itself as coordination?',
    'Compare engagement-ranked platforms to diversity-weighted or human-curated systems on three metrics: (1) information accuracy/reliability, (2) creator incentive alignment, (3) consumer information diversity. If engagement-ranked systems score lower on all three, the coordination claim is false.',
    'If coordination is genuine: constraint is Rope (beneficiary''s perspective) or Tangled Rope (moderate victim''s perspective). If coordination is label-fraud: constraint is primarily Snare with coordination justification as cover story. This is the mandatrophy resolution question for algorithmic curation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_engagement_coordination_boundary, empirical, 'Whether engagement ranking performs genuine coordination or is pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_information_curation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcur_tr_t0, algorithmic_information_curation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algcur_tr_t3, algorithmic_information_curation, theater_ratio, 3, 0.42).
narrative_ontology:measurement(algcur_tr_t6, algorithmic_information_curation, theater_ratio, 6, 0.52).
narrative_ontology:measurement(algcur_tr_t10, algorithmic_information_curation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(algcur_be_t0, algorithmic_information_curation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algcur_be_t3, algorithmic_information_curation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(algcur_be_t6, algorithmic_information_curation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(algcur_be_t10, algorithmic_information_curation, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_information_curation, information_standard).
narrative_ontology:boltzmann_floor_override(algorithmic_information_curation, 0.1).
narrative_ontology:affects_constraint(algorithmic_information_curation, filter_bubble_amplification).
narrative_ontology:affects_constraint(algorithmic_information_curation, social_media_engagement_addiction).
narrative_ontology:affects_constraint(algorithmic_information_curation, misinformation_cascade_dynamics).

% DUAL FORMULATION NOTE:
% Algorithmic information curation decomposes into three structurally distinct constraints: (1) engagement-based ranking (this story, ε=0.62, Tangled Rope) — the primary constraint determining information distribution, (2) filter bubble amplification (ε=0.55, Tangled Rope) — the secondary effect of personalized algorithmic ranking creating echo chambers, (3) engagement-addiction dynamics (ε=0.70, Snare) — the behavioral extraction mechanism via intermittent reinforcement. Each has distinct mechanisms and measurable ε values. This story focuses on the curation constraint itself; the others capture downstream effects and behavioral extraction. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_information_curation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
