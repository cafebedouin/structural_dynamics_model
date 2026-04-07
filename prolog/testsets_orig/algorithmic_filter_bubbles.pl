% ============================================================================
% CONSTRAINT STORY: algorithmic_filter_bubbles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_filter_bubbles, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_filter_bubbles
 *   human_readable: Algorithmic Filter Bubbles in Social Media and Content Platforms
 *   domain: digital_platforms/epistemic_regulation
 *
 * SUMMARY:
 *   Algorithmic filter bubbles represent a structural constraint where
 *   platform operators have designed information distribution systems
 *   optimized for engagement and revenue, not for epistemic diversity or user
 *   autonomy. The constraint exhibits all six classification types across
 *   different observer positions: immutable natural law (analytical
 *   observer), degraded theater (content moderation system), temporary
 *   coordination failure with regulatory sunset (coalition advocates),
 *   beneficial coordination (platform operators), mixed extraction and
 *   benefit (intermediaries), and pure extraction (isolated users). The
 *   extractiveness value (0.58) reflects a moderate-to-high extraction
 *   mechanism: platforms extract user attention, behavioral data, and
 *   epistemic autonomy in exchange for 'free' distribution and personalized
 *   content curation. The trajectory from 0.32 to 0.58 over the interval
 *   shows accumulating extraction as algorithmic optimization intensified and
 *   user awareness remained low. Theater ratio (0.65) reflects the
 *   performative gap between stated platform values (diverse, authentic, safe
 *   information ecosystems) and actual algorithmic behavior (optimize for
 *   engagement and user retention regardless of epistemic diversity cost).
 *
 * KEY AGENTS:
 *   - Platform Operators (institutional/arbitrage): Primary beneficiaries — capture advertising revenue, user attention, behavioral data, and network effects through algorithmic personalization
 *   - Isolated Users (powerless/trapped): Primary victims — trapped in filter bubbles with opaque algorithms, minimal exit options (platform switching is costly), and extraction of attention and autonomy
 *   - Journalists and News Organizations (moderate/constrained): Secondary victims and partial beneficiaries — benefit from algorithmic distribution but constrained by opaque ranking that determines visibility independent of journalistic quality
 *   - Content Creators with Polarizing Content (institutional/arbitrage): Secondary beneficiaries — algorithmic amplification rewards sensational, divisive, and emotionally engaging content at the expense of nuance and diversity
 *   - Advertisers and Data Brokers (institutional/arbitrage): Secondary beneficiaries — access fine-grained behavioral data and microtargeting capabilities enabled by algorithmic profiling
 *   - Digital Rights and Regulatory Advocates (organized/constrained): Organized agents seeing the constraint as temporary — pushing for algorithmic transparency, interoperability, and regulatory oversight with 5-10 year sunset horizon
 *   - Content Moderation Infrastructure (institutional/arbitrage): Institutional actor maintaining performative compliance with stated values while underlying algorithms optimize for engagement
 *   - Epistemic Commons and Democratic Deliberation (powerless/trapped): Abstract collective victim — cannot organize, advocate, or exit; bears cost of information fragmentation and institutional distrust
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_filter_bubbles, 0.58).
domain_priors:suppression_score(algorithmic_filter_bubbles, 0.62).
domain_priors:theater_ratio(algorithmic_filter_bubbles, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_filter_bubbles, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_filter_bubbles, tangled_rope).
narrative_ontology:human_readable(algorithmic_filter_bubbles, "Algorithmic Filter Bubbles in Social Media and Content Platforms").
narrative_ontology:topic_domain(algorithmic_filter_bubbles, "digital_platforms/epistemic_regulation").

domain_priors:requires_active_enforcement(algorithmic_filter_bubbles).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_filter_bubbles, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_filter_bubbles, content_creators_with_polarizing_content).
narrative_ontology:constraint_beneficiary(algorithmic_filter_bubbles, advertisers_targeting_microcohorts).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, users_seeking_diverse_information).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, epistemic_commons).
narrative_ontology:constraint_victim(algorithmic_filter_bubbles, democratic_deliberation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED USER (SNARE) — Users trapped in algorithmic filter bubbles have minimal exit capacity. Switching platforms requires rebuilding social graphs and relearning interfaces. Algorithmic personalization is not transparent — users cannot directly manipulate their feed to access diverse content. The constraint extracts attention, data, and cognitive autonomy with high suppression: algorithmic opacity prevents users from understanding why they see what they see, and the filter bubble mechanism is designed to be addictive. Full extraction from the user's epistemic autonomy.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JOURNALIST/INTERMEDIARY (TANGLED ROPE) — News organizations and independent journalists benefit from platform distribution (reach, engagement metrics) but are constrained by algorithmic ranking. They must optimize content for algorithmic promotion while maintaining editorial standards. The constraint provides genuine coordination (distributing information at scale) alongside asymmetric extraction (algorithmic gatekeeping determines visibility regardless of quality or newsworthiness). High suppression: journalists cannot reliably predict what the algorithm will promote.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination mechanism: algorithmic curation solves the problem of information overload at scale. The platform benefits from user engagement (which drives advertising revenue), content creator participation, and network effects. From this perspective, the algorithm is a genuine coordination solution that enables billions of users to access personalized content. The platform has high exit capacity — they can modify the algorithm unilaterally. Pure beneficiary.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY INTERVENTION COALITION (SCAFFOLD) — Organized actors (digital rights advocates, regulators, interoperability advocates) see filter bubbles as a temporary coordination failure solvable through transparency requirements, algorithmic auditing, and platform regulation (EU Digital Services Act, algorithmic accountability legislation). These actors have agency and see an exit path: regulatory frameworks that mandate algorithmic transparency and user choice would reduce the extraction mechanism. The constraint has a sunset clause embedded in emerging regulation — as transparency mandates and portability requirements mature, the algorithmic opacity that enables suppression diminishes.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT MODERATION INFRASTRUCTURE (PITON) — Community guidelines and content moderation policies are largely performative theater: the stated goal is 'safe, authentic, diverse information ecosystems,' but the implemented mechanism (algorithmic filtering for engagement) directly undermines this goal. Moderation decisions are made by algorithms that optimize for engagement, not for epistemic health. The system maintains the performance of caring about quality and diversity while the underlying extraction mechanism (engagement maximization) operates in the opposite direction. Theater ratio 0.65 reflects this decoupling. The moderation apparatus persists through institutional inertia despite being degraded.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some degree of information filtering is inherent to human cognition and bounded attention: we cannot process all available information, so we must filter. Algorithms are simply automating what humans do biologically. From this view, filter bubbles are an immutable property of information systems at scale. However, this perspective naturalizes what is actually a contingent design choice — the algorithms are optimized for engagement and profit, not for cognitive capacity or epistemic diversity. The analytical observer risks mistaking a particular business model for a law of nature.
constraint_indexing:constraint_classification(algorithmic_filter_bubbles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_filter_bubbles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_filter_bubbles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_filter_bubbles, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_filter_bubbles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_filter_bubbles, TR),
    TR >= 0.70.

:- end_tests(algorithmic_filter_bubbles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. Platforms extract user attention (time in feed), behavioral data (click patterns, dwell time, social connections), and epistemic autonomy (through opaque algorithmic selection). The extraction is not total — users do receive value in the form of personalized content discovery and social connectivity. However, the personalization is optimized for platform benefit (engagement), not user benefit (epistemic diversity). The increasing trajectory (0.32→0.58) reflects intensifying algorithmic optimization and decreasing user awareness. Suppression (0.62): High. Multiple suppression mechanisms operate: algorithmic opacity prevents users from understanding why they see what they see; platform switching costs (network effects, data lock-in, interface learning curves) trap users; algorithmic design obscures the personalization process from users; and recommendation diversity is actively suppressed in favor of engagement-optimized content. Theater ratio (0.65): Moderate-to-high. Platform public positions emphasize 'diverse information ecosystems' and 'community standards' while algorithmic systems actively optimize for engagement and user retention. Content moderation is framed as upholding community values but actually serves platform liability reduction and engagement maximization. The performative gap has widened as awareness of filter bubbles increased, requiring more extensive theater to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between institutional beneficiaries who see the constraint as coordination and powerless victims who see it as extraction. Platform operators experience algorithmic personalization as solving a genuine technical problem (information overload). Users experience it as opacity and control loss. Journalists experience both: benefits from algorithmic distribution but constraints from algorithmic gatekeeping. Regulatory advocates see a temporary institutional failure fixable through transparency and interoperability. The isolated user's Snare classification reflects that they have no escape path — algorithmic opacity prevents informed choice, platform switching costs are prohibitive, and user agency over feed personalization is minimal. The regulatory coalition's Scaffold classification reflects organized agency and a visible sunset (regulatory frameworks) — but this sunset depends on political will and platform cooperation, both uncertain. The analytical observer's false summit (Mountain) naturalizes what is actually a design choice: algorithms could be optimized for epistemic diversity, user autonomy, and democratic deliberation rather than engagement. The apparent immutability is contingent on maintaining engagement as the optimization target.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from beneficiary/victim status plus power/exit parameters. Beneficiaries with arbitrage exit (platform operators) have d ≈ 0.05 → low experienced extraction. Victims with trapped exit (isolated users) have d ≈ 0.95 → high experienced extraction. Mixed roles with constrained exit (journalists) have d ≈ 0.55 → moderate extraction. Advocates for abstract victims with organized power (regulators) have d ≈ 0.60 → moderate experienced extraction (as proxy for the victims they represent). The suppression term is not scaled by d — it remains 0.62 across all perspectives because suppression is a structural property of the constraint (algorithmic opacity, platform lock-in, information asymmetry) that affects all agents regardless of their power. The extractiveness is scaled by f(d) and scope σ(S), producing different effective extraction (chi) values across perspectives. This explains why the same constraint appears as rope to beneficiaries and snare to victims: their structural positions produce different d values, which map through f(d) to produce different chi values, which classify differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is not yet fully resolved. The constraint is claimed as Tangled Rope based on analytical perspective, but the empirical evidence is still in flux. Resolution requires answering: (1) Can algorithmic transparency sufficiently reduce extraction without compromising platform functionality? If yes, the constraint moves toward Rope with a sunset (Scaffold). (2) Is engagement optimization inherently extractive, or can it be redesigned? If redesignable, Scaffold sunset is realistic. If inherent, deeper structural change (different business model) is required. (3) Can interoperability standards meaningfully reduce suppression? If yes, user exit options improve and suppression decreases, potentially moving the constraint toward Scaffold or even Rope. The current tangled_rope classification reflects that genuine coordination (algorithmic curation solves information overload) coexists with asymmetric extraction (platform operators capture value, users bear epistemic costs). Resolving mandatrophy requires empirical evidence on whether the coordination and extraction can be decoupled through regulatory intervention or whether they are intrinsically linked through engagement optimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_threshold,
    'What level of algorithmic transparency would sufficiently reduce filter bubble extraction without undermining platform functionality?',
    'Empirical testing of transparency mechanisms (explainable AI, recommendation diversity controls, audit trails) against user engagement metrics and information diversity metrics',
    'If low threshold: transparency alone suffices to reduce extraction to rope levels. If high threshold: transparency is necessary but insufficient — deeper structural changes (engagement metric redesign, portfolio diversity optimization) required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_threshold, empirical, 'Threshold for algorithmic transparency sufficiency in reducing extraction').

omega_variable(
    engagement_optimization_inevitability,
    'Is optimization for engagement metrics (watch time, click-through, re-shares) inherently extractive, or can engagement metrics be redesigned to align with epistemic diversity?',
    'Alternative engagement metrics that incorporate information diversity, exposure to contradictory viewpoints, and time spent on complex (non-sensational) content; empirical outcomes when platforms implement alternative metrics',
    'If inherently extractive: filter bubbles cannot be solved without replacing the engagement optimization model entirely. If redesignable: scaffold sunset is realistic — new metrics could transition platforms to epistemic-health optimization within 5-10 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_optimization_inevitability, empirical, 'Whether engagement optimization is inherently extractive or redesignable').

omega_variable(
    user_agency_restoration_capacity,
    'Can algorithmic filter bubbles be meaningfully addressed by giving users more direct control (recommendation diversity sliders, explicit feed preferences), or is the filter bubble effect primarily driven by algorithmic opacity and psychological habituation rather than user choice?',
    'Randomized controlled trials with transparency + control mechanisms; measurement of whether users choosing diverse feeds actually consume diverse content vs. resorting to algorithmic curation',
    'If agency-based solution works: users can escape filter bubbles through informed choice (exit_options upgrade from trapped to constrained). If habituation dominates: user choice is illusory, and suppression remains high even with transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_restoration_capacity, empirical, 'Whether user control can restore epistemic agency in filter bubbles').

omega_variable(
    cross_platform_interoperability_feasibility,
    'Would open standards for user data portability and algorithmic recommendations (ActivityPub federation, portable identity, interoperable feeds) sufficiently reduce platform lock-in to lower suppression?',
    'Deployment outcomes of federated social networks (Mastodon, Bluesky, Threads interoperability); user switching rates and feed diversity when platforms implement portability standards',
    'If feasible: exit_options improve from trapped to constrained/arbitrage — users can switch platforms with portable feeds and social graphs. If not feasible: network effects and data lock-in persist despite technical standards.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_platform_interoperability_feasibility, empirical, 'Whether interoperability can sufficiently reduce platform lock-in').

omega_variable(
    epistemic_commons_boundary,
    'Is there a measurable threshold below which information diversity in algorithmic feeds produces objectively degraded epistemic outcomes (inability to form shared factual beliefs, cascading disbelief in all institutions)?',
    'Cross-sectional and longitudinal studies of belief fragmentation, fact-checking success rates, and institutional trust as functions of algorithmic feed diversity metrics',
    'If threshold exists and measurable: can define ''adequate epistemic health'' and regulate toward it. If threshold is vague or nonexistent: epistemic commons protection is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_boundary, conceptual, 'Measurability of epistemic commons degradation threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_filter_bubbles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afb_tr_t0, algorithmic_filter_bubbles, theater_ratio, 0, 0.38).
narrative_ontology:measurement(afb_tr_t5, algorithmic_filter_bubbles, theater_ratio, 5, 0.52).
narrative_ontology:measurement(afb_tr_t10, algorithmic_filter_bubbles, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(afb_be_t0, algorithmic_filter_bubbles, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(afb_be_t5, algorithmic_filter_bubbles, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(afb_be_t10, algorithmic_filter_bubbles, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_filter_bubbles, information_standard).
narrative_ontology:boltzmann_floor_override(algorithmic_filter_bubbles, 0.12).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, algorithmic_bias_in_hiring).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, recommendation_system_lock_in).
narrative_ontology:affects_constraint(algorithmic_filter_bubbles, epistemic_fragmentation_and_polarization).

% DUAL FORMULATION NOTE:
% Algorithmic filter bubbles decompose into multiple structurally distinct constraints: (1) algorithmic opacity (information asymmetry) with high theater but lower extractiveness, (2) platform lock-in (switching costs and network effects) with high suppression, and (3) engagement optimization (business model alignment with epistemic degradation) with high extractiveness. These are linked by causal and institutional coupling but represent distinct mechanisms. This story addresses the unified constraint; downstream constraints address the component mechanisms separately. The unified extractiveness (0.58) is an aggregate of these components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
