% ============================================================================
% CONSTRAINT STORY: algorithmic_curation_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_curation_lock_in, []).

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
 *   constraint_id: algorithmic_curation_lock_in
 *   human_readable: Algorithmic Curation Lock-In in Content Distribution
 *   domain: digital_platforms/information_architecture
 *
 * SUMMARY:
 *   Algorithmic curation lock-in describes the structural constraint where
 *   platforms use proprietary recommendation algorithms to route content to
 *   users, creating dependencies for both content creators and users on a
 *   single actor's ranking mechanism. Creators cannot reach audiences without
 *   algorithmic visibility; users cannot discover content outside the
 *   algorithm's filtered view; competitors cannot build viable alternatives
 *   without achieving the same scale that the incumbent provides. The
 *   constraint exhibits genuine coordination benefits (solving the discovery
 *   problem in high-dimensional content spaces) alongside extractive
 *   mechanisms (concentration of attention distribution power, opaque ranking
 *   that prevents creator agency, lock-in that prevents exit). Theater has
 *   increased over time as platforms added regulatory transparency features
 *   while keeping ranking mechanisms proprietary. Extractiveness has grown as
 *   algorithmic sophistication increased, making it harder for emerging
 *   creators to achieve visibility through quality alone — the algorithm now
 *   requires understanding engagement metrics, audience psychology, and
 *   platform-specific affordances. This is a tangled rope at the analytical
 *   level: the coordination function (discovery) is real and valuable, but
 *   the implementation locks all parties into a centralized mechanism that
 *   extracts value by controlling information distribution.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — invisible to users without algorithmic ranking; cannot build audience or exit without abandoning their work
 *   - Mid-Tier Creators: Secondary victims (moderate/constrained) — benefit from algorithmic amplification but constrained by high switching costs and must optimize for metrics rather than audience preferences
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — control attention distribution and advertiser access; can change algorithms or policies at will
 *   - Decentralized Coalition: Organized challengers (organized/constrained) — building protocol-based alternatives but face network effects and lack incumbent infrastructure
 *   - Regulatory Agencies: Institutional actors (institutional/arbitrage) — enforce transparency requirements and appeals processes; see compliance as checkbox exercise rather than structural change
 *   - Users: Secondary victims (moderate/constrained) — discover content only through curated feeds; experience filter bubble effects and algorithmic manipulation of preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_curation_lock_in, 0.58).
domain_priors:suppression_score(algorithmic_curation_lock_in, 0.65).
domain_priors:theater_ratio(algorithmic_curation_lock_in, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_curation_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_curation_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_curation_lock_in, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_curation_lock_in, tangled_rope).
narrative_ontology:human_readable(algorithmic_curation_lock_in, "Algorithmic Curation Lock-In in Content Distribution").
narrative_ontology:topic_domain(algorithmic_curation_lock_in, "digital_platforms/information_architecture").

domain_priors:requires_active_enforcement(algorithmic_curation_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_curation_lock_in, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_curation_lock_in, high_ranking_content_producers).
narrative_ontology:constraint_victim(algorithmic_curation_lock_in, low_ranking_content_creators).
narrative_ontology:constraint_victim(algorithmic_curation_lock_in, user_discovery_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Small creators with no algorithmic visibility have no mechanism to build audience outside the algorithm's recommendations. Trapped: cannot reach users, cannot build reputation, cannot exit the platform without abandoning their work entirely. The algorithm's opacity means they cannot understand why they are invisible or how to improve position. Maximum extraction — the constraint consumes their effort while distributing rewards to established creators.
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER CREATOR (TANGLED ROPE) — Constrained by high switching costs (audience lock-in, platform-specific tools) but benefits from algorithmic amplification for their content. Experiences both coordination (algorithm distributes their work to interested audiences) and extraction (must game metrics to maintain visibility, algorithm captures derivative value). Can theoretically exit to other platforms but faces audience loss and retraining costs.
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Operates the curation algorithm as a coordination mechanism: routes content to interested users, solves the discovery problem, enables creators to reach audiences at scale. Experiences the constraint as pure coordination with benefits to all parties. Arbitrage available: can switch algorithms, update models, change ranking criteria. The extraction is invisible from this perspective because it is structurally downstream — the platform captures user attention and advertiser value, which is experienced as the service's core function, not as extraction.
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED PROTOCOL COALITION (TANGLED ROPE) — Organized advocates (interoperability standards, federation protocols, open-source algorithms) see the lock-in as solvable through protocol alternatives. Constrained: building alternative infrastructure requires ecosystem coordination and faces network effect headwinds. Benefits from genuine coordination benefits of algorithmic curation itself (the problem of discovery is real). The extraction is the platform's monopoly on the curation mechanism; solving it requires both coordination (building standards) and fighting extraction (overcoming network lock-in).
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Platform regulation (DSA, DMA, content moderation requirements) adds mandatory algorithmic transparency and appeals processes. Theater_ratio high: much of regulatory compliance is performative auditing and checkbox transparency. The underlying curation mechanism remains opaque because true algorithmic transparency would expose the platform's extraction logic to competitors and creators alike. Regulators maintain the framework through institutional obligation despite knowing the actual effect is limited. The constraint persists through inertia and political performance.
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the long view, algorithmic curation solves the genuine discovery problem in a high-dimensionality information space (too much content, too many creators, users cannot survey all options). The coordination function is real and valuable. But the implementation locks users and creators into a single algorithm, concentrating power over attention distribution in a single actor. The lock-in is not inherent to curation — it is a choice to use proprietary algorithms with closed training data and opaque ranking. Alternative implementations (federated curation, open-source ranking, user-customizable algorithms) would preserve the coordination benefit while reducing extraction. The constraint is extractive only because of the architectural choice to centralize and privatize the curation mechanism.
constraint_indexing:constraint_classification(algorithmic_curation_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_curation_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_curation_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_curation_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_curation_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_curation_lock_in, TR),
    TR >= 0.70.

:- end_tests(algorithmic_curation_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and increasing. The platform captures value by controlling attention distribution — creators must compete within the algorithm's criteria, users see filtered views that optimize for engagement rather than utility, and the platform captures advertiser surplus. Early internet curation (0.28) was less extractive because it relied on human editors and discovery mechanisms that were less powerful at predicting user engagement. As machine learning improved (0.42 at midpoint), algorithms became better at predicting and manipulating behavior, increasing extraction. Current extractiveness (0.58) reflects the sophisticated engagement optimization that treats users as attention sources and creators as content suppliers. Suppression (0.65): High. Creators lack transparency into ranking mechanisms, users lack alternatives to curated discovery, and switching costs lock both parties in. The opacity is not accidental — it protects the algorithm's competitive advantage and prevents gaming. This high suppression distinguishes the constraint from pure coordination. Theater ratio (0.68): High. Regulatory compliance (algorithm transparency statements, appeals processes, audit trails) is largely performative — the actual ranking mechanism remains opaque, and transparency statements often reveal little about how content is weighted or scored. The theater has increased as regulators have required disclosure, yet the underlying curation mechanism remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the platform operator (rope) and emerging creators (snare) is maximal. Both observe the same algorithmic curation mechanism. The platform sees it as solving the discovery problem — matching content to interested users at massive scale. Creators see it as an opaque barrier to visibility with no mechanism for improvement. The mid-tier creator (tangled rope) experiences both: they benefit from algorithmic amplification when the algorithm favors their content, but are suppressed if the algorithm deprioritizes their category. The regulatory framework (piton) sees the constraint as degraded — transparency requirements and appeals processes are maintained through political obligation despite limited effect on actual curation. The analytical observer (tangled rope) sees the real tension: the coordination benefit is genuine (curation is hard at scale), but the implementation extracts by locking all parties into proprietary systems rather than using open, federated mechanisms that would preserve the coordination benefit while reducing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position: platform operators benefit from the constraint (low d ~0.10, negative chi), mid-tier creators experience mixed coordination and extraction (d ~0.55), emerging creators experience pure extraction (d ~0.92, high chi). The divergence across perspectives is not driven by different views of the same fact but by genuinely different structural positions. The platform operator's d is low because they control the curation mechanism — the constraint flows extraction toward them. Emerging creators' d is high because they are subject to algorithmic judgment with no transparency or appeal. Users' d would be moderate-high (~0.75) because they are targets of engagement optimization. The organized coalition's d is moderate-high (~0.70) because they are victims of lock-in effects but retain some exit capacity through protocol development. Regulatory actors' d is low (~0.20) because they are nominally external arbiters with institutional power, though captured by incumbent platforms through lobbying and operational complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the curation problem (genuine coordination) from the platform's solution (extractive lock-in). The coordination benefit is real: without algorithmic curation, users cannot survey billions of content items, and creators cannot reach audiences at scale. But the implementation is extractive: proprietary algorithms, opaque ranking, high switching costs, and engagement optimization over user utility. A tangled rope classification is correct because both functions exist. The extraction is not inherent to curation — alternative implementations (federated ranking, user-customizable algorithms, open-source systems) would preserve the coordination benefit while reducing extraction. The constraint is extractive only because the platform chooses to centralize and privatize the curation mechanism. The theater ratio (0.68) reveals that regulatory approaches (transparency mandates) have not addressed the underlying architecture — they have added performative compliance without enabling creator agency. The analytical perspective correctly identifies this as a choice point: curation + coordination is possible without lock-in, but the incumbent platform benefits from lock-in, so regulatory pressure must be sufficient to overcome the business incentive to maintain opacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_paradox,
    'Can true algorithmic transparency coexist with platform competitiveness, or does exposing the ranking mechanism enable gaming that degrades curation quality?',
    'Comparative study of platform transparency levels (YouTube''s ranking factors vs TikTok''s opacity) and resulting creator gaming behaviors; analysis of open-source recommendation systems and their resistance to manipulation',
    'If transparency enables gaming: lock-in is a necessary cost of curation integrity (Rope/Scaffold from more perspectives). If gaming is contained: lock-in is a choice to extract rather than coordinate (Snare/Tangled Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_paradox, empirical, 'Whether algorithmic transparency enables gaming that degrades quality').

omega_variable(
    federation_feasibility_threshold,
    'What scale of user base is required before federated/decentralized curation becomes functionally equivalent to centralized algorithms in discovery quality?',
    'Pilot studies of Mastodon, Bluesky, or Nostr discovery mechanisms; measurement of user satisfaction and content diversity metrics compared to centralized platforms at equivalent scale',
    'If threshold < current alternative platform scale: lock-in is not inevitable (Scaffold perspective gains strength). If threshold >> current alternatives: lock-in is a persistent network effect (Snare perspective for emerging creators is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_feasibility_threshold, empirical, 'Minimum scale for federated curation to match centralized performance').

omega_variable(
    attention_utility_misalignment,
    'How much of the platform''s algorithmic objective (engagement/time-on-platform) diverges from user utility (finding content the user actually wants)?',
    'User preference studies comparing recommended content quality (user satisfaction, usefulness) to algorithmic ranking; analysis of engagement-optimized vs discovery-optimized ranking on satisfaction metrics',
    'If divergence is small: curation is largely beneficial (Rope/Tangled Rope). If divergence is large: curation mechanism extracts user attention for platform value (Snare/Tangled Rope with high suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_utility_misalignment, empirical, 'Divergence between engagement optimization and user utility').

omega_variable(
    creator_skill_barrier_internalization,
    'Do creators internalize the algorithmic barrier as a natural limitation of ''content quality'' and ''audience taste,'' or do they perceive it as an extractive lock-in mechanism?',
    'Qualitative interviews with emerging and mid-tier creators; analysis of creator forums, migration patterns when alternative platforms emerge, and expressed barriers to exit',
    'If internalized: suppression is higher (creators stop trying if they believe the barrier is permanent). If perceived as extraction: organizing potential increases (creators may coordinate on alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_skill_barrier_internalization, conceptual, 'Whether algorithmic barriers are internalized as inevitable or perceived as extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_curation_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcur_tr_t0, algorithmic_curation_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(algcur_tr_t5, algorithmic_curation_lock_in, theater_ratio, 5, 0.52).
narrative_ontology:measurement(algcur_tr_t10, algorithmic_curation_lock_in, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(algcur_be_t0, algorithmic_curation_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(algcur_be_t5, algorithmic_curation_lock_in, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(algcur_be_t10, algorithmic_curation_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_curation_lock_in, information_standard).
narrative_ontology:affects_constraint(algorithmic_curation_lock_in, recommendation_system_filter_bubble).
narrative_ontology:affects_constraint(algorithmic_curation_lock_in, creator_income_concentration).

% DUAL FORMULATION NOTE:
% Algorithmic curation lock-in is upstream of both filter bubble dynamics (which assumption of algorithmic ranking creates) and creator income concentration (which emerges from algorithmic visibility inequality). Each downstream constraint has its own extractiveness reflecting specific mechanisms; this story captures the structural lock-in that enables both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_curation_lock_in, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
