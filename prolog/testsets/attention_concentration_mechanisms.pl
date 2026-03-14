% ============================================================================
% CONSTRAINT STORY: attention_concentration_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_concentration_mechanisms, []).

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
 *   constraint_id: attention_concentration_mechanisms
 *   human_readable: Attention Concentration Mechanisms in Information Ecosystems
 *   domain: cognitive_economics/information_systems
 *
 * SUMMARY:
 *   Attention concentration mechanisms in information ecosystems create a
 *   structural tension between the coordination function of curation (helping
 *   agents navigate information abundance) and the extraction function of
 *   algorithmic amplification (concentrating visibility on high-engagement
 *   content to serve engagement metrics and advertiser interests). The
 *   constraint exhibits varying classifications across different observer
 *   positions: algorithmic curators perceive it as a coordination mechanism
 *   solving orientation problems; marginal information sources perceive it as
 *   pure extraction that suppresses their visibility; individual users
 *   experience mixed coordination and extraction (curation reduces cognitive
 *   load while algorithmic nudging manipulates behavior); open-source
 *   alternatives frame it as a temporary institutional phenomenon with a
 *   sunset clause (decentralized curation will eventually displace
 *   concentrated algorithmic control). The extractiveness trajectory (0.32 →
 *   0.58 over 16 years) reflects the progressive coupling of algorithmic
 *   recommendation with engagement metrics and advertiser targeting,
 *   amplifying the extraction component beyond the original coordination
 *   function. The theater ratio (0.42 → 0.68) indicates increasing
 *   performativity as traditional editorial review persists alongside
 *   algorithmic curation despite algorithmic control over actual visibility
 *   distribution.
 *
 * KEY AGENTS:
 *   - Attention Aggregators: Primary beneficiaries (institutional/arbitrage) — platforms and algorithmic curators capture engagement data, advertiser value, and user behavioral leverage
 *   - Attention Commons: Primary victims (powerless/trapped) — collective epistemic resource (journalism, scholarship, niche expertise) systematically devalued by concentration mechanisms
 *   - Marginal Information Sources: Secondary victims (powerless/trapped) — individual researchers and niche publications face algorithmic suppression despite information quality
 *   - Individual Attention Consumers: Mixed (moderate/constrained) — benefit from reduced cognitive load but experience behavioral manipulation and filter bubbles
 *   - Open Information Infrastructure Coalition: Organized agents (organized/constrained) — building decentralized alternatives but constrained by network effects and platform lock-in
 *   - Traditional Editorial Gatekeeping: Degraded institution (institutional/arbitrage) — maintains ritual authority while lacking actual control over attention distribution (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent technological choices as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_concentration_mechanisms, 0.58).
domain_priors:suppression_score(attention_concentration_mechanisms, 0.62).
domain_priors:theater_ratio(attention_concentration_mechanisms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_concentration_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_concentration_mechanisms, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attention_concentration_mechanisms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_concentration_mechanisms, tangled_rope).
narrative_ontology:human_readable(attention_concentration_mechanisms, "Attention Concentration Mechanisms in Information Ecosystems").
narrative_ontology:topic_domain(attention_concentration_mechanisms, "cognitive_economics/information_systems").

domain_priors:requires_active_enforcement(attention_concentration_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_concentration_mechanisms, attention_aggregators).
narrative_ontology:constraint_beneficiary(attention_concentration_mechanisms, algorithmic_curators).
narrative_ontology:constraint_victim(attention_concentration_mechanisms, attention_commons).
narrative_ontology:constraint_victim(attention_concentration_mechanisms, marginal_information_sources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION COMMONS (SNARE) — Collective epistemic resource (journalism, scholarship, discourse) cannot exit the concentration mechanism. Faces systematic devaluation as algorithmic curation concentrates visibility on already-visible sources. No organizational power to negotiate extraction terms. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINAL INFORMATION SOURCES (SNARE) — Individual researchers, niche publications, local journalists trapped by algorithmic preference for concentrated attention pools. Creating high-quality information yields minimal visibility. Exit would require abandoning their epistemic domain entirely. Suppression through algorithmic ranking creates perverse incentive structures.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ATTENTION AGGREGATORS (ROPE) — Platforms and algorithmic curators (social media, search engines, recommendation systems) perceive the constraint as coordination mechanism: concentrating attention solves the problem of user orientation in information abundance. Net beneficiaries who experience the mechanism as serving genuine function while extracting disproportionate value.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL ATTENTION CONSUMERS (TANGLED ROPE) — Users benefit from algorithmic curation (reduced cognitive load in information abundance) while experiencing extraction (manipulation toward engagement metrics, filter bubbles, behavioral nudging). Constrained exit — switching costs and network effects lock users into platforms despite awareness of manipulation.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EDITORIAL GATEKEEPING (PITON) — Legacy editorial review and curation (print journalism editorial boards, academic journal peer review) are increasingly performative. Algorithmic attention mechanisms have outcompeted editorial judgment; the traditional gatekeeping ritual persists through institutional inertia but with degraded functional coordination. Theater ratio elevated as editorial review maintains ritual value without controlling actual attention distribution.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN INFORMATION INFRASTRUCTURE COALITION (TANGLED ROPE) — Organized agents (decentralized social networks, open publishing platforms, community-based curation models) experience both genuine coordination function (building alternative attention mechanisms) and extraction (resource scarcity, network effects favoring incumbents, algorithm opacity). Constrained by network effects and platform lock-in despite having organizational power.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attention concentration may appear as an inevitable consequence of cognitive limits and information abundance: bounded rationality creates unavoidable need for filtering. This perspective risks naturalizing contingent technological choices (algorithmic amplification, engagement metrics, centralized platforms) as inherent to human cognition. The engine's false summit detector will identify this as naturalization requiring structural scrutiny.
constraint_indexing:constraint_classification(attention_concentration_mechanisms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_concentration_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_concentration_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_concentration_mechanisms, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_concentration_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_concentration_mechanisms, TR),
    TR >= 0.70.

:- end_tests(attention_concentration_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts value from marginal information sources by suppressing their visibility while concentrating attention on already-visible sources, generating network effects that benefit aggregators. The trajectory from 0.32 to 0.58 reflects progressive coupling with engagement metrics and advertiser targeting, which have amplified the extraction component beyond initial curation function. Suppression (0.62): High. Algorithmic ranking creates systematic barriers to visibility for niche, specialized, or contrarian information sources. Users lack transparency into ranking mechanisms and face cognitive costs of seeking alternatives. Suppression operates through platform design (opaque algorithms), economic mechanisms (engagement metrics reward concentrated attention), and behavioral mechanisms (users internalize algorithmic selection as authoritative). Theater ratio (0.68): High and rising. Traditional editorial review (institutional authority over information quality) persists as performative ritual while algorithmic curation controls actual attention distribution. Editorial gatekeeping maintains legitimacy narrative ('professional journalism,' 'peer review') despite algorithmic systems making actual distribution decisions. The trajectory from 0.42 to 0.68 shows increasing theater as the gap widens between editorial authority claims and algorithmic control reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence between beneficiary and victim positions. Aggregators perceive a coordination mechanism solving genuine orientation problems — users need filtering, curation provides value. Victims perceive pure extraction — their content is suppressed by design regardless of quality, and the suppression persists despite algorithmic improvements. This gap is not merely disagreement about value but fundamental disagreement about function: is the mechanism primarily coordinating information or primarily extracting attention from marginal sources? The tangled_rope classification represents this hybrid: genuine coordination function coexists with asymmetric extraction of attention and behavioral data from users and content creators.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's power level, exit options, and beneficiary/victim relationship to the concentration mechanism. Attention aggregators have low d (beneficiaries with arbitrage exit — can switch targeting without leaving the attention extraction business). Marginal sources have high d (victims with trapped exit — cannot escape algorithmic suppression without abandoning their epistemic domain). Individual users have moderate d (both benefit from curation and experience extraction, with constrained exit through network effects). The analytical observer derives d from spectatorial position (analytical/analytical typically d ≈ 0.72) but risks identity-locking into a naturalization frame that prevents seeing the extractive structure. The open coalition has moderate d (organized agents with constrained exit — they have power to build alternatives but face network effects favoring incumbents).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that attention concentration mechanisms exhibit all six DR types but with critical structural distinctions: (1) Beneficiaries (aggregators) experience coordination and correctly perceive it (Rope). (2) Victims experience pure extraction given no exit option (Snare). (3) Mixed agents experience the hybrid (Tangled Rope). (4) Degraded editorial authority persists as theater (Piton). (5) Open alternatives propose a sunset clause mechanism (Scaffold). (6) The analytical observer risks false summit (Mountain) by naturalizing contingent design choices. The resolution requires recognizing that the same constraint has different ε values depending on whether you measure its coordination function (lower ε, more rope-like) or its extraction function (higher ε, more snare-like). These are not two different constraints — they are two inextricably coupled functions of the same mechanism. Tangled_rope is the only type that captures this hybrid nature without collapsing to false purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_vs_inherent_concentration,
    'Is attention concentration an inherent property of human cognitive limits or a contingent amplification engineered by algorithmic ranking systems?',
    'Comparative analysis: attention distribution patterns pre-algorithm (broadcast media, print publishing) vs post-algorithm (social media, search); cross-cultural data on attention concentration in societies with different technological stacks',
    'If inherent: constraint may be closer to mountain (unavoidable). If engineered: constraint is clearly snare/tangled_rope (contingent, reversible, extractive). This determines whether alternative attention architectures are possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_vs_inherent_concentration, empirical, 'Whether attention concentration is cognitive limit or algorithmic design').

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the attention concentration mechanism''s function is genuine coordination (helping users navigate information) vs extraction (capturing engagement for metrics/revenue)?',
    'User welfare analysis: does algorithmic curation improve user decision-making quality, or does it optimize for engagement metrics that correlate with user manipulation? Comparison of information quality consumed before/after algorithmic feed adoption.',
    'If primarily coordination: tangled_rope classification confirmed with high coordination component. If primarily extraction: snare/piton classification more accurate, coordination function is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary between coordination and extraction in attention mechanisms').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of marginal information sources structural (platform design blocking visibility) or internalized (users trust algorithmic ranking and cease seeking alternatives)?',
    'Behavioral tracking: do users actively search beyond algorithmic feed recommendations, or do they accept algorithmic selection as authoritative? Post-platform-exit suppression persistence.',
    'If structural: suppression persists if alternatives exist. If internalized: even after switching platforms, users may not recover diverse information-seeking behavior. Affects whether open alternatives can succeed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in users').

omega_variable(
    decentralized_alternative_viability,
    'Can decentralized or user-controlled curation systems achieve sufficient coordination function to displace centralized algorithmic attention concentration?',
    'Network adoption analysis: comparison of information diversity, decision quality, and user satisfaction in decentralized networks (Mastodon, Bluesky, jury-based curation) vs centralized platforms over 3-5 year windows',
    'If viable: scaffold perspective is structural (sunset clause is real). If not viable: decentralized alternative is aspirational rather than actual mechanism, and concentration mechanism has higher resistance to displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_alternative_viability, empirical, 'Whether decentralized curation can replace centralized attention concentration').

omega_variable(
    multi_platform_attention_arbitrage,
    'Can users achieve meaningful attention diversification through multi-platform engagement without incurring prohibitive cognitive cost?',
    'User behavior studies: what fraction of users actively consume from multiple algorithmic feeds with different curators? Time/cognitive cost analysis for maintaining diverse information sources.',
    'If achievable: constrains effective suppression (users have viable mobile exit). If not: suppression is higher than measured (mobility is theoretical rather than practical due to cognitive cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_platform_attention_arbitrage, empirical, 'Whether multi-platform attention arbitrage is cognitively feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_concentration_mechanisms, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_concentration_mechanisms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(attn_tr_t8, attention_concentration_mechanisms, theater_ratio, 8, 0.58).
narrative_ontology:measurement(attn_tr_t16, attention_concentration_mechanisms, theater_ratio, 16, 0.68).
narrative_ontology:measurement(attn_tr_t4, attention_concentration_mechanisms, theater_ratio, 4, 0.5).
narrative_ontology:measurement(attn_tr_t12, attention_concentration_mechanisms, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_concentration_mechanisms, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(attn_be_t8, attention_concentration_mechanisms, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(attn_be_t16, attention_concentration_mechanisms, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(attn_be_t4, attention_concentration_mechanisms, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(attn_be_t12, attention_concentration_mechanisms, base_extractiveness, 12, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_concentration_mechanisms, information_standard).
narrative_ontology:boltzmann_floor_override(attention_concentration_mechanisms, 0.12).
narrative_ontology:affects_constraint(attention_concentration_mechanisms, algorithmic_opacity_mechanisms).
narrative_ontology:affects_constraint(attention_concentration_mechanisms, engagement_metric_distortion).
narrative_ontology:affects_constraint(attention_concentration_mechanisms, network_effect_lock_in).

% DUAL FORMULATION NOTE:
% Attention concentration mechanisms decompose into three structurally distinct constraints: algorithmic opacity (users cannot audit ranking criteria), engagement metric distortion (optimization for engagement creates misalignment with information quality), and network effect lock-in (users trapped by coordinated switching costs). Each has different ε values and failure modes. This story represents the aggregate constraint; the three components can be modeled separately for finer structural analysis. Affects downstream constraints: filter bubble formation depends on attention concentration; misinformation amplification correlates with concentration mechanisms; epistemically-motivated polarization couples to concentrated attention distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_concentration_mechanisms, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
