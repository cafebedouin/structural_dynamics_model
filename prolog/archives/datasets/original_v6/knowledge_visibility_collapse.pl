% ============================================================================
% CONSTRAINT STORY: knowledge_visibility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_visibility_collapse, []).

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
 *   constraint_id: knowledge_visibility_collapse
 *   human_readable: Knowledge Visibility Collapse
 *   domain: epistemic_systems/information_access
 *
 * SUMMARY:
 *   Knowledge visibility collapse describes the structural constraint wherein
 *   algorithmic curation, engagement optimization, and platform gatekeeping
 *   systematically suppress the discoverability of knowledge that falls
 *   outside high-engagement patterns. The constraint exhibits core features
 *   of a tangled rope: genuine coordination function (algorithmic filtering
 *   solves attention scarcity), asymmetric extraction (visibility benefits
 *   concentrate on high-engagement content while niche knowledge disappears),
 *   and active enforcement (algorithmic architecture maintains the visibility
 *   distribution). The constraint operates at multiple scales simultaneously:
 *   for individual information seekers (trapped in filter bubbles), for the
 *   knowledge commons (power-law visibility distribution becomes
 *   self-reinforcing), and for epistemic institutions (traditional
 *   universities and libraries lose visibility advantage). The theater ratio
 *   (0.68) reflects that much of contemporary knowledge discovery appears to
 *   operate through neutral algorithmic systems, while systematic suppression
 *   of non-monetizable knowledge operates invisibly beneath the surface of
 *   apparent accessibility. The extractiveness trajectory shows the
 *   constraint accumulating over time: initial engagement optimization
 *   (extractiveness 0.35) becomes layered with explicit attention-capture
 *   mechanisms and visibility commodification (extractiveness 0.58), while
 *   the performance of neutrality increases (theater 0.48 → 0.68) as
 *   suppression becomes more systematic and less visible.
 *
 * KEY AGENTS:
 *   - Information Seekers: Primary victims (powerless/trapped) — face visibility collapse and filter bubbles with no exit mechanism
 *   - Knowledge Commons: Primary victim (powerless/trapped) — abstract epistemic infrastructure that cannot organize or advocate
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture attention value and advertising revenue from visibility gatekeeping
 *   - Peripheral Researchers: Secondary victim (moderate/constrained) — face suppression of niche knowledge while depending on platform distribution
 *   - Traditional Epistemic Institutions: Secondary actor (institutional/constrained) — universities and journals lose structural visibility advantage to platforms
 *   - Open Knowledge Movement: Organized agents (organized/mobile) — building decentralized alternatives with sunset logic for platform dependence
 *   - Engagement Optimization Systems: Institutional mechanism (institutional/arbitrage) — technical systems that enforce visibility distribution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing platform visibility patterns as inherent epistemic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_visibility_collapse, 0.58).
domain_priors:suppression_score(knowledge_visibility_collapse, 0.62).
domain_priors:theater_ratio(knowledge_visibility_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_visibility_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_visibility_collapse, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(knowledge_visibility_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_visibility_collapse, tangled_rope).
narrative_ontology:human_readable(knowledge_visibility_collapse, "Knowledge Visibility Collapse").
narrative_ontology:topic_domain(knowledge_visibility_collapse, "epistemic_systems/information_access").

domain_priors:requires_active_enforcement(knowledge_visibility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_visibility_collapse, platform_operators).
narrative_ontology:constraint_beneficiary(knowledge_visibility_collapse, content_curators).
narrative_ontology:constraint_beneficiary(knowledge_visibility_collapse, algorithmic_gatekeepers).
narrative_ontology:constraint_victim(knowledge_visibility_collapse, knowledge_commons).
narrative_ontology:constraint_victim(knowledge_visibility_collapse, information_seekers).
narrative_ontology:constraint_victim(knowledge_visibility_collapse, peripheral_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INFORMATION SEEKER (SNARE) — Cannot exit the algorithmic filtering system without abandoning access entirely. Faces algorithmic suppression of non-trending knowledge, visibility collapse of specialized topics, and filter bubbles that reinforce existing knowledge patterns. No alternative pathways for discovering peripheral or contradictory information. Maximum extraction experienced — the seeker's epistemic autonomy is constrained by invisible algorithmic boundaries.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE KNOWLEDGE COMMONS (SNARE) — Epistemic infrastructure cannot organize or exit. Systematic suppression of non-monetizable knowledge, degradation of public scholarship discovery mechanisms, and architectural bias toward commercial content. The commons bears the cost of visibility collapse while having no advocate or escape mechanism. Abstract collective good with no exit options.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PERIPHERAL RESEARCHER (TANGLED ROPE) — Structurally constrained by visibility barriers (small institutional affiliation, non-English language, niche topics) but benefits from platform access and distribution mechanisms. Faces extraction through reduced citation rates and difficult knowledge discovery, yet also coordinates with peers through the same algorithmic systems. Moderate asymmetric extraction with genuine but constrained agency.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination mechanism: algorithmic filtering solves the attention scarcity problem by distributing visibility according to engagement metrics. Net beneficiary of visibility collapse — captures advertising value and user attention. Can exit or modify the constraint (and has arbitrage options via alternative algorithms), but optimization for engagement is the core coordination function. Low experienced extraction.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE TRADITIONAL EPISTEMIC INSTITUTION (TANGLED ROPE) — Universities, journals, libraries once controlled knowledge visibility and benefited from that structural role. Now constrained by platform algorithms that bypass traditional gatekeeping. Experiences extraction as reduced citation influence and student attention, but also benefits from platform distribution and reduced publishing costs. Institutional actor with constrained exit — cannot abandon digital platforms without losing relevance, but platforms have redefined visibility criteria.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized agents (arXiv, Wikipedia, open-access mandates, indexing projects) building alternative discovery mechanisms with sunset logic. See visibility collapse as a temporary institutional problem created by commercial optimization and view decentralized, open-protocol alternatives as pathways to exit. High agency and clear exit strategy — low experienced extraction because the coalition perceives and is constructing the dissolving constraint.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: THE SEARCH AND DISCOVERY RITUAL (PITON) — Traditional information retrieval and library science once performed genuine discovery function. Now degraded to performative compliance: search engine optimization, algorithmic theater, and engagement metrics create appearance of knowledge access while systematic suppression operates invisibly. The ritual persists through institutional inertia and user habituation despite low functional verification. Theater ratio 0.68 reflects that much visible search activity is performance rather than effective discovery.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, piton,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal scope, visibility collapse appears as an immutable consequence of information scarcity: with infinite knowledge production and finite human attention, some knowledge must remain invisible. This perspective naturalizes the constraint as inherent to epistemics itself. However, structural data contradicts this — the specific mechanisms (algorithmic optimization, engagement metrics, attention capture) are contingent institutional designs, not natural laws.
constraint_indexing:constraint_classification(knowledge_visibility_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_visibility_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_visibility_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_visibility_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_visibility_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_visibility_collapse, TR),
    TR >= 0.70.

:- end_tests(knowledge_visibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platform operators and engagement-optimizing systems capture significant value through visibility gatekeeping: advertising revenue, user attention concentration, and control over knowledge discovery pathways. The extraction is not maximal because legitimate coordination function exists (attention scarcity is real, algorithmic filtering solves a genuine problem), and information seekers do access substantial knowledge despite visibility collapse. The intermediate value reflects mixed mechanisms. Suppression (0.62): Moderate-high. Barriers to discovering non-trending knowledge include algorithmic suppression of niche content, filter bubble effects, platform-specific visibility metrics, and lack of alternative discovery infrastructure for most users. However, complete suppression does not occur — persistent searchers can find peripheral knowledge, and open mechanisms (arXiv, Wikipedia, institutional repositories) provide partial alternatives. Suppression is systematic but not total. Theater ratio (0.68): High. Contemporary knowledge discovery appears to operate through neutral, objective algorithmic systems while significant suppression occurs invisibly. The performance of 'searching the whole internet' masks systematic visibility distribution. SEO theater, algorithmic explanation theater, and 'relevant results' framing create appearance of comprehensive discovery while architecture suppresses categories of knowledge. The theater has increased as engagement optimization has become more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a dramatic perspectival gap driven by exit options and beneficiary status. The platform operator sees coordination (Rope) — algorithmic filtering genuinely solves attention scarcity and enables knowledge access at scale. The information seeker sees pure extraction (Snare) — imprisoned in filter bubbles with no escape. The peripheral researcher sees mixed coordination and extraction (Tangled Rope) — benefits from platform distribution but suffers from visibility suppression. The traditional epistemic institution sees institutional reshuffling (Tangled Rope) — once a visibility beneficiary, now constrained by platforms. The open knowledge movement sees a temporary institutional failure with exit pathway (Scaffold) — decentralized discovery mechanisms can replace platform gatekeeping. The knowledge commons sees invisible extraction (Snare) — the power-law visibility distribution is self-reinforcing and the commons has no organizing mechanism. The immediate observer of search systems sees performative discovery (Piton) — the appearance of neutral algorithmic search theater masks systematic suppression. The civilizational analyst risks seeing immutable epistemic law (Mountain) — information scarcity necessitates some knowledge invisibility — but structural data reveals this as a false summit: the specific mechanisms (engagement metrics, algorithm opacity, platform monopoly) are contingent design choices, not necessary features.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective flows from structural position relative to the visibility constraint. Information seekers (powerless/trapped) experience maximum d ≈ 0.95, experiencing nearly full extraction through complete dependence on visibility systems with no exit. The knowledge commons (powerless/trapped) similarly experiences high d — unable to organize or exit, bearing full cost of visibility suppression. Peripheral researchers (moderate/constrained) experience moderate d ≈ 0.70 — face significant visibility barriers but retain some agency through alternative publishing, citation networks, and non-platform discovery. Platform operators (institutional/arbitrage) experience low d ≈ 0.10 — net beneficiaries with arbitrage options and optimization freedom; experience the constraint as coordination rather than extraction. Traditional institutions (institutional/constrained) experience intermediate d ≈ 0.55 — constrained by platform dominance but retain institutional visibility (university affiliation still matters), benefiting from platform distribution while losing gatekeeping advantage. Open knowledge movement (organized/mobile) experiences moderate d ≈ 0.40 — constrained by platform monopolization but with clear exit pathways and organizational agency to build alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival multiplicity. The question 'Is knowledge visibility collapse extraction or coordination?' has no single answer because it depends entirely on structural position. From the platform's perspective, it is coordination (genuine function). From the trapped information seeker's perspective, it is extraction (no exit). The tangled rope classification captures the truth: both mechanisms operate simultaneously. The mandatrophy prevention mechanism is that the constraint includes genuine coordination function (algorithmic filtering solves attention scarcity) while maintaining asymmetric extraction (visibility concentration benefits high-engagement content). The classification prevents mislabeling this as pure rope (which would require suppression ≤ 0.40 and false beneficiary claims from victims) while also preventing mislabeling as pure snare (which would require zero coordination function). The intermediate extractiveness (0.58) and high suppression (0.62) are consistent with tangled rope classification. The scaffold and open knowledge movement perspectives reveal the exit pathway: decentralized alternatives are structurally possible, making the constraint potentially temporary rather than natural. The piton perspective reveals institutional inertia — users continue using platform discovery systems partly from habit, partly from lack of accessible alternatives, and partly because platforms' scale advantages are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_visibility_threshold,
    'At what point does algorithmic filtering transition from attention coordination to knowledge suppression?',
    'Comparative analysis of discoverability rates for: high-engagement content vs high-quality niche content; trending topics vs specialist knowledge; commercial vs non-monetizable information. Measurement of citation impact disparities between platform-visible and platform-obscured research.',
    'If threshold is low (early filtering is excessive): knowledge suppression mechanism is severe and classification as Snare confirmed. If threshold is high (filtering serves genuine coordination): Rope classification becomes more defensible from platform operator perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_visibility_threshold, empirical, 'Threshold between attention coordination and knowledge suppression').

omega_variable(
    alternative_discovery_sufficiency,
    'Do decentralized discovery mechanisms (federated search, open indices, cross-platform aggregation) achieve discoverability parity with platform algorithms for peripheral knowledge?',
    'Longitudinal comparison of discovery rates for niche topics across platforms and decentralized systems; measurement of citation trajectories for research discovered via alternative mechanisms vs algorithmic feeds; user studies on knowledge access patterns.',
    'If sufficient: scaffold perspective confirmed — open alternatives can provide real exit. If insufficient: peripheral researchers remain trapped regardless of platform choice, suggesting visibility collapse is more fundamental than current constraint model captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_discovery_sufficiency, empirical, 'Whether decentralized discovery mechanisms achieve platform parity').

omega_variable(
    suppression_mechanism_intentionality,
    'Is visibility collapse a deliberate extraction mechanism or an unintended consequence of engagement optimization?',
    'Platform algorithm audits; internal documentation and design discussions; comparative analysis of visibility patterns before and after algorithmic optimization cycles; measurement of platform revenue correlation with visibility suppression of non-monetizable content.',
    'If deliberate: classification as pure Snare becomes stronger. If unintended: constraint may be reclassifiable as Scaffold with sunset possibility through algorithm redesign. If mixed (some deliberate, some unintended): confirms Tangled Rope classification — extraction layered on coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_intentionality, empirical, 'Whether visibility suppression is deliberate or unintended consequence').

omega_variable(
    knowledge_commons_recovery_capacity,
    'Can the knowledge commons rebuild epistemic autonomy once visibility collapse mechanisms are identified, or is the damage to discovery habits and institutional trust permanent?',
    'Historical analysis of epistemic institution recovery after visibility crises; measurement of user behavior changes following algorithmic transparency efforts; longitudinal tracking of knowledge diversity trends post-intervention.',
    'If recoverable: visibility collapse is a Tangled Rope with possible reformation pathway. If permanent: constrains the piton and mountain perspectives — institutions cannot exit or recover even if structures change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_commons_recovery_capacity, empirical, 'Whether knowledge commons can recover epistemic autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_visibility_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kvc_tr_t0, knowledge_visibility_collapse, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kvc_tr_t3, knowledge_visibility_collapse, theater_ratio, 3, 0.55).
narrative_ontology:measurement(kvc_tr_t6, knowledge_visibility_collapse, theater_ratio, 6, 0.62).
narrative_ontology:measurement(kvc_tr_t10, knowledge_visibility_collapse, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(kvc_be_t0, knowledge_visibility_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kvc_be_t3, knowledge_visibility_collapse, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(kvc_be_t6, knowledge_visibility_collapse, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(kvc_be_t10, knowledge_visibility_collapse, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_visibility_collapse, information_standard).
narrative_ontology:affects_constraint(knowledge_visibility_collapse, filter_bubble_lock_in).
narrative_ontology:affects_constraint(knowledge_visibility_collapse, epistemic_monoculture_formation).
narrative_ontology:affects_constraint(knowledge_visibility_collapse, knowledge_commons_degradation).

% DUAL FORMULATION NOTE:
% Knowledge visibility collapse represents the macro-level constraint; it affects specific downstream constraints around filter bubbles, epistemic monoculture, and knowledge commons degradation. The visibility constraint has its own ε (0.58) reflecting the balance of coordination and extraction; downstream constraints have their own ε values reflecting specific manifestations of visibility suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_visibility_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
