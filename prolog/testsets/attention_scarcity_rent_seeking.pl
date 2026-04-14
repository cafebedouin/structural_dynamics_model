% ============================================================================
% CONSTRAINT STORY: attention_scarcity_rent_seeking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_scarcity_rent_seeking, []).

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
 *   constraint_id: attention_scarcity_rent_seeking
 *   human_readable: Attention Scarcity Rent Seeking
 *   domain: media/attention_economics/information_asymmetry
 *
 * SUMMARY:
 *   Attention scarcity rent seeking describes the structural extraction of
 *   human attention through algorithmic ranking, engagement maximization, and
 *   informational asymmetry between platform operators and users. The
 *   constraint exhibits a fundamental tension between the genuine
 *   coordination function platforms provide (connecting creators to
 *   audiences, reducing search costs for information) and the extractive
 *   mechanisms embedded in algorithmic curation (engagement-maximization
 *   driving sensationalism, recommendation loops creating filter bubbles,
 *   opaque ranking criteria distributing visibility rewards to extracted
 *   attention). The theater ratio (0.68) reflects that attention platforms
 *   combine real information distribution with substantial performative
 *   elements: engagement metrics that incentivize inauthentic behavior, viral
 *   mechanics that reward emotional manipulation over epistemic value, and
 *   algorithmic displays of 'trending' content that are largely constructed
 *   rather than emergent. The extractiveness trajectory (0.35 → 0.58) shows
 *   extraction accumulating as platforms mature: early phases emphasize
 *   coordination (connecting users to content), later phases add extraction
 *   layers (engagement maximization, algorithmic nudging, data harvesting).
 *   The suppression (0.62) reflects multiple barrier mechanisms: technical
 *   barriers (alternative platforms lack network effects), cognitive barriers
 *   (users internalize platform-as-default), and economic barriers (creators
 *   depend on platform reach).
 *
 * KEY AGENTS:
 *   - Information Seekers: Primary victims (powerless/trapped) — cannot exit attention scarcity; perceive platform attention as inevitable
 *   - Marginal Creators: Secondary victims (moderate/constrained) — depend on platform distribution; face algorithmic extraction of visibility; can exit platforms but lose audience
 *   - Attention Concentrators: Primary beneficiaries (institutional/arbitrage) — celebrities, major brands, coordinated campaigns; benefit from platform reach and visibility ranking advantages
 *   - Attention Regulation Coalition: Organized agents (organized/constrained) — digital literacy advocates, creator unions, platform regulation advocates; building alternative pathways and technical/policy sunset mechanisms
 *   - Algorithmic Platforms: Institutional beneficiary (institutional/arbitrage) — capture attention arbitrage between users and advertisers/content partners; operate the extraction mechanism
 *   - Advertising Industry: Secondary institutional actor (institutional/arbitrage) — traditional beneficiary but degraded to piton status as direct targeting reduces platform coordination role
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing attention extraction as inevitable property of cognition rather than contingent design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_scarcity_rent_seeking, 0.58).
domain_priors:suppression_score(attention_scarcity_rent_seeking, 0.62).
domain_priors:theater_ratio(attention_scarcity_rent_seeking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_scarcity_rent_seeking, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_scarcity_rent_seeking, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(attention_scarcity_rent_seeking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_scarcity_rent_seeking, tangled_rope).
narrative_ontology:human_readable(attention_scarcity_rent_seeking, "Attention Scarcity Rent Seeking").
narrative_ontology:topic_domain(attention_scarcity_rent_seeking, "media/attention_economics/information_asymmetry").

domain_priors:requires_active_enforcement(attention_scarcity_rent_seeking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_scarcity_rent_seeking, attention_concentrators).
narrative_ontology:constraint_beneficiary(attention_scarcity_rent_seeking, algorithmic_platforms).
narrative_ontology:constraint_victim(attention_scarcity_rent_seeking, information_seekers).
narrative_ontology:constraint_victim(attention_scarcity_rent_seeking, epistemic_commons).
narrative_ontology:constraint_victim(attention_scarcity_rent_seeking, marginal_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION SEEKER (SNARE) — Trapped in attention scarcity. Must navigate platforms designed to extract attention allocation decisions. No viable exit: alternative information sources require the same attention investment, and the platforms monopolize access routes. Suppression is total — the user perceives the constraint as the natural state of media, not as an extractive architecture.
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINAL CREATOR (TANGLED ROPE) — Benefits from platform distribution (genuine coordination function) but faces algorithmic extraction of visibility. Must produce higher-quality or more sensational content to achieve equivalent reach. Constrained exit: can leave the platform but lose access to audience. Mixed experience: real coordination value and real extraction.
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ATTENTION CONCENTRATOR (ROPE) — Institutional beneficiary (celebrities, major brands, coordinated campaigns). Experiences constraint as pure coordination mechanism: platforms solve the problem of reaching target audiences efficiently. Arbitrage exit available: can shift between platforms or build direct audience relationships. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ATTENTION REGULATION COALITION (SCAFFOLD) — Organized agents (digital literacy initiatives, content creator unions, platform regulation movements, attention-limiting technologies) view attention extraction as a temporary institutional failure with policy/technical sunset pathways. See algorithmic transparency, creator funds, and federated alternatives as reducing the extraction mechanism. Low effective extraction because the coalition perceives and builds toward exit.
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISING INDUSTRY (PITON) — Once the primary function of attention platforms (connecting advertisers to audiences), advertising has degraded to a secondary theater. Attention extraction now runs through engagement maximization, recommendation algorithms, and data valuation rather than traditional ad placement. The industry maintains its attachment to attention platforms through institutional inertia despite declining efficacy — direct targeting and programmatic ad networks reduce the platforms' coordination role, yet the platforms remain central. High theater ratio (ritualized metrics, viewability fraud, bot traffic).
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, attention scarcity is a fundamental property of human cognition: humans have bounded attention, and allocation of scarce attention is inherent to information processing. No escape from zero-sum attention competition. However, the structural data reveals this as a false summit: the severity of modern attention extraction depends on algorithmic amplification, institutional concentration, and suppression of alternatives — all contingent design choices, not laws of cognition.
constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_scarcity_rent_seeking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_scarcity_rent_seeking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_scarcity_rent_seeking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_scarcity_rent_seeking, TR),
    TR >= 0.70.

:- end_tests(attention_scarcity_rent_seeking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The primary extraction mechanism is the asymmetry between platform knowledge of user attention allocation (complete visibility) and user knowledge of algorithmic ranking (opaque). This asymmetry enables platforms to extract attention surplus through engagement maximization and visibility ranking without transparency. The value reflects that extraction is real and systematic but not maximal — users can still find content they value, and many users accept the tradeoff. Suppression (0.62): High. Multiple layers of suppression operate: technical (alternative platforms have inferior network effects), economic (creators depend on platform reach for income), cognitive (platforms are seen as inevitable default), and institutional (regulatory frameworks treat platform attention distribution as natural property rights rather than contingent policy). Suppression is not total — some users successfully migrate, some creators build independent audiences — but barriers are substantial. Theater ratio (0.68): Moderately high. Attention platforms combine real distribution function (creators reach audiences, users find information) with substantial performance theater: engagement metrics are gamed, viral content is often artificially amplified, trending sections are algorithmically constructed rather than emergent, and success metrics (views, engagement) become divorced from epistemic or artistic value. The theater has increased over time as engagement maximization has become more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates core perspectival divergence. Information seekers see snare: they are trapped in attention scarcity with no alternatives. Marginal creators see tangled rope: real platform benefits (audience reach) mixed with real extraction (algorithmic suppression of visibility). Attention concentrators see rope: platforms solve the pure coordination problem of reaching target audiences. The regulation coalition sees scaffold: the constraint is temporary, with clear policy/technical sunset pathways (algorithmic transparency, creator compensation, federated alternatives). The advertising industry sees piton: their original function (connecting advertisers to users) has degraded, but they maintain institutional presence through inertia. The analytical observer risks seeing mountain: attention scarcity is inherent to cognition, extraction is inevitable. The structural data reveals the mountain as a false summit — the magnitude of modern attention extraction depends entirely on algorithmic architecture, platform incentives, and regulatory choices, not on the fact that attention is scarce.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent. Information seekers as powerless/trapped agents experience high d (close to 1.0), meaning they bear extraction asymmetrically. Marginal creators as moderate/constrained agents experience moderate-high d (0.65-0.75) — they have some exit options but face significant costs. Attention concentrators as institutional/arbitrage agents experience low d (0.10-0.20) — they are beneficiaries with exit options. The platform itself (the constraint operator) experiences negative d (~0.05) — it is the primary beneficiary. The scaffold perspective (organized/constrained) has differentiated d based on coalition power: as regulation and alternatives mature, d decreases because exit options improve. This explains why the scaffold sees lower effective extraction (χ) despite the same base ε — the coalition's greater power and visible exit paths reduce the experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'attention allocation' can be genuinely coordinated (connecting creators to audiences solves a real problem) while simultaneously enabling extraction (asymmetric knowledge and opaque ranking enable rent seeking). The tangled rope classification captures this: the coordination function is real, not a cover story for extraction, but the constraint also manifests extractive dynamics. The constraint avoids mandatrophy by declaring both beneficiaries (attention concentrators benefit from algorithmic amplification) and victims (information seekers and marginal creators bear extraction costs). The distinction between coordination and extraction is not whether attention is allocated, but whether the allocation is transparent, user-controlled, and reciprocal. Platform algorithmic ranking is extraction precisely because users do not know how it works and cannot control it, even though the platforms genuinely coordinate information distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_extraction_vs_coordination_boundary,
    'At what point does attention allocation become extraction rather than coordination?',
    'User satisfaction metrics, time-on-platform vs time-wasted, cognitive load assessment, comparison to user-controlled vs algorithm-controlled feed interfaces',
    'If user derives high value: constraint is primarily Rope (coordination). If user perceives manipulation: constraint is primarily Snare (extraction). The boundary determines whether platforms are public goods or rent-extraction systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_extraction_vs_coordination_boundary, empirical, 'Boundary between attention coordination and attention extraction').

omega_variable(
    algorithmic_suppression_internalization,
    'Is the suppression of attention alternatives structural (technical barriers to discovery) or internalized (users believe platform attention is the default)?',
    'User behavior on federated, algorithm-free, or attention-limited alternatives; switching costs when suppression is removed; preference reversals after exposure to non-extractive interfaces',
    'If structural: exit barriers are real and tangible (increase experienced extraction). If internalized: users carry the suppression cognitively even after technical barriers are removed (suggest identity_locked exit modulation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_suppression_internalization, empirical, 'Structural vs internalized suppression of attention alternatives').

omega_variable(
    rent_source_sustainability,
    'Is attention rent sustainable through advertiser dependence or through direct monetization (subscriptions, content sales)?',
    'Historical analysis of platform revenue models; correlation between advertiser churn and attention extraction intensity; platform survival under ad-free models',
    'If advertiser-dependent: constraint will persist as long as advertising exists (no sunset). If direct monetization possible: constraint can be relaxed through business model transition (possible sunset to Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rent_source_sustainability, empirical, 'Sustainability of attention rent through different monetization mechanisms').

omega_variable(
    collective_action_threshold_for_creators,
    'At what creator coalition size can marginal creators negotiate algorithmic transparency or algorithmic choice from platforms?',
    'Historical precedent of creator unionization (Writers Guild AI demands, YouTube monetization disputes); threshold analysis of creator coalition power relative to platform power',
    'If threshold is low: marginal creators can organize and convert Tangled Rope to Rope (reduced extraction). If threshold is high: creators remain dispersed and subordinate (Tangled Rope persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold_for_creators, empirical, 'Coalition size threshold for creator power relative to platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_scarcity_rent_seeking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_scarcity_rent_seeking, theater_ratio, 0, 0.42).
narrative_ontology:measurement(attn_tr_t5, attention_scarcity_rent_seeking, theater_ratio, 5, 0.57).
narrative_ontology:measurement(attn_tr_t10, attention_scarcity_rent_seeking, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_scarcity_rent_seeking, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, attention_scarcity_rent_seeking, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(attn_be_t10, attention_scarcity_rent_seeking, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_scarcity_rent_seeking, information_standard).
narrative_ontology:affects_constraint(attention_scarcity_rent_seeking, algorithmic_opacity).
narrative_ontology:affects_constraint(attention_scarcity_rent_seeking, engagement_maximization_feedback_loop).
narrative_ontology:affects_constraint(attention_scarcity_rent_seeking, creator_dependency_asymmetry).
narrative_ontology:affects_constraint(attention_scarcity_rent_seeking, epistemic_commons_degradation).

% DUAL FORMULATION NOTE:
% Attention scarcity rent seeking operates at the intersection of multiple structural constraints. The upstream constraint (algorithmic opacity) enables attention extraction by hiding ranking criteria. The downstream constraints (creator dependency, epistemic degradation) are consequences of sustained attention extraction. Decomposition: algorithmic_opacity (ε=0.45, Tangled Rope: platforms coordinate user-content matching but hide ranking), engagement_maximization_feedback_loop (ε=0.62, Snare: algorithmic incentives drive sensationalism with no exit for content quality standards), creator_dependency_asymmetry (ε=0.55, Tangled Rope: platforms enable distribution but creators cannot exit), epistemic_commons_degradation (ε=0.68, Snare: attention concentration creates filter bubbles and information stratification). The attention_scarcity_rent_seeking story is the integrative constraint showing how these mechanisms compound into systemic rent seeking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_scarcity_rent_seeking, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
