% ============================================================================
% CONSTRAINT STORY: attention_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_extraction, []).

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
 *   constraint_id: attention_extraction
 *   human_readable: Attention Extraction in Digital and Media Systems
 *   domain: media/technology/cognitive
 *
 * SUMMARY:
 *   Attention extraction describes the structural mechanism by which digital
 *   platforms and media systems capture, measure, and commodify human
 *   attention for advertiser benefit. The constraint operates across consumer
 *   platforms (social media, streaming, search), creator platforms (YouTube,
 *   TikTok, Twitch), and ad-supported media (news, podcasts). The system
 *   exhibits the full taxonomy of DR types across different perspectives:
 *   pure extraction from the user's view (snare), coordination with embedded
 *   rent from the platform's view (rope), mixed coordination and capture from
 *   organized advertisers and creators (tangled rope), and a degraded
 *   measurement apparatus that was designed to track user welfare but now
 *   optimizes for addictive design (piton). The extractiveness has risen from
 *   0.35 to 0.62 over the interval as algorithmic ranking has become
 *   increasingly sophisticated, personalizing attention capture through
 *   variable reward schedules, social comparison, and psychological exploits.
 *   Theater ratio has increased from 0.32 to 0.55 as metrics like
 *   'engagement' and 'watch time' have become ends in themselves rather than
 *   signals of user satisfaction.
 *
 * KEY AGENTS:
 *   - Attention Users: Primary victims (powerless/trapped) — structurally dependent on platforms for communication, commerce, and social participation; face algorithmic design optimized for attention capture
 *   - Content Creators: Secondary victims (moderate/constrained) — depend on platform distribution but have partial agency through content strategy and platform diversification
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture rent through attention aggregation and algorithmic ranking; experience constraint as coordination mechanism
 *   - Advertising Industry: Organized victims and beneficiaries (organized/constrained) — benefit from targeting but pay extraction rent through platform pricing and algorithmic opacity
 *   - Attention Measurement Apparatus: Institutional actor (powerful/mobile) — maintains performative metrics (engagement, watch time) that optimize for capture rather than user utility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform consolidation and attention lock-in as inherent to information systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_extraction, 0.62).
domain_priors:suppression_score(attention_extraction, 0.68).
domain_priors:theater_ratio(attention_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_extraction, extractiveness, 0.62).
narrative_ontology:constraint_metric(attention_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(attention_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_extraction, snare).
narrative_ontology:human_readable(attention_extraction, "Attention Extraction in Digital and Media Systems").
narrative_ontology:topic_domain(attention_extraction, "media/technology/cognitive").

domain_priors:requires_active_enforcement(attention_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_extraction, attention_brokers).
narrative_ontology:constraint_beneficiary(attention_extraction, advertising_platforms).
narrative_ontology:constraint_victim(attention_extraction, attention_users).
narrative_ontology:constraint_victim(attention_extraction, collective_epistemic_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION USER (SNARE) — Structurally trapped. The user cannot exit without abandoning digital participation, social connection, professional communication, and economic access. Suppression is severe: algorithmic design, variable reward schedules, social pressure, platform lock-in, and absence of viable alternatives constrain exit to materially costly withdrawal. Experiences maximum extraction — attention is harvested continuously with minimal coordination benefit.
constraint_indexing:constraint_classification(attention_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (SNARE) — Constrained rather than fully trapped. Creators depend on platform distribution for income and audience but have partial agency through content strategy, channel diversification, and migration to alternative platforms. Faces extraction through algorithmic ranking opacity, unilateral rule changes, and revenue-share asymmetries. High suppression but not total — some leverage exists through community switching costs.
constraint_indexing:constraint_classification(attention_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination. Attention aggregation solves the matching problem: connecting creators with audiences, advertisers with targets, users with relevant content. The platform extracts rent through this coordinative function. Net beneficiary — the extraction flow runs toward this agent. Exit options are arbitrage: can move capital, redeploy infrastructure, pivot business models with minimal cost.
constraint_indexing:constraint_classification(attention_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY (TANGLED ROPE) — Organized actors (brands, agencies, advertisers) benefit from attention-targeting coordination that matches ads to high-intent audiences. Also bears extraction through platform-set pricing, algorithmic opacity, and vendor lock-in. Benefits from the coordination function (targeted reach) and simultaneously pays extraction rent (CPM/CPC pricing, attention hijacking for brand-hostile contexts). Medium suppression: the industry can coordinate, buy alternative channels, and develop countermeasures, but platform dominance creates constrained agency.
constraint_indexing:constraint_classification(attention_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTENTION MEASUREMENT APPARATUS (PITON) — Metrics like 'engagement,' 'watch time,' 'scroll depth,' and 'session duration' were designed to measure user experience and platform health. The apparatus is now largely performative: these metrics guide optimization toward addictive design rather than user utility. The measurement ritual persists through institutional inertia despite degraded correlation with actual user welfare. Theater ratio reflects that much platform activity is directed toward gaming these metrics rather than serving user or advertiser needs. The piton derives from high theater, not from high extraction — the system persists because no fully functional alternative metrics exist.
constraint_indexing:constraint_classification(attention_extraction, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attention extraction appears immutable: human attention is finite, systems that aggregate and direct attention create friction, and some degree of friction conversion (attention surplus captured as value) is inherent to attention markets. However, the structural data contradicts the mountain classification — suppression is institutional (algorithmic design, network effects), not physical; beneficiaries are identifiable and removable; victims have alternatives (though costly). The engine will compute this as a false summit, revealing that 'scarcity inherent to attention' naturalizes what is actually institutional lock-in.
constraint_indexing:constraint_classification(attention_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_extraction, TR),
    TR >= 0.70.

:- end_tests(attention_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising. Platforms extract attention through algorithmic ranking optimized for engagement rather than user utility, variable reward schedules that trigger dopamine responses, and architectural features (infinite scroll, autoplay, notifications) designed to maximize session time and clicks. The extraction is not total — users retain choice and agency — but the design actively works against exit. The upward trajectory reflects increasing sophistication of attention capture mechanisms. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) Technical: algorithmic opacity prevents users from understanding ranking rules; platform-specific features create switching costs. (2) Social: network effects and social connection tie users to platforms; FOMO and social comparison drive continued engagement. (3) Economic: advertising subsidizes platform access; alternatives require payment or feature trade-offs. (4) Psychological: variable reward schedules and social validation create habit formation. Exit is possible but costly. Theater ratio (0.55): Moderate. Measurement metrics (engagement, watch time, session duration) were originally designed to track user experience but are now primary optimization targets. Platforms report on these metrics as indicators of health and user satisfaction, but they increasingly correlate with addictive design rather than user welfare. The theater reflects this instrumental use of metrics as cover stories for extractive optimization.
 *
 * PERSPECTIVAL GAP:
 *   Users perceive snare (maximum extraction, no exit); platforms perceive rope (coordination with legitimate rent extraction); creators and advertisers perceive tangled rope (mixed benefit and extraction). The analytical observer risks perceiving mountain (attention scarcity is inevitable), but this naturalizes institutional choices (algorithmic ranking, network consolidation) as physical constraints. The gap between snare and rope arises from inverted directionality: the same constraint that extracts from users (snare) provides benefit to platforms (rope). The gap between these and the platform operator's experience reflects genuine perspectival difference — the operator is solving a real coordination problem (matching users to content, advertisers to audiences) but solving it through mechanisms that extract from users.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Users (victim + trapped) generate high d; platform operators (beneficiary + arbitrage) generate low d. The scope modifier σ(S) = 1.2 (global) amplifies effective extraction, raising chi from epsilon alone. The suppression metric (0.68) is unscaled — it represents structural barriers to exit independent of power or scope. The piton classification depends on theater_ratio (0.55), not on high chi — the measurement apparatus is degraded, not extractive per se.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE vs ROPE DIVERGENCE: This constraint resolves mandatrophy through perspectival decomposition. The snare classification from the user's view is not a misidentification of coordination — users genuinely do not experience coordination benefit (they would prefer less attention capture, not more efficient matching). The rope classification from the platform's view is not a misidentification of extraction — platforms genuinely are solving a coordination problem (market matching) even as they extract rent. Both are accurate from their respective structural positions. The mandatrophy dissolves when we recognize that inverted directionality produces inverted classifications: what looks like pure coordination from one side looks like pure extraction from the other. The analytical observer's mountain classification is a false summit — it naturalizes the platform arrangement as inevitable, missing that network effects are contingent on technical and regulatory design choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_agency_boundary,
    'What separates consensual engagement from addictive hijacking in attention capture?',
    'Behavioral neuroscience studies on variable reward schedules; user exit rate analysis when friction is removed; comparative metrics on ''wanted'' vs ''unintended'' usage patterns',
    'If boundary is clear and measurable: suppression is institutional design, extractiveness is high, snare classification stands. If boundary is blurred: system has genuine coordination function, classification shifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_agency_boundary, empirical, 'Boundary between user agency and addictive design in attention capture').

omega_variable(
    alternative_platform_viability,
    'Are decentralized, open-source, or non-extractive platform alternatives structurally viable at scale, or is platform consolidation inherent to network effects?',
    'Case study of alternative platforms (Mastodon, Bluesky, nonprofit models); analysis of cost structure differences; comparison of user retention and feature parity; network effect modeling',
    'If viable: trapped classification is inaccurate, user exit is constrained rather than trapped. If not viable: trapped is correct, suppression is higher than measured (includes inherent technical lock-in).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether viable non-extractive platform alternatives exist at scale').

omega_variable(
    epistemic_capacity_measurement,
    'How much of collective epistemic degradation (polarization, attention fragmentation, misinformation persistence) is caused by attention extraction mechanisms vs other factors (algorithmic ranking, scale of information flow)?',
    'Comparison of epistemic outcomes in high-extraction vs low-extraction information systems; modeling of attention distribution effects on belief formation; case studies of communities with alternative information architectures',
    'If extraction is primary driver: victims include ''collective_epistemic_capacity'' at high severity. If secondary: reclassify epistemic harm as downstream of ranking, not extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_capacity_measurement, empirical, 'Causal linkage between attention extraction and epistemic degradation').

omega_variable(
    suppression_internalization,
    'Is measured suppression (0.68) primarily structural (platform features, technical barriers, lock-in) or internalized (users have normalized the constraint and don''t perceive alternatives)?',
    'Longitudinal study of user perception shifts; analysis of exit attempts and reasons cited; comparison of suppression experience before/after platform migration or imposed breaks',
    'If structural: suppression metric is accurate. If internalized: effective suppression is higher — users would continue experiencing constraint after leaving platform, through norm adoption and identity integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree to which suppression is structural vs internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_extraction, theater_ratio, 0, 0.32).
narrative_ontology:measurement(attn_tr_t5, attention_extraction, theater_ratio, 5, 0.43).
narrative_ontology:measurement(attn_tr_t10, attention_extraction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, attention_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(attn_be_t10, attention_extraction, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_extraction, resource_allocation).
narrative_ontology:affects_constraint(attention_extraction, algorithmic_opacity).
narrative_ontology:affects_constraint(attention_extraction, network_effects_lock_in).
narrative_ontology:affects_constraint(attention_extraction, epistemic_polarization).
narrative_ontology:affects_constraint(attention_extraction, psychological_addiction_design).

% DUAL FORMULATION NOTE:
% Attention extraction decomposes into multiple structurally distinct constraints: (1) attention_extraction (this story) — the core extraction mechanism; (2) algorithmic_opacity — the opacity of ranking rules that prevents user agency (higher epsilon); (3) network_effects_lock_in — the technical switching costs (distinct epsilon); (4) epistemic_polarization — the downstream effect on collective knowledge (different epsilon, different victims). These are linked: opacity enables extraction, extraction is enabled by lock-in, both produce epistemic harm. Each has its own metrics and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
