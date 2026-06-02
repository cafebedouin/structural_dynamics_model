% ============================================================================
% CONSTRAINT STORY: facebook_content_moderation_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_facebook_content_moderation_opacity, []).

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
 *   constraint_id: facebook_content_moderation_opacity
 *   human_readable: Facebook Content Moderation Opacity and Asymmetric Enforcement
 *   domain: social_media_governance/platform_power
 *
 * SUMMARY:
 *   Facebook's content moderation system operates at the intersection of
 *   coordination and extraction. The platform must enforce community
 *   standards to maintain trust and prevent harm (genuine coordination
 *   function), but does so through opaque systems that lack transparent
 *   appeals, use asymmetric criteria across political factions, and remain
 *   invisible to external audit. This opacity enables Meta to calibrate
 *   enforcement to serve state interests, advertiser preferences, and
 *   internal political positioning while maintaining the appearance of
 *   neutral application of rules. The constraint exhibits all the signatures
 *   of tangled rope: genuine coordination benefits (the platform enables
 *   billions to communicate), asymmetric extraction (enforcement is
 *   predictable for dominant narratives, opaque and punitive for minorities),
 *   active enforcement (Meta employs thousands of moderators and develops ML
 *   systems to detect and suppress content), and high suppression (trapped
 *   creators have no exit pathway, marginalized communities face algorithmic
 *   suppression). The theater ratio has increased over time as transparency
 *   theater (the Oversight Board, transparency reports) has proliferated
 *   while actual enforcement opacity persists.
 *
 * KEY AGENTS:
 *   - Meta Corporation: Primary beneficiary (institutional/arbitrage) — controls enforcement standards, receives deference from states, maintains platform dominance through opacity
 *   - Marginalized Communities and Political Minorities: Primary victims (powerless/trapped) — face asymmetric enforcement with no recourse, trapped by network effects and algorithmic suppression
 *   - Content Creators and Independent Media: Secondary victims (powerful/constrained) — benefit from platform reach but constrained by unpredictable enforcement that can suppress content or violate editorial judgment
 *   - State Regulatory Bodies and Intelligence Agencies: Mixed beneficiary-victim (organized/constrained) — benefit from platform moderation that suppresses dissent while constrained by regulatory transparency requirements and public accountability
 *   - Transparency Advocacy Coalition: Organized agents working toward sunset (organized/constrained) — pushing regulatory transparency requirements (Digital Services Act, transparency mandates) that reduce opacity over time
 *   - Epistemic Commons: Victim (powerless/trapped) — suffers from enforcement that silences expert voices, amplifies misinformation from preferred actors, and prevents public understanding of moderation rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(facebook_content_moderation_opacity, 0.58).
domain_priors:suppression_score(facebook_content_moderation_opacity, 0.68).
domain_priors:theater_ratio(facebook_content_moderation_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(facebook_content_moderation_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(facebook_content_moderation_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(facebook_content_moderation_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(facebook_content_moderation_opacity, tangled_rope).
narrative_ontology:human_readable(facebook_content_moderation_opacity, "Facebook Content Moderation Opacity and Asymmetric Enforcement").
narrative_ontology:topic_domain(facebook_content_moderation_opacity, "social_media_governance/platform_power").

domain_priors:requires_active_enforcement(facebook_content_moderation_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(facebook_content_moderation_opacity, meta_corporation).
narrative_ontology:constraint_beneficiary(facebook_content_moderation_opacity, dominant_political_factions).
narrative_ontology:constraint_beneficiary(facebook_content_moderation_opacity, state_intelligence_agencies).
narrative_ontology:constraint_victim(facebook_content_moderation_opacity, content_creators).
narrative_ontology:constraint_victim(facebook_content_moderation_opacity, marginalized_communities).
narrative_ontology:constraint_victim(facebook_content_moderation_opacity, political_minorities).
narrative_ontology:constraint_victim(facebook_content_moderation_opacity, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CONTENT CREATOR (SNARE) — Small creators and members of political minorities face asymmetric enforcement with no recourse. Moderation decisions are opaque, appeals are rejected with generic explanations, and alternatives (other platforms) have negligible reach. Trapped by network effects and the absence of transparent enforcement criteria. Bears full cost of opacity without ability to predict or contest enforcement.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAJOR MEDIA ORGANIZATIONS (TANGLED ROPE) — Benefit from platform reach and audience access (coordination function) but constrained by unpredictable moderation that can suppress stories or amplify competitors. Coordination benefit (distribution to billions) coexists with extractive asymmetry (opaque enforcement favors certain narratives). Can partially exit (own websites) but at substantial cost to reach.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: META CORPORATION (ROPE) — Benefits from opacity itself: opaque enforcement maximizes platform control, deferential positioning relative to state and advertisers, and avoidance of antitrust scrutiny. Sees moderation opacity as pure coordination function: enforcing community standards through decentralized systems that appear to lack central control. Experiences no extraction — the constraint serves their interests.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE REGULATORY BODIES (TANGLED ROPE) — States benefit from platform moderation that suppresses dissent and enables surveillance (coordination: social order maintenance) while constrained by regulatory requirements for transparency and due process. Negotiate with Meta for preferential enforcement (suppressing certain political content, enabling state requests) while facing public accountability for not regulating platforms. Mixed benefit/extraction depending on political alignment.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSPARENCY ADVOCACY COALITION (SCAFFOLD) — Organized advocates (civil rights groups, transparency NGOs, journalist associations) see opacity as a temporary structural problem being solved through regulatory pressure (Digital Services Act, transparency mandates, lawsuit discovery). Sunset logic: as transparency requirements increase and moderation appeal mechanisms mature, the opacity-enabled extraction declines. Classified as scaffold because suppression (legal barriers to freedom of information) is declining over generational timeframe.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM NEUTRALITY FICTION (PITON) — The intellectual and regulatory framework treating platforms as neutral conduits for user speech persists despite overwhelming structural evidence that Facebook actively shapes content distribution and enforcement. This fiction is maintained through performative compliance (publishing moderation transparency reports, establishing the Oversight Board) that create theater without substantive change. High theater ratio: the Oversight Board reviews <0.1% of enforcement decisions; transparency reports use aggregated statistics that hide enforcement asymmetry. Classification is piton: the framework is degraded but persists through institutional inertia and legal precedent.
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, Facebook's content moderation opacity serves genuine coordination functions (community standard enforcement, network trust maintenance) alongside extractive asymmetries (asymmetric political enforcement, surveillance leverage, advertiser preference). The constraint cannot be classified as pure extraction because coordination benefits are real; cannot be rope because suppression and asymmetry are structural. Sees the core ambiguity: how much of the opacity is necessary technical debt, and how much is deliberate design choice?
constraint_indexing:constraint_classification(facebook_content_moderation_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(facebook_content_moderation_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(facebook_content_moderation_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(facebook_content_moderation_opacity, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(facebook_content_moderation_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(facebook_content_moderation_opacity, TR),
    TR >= 0.70.

:- end_tests(facebook_content_moderation_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Meta captures substantial private value from opacity: it enables asymmetric political leverage, state negotiation (intelligence cooperation), advertiser preference enforcement, and avoidance of regulatory scrutiny. But the extraction is not total snare-level (0.70+) because genuine coordination benefits (trust maintenance, standard enforcement) are real — the platform must actually function to be valuable. The rising trajectory (0.35 → 0.58 over interval) reflects increasing strategic use of enforcement opacity as political/commercial leverage. Suppression (0.68): High. Creators face multiple barriers: algorithmic suppression of content, demotion in feeds, account restrictions, bans with minimal recourse. Appeals are opaque, reinstatement is uncertain, and alternative platforms have negligible reach. Network effects trap marginalized communities. No transparent standards enable prediction or contestation. Theater ratio (0.65): Moderate-high. The Oversight Board, transparency reports, and public statements about content moderation create theater without substantive change — the board reviews <0.1% of decisions, transparency reports aggregate statistics that hide asymmetry, public statements claim neutrality while enforcement remains asymmetric. However, theater is not total (0.70+) because some real enforcement work happens and some transparency mechanisms function at scale (e.g., removal of demonstrable illegal content). The rising trajectory (0.42 → 0.65) reflects proliferation of transparency theater as political pressure increased without corresponding reduction in opacity.
 *
 * PERSPECTIVAL GAP:
 *   CORRECTED: The perspectival gap emerges from directionality differences. Meta sees rope (low extraction experienced); marginalized creators see snare (high extraction experienced); media organizations see tangled_rope (mixed benefit and extraction); states see tangled_rope with opposite sign (coordination benefit from suppression, constraint from transparency requirements); the analytical observer sees the full tangled_rope structure that non-beneficiaries cannot perceive because they are experiencing maximum extraction. The classification gap (snare vs rope vs tangled_rope) is not disagreement about facts — it is genuine structural difference in experienced extractiveness based on agent position.
 *
 * DIRECTIONALITY LOGIC:
 *   Meta's directionality (d ≈ 0.10) reflects institutional power + arbitrage exit: they can choose platforms, jurisdictions, and moderation strategies without meaningful cost. The sigmoid produces low effective extraction for Meta — they are a beneficiary. Marginalized communities' directionality (d ≈ 0.92) reflects powerless position + trapped exit: no alternatives with comparable reach, subject to enforcement with no recourse. The sigmoid produces maximum effective extraction — they bear full cost. Major media's directionality (d ≈ 0.58) reflects powerful position but constrained exit: they need platform reach and cannot fully exit without losing audience, subject to moderation decisions that affect revenue. The sigmoid produces moderate effective extraction — mixed coordination benefit and extraction cost. States' directionality is context-dependent: aligned states may have low d (arbitrage-like access to enforcement), opposed states may have higher d (constrained by transparency regulations). This directionality structure explains the perspectival gap: same base properties, radically different experienced extractiveness depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy by distinguishing coordination function (enforcing community standards, maintaining trust) from extraction mechanism (asymmetric enforcement, opacity, political leverage). The tangled_rope classification correctly captures both: (1) genuine coordination benefit — billions communicate through maintained platform; (2) asymmetric extraction — enforcement standards are opaque and asymmetric, benefiting dominant actors. If classified as pure snare, the analysis ignores the coordination benefits and Meta's legitimate enforcement role. If classified as pure rope, the analysis ignores the asymmetric enforcement and extractive opacity. Tangled_rope is the only classification that does not require mislabeling either the coordination or the extraction component. The analytical observer's perspective (tangled_rope) at civilizational scope confirms this: both the coordination and the extraction are structurally real, operating in the same system, and cannot be cleanly separated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_vs_design_choice,
    'Is content moderation opacity technically necessary infrastructure cost or deliberate design choice to enable asymmetric control?',
    'Comparative analysis of moderation transparency in competitors (YouTube, TikTok, X) who operate equivalent scale; audit of internal Meta communications regarding transparency implementation; correlation between periods of increased transparency and shifts in enforcement patterns',
    'If necessary: reclassify base_extractiveness downward; more of the suppression is technical debt than intentional. If deliberate: confirm current classification; opacity is extraction mechanism not infrastructure cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_vs_design_choice, empirical, 'Whether moderation opacity is technical necessity or deliberate design').

omega_variable(
    state_influence_asymmetry_magnitude,
    'What proportion of enforcement asymmetry results from state pressure versus Meta''s independent preference for suppressing political minorities?',
    'Forensic analysis of Meta''s government requests database combined with enforcement pattern correlation; interviews with Meta moderation staff and platform governance teams; comparison of state vs Meta-initiated enforcement asymmetries across jurisdictions',
    'High state influence: reclassify victims as including ''state intelligence agencies'' with shared extraction benefit, reducing victim solidarity. Low state influence: Meta appears as primary beneficiary with less external pressure justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_influence_asymmetry_magnitude, empirical, 'Proportion of enforcement asymmetry from state vs Meta preference').

omega_variable(
    oversight_board_capture_risk,
    'Does Meta''s Oversight Board function as genuine external review (reducing theater) or as theater legitimizing Meta''s enforcement (increasing theater)?',
    'Analysis of Oversight Board decision independence from Meta preferences; implementation rates of board recommendations; comparison of board member influence before/after appointment; statistical analysis of board decision correlation with Meta management positions',
    'If genuine external review: theater_ratio should decrease, classification shifts toward rope. If Meta capture: theater_ratio confirmed/increases, classification deepens as snare for creators and tangled_rope for media.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oversight_board_capture_risk, empirical, 'Whether Oversight Board functions as genuine review or capture mechanism').

omega_variable(
    network_effect_exit_cost,
    'What is the realistic exit cost (in reach/revenue/audience) for creators, media, and marginalized communities to migrate to alternative platforms?',
    'Comparative reach analysis: audience size on Facebook vs alternatives for equivalent creator types; revenue differential for creators using multi-platform strategy; historical data on platform migration success (Bluesky, Threads, Mastodon) tracking audience retention',
    'If exit cost is catastrophic (>90% reach loss): exit_options for creators should be ''trapped'' not ''constrained''; snare classification confirmed. If exit cost is manageable: reclassify as ''constrained'' with non-trivial mobility; perspectives shift upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_exit_cost, empirical, 'Realistic exit cost in reach and revenue for platform alternatives').

omega_variable(
    enforcement_asymmetry_detection,
    'Can enforcement asymmetry (favoring dominant factions, suppressing minorities) be reliably detected through comparative audits, or is it hidden below measurement noise?',
    'Large-scale auditing: submit equivalent content across political categories and track enforcement rates; network analysis of suppression patterns by creator network centrality; comparison of enforcement against moderation guidelines explicitly vs implicit political judgment',
    'If reliably detected: asymmetry is measurable and documentable; suppression quantified. If hidden in noise: suppression is higher than measured (creators cannot prove enforcement is asymmetric) — reclassify suppression upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_detection, empirical, 'Whether enforcement asymmetry is reliably detectable or hidden in noise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(facebook_content_moderation_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbmod_tr_t0, facebook_content_moderation_opacity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fbmod_tr_t5, facebook_content_moderation_opacity, theater_ratio, 5, 0.58).
narrative_ontology:measurement(fbmod_tr_t10, facebook_content_moderation_opacity, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(fbmod_be_t0, facebook_content_moderation_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fbmod_be_t5, facebook_content_moderation_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fbmod_be_t10, facebook_content_moderation_opacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(facebook_content_moderation_opacity, enforcement_mechanism).
narrative_ontology:affects_constraint(facebook_content_moderation_opacity, platform_algorithmic_amplification).
narrative_ontology:affects_constraint(facebook_content_moderation_opacity, social_media_political_polarization).
narrative_ontology:affects_constraint(facebook_content_moderation_opacity, content_creator_economic_dependency).

% DUAL FORMULATION NOTE:
% Content moderation opacity is downstream of Meta's platform design choices and business model (advertising targeting, user engagement optimization) but represents a distinct structural constraint. The upstream constraints reflect platform architecture decisions; the moderation opacity constraint reflects governance and enforcement asymmetry. These are linked: engagement optimization drives moderation decisions, which in turn shapes creator behavior and content distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(facebook_content_moderation_opacity, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
