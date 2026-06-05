% ============================================================================
% CONSTRAINT STORY: algorithmic_content_filtering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_content_filtering, []).

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
 *   constraint_id: algorithmic_content_filtering
 *   human_readable: Algorithmic Content Filtering in Digital Platforms
 *   domain: technology/governance/information_systems
 *
 * SUMMARY:
 *   Algorithmic content filtering on digital platforms operates as a hybrid
 *   constraint combining genuine coordination functions (abuse mitigation,
 *   platform viability) with asymmetric extraction mechanisms targeting
 *   powerless agents (shadowbanned creators, marginalized voices,
 *   identity-locked users). The constraint classifies as Tangled Rope at the
 *   analytical level: filtering requires active algorithmic enforcement,
 *   produces measurable benefits for platform operators and advertisers
 *   (coordination), yet systematically extracts from content creators and
 *   constrains information consumers through opaque, unappealable
 *   suppression. The theater ratio has increased from 0.42 to 0.68 over the
 *   measured interval, indicating rising performativity as compliance
 *   infrastructure (transparency reports, appeals processes) has proliferated
 *   without enabling genuine algorithmic scrutiny. Base extractiveness has
 *   risen 0.35 to 0.58, reflecting both algorithmic sophistication and
 *   reduced friction for platforms to apply filtering. The constraint
 *   exhibits all six classification types depending on observational
 *   position: pure extraction (Snare) from the powerless creator perspective,
 *   mixed extraction-coordination (Tangled Rope) from organized coalition and
 *   analytical perspectives, pure coordination (Rope) from platform and
 *   advertiser perspectives, degraded ritual (Piton) from compliance
 *   apparatus perspective, and identity-locked entrapment (Snare via
 *   identity_locked) from consumers whose social identity is platform-fused.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — filtering serves business objectives of engagement optimization and advertiser relationships; zero meaningful suppression experienced
 *   - Advertisers: Secondary beneficiary (powerful/arbitrage) — filtering ensures brand safety and reaches desirable audiences; pure coordination function with no extraction cost
 *   - Content Creators: Primary victim (powerless/trapped or moderate/constrained) — experience suppression without transparency or appeal; shadowbanning erodes income and reach
 *   - Marginalized Communities: Secondary victim (moderate/constrained, beneficiary of abuse filtering but also harmed by speech suppression) — experience tangled coordination-extraction mix
 *   - Information Consumers: Tertiary victim (powerless/identity_locked) — structurally mobile but identity-fused with platform; algorithmic filtering constrains information diet without awareness
 *   - Organized Creator Coalition: Moderate agent (organized/constrained) — benefit from abuse filtering but bear labor costs of documentation and collective action to contest suppression
 *   - Regulatory Bodies: Institutional actor (institutional/constrained) — maintain compliance theater; constrained by platform proprietary claims and technical complexity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees genuine coordination function intertwined with systematic extraction from powerless agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_content_filtering, 0.58).
domain_priors:suppression_score(algorithmic_content_filtering, 0.65).
domain_priors:theater_ratio(algorithmic_content_filtering, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_content_filtering, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_content_filtering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_content_filtering, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_content_filtering, tangled_rope).
narrative_ontology:human_readable(algorithmic_content_filtering, "Algorithmic Content Filtering in Digital Platforms").
narrative_ontology:topic_domain(algorithmic_content_filtering, "technology/governance/information_systems").

domain_priors:requires_active_enforcement(algorithmic_content_filtering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_content_filtering, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_content_filtering, advertisers).
narrative_ontology:constraint_victim(algorithmic_content_filtering, content_creators).
narrative_ontology:constraint_victim(algorithmic_content_filtering, information_consumers).
narrative_ontology:constraint_victim(algorithmic_content_filtering, marginalized_voices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SHADOWBANNED CONTENT CREATOR (SNARE) — No visibility into why content is filtered; no meaningful appeal mechanism; cannot exit without abandoning their audience and platform infrastructure. Complete extraction: algorithmic suppression determines their reach without transparency or recourse. No coordination benefit — pure extraction mechanism.
constraint_indexing:constraint_classification(algorithmic_content_filtering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY-LOCKED INFORMATION CONSUMER (SNARE via identity_locked) — Structurally mobile (could leave platform) but identity is fused with their social graph, community presence, and digital reputation on the platform. Exit would require becoming a different person socially. Filtering constrains their information diet without their awareness, but leaving is identity-suicide. Classification as Snare from identity_locked exit reflects that perceptual mobility does not translate to structural freedom.
constraint_indexing:constraint_classification(algorithmic_content_filtering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: MARGINALIZED COMMUNITY (TANGLED ROPE) — Genuine coordination benefit: filtering removes harassment, hate speech, and coordinated attacks. High suppression cost: filtering also silences marginalized voices, especially on politically contested topics. Constrained exit: switching platforms abandons access to community organizing infrastructure. Mixed experience — extraction and coordination intertwined.
constraint_indexing:constraint_classification(algorithmic_content_filtering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences filtering as pure coordination: managing abuse, maintaining platform health, enabling advertisers to reach 'safe' audiences. High arbitrage exit: can migrate filtering parameters and algorithms across contexts. No meaningful extraction from platform's perspective — filtering is internal cost management. Beneficiary position with zero experienced suppression.
constraint_indexing:constraint_classification(algorithmic_content_filtering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISER (ROPE) — Pure coordination: filtering ensures brand safety and reaches audiences in 'acceptable' contexts. Arbitrage exit: can adjust spending across platforms and campaigns based on filtering outcomes. Net beneficiary — filtering serves advertiser interests at no cost to advertiser.
constraint_indexing:constraint_classification(algorithmic_content_filtering, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COMPLIANCE APPARATUS (PITON) — Content moderation policies and transparency reports are largely performative: platforms publish moderation statistics and appeal procedures that appear robust but filter algorithmic logic remains proprietary and opaque. Theater ratio is high because the compliance apparatus maintains the appearance of fairness without enabling genuine algorithmic scrutiny. Institutional actors maintain the ritual because regulation requires it, not because it functionally prevents extraction.
constraint_indexing:constraint_classification(algorithmic_content_filtering, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ORGANIZED CREATOR COALITION (TANGLED ROPE) — Organized agents (creator unions, content advocacy groups) see filtering as both coordination mechanism and extraction device. Genuine coordination benefit: filtering enables platform viability and joint defense against abuse. Asymmetric extraction: algorithmic opacity and appeal barriers impose labor costs (understanding policies, documenting suppression, organizing collective action). Constrained exit: leaving the platform means losing reach and income. Active enforcement required — platforms must actively apply and update filtering rules.
constraint_indexing:constraint_classification(algorithmic_content_filtering, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational timescale, algorithmic filtering coordinates necessary platform functions (abuse management, context relevance) while extracting from those with lowest power to opt-out or influence parameters. Genuine coordination function exists alongside asymmetric extraction. Neither pure coordination (rope) nor pure extraction (snare) — the constraint genuinely does both. Active enforcement is required: filtering must be continuously applied and updated. Benef beneficiary and victim groups are clearly identifiable. Classification as Tangled Rope is stable across analytical contexts.
constraint_indexing:constraint_classification(algorithmic_content_filtering, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_content_filtering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_content_filtering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_content_filtering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_content_filtering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_content_filtering, TR),
    TR >= 0.70.

:- end_tests(algorithmic_content_filtering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint produces measurable extraction from creators (reduced visibility, shadowbanning, algorithmic suppression) and consumers (limited information access through curated feeds). The extraction is not as severe as pure algorithmic oppression would be (0.75+) because: (1) creators can theoretically optimize for algorithmic visibility, (2) some users can arbitrage between platforms, and (3) some platforms offer algorithmic control (chronological feeds, feed preferences). However, extraction is substantial because: (1) algorithmic parameters remain opaque, (2) appeal mechanisms are ineffective (per omega findings), and (3) platforms have asymmetric power to modify rules unilaterally. Suppression (0.65): High. Barriers to exit include network effects (social graphs are platform-specific), identity fusion (users' social identity is tied to platform presence), and infrastructure dependency (creators have invested in audience and tools on specific platforms). Suppression mechanisms include: (1) algorithmic opacity preventing informed challenge, (2) ineffective appeals processes creating perception of powerlessness, (3) algorithmic punishment for attempted gaming (posts explicitly testing suppression get suppressed), (4) creation cost sunk into platform-specific tools and audience relationships. Theater ratio (0.68): High. Transparency reports, moderation statistics, and appeals procedures are substantially performative: platform policies and stated objectives do not predict actual suppression outcomes (omega variable: transparency_report_effectiveness). Compliance infrastructure maintains legitimacy without enabling genuine accountability. Theater has increased over interval as platforms have professionalized compliance reporting while maintaining algorithmic opacity.
 *
 * PERSPECTIVAL GAP:
 *   Platform Operator vs. Powerless Creator: Platform sees coordination problem (how to maintain abuse-free space enabling advertiser relationships). Creator sees extraction mechanism (algorithmic suppression eroding their reach and income). Both are empirically correct — filtering does coordinate abuse mitigation, and it does extract from creators. The gap reflects genuine structural asymmetry, not measurement disagreement. Marginalized Community dual benefit/victim status: Same filtering that removes targeted harassment also silences marginalized organizing and speech. This is not a perspective gap but a genuine structural reality — the constraint simultaneously coordinates community safety and extracts community voice. The Tangled Rope classification captures this simultaneity without collapsing it into artificial clarity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Platform operators have arbitrage exit (can shift filtering rules across contexts) and benefit from filtering outcomes → low d → low χ. Creators have trapped or constrained exit (cannot easily leave without audience loss) and bear costs of suppression → high d → high χ. Identity-locked consumers have structural mobility (could switch platforms) but psychological entrapment through identity fusion → intermediate d reflecting constrained + identity_locked → intermediate χ. The sigmoid f(d) amplifies the directionality differences into experienced extractiveness differences: beneficiaries experience negative χ (subsidy), powerless victims experience χ approaching 1.42 (maximum extraction), moderate agents experience χ in 0.60-1.00 range (mixed but extraction-dominated). Scope modifiers σ(S): global scope (σ=1.2) amplifies extraction visibility because filtering affects billions of users simultaneously across geographies, making coordination failures and extraction mechanisms highly visible.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED by recognizing that the constraint is genuinely hybrid: it performs both coordination (abuse mitigation, platform viability) and extraction (creator suppression, information control). The resolution rejects false dichotomy between 'is this coordination?' and 'is this extraction?' The structure unambiguously exhibits both. What prevents mislabeling: (1) Beneficiary/victim declarations are clear — platforms and advertisers benefit, creators and marginalized voices bear costs; (2) Active enforcement is required — filtering is not emergent from user behavior but actively applied by algorithms; (3) Asymmetric extraction exists — platforms can modify filtering rules unilaterally while creators cannot contest them meaningfully; (4) Genuine coordination function exists — filtering does prevent harassment and abuse, creating positive externality for vulnerable communities. The Tangled Rope classification accepts the simultaneity: yes, there is coordination; yes, there is extraction; no, you cannot reduce one to the other. Theater ratio rising from 0.42 to 0.68 reflects constraint degradation toward Piton characteristics — as transparency infrastructure has proliferated without enabling real algorithmic contestation, performativity has increased. The Piton perspective from compliance apparatus is diagnostically important: it recognizes that transparency theater may eventually dominate the constraint if appeals remain ineffective and algorithmic opacity persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_mechanism,
    'Is suppression driven by algorithmic intent (deliberate filtering rules) or by opaque emergent properties of recommendation systems trained on engagement metrics?',
    'Direct algorithmic auditing and explainability testing; comparison of filtering outcomes when engagement-optimization is disabled vs enabled; platform disclosure of training objectives and loss functions',
    'If deliberate: constraint is intentional Snare with clear beneficiaries and victims. If emergent: constraint is unintended Piton (degraded system maintained by inertia). Classification would shift from Tangled Rope toward pure Snare if deliberate, or toward Piton if emergent without intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_mechanism, empirical, 'Whether filtering is deliberate algorithmic design or emergent from optimization objectives').

omega_variable(
    marginalized_voice_filter_asymmetry,
    'Does algorithmic filtering disproportionately suppress marginalized or politically disfavored voices compared to dominant voices, even controlling for content violating stated policies?',
    'Large-scale comparative content analysis: identical content posted from different demographic/ideological positions, tracking suppression rates; audit of false-positive rates across demographic groups; audit of appeal success rates by creator identity',
    'If asymmetric suppression exists: mechanism is extractive targeting of powerless agents (Snare features dominate). If uniform suppression: constraint is coordination mechanism with side effects (Rope features dominate). Critical for determining whether ''marginalized community'' perspective sees genuine mixed extraction or is experiencing worse-than-proportional targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_filter_asymmetry, empirical, 'Whether filtering asymmetrically targets marginalized voices beyond policy violations').

omega_variable(
    identity_lock_exit_empirical,
    'Do users classified as ''identity_locked'' actually exit platforms at lower rates than structurally similar users without identity fusion, controlling for network effects?',
    'Longitudinal user tracking: departure rates from platforms by baseline social-graph size and community engagement intensity; post-exit survey data on whether identity/community loss was cited as barrier to leaving; comparison with geographic isolation constraints (actual trapped exit)',
    'If identity_locked users depart at significantly lower rates: classification is empirically validated. If departure rates are similar to trapped users: the exit category may be overstated relative to actual behavioral constraint. If departure rates are high: identity_lock is weaker than assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_exit_empirical, empirical, 'Empirical validation of identity_locked exit category through user departure rates').

omega_variable(
    transparency_report_effectiveness,
    'Do platform transparency reports and moderation appeals processes actually enable users to understand or contest algorithmic filtering decisions, or are they primarily performative compliance theater?',
    'User survey of appeal success rates, appeal decision comprehensibility, and perceived fairness; comparison of stated filtering criteria in policies vs actual algorithmic behavior (black-box testing); analysis of whether appeals produce policy or behavioral changes',
    'If effective: theater_ratio should be lower (0.40-0.50 range); regulatory compliance apparatus moves toward genuine coordination function (Rope classification from institutional perspective). If performative: theater_ratio confirmed at 0.68+; Piton classification validated; compliance apparatus is maintenance of false legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_report_effectiveness, empirical, 'Whether transparency and appeals procedures are functionally effective or performative').

omega_variable(
    extraction_beneficiary_intent,
    'Do platforms deliberately design filtering to extract advertiser surplus and creator lock-in, or do they optimize for engagement/safety metrics that produce extraction as unintended consequence?',
    'Historical analysis of platform feature releases and algorithm changes; correlation with revenue metrics and advertiser satisfaction; internal documentation (leaked memos, litigation discovery, regulatory filings); comparison of filtering outcomes with platform business model incentives',
    'If deliberate extraction design: Snare characteristics dominate, beneficiary intent is clear. If unintended: constraint approaches Piton (emergence + inertia) or confused Tangled Rope (coordination function exists but extraction is byproduct). This affects mandatrophy resolution — deliberate extraction requires stronger remedies; unintended extraction might be addressed by changing optimization targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_intent, conceptual, 'Whether filtering extraction is deliberate design or unintended consequence of engagement optimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_content_filtering, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acf_tr_t0, algorithmic_content_filtering, theater_ratio, 0, 0.42).
narrative_ontology:measurement(acf_tr_t3, algorithmic_content_filtering, theater_ratio, 3, 0.55).
narrative_ontology:measurement(acf_tr_t6, algorithmic_content_filtering, theater_ratio, 6, 0.64).
narrative_ontology:measurement(acf_tr_t9, algorithmic_content_filtering, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(acf_be_t0, algorithmic_content_filtering, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acf_be_t3, algorithmic_content_filtering, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(acf_be_t6, algorithmic_content_filtering, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(acf_be_t9, algorithmic_content_filtering, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_content_filtering, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_content_filtering, platform_engagement_optimization).
narrative_ontology:affects_constraint(algorithmic_content_filtering, digital_attention_markets).
narrative_ontology:affects_constraint(algorithmic_content_filtering, content_moderation_labor).

% DUAL FORMULATION NOTE:
% Algorithmic content filtering decomposes into multiple structurally distinct constraints: (1) abuse prevention coordination mechanism (ε ≈ 0.25, Rope), (2) algorithmic opacity extraction system (ε ≈ 0.65, Snare), (3) compliance theater (ε ≈ 0.35, Piton). The present story captures the aggregate constraint at ε=0.58 (Tangled Rope) where all three mechanisms are active simultaneously. Upstream: engagement optimization algorithms that create the training data for content filters. Downstream: content moderation labor systems that enforce policies and handle appeals; platform business models that depend on advertiser relationships enabled by filtering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_content_filtering, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
