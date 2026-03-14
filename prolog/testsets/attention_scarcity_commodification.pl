% ============================================================================
% CONSTRAINT STORY: attention_scarcity_commodification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_scarcity_commodification, []).

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
 *   constraint_id: attention_scarcity_commodification
 *   human_readable: Attention Scarcity Commodification
 *   domain: digital_economy/cognitive/social
 *
 * SUMMARY:
 *   Attention scarcity commodification is the process by which human
 *   attentional capacity — inherently finite and distributively shared — has
 *   been converted into a privatized, extractable commodity through digital
 *   platforms. The constraint creates a structural tension between the
 *   genuine coordination benefits of content distribution platforms
 *   (connecting creators to audiences, users to information, advertisers to
 *   consumers) and the extractive mechanisms that platforms deploy to
 *   maximize engagement and monetize attention as sellable inventory. The
 *   core mechanism is algorithmic: ranking systems optimized for user
 *   engagement (time-on-platform, click-through) rather than user interest,
 *   information quality, or cognitive wellbeing. This creates asymmetric
 *   information flows where users are attention sources, creators are content
 *   suppliers, advertisers are revenue sources, and platforms are
 *   intermediaries capturing surplus through behavioral prediction and
 *   targeting. The constraint exhibits all six classification types depending
 *   on the observer's structural position: immutable natural law (mountain,
 *   from the view that attention is inherently scarce), pure coordination
 *   (rope, from platforms and advertisers), mixed coordination-extraction
 *   (tangled rope, from creators and regulators), pure extraction (snare,
 *   from attention bearers with no exit), temporary problem being solved
 *   (scaffold, from regulators building countervailing institutions), and
 *   degraded ritual (piton, from institutional ideology that naturalizes
 *   extraction as neutral optimization).
 *
 * KEY AGENTS:
 *   - Attention Bearers: Primary victims (powerless/trapped) — individuals whose attentional capacity is extracted through algorithmic optimization; face high exit costs (social isolation, economic penalties, information access loss)
 *   - Content Creators: Secondary victims (moderate/constrained) — benefit from platform distribution but face algorithmic suppression, engagement farming, and economic dependency; constrained exit options
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture surplus through attention arbitrage; experience constraint as coordination function with high exit options
 *   - Advertisers: Secondary beneficiaries (institutional/arbitrage) — gain access to targeted attention and behavioral prediction; minimal extraction cost
 *   - Regulatory Coalition: Organized countervailing agents (organized/constrained) — recognize extraction mechanisms and mobilize around attention rights, algorithmic transparency, data portability; face political capture and institutional constraints
 *   - Attention Economics Ideology: Institutional actor (institutional/arbitrage) — the performative language and metrics that naturalize extraction as technical optimization; persists through path dependence despite acknowledged pathologies
 *   - Information Ecology: Implicit victim (powerless/trapped) — abstract collective good (epistemic commons) that bears cost of attention polarization and misinformation persistence; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_scarcity_commodification, 0.58).
domain_priors:suppression_score(attention_scarcity_commodification, 0.65).
domain_priors:theater_ratio(attention_scarcity_commodification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_scarcity_commodification, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_scarcity_commodification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(attention_scarcity_commodification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_scarcity_commodification, tangled_rope).
narrative_ontology:human_readable(attention_scarcity_commodification, "Attention Scarcity Commodification").
narrative_ontology:topic_domain(attention_scarcity_commodification, "digital_economy/cognitive/social").

domain_priors:requires_active_enforcement(attention_scarcity_commodification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_scarcity_commodification, platform_operators).
narrative_ontology:constraint_beneficiary(attention_scarcity_commodification, advertisers).
narrative_ontology:constraint_beneficiary(attention_scarcity_commodification, content_creators_at_scale).
narrative_ontology:constraint_victim(attention_scarcity_commodification, attention_bearers).
narrative_ontology:constraint_victim(attention_scarcity_commodification, cognitive_commons).
narrative_ontology:constraint_victim(attention_scarcity_commodification, information_ecology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION BEARER (SNARE) — Individual users cannot exit attention platforms without severe social and economic penalties (isolation, employment impact, information access loss). Their attentional capacity is systematically extracted via algorithmic optimization for engagement, behavioral prediction, and advertising targeting. No meaningful alternatives exist. Maximum suppression and extraction experienced by this agent.
constraint_indexing:constraint_classification(attention_scarcity_commodification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTENT CREATOR (TANGLED ROPE) — Derives genuine benefit from platform coordination (audience access, distribution infrastructure, payment mechanisms) but simultaneously bears extraction through algorithmic ranking manipulation, unpaid labor farming, and algorithmic suppression of content that doesn't maximize engagement. Faces high costs to exit (platform switching, audience fragmentation) but maintains some agency through content strategy and multi-platform presence.
constraint_indexing:constraint_classification(attention_scarcity_commodification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences attention commodification as a genuine coordination problem: connecting users to content, creators to audiences, and advertisers to targets. Views the constraint as enabling cooperative problem-solving (matching attention with content). Net beneficiary with exit options (can shift to alternative business models, can arbitrage user switching costs). Experiences effective extraction as negative or zero.
constraint_indexing:constraint_classification(attention_scarcity_commodification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER (ROPE) — Benefits from coordinated attention allocation (precision targeting, behavioral prediction, conversion optimization). Experiences the constraint as a pure coordination mechanism connecting their messages with receptive audiences. Faces minimal suppression and experiences effective extraction as coordination benefit rather than extraction cost.
constraint_indexing:constraint_classification(attention_scarcity_commodification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — Organized agents (regulators, civil society, privacy advocates) recognize both genuine coordination functions (content distribution, creator monetization) and extractive mechanisms (behavioral manipulation, attention farming, algorithmic oppression). See the constraint as modifiable through enforcement: data portability rights, algorithmic transparency, attention limits. Constrained by institutional capacity and political capture but mobilized by public health and autonomy concerns.
constraint_indexing:constraint_classification(attention_scarcity_commodification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ATTENTION ECONOMICS IDEOLOGY (PITON) — The language of 'user engagement,' 'attention metrics,' and 'optimization' has become performative institutional theater. Platforms frame attention extraction as neutral technical problem-solving ('we're connecting users with what they want'), yet the actual function has degraded: algorithmic optimization no longer reliably serves user or content quality but increasingly serves advertiser and platform revenue. The ideology persists despite acknowledged pathologies (mental health crisis, epistemic pollution, polarization) because the institutional machinery remains profitable and has created path dependencies.
constraint_indexing:constraint_classification(attention_scarcity_commodification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, attention is inherently scarce: human consciousness has fixed bandwidth, and any system for allocating attention among competing claims must make tradeoffs. This perspective sees attention scarcity as an immutable fact of neurology and epistemology. However, the structural data reveals this as a false summit: the *commodification* of attention is contingent and institutional (advertising-driven business models, algorithmic ranking, behavioral prediction), not a law of attention itself. Natural abundance (shared attention through collective sense-making) has been replaced by artificial scarcity (individualized feeds optimizing for engagement) to enable extraction. The mountain classification naturalizes what is actually a designed institutional arrangement.
constraint_indexing:constraint_classification(attention_scarcity_commodification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_scarcity_commodification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_scarcity_commodification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_scarcity_commodification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_scarcity_commodification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_scarcity_commodification, TR),
    TR >= 0.70.

:- end_tests(attention_scarcity_commodification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts attention value from bearers through engagement optimization, behavioral prediction, and advertising targeting. Extraction is asymmetric — platforms capture attentional surplus while users experience cognitive friction (infinite scroll, notification loops, algorithmic feed addiction). The value is not zero but substantial and increasing over the measurement interval (0.35 → 0.58), indicating rent-seeking escalation as competition for attention intensifies. Not at maximum extraction (>0.70) because some users derive genuine value from platform coordination (content discovery, creative outlets, social connection), and some creators capture meaningful economic returns. Suppression (0.65): High. Barriers to exit are substantial: switching costs (audience fragmentation for creators, social network effects for users), economic dependency (creator income concentrated on platforms), technological lock-in (data portability barriers, algorithmic ranking opacity), and internalized habituation (behavioral conditioning from engagement optimization). Suppression is enforced both structurally (institutional switching costs) and through internalized patterns (identity fusion with platform presence, attention conditioning). Theater ratio (0.58): Moderate-high and increasing. Platform rhetoric around 'user experience optimization,' 'personalized recommendations,' and 'algorithmic fairness' is increasingly performative. The actual function has decoupled from stated intent: engagement optimization diverges from user interest, algorithmic transparency remains minimal despite regulatory pressure, and 'fairness' metrics mask extraction mechanics. Theater ratio growth (0.35 → 0.58) reflects widening gap between institutional narrative and structural reality as pathologies become public.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is unusually large because beneficiaries and victims occupy completely different epistemological positions. Platforms experience attention commodity as neutral mechanism ('we're connecting users with content'); users experience it as behavioral capture ('I cannot control my attention'). The gap is not just in metrics but in perceived mutability: platforms see the constraint as unchangeable (market competition forces engagement optimization) and therefore classify it as mountain; users see it as changeable in principle but insurmountable in practice (trapped → immediate perception of mountain, but could be rope at biographical horizon if identity lock breaks). The analytical mountain classification reveals the danger of naturalizing institutional arrangements: treating attention commodification as immutable scarcity rather than contingent extraction enables the constraint to persist unchallenged.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from structural relationship and exit options. Attention bearers (victims + trapped) derive high d → high f(d) → experience maximum χ. Content creators (victims + constrained) derive moderate-high d; face high exit costs but maintain some agency through multi-platform strategy and content selection → moderate χ. Platform operators (beneficiaries + arbitrage) derive low d → low/negative f(d) → experience coordination benefit. Advertisers (beneficiaries + arbitrage) derive low d → coordination benefit without extraction perception. Regulators (organized + constrained) derive moderate d; have institutional agency but face political capture and incumbent resistance → moderate χ. Information ecology (implicit victim + trapped) derives maximum d but lacks agency to organize; cannot be represented as institutional actor with power or exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Attention scarcity commodification resolves the mandatrophy by disambiguating the natural scarcity (attention is finite) from the artificial scarcity (attention is commodified and private). The mountain perspective claims that attention scarcity is immutable, implying that commodification is not an extractive choice but an inevitable consequence of allocation under scarcity. This is false. The coordinate analysis shows that alternative coordination mechanisms exist (nonprofit platforms, RSS aggregators, non-algorithmic bulletin boards) that allocate scarce attention without commodification and without extraction. The constraint is therefore not a mountain but a tangled rope: genuine coordination function (connecting creators and audiences) bundled with extractive mechanisms (engagement farming, behavioral targeting, suppression of alternatives). The mandatrophy resolution requires separating the coordination problem (how do we match attention with content?) from the extraction mechanism (how do we capture surplus attention and monetize it?) and showing that the former does not necessitate the latter. This enables classification of decentralized, non-profit, or algorithm-free alternatives as higher-coordination, lower-extraction solutions to the same coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_intent_ambiguity,
    'Is algorithmic ranking optimization primarily a coordination mechanism (matching attention with relevance) or an extraction mechanism (maximizing engagement and ad exposure)?',
    'Comparative algorithmic audit: measure engagement-optimized ranking against relevance-optimized ranking; track user satisfaction, information quality, behavioral outcome differences. Examine internal platform documentation of optimization targets and objectives.',
    'If coordination dominant: classification shifts toward Rope across perspectives. If extraction dominant: classification shifts toward Snare/Tangled Rope. If mixed but measurably asymmetric: Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_intent_ambiguity, empirical, 'Whether algorithmic ranking serves coordination or extraction').

omega_variable(
    cognitive_addiction_mechanism,
    'Is suppression of exit primarily structural (economic/social barriers) or internalized (behavioral conditioning, habit formation, identity fusion with platforms)?',
    'Post-exit cognitive patterns: tracking users who cease platform use (voluntary or forced); measuring whether attention-seeking behavior shifts to alternatives (relapse patterns) or persists after substitutes are unavailable (internalization signal). Neuroimaging of platform-conditional dopamine response.',
    'If primarily structural: exit_options classification of ''trapped'' is correct. If primarily internalized: exit_options should upgrade to ''identity_locked'' (cognitive capture), changing mountain perspective to rope. If bidirectional: suppression higher than structural measure alone indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_addiction_mechanism, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    alternative_coordination_viability,
    'Do decentralized, nonprofit, or algorithm-free platforms (Mastodon, Bluesky, email lists, RSS aggregators) provide functionally equivalent coordination at lower extraction cost?',
    'Comparative platform audit: measure user satisfaction, content discoverability, creator monetization, advertiser reach across centralized extraction-optimized and decentralized low-extraction alternatives. Track adoption barriers and switching costs.',
    'If alternatives viable: extraction is rent-seeking behavior by incumbents; suppression is artificially enforced through switching costs rather than structural necessity. If alternatives fail: suppression may reflect genuine coordination cost rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether decentralized platforms can provide equivalent coordination at lower extraction').

omega_variable(
    creator_economic_dependency,
    'How many content creators are economically dependent on platform revenue (>50% income) versus using platforms as supplementary distribution?',
    'Creator surveys and income audits; longitudinal tracking of creator exit and platform switching; comparison of full-time creators on extraction-optimized vs alternative platforms.',
    'High dependency (>60%) escalates tangled_rope classification toward snare for creator perspective. Low dependency suggests more constrained exit_options than trapped classification. Affects directionality derivation for content creator agent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creator_economic_dependency, empirical, 'Creator economic dependency on platform ecosystems').

omega_variable(
    information_ecology_externality,
    'What proportion of attention scarcity commodification''s damage externalized to information ecology (polarization, epistemic collapse, epistemic pollution) is internalized by platform operators or users as a cost?',
    'Quantify downstream information quality degradation (polarization metrics, misinformation persistence, expert authority erosion) attributable to attention commodity optimization. Audit platform and user accounting of these costs (inclusion in business models, user awareness, regulatory penalty incorporation).',
    'If externalized (not internalized): extractiveness and suppression are underestimated; true chi is higher. If partially internalized: base metrics are accurate. If fully internalized: constraint might be lower type (Rope or Scaffold if corrective mechanisms exist).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_ecology_externality, empirical, 'Externalization of information ecology damage from attention commodification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_scarcity_commodification, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_scarcity_commodification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(attn_tr_t5, attention_scarcity_commodification, theater_ratio, 5, 0.48).
narrative_ontology:measurement(attn_tr_t10, attention_scarcity_commodification, theater_ratio, 10, 0.58).
narrative_ontology:measurement(attn_tr_t15, attention_scarcity_commodification, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_scarcity_commodification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, attention_scarcity_commodification, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(attn_be_t10, attention_scarcity_commodification, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(attn_be_t15, attention_scarcity_commodification, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_scarcity_commodification, resource_allocation).
narrative_ontology:boltzmann_floor_override(attention_scarcity_commodification, 0.18).
narrative_ontology:affects_constraint(attention_scarcity_commodification, information_epistemology_degradation).
narrative_ontology:affects_constraint(attention_scarcity_commodification, behavioral_addiction_conditioning).
narrative_ontology:affects_constraint(attention_scarcity_commodification, creator_economic_extraction).

% DUAL FORMULATION NOTE:
% Attention scarcity commodification decomposes into three structurally distinct constraints: (1) the allocation coordination problem (matching attention with relevant content), (2) the extraction mechanism (engagement optimization and behavioral prediction), and (3) the epistemic externality (information ecology pollution from polarization and misinformation). Each has distinct epsilon and classification. The primary story addresses the commodification of attention itself; downstream stories address specific extraction pathways and their information ecology consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_scarcity_commodification, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
