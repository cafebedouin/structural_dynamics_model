% ============================================================================
% CONSTRAINT STORY: algorithmic_attention_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_attention_capture, []).

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
 *   constraint_id: algorithmic_attention_capture
 *   human_readable: Algorithmic Attention Capture in Digital Platforms
 *   domain: digital_economy/cognitive_capture
 *
 * SUMMARY:
 *   Algorithmic attention capture represents a structural constraint where
 *   digital platforms use machine learning systems optimized for user
 *   engagement to systematically direct cognitive resources toward platform
 *   activities, advertising, and content that serves platform revenue rather
 *   than user interests. The constraint operates through multiple mechanisms:
 *   variable reward schedules (intermittent reinforcement), behavioral
 *   surplus extraction (prediction and manipulation of user decisions),
 *   identity fusion (platform-constituted professional/social presence), and
 *   network effects (exit costs from switching). The extractiveness has
 *   increased over 15 years as algorithmic sophistication has grown,
 *   optimization targets have expanded from engagement metrics to behavioral
 *   prediction, and user dependency on platform-mediated access to
 *   employment, social connection, and information has deepened. Theater
 *   ratio has increased as regulatory pressure (DSA, GDPR, content
 *   moderation) has created compliance theater that mimics constraint without
 *   addressing underlying extraction mechanisms. The constraint exhibits
 *   different structural properties from different observer positions: pure
 *   extraction (snare) from trapped end users, coordination mechanism (rope)
 *   from platforms, regulatory theater (piton) from governments, and arms
 *   race compression (tangled rope) from competitors and advertisers. The
 *   analytical observer risks naturalizing this as inevitable feature of
 *   attention economics rather than recognizing it as designed extraction.
 *
 * KEY AGENTS:
 *   - End users (powerless/trapped): Bear full cost of attention capture; locked in through social/employment dependencies; lack exit capacity. Primary victim group.
 *   - Identity-locked users (powerless/identity_locked): Subset of end users whose professional or social identity is constituted through platform presence (creators, influencers, networked professionals). Cannot exit without abandoning self-concept.
 *   - Platform operators (institutional/arbitrage): Primary beneficiaries; control algorithmic mechanisms; can arbitrage across business models but currently profit-optimized for attention extraction.
 *   - Attention merchants (institutional/arbitrage): Advertisers and data brokers benefiting from targeted access to captured attention. Secondary beneficiaries.
 *   - Competing platforms (institutional/constrained): Trapped in attention arms race; must optimize for engagement to compete despite knowing engagement optimization extracts from users. Paradoxical position as both beneficiaries and victims.
 *   - Advertisers (moderate/constrained): Benefit from algorithmic targeting; constrained by platform dependency; cannot easily shift to alternative channels.
 *   - Regulatory bodies (organized/constrained): Create compliance theater (transparency reports, audits) that appears to constrain extraction but leaves optimization mechanisms intact. Theater masks persistent extraction.
 *   - Analytical observer (analytical/analytical): Risks naturalizing extraction as inevitable feature of attention economics rather than recognizing designed mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_attention_capture, 0.68).
domain_priors:suppression_score(algorithmic_attention_capture, 0.72).
domain_priors:theater_ratio(algorithmic_attention_capture, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_attention_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(algorithmic_attention_capture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_attention_capture, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_attention_capture, snare).
narrative_ontology:human_readable(algorithmic_attention_capture, "Algorithmic Attention Capture in Digital Platforms").
narrative_ontology:topic_domain(algorithmic_attention_capture, "digital_economy/cognitive_capture").

domain_priors:requires_active_enforcement(algorithmic_attention_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_attention_capture, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_attention_capture, attention_merchants).
narrative_ontology:constraint_victim(algorithmic_attention_capture, end_users).
narrative_ontology:constraint_victim(algorithmic_attention_capture, cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The end user is structurally trapped. Material exit costs are high (social isolation, employment disadvantage, access to services dependent on platform presence). Algorithmic capture operates at cognitive level through intermittent reinforcement and variable reward schedules. User perceives extraction clearly but lacks exit capacity. Maximum experienced extraction — the system is designed to prevent exit through multi-level dependency.
constraint_indexing:constraint_classification(algorithmic_attention_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Subset of end users for whom identity is constituted through platform presence. Professional identity (content creator, brand, influencer), social identity (community membership, reputation status), relational identity (maintained connections) are fused with the platform. Exit would require abandoning not just the service but the self-concept built within it. Structurally mobile but identity prevents exercise of exit options. Cognitive capture compounds material dependency.
constraint_indexing:constraint_classification(algorithmic_attention_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% Platform operator experiences the constraint as coordination mechanism. Attention capture enables platform's core coordination function — connecting users, advertisers, and content creators. Extraction runs toward platform: advertising revenue, user data, behavioral surplus. Arbitrage exit available — platform can shift business models, though currently dominant model sustains investment incentives. Net beneficiary position.
constraint_indexing:constraint_classification(algorithmic_attention_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Advertisers benefit from algorithmic targeting and attention concentration but are also captured. They depend on platform's attention mechanisms to reach audiences; they cannot build comparable infrastructure independently. Benefits from extraction (targeted reach) alongside vulnerability to extraction (platform algorithm changes, margin compression). Constrained exit — shifting platforms is costly but possible.
constraint_indexing:constraint_classification(algorithmic_attention_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Digital Services Acts, content moderation obligations, and data protection regulations create performative compliance theater. Platforms maintain algorithmic attention capture while demonstrating compliance through transparency reports, algorithmic audits, and oversight mechanisms that are largely theatrical. Actual extraction mechanisms remain intact. Regulatory theater persists through institutional inertia — genuine alternatives (algorithmic transparency, user control mechanisms) exist but are not structurally enforced. Theater ratio high because enforcement capacity is weak relative to platform scale.
constraint_indexing:constraint_classification(algorithmic_attention_capture, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Competing platforms experience attention capture as both extraction mechanism and coordination tool. They must use similar algorithmic techniques to compete, making them victims of the attention economy arms race. They benefit from user data and engagement but are constrained by network effects that lock users into dominant platforms. Cannot exit the arms race without losing competitive position. Both extraction and coordination present.
constraint_indexing:constraint_classification(algorithmic_attention_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% From civilizational perspective, algorithmic attention capture appears as a structural feature of attention economies — inevitable consequence of scarcity, competition, and incentive alignment. Risks naturalizing what is a designed extraction mechanism. The constraint exhibits properties of both natural selection (platforms compete for attention) and predatory design (optimization explicitly targets behavioral vulnerabilities). Analytical position sees both the coordination function (matching supply/demand) and the extraction mechanism (surplus capture through cognitive engineering).
constraint_indexing:constraint_classification(algorithmic_attention_capture, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_attention_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_attention_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_attention_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_attention_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_attention_capture, TR),
    TR >= 0.70.

:- end_tests(algorithmic_attention_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting significant asymmetry between platform benefit and user cost. The 15-year trajectory shows extraction increasing as algorithmic sophistication improved (0.35 → 0.68). This is not inevitable economic feature but result of deliberate optimization for engagement and behavioral prediction. Users clearly bear costs (attention redirected, decision-making altered, behavioral data harvested); platforms capture almost all benefit. Suppression (0.72): Very high. Exit barriers operate at multiple levels: material (social/employment integration with platforms), technological (designed to maximize engagement), cognitive (intermittent reinforcement), identity (self-concept fusion for identity-locked users), and network (switching costs from reduced connectivity). Users perceive extraction clearly (attention drain, algorithm-driven recommendations, targeted advertising) but cannot exit. Theater ratio (0.55): Moderate-high and increasing. Regulatory compliance (DSA transparency, GDPR access rights, content moderation) creates observable theater, but underlying optimization mechanisms remain intact. User controls are performative — algorithmic recommendations continue despite controls. This theater legitimacy crisis is driving measurement trajectory: as regulation expanded (t=5-15), theater ratio rose without extractiveness declining.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. End users (trapped) experience pure extraction (snare) — they are systematically captured with no viable exit. Identity-locked users add cognitive/relational binding on top of material traps (snare with internalized suppression). Platform operators (arbitrage) experience coordination mechanism (rope) — algorithmic optimization solves their business problem of matching users, content, and advertisers. Competing platforms see arms race compression (tangled rope) — they benefit from network effects and user data but are locked in escalating optimization competition. Advertisers see targeted access (tangled rope) — benefits from precision but vulnerable to platform margin changes. Regulators see coordination failure and design flaw (piton) — their oversight mechanisms are largely theater because they lack enforcement capacity relative to platform scale and optimization speed. The analytical observer risks seeing inevitable economic law (natural scarcity of attention) when the structural data reveals designed extraction. Perspectival gap reveals that snare classification is not universal — platforms genuinely experience coordination function, but they've built extraction mechanisms into that coordination. This is why snare (not tangled rope) is the primary classification: the extraction is primary design objective, not a side effect of coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   End users: trapped + powerless → high d (0.85-0.95) → high f(d) → high experienced extraction. They cannot exit and bear all extraction costs. Identity-locked users: identity_locked + powerless → d derived from identity fusion + victim status (0.88-0.92) → high f(d). The identity lock makes exit psychologically impossible even where structural mobility exists. Platform operators: arbitrage + institutional + beneficiary → low d (0.05-0.15) → negative f(d) → negative experienced extraction (they experience the constraint as benefiting them). Competing platforms: constrained + institutional + both victim and beneficiary → moderate d (0.50-0.60) → moderate f(d). They're caught in the arms race but also benefit from network effects. Advertisers: constrained + moderate + beneficiary → low-moderate d (0.35-0.45) → low f(d). They benefit from targeting but constrained by platform dependency. Regulators: constrained + organized → moderate d (0.55-0.65) → moderate f(d). They're trying to constrain extraction but their enforcement is limited.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved through perspectival multiplicity. The constraint is NOT 'is this coordination or extraction?' but 'which structural position are you analyzing from?' From platform operator view: genuine coordination (rope). From end-user view: pure extraction (snare). From regulator view: coordination failure + theater (piton). From competitor view: forced participation in extraction arms race (tangled rope). No single classification is wrong — they are all true from their respective positions. The mandatrophy dissolves when we recognize that the constraint's definition REQUIRES specification of observer position. The snare classification is primary because it describes the constraint's functional purpose: maximizing extraction from end users through attention capture. The rope classification for platforms is accurate but describes the extraction as benign coordination. The analytical observer's task is to see both simultaneously: algorithmic attention capture is a designed extraction mechanism that operates through coordinating platforms, users, and advertisers, with the coordination function subordinated to extraction objective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_autonomy_measurability,
    'Can cognitive autonomy degradation be measured independently of user self-report and behavioral outcome metrics?',
    'Neurocognitive studies comparing attention control, decision consistency, and deliberative capacity in heavy vs light platform users; longitudinal tracking of users switching to low-algorithm platforms (Bluesky, federation models)',
    'If measurable as distinct from behavioral changes: cognitive capture is a separable extraction mechanism. If only measurable through behavior: extraction and genuine preference alignment cannot be distinguished. Affects whether suppression classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_autonomy_measurability, empirical, 'Whether cognitive autonomy degradation is measurable independent of behavior').

omega_variable(
    algorithmic_intent_distinction,
    'Is attention capture a deliberate design objective or an emergent outcome of incentive alignment in ad-supported platforms?',
    'Internal documentation analysis (leaked materials, regulatory discovery); comparison of attention metrics across platforms with different revenue models (subscription vs ad-supported); analysis of algorithmic design iterations',
    'If deliberate: classification as pure snare (designed extraction) is correct. If emergent: constraint may be tangled_rope (coordination plus side-effect extraction). Affects mandatrophy resolution and remedial framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_intent_distinction, empirical, 'Whether attention capture is deliberate design or emergent outcome').

omega_variable(
    identity_lock_reversibility,
    'For identity-locked users who exit platforms, how quickly does identity reconstruction occur and to what extent does the abandoned platform identity persist?',
    'Longitudinal interviews with users who left high-engagement platforms; measurement of reorientation time, identity continuity challenges, and cognitive/relational costs of exit',
    'If reversible within months: identity lock is substantial but not permanent structural capture. If irreversible or requiring years: identity lock represents a distinct extraction mechanism operating post-exit. Affects whether identity_locked classification is appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity lock reverses after platform exit').

omega_variable(
    regulatory_theater_effectiveness,
    'Do transparency reports, algorithmic audits, and user data access mechanisms create meaningful constraints on algorithmic attention capture or are they primarily performative?',
    'Comparison of attention metrics before/after regulatory intervention; analysis of user behavior changes following transparency implementations; measurement of actual algorithmic changes in response to regulatory pressure',
    'If effective: piton classification is incorrect; regulatory mechanism should be scaffold or tangled_rope. If performative: piton is correct, and regulatory theater provides false legitimacy for extraction mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_theater_effectiveness, empirical, 'Whether regulation constrains algorithmic attention capture or is theater').

omega_variable(
    network_effects_exit_ceiling,
    'What is the maximum viable market share for non-algorithmic-capture platforms given network effect lock-in?',
    'Analysis of federation protocol adoption, decentralized platform growth trajectories, user switching costs between equivalent services; simulation of tipping points in multi-platform user behavior',
    'If ceiling exists below critical mass: exit mechanisms are structurally impossible, and constraint is immutable (mountain-like). If ceiling is higher: meaningful alternative platforms can emerge through coordination. Affects whether snare classification understates structural desperation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_exit_ceiling, empirical, 'Maximum viable market share for non-algorithmic alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_attention_capture, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcap_tr_t0, algorithmic_attention_capture, theater_ratio, 0, 0.25).
narrative_ontology:measurement(algcap_tr_t5, algorithmic_attention_capture, theater_ratio, 5, 0.4).
narrative_ontology:measurement(algcap_tr_t10, algorithmic_attention_capture, theater_ratio, 10, 0.55).
narrative_ontology:measurement(algcap_tr_t15, algorithmic_attention_capture, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(algcap_be_t0, algorithmic_attention_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algcap_be_t5, algorithmic_attention_capture, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(algcap_be_t10, algorithmic_attention_capture, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(algcap_be_t15, algorithmic_attention_capture, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_attention_capture, resource_allocation).
narrative_ontology:boltzmann_floor_override(algorithmic_attention_capture, 0.18).
narrative_ontology:affects_constraint(algorithmic_attention_capture, behavioral_surplus_extraction).
narrative_ontology:affects_constraint(algorithmic_attention_capture, social_graph_lock_in).
narrative_ontology:affects_constraint(algorithmic_attention_capture, attention_economy_arms_race).
narrative_ontology:affects_constraint(algorithmic_attention_capture, digital_consent_theater).

% DUAL FORMULATION NOTE:
% Algorithmic attention capture is upstream of specific extraction mechanisms (behavioral surplus, graph lock-in, consent theater). Attention capture is the coordination mechanism that enables these downstream constraints. This story focuses on the capture mechanism itself; downstream stories decompose specific extraction pathways and their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_attention_capture, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
