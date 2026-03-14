% ============================================================================
% CONSTRAINT STORY: algorithmic_curation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_curation, []).

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
 *   constraint_id: algorithmic_curation
 *   human_readable: Algorithmic Curation and Content Distribution Control
 *   domain: digital_platforms/information_systems
 *
 * SUMMARY:
 *   Algorithmic curation on digital platforms creates a structural constraint
 *   that appears simultaneously as a coordination mechanism (solving content
 *   discovery at scale), an extractive apparatus (capturing user attention
 *   and behavioral data), and a gatekeeping system (marginalizing voices that
 *   don't optimize for engagement metrics). The constraint embeds genuine
 *   coordination function — distributing relevant content to billions of
 *   users requires automated selection — alongside asymmetric extraction:
 *   platforms benefit from engagement optimization while users and
 *   non-privileged creators bear costs. The extractiveness has increased over
 *   the decade as algorithms have become more sophisticated at behavioral
 *   prediction and engagement capture, while the theater ratio has risen as
 *   regulatory compliance theater (transparency reports, content moderation
 *   policies) has proliferated without constraining underlying curation
 *   mechanics. The constraint exhibits strong perspectival divergence:
 *   high-engagement creators see pure coordination (Rope), while marginalized
 *   voices see extraction (Snare). Identity-locked creators experience the
 *   constraint as traplike despite structural mobility. The analytical
 *   observer sees the hybrid nature: genuine coordination infrastructure with
 *   genuine extraction embedded within it.
 *
 * KEY AGENTS:
 *   - Platform Operators: Institutional beneficiary (institutional/arbitrage) — control algorithmic parameters, benefit from engagement-driven business model, can modify constraints unilaterally
 *   - Marginalized Voices: Primary victim (powerless/trapped) — face algorithmic suppression, lack alternative distribution channels, have no recourse within platform systems
 *   - Information Seekers: Secondary victim (moderate/constrained) — benefit from filtering but constrained by engagement optimization that diverges from preference, face network-effect switching costs
 *   - High-Engagement Content Creators: Beneficiary (powerful/mobile) — capture algorithmic amplification, can adapt to optimization targets, have resource to engage alternative platforms
 *   - Creator Identity-Locked to Platform: Victim (powerless/identity_locked) — professional identity and income constituted through platform success, cannot exit despite awareness
 *   - Regulatory Framework: Institutional actor (institutional/constrained) — maintains performative compliance theater, constrained by platform design to ineffectual implementation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes both genuine coordination and genuine extraction functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_curation, 0.58).
domain_priors:suppression_score(algorithmic_curation, 0.65).
domain_priors:theater_ratio(algorithmic_curation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_curation, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_curation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_curation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_curation, tangled_rope).
narrative_ontology:human_readable(algorithmic_curation, "Algorithmic Curation and Content Distribution Control").
narrative_ontology:topic_domain(algorithmic_curation, "digital_platforms/information_systems").

domain_priors:requires_active_enforcement(algorithmic_curation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_curation, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_curation, high_engagement_content_creators).
narrative_ontology:constraint_victim(algorithmic_curation, marginalized_voices).
narrative_ontology:constraint_victim(algorithmic_curation, information_seekers).
narrative_ontology:constraint_victim(algorithmic_curation, algorithmic_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED VOICE (SNARE) — Content creators from underrepresented communities face algorithmic suppression with no meaningful exit. Cannot reach audiences without platform access; algorithmic ranking is opaque and unappealable. Trapped within the system with no alternative distribution channels at comparable scale. Extraction is severe and coercive.
constraint_indexing:constraint_classification(algorithmic_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMATION SEEKER (TANGLED ROPE) — Users benefit from curation that filters signal from noise, but face algorithmic ranking that prioritizes engagement over accuracy. Constrained by high switching costs (network effects, switching to alternative platforms requires abandoning accumulated recommendations). Experience genuine coordination (filtering) alongside extraction (attention capture, data harvesting).
constraint_indexing:constraint_classification(algorithmic_curation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences algorithmic curation as genuine coordination: solving the problem of content discovery at scale. Solves collective action problem (which content to show to whom) with minimal constraint on their own operations. Can exit the constraint by modifying the algorithm; benefits from the engagement it drives. Net beneficiary.
constraint_indexing:constraint_classification(algorithmic_curation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-ENGAGEMENT CONTENT CREATOR (TANGLED ROPE) — Benefits from algorithmic amplification of content that drives engagement (viral loops, sensational claims). Extraction runs toward them through preferential ranking. Also constrained by algorithm's optimization target — must optimize for engagement metrics rather than accuracy or user benefit. Powerful enough to game the system; mobile enough to exit if algorithmic reward structure changes. Mixed coordination-extraction relationship.
constraint_indexing:constraint_classification(algorithmic_curation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Content moderation policies and algorithmic transparency mandates are largely performative: platforms publish transparency reports that obscure algorithmic decision-making; compliance theater with minimal actual constraint on curation practices. Regulations persist through institutional inertia (EU DSA, US Section 230) but their functional effectiveness is degraded. Theater ratio high because compliance mechanisms don't constrain the underlying extraction.
constraint_indexing:constraint_classification(algorithmic_curation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CREATOR IDENTITY-LOCKED TO PLATFORM (SNARE) — Content creators whose professional identity and income stream are constituted through platform success cannot exit despite awareness of extractive dynamics. Identity as 'influencer' or 'content creator' is inseparable from algorithmic favor. Even if alternative platforms exist, exit would require abandoning the identity and audience built within this constraint. Structurally mobile (could delete account) but functionally trapped by identity fusion. High extraction experienced through dependency.
constraint_indexing:constraint_classification(algorithmic_curation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, algorithmic curation performs genuine coordination function (content discovery) while embedding asymmetric extraction (attention capture, behavioral prediction, engagement optimization for platform benefit). Both functions are real and structural. Classification reflects hybrid nature: genuine coordination benefit with genuine extraction cost.
constraint_indexing:constraint_classification(algorithmic_curation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_curation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_curation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_curation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_curation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_curation, TR),
    TR >= 0.70.

:- end_tests(algorithmic_curation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Measured as the proportion of curation infrastructure optimized for platform benefit (engagement capture, behavioral profiling, attention economics) rather than user utility alignment. Measured at 0.32 ten years ago when engagement optimization was less sophisticated; increased to 0.58 as machine learning enabled finer behavioral prediction. Not maximal (0.70+) because genuine coordination function persists — content discovery at scale genuinely requires algorithmic selection. Suppression (0.65): Measured as barriers to alternative platforms (network effects, switching costs, audience lock-in) plus algorithmic opacity (users cannot understand or appeal ranking decisions). High because marginalized creators face both platform-enforced policy suppression and emergent suppression from engagement-optimization. Theater ratio (0.68): Measured as ratio of performative compliance (transparency reports, content moderation policy pages) to actual constraint on curation. Rising over time as regulatory pressure has increased but algorithmic behavior has not substantively changed. High theater reflects that compliance mechanisms obscure rather than constrain underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators (Rope) perceive algorithmic curation as solving a coordination problem they control. Marginalized voices (Snare) perceive the same curation as extractive gatekeeping with no recourse. Information seekers (Tangled Rope) perceive both filtering benefit and attention capture. High-engagement creators (Tangled Rope) perceive the algorithm as rewarding their work while constraining content choices. Identity-locked creators (Snare via identity_locked) perceive extraction but cannot exit because their identity is constituted through platform success. The regulatory framework (Piton) perceives its own policy implementation as performative theater — compliance exists but doesn't change curation behavior. The analytical observer (Tangled Rope) perceives that the perspectival gap itself is the diagnostic signal: the constraint performs genuine coordination while embedding asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators benefit from algorithmic curation with arbitrage-level exit options (can modify algorithm at will, not bound by its outputs). Derived d ≈ 0.05. High-engagement creators benefit from engagement optimization and have mobile exit (can build audience elsewhere or modify content strategy). Derived d ≈ 0.30. Information seekers bear coordination costs (limited choice) and extraction costs (attention capture) but benefit from filtering; have constrained exit (network effects). Derived d ≈ 0.55. Marginalized voices bear pure extraction through algorithmic suppression and have trapped exit (no alternative platforms at comparable scale). Derived d ≈ 0.95. Creators identity-locked to platform have structurally mobile exit but are functionally trapped by identity fusion; bear extraction while identity depends on platform success. Derived d ≈ 0.85 (high extraction experienced, but cognitive mechanism makes exit unthinkable). The directionality pipeline correctly differentiates these positions: beneficiaries with exit get low d, victims with exit restrictions get high d.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids false classification through perspectival clarity. The temptation is to classify as pure Rope (it's coordination infrastructure) or pure Snare (it's clearly extractive). The tangled rope classification captures that BOTH are true: algorithms genuinely solve content discovery (a coordination problem) while genuinely optimizing for platform benefit over user preference (extraction). The theater ratio rising alongside extractiveness (both increasing over the interval) suggests the constraint is drifting toward Snare + Piton hybrid (more extraction + more performative compliance). The perspectival gap is diagnostic: if all perspectives saw Rope, the classification would be false. The snare experiences from marginalized voices confirm the extraction is real, not illusory. The piton perspective on regulatory compliance confirms that enforcement theater doesn't reduce extraction. Mandatrophy is resolved by insisting on both the coordination function AND the extraction asymmetry in the same story, rather than trying to force a single-function classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_gap,
    'Is algorithmic opacity a technical necessity or a deliberate design choice to enable extraction?',
    'Comparative analysis of fully-transparent curation systems (e.g., chronological feeds) vs opaque optimization; user behavior and platform revenue correlation',
    'If technical necessity: suppression is lower than measured (opacity is coordination cost). If design choice: extraction is higher (opacity enables behavioral capture). Current evidence suggests deliberate design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_gap, empirical, 'Whether algorithmic opacity is necessary or chosen for extraction').

omega_variable(
    engagement_optimization_vs_utility_alignment,
    'Do algorithms optimize for user-reported preference or for engagement metrics that diverge from user utility?',
    'A/B testing: chronological feed vs engagement-optimized feed vs user-preference-aligned feed; satisfaction metrics; long-term vs short-term preference revelation',
    'If aligned: constraint classifies as rope (pure coordination). If divergent: constraint is snare with hidden extraction. Evidence shows strong divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_optimization_vs_utility_alignment, empirical, 'Alignment between algorithmic optimization target and user utility').

omega_variable(
    marginalized_voice_visibility_floor,
    'What proportion of algorithm-suppressed content is suppressed due to platform-enforced policies vs emergent from engagement-optimization?',
    'Content analysis of suppressed posts by creator demographic; correlation with policy violations vs engagement metrics; measurement of algorithmic visibility by creator verification status and follower count',
    'If mostly policy-enforced: suppression is justified coordination. If mostly engagement-emergent: suppression is structural extraction targeting marginalized creators. Evidence shows emergence dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_visibility_floor, empirical, 'Source of suppression for marginalized voice content').

omega_variable(
    identity_lock_reversibility,
    'Can creators locked into platform identity successfully transition to alternative platforms without losing audience and revenue?',
    'Historical data on creator transitions (YouTube to Rumble, Twitter to Bluesky); audience retention and revenue recovery rates; network effects measurement',
    'If reversible: exit_options should be constrained not trapped. If irreversible: true identity lock exists. Current evidence shows high switching costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of creator platform identity lock').

omega_variable(
    coordination_vs_extraction_proportion,
    'What fraction of algorithmic curation cost is genuine coordination overhead vs extractive optimization for platform benefit?',
    'Cost accounting: infrastructure required for content discovery vs infrastructure used for behavioral profiling and attention capture; user satisfaction with curation quality vs platform revenue from engagement data',
    'High coordination proportion: rope or scaffold. High extraction proportion: snare or piton. Current measurement suggests 40% coordination, 60% extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_proportion, empirical, 'Proportion of curation cost attributable to coordination vs extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_curation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcur_tr_t0, algorithmic_curation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algcur_tr_t5, algorithmic_curation, theater_ratio, 5, 0.55).
narrative_ontology:measurement(algcur_tr_t10, algorithmic_curation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(algcur_be_t0, algorithmic_curation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(algcur_be_t5, algorithmic_curation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(algcur_be_t10, algorithmic_curation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_curation, information_standard).
narrative_ontology:boltzmann_floor_override(algorithmic_curation, 0.12).
narrative_ontology:affects_constraint(algorithmic_curation, attention_economy).
narrative_ontology:affects_constraint(algorithmic_curation, platform_monopoly).
narrative_ontology:affects_constraint(algorithmic_curation, behavioral_prediction).

% DUAL FORMULATION NOTE:
% Algorithmic curation decomposes into three structurally distinct constraints: (1) content_discovery_coordination (ε≈0.15, pure Rope—the genuine technical problem), (2) engagement_optimization_extraction (ε≈0.62, Snare—the behavioral capture mechanism), (3) marginalized_voice_suppression (ε≈0.75, Snare—the systematic exclusion). These three stories are linked by network effects: engagement optimization drives marginalized voice suppression; both require coordination infrastructure. The present story represents the unified constraint viewed from multiple perspectives rather than decomposed into its structural components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_curation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
