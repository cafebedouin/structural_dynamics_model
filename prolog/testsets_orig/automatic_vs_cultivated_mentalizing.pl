% ============================================================================
% CONSTRAINT STORY: automatic_vs_cultivated_mentalizing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_automatic_vs_cultivated_mentalizing, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: automatic_vs_cultivated_mentalizing
 *   human_readable: Automatic vs Cultivated Mentalizing: The Cognitive Effort Differential
 *   domain: cognitive_science/cultural_theory/media_studies
 *
 * SUMMARY:
 *   The distinction between automatic mentalizing (reading familiar signals
 *   effortlessly) and cultivated mentalizing (sustained reconstruction of
 *   unfamiliar motivations under ambiguity) represents a structural
 *   constraint on human social cognition. This is not a policy choice,
 *   cultural norm, or institutional arrangement — it is a feature of
 *   cognitive architecture observable across all human populations. Automatic
 *   mentalizing operates via pattern-matching to stored schemas: when
 *   observing agents whose motivational structures are familiar (same
 *   culture, same professional context, same relational norms),
 *   perspective-taking happens rapidly and with minimal working memory load.
 *   Cultivated mentalizing operates via deliberate hypothesis generation and
 *   testing: when tracking motivations of structurally unfamiliar agents
 *   (different cultural frameworks, unfamiliar institutional logics, novel
 *   relational configurations), sustained attention and working memory
 *   resources are required. The effort differential is measurable: reaction
 *   time studies show faster perspective-taking for familiar vs unfamiliar
 *   agents; fMRI studies show greater prefrontal activation (executive
 *   control) for unfamiliar perspective-taking; longitudinal studies show
 *   that specific unfamiliar frameworks can become automatic through extended
 *   exposure, but the underlying effort asymmetry persists — newly learned
 *   patterns always require more resources than deeply ingrained ones until
 *   they too become automatic. This constraint has no beneficiaries in the
 *   structural sense — it is a cognitive universal that all agents
 *   experience. Media institutions and cultural gatekeepers may exploit the
 *   differential (designing content for automatic consumption to maximize
 *   audience size), but they do not benefit FROM the constraint existing —
 *   they adapt TO it.
 *
 * KEY AGENTS:
 *   - Cognitively Constrained Agent: Universal human subject (powerless/trapped) — experiences effort differential as unchangeable cognitive limit
 *   - Cultural Learner: Agent acquiring new frameworks (moderate/constrained) — can shift specific content from unfamiliar to familiar through practice, but underlying differential persists
 *   - Media Institution: Content designer (institutional/mobile) — adapts content strategy to audience effort constraints but cannot eliminate the differential
 *   - Cognitive Scientist: Analytical observer (analytical/analytical) — measures the differential as a structural feature of mentalizing architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(automatic_vs_cultivated_mentalizing, 0.08).
domain_priors:suppression_score(automatic_vs_cultivated_mentalizing, 0.03).
domain_priors:theater_ratio(automatic_vs_cultivated_mentalizing, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, extractiveness, 0.08).
narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(automatic_vs_cultivated_mentalizing, mountain).
narrative_ontology:human_readable(automatic_vs_cultivated_mentalizing, "Automatic vs Cultivated Mentalizing: The Cognitive Effort Differential").
narrative_ontology:topic_domain(automatic_vs_cultivated_mentalizing, "cognitive_science/cultural_theory/media_studies").

domain_priors:emerges_naturally(automatic_vs_cultivated_mentalizing).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COGNITIVELY CONSTRAINED AGENT (MOUNTAIN) — Experiences the effort differential as an unchangeable cognitive limit. Automatic mentalizing (reading familiar signals) happens effortlessly; sustained reconstruction of unfamiliar motivations requires deliberate attention and working memory resources. Cannot exit this constraint through willpower alone — the effort asymmetry is a structural feature of cognitive architecture.
constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CULTURAL LEARNER (MOUNTAIN) — Can develop facility with specific unfamiliar frameworks through sustained practice (learning a new culture's norms, professional socialization), but the underlying effort differential persists: newly learned patterns require more cognitive resources than deeply familiar ones until they become automatic through extended exposure. The constraint is modifiable at the margin but not eliminable — even expert cross-cultural navigators experience higher cognitive load when tracking unfamiliar motivations.
constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MEDIA INSTITUTION (MOUNTAIN) — Designs content around the effort differential: familiar character archetypes and motivational patterns reduce audience cognitive load, enabling passive consumption. Unfamiliar perspectives require active reconstruction, limiting audience size. The institution can choose which side of the differential to target but cannot eliminate the differential itself — it is a constraint on audience attention economics, not a policy choice.
constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Identifies the effort differential as a structural feature of human mentalizing architecture. Automatic mentalizing relies on pattern-matching to stored schemas (System 1); cultivated mentalizing requires sustained working memory engagement and hypothesis testing (System 2). The differential is measurable across cultures and developmental stages. No intervention eliminates it — training can shift specific content from unfamiliar to familiar, but the underlying automatic/effortful distinction persists as a cognitive universal.
constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automatic_vs_cultivated_mentalizing_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_vs_cultivated_mentalizing, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, ExtMetricName, E),
    domain_priors:suppression_score(automatic_vs_cultivated_mentalizing, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(automatic_vs_cultivated_mentalizing),
    narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(automatic_vs_cultivated_mentalizing, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(automatic_vs_cultivated_mentalizing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint imposes cognitive costs (effort required for unfamiliar perspective-taking) but these are inherent processing costs, not asymmetric extraction. No agent captures rents from the differential existing — it is a shared cognitive limit. The small non-zero value reflects that the differential creates opportunity costs (time/attention spent on effortful mentalizing cannot be spent on other tasks), but this is coordination cost, not extraction. Suppression (0.03): Minimal. Agents are not prevented from engaging in cultivated mentalizing — the constraint is an effort differential, not a prohibition. Sustained attention to unfamiliar motivations is possible; it simply requires more cognitive resources than automatic mentalizing. The small non-zero value reflects that cognitive load can function as a soft barrier (agents may avoid effortful mentalizing when automatic options are available), but this is a resource constraint, not active suppression. Theater ratio (0.15): Very low. The constraint is functional, not performative. The effort differential is a real feature of cognitive processing, measurable via reaction time, neural activation, and task performance. Some theatrical overlay exists (cultural narratives about 'empathy' that conflate automatic and cultivated mentalizing, self-help claims about 'perspective-taking' that ignore the effort differential), but the core constraint is not maintained through ritual — it persists because it is a structural feature of how mentalizing works. Accessibility collapse (0.92): Very high. The constraint is accessible to introspection and empirical measurement. Agents can notice that tracking unfamiliar motivations feels harder than reading familiar signals; experimental paradigms can measure the effort differential directly. Resistance (0.08): Very low. The constraint does not resist investigation — it is one of the most studied phenomena in social cognition. No institutional or epistemic barriers prevent agents from recognizing the differential.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents classify it as mountain because it is a genuine cognitive universal. The powerless agent experiences it as an unchangeable limit on immediate perspective-taking capacity. The moderate agent experiences it as a biographical constraint that can be modulated through learning (specific unfamiliar frameworks can become familiar) but not eliminated (the underlying effort differential persists). The institutional agent experiences it as a structural constraint on content design and audience attention economics. The analytical observer measures it as a feature of mentalizing architecture. All perspectives converge on mountain because the constraint is immutable: no amount of training, institutional reform, or cultural change can eliminate the effort differential between automatic and cultivated mentalizing. Training can shift specific content from unfamiliar to familiar, but the underlying automatic/effortful distinction is a cognitive universal.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a genuine natural law constraint with no beneficiaries or victims in the structural sense. All agents experience the effort differential as a cognitive universal. The constraint does not extract from some agents to benefit others — it imposes symmetric costs (unfamiliar perspective-taking requires effort for everyone). Media institutions and cultural gatekeepers adapt their strategies to the differential (designing for automatic consumption to reduce audience cognitive load), but they are responding to the constraint, not benefiting from its existence. If the differential disappeared (if all mentalizing became effortless regardless of familiarity), media institutions would adapt their content strategies accordingly — they have no stake in the constraint persisting. The directionality values for all perspectives derive from the canonical fallback (no beneficiary/victim data), and all perspectives classify as mountain because the constraint is immutable at all time horizons and power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints involve extraction or coordination — some are genuine natural laws. The effort differential between automatic and cultivated mentalizing is not a policy choice that could be reformed, a cultural norm that could be renegotiated, or an institutional arrangement that could be redesigned. It is a structural feature of human cognitive architecture, observable across all populations and developmental stages. The constraint imposes costs (cognitive effort required for unfamiliar perspective-taking) but these are inherent processing costs, not asymmetric extraction. No agent benefits from the constraint existing — all agents experience it as a shared cognitive limit. The classification as mountain from all perspectives reflects that the constraint is immutable at all time horizons and power levels. This is the paradigm case of a constraint that should NOT be decomposed into coordination and extraction components — it is neither. It is a cognitive universal that all agents must navigate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(automatic_vs_cultivated_mentalizing, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(automatic_vs_cultivated_mentalizing, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a cognitive primitive — it does not decompose into multiple structurally distinct claims with different epsilon values. The effort differential is a single measurable phenomenon (reaction time, neural activation, task performance) that is stable across observables. Constraints downstream of this one (e.g., media content design strategies, cultural gatekeeping norms, cross-cultural communication barriers) have their own extractiveness values reflecting institutional and cultural arrangements, but the underlying cognitive differential is a mountain-class constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
