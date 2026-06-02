% ============================================================================
% CONSTRAINT STORY: perceptual_immediacy_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perceptual_immediacy_bias, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perceptual_immediacy_bias
 *   human_readable: Perceptual Immediacy Bias in Human Cognition
 *   domain: epistemology/cognitive_science/neuroscience
 *
 * SUMMARY:
 *   The perceptual immediacy bias is a structural feature of human
 *   neurocognitive architecture: the perceptual system is optimized for
 *   detecting and responding to sudden changes (predators, falling objects,
 *   social threats) at the expense of sensitivity to gradual accumulation
 *   (erosion, aging, climate shifts, institutional drift). This asymmetry is
 *   measurable in reaction time studies, neurological imaging, and attention
 *   allocation patterns. Sudden stimuli trigger automatic orienting
 *   responses; gradual changes require deliberate cognitive construction to
 *   become salient. The constraint is universal across human populations and
 *   appears invariant to cultural context, suggesting deep evolutionary
 *   origins in selection pressure for immediate threat detection. This is a
 *   canonical mountain: the base extractiveness is negligible (0.08 — the
 *   'cost' is merely the opportunity cost of not perceiving slow processes
 *   automatically), suppression is minimal (0.02 — no active enforcement
 *   prevents awareness of gradual change, only the passive absence of
 *   automatic salience), accessibility collapse is high (0.92 — compensatory
 *   strategies like time-lapse photography or statistical monitoring exist
 *   but do not eliminate the underlying perceptual asymmetry), and resistance
 *   is low (0.08 — attempts to 'train' automatic sensitivity to slow changes
 *   have shown minimal success). The constraint emerges naturally from the
 *   structure of the perceptual system and requires no institutional
 *   maintenance.
 *
 * KEY AGENTS:
 *   - Individual Human Observer: Universal subject (powerless/trapped) — cannot exit their own perceptual architecture; experiences the bias as an unchangeable feature of perception
 *   - Scientific Community: Organized collective (organized/constrained) — develops compensatory instruments but cannot eliminate the underlying asymmetry
 *   - Institutional Decision-Maker: Institutional actor (institutional/arbitrage) — has access to data systems but faces the same perceptual constraint; must actively construct awareness of slow-moving risks
 *   - Analytical Observer: Meta-level perspective (analytical/analytical) — recognizes the constraint as a structural property of human neurocognition shaped by evolutionary selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perceptual_immediacy_bias, 0.08).
domain_priors:suppression_score(perceptual_immediacy_bias, 0.02).
domain_priors:theater_ratio(perceptual_immediacy_bias, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perceptual_immediacy_bias, extractiveness, 0.08).
narrative_ontology:constraint_metric(perceptual_immediacy_bias, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(perceptual_immediacy_bias, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(perceptual_immediacy_bias, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(perceptual_immediacy_bias, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perceptual_immediacy_bias, mountain).
narrative_ontology:human_readable(perceptual_immediacy_bias, "Perceptual Immediacy Bias in Human Cognition").
narrative_ontology:topic_domain(perceptual_immediacy_bias, "epistemology/cognitive_science/neuroscience").

domain_priors:emerges_naturally(perceptual_immediacy_bias).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL HUMAN (MOUNTAIN) — Cannot exit the constraint of their own perceptual architecture. The bias toward immediate stimuli over gradual change is hardwired into neural response patterns shaped by evolutionary selection. No amount of conscious effort eliminates the differential reaction time to sudden vs slow changes.
constraint_indexing:constraint_classification(perceptual_immediacy_bias, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SCIENTIFIC COMMUNITY (MOUNTAIN) — Can develop compensatory instruments (time-lapse photography, longitudinal studies, statistical process control) but cannot eliminate the underlying perceptual asymmetry. The constraint remains: human observers must actively construct awareness of slow processes through deliberate methodology, while fast changes register automatically.
constraint_indexing:constraint_classification(perceptual_immediacy_bias, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL ACTOR (MOUNTAIN) — Even with access to data systems and analytical tools, institutional actors face the same perceptual architecture. Early warning systems for slow-moving crises (climate change, soil depletion, demographic shifts) require active construction and maintenance because the underlying perceptual bias does not register gradual accumulation as salient.
constraint_indexing:constraint_classification(perceptual_immediacy_bias, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes the perceptual immediacy bias as a structural feature of human neurocognitive architecture. The differential response to sudden vs gradual stimuli is measurable across cultures, developmental stages, and historical periods. The constraint is not a policy choice or institutional arrangement but a property of the perceptual apparatus itself, shaped by evolutionary selection for immediate threat detection.
constraint_indexing:constraint_classification(perceptual_immediacy_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perceptual_immediacy_bias_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(perceptual_immediacy_bias, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perceptual_immediacy_bias, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(perceptual_immediacy_bias, ExtMetricName, E),
    domain_priors:suppression_score(perceptual_immediacy_bias, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(perceptual_immediacy_bias),
    narrative_ontology:constraint_metric(perceptual_immediacy_bias, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(perceptual_immediacy_bias, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(perceptual_immediacy_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal direct cost — humans can perceive slow changes when they deliberately attend to them or use instruments. The 'extraction' is merely the opportunity cost of not having automatic salience for gradual processes, which is negligible compared to the benefit of rapid threat detection. Suppression (0.02): Minimal. No active mechanism prevents awareness of slow changes; the constraint is purely the passive absence of automatic salience. Humans can and do construct awareness of gradual processes through deliberate effort, time-lapse observation, statistical analysis, and cultural transmission of knowledge about slow phenomena. Accessibility collapse (0.92): Very high. Compensatory strategies exist (longitudinal studies, process control charts, climate models, actuarial tables) but they do not change the underlying perceptual asymmetry — they work around it. The constraint remains: slow changes do not trigger automatic orienting responses. Resistance (0.08): Very low. The perceptual bias is robust across attempts to modify it. Training studies show minimal success in creating automatic sensitivity to gradual change. The asymmetry persists across cultures, developmental stages, and historical periods. Theater ratio (0.05): Negligible. There is no performative component — the constraint is a direct property of the perceptual system with no institutional overlay.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all perspectives classify as mountain because the underlying structure is invariant across observation contexts. The individual human cannot exit their perceptual architecture. The scientific community can develop compensatory instruments but cannot eliminate the bias. Institutional actors face the same constraint despite access to data systems. The analytical observer recognizes the constraint as a structural property of human neurocognition. The uniformity across perspectives is diagnostic: when a constraint classifies as mountain from powerless/trapped, organized/constrained, institutional/arbitrage, and analytical/analytical perspectives, the classification is robust. This is not a false summit — the structural data (low extractiveness, minimal suppression, high accessibility collapse, low resistance, natural emergence) confirms the mountain classification. The constraint is not a naturalized institutional arrangement but an actual property of the perceptual system.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type mountain constraint with no beneficiaries or victims in the structural sense. The perceptual immediacy bias is not an extraction mechanism — it is a design trade-off in the perceptual system. All human observers experience the same constraint regardless of power, exit options, or scope. The constraint does not flow asymmetrically from one group to another; it is a universal feature of human neurocognition. Directionality is not applicable because there is no extraction flow to measure. The low extractiveness (0.08) reflects that the constraint imposes minimal cost — the opportunity cost of not perceiving slow processes automatically is small compared to the benefit of rapid threat detection that the bias enables.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints are extraction mechanisms. The perceptual immediacy bias is a design trade-off in the human perceptual system: optimization for immediate threat detection necessarily creates relative insensitivity to gradual change. The 'cost' is not extraction (asymmetric transfer of value from one group to another) but opportunity cost (the absence of a feature that would be beneficial but is incompatible with the existing optimization). The constraint is not maintained by any institutional actor and benefits no particular group — it is a universal feature of human neurocognition. The mountain classification is appropriate because the constraint emerges naturally from the structure of the perceptual apparatus, exhibits high resistance to modification, and imposes minimal suppression (humans can perceive slow changes when they deliberately attend to them). This is a canonical example of a true mountain: a structural limit that is not a disguised snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perceptual_immediacy_bias, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perceptual_immediacy_bias, information_standard).
narrative_ontology:affects_constraint(perceptual_immediacy_bias, climate_change_political_inaction).
narrative_ontology:affects_constraint(perceptual_immediacy_bias, soil_depletion_agricultural_policy).
narrative_ontology:affects_constraint(perceptual_immediacy_bias, institutional_drift_detection_failure).
narrative_ontology:affects_constraint(perceptual_immediacy_bias, boiling_frog_syndrome).

% DUAL FORMULATION NOTE:
% The perceptual immediacy bias is an upstream constraint that affects multiple downstream institutional and policy failures. The bias itself is a mountain (low extractiveness, natural emergence, high resistance). Downstream constraints that exploit or are shaped by the bias (e.g., political systems optimized for immediate crises rather than slow-moving risks) have their own extractiveness values reflecting the institutional arrangements built on top of the perceptual constraint. The perceptual bias is a necessary but not sufficient condition for these downstream failures — institutional design choices determine whether the bias is compensated for or amplified.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
