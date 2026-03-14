% ============================================================================
% CONSTRAINT STORY: childhood_magical_thinking_development
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_childhood_magical_thinking_development, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: childhood_magical_thinking_development
 *   human_readable: Childhood Magical Thinking Development
 *   domain: cognitive_development/psychology
 *
 * SUMMARY:
 *   Childhood magical thinking is a universal, invariant stage in cognitive
 *   development where children ages ~18 months to ~6-7 years operate from
 *   epistemic premises that violate adult causal reasoning: objects have
 *   animacy based on movement, thoughts can directly influence external
 *   events (magical causality), people possess fixed quantities of abstract
 *   properties (conservation failure), and categories shift fluidly without
 *   essences (pre-operational reasoning). This is not a constraint that can
 *   be escaped, negotiated, or suppressed through social enforcement — it is
 *   a structural feature of how the human prefrontal cortex develops across
 *   its first decade. No agent is being extracted from; no beneficiary
 *   captures value. The 'constraint' is the developmental stage itself:
 *   children are locked into magical reasoning modes not by any force but by
 *   their own cognitive architecture. This makes childhood magical thinking a
 *   canonical natural law constraint — it exhibits the signature properties:
 *   zero degrees of freedom for the agent, emergence from biological
 *   substrate rather than social choice, complete accessibility collapse (no
 *   alternatives available at that developmental stage), and minimal
 *   suppression mechanism (no coercion is possible because the constraint is
 *   intrinsic to cognition).
 *
 * KEY AGENTS:
 *   - The Child: The agent experiencing the constraint (moderate/constrained at parent's time horizon, powerless/trapped at their own cognitive horizon) — not a victim but a developmentally-staged agent whose cognitive capacities unfold according to neurobiological timetable
 *   - Parents and Educators: Institutional actors (moderate/constrained) — must work within the child's developmental stage rather than against it; experience magical thinking as an immutable constraint on pedagogical possibilities
 *   - Developmental Psychologists: Observers (analytical/analytical) — map the invariant stages of magical thinking across populations and contexts; confirm the constraint's universality
 *   - Neuroscientists: Analytical observers (analytical/analytical) — identify the biological substrate (prefrontal maturation, synaptic pruning) that instantiates the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(childhood_magical_thinking_development, 0.08).
domain_priors:suppression_score(childhood_magical_thinking_development, 0.02).
domain_priors:theater_ratio(childhood_magical_thinking_development, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(childhood_magical_thinking_development, extractiveness, 0.08).
narrative_ontology:constraint_metric(childhood_magical_thinking_development, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(childhood_magical_thinking_development, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(childhood_magical_thinking_development, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(childhood_magical_thinking_development, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(childhood_magical_thinking_development, mountain).
narrative_ontology:human_readable(childhood_magical_thinking_development, "Childhood Magical Thinking Development").
narrative_ontology:topic_domain(childhood_magical_thinking_development, "cognitive_development/psychology").

domain_priors:emerges_naturally(childhood_magical_thinking_development).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPMENTAL PSYCHOLOGIST (MOUNTAIN) — Childhood magical thinking is an invariant stage in cognitive development across all cultures and neurotypical developmental trajectories. Emerges ~18-24 months, peaks ~3-5 years, declines as concrete operational reasoning develops. This progression is neither extractive nor suppressible — it is a structural feature of how human cognitive capacities unfold.
constraint_indexing:constraint_classification(childhood_magical_thinking_development, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CHILD'S COGNITIVE EXPERIENCE (MOUNTAIN) — From the child's internal perspective, magical thinking is not a constraint but the primary epistemic mode available during this developmental window. The child cannot exit because their cognitive architecture does not yet support causal reasoning, theory of mind, or conservation principles. No coercion required — the cognitive stage itself sets the boundary.
constraint_indexing:constraint_classification(childhood_magical_thinking_development, mountain,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PARENT/EDUCATOR (MOUNTAIN) — Parents and educators encounter magical thinking as an immutable developmental stage that cannot be suppressed or accelerated meaningfully. Attempts to force abstract reasoning (conservation tasks, counterfactual thinking) consistently fail before ~6-7 years regardless of instruction. The constraint is the child's developmental stage, not any social enforcement — it is a structural bottleneck in cognitive capacity.
constraint_indexing:constraint_classification(childhood_magical_thinking_development, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: NEUROSCIENTIST (MOUNTAIN) — Magical thinking correlates with documented developmental stages in prefrontal cortex maturation, synaptic pruning, and myelination timelines. The constraint emerges from neurobiological architecture — the myelination of frontal-parietal circuits supporting abstract reasoning is not complete until mid-childhood. Exit or suppression would require rewiring brain development itself, which is not behaviorally feasible.
constraint_indexing:constraint_classification(childhood_magical_thinking_development, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(childhood_magical_thinking_development_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(childhood_magical_thinking_development, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(childhood_magical_thinking_development, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(childhood_magical_thinking_development, ExtMetricName, E),
    domain_priors:suppression_score(childhood_magical_thinking_development, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(childhood_magical_thinking_development),
    narrative_ontology:constraint_metric(childhood_magical_thinking_development, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(childhood_magical_thinking_development, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(childhood_magical_thinking_development_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. Childhood magical thinking is not an extraction mechanism — no agent enriches themselves at another's expense. The constraint is purely developmental. The minimal non-zero value reflects that measurement has some inherent noise and that the cognitive stage does impose costs (inefficiency in reasoning, vulnerability to false causal inferences) but these are costs of cognitive development, not extraction. Suppression (0.02): Near-zero. No active enforcement is required or possible. The constraint is not maintained through coercion because it emerges from cognitive architecture itself. Theater ratio (0.15): Low. There is no performative layer — magical thinking is genuine cognitive content, not a ritual maintained for appearance. The small non-zero value reflects that parental responses to magical thinking sometimes include ceremonial/playful elements (indulging the child's magical narratives) but these are peripheral to the constraint itself. Accessibility collapse (0.92): Very high. The child has zero alternatives to magical reasoning at the ages when magical thinking dominates. There is no accessible pathway to concrete operational reasoning until the cognitive stage develops. Resistance (0.08): Very low. The constraint cannot be resisted because it is constitutive of the child's cognitive mode. Resistance would require the child to adopt reasoning modes their brain is not yet capable of supporting.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on mountain classification. There is minimal perspectival gap because the constraint is structurally invariant — the child's own experience (trapped by cognitive stage), the parent's experience (constrained by the child's stage), the psychologist's observation (universal developmental stage), and the neuroscientist's substrate (cortical maturation timelines) all point to the same immutable feature. The absence of perspectival disagreement is itself diagnostic: when a constraint classifies as mountain from all positions, the mountain classification is typically correct — the constraint is truly invariant rather than just appearing so from a particular vantage.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies to natural law constraints. Childhood magical thinking has no beneficiary-victim structure because it is not a transfer mechanism. The constraint is intrinsic to cognitive development, not a relationship between agents. There is no d value, no f(d) computation, and no asymmetric extraction — the constraint affects all agents in the specified developmental window equally.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution required. Childhood magical thinking is a uniform-type mountain constraint — it classifies identically from all perspectives. The analytical observer need not worry about mislabeling extraction as coordination or vice versa because no extraction or coordination exists. The constraint is purely developmental, emerging from biological substrate rather than social choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_variation_in_magical_thinking_expression,
    'Do cultural differences in magical thinking manifestation reflect genuine cross-cultural variability in the underlying cognitive stage or merely variation in how parents and educators respond to and frame the same developmental constraint?',
    'Longitudinal cross-cultural cognitive assessment using culture-neutral conservation and theory-of-mind tasks; controlled comparison of spontaneous magical thinking expressions vs. adult-scaffolded expressions across societies with different pedagogies',
    'If genuine variability: the mountain classification may need stratification by cultural context. If mere surface variation: the mountain classification holds universally — the constraint is cognitively invariant, cultural differences are in how adults interpret and respond to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_variation_in_magical_thinking_expression, empirical, 'Whether cultural variation reflects the constraint itself or only its expression').

omega_variable(
    suppression_via_intensive_instruction,
    'Can intensive instruction in logic, counterfactual reasoning, or formal systems meaningfully accelerate the exit from magical thinking, or does such instruction merely train performance on specific tasks while leaving core magical reasoning intact?',
    'Comparison of trained vs. untrained children on trained tasks vs. novel reasoning problems; assessment of transfer of reasoning skills across domains and contexts',
    'If suppression is possible: magical thinking is less immutable than the mountain classification suggests — it would be constrained rather than trapped. If suppression fails: magical thinking is purely developmental and resistant to intervention, confirming the mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_via_intensive_instruction, empirical, 'Whether magical thinking can be suppressed via intensive instruction').

omega_variable(
    neurodivergent_deviation_pathways,
    'Do neurodivergent children (autism spectrum, intellectual disability, specific language impairment) follow different magical thinking trajectories than neurotypical children, and if so, what does this reveal about the constraint''s mechanism?',
    'Longitudinal cognitive assessment of neurodivergent populations using same tasks as neurotypical studies; identification of which children follow atypical timelines and what accounts for the deviation',
    'If all neurodivergent groups follow similar timelines to neurotypical peers: the constraint is deeply embedded in human cognitive architecture. If timelines vary by neurodevelopmental condition: the constraint is neurotype-specific and the mountain classification applies only to neurotypical development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodivergent_deviation_pathways, empirical, 'Whether magical thinking trajectories are invariant across neurodevelopmental conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(childhood_magical_thinking_development, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_tr_t0, childhood_magical_thinking_development, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cmt_tr_t5, childhood_magical_thinking_development, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cmt_tr_t10, childhood_magical_thinking_development, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(cmt_be_t0, childhood_magical_thinking_development, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cmt_be_t5, childhood_magical_thinking_development, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(cmt_be_t10, childhood_magical_thinking_development, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(childhood_magical_thinking_development, information_standard).
narrative_ontology:affects_constraint(childhood_magical_thinking_development, childhood_theory_of_mind_development).
narrative_ontology:affects_constraint(childhood_magical_thinking_development, conservation_principle_acquisition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
