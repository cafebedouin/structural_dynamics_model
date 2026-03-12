% ============================================================================
% CONSTRAINT STORY: temporal_perception_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_perception_mismatch, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_perception_mismatch
 *   human_readable: Temporal Perception Mismatch in Human Threat Detection
 *   domain: cognitive_science/evolutionary_psychology/philosophy_of_mind
 *
 * SUMMARY:
 *   The temporal perception mismatch describes a structural feature of human
 *   cognitive architecture: neural threat-detection systems (amygdala,
 *   insula, anterior cingulate cortex) evolved to prioritize immediate
 *   dangers over slow-accumulating risks. This is not a policy choice,
 *   cultural norm, or institutional arrangement — it is a constraint imposed
 *   by evolutionary history operating on neural circuitry. The mismatch
 *   manifests across multiple empirical domains: hyperbolic discount rates in
 *   intertemporal choice (present bias), differential physiological arousal
 *   to acute vs chronic stressors (cortisol response asymmetry), attentional
 *   capture by sudden movement vs gradual change (change blindness for slow
 *   processes), and risk perception asymmetries (vivid rare events vs
 *   statistical accumulation). The constraint is universal across human
 *   populations, resistant to individual effort and institutional
 *   intervention, and emerges naturally from selection pressures in ancestral
 *   environments where immediate threats (predators, acute injury, social
 *   conflict) had higher fitness consequences than slow accumulation
 *   (resource depletion, environmental degradation, cumulative toxin
 *   exposure). Modern environments invert this structure: the most
 *   consequential threats are now chronic and diffuse (climate change,
 *   antibiotic resistance, soil depletion, biodiversity loss), but the
 *   perceptual architecture remains calibrated to the ancestral threat
 *   landscape.
 *
 * KEY AGENTS:
 *   - Individual Human: Universal agent (powerless/trapped) — cannot exit evolutionary architecture; experiences the mismatch as unchangeable perceptual reality
 *   - Policy Institution: Institutional actor (institutional/arbitrage) — designs interventions around the constraint but cannot eliminate it; treats temporal myopia as a fixed parameter
 *   - Collective Action Coalition: Organized groups (organized/constrained) — face the mismatch as an immutable coordination barrier when attempting long-term collective action
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as a genuine natural law rooted in evolutionary neurobiology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_perception_mismatch, 0.08).
domain_priors:suppression_score(temporal_perception_mismatch, 0.02).
domain_priors:theater_ratio(temporal_perception_mismatch, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_perception_mismatch, extractiveness, 0.08).
narrative_ontology:constraint_metric(temporal_perception_mismatch, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(temporal_perception_mismatch, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_perception_mismatch, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temporal_perception_mismatch, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_perception_mismatch, mountain).
narrative_ontology:human_readable(temporal_perception_mismatch, "Temporal Perception Mismatch in Human Threat Detection").
narrative_ontology:topic_domain(temporal_perception_mismatch, "cognitive_science/evolutionary_psychology/philosophy_of_mind").

domain_priors:emerges_naturally(temporal_perception_mismatch).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL HUMAN (MOUNTAIN) — Cannot exit evolutionary architecture. Experiences acute threat salience and chronic threat blindness as unchangeable features of perception. No institutional mediation changes the underlying neural response asymmetry.
constraint_indexing:constraint_classification(temporal_perception_mismatch, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: POLICY INSTITUTION (MOUNTAIN) — Can design interventions around the mismatch but cannot eliminate it. Institutions experience the constraint as a fixed parameter: discount rates, salience asymmetries, and temporal myopia are constants to work around, not variables to change.
constraint_indexing:constraint_classification(temporal_perception_mismatch, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COLLECTIVE ACTION COALITION (MOUNTAIN) — Organized groups attempting to coordinate around long-term threats (climate, biodiversity, pandemic preparedness) face the mismatch as an immutable coordination barrier. Coalition formation does not change individual temporal perception architecture.
constraint_indexing:constraint_classification(temporal_perception_mismatch, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal context, the temporal perception mismatch is a structural feature of evolved cognitive architecture. Amygdala-mediated threat detection evolved under selection pressures favoring immediate-threat salience; no amount of cultural evolution or institutional design eliminates the underlying neural asymmetry. This is a genuine natural law, not a naturalized contingency.
constraint_indexing:constraint_classification(temporal_perception_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_perception_mismatch_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(temporal_perception_mismatch, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_perception_mismatch, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temporal_perception_mismatch, ExtMetricName, E),
    domain_priors:suppression_score(temporal_perception_mismatch, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temporal_perception_mismatch),
    narrative_ontology:constraint_metric(temporal_perception_mismatch, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temporal_perception_mismatch, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temporal_perception_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes costs (difficulty coordinating around long-term threats, present bias in decision-making, vulnerability to slow-accumulating harms) but these are not extraction in the DR sense — no agent benefits asymmetrically from the constraint's existence. The costs are symmetric and universal. The small non-zero value reflects measurement noise and the fact that some agents (those with longer natural time horizons, lower discount rates, or better executive function) bear slightly lower costs than others, creating minor asymmetry. Suppression (0.02): Negligible. The constraint does not suppress alternatives through coercion — it is a perceptual limit, not an enforcement mechanism. Individuals and institutions can build compensatory systems (commitment devices, institutional memory, long-term planning frameworks) but these work around the constraint rather than being suppressed by it. The non-zero value reflects that the perceptual asymmetry makes certain cognitive strategies (sustained attention to gradual change, intuitive grasp of exponential growth, visceral response to statistical risk) structurally harder to access. Theater ratio (0.05): Negligible. There is no performative overlay — the constraint is its function. Neural response asymmetries are directly measurable via fMRI, skin conductance, and behavioral choice. Accessibility collapse (0.92): Very high. Across all measurement methodologies (neuroscience, behavioral economics, evolutionary psychology, philosophy of mind), the constraint converges to the same structural description: evolved threat-detection architecture prioritizes immediate over chronic dangers. Resistance (0.08): Very low. Attempts to eliminate the constraint (cognitive training, institutional design, cultural evolution) produce marginal improvements in compensatory strategies but do not change the underlying neural asymmetry. The constraint persists across all known human populations and developmental stages.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspectival invariance rather than a gap: all perspectives classify as mountain. The individual human sees an unchangeable perceptual limit. The policy institution sees a fixed parameter to design around. The collective action coalition sees an immutable coordination barrier. The analytical observer sees a genuine natural law rooted in evolutionary neurobiology. The uniformity across perspectives is diagnostic: when a constraint classifies as mountain from powerless/trapped, institutional/arbitrage, organized/constrained, and analytical/analytical contexts, and when base extraction is ≤ 0.25, suppression is ≤ 0.05, accessibility collapse is ≥ 0.85, and resistance is ≤ 0.15, the constraint is a genuine natural law, not a naturalized contingency. The temporal perception mismatch passes all gates for the mountain classification and shows no perspectival variation that would indicate hidden extraction or institutional contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims in the structural sense. All agents experience the constraint as a fixed perceptual limit. Directionality values default to the analytical canonical (d ≈ 0.72) for all perspectives because there is no extraction flow to differentiate agents. The constraint imposes symmetric costs: everyone faces the same temporal perception mismatch regardless of power, exit options, or scope. The slight variation in experienced cost (some individuals have lower discount rates, some institutions have better long-term planning capacity) does not constitute asymmetric extraction — it reflects variation in compensatory capacity, not differential positioning relative to an extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates what a genuine natural law looks like in the DR framework. It is NOT a policy choice being naturalized (no institutional enforcement mechanism). It is NOT a coordination problem being misidentified as immutable (no beneficiaries capturing rents). It is NOT a cultural norm being universalized (cross-cultural invariance in neural architecture). It is a structural feature of evolved cognitive systems, measurable via multiple independent observables (neural imaging, behavioral choice, physiological response), resistant to intervention, and universal across human populations. The constraint resolves the mandatrophy by showing that some limits are real: not all mountains are false summits. The temporal perception mismatch is a constraint imposed by evolutionary history on neural circuitry, and no amount of institutional design, cultural evolution, or individual effort eliminates the underlying asymmetry. Compensatory strategies exist (commitment devices, institutional memory, statistical training, visualization tools) but these work around the constraint rather than removing it. The mountain classification is correct from all perspectives because the constraint genuinely has no degrees of freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_perception_mismatch, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The temporal perception mismatch is a single constraint with a single stable epsilon value across all observables. Neural imaging (fMRI asymmetry in amygdala response to acute vs chronic threat cues), behavioral economics (hyperbolic discount rates in intertemporal choice), evolutionary psychology (fitness consequences of immediate vs delayed threats in ancestral environments), and philosophy of mind (phenomenology of temporal salience) all converge on the same structural description. This is not a constraint family requiring decomposition — it is a unitary natural law with multiple measurement methodologies that yield consistent results.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
