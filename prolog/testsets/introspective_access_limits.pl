% ============================================================================
% CONSTRAINT STORY: introspective_access_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_introspective_access_limits, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: introspective_access_limits
 *   human_readable: Introspective Access Limits in Real-Time Depletion vs Avoidance Discrimination
 *   domain: applied_ethics/social_psychology/phenomenology_of_attention
 *
 * SUMMARY:
 *   The inability to reliably distinguish genuine resource depletion from
 *   motivated avoidance in real-time first-person experience represents a
 *   structural limit of introspective access. When an agent reports 'I'm too
 *   tired to engage with this conversation,' the phenomenology does not
 *   encode whether the tiredness reflects actual cognitive resource
 *   exhaustion or motivated redirection of attention away from an aversive
 *   topic. The counterfactual test — would capacity suddenly reappear if the
 *   conversation shifted to a topic of high interest? — is not runnable in
 *   real-time and remains ambiguous even in retrospect. This constraint
 *   appears as a mountain (natural law of first-person epistemology) from
 *   most perspectives, but the presence of identifiable beneficiaries (agents
 *   who use depletion claims as socially acceptable exits from unwanted
 *   obligations) triggers the false summit detector. The question is whether
 *   this is an irreducible feature of embodied cognition or a constructed
 *   ambiguity maintained by those who benefit from it.
 *
 * KEY AGENTS:
 *   - The Depleted Agent (Immediate): Primary experiencer (powerless/trapped) — has no reliable introspective access to resolve the ambiguity in the moment
 *   - The Reflective Agent (Biographical): Retrospective analyst (moderate/constrained) — can recognize patterns but cannot retrodict specific episodes with confidence
 *   - Clinical Psychology Community: Research collective (organized/mobile) — attempts third-person resolution through physiological and behavioral measures; faces replication crisis in ego depletion research
 *   - Phenomenological Tradition: Institutional knowledge base (institutional/arbitrage) — has converged on introspective limits as a structural feature after centuries of investigation
 *   - Avoidance Rationalizers: Beneficiary group (institutional/arbitrage) — agents who use the ambiguity to exit unwanted obligations without social cost; experience the constraint as coordination (rope)
 *   - Analytical Observer: Meta-level analyst (analytical/analytical) — evaluates whether the constraint is a genuine natural law or a false summit maintained by beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(introspective_access_limits, 0.08).
domain_priors:suppression_score(introspective_access_limits, 0.02).
domain_priors:theater_ratio(introspective_access_limits, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(introspective_access_limits, extractiveness, 0.08).
narrative_ontology:constraint_metric(introspective_access_limits, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(introspective_access_limits, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(introspective_access_limits, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(introspective_access_limits, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(introspective_access_limits, mountain).
narrative_ontology:human_readable(introspective_access_limits, "Introspective Access Limits in Real-Time Depletion vs Avoidance Discrimination").
narrative_ontology:topic_domain(introspective_access_limits, "applied_ethics/social_psychology/phenomenology_of_attention").

domain_priors:emerges_naturally(introspective_access_limits).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(introspective_access_limits, avoidance_rationalizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPLETED AGENT IMMEDIATE (MOUNTAIN) — In the moment of claimed depletion, the agent has no reliable introspective access to distinguish genuine resource exhaustion from motivated avoidance. The phenomenology is identical: both feel like 'I cannot do this right now.' No amount of willpower or self-examination can resolve the ambiguity from within the experience. This is a structural limit of first-person access, not a correctable epistemic failure.
constraint_indexing:constraint_classification(introspective_access_limits, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: REFLECTIVE AGENT BIOGRAPHICAL (MOUNTAIN) — Even with biographical hindsight and pattern recognition, the agent cannot reliably retrodict which past episodes were genuine depletion vs avoidance. The counterfactual test ('would I have had capacity if the conversation got interesting?') is not actually runnable — the conversation did not get interesting, so the agent never knows what would have happened. Retrospective classification remains structurally ambiguous.
constraint_indexing:constraint_classification(introspective_access_limits, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: CLINICAL PSYCHOLOGY COMMUNITY (MOUNTAIN) — Organized research programs (ego depletion studies, self-control research, burnout assessment) cannot resolve the depletion/avoidance distinction through third-person measurement. Physiological correlates (cortisol, glucose, neural activation) do not cleanly separate the categories. The replication crisis in ego depletion research reflects this structural ambiguity: the phenomenon is real but the mechanism is not isolable because the first-person experience does not carve nature at its joints.
constraint_indexing:constraint_classification(introspective_access_limits, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PHENOMENOLOGICAL TRADITION (MOUNTAIN) — The philosophical tradition recognizes this as a structural feature of embodied cognition: the agent is not transparent to themselves. Introspective access has inherent limits. The inability to distinguish depletion from avoidance in real-time is not a bug to be fixed but a feature of how first-person experience works. This is a mountain from the institutional perspective because the tradition has converged on this as a limit case after centuries of investigation.
constraint_indexing:constraint_classification(introspective_access_limits, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint is a genuine natural law of first-person epistemology. The phenomenology of depletion and the phenomenology of avoidance are not reliably distinguishable from within because they share the same proximal mechanism: both involve the agent's executive function declining to allocate resources to the task. Whether the decline is due to resource scarcity (depletion) or motivated redirection (avoidance) is not encoded in the immediate experience. This is an accessibility collapse in the technical sense: the information required to make the distinction is not available to the introspecting agent at decision time.
constraint_indexing:constraint_classification(introspective_access_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: AVOIDANCE RATIONALIZER (ROPE) — Agents who benefit from the ambiguity (those who use 'I'm too tired' as a socially acceptable exit from unwanted obligations) experience the constraint as coordination: the shared inability to distinguish depletion from avoidance creates a norm where claiming depletion is not challengeable. This is a low-extraction coordination mechanism from the beneficiary's perspective — the constraint enables a face-saving exit strategy that all parties tacitly accept. The beneficiary sees rope because the constraint solves their problem (exiting without conflict) at minimal cost.
constraint_indexing:constraint_classification(introspective_access_limits, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(introspective_access_limits_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(introspective_access_limits, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(introspective_access_limits, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(introspective_access_limits, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(introspective_access_limits, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(introspective_access_limits, ExtMetricName, E),
    domain_priors:suppression_score(introspective_access_limits, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(introspective_access_limits),
    narrative_ontology:constraint_metric(introspective_access_limits, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(introspective_access_limits, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(introspective_access_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal extraction — the cost is epistemic uncertainty about one's own mental states, not material or social harm. The primary 'extraction' is the inability to self-diagnose accurately, which is a knowledge limit rather than a resource transfer. The presence of beneficiaries (avoidance rationalizers) suggests some agents capture value from the ambiguity, but the magnitude is small — the benefit is a face-saving exit strategy, not systematic rent extraction. Suppression (0.02): Minimal. Agents are not coerced into accepting the constraint — it is a structural feature of first-person experience. No enforcement mechanism is required because the constraint emerges from the architecture of introspective access itself. Theater ratio (0.15): Very low. There is minimal performative content — the constraint is not maintained through ritual or institutional theater. The ambiguity is genuine, not constructed through social performance. Accessibility collapse (0.92): Very high. The information required to distinguish depletion from avoidance is not accessible to the introspecting agent at decision time. The phenomenology does not encode the causal history of the resource allocation decision. Resistance (0.08): Very low. The constraint has been stable across cultures and historical periods. No intervention (training, therapy, pharmacology) has reliably improved real-time discrimination accuracy. Emerges naturally (true): The constraint arises from the structure of embodied cognition, not from institutional design or policy choice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a subtle perspectival gap: most agents experience it as an immutable natural law (mountain), but the presence of beneficiaries who use the ambiguity strategically suggests the possibility of a false summit. The avoidance rationalizer sees rope — the constraint solves their problem (exiting unwanted obligations) at minimal cost. The clinical psychology community sees mountain but with frustration — they recognize the limit but wish it were surmountable. The phenomenological tradition sees mountain with acceptance — this is how first-person experience works, and no amount of investigation will change it. The analytical observer must adjudicate: is this a genuine natural law with incidental beneficiaries, or a constructed constraint naturalized by those who benefit? The omega variables document this irreducible uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has an unusual directionality structure: most agents are neither clear beneficiaries nor clear victims. The primary experiencer (the depleted/avoiding agent) is not extracted from in a material sense — they simply lack reliable self-knowledge. The beneficiary group (avoidance rationalizers) is identifiable but small, and their benefit is modest (a socially acceptable exit strategy). The engine derives low d values for most perspectives because the constraint does not create a strong extraction gradient. The avoidance rationalizer perspective gets the lowest d (beneficiary + arbitrage exit) and experiences the constraint as rope. The immediate depleted agent gets moderate d (powerless + trapped, but no clear victim status) and still experiences mountain because the extraction is epistemic rather than material. The analytical observer gets canonical analytical d and experiences mountain because the structural data supports the natural law hypothesis despite the presence of beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN WITH FSM TRIGGER: This constraint is classified as mountain by all perspectives except the beneficiary (rope), but the presence of identifiable beneficiaries triggers the false summit detector. The mandatrophy resolution is that the constraint is LIKELY a genuine natural law (the structural evidence strongly supports the mountain classification) but the beneficiary presence creates irreducible uncertainty. The omega variables document three empirical tests that could resolve the ambiguity: (1) cross-cultural phenomenological studies to test whether the access limit is universal or culturally constructed, (2) experimental validation of the counterfactual interest test, and (3) physiological marker studies to test whether third-person measurement can bypass the first-person limit. If all three tests confirm the mountain hypothesis, the beneficiaries are incidental (they exploit a natural law but do not maintain it). If any test disconfirms, the constraint may be a false summit — a constructed limit naturalized by those who benefit from the ambiguity. The current evidence leans toward genuine mountain, but the FSM trigger correctly flags the constraint for ongoing scrutiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the introspective access limit a genuine natural law of first-person epistemology, or a constructed constraint that benefits agents who rationalize avoidance as depletion?',
    'Cross-cultural phenomenological studies; investigation of whether training (meditation, metacognitive therapy, attention regulation practices) can improve real-time discrimination accuracy; longitudinal tracking of agents who claim the distinction is learnable',
    'If genuine mountain: the constraint is an irreducible feature of embodied cognition and no intervention can resolve it. If constructed: the constraint is maintained by agents who benefit from the ambiguity, and targeted training could improve discrimination accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, empirical, 'Whether introspective access limits are natural law or constructed').

omega_variable(
    counterfactual_test_validity,
    'Is the counterfactual test (''would capacity reappear if the conversation got interesting?'') a valid diagnostic, or does it conflate motivation with capacity?',
    'Experimental manipulation: present agents claiming depletion with high-interest tasks and measure performance; compare to baseline capacity measures; control for Hawthorne effects and demand characteristics',
    'If valid: the counterfactual test can retrospectively classify episodes, reducing the constraint''s extractiveness. If invalid: the test itself is contaminated by the same ambiguity it attempts to resolve, and the mountain remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_test_validity, empirical, 'Whether counterfactual interest test reliably distinguishes depletion from avoidance').

omega_variable(
    physiological_marker_sufficiency,
    'Do physiological markers (cortisol, glucose, HRV, neural activation patterns) provide sufficient third-person evidence to resolve the first-person ambiguity?',
    'Meta-analysis of ego depletion and self-control studies correlating physiological measures with task performance; identification of reliable biomarkers that predict capacity independent of self-report',
    'If sufficient: third-person measurement can bypass the first-person access limit, converting the mountain into a measurement problem. If insufficient: the ambiguity is irreducible even with full physiological data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physiological_marker_sufficiency, empirical, 'Whether physiological markers can resolve depletion/avoidance distinction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(introspective_access_limits, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intro_tr_t0, introspective_access_limits, theater_ratio, 0, 0.15).
narrative_ontology:measurement(intro_tr_t50, introspective_access_limits, theater_ratio, 50, 0.15).
narrative_ontology:measurement(intro_tr_t100, introspective_access_limits, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(intro_be_t0, introspective_access_limits, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(intro_be_t50, introspective_access_limits, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(intro_be_t100, introspective_access_limits, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(introspective_access_limits, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a singleton — it does not decompose into multiple stories with different epsilon values. The depletion/avoidance distinction is a single structural ambiguity, not a family of related claims. If future analysis identifies separable sub-constraints (e.g., physical depletion vs cognitive depletion vs emotional depletion), those would be modeled as distinct stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
