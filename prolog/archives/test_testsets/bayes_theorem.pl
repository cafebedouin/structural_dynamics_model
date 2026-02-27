% ============================================================================
% CONSTRAINT STORY: bayes_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bayes_theorem, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bayes_theorem
 *   human_readable: Bayesian Reasoning Bias (Base Rate Neglect)
 *   domain: social/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the cognitive bias of base rate neglect, where
 *   individuals systematically underutilize general statistical information
 *   (base rates) in favor of specific, often more vivid, case information.
 *   This deviation from normative Bayesian reasoning is a robust feature of
 *   human cognition, often explained by dual-process theories where fast,
 *   intuitive 'Type 1' thinking overrides slow, deliberate 'Type 2' analysis.
 *   The constraint is not an external rule but an internal, emergent property
 *   of the human cognitive apparatus.
 *
 * KEY AGENTS:
 *   - Untrained Individuals: The subjects of the bias (e.g., jurors, patients, investors), who experience it as their natural mode of judgment.
 *   - Manipulators: Agents who exploit the bias for strategic gain (e.g., marketers, demagogues, trial lawyers).
 *   - Mitigators: Agents who design systems or educational programs to counteract the bias (e.g., statisticians, UI/UX designers, public health officials).
 *   - Analytical Observer: The cognitive scientist or statistician who studies the bias as a formal object.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bayes_theorem, 0.08).
domain_priors:suppression_score(bayes_theorem, 0.04).
domain_priors:theater_ratio(bayes_theorem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bayes_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(bayes_theorem, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(bayes_theorem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bayes_theorem, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(bayes_theorem, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bayes_theorem, mountain).
narrative_ontology:human_readable(bayes_theorem, "Bayesian Reasoning Bias (Base Rate Neglect)").
narrative_ontology:topic_domain(bayes_theorem, "social/cognitive_science").

domain_priors:emerges_naturally(bayes_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNTRAINED INDIVIDUAL (MOUNTAIN) — For an individual without statistical training, the bias is an inescapable feature of their own cognition. It's not a rule imposed on them; it's the path of least resistance for their thinking. They are trapped within their own intuitive judgments, making this a Mountain from their perspective.
constraint_indexing:constraint_classification(bayes_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MANIPULATOR (MOUNTAIN) — An agent who exploits the bias (e.g., a marketer using a vivid testimonial over statistics) also perceives the bias itself as a Mountain. It is a reliable, unchangeable feature of the human landscape they can leverage. The low base extractiveness (ε=0.08) of the bias itself means it remains a Mountain even for a beneficiary. The exploitation is a separate, downstream constraint.
constraint_indexing:constraint_classification(bayes_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MITIGATOR (MOUNTAIN) — An educator or designer working to counteract the bias sees it as a fundamental obstacle to be engineered around. They don't try to change the mountain but build paths (scaffolds) over it, like presenting data in frequencies. The underlying cognitive tendency remains a fixed part of the terrain.
constraint_indexing:constraint_classification(bayes_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a formal perspective, the bias is a well-documented, predictable deviation from normative Bayesian reasoning. It functions as a natural law of cognitive science. Its properties are stable and discoverable, meeting the criteria for a Mountain.
constraint_indexing:constraint_classification(bayes_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bayes_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(bayes_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bayes_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bayes_theorem, ExtMetricName, E),
    domain_priors:suppression_score(bayes_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(bayes_theorem),
    narrative_ontology:constraint_metric(bayes_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(bayes_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(bayes_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.08) is very low because the bias itself does not extract value; it is a cognitive error pattern. The consequences of decisions made under this bias can be highly extractive, but those are downstream effects modeled as separate constraints. Suppression (0.04) is also very low, as the bias can be consciously overcome with training and effortful application of statistical rules (Type 2 thinking); there is no external force preventing this. The constraint is classified as a Mountain because it emerges naturally from cognitive architecture (emerges_naturally=true), is the default mode of thinking (accessibility_collapse=0.90), and the path of least cognitive resistance (resistance=0.10).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification; all perspectives correctly identify the constraint as a Mountain. The difference between agents is not in how they classify the bias, but in their strategic response to it. The untrained individual is trapped by it, the manipulator leverages it as high ground, and the mitigator attempts to build routes around it. The uniform classification demonstrates the robustness of the Mountain type for fundamental cognitive or physical laws.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint has no formal beneficiaries or victims. However, in practice, those who understand and exploit the bias become de facto beneficiaries, while those subject to it without awareness become de facto victims. This dynamic is modeled by linking this Mountain to downstream constraints (Snares, Tangled Ropes) where the extraction actually occurs. The Mountain itself is symmetric; its effects are not.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a critical case for preventing mandatrophy through decomposition. An analyst might observe a person being financially ruined by a scam that exploits base rate neglect and incorrectly classify the bias itself as a Snare. This is an error. The framework correctly decomposes the situation into two linked constraints: 1) The cognitive bias, a Mountain with ε≈0.08. 2) The financial scam, a Snare with ε≈0.80, which is causally dependent on the Mountain. This separation of the underlying natural law from the contingent social system that exploits it is essential for accurate intervention design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bayes_theorem, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bayes_theorem, misinformation_virality).
narrative_ontology:affects_constraint(bayes_theorem, predatory_lending_models).
narrative_ontology:affects_constraint(bayes_theorem, forensic_evidence_fallibility).

% DUAL FORMULATION NOTE:
% This constraint models the cognitive bias itself as a Mountain of human psychology. Downstream social and economic systems that exploit this bias (e.g., marketing strategies, legal arguments) should be modeled as separate, higher-extraction constraints (Ropes, Tangled Ropes, or Snares) that are structurally dependent on this foundational Mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
