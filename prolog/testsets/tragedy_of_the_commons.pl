% ============================================================================
% CONSTRAINT STORY: tragedy_of_the_commons
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_tragedy_of_the_commons, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tragedy_of_the_commons
 * human_readable: The Tragedy of the Commons
 * domain: economic/social
 * * SUMMARY:
 * A situation where individual users of a shared resource, acting independently
 * and rationally in their own self-interest, behave contrary to the common good
 * by depleting or spoiling that resource through their collective action. The
 * constraint is the logic of ruin that emerges in an unregulated commons.
 * * KEY AGENTS:
 * - Subsistence User: Subject (Powerless), forced to over-exploit to survive.
 * - Aggressive Defector: Beneficiary (Institutional/Organized), maximizes private gain by defecting.
 * - Analytical Observer: Auditor (Analytical), models the game-theoretic structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(tragedy_of_the_commons, 0.70). % Snare extraction >= 0.46
domain_priors:suppression_score(tragedy_of_the_commons, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(tragedy_of_the_commons, 0.10).       % Low theater; this is a highly functional dynamic.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(tragedy_of_the_commons, extractiveness, 0.70).
narrative_ontology:constraint_metric(tragedy_of_the_commons, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(tragedy_of_the_commons, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Hardin's original framing presents it as an inevitable, mathematical outcome.
narrative_ontology:constraint_claim(tragedy_of_the_commons, tangled_rope).
narrative_ontology:human_readable(tragedy_of_the_commons, "The Tragedy of the Commons").
narrative_ontology:topic_domain(tragedy_of_the_commons, "economic/social").

% Binary flags
% To solve the tragedy, "mutual coercion, mutually agreed upon" is required.
domain_priors:requires_active_enforcement(tragedy_of_the_commons).

% Structural property derivation hooks:
% Beneficiaries exist (those who defect first/hardest), implying a coordination function to manage them.
narrative_ontology:constraint_beneficiary(tragedy_of_the_commons, aggressive_defector).
% Victims exist (the community and future generations), implying asymmetric extraction.
narrative_ontology:constraint_victim(tragedy_of_the_commons, collective_community).
narrative_ontology:constraint_victim(tragedy_of_the_commons, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBSISTENCE USER (SNARE)
% Forced by the structure to participate in the destruction of the very resource
% they rely on. Failing to add another animal guarantees starvation today, while
% adding one only slightly hastens eventual starvation for all.
constraint_indexing:constraint_classification(tragedy_of_the_commons, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE REGULATORY BODY (ROPE)
% Views the tragedy as a coordination problem to be solved with the "Rope" of
% "mutual coercion, mutually agreed upon." They create and enforce rules (quotas,
% taxes) to align individual incentives with the long-term health of the commons.
constraint_indexing:constraint_classification(tragedy_of_the_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system has a clear coordination function (managing the resource) but also
% enables severe asymmetric extraction by defectors. It requires active enforcement
% to prevent collapse. This combination is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE OSTROM-STYLE COMMUNITY (ROPE)
% For an organized local community with high social capital, the tragedy is a
% solvable coordination challenge (a Rope) managed through social norms, mutual
% monitoring, and graduated sanctions, avoiding the need for external enforcement.
constraint_indexing:constraint_classification(tragedy_of_the_commons, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tragedy_of_the_commons_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(tragedy_of_the_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.70) is high because an individual defector captures
 * 100% of the marginal benefit of over-consumption, while socializing the cost
 * across all users. This creates a powerful incentive to defect. The suppression
 * score (0.40) is moderate; while cooperation is suppressed by the payoff
 * matrix, alternatives like privatization or Ostrom-style governance are known.
 *
 * The key insight is the perspectival gap. For the powerless user, it's a Snare;
 * they are trapped in a self-destructive loop. For an institutional regulator or
 * an organized community, it's a Rope; a coordination problem to be solved.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The analytical classification is Tangled Rope, not Mountain. While Hardin
 * framed the logic as an inescapable "tragedy," the existence of solutions
 * (regulation, social norms) proves it is a constructed system, not a natural
 * law. It is a Tangled Rope because it possesses both a genuine coordination
 * function (how to manage the commons) and enables severe asymmetric extraction
 * (by defectors). Acknowledging both facets prevents misclassifying it as
 * either pure coordination (Rope) or pure, unchangeable law (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Can human cooperative norms scale to global problems without coercive hierarchy?
omega_variable(
    omega_tragedy_of_the_commons,
    'Can human cooperative norms (Ostrom-style solutions) scale to global scope without top-down institutional enforcement?',
    'Observation of global climate agreements and their enforcement rates over decades.',
    'If Yes: Global commons are a Rope. If No: Global commons are a Tangled Rope requiring coercive enforcement to function.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(tragedy_of_the_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the intensification of the tragedy as the resource becomes scarcer.
% The potential for extraction increases as competition for the last remaining
% units of the resource grows more fierce. Theater remains low.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(tragedy_of_the_commons_tr_t0, tragedy_of_the_commons, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tragedy_of_the_commons_tr_t5, tragedy_of_the_commons, theater_ratio, 5, 0.08).
narrative_ontology:measurement(tragedy_of_the_commons_tr_t10, tragedy_of_the_commons, theater_ratio, 10, 0.10).

% Extraction over time (intensifies as resource depletes):
narrative_ontology:measurement(tragedy_of_the_commons_ex_t0, tragedy_of_the_commons, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(tragedy_of_the_commons_ex_t5, tragedy_of_the_commons, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(tragedy_of_the_commons_ex_t10, tragedy_of_the_commons, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This is a classic resource allocation problem.
narrative_ontology:coordination_type(tragedy_of_the_commons, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */