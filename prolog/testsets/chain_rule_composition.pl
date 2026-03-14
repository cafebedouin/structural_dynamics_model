% ============================================================================
% CONSTRAINT STORY: chain_rule_composition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chain_rule_composition, []).

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
 *   constraint_id: chain_rule_composition
 *   human_readable: Chain Rule Composition in Differential Calculus
 *   domain: mathematics/differential_calculus
 *
 * SUMMARY:
 *   The chain rule for differentiation is a fundamental theorem of calculus
 *   stating that the derivative of a composite function f(g(x)) is the
 *   product of the derivative of the outer function f evaluated at g(x) times
 *   the derivative of the inner function g evaluated at x. This constraint
 *   exhibits complete uniformity across all observational contexts: every
 *   agent, from the novice student to the theoretical mathematician to the
 *   automated differentiation algorithm, encounters the chain rule as an
 *   unchangeable structural property of function composition. There is no
 *   perspectival disagreement about whether the rule holds, no exit option
 *   that circumvents the composition law, and no reformulation that dissolves
 *   the underlying logical necessity. The constraint is a canonical example
 *   of a natural law within mathematics — not because of physical causation
 *   but because the rule follows necessarily from the definition of the
 *   derivative and the definition of function composition.
 *
 * KEY AGENTS:
 *   - Student Learning Calculus: Encounters chain rule as binding logical necessity when computing derivatives of composite functions
 *   - Mathematician Proving Theorems: Cannot avoid the chain rule structure when deriving properties of differentiable functions
 *   - Automated Differentiation System: Must encode the composition law to correctly compute gradients in computational frameworks
 *   - Analytical Observer: Sees the chain rule as logically required by foundational definitions, not as a contingent constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chain_rule_composition, 0.08).
domain_priors:suppression_score(chain_rule_composition, 0.02).
domain_priors:theater_ratio(chain_rule_composition, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chain_rule_composition, extractiveness, 0.08).
narrative_ontology:constraint_metric(chain_rule_composition, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(chain_rule_composition, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(chain_rule_composition, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(chain_rule_composition, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chain_rule_composition, mountain).
narrative_ontology:human_readable(chain_rule_composition, "Chain Rule Composition in Differential Calculus").
narrative_ontology:topic_domain(chain_rule_composition, "mathematics/differential_calculus").

domain_priors:emerges_naturally(chain_rule_composition).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT (MOUNTAIN) — Any agent attempting to compute derivatives of composite functions encounters the chain rule as an unchangeable logical necessity. The structure is immutable regardless of skill level, notation system, or pedagogical framing. No exit from the composition of functions; no alternative to the chain rule's formula.
constraint_indexing:constraint_classification(chain_rule_composition, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICIAN (MOUNTAIN) — Even the most sophisticated mathematical reasoning cannot escape the chain rule structure. Proofs of calculus theorems depend on it; reformulations in different notation systems (Leibniz, operator notation, category theory) all reduce to the same underlying composition law. The constraint is not pedagogical but structural to the definition of the derivative itself.
constraint_indexing:constraint_classification(chain_rule_composition, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — At the highest abstraction level, the chain rule follows directly from the definition of the derivative as a limit and the definition of function composition. The structure is logically required by these foundational definitions. No agent perspective can revise the logical entailment: d/dx[f(g(x))] = f'(g(x)) · g'(x) is not a contingent constraint but a mathematical truth.
constraint_indexing:constraint_classification(chain_rule_composition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chain_rule_composition_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(chain_rule_composition, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chain_rule_composition, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(chain_rule_composition, ExtMetricName, E),
    domain_priors:suppression_score(chain_rule_composition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(chain_rule_composition),
    narrative_ontology:constraint_metric(chain_rule_composition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(chain_rule_composition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(chain_rule_composition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The chain rule extracts nothing from any agent because it is not coercive — it is a logical entailment. The low value reflects that the constraint is descriptive (how derivatives compose) rather than extractive (who bears costs for computing them). Suppression (0.02): Negligible. There are no barriers to applying the chain rule because it is a free mathematical insight, not a controlled resource. Theater ratio (0.05): Negligible. The chain rule requires almost no performative display — the formula either produces correct derivatives or it does not. Accessibility collapse (0.92): Near-total. Every agent with basic calculus knowledge has complete access to the chain rule formula and can apply it. The constraint offers no hidden layers or asymmetric information advantage. Resistance (0.08): Very low. No meaningful resistance is possible — the rule cannot be resisted because it is not imposed by an external actor but is constitutive of how derivatives behave.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All three perspectives classify the constraint identically as a mountain. The powerless student, the powerful mathematician, and the analytical observer all encounter the same logical structure. This uniformity is the defining feature of a natural law within mathematics: the constraint's type does not depend on who is measuring it or what time horizon they adopt. The constraint classifies as mountain from all values of (agent_power, time_horizon, exit_options, spatial_scope).
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply to this constraint because there are no beneficiaries or victims. The chain rule does not extract resources from any agent; it describes how derivatives of composite functions relate to derivatives of component functions. All agents are symmetrically positioned relative to the constraint: none benefit, none bear costs, all are bound by the same logical necessity. This is a key signature of mountain-type constraints in mathematics — complete absence of asymmetric benefit or burden.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy trivially: it is pure mathematical truth with no extractive or coordinating overlay. The chain rule is not a rule imposed by an institution or an agent; it is a rule that follows from definitions. There is no mandatrophy to resolve because there is no pretense of coordination or coercion masking an alternative function. The constraint simply is what it claims to be: a theorem of differential calculus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_to_higher_dimensions,
    'Does the chain rule generalize consistently to functions of multiple variables, and does the multivariate Jacobian composition law have the same immutability as the univariate form?',
    'Verification that the Jacobian chain rule df/dx[f∘g](x) = Df(g(x)) · Dg(x) follows from the same derivative definition principles as the univariate case',
    'If true (which it is): the constraint extends to arbitrary dimensions and coordinate systems. The immutability is not an artifact of one-dimensional notation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalization_to_higher_dimensions, empirical, 'Generalization consistency across dimensional spaces').

omega_variable(
    foundational_formalism_dependence,
    'Does the chain rule depend on classical real analysis, or does it emerge identically in constructive mathematics, non-standard analysis, and synthetic differential geometry?',
    'Cross-formalism analysis: derive the chain rule in intuitionistic logic, in hyperreal frameworks, and in topos-theoretic differential geometry',
    'If consistent across all formalisms: the constraint is truly foundational (mountain). If divergent: the constraint is dependent on a particular foundational choice (mountain only within classical analysis, not universal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_formalism_dependence, conceptual, 'Formalism-independence of the chain rule structure').

omega_variable(
    discrete_analogue_necessity,
    'Do discrete and finite-difference analogues of function composition require an analogous composition law, or is there a discrete regime where composition can be computed without the chain rule structure?',
    'Examination of finite-difference approximations, discrete calculus on graphs, and automated differentiation: do these require composition-based differentiation rules analogous to the chain rule?',
    'If yes: the constraint extends to computational and discrete domains — immutability spans both analytic and algorithmic contexts. If no: the constraint is specific to continuous analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discrete_analogue_necessity, empirical, 'Necessity of composition law in discrete computational frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chain_rule_composition, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chain_rule_composition, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
