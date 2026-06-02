% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monty_hall_conditional_probability, []).

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
 *   constraint_id: monty_hall_conditional_probability
 *   human_readable: Monty Hall Problem: Conditional Probability Structure
 *   domain: mathematical/probability_theory
 *
 * SUMMARY:
 *   The Monty Hall problem is a canonical exemplar of a pure mathematical
 *   constraint: a logical structure that determines outcomes through the
 *   interaction of (1) initial uniform distribution over three equally
 *   probable door locations, (2) the host's knowledge of the prize location
 *   and commitment to reveal a non-winning door, and (3) the contestant's
 *   ability to observe which door the host opened and condition their
 *   probability estimate on this observation. The constraint is the
 *   conditional probability structure itself — P(prize behind contestant's
 *   initial door | host reveals empty door) = 1/3, therefore P(prize behind
 *   remaining door | host reveals empty door) = 2/3. This is not contingent
 *   on anyone's belief, power, resources, or preferences. No agent can
 *   negotiate with, exit from, or strategically circumvent the mathematical
 *   structure. The apparent paradox arises from human intuition treating the
 *   problem as symmetric when the problem is asymmetric: the host's knowledge
 *   breaks the symmetry and creates the update. The constraint exhibits zero
 *   degrees of freedom for classification — it is a mountain from all
 *   possible observation points.
 *
 * KEY AGENTS:
 *   - Contestant: The agent facing the choice; experiences the constraint as an immutable logical structure determining their winning probability
 *   - Host: The agent with privileged knowledge; cannot escape the conditional probability structure even from a position of complete information and control
 *   - Analytical Observer: The mathematician or probabilist analyzing the structure; observes the constraint as a theorem in probability theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monty_hall_conditional_probability, 0.08).
domain_priors:suppression_score(monty_hall_conditional_probability, 0.02).
domain_priors:theater_ratio(monty_hall_conditional_probability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, 0.08).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monty_hall_conditional_probability, mountain).
narrative_ontology:human_readable(monty_hall_conditional_probability, "Monty Hall Problem: Conditional Probability Structure").
narrative_ontology:topic_domain(monty_hall_conditional_probability, "mathematical/probability_theory").

domain_priors:emerges_naturally(monty_hall_conditional_probability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTESTANT (IMMEDIATE) — Faces an irreducible logical structure. The contestant's initial choice has probability 1/3 of being correct; the host's revelation updates this to 2/3 for switching. This is not a contingent rule the contestant can negotiate or escape. The mathematical structure determines the outcome regardless of the contestant's power, resources, or preferences.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: HOST (BIOGRAPHICAL) — Even from a position of complete control (the host knows the prize location and chooses which door to reveal), the host cannot escape the conditional probability structure. The host's knowledge creates an asymmetry in information but does NOT eliminate the mathematical constraint. If the host reveals a non-winning door (which they must), the conditional probability updates to 2/3 for switching regardless of the host's intentions or actions.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (CIVILIZATIONAL) — The constraint is a theorem in probability theory, not contingent on any particular agent's belief, action, or choice. The logical structure persists across all possible agents and all time horizons. The conditional probability formula P(prize behind door 2 | host reveals door 3) = 2/3 is a logical consequence of the initial setup and follows from Bayes' theorem.
constraint_indexing:constraint_classification(monty_hall_conditional_probability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monty_hall_conditional_probability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, ExtMetricName, E),
    domain_priors:suppression_score(monty_hall_conditional_probability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monty_hall_conditional_probability),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monty_hall_conditional_probability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monty_hall_conditional_probability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract from any agent in the economic or political sense. It is a pure informational/logical structure. The small non-zero value reflects that the constraint has a minimal 'cost' in the sense of computational complexity or epistemic effort required to understand it. Suppression (0.02): Negligible. The constraint imposes no barriers to understanding or action beyond the cognitive effort of learning probability theory. There are no institutional, legal, or physical mechanisms suppressing alternatives — only the logical necessity that if the host reveals a non-winning door, the probability structure updates accordingly. Theater ratio (0.15): Very low. The Monty Hall setup is purely functional — no performative layer obscures the mechanism. The constraint's function IS understanding conditional probability; there is no gap between what it claims to do and what it does. Accessibility collapse (0.89): Very high. Once the logical structure is understood, there is almost no alternative interpretation. The update to 2/3 is logically entailed by the setup. Resistance (0.05): Near-zero. The constraint does not resist being modeled, tested, or verified. Monte Carlo simulations confirm the 2/3 probability within sampling error. Mathematical proof is available and uncontested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap — all three perspectives produce the same classification (Mountain) from vastly different structural positions (powerless/trapped, powerful/mobile, analytical/analytical). The contestant cannot negotiate the probability structure; the host cannot exploit it to their advantage despite their knowledge advantage; the analytical observer sees it as a pure logical theorem. The absence of perspectival gap is diagnostic of a genuine natural law. The constraint's universality is the signal. Where there are multiple institutional actors with different structural relationships to the same logical structure, they all perceive the same mathematical truth, even as they may disagree on its implications or how to act on it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) does not apply to this constraint in the standard sense because there is no asymmetric extraction flowing from one agent to another. The constraint is not a mechanism for transferring resources, information advantage, or power. All agents — powerless and powerful, knowledgeable and ignorant, individual and institutional — are equally subject to the same logical structure. The contestant's 1/3 initial probability and 2/3 posterior probability are not contingent on power dynamics; the host's knowledge creates an asymmetry in information but not in who the constraint 'extracts from.' Both agents are constrained by the same mathematical fact. In the DR framework, constraints with d ≈ 0.5 (symmetric impact) or constraints where no agent is systematically advantaged or disadvantaged by the logical structure typically classify as Mountains because the structure is not about differential extraction but about invariant logical facts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_of_host_strategy,
    'Does the host''s strategy (random vs. deliberate door selection among non-winning options) affect the conditional probability that drives the constraint?',
    'Formal Bayesian analysis with different host strategies (uniform randomness over non-winning doors vs. preference for a specific non-winning door); Monte Carlo verification across strategy space',
    'If all strategies yield 2/3: the constraint is invariant across host behavior models. If strategies diverge: the constraint''s core structure persists (non-zero incentive to switch) but the exact update value depends on the host''s knowledge and behavior, making it slightly less purely natural-law. Current analysis: constraint is invariant — 2/3 holds for all valid host strategies that reveal a non-winning door.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_of_host_strategy, empirical, 'Host strategy invariance of the conditional probability update').

omega_variable(
    contestant_knowledge_state,
    'How does the contestant''s prior knowledge (about the host''s knowledge and strategy) affect the conditional probability calculation?',
    'Formal specification of contestant''s epistemic state before and after host''s reveal; Bayesian update under different knowledge assumptions',
    'If the contestant must know the host knows: the 2/3 probability is derived from this assumption. If the contestant is uncertain about the host''s knowledge: the update value changes. Current analysis: the standard Monty Hall setup assumes the contestant knows (or can infer) that the host knows and will reveal a non-winning door. Under this standard interpretation, the constraint is invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contestant_knowledge_state, empirical, 'Role of contestant''s epistemic state in the conditional probability update').

omega_variable(
    invariance_across_measurement_frameworks,
    'Is the conditional probability structure invariant across different mathematical frameworks (classical probability, Bayesian, information-theoretic)?',
    'Derive the Monty Hall outcome using classical probability, Bayesian updating, and information theory; verify equivalence of conclusions',
    'If invariant: the constraint is a deep mathematical truth, not an artifact of one framework''s assumptions. If framework-dependent: the constraint is less purely natural and more a feature of the chosen model. Current analysis: the outcome is invariant — all frameworks yield the same 2/3 posterior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(invariance_across_measurement_frameworks, empirical, 'Mathematical framework invariance of the conditional probability result').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monty_hall_conditional_probability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mhcp_tr_t0, monty_hall_conditional_probability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mhcp_tr_t50, monty_hall_conditional_probability, theater_ratio, 50, 0.15).
narrative_ontology:measurement(mhcp_tr_t100, monty_hall_conditional_probability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(mhcp_be_t0, monty_hall_conditional_probability, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mhcp_be_t50, monty_hall_conditional_probability, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(mhcp_be_t100, monty_hall_conditional_probability, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monty_hall_conditional_probability, information_standard).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, bayes_theorem_fundamentals).
narrative_ontology:affects_constraint(monty_hall_conditional_probability, conditional_probability_paradox_resolution).

% DUAL FORMULATION NOTE:
% The Monty Hall problem is often framed as a puzzle about human psychology or decision theory rather than as a pure probability constraint. This story focuses on the mathematical constraint itself — the logical structure that makes the 2/3 probability inevitable. Related constraints about the psychological barriers to understanding or accepting the result (cognitive biases, intuition resistance) are separate constraints with higher extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
