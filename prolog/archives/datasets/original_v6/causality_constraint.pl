% ============================================================================
% CONSTRAINT STORY: causality_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_causality_constraint, []).

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
 *   constraint_id: causality_constraint
 *   human_readable: The Causal Structure of Observable Reality
 *   domain: metaphysics/epistemology/physics
 *
 * SUMMARY:
 *   Causality—the structural relationship in which effects follow from prior
 *   causes—is a fundamental constraint on how reality operates and how agents
 *   reason about it. Unlike institutional or coordination constraints,
 *   causality is not extractive or extractive-avoidable; it is a feature of
 *   reality itself that no agent or institution can exit or circumvent. The
 *   constraint appears identically as a mountain (immutable natural law) from
 *   all perspectives: physical theorists, individual agents, institutions,
 *   and analytical observers all find themselves bound by the principle that
 *   actions have consequences determined by prior conditions. The constraint
 *   does not distribute extraction asymmetrically, does not require
 *   suppression of alternatives (alternatives are impossible), and does not
 *   rely on theater or performative maintenance. It simply is. The theater
 *   ratio (0.15) reflects minimal performative content—causal discourse is
 *   remarkably direct and functional. The extractiveness (0.12) is near the
 *   noise floor, indicating the constraint operates almost entirely through
 *   structural necessity rather than resource capture. Suppression (0.03) is
 *   negligible because there are no alternatives to suppress.
 *
 * KEY AGENTS:
 *   - Physical Reality: The fundamental agent bearing no extractive relationship—causality is a property of how reality operates, not a social artifact
 *   - All Agents (Collective): Any agent that acts, reasons, or predicts presupposes causal structure; all bear equal structural relationship to the constraint
 *   - Analytical Observers: Must reason within causal frameworks; cannot escape the constraint even when analyzing it
 *   - Physics Theories: Formalized systems for capturing causal relationships; success measured by predictive power that depends on causal structure holding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(causality_constraint, 0.12).
domain_priors:suppression_score(causality_constraint, 0.03).
domain_priors:theater_ratio(causality_constraint, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(causality_constraint, extractiveness, 0.12).
narrative_ontology:constraint_metric(causality_constraint, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(causality_constraint, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(causality_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(causality_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(causality_constraint, mountain).
narrative_ontology:human_readable(causality_constraint, "The Causal Structure of Observable Reality").
narrative_ontology:topic_domain(causality_constraint, "metaphysics/epistemology/physics").

domain_priors:emerges_naturally(causality_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a civilizational/universal perspective, causality appears as a fundamental constraint on how reality operates. Any agent reasoning about consequences, planning actions, or making predictions presupposes causal structure. The constraint is logically and empirically unavoidable across all observational contexts.
constraint_indexing:constraint_classification(causality_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even agents with significant power and mobility cannot exit causal structure. Physics itself is built on identifying causal relationships between initial conditions and outcomes. No amount of institutional power or observational mobility allows escape from the constraint that effects follow causes.
constraint_indexing:constraint_classification(causality_constraint, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% An individual agent with no power and no exit options experiences causality as an immutable constraint on their agency. Their actions have consequences; those consequences follow from prior conditions. The constraint permits no exit.
constraint_indexing:constraint_classification(causality_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Institutions making decisions under uncertainty must presuppose causal relationships between policy interventions and outcomes. Even with institutional power and arbitrage options, the constraint that actions have consequences—determined by underlying causal laws—is inescapable.
constraint_indexing:constraint_classification(causality_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(causality_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(causality_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(causality_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(causality_constraint, ExtMetricName, E),
    domain_priors:suppression_score(causality_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(causality_constraint),
    narrative_ontology:constraint_metric(causality_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(causality_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(causality_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Causality extracts nothing from any agent. The constraint operates through logical and empirical necessity, not through resource capture. The small non-zero value reflects measurement uncertainty and the challenge of isolating the pure constraint from any institutional framing that might accompany its description. Suppression (0.03): Minimal. There are no meaningful alternatives to suppress—acausal action is logically incoherent, and agents have no meaningful freedom to violate causal structure. The negligible value reflects that suppression typically means preventing awareness of alternatives; no such prevention is needed when alternatives are impossible. Theater ratio (0.15): Low. Causal discourse is remarkably direct. Physicists describe causal relationships; engineers design systems exploiting causal relationships; agents plan based on causal expectations. Minimal performative content surrounds the actual constraint. The small non-zero value reflects that even describing causality requires some conceptual apparatus and language choice, which introduce minor theatrical elements, but these are distinctly secondary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is unique in the DR system: there is NO perspectival gap. All observers—regardless of power, time horizon, exit options, or spatial scope—classify causality identically as mountain. A powerless individual and a powerful institution experience causality with the same force. An observer at immediate time scale and one at civilizational time scale find causality equally immutable. This uniformity is diagnostic: it indicates that the constraint is genuinely fundamental rather than institutional. The analytical observer does not perceive a false summit because there is no alternative interpretation from which to view the summit. Causality simply is, from all vantage points.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis is inapplicable to causality in the standard sense. The constraint has no beneficiary—no agent profits from causality's existence. It has no victim—no agent bears costs beyond the mere fact that they exist in a causal universe. The constraint is symmetric across all agents and all power positions. This symmetry is the defining feature of a natural law constraint: it does not distribute extraction; it structures the space in which extraction could theoretically occur.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: The causality constraint resolves all mandatrophy concerns by its absolute uniformity. There is no risk of misclassifying extraction as coordination because causality provides neither extraction nor coordination—it is the substrate on which both are possible. There is no risk of false summit because the analytical observer has no alternative framing from which to reinterpret the constraint. The only residual ambiguity is whether causality itself is monolithic (a single unified constraint at all scales) or emergent-in-appearance (a multi-scale phenomenon with potentially different causal architectures at quantum vs classical vs cosmological scales). This is captured in omega variables. The mandatrophy is fully resolved: causality is a mountain, not because of institutional claim, but because exit is literally impossible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_measurement_collapse,
    'Does measurement-induced collapse in quantum mechanics represent a genuine breach of classical causality, or does it preserve causal structure in a way that classical intuition fails to capture?',
    'Empirical investigation of decoherence mechanisms, ontological interpretations of quantum mechanics, and relationship between information gain and state evolution',
    'If collapse is ontologically real: causality permits acausal correlations under quantum conditions. If decoherence is the complete picture: causality is preserved; apparent acausality is measurement-context-dependence. If relational interpretation holds: causality is relational but still structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_measurement_collapse, empirical, 'Whether quantum measurement collapse violates or preserves causal structure').

omega_variable(
    temporal_directionality_origin,
    'Why does causality have a temporal direction (causes preceding effects) when the underlying physical laws are time-reversible?',
    'Entropy analysis, boundary conditions at the Big Bang, retrocausal consistency, and whether temporal asymmetry is fundamental or derived from cosmological initial conditions',
    'If temporal direction is fundamental: causality has an additional constraint (asymmetry) beyond mere effect-follows-cause. If derived from initial conditions: causality is more general than temporal directionality suggests, and retrocausality becomes coherent under appropriate boundary conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_directionality_origin, conceptual, 'The origin of temporal directionality in causal structure').

omega_variable(
    counterfactual_dependence_reduction,
    'Is causal dependence fundamentally irreducible to counterfactual conditionals, or can causality be fully grounded in counterfactual dependence without additional causal machinery?',
    'Formal analysis of overdetermination cases, preemption scenarios, and whether counterfactual models capture cases where causes and effects covary but neither determines the other',
    'If causality is irreducible: causal structure is more primitive than counterfactual logic; some causal claims cannot be expressed as conditionals. If reducible: causality is fundamentally about conditional dependence; all causal facts supervene on counterfactual facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_dependence_reduction, conceptual, 'Whether causal dependence reduces to counterfactual dependence').

omega_variable(
    emergent_causality_at_scales,
    'Does causality differ in structure at different scales (quantum, classical, cosmological), or is macro-level causality an emergence-in-appearance from a single underlying causal architecture?',
    'Scale-dependent causal analysis, effective field theory framework validation, and whether causal mechanisms differ or only the observable patterns differ across scales',
    'If genuinely multi-scale: causality is not monolithic; different physical scales may have incompatible causal structures. If unified emergent: causality is scale-invariant in structure; apparent differences reflect measurement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_causality_at_scales, empirical, 'Whether causality has a unified structure across physical scales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(causality_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caus_tr_t0, causality_constraint, theater_ratio, 0, 0.12).
narrative_ontology:measurement(caus_tr_t5, causality_constraint, theater_ratio, 5, 0.14).
narrative_ontology:measurement(caus_tr_t10, causality_constraint, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(caus_be_t0, causality_constraint, base_extractiveness, 0, 0.11).
narrative_ontology:measurement(caus_be_t5, causality_constraint, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(caus_be_t10, causality_constraint, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(causality_constraint, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
