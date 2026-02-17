% ============================================================================
% CONSTRAINT STORY: goldbach_conjecture
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_goldbach_conjecture, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: goldbach_conjecture
 *   human_readable: Goldbach's Strong Conjecture
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Goldbach's Strong Conjecture asserts that every even integer greater than 2
 *   is the sum of two prime numbers. Despite being verified for all even integers
 *   up to 4 x 10^18, it remains unproven. It represents a fundamental,
 *   unchangeable structural property of integers that emerges naturally from
 *   the axioms of arithmetic. As a mathematical truth, it is a canonical
 *   example of a Mountain constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Number Theorist: Analytical observer who studies the conjecture's properties.
 *   - Computational Searcher: Agent testing the conjecture against vast numbers,
 *     experiencing its immutability directly.
 *   - Any System Bound by Arithmetic: Any logical or computational system is
 *     powerless and trapped by the conjecture's truth (or falsehood).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The conjecture itself extracts nothing; it is a statement of fact.
% The *problem of proving it* extracts cognitive effort, but that is an
% epistemic, not a structural, property of the conjecture.
domain_priors:base_extractiveness(goldbach_conjecture, 0.08).

% Rationale: The conjecture "suppresses" the existence of even integers that
% are not the sum of two primes. This is a total logical suppression. However,
% it does not suppress alternative mathematical research, so the score is low.
domain_priors:suppression_score(goldbach_conjecture, 0.05).

% Rationale: A mathematical conjecture has zero performative aspect.
domain_priors:theater_ratio(goldbach_conjecture, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goldbach_conjecture, extractiveness, 0.08).
narrative_ontology:constraint_metric(goldbach_conjecture, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(goldbach_conjecture, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(goldbach_conjecture, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(goldbach_conjecture, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(goldbach_conjecture, mountain).
narrative_ontology:human_readable(goldbach_conjecture, "Goldbach's Strong Conjecture").

% --- Binary flags ---
% This constraint is a candidate for a uniform-type Mountain.
% No sunset clause or active enforcement is applicable.

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the structure of arithmetic. Required for the
% mountain metric gate to fire.
domain_priors:emerges_naturally(goldbach_conjecture).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint (a natural law of mathematics), Goldbach's
% Conjecture does not have structural beneficiaries or victims. The concepts
% of coordination and asymmetric extraction do not apply.
% No enrichment needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it reflects a fundamental,
% unchangeable property of its domain.

% PERSPECTIVE 1: THE COMPUTATIONAL SYSTEM (POWERLESS)
% A system bound by arithmetic has no choice but to obey the conjecture.
constraint_indexing:constraint_classification(goldbach_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE RESEARCH COMMUNITY (INSTITUTIONAL)
% The community of mathematicians treats the conjecture as a fixed feature
% of the landscape they are exploring.
constraint_indexing:constraint_classification(goldbach_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which sees the conjecture as a candidate
% for a natural law of mathematics.
constraint_indexing:constraint_classification(goldbach_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goldbach_conjecture_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from multiple perspectives.
    constraint_indexing:constraint_classification(goldbach_conjecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goldbach_conjecture, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain.

test(mountain_threshold_validation) :-
    % Verify metrics are within the strict bounds for a Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(goldbach_conjecture, ExtMetricName, E),
    narrative_ontology:constraint_metric(goldbach_conjecture, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the NL profile metrics required for certification are present and valid.
    narrative_ontology:constraint_metric(goldbach_conjecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(goldbach_conjecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(goldbach_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Goldbach Conjecture is modeled as a uniform-type Mountain. Its base
 *   extractiveness (0.08) and suppression (0.05) are set to the low values
 *   characteristic of a natural law. The "extraction" often associated with
 *   it (i.e., centuries of research effort) is an epistemic property of its
 *   *unproven status*, not a structural property of the conjecture itself. The
 *   constraint is the mathematical reality, which is non-extractive.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a candidate for a fundamental mathematical
 *   truth, its classification is invariant. All agents, regardless of power,
 *   time horizon, or exit options, perceive it as an unchangeable feature of
 *   their environment. This invariance is the hallmark of a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint has no structural beneficiaries or victims.
 *   Directionality is not a relevant concept for a natural law. The system
 *   will derive a symmetric directionality (d=0.5) due to the absence of
 *   beneficiary/victim declarations, but the resulting effective extraction (χ)
 *   remains extremely low and does not alter the Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Modeling this as a Mountain correctly separates the constraint (the
 *   mathematical truth) from the human activity around it (the search for a
 *   proof). An incorrect analysis might label it a Snare, conflating the
 *   difficulty of the *problem* with the nature of the *constraint*. This
 *   model avoids that error, preserving the integrity of the classification
 *   system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_goldbach_conjecture,
    'Is the Goldbach Conjecture provable within standard axiomatic systems (e.g., ZFC), or is it independent?',
    'A proof of the conjecture, or a proof of its independence from ZFC.',
    'If provable, it is a confirmed Mountain. If independent, it is a Mountain whose truth is contingent on the chosen axiomatic system, revealing a deeper structural limit.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_goldbach_conjecture, conceptual, 'The provability or independence of the conjecture within ZFC.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(goldbach_conjecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a mathematical constant, its properties do not drift over time.
% Base extractiveness is below the 0.46 threshold for requiring this section.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for a fundamental mathematical conjecture. It has no
% coordination function or structural coupling in the sense of this framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. The absence of beneficiary/victim data correctly leads to
% a symmetric directionality calculation, which is appropriate for a natural law.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */