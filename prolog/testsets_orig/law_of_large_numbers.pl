% ============================================================================
% CONSTRAINT STORY: law_of_large_numbers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_law_of_large_numbers, []).

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
 *   constraint_id: law_of_large_numbers
 *   human_readable: Law of Large Numbers
 *   domain: mathematics/probability_theory
 *
 * SUMMARY:
 *   The Law of Large Numbers (LLN) is a mathematical theorem stating that the
 *   sample mean of independent, identically distributed random variables
 *   converges to the expected value as sample size approaches infinity. It is
 *   the canonical example of a mountain constraint — an immutable logical
 *   necessity with zero degrees of freedom for all observers. Unlike social
 *   or physical constraints, LLN generates no perspectival gaps: all agents
 *   (gambler, casino, analyst, wealthy optimizer) experience it identically
 *   as inescapable convergence. The constraint permits no extraction (ε=0.12,
 *   reflecting only minimal epistemic cost in computing/verifying the theorem
 *   itself), no suppression (suppression=0.02, minimal institutional
 *   friction), and no theater (theater_ratio=0.05, the constraint is
 *   functionally transparent). The measurements show flat trajectories
 *   reflecting that LLN's truth content does not degrade over time — it is
 *   logically invariant, not subject to institutional drift or cyclical
 *   dynamics.
 *
 * KEY AGENTS:
 *   - The Gambler: Trapped individual (powerless/trapped) — subject to convergence regardless of strategy
 *   - The Casino or Insurance Company: Institutional beneficiary (institutional/arbitrage) — benefits from LLN guarantee but cannot escape it; arbitrage option does not permit escape from mathematical law
 *   - The Mathematical Observer: Analytical agent (analytical/analytical) — verifies the theorem; sees logical necessity rather than institutional arrangement
 *   - The Wealthy Optimizer: Powerful individual (powerful/mobile) — possesses resources and flexibility but neither circumvents the constraint nor extracts advantage from it relative to other agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(law_of_large_numbers, 0.12).
domain_priors:suppression_score(law_of_large_numbers, 0.02).
domain_priors:theater_ratio(law_of_large_numbers, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(law_of_large_numbers, extractiveness, 0.12).
narrative_ontology:constraint_metric(law_of_large_numbers, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(law_of_large_numbers, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(law_of_large_numbers, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(law_of_large_numbers, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(law_of_large_numbers, mountain).
narrative_ontology:human_readable(law_of_large_numbers, "Law of Large Numbers").
narrative_ontology:topic_domain(law_of_large_numbers, "mathematics/probability_theory").

domain_priors:emerges_naturally(law_of_large_numbers).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAMBLER (MOUNTAIN) — Cannot escape the convergence of sample means to expected values regardless of individual strategy. The constraint is immutable from this position: no betting system, no favorable conditions, no escape velocity. The statistical truth operates with zero degrees of freedom.
constraint_indexing:constraint_classification(law_of_large_numbers, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTITUTIONAL BENEFICIARY (MOUNTAIN) — Also bound by LLN. The constraint is not an exploitable asymmetry but an inescapable law that operates in their favor. Long-run profitability is mathematically guaranteed, not contingent on superior strategy or hidden extraction. The constraint protects them equally — their exit option is arbitrage, but there is no arbitrage from a law of nature.
constraint_indexing:constraint_classification(law_of_large_numbers, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Verifiable as a mathematical theorem with proof. LLN is deducible from axiomatic probability theory. No empirical measurement, no institutional friction, no alternative framing changes the logical necessity. The constraint is a feature of the logical structure of probability itself, not a social arrangement or physical contingency that could be otherwise.
constraint_indexing:constraint_classification(law_of_large_numbers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: WEALTHY OPTIMIZER (MOUNTAIN) — Even with resources to reshape sample conditions, capital to wait for convergence, or influence to change institutions, cannot escape LLN. The constraint operates orthogonally to power. Wealth and mobility provide no advantage in circumventing a mathematical law. The convergence rate may be negotiable (large-n vs small-n), but convergence itself is not.
constraint_indexing:constraint_classification(law_of_large_numbers, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(law_of_large_numbers_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(law_of_large_numbers, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(law_of_large_numbers, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(law_of_large_numbers, ExtMetricName, E),
    domain_priors:suppression_score(law_of_large_numbers, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(law_of_large_numbers),
    narrative_ontology:constraint_metric(law_of_large_numbers, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(law_of_large_numbers, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(law_of_large_numbers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. LLN generates no net extraction flow between agents — it operates symmetrically on all parties. The value captures only epistemic cost (learning the theorem, computing implications) and is distributed equally. Suppression (0.02): Negligible. The constraint is transparent and logically deducible; there are no mechanisms preventing alternative understanding or exit. Theater ratio (0.05): Near-zero. LLN is functionally, not performatively, maintained. The theorem either holds or it does not. There is no ritual, proxy metric, or institutional theater that sustains it.
 *
 * PERSPECTIVAL GAP:
 *   No gap exists. All four perspectives classify as mountain. The gambler and casino agree: convergence is immutable. The analyst confirms: the proof is sound. The wealthy optimizer observes: resources provide no escape velocity from a logical necessity. This uniformity is diagnostic of a true mountain constraint — it is invariant under all context transformations. The absence of perspectival gap is the evidence that LLN is not socially constructed, contingent, or subject to power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation does not apply in the standard form because there are no beneficiaries or victims. LLN does not extract value from one agent to another. Both the gambler and the casino are subject to identical convergence. The institutional observer derives no arbitrage. The mathematical observer derives no position advantage. This is the defining feature distinguishing mountains from all other constraint types: d is not meaningful, and χ collapses to zero because there is no power asymmetry to modulate.
 *
 * MANDATROPHY ANALYSIS:
 *   LLN resolves mandatrophy trivially: it is a pure mathematical constraint with no coordination function and no extraction mechanism. The mandatrophy question ('Is this coordination or extraction?') does not apply because LLN is neither. It is a logical necessity. The mountain classification is confirmed by: (1) ε ≤ 0.25 (actually 0.12), (2) suppression ≤ 0.05 (actually 0.02), (3) accessibility_collapse ≥ 0.85 (actually 0.92, reflecting the theorem's universal availability and transparency), (4) resistance ≤ 0.15 (actually 0.08, minimal institutional or cognitive resistance to accepting the theorem), and (5) emerges_naturally = true (the constraint is deducible from axioms, not socially constructed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_vs_mathematical_domain,
    'Does LLN classify as a pure mathematical constraint (mountain of logic) or as an empirical claim about finite populations?',
    'Distinction between the theorem statement (asymptotic behavior as n→∞) and real-world application (finite n with measurement error and non-ergodic systems). Test whether finite-n deviations or non-ergodic effects transform the classification.',
    'If pure mathematical: remains mountain indefinitely. If empirical: finite-n constraints and measurement error introduce scaffolding or piton dynamics at practical scales.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_vs_mathematical_domain, conceptual, 'Boundary between mathematical necessity and empirical applicability').

omega_variable(
    independence_assumption_fragility,
    'How robust is LLN to violations of independence and identical distribution (i.i.d.) assumptions in practice?',
    'Analysis of real data streams: correlations, non-stationarity, measurement dependencies. Test convergence rate degradation under mild violations. Establish critical threshold where LLN guarantees become probabilistic rather than certain.',
    'If robust: mountain classification holds even for near-violations. If fragile: practical systems occupy a Scaffold or Tangled Rope domain where independence violations create extraction risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_assumption_fragility, empirical, 'Robustness of convergence to i.i.d. assumption violations').

omega_variable(
    finite_time_extraction_window,
    'In finite time before convergence, can institutional actors extract rents by manipulating the variance term or convergence rate?',
    'Game theory analysis of pre-convergence extraction strategies. Quantify the rents available from controlling information about convergence rate. Measure whether this constitutes a separate Snare constraint nested within the LLN mountain.',
    'If extraction windows are large: a Snare-like constraint operates beneath the mathematical guarantee. If windows are negligible: LLN''s mountain status is operationally confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finite_time_extraction_window, empirical, 'Pre-convergence extraction opportunities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(law_of_large_numbers, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lln_tr_t0, law_of_large_numbers, theater_ratio, 0, 0.02).
narrative_ontology:measurement(lln_tr_t100, law_of_large_numbers, theater_ratio, 100, 0.05).
narrative_ontology:measurement(lln_tr_t1000, law_of_large_numbers, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(lln_be_t0, law_of_large_numbers, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lln_be_t100, law_of_large_numbers, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(lln_be_t1000, law_of_large_numbers, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(law_of_large_numbers, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
