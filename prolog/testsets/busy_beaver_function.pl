% ============================================================================
% CONSTRAINT STORY: busy_beaver_function
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_busy_beaver_function, []).

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
 *   constraint_id: busy_beaver_function
 *   human_readable: Busy Beaver Function Uncomputability
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   The Busy Beaver function BB(n) is a mathematical function that computes
 *   the maximum number of steps a Turing machine with n states can execute
 *   before halting (when started on a blank tape). It is not only
 *   uncomputable — no algorithm can compute BB(n) for all n — but also
 *   exhibits growth faster than any computable function. The uncomputability
 *   does not stem from current limitations in mathematics, engineering, or
 *   computer science; it is a direct consequence of the Halting Problem. This
 *   constraint serves as a mathematical exemplar of a pure mountain: an
 *   unchangeable, observer-independent limit on what can be computed. Unlike
 *   constraints involving institutions, incentives, or coordination problems,
 *   the Busy Beaver function presents a limit that holds regardless of power,
 *   resources, or framing. Every observer — from powerless to institutional
 *   to analytical — faces the same brick wall: BB(n) cannot be computed.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Analytical observers (analytical/analytical) — can identify the limit precisely but cannot overcome it
 *   - Computational Agents: All power levels — face the uncomputability limit equally; no agent escapes through superior resources or alternative pathways
 *   - Formal Mathematics Institutions: Institutional actors (institutional/arbitrage) — maintain research programs studying BB bounds, but institutionalization cannot solve a fundamentally insoluble problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(busy_beaver_function, 0.08).
domain_priors:suppression_score(busy_beaver_function, 0.02).
domain_priors:theater_ratio(busy_beaver_function, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_function, extractiveness, 0.08).
narrative_ontology:constraint_metric(busy_beaver_function, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(busy_beaver_function, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(busy_beaver_function, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(busy_beaver_function, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(busy_beaver_function, mountain).
narrative_ontology:human_readable(busy_beaver_function, "Busy Beaver Function Uncomputability").
narrative_ontology:topic_domain(busy_beaver_function, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(busy_beaver_function).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL COMMUNITY (MOUNTAIN) — The Busy Beaver function BB(n) is fundamentally uncomputable. No algorithm can compute BB(n) for all n. This is not a contingent limitation of current mathematics or engineering — it follows from the Halting Problem. No observer, no matter their power or resources, can escape this limit through any process or alternative framing.
constraint_indexing:constraint_classification(busy_beaver_function, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL AGENCY (MOUNTAIN) — Even a powerful computational agent with arbitrary resources cannot compute BB(n) for large n. The function grows faster than any computable function. This is a structural property of computation itself, not a limitation of available hardware or algorithms. No exit option exists — mobility is constrained by the laws of computation.
constraint_indexing:constraint_classification(busy_beaver_function, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL MATHEMATICS INSTITUTIONS (MOUNTAIN) — From an institutional perspective, the Busy Beaver function represents an intrinsic frontier of formal provability. Institutional mathematics — academic departments, research programs, peer review systems — cannot institutionalize a solution to the Busy Beaver problem because the problem is not solvable within any consistent formal system. This is not a failure of institutional design; it is a fundamental limit on what institutions can accomplish through formalization.
constraint_indexing:constraint_classification(busy_beaver_function, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_function_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(busy_beaver_function, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_function, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(busy_beaver_function, ExtMetricName, E),
    domain_priors:suppression_score(busy_beaver_function, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(busy_beaver_function),
    narrative_ontology:constraint_metric(busy_beaver_function, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(busy_beaver_function, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(busy_beaver_function_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract value from any agent — it is not an asymmetric relationship. Instead, it describes a uniform limit on all computation. No agent benefits at others' expense. The small non-zero value (0.08 rather than 0.00) reflects the trivial but necessary recognition that establishing and proving the uncomputability theorem required mathematical labor and attention — the overhead of knowledge itself. Suppression (0.02): Negligible. There are no alternative pathways, no hidden possibilities, no suppressed options. The limit is transparent and applies equally to all. Theater ratio (0.05): Minimal. The constraint does not hide its nature behind performative activity. Proofs of uncomputability are direct and verifiable. No theater is needed to maintain the constraint. Accessibility collapse (0.92): High. The claim that BB(n) is uncomputable is nearly universally acknowledged within mathematics and computability theory. Counterarguments are isolated and do not gain institutional traction. Resistance (0.03): Low. There is minimal resistance to the fact of uncomputability. The mathematical community has fully internalized the Halting Problem and its consequences.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits uniformity across perspectives — all observers classify it as mountain. This uniformity is diagnostic of a true natural law rather than a contingent institutional arrangement. The mathematical community, computational agents, and formal institutions all experience the same limit with equal force. There is no perspectival gap because there is no asymmetric relationship. The Busy Beaver function constrains all agents equally, regardless of their structural position. This is the defining signature of a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is needed for this constraint because there are no beneficiaries or victims. The Busy Beaver function does not extract from some agents to benefit others. It simply imposes a uniform, universal limit on computation. All agents experience the constraint with d = 0.5 (symmetry) with respect to the constraint itself, but this is not a meaningful directionality computation — it reflects that the constraint is not about directed extraction but about the structure of computation itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_church_turing,
    'Does the Church-Turing thesis hold as a physical law, or only as a mathematical conjecture?',
    'Discovery of a physical process capable of solving the Halting Problem would falsify Church-Turing and potentially enable Busy Beaver computation through non-standard (hypercomputation or quantum) means. Alternatively, proof that all physical processes respect Church-Turing would establish it as a natural law rather than a convention.',
    'If Church-Turing is merely a conjecture: Busy Beaver may be computable through undiscovered physical mechanisms, and the mountain classification would degrade to snare or tangled_rope (the limit is institutional/contingent, not natural). If Church-Turing is a natural law: the mountain classification is unassailable across all possible futures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_church_turing, empirical, 'Whether Church-Turing thesis is physical law or mathematical convention').

omega_variable(
    oracle_access_interpretation,
    'If we grant access to an oracle for the Halting Problem, does the oracle constitute a ''solution'' to Busy Beaver computability, or does it merely push the problem to a higher level of the Arithmetical Hierarchy?',
    'Proof-theoretic analysis of whether oracle hierarchies (Turing degrees) offer genuine escape from uncomputability or merely redefine the problem. Philosophical analysis of whether non-effective oracles count as ''solutions'' in any meaningful sense.',
    'If oracles solve the problem: Busy Beaver is conditionally computable given superhuman resources, suggesting snare or tangled_rope rather than mountain. If oracles merely reframe: the mountain stands — there is no escape, only higher levels of the same fundamental limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_access_interpretation, conceptual, 'Whether oracle hierarchies offer genuine escape from uncomputability').

omega_variable(
    lower_bound_methodology_open,
    'Are the best known lower bounds on BB(n) (established through independent Turing machine simulation and theorem proving) approaching the true values, or do they plateau at some fraction of the true values?',
    'Longitudinal analysis of lower bound progression over decades. Detection of whether the rate of improvement follows a computable asymptotic or shows evidence of hitting a computational frontier.',
    'If bounds are improving monotonically: BB(n) may be partially approximable, and the constraint might be tangled_rope (imperfect knowledge of an uncomputable function). If bounds plateau: the constraint is mountain — we cannot in principle approach the true value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lower_bound_methodology_open, empirical, 'Whether lower bounds on BB(n) can be improved arbitrarily or plateau').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(busy_beaver_function, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(busy_tr_t0, busy_beaver_function, theater_ratio, 0, 0.05).
narrative_ontology:measurement(busy_tr_t50, busy_beaver_function, theater_ratio, 50, 0.05).
narrative_ontology:measurement(busy_tr_t100, busy_beaver_function, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(busy_be_t0, busy_beaver_function, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(busy_be_t50, busy_beaver_function, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(busy_be_t100, busy_beaver_function, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(busy_beaver_function, information_standard).
narrative_ontology:affects_constraint(busy_beaver_function, halting_problem_uncomputability).
narrative_ontology:affects_constraint(busy_beaver_function, rice_theorem_decidability_collapse).
narrative_ontology:affects_constraint(busy_beaver_function, godel_incompleteness_theorem).

% DUAL FORMULATION NOTE:
% The Busy Beaver function is downstream of the Halting Problem. Both are manifestations of the same fundamental uncomputability. The Busy Beaver function provides a concrete quantitative measure (the growth rate of BB(n)) that makes the Halting Problem's implications tangible. The constraint family links through the Arithmetical Hierarchy: the Halting Problem lives at level 0-1; the Busy Beaver function's non-reducibility to standard arithmetic lives higher in the hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
