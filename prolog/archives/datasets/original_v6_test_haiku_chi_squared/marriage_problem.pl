% ============================================================================
% CONSTRAINT STORY: marriage_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_problem, []).

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
 *   constraint_id: marriage_problem
 *   human_readable: The 37% Rule (Optimal Stopping Problem)
 *   domain: mathematical/algorithmic/decision_theory
 *
 * SUMMARY:
 *   The 37% Rule (or more precisely, the 1/e ≈ 0.368 threshold) is a
 *   mathematical law governing optimal stopping under incomplete information.
 *   In the classic 'Marriage Problem' (a variant of the Secretary Problem),
 *   an agent must select the best candidate from a sequence of N applicants
 *   without knowing future candidates' quality or having the ability to
 *   recall rejected candidates. The mathematical theorem states that the
 *   optimal strategy is to observe and reject the first ⌈N/e⌉ candidates,
 *   then accept the first candidate better than all those observed. This
 *   strategy achieves a success probability approaching 1/e as N → ∞. The
 *   constraint is not a policy choice, institutional arrangement, or power
 *   structure — it is a theorem in decision theory, probability, and optimal
 *   control. It applies universally across all sequential-choice scenarios
 *   with identical information structure. The rule's extractiveness is
 *   minimal (ε=0.08) because it represents a structural ceiling on success
 *   rates, not an agent extracting value from others. Suppression is
 *   negligible (0.02) — the constraint is transparent and requires no
 *   coercion. Theater is low (0.15) — the mathematical proof is direct and
 *   contains no performative element. This is a canonical mountain
 *   constraint.
 *
 * KEY AGENTS:
 *   - Decision-Maker: The agent conducting the sequential search (powerless/trapped) — cannot escape the mathematical limit
 *   - Candidate Pool: The N applicants (implicit, symmetric) — their ordering and ranking structure define the problem
 *   - Mathematical Community: The observer of the theoretical structure (analytical) — proves and validates the theorem
 *   - Implementation Institution: Any organization applying the rule (institutional/arbitrage) — benefits from understanding the constraint but cannot exceed its bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_problem, 0.08).
domain_priors:suppression_score(marriage_problem, 0.02).
domain_priors:theater_ratio(marriage_problem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_problem, extractiveness, 0.08).
narrative_ontology:constraint_metric(marriage_problem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(marriage_problem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_problem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(marriage_problem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_problem, mountain).
narrative_ontology:human_readable(marriage_problem, "The 37% Rule (Optimal Stopping Problem)").
narrative_ontology:topic_domain(marriage_problem, "mathematical/algorithmic/decision_theory").

domain_priors:emerges_naturally(marriage_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECISION-MAKER IN REAL TIME (MOUNTAIN) — The 37% rule is an irreducible structural constraint on choice under incomplete information. The decision-maker cannot escape the mathematical limit: if they reject more than 37% of candidates without committing, they risk rejecting the true optimum with high probability. The constraint emerges from information theory, not institutional design. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL OBSERVER (MOUNTAIN) — From the perspective of probability theory and optimal control, the 37% threshold is a mathematical law: for large N, the probability of success approaches 1/e ≈ 0.368 when using the optimal strategy (reject the first 37%, then accept the first candidate better than all seen so far). This is a proven theorem, not a negotiable institutional arrangement. The constraint is universal, scale-invariant, and irreversible. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: POWERFUL AGENT (MOUNTAIN) — Even an agent with significant resources (ability to run multiple decision cycles, hire consultants, use algorithms) cannot escape the 37% rule's structural constraint. The law applies regardless of power: gathering information in advance (via reference checking, background investigation) simply redefines what counts as 'information observed' in the sequence, but the mathematical structure persists. The agent can mitigate risk through preprocessing, but cannot repeal the fundamental trade-off between exploration and exploitation. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL ACTOR (MOUNTAIN) — Whether selecting employees, customers, or partners, an institution implementing a hiring or matching process cannot escape the 37% rule. It applies across all domains where sequential choice occurs under incomplete information. The constraint is the same whether N=10 (candidates for a hire) or N=1000 (dating pool). The institution can only change the base rate of success by changing N, not by escaping the mathematical structure. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(marriage_problem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(marriage_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_problem, ExtMetricName, E),
    domain_priors:suppression_score(marriage_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_problem),
    narrative_ontology:constraint_metric(marriage_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The 37% rule is not an extraction mechanism — it is a natural law of decision-making. It imposes a constraint on achievable success rates (max ~37% for large N), not a redistribution from one agent to another. The low value reflects that no party is being exploited; all agents face the same mathematical ceiling. Suppression (0.02): Negligible. The rule requires no coercion or enforcement. Agents are free to use any strategy they choose; the theorem simply proves which strategy is optimal. Theater ratio (0.15): Low. The mathematical derivation is rigorous and transparent. Unlike institutional constraints that maintain themselves through narrative or performance, the 37% rule's maintenance requires only publication of mathematical proofs — minimal theater. Claimed type: Mountain. The rule exhibits all four mountain signatures: (1) Emerges naturally from information-theoretic structure, not institutional design. (2) Accessibility collapse = 0.92 — understanding the theorem requires mathematical sophistication, but the result is accessible to anyone willing to learn probability theory. (3) Resistance = 0.08 — minimal resistance to the law itself, though individuals may resist accepting its implications for their own decision-making. (4) Extractiveness ≤ 0.25 and suppression ≤ 0.05 satisfied.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, the 37% rule exhibits zero perspectival gap: all four perspectives classify it identically as Mountain. This uniformity is the defining feature of a true natural law. The decision-maker in real time, the mathematical observer, the powerful agent, and the institutional actor all face the same constraint with no disagreement about its nature. The lack of gap does not indicate a weakness in the analysis — it indicates the constraint's universality. There is no tension between these perspectives because the rule applies uniformly regardless of power, exit options, or temporal horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The 37% rule does not have traditional beneficiaries or victims because it is not an extraction mechanism. No agent benefits from the constraint, and no agent is harmed by it — all agents face it equally. The rule imposes a ceiling on achievable success rates (~37% for large N), which affects all decision-makers symmetrically. This is the defining characteristic of a pure structural constraint rather than a redistribution mechanism. Each perspective derives d from its observation position: a decision-maker faces the constraint directly (d≈0.50, symmetric), the mathematical observer studies it analytically (d≈0.72), a powerful agent still cannot escape it (d≈0.48), and an institution implementing it faces it institutionally (d≈0.05, slight beneficiary through understanding). But none of these directionalities represent extraction — they represent different structural positions relative to a universal law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cardinal_vs_ordinal_information,
    'Does the 37% rule remain optimal if the decision-maker has partial cardinal information (not just ordinal rankings)?',
    'Theoretical extension to information-theoretic frameworks where candidates carry utility values; comparison of optimal thresholds under different information structures',
    'If cardinal information changes the threshold: the rule is contingent on the ordinal-ranking assumption and not a universal law. If the threshold remains ~37%: the rule''s universality is confirmed across information structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cardinal_vs_ordinal_information, empirical, 'Whether cardinal utility information preserves the 37% threshold').

omega_variable(
    horizon_finiteness_effects,
    'What happens to the 37% rule in finite-horizon settings with small N (N<10)? Does it still apply, or do discrete effects dominate?',
    'Computational solution of the optimal stopping problem for small N; comparison of analytic 37% threshold to discrete optimal policies',
    'If discrete effects dominate small N: the rule is an asymptotic approximation, not a true law. If the rule holds for small N: the mountain classification is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizon_finiteness_effects, empirical, 'Applicability of 37% rule to finite, small-N scenarios').

omega_variable(
    no_recall_assumption_necessity,
    'Is the no-recall constraint (inability to return to a rejected candidate) essential to the 37% rule, or does it hold under partial recall?',
    'Theoretical analysis and simulation of optimal stopping with k-step recall; determination of whether recall changes the structure of the optimal policy',
    'If recall eliminates the 37% threshold: the rule depends on a specific institutional constraint (no recall), not on pure mathematics. If the rule persists under partial recall: its universality is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(no_recall_assumption_necessity, empirical, 'Whether the no-recall assumption is essential to the 37% rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_problem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optim_tr_t0, marriage_problem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(optim_tr_t50, marriage_problem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(optim_tr_t100, marriage_problem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(optim_be_t0, marriage_problem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(optim_be_t50, marriage_problem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(optim_be_t100, marriage_problem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_problem, information_standard).
narrative_ontology:affects_constraint(marriage_problem, exploration_exploitation_tradeoff).
narrative_ontology:affects_constraint(marriage_problem, information_value_paradox).

% DUAL FORMULATION NOTE:
% The 37% rule is upstream in a constraint family involving sequential decision-making. The exploration-exploitation tradeoff and information value paradox are downstream manifestations of the same mathematical structure. All three constraints share the same ε range and emerge from identical information-theoretic limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
