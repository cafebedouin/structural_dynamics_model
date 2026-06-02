% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_busy_beaver_noncomputability, []).

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
 *   constraint_id: busy_beaver_noncomputability
 *   human_readable: Non-Computability of the Busy Beaver Function
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   The non-computability of the Busy Beaver function Σ(n) is a mathematical
 *   fact derived from Turing's halting theorem and the definition of the
 *   function itself. Σ(n) represents the maximum number of steps a halting
 *   Turing machine with n states can execute before terminating. The function
 *   grows faster than any computable function — it dominates all recursive
 *   functions. Rado proved in 1962 that no algorithm can compute Σ(n) for
 *   arbitrary n. This is not a conjecture, not a current gap in knowledge,
 *   not an engineering problem. It is a logical consequence of the
 *   definitions. The constraint has zero degrees of freedom: no
 *   reformulation, no axiom choice, no technological advance can eliminate
 *   the barrier. The constraint's empirical validation is universal and
 *   immediate — any attempt to compute Σ for large n fails not because we
 *   lack resources but because the function's definition mathematically
 *   entails uncomputability. This makes Σ's non-computability the canonical
 *   example of a Mountain constraint: it emerges naturally, exhibits
 *   accessibility collapse (agents cannot access even a general strategy for
 *   computing the function), and has near-zero resistance (no coherent
 *   alternative framing exists in mathematics).
 *
 * KEY AGENTS:
 *   - Computational Theorist: Analytical observer (analytical/analytical) — sees the logical necessity of the barrier
 *   - Algorithm Seeker: Any agent attempting computation (powerless/trapped) — faces an absolute barrier with zero escape routes
 *   - Research Institution: Collective effort (institutional/analytical) — cannot coordinate away the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.12).
domain_priors:suppression_score(busy_beaver_noncomputability, 0.03).
domain_priors:theater_ratio(busy_beaver_noncomputability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.12).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).
narrative_ontology:human_readable(busy_beaver_noncomputability, "Non-Computability of the Busy Beaver Function").
narrative_ontology:topic_domain(busy_beaver_noncomputability, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(busy_beaver_noncomputability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL THEORIST (MOUNTAIN) — The non-computability of Σ emerges directly from the definition of the function and Turing's halting theorem. No alternative formalism, no hidden assumptions, no escape route. The barrier is logical, not institutional or contingent. Universal scope; zero degrees of freedom across all computational models satisfying Church-Turing thesis.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM SEEKER (MOUNTAIN) — Any agent attempting to compute Σ(n) for arbitrary n faces an absolute barrier. Not a resource constraint, not an engineering problem, not a matter of time or funding. The function is provably uncomputable by any algorithm. The barrier is structural to the mathematical universe, independent of computational capacity or cleverness.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (MOUNTAIN) — No institution, no matter how well-funded or coordinated, can hire its way out of this constraint. The non-computability is a logical fact, not an empirical discovery or a policy choice. The constraint persists across all possible institutional arrangements and technological substrates.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_noncomputability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, ExtMetricName, E),
    domain_priors:suppression_score(busy_beaver_noncomputability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(busy_beaver_noncomputability),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(busy_beaver_noncomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint does not extract value from any agent; it simply marks a boundary that is impossible to cross. The low value reflects that this is a natural limit, not a mechanism that benefits some at the expense of others. Suppression (0.03): Minimal. The barrier does not require active enforcement or suppression of alternatives — the mathematical structure itself prevents computation. Accessibility collapse (0.92): Very high. No agent can access a strategy for computing Σ(n) because the mathematical definition entails uncomputability. The barrier is total and universal. Resistance (0.08): Very low. There is no coherent alternative to the standard proof of non-computability within classical mathematics. All attempts to formalize an escape route either violate the definitions or collapse into the same proof. Theater ratio (0.05): Nearly zero. The constraint requires no performative maintenance or institutional theater. It does not rest on interpretations or institutional practices. The proof is transparent and repeatable by anyone with knowledge of Turing theory.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for this constraint. All three perspectives — the theorist, the algorithm seeker, and the institution — converge on the mountain classification. This convergence is diagnostic: it indicates that the constraint is genuinely a natural law rather than an institutional arrangement misrepresented as natural law. The constraint imposes the same barrier from every observer position. The theoretical mathematician sees logical necessity; the applied researcher seeking to compute Σ sees an impassable wall; the institution seeking to fund computation toward this goal discovers the funding is irrelevant to the mathematical impossibility. This uniformity across perspectives is the signature of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has no beneficiaries and no victims in the standard sense. It is not a mechanism through which some agents extract from others. Rather, it is a universal limit on what all agents (at any power level, temporal horizon, or scope) can achieve through computation. The directionality formula d applies to constraints where agents have asymmetric relationships to extraction flows. Here, there is no extraction flow — there is only a boundary. The analytical observer's d ≈ 0.72 (canonical fallback) is misleading if interpreted as 'experienced extractiveness.' In this constraint's case, f(d) produces an effective extraction that is substantively zero because the constraint has no extraction mechanism. The low base_extractiveness (0.12) reflects this accurately: the constraint bounds possibility but does not benefit anyone at anyone's expense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_access_boundary,
    'Does hypercomputational access to an oracle machine or continuous-valued computation change the non-computability of Σ within classical Turing theory?',
    'Formal analysis of oracle-assisted computation models and their relationship to standard Turing completeness. Determination of whether non-Turing models are coherent extensions of classical computability or categorical rejections of it.',
    'If oracle access changes classification: the barrier is contingent on the Turing model choice, suggesting a false summit (mountain-only claim naturalizes one model as universal). If oracle access does NOT change it: the non-computability is deeper than model-dependence, confirming the mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_access_boundary, conceptual, 'Whether oracle/hypercomputation models change non-computability status').

omega_variable(
    finite_approximation_utility,
    'Do finite computable lower bounds on Σ(n) (e.g., Σ(5)=47,176,870) satisfy practical research goals sufficiently that the non-computability of the full function is a real constraint on actual inquiry, or is it a theoretical boundary with negligible practical impact?',
    'Domain-specific survey of working computability researchers: What is the actual research utility of computing exact values of Σ(n) vs. computing best-known lower bounds? How many open problems require the exact value vs. accept approximations?',
    'If exact values are practically essential: the constraint is functionally tight and research-limiting (mountain remains appropriate). If approximations suffice: the constraint is functionally loose and research is approximately liberated (suggests reclassification toward rope or piton — the non-computability exists but does not bind actual practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finite_approximation_utility, empirical, 'Practical vs theoretical impact of non-computability on research').

omega_variable(
    independence_from_axiomatics,
    'Is the non-computability of Σ dependent on the choice of formal axiom system (ZFC, PA, etc.), or is it independent of standard axiomatizations?',
    'Meta-mathematical analysis of which axiom systems can prove Σ''s non-computability. Determination of whether different axiomatizations produce different computability classes for Σ.',
    'If dependent on axiom choice: the constraint is contingent on formal commitments, not on mathematical truth itself (false summit candidate; reclassify to rope or tangled_rope reflecting the choice architecture). If independent: the constraint is foundational to mathematics regardless of axiomatization (confirms mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independence_from_axiomatics, conceptual, 'Axiom-independence of Σ non-computability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(busy_beaver_noncomputability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bb_tr_t0, busy_beaver_noncomputability, theater_ratio, 0, 0.04).
narrative_ontology:measurement(bb_tr_t50, busy_beaver_noncomputability, theater_ratio, 50, 0.05).
narrative_ontology:measurement(bb_tr_t100, busy_beaver_noncomputability, theater_ratio, 100, 0.06).

% Extraction over time
narrative_ontology:measurement(bb_be_t0, busy_beaver_noncomputability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(bb_be_t50, busy_beaver_noncomputability, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(bb_be_t100, busy_beaver_noncomputability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(busy_beaver_noncomputability, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
