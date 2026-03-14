% ============================================================================
% CONSTRAINT STORY: cantor_diagonal_argument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cantor_diagonal_argument, []).

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
 *   constraint_id: cantor_diagonal_argument
 *   human_readable: Cantor's Diagonal Argument — Uncountability of the Reals
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   Cantor's diagonal argument (1891) demonstrates that the real numbers are
 *   uncountable — there exists no bijection between the natural numbers and
 *   the real numbers. The proof assumes a hypothetical enumeration of the
 *   reals as an infinite sequence, constructs a new real number by varying
 *   the diagonal entries, and derives a contradiction: the constructed number
 *   differs from every enumerated number yet is itself a real number. The
 *   argument has remained structurally unchanged for 135 years and appears in
 *   every mathematical logic text as a canonical proof. No reformulation, no
 *   alternative axiomatization, no pragmatic approximation scheme can
 *   eliminate its force. This makes it a textbook example of a mathematical
 *   natural law — a constraint that emerges from the structure of formal
 *   systems themselves rather than from contingent institutional
 *   arrangements, policy choices, or physical limitations.
 *
 * KEY AGENTS:
 *   - Mathematical community: Observers and validators (institutional/arbitrage) — benefit from clarity, unable to escape the constraint
 *   - Constructivist school: Partial dissenters (powerful/mobile) — reinterpret but cannot escape the diagonal mechanism
 *   - Finitist school: Total dissenters (powerful/mobile) — reject the framework itself, but cannot prove their framework is consistent without accepting metatheoretic argument (which uses similar diagonalization)
 *   - Formal systems themselves: The true agents (analytical/analytical) — all sufficiently powerful axiom systems contain the diagonal argument
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantor_diagonal_argument, 0.08).
domain_priors:suppression_score(cantor_diagonal_argument, 0.02).
domain_priors:theater_ratio(cantor_diagonal_argument, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantor_diagonal_argument, extractiveness, 0.08).
narrative_ontology:constraint_metric(cantor_diagonal_argument, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cantor_diagonal_argument, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cantor_diagonal_argument, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cantor_diagonal_argument, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cantor_diagonal_argument, mountain).
narrative_ontology:human_readable(cantor_diagonal_argument, "Cantor's Diagonal Argument — Uncountability of the Reals").
narrative_ontology:topic_domain(cantor_diagonal_argument, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(cantor_diagonal_argument).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRUCTIVIST MATHEMATICIAN (MOUNTAIN) — Cannot escape the diagonal argument's structural force even from constructivist foundations. The argument demonstrates a proof technique (diagonalization) that works without classical excluded middle. The uncountability conclusion may be interpreted constructively as: 'no computable enumeration of the reals is complete.' The constraint persists; only the interpretation shifts.
constraint_indexing:constraint_classification(cantor_diagonal_argument, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The diagonal argument is a self-contained logical structure. Given ZFC axioms and the definition of real numbers as infinite decimal sequences, the proof is irreducible. The argument admits no reformulation, no interpretation, no exit. It is a mathematical natural law.
constraint_indexing:constraint_classification(cantor_diagonal_argument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: FORMALIST MATHEMATICIAN (MOUNTAIN) — Even institutional mathematics, which might have the power to redefine axioms, cannot escape the constraint. Any formal system powerful enough to express arithmetic will contain a variant of the diagonal argument (Gödel's incompleteness theorems). The constraint is system-invariant.
constraint_indexing:constraint_classification(cantor_diagonal_argument, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: PRAGMATIC APPLIED MATHEMATICIAN (MOUNTAIN) — Even those seeking practical workarounds (approximations, computable reals, numerical methods) cannot escape the logical structure. Any finite algorithmic enumeration will provably miss reals — the diagonal argument proves this fact. Mobility in application doesn't liberate from the constraint.
constraint_indexing:constraint_classification(cantor_diagonal_argument, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantor_diagonal_argument_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cantor_diagonal_argument, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantor_diagonal_argument, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cantor_diagonal_argument, ExtMetricName, E),
    domain_priors:suppression_score(cantor_diagonal_argument, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cantor_diagonal_argument),
    narrative_ontology:constraint_metric(cantor_diagonal_argument, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cantor_diagonal_argument, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cantor_diagonal_argument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The diagonal argument extracts nothing from any agent — it is a pure logical structure with zero asymmetric cost to any party. No group benefits at another's expense. The slight non-zero value reflects that the proof requires axioms (ZFC) that some frameworks reject, but rejection is possible, not coercive. Suppression (0.02): Minimal. The proof is fully transparent and can be understood in principle by any trained logician. There are no hidden assumptions (other than the axiom choices), no alternative interpretations suppressed by institutional power, no counter-evidence hidden. Theater ratio (0.15): Minimal. The proof is almost entirely functional with negligible performative content. The presentation is presentation of the logical structure, not concealment. The slight non-zero value reflects pedagogical framing (how the diagonal construction is introduced) and interpretative variance (constructivist vs classical readings), but these are genuine interpretive differences, not theater masking a hidden mechanism.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the diagonal argument is invariant across all observer positions. The constructivist sees the same logical structure as the formalist — they disagree on interpretation (whether infinite sequences are 'real' or merely useful fictions) but not on the proof's validity within their respective frameworks. The pragmatist sees the same constraint as the theoretical mathematician — they can't escape it any more than the theorist can. The applied mathematician can't solve the uncountability problem by computation; the theorist can't solve it by axiom revision. All positions converge on a single classification: mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The diagonal argument has no directionality because it has no beneficiaries and no victims. It is a pure logical constraint with zero extractiveness. There is no 'd' value to compute because the constraint creates no asymmetric cost distribution. This is characteristic of mathematical natural laws — they constrain all agents equally and benefit none specifically. The argument is what it is, independent of who observes it or what they do with the information.
 *
 * MANDATROPHY ANALYSIS:
 *   TRIVIAL RESOLUTION: This constraint does not instantiate mandatrophy because it contains zero extraction (ε = 0.08, treated as essential overhead rather than extractive). The six constraint types would produce identical classifications (all mountain) from all perspectives. The perspectival uniformity is the diagnostic signature of a true natural law. No type confusion, no classification danger, no risk of mislabeling. The constraint is what mathematics says it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_necessity,
    'Is the diagonal argument''s force epistemic (all valid mathematical frameworks must agree) or merely proof-theoretic (the argument is valid within a specific axiom system)?',
    'Examine whether the diagonal argument can be expressed and proven in ALL models of arithmetic, including non-standard models, intuitionistic logic, and category-theoretic foundations.',
    'If epistemic: the constraint is a true mathematical natural law. If proof-theoretic: it''s a property of classical set theory that intuitionistic, linear, or paraconsistent mathematicians can reject.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_vs_necessity, conceptual, 'Whether the diagonal argument is a universal mathematical necessity or framework-specific').

omega_variable(
    computable_vs_abstract,
    'Does the diagonal argument prove the existence of uncomputable reals or merely that no finite enumeration algorithm can cover all reals within classical ZFC?',
    'Distinguish between: (A) the existence of a real number that no Turing machine can output (ontological claim), and (B) the non-existence of a total computable function mapping N to R (epistemic claim about algorithms).',
    'If (A): the argument constrains what kinds of objects exist in abstract mathematics. If (B): the argument constrains what algorithms can do, not what reals exist. Different models of computation resolve this differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computable_vs_abstract, conceptual, 'Whether the diagonal argument constrains ontology or just algorithmic capability').

omega_variable(
    finitism_escape,
    'Can finitist or ultra-intuitionist frameworks reject infinite sequences entirely and thereby escape the diagonal argument''s force?',
    'Formal analysis of whether finitist arithmetic (e.g., Peano + restricted induction) can be consistently extended without encountering a diagonal-like argument at the level of finite encodings.',
    'If escapable: the constraint applies only to frameworks accepting actual infinity. If inescapable: the constraint operates at a more fundamental level than just real numbers — it''s about asymmetries in enumerable vs non-enumerable collections at any cardinality level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finitism_escape, conceptual, 'Whether finitist frameworks can escape the diagonal argument').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cantor_diagonal_argument, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cant_tr_t0, cantor_diagonal_argument, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cant_tr_t150, cantor_diagonal_argument, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(cant_be_t0, cantor_diagonal_argument, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cant_be_t150, cantor_diagonal_argument, base_extractiveness, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cantor_diagonal_argument, information_standard).
narrative_ontology:affects_constraint(cantor_diagonal_argument, godel_incompleteness_first).
narrative_ontology:affects_constraint(cantor_diagonal_argument, halting_problem).
narrative_ontology:affects_constraint(cantor_diagonal_argument, transfinite_hierarchy).

% DUAL FORMULATION NOTE:
% The diagonal argument is the foundational structure for three major mathematical constraints: Gödel's incompleteness theorems (which use diagonalization to construct unprovable true statements), the halting problem (which uses diagonalization to show undecidability), and Cantor's transfinite hierarchy (which uses diagonalization to show no set can contain its own powerset). Each downstream constraint has higher extractiveness because they are applied to institutional domains (logic, computation, foundations) where the abstract result generates practical limitations. The diagonal argument itself is pure mathematics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
