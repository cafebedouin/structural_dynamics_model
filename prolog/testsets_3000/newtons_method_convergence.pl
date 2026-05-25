% ============================================================================
% CONSTRAINT STORY: newtons_method_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_newtons_method_convergence, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: newtons_method_convergence
 *   human_readable: Newton's Method Convergence Guarantee
 *   domain: mathematics/technological
 *
 * SUMMARY:
 *   Newton's Method convergence guarantee is a mathematical theorem, not a
 *   policy or institutional arrangement. The method finds roots of a function
 *   f(x) by iterating x_{n+1} = x_n - f(x_n)/f'(x_n), achieving quadratic
 *   convergence when the function is sufficiently smooth and the initial
 *   guess is sufficiently close to a simple root. This constraint exhibits
 *   the defining properties of a mountain: (1) it emerges from mathematical
 *   logic, not enforcement; (2) accessibility to the constraint is universal
 *   — all mathematicians, engineers, and students encounter the same theorem;
 *   (3) resistance to the constraint is zero — no one claims Newton's Method
 *   should converge differently, or that the theorem is an unfair
 *   institutional imposition; (4) base extractiveness is minimal — the
 *   theorem itself creates no asymmetric value capture. The convergence
 *   guarantee is a natural law of mathematics.
 *
 * KEY AGENTS:
 *   - The Mathematical Community: Universal observer (analytical/analytical) — all mathematicians recognize the theorem equally
 *   - The Implementer: Engineer (powerful/mobile) — works within the constraint's boundaries; cannot exceed them
 *   - The Learner: Student (moderate/constrained) — must master the theorem's conditions; cannot negotiate them away
 *   - The Algorithm Designer: Institutional actor (institutional/arbitrage) — chooses which method to use; cannot change Newton's Method's properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(newtons_method_convergence, 0.12).
domain_priors:suppression_score(newtons_method_convergence, 0.03).
domain_priors:theater_ratio(newtons_method_convergence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(newtons_method_convergence, extractiveness, 0.12).
narrative_ontology:constraint_metric(newtons_method_convergence, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(newtons_method_convergence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(newtons_method_convergence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(newtons_method_convergence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(newtons_method_convergence, mountain).
narrative_ontology:human_readable(newtons_method_convergence, "Newton's Method Convergence Guarantee").
narrative_ontology:topic_domain(newtons_method_convergence, "mathematics/technological").

domain_priors:emerges_naturally(newtons_method_convergence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICIAN / ANALYTICAL OBSERVER (MOUNTAIN) — Viewed from pure mathematics, Newton's Method convergence is a theorem with precise conditions. The quadratic convergence guarantee near a simple root is a mathematical law: given sufficient smoothness (C² continuity) and a good initial guess within the basin of attraction, convergence is provably inevitable. This is not a policy choice or institutional arrangement — it is a logical consequence of the Taylor expansion and contraction mapping principles. No suppression, no extraction, no alternatives that work better in the ideal case.
constraint_indexing:constraint_classification(newtons_method_convergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: NUMERICAL ANALYST / IMPLEMENTER (MOUNTAIN) — Even from the perspective of someone implementing Newton's Method in software, the core mathematical constraint is immutable. The conditions (smooth function, good initial guess, simple root) cannot be negotiated away. Convergence failure modes (basin of attraction boundary, multiple roots, non-smooth regions) are not suppressible — they are structural features of the mathematical space. The analyst can work around them, but cannot overcome the underlying theorem.
constraint_indexing:constraint_classification(newtons_method_convergence, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ALGORITHM DESIGNER / ENGINEER (MOUNTAIN) — From the viewpoint of computational engineers designing robust root-finding systems, Newton's Method convergence guarantee is a mountain: it defines what is possible and impossible. You cannot 'extract' extra convergence speed by institutional pressure or extraction. You can only choose: use Newton's Method in its proper domain (smooth, well-conditioned, good initial guess) where convergence is guaranteed, or use hybrid methods (Newton-Bisection, Levenberg-Marquardt) that add robustness at the cost of speed. The mathematical constraint on convergence speed and radius of convergence is fixed.
constraint_indexing:constraint_classification(newtons_method_convergence, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STUDENT / LEARNER (MOUNTAIN) — No matter what level of mathematical sophistication, the convergence guarantee is accessible and invariant. A calculus student learning Newton's Method and an expert numerical analyst both encounter the same underlying mathematical law: quadratic convergence near a simple root under smooth conditions. The law does not change based on the learner's power or position. This is the defining feature of a true mountain — universality across all observational contexts.
constraint_indexing:constraint_classification(newtons_method_convergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(newtons_method_convergence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(newtons_method_convergence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(newtons_method_convergence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(newtons_method_convergence, ExtMetricName, E),
    domain_priors:suppression_score(newtons_method_convergence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(newtons_method_convergence),
    narrative_ontology:constraint_metric(newtons_method_convergence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(newtons_method_convergence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(newtons_method_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Newton's Method creates no asymmetric value extraction because the convergence guarantee is a public mathematical fact. Anyone who understands the method has equal access to the knowledge. No actor benefits from suppressing the knowledge or enforcing artificial barriers to its use. The small nonzero value (0.12 rather than 0.00) accounts for the minimal effort required to verify the proof — there is a small threshold of mathematical training needed to fully grasp the convergence proof, creating a modest accessibility gradient. But this is not extraction in the DR sense; it is baseline epistemic entry cost. Suppression (0.03): Negligible. The theorem cannot be suppressed. It is universally taught in numerical analysis courses and is standard reference material. Its conditions are clearly documented. There are no alternatives that compete with the theorem or that would benefit from the theorem's suppression. Resistance (0.08): Minimal. No significant resistance exists to accepting the convergence guarantee. The theorem is 200+ years old and universally accepted. Some practitioners prefer alternative methods (bisection, Secant method, Levenberg-Marquardt) for specific problem classes, but they choose these freely based on the problem structure — they do not deny Newton's convergence guarantee or resist acknowledging it. Theater ratio (0.15): Very low. The convergence guarantee generates minimal performative activity. Numerical analysts may write papers about improvements or applications, but the core theorem is not dressed up or ritualized. The proof is straightforward and the conditions are clearly statable. Accessibility collapse (0.92): High. The constraint is accessible to anyone with basic calculus and linear algebra. The proof is elementary (Taylor expansion + contraction mapping). University students encounter it routinely. Universality of access is the hallmark of a mountain.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification_bottleneck exemplar (which produced all six types from different perspectives), Newton's Method convergence produces MOUNTAIN from all perspectives. This is not a perspectival gap — it is perspectival invariance. The mathematical law is the same whether viewed by a student, a numerical analyst, a pure mathematician, or an engineer. The reason is that the constraint emerges from logic, not from institutional power structures. When a constraint is truly a mountain, all observational angles converge on the same classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain constraints. Newton's Method convergence does not involve asymmetric power relationships, extraction, or suppression. All agents relate to the constraint equally — as observers of a mathematical law, not as beneficiaries or victims of an institutional mechanism. The method's properties are the same regardless of who applies it.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: Newton's Method convergence demonstrates the mandatrophy resolution for true mountains. The constraint is not contestable from any structural position. Mathematicians cannot claim the theorem is wrong, engineers cannot claim it is unfairly imposed on them, and learners cannot claim it extracts unfair value. The theorem's conditions are neither hidden nor negotiable — they are transparently stated and universally acknowledged. This is the gold standard for distinguishing natural law constraints from institutional arrangements disguised as laws. Compare with the analytical perspective in verification_bottleneck, which claims convergence lag is 'inherent to science' but turns out to be contingent institutional arrangements. Newton's Method convergence is the true inherent constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(newtons_method_convergence, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
