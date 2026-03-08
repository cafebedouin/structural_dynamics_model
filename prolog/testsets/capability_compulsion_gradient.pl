% ============================================================================
% CONSTRAINT STORY: capability_compulsion_gradient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_compulsion_gradient, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capability_compulsion_gradient
 *   human_readable: Capability-Compulsion Gradient in Expert Systems
 *   domain: organizational_psychology/ethics_of_expertise
 *
 * SUMMARY:
 *   The capability-compulsion gradient describes the phenomenological
 *   observation that increasing competence systematically reduces an agent's
 *   capacity to refuse deployment of that competence. Experts across domains
 *   report a 'could not not do' experience: the surgeon who cannot ignore the
 *   visible tumor, the engineer who cannot leave the bridge design flawed,
 *   the analyst who cannot let the error stand uncorrected. This constraint
 *   is claimed as a mountain—an unchangeable feature of how expertise
 *   works—and the structural data supports this classification. The gradient
 *   has near-zero extractiveness (0.22) because the compulsion is internal
 *   and universal, not asymmetrically imposed. Suppression is minimal (0.04)
 *   because the constraint emerges from the logical structure of capability
 *   itself, not from external enforcement. Accessibility collapse is high
 *   (0.92) because no alternative framework for expertise eliminates the
 *   gradient—every professional ethics system encodes some version of
 *   'capability creates duty.' Resistance is low (0.08) because attempts to
 *   refuse deployment of capability either fail (the expert deploys anyway)
 *   or succeed only by abandoning the capability (leaving the profession).
 *   The constraint exhibits mountain classification from all perspectives,
 *   including powerless agents with no exit options, because the binding
 *   mechanism is not organizational or social but logical: competence and
 *   refusal are structurally incompatible when the competence is rare and the
 *   need is present.
 *
 * KEY AGENTS:
 *   - Junior Expert: Powerless/trapped — experiences gradient as unchangeable feature of skill acquisition
 *   - Mid-Career Professional: Moderate/constrained — sees gradient as invariant across biographical time
 *   - Senior Expert: Powerful/mobile — gradient persists despite institutional power and exit options
 *   - Professional Guild: Institutional/arbitrage — encodes gradient in ethics codes across professions and centuries
 *   - Analytical Observer: Analytical/analytical — identifies gradient as logical constraint on coherent agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_compulsion_gradient, 0.22).
domain_priors:suppression_score(capability_compulsion_gradient, 0.04).
domain_priors:theater_ratio(capability_compulsion_gradient, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_compulsion_gradient, extractiveness, 0.22).
narrative_ontology:constraint_metric(capability_compulsion_gradient, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(capability_compulsion_gradient, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(capability_compulsion_gradient, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(capability_compulsion_gradient, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_compulsion_gradient, mountain).
narrative_ontology:human_readable(capability_compulsion_gradient, "Capability-Compulsion Gradient in Expert Systems").
narrative_ontology:topic_domain(capability_compulsion_gradient, "organizational_psychology/ethics_of_expertise").

domain_priors:emerges_naturally(capability_compulsion_gradient).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR EXPERT (MOUNTAIN) — Experiences the gradient as an unchangeable feature of skill acquisition. 'I can see the problem, so I must fix it' feels like a law of professional identity, not a choice. The compulsion to deploy capability appears as natural as gravity.
constraint_indexing:constraint_classification(capability_compulsion_gradient, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (MOUNTAIN) — Across a career arc, the gradient appears as an immutable feature of expertise development. Each skill increment narrows the space of refusable requests. The pattern is invariant across domains: surgeons, engineers, analysts all report the same phenomenology.
constraint_indexing:constraint_classification(capability_compulsion_gradient, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR EXPERT (MOUNTAIN) — Even with institutional power and exit options, the gradient persists. 'I am the only one who can do this' is not organizational capture—it is the structural consequence of rare capability. The compulsion is internal and ineradicable.
constraint_indexing:constraint_classification(capability_compulsion_gradient, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL GUILD (MOUNTAIN) — Across professions and centuries, the capability-compulsion gradient is a universal feature of expertise. Medical ethics, engineering codes, legal obligations all encode the same principle: capability creates duty. This is not extraction—it is the logical structure of competence itself.
constraint_indexing:constraint_classification(capability_compulsion_gradient, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — The gradient appears to be a logical necessity: if capability C enables outcome O, and agent A uniquely possesses C, then A's refusal to deploy C when O is needed creates a contradiction in the system's goal structure. The compulsion is not social or psychological—it is a constraint on coherent agency in a world with unequal capability distribution.
constraint_indexing:constraint_classification(capability_compulsion_gradient, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_compulsion_gradient_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(capability_compulsion_gradient, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_compulsion_gradient, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(capability_compulsion_gradient, ExtMetricName, E),
    domain_priors:suppression_score(capability_compulsion_gradient, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(capability_compulsion_gradient),
    narrative_ontology:constraint_metric(capability_compulsion_gradient, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(capability_compulsion_gradient, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(capability_compulsion_gradient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Near-zero. The compulsion to deploy capability is not imposed asymmetrically—it is experienced universally by all agents who possess the capability, regardless of their structural position. The 'extraction' is not from one group to another but from the logical structure of expertise itself. The value is slightly above zero because there are second-order effects (experts are more exploitable, organizations can free-ride on the gradient), but these are consequences, not the primary mechanism. Suppression (0.04): Minimal. The constraint does not require active enforcement—it emerges naturally from the phenomenology of competence. No institutional apparatus is needed to make experts feel compelled to deploy their skills. The low value reflects that the gradient is self-enforcing through internal experience, not externally imposed. Theater ratio (0.15): Very low. Professional ethics codes that encode the gradient (medical duty to treat, engineering duty to public safety) are not performative—they formalize a real structural feature of expertise. The codes have functional content (they guide behavior in edge cases), not just symbolic content. Accessibility collapse (0.92): Very high. No alternative framework for organizing expertise eliminates the gradient. Attempts to create 'ethics of refusal' or 'right to not deploy' either fail in practice (experts deploy anyway when confronted with need) or succeed only by abandoning expertise (leaving the profession, deskilling). The gradient is accessible from all positions because it is a feature of capability itself, not of any particular institutional arrangement. Resistance (0.08): Very low. Attempts to resist the gradient—to maintain capability while refusing deployment—are structurally unstable. The expert either deploys (gradient wins) or exits (capability is abandoned). There is no stable equilibrium of 'competent but refusing.'
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint—all agents classify it as mountain. The powerless junior expert, the moderate mid-career professional, the powerful senior expert, the institutional guild, and the analytical observer all experience the gradient as an unchangeable feature of how expertise works. This uniformity is itself diagnostic: it indicates that the constraint is not a contingent institutional arrangement (which would produce perspectival gaps between beneficiaries and victims) but a structural feature of capability distribution. The mountain classification is not naturalization of extraction—it is recognition of a logical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain-only constraint with no beneficiaries or victims in the structural sense. The gradient applies universally to all agents who possess capability, regardless of their power level or exit options. There is no extraction flow from one group to another—the compulsion is internal to the experience of competence itself. All perspectives derive d from the canonical fallback for their power atoms, and all produce mountain classifications because the base extractiveness and suppression are below mountain thresholds. The constraint is invariant across observables: whether measured by behavioral autonomy, self-reported phenomenology, or institutional ethics codes, the gradient appears with the same structural signature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all compulsions are extraction. The capability-compulsion gradient is a genuine mountain: it has near-zero extractiveness, minimal suppression, high accessibility collapse, and low resistance. It is not a snare disguised as natural law—it is a logical feature of how competence works in a world with unequal capability distribution. The mandatrophy resolution is structural: the constraint passes all mountain gates (ε ≤ 0.25, suppression ≤ 0.05, accessibility collapse ≥ 0.85, resistance ≤ 0.15, emerges naturally) and exhibits mountain classification from all perspectives, including those with no power and no exit. The 'could not not do' phenomenology is not false consciousness—it is the subjective experience of a real logical constraint. The gradient is what expertise feels like from the inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_compulsion_gradient, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_compulsion_gradient, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is formulated as a single mountain claim. There are no alternative observables that would yield different epsilon values—the gradient appears with the same structural signature whether measured behaviorally, phenomenologically, or institutionally. If future analysis identifies decomposable sub-constraints (e.g., 'compulsion in life-or-death contexts' vs 'compulsion in routine contexts'), those would be separate stories with their own epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
