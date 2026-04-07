% ============================================================================
% CONSTRAINT STORY: self_reference_in_type_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_reference_in_type_theory, []).

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
 *   constraint_id: self_reference_in_type_theory
 *   human_readable: Self-Reference Prohibition in Type Theory
 *   domain: mathematical_logic/type_theory
 *
 * SUMMARY:
 *   Self-reference in type theory is the prohibition against constructing a
 *   type that directly references itself in its own definition. This
 *   constraint emerges from the mathematical requirement to preserve
 *   consistency and decidability in formal systems. A type A cannot have a
 *   term of type A in its own constructor — doing so would permit infinite
 *   regress and violate the logical stratification that makes type systems
 *   sound. The constraint is not an institutional rule, a policy decision, or
 *   a design choice subject to negotiation. It is an irreducible mathematical
 *   law that follows necessarily from the axioms of type theory. Any attempt
 *   to circumvent it either fails to produce a valid type (the proposed
 *   definition is rejected during type-checking) or secretly introduces
 *   hidden stratification (universe levels, impredicative quantification)
 *   that preserves the prohibition while making it less obvious. The
 *   theatrical content is minimal — type checkers enforce the prohibition as
 *   a direct logical consequence, not as a performative ritual. The
 *   accessibility collapse is near-total: there is no way to make
 *   self-referential types 'work' within a sound system without abandoning
 *   consistency or decidability.
 *
 * KEY AGENTS:
 *   - Type Theoretic Learner: Primary victim of the constraint (powerless/trapped) — cannot construct desired self-referential structures; prohibition is absolute
 *   - Formal Verification Community: Institutional beneficiary (institutional/arbitrage) — the stratification ensures decidable type checking and proof automation
 *   - Logic Researchers: Analytical observer (analytical/analytical) — understand the prohibition as a necessary consequence of consistency requirements
 *   - Proof Assistants (Coq, Lean, Agda): Institutional enforcers (institutional/arbitrage) — enforce the stratification, but enforcement follows from mathematics, not from policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_reference_in_type_theory, 0.18).
domain_priors:suppression_score(self_reference_in_type_theory, 0.04).
domain_priors:theater_ratio(self_reference_in_type_theory, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_reference_in_type_theory, extractiveness, 0.18).
narrative_ontology:constraint_metric(self_reference_in_type_theory, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(self_reference_in_type_theory, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(self_reference_in_type_theory, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(self_reference_in_type_theory, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_reference_in_type_theory, mountain).
narrative_ontology:human_readable(self_reference_in_type_theory, "Self-Reference Prohibition in Type Theory").
narrative_ontology:topic_domain(self_reference_in_type_theory, "mathematical_logic/type_theory").

domain_priors:emerges_naturally(self_reference_in_type_theory).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TYPE-THEORETIC LEARNER (MOUNTAIN) — Any agent attempting to construct self-referential types within a sound type system encounters an absolute barrier. The prohibition is not a policy constraint or institutional arrangement but a mathematical necessity. No exit, no workaround, no arbitrage. The constraint is immutable from the inside because it is intrinsic to the type hierarchy itself.
constraint_indexing:constraint_classification(self_reference_in_type_theory, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: FORMAL ANALYST (MOUNTAIN) — From the outside, the prohibition on self-referential types is an irreducible mathematical law. The stratification of types into levels (Type₀, Type₁, Type₂, ...) is not contingent on observer position or resource availability. It emerges necessarily from the requirements of logical consistency and decidability. The constraint has zero degrees of freedom.
constraint_indexing:constraint_classification(self_reference_in_type_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: TYPE THEORY COMMUNITY (MOUNTAIN) — Even institutional agents (proof assistants, formal verification frameworks, logic researchers) cannot arbitrage around the self-reference prohibition. Dependent types, universe polymorphism, and inductive families all operate within the hierarchy and preserve stratification. The constraint is not imposed by the community — it is entailed by the mathematics itself. The community's role is fidelity to the constraint, not enforcement of it.
constraint_indexing:constraint_classification(self_reference_in_type_theory, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_reference_in_type_theory_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(self_reference_in_type_theory, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_reference_in_type_theory, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(self_reference_in_type_theory, ExtMetricName, E),
    domain_priors:suppression_score(self_reference_in_type_theory, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(self_reference_in_type_theory),
    narrative_ontology:constraint_metric(self_reference_in_type_theory, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(self_reference_in_type_theory, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(self_reference_in_type_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Minimal. The constraint does not extract value from anyone. It is not a redistribution mechanism or an asymmetric advantage system. The prohibition operates uniformly — all agents attempting self-reference are blocked equally. Some may experience this as a burden (those seeking self-referential definitions), but burden is not the same as extraction. No agent benefits from other agents' inability to construct self-referential types. The value reflects that this is a pure structural limit, not a capturing mechanism. Suppression (0.04): Minimal. Suppression in the constraint framework measures barriers to exit. The prohibition on self-reference is not a barrier that could be overcome through effort, negotiation, or resource investment — it is a logical ceiling. The low suppression score reflects that agents are not being coerced or pressured; they are encountering a mathematical limit. Theater ratio (0.08): Minimal. Type-checking rejection of self-referential definitions happens cleanly and deterministically. No performative ritual, no ambiguity, no theatrical delay. The error message is immediate and precise. The low theater reflects that the constraint's function (preserving consistency) is fully transparent in its operation.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the constraint classifies identically as Mountain from all perspectives. The learner sees an immutable prohibition; the analyst sees a logical necessity; the institutional community sees a mathematical requirement, not a policy they impose. The uniformity of classification is itself diagnostic — this is a genuine natural law, not a contingent institutional arrangement misrepresented as natural law. The false summit detector should pass this constraint without flagging.
 *
 * DIRECTIONALITY LOGIC:
 *   The prohibition on self-reference has no directionality in the DR sense because there is no asymmetric extraction. There is no beneficiary group gaining advantage from the prohibition (proof assistants benefit from the stratification, but this is incidental structural benefit, not extraction from victims). There is no victim group bearing costs while others gain. All agents are uniformly subject to the same mathematical limit. The constraint does not fit the extraction/coordination dimensions because it is purely structural — a consequence of logical requirements, not a distribution mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN-ONLY CONSTRAINT: Self-reference prohibition in type theory is a uniform-type constraint where all perspectives produce Mountain classification. No mandatrophy resolution is required because there is no ambiguity between classification types. The constraint is invariant across all observables and measurement methodologies. Whether measured through computational halting, logical consistency, decidability, or formal proof, self-reference prohibition remains an irreducible mathematical law. The theatrical content is genuinely minimal (not hidden), the accessibility collapse is genuinely high (not obscured), and the constraint emerges necessarily from mathematical axioms (not imposed by convention). This is the canonical exemplar of a true mountain constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impredicativity_boundary,
    'Does the self-reference prohibition hold universally, or does impredicative type theory represent a structurally different constraint that allows limited self-reference?',
    'Comparative analysis of predicative (Coq, Lean) vs impredicative (System F, Calculus of Constructions) type systems; examination of whether impredicative systems'' consistency proofs depend on hidden stratification or genuine self-reference tolerance',
    'If impredicativity is genuinely self-referential: the prohibition is system-dependent (Rope or Tangled Rope under impredicative systems, Mountain under predicative systems). If impredicativity achieves consistency through hidden stratification: the prohibition is universal (Mountain in all systems).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impredicativity_boundary, empirical, 'Whether self-reference prohibition is universal or system-dependent').

omega_variable(
    universe_polymorphism_escape,
    'Does universe polymorphism in modern type theories (Lean 4, Coq with universe polymorphism) create functional loopholes in the self-reference prohibition, even though formal stratification persists?',
    'Case studies of universe-polymorphic definitions that appear to reference themselves; formal proof that such definitions still maintain strict hierarchy at instantiation. Analysis of whether universe polymorphism trades syntactic self-reference for semantic self-instantiation.',
    'If universe polymorphism enables semantic self-reference despite syntactic prohibition: the constraint becomes Rope (appears immutable but has hidden degrees of freedom via universe instantiation). If hierarchy is truly preserved: Mountain classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universe_polymorphism_escape, empirical, 'Whether universe polymorphism enables functional self-reference').

omega_variable(
    hott_higher_inductive_boundary,
    'Do higher inductive types in Homotopy Type Theory enable pathological self-reference that classical type theory forbids?',
    'Formal analysis of higher inductive type definitions and their consistency proofs; examination of whether path/equality induction creates new self-referential degrees of freedom',
    'If HoTT''s richer structure allows self-reference: the prohibition is contingent on classical type theory (not truly universal Mountain). If higher inductives preserve stratification: Mountain stands across HoTT.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hott_higher_inductive_boundary, empirical, 'Whether HoTT higher inductive types escape stratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_reference_in_type_theory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(selfref_tr_t0, self_reference_in_type_theory, theater_ratio, 0, 0.08).
narrative_ontology:measurement(selfref_tr_t50, self_reference_in_type_theory, theater_ratio, 50, 0.08).
narrative_ontology:measurement(selfref_tr_t100, self_reference_in_type_theory, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(selfref_be_t0, self_reference_in_type_theory, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(selfref_be_t50, self_reference_in_type_theory, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(selfref_be_t100, self_reference_in_type_theory, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_reference_in_type_theory, information_standard).
narrative_ontology:affects_constraint(self_reference_in_type_theory, girards_paradox_avoidance).
narrative_ontology:affects_constraint(self_reference_in_type_theory, russells_paradox_barrier).
narrative_ontology:affects_constraint(self_reference_in_type_theory, curry_howard_type_safety).

% DUAL FORMULATION NOTE:
% Self-reference prohibition in type theory forms a constraint family with related logical barriers (Russell's paradox, Girard's paradox, the halting problem). Each constraint has its own ε and structural narrative, but all are Mountain-type manifestations of the same underlying principle: the mathematical necessity of stratification to preserve consistency and avoid infinite regress.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
