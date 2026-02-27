% ============================================================================
% CONSTRAINT STORY: tractarian_logic_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tractarian_logic_limit, []).

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
 *   constraint_id: tractarian_logic_limit
 *   human_readable: The Limits of Language (Tractatus)
 *   domain: philosophical/logical
 *
 * SUMMARY:
 *   Wittgenstein's Tractatus Logico-Philosophicus presents a foundational
 *   claim about the structure of language and its limits: the world consists
 *   of facts (not things); facts are configurations of atomic facts;
 *   propositions are truth functions of elementary propositions that picture
 *   atomic facts; whatever cannot be expressed in logically well-formed
 *   propositions is literally unsayable. This constraint operates at the
 *   level of logical structure itself, not as a regulatory imposition by any
 *   agent or institution. No one extracts value from the constraint by
 *   enforcing it; rather, it describes the unchangeable relationship between
 *   language, thought, and reality. The constraint is invariant across all
 *   observers and contexts: the logician, the metaphysician, the linguist,
 *   and the analytical philosopher all encounter the same boundary. The
 *   theatrical component (theater_ratio ≈ 0.15) reflects only the
 *   communicative necessity of pointing to the limit (Wittgenstein must
 *   gesture toward silence); there is no performative excess or hidden
 *   function. The extractiveness (0.18) is minimal and represents only the
 *   cognitive overhead of accepting the constraint, not any structural
 *   asymmetry of benefit and burden. No beneficiary group extracts at the
 *   expense of a victim group.
 *
 * KEY AGENTS:
 *   - The Logician: Analytical perspective (analytical/analytical) — sees the constraint as a formal consequence of propositional logic itself
 *   - The Metaphysician: Powerful perspective (powerful/analytical) — encounters the constraint as an impassable boundary limiting metaphysical inquiry into the mystical
 *   - The Linguistic Analyst: Analytical perspective (analytical/analytical) — understands the constraint as emerging from the picture theory of meaning
 *   - The Philosophical Community: Distributed perspective (analytical/analytical) — accepts or contests the constraint through formal and conceptual argument, not through enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tractarian_logic_limit, 0.18).
domain_priors:suppression_score(tractarian_logic_limit, 0.03).
domain_priors:theater_ratio(tractarian_logic_limit, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tractarian_logic_limit, extractiveness, 0.18).
narrative_ontology:constraint_metric(tractarian_logic_limit, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(tractarian_logic_limit, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tractarian_logic_limit, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tractarian_logic_limit, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tractarian_logic_limit, mountain).
narrative_ontology:human_readable(tractarian_logic_limit, "The Limits of Language (Tractatus)").
narrative_ontology:topic_domain(tractarian_logic_limit, "philosophical/logical").

domain_priors:emerges_naturally(tractarian_logic_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the standpoint of formal logic and philosophy of language, the tractarian constraint is a structural necessity: language mirrors logical form; what can be said must be expressible in logically well-formed propositions; what cannot be expressed in logical form cannot be meaningfully said. This is a mountain — an immutable feature of the relationship between language, thought, and reality. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of formal logic, the constraint is proven via the structure of propositional calculus itself. Every meaningful proposition is a truth function of elementary propositions. This is not a regulation or convention that could be otherwise; it follows from the logical structure of meaning. No agent is extracted from; no asymmetry exists. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of metaphysical inquiry, the tractarian limit is an impassable boundary: whatever lies beyond the reach of logical expression — the mystical, ethics, aesthetics, the nature of the subject, God — is literally unsayable. One can point to it (as Wittgenstein does), but cannot speak it. This is not a suppression imposed by an external agent; it is a structural feature of language itself. The boundary is absolute and unchangeable. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of philosophy of language, the constraint emerges from the picture theory of meaning: propositions are pictures of possible facts; elementary propositions correspond to atomic facts; complex propositions are truth functions of elementary propositions. This structural homology between language and reality is not contingent — it is what makes language meaningful at all. No exit, no arbitrage, no suppression of alternatives (they do not exist). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tractarian_logic_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tractarian_logic_limit, ExtMetricName, E),
    domain_priors:suppression_score(tractarian_logic_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tractarian_logic_limit),
    narrative_ontology:constraint_metric(tractarian_logic_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tractarian_logic_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tractarian_logic_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Minimal. The tractarian constraint does not extract value from any agent to benefit another. It describes a structural feature of language that applies universally. The small non-zero value reflects only the cognitive cost of recognizing and working within the constraint — not genuine extraction. No agent is privileged by the constraint; all speakers and thinkers encounter the same limit. Suppression (0.03): Negligible. There are no coercive mechanisms, institutional enforcement, or suppressed alternatives. The constraint is not maintained through force or ideology; it follows from logical structure. The tiny non-zero value reflects only that awareness of the constraint may inhibit attempts to transgress it — but transgression is logically impossible, not institutionally forbidden. Theater ratio (0.15): Very low. The constraint is not performative. Wittgenstein's famous conclusion — 'Whereof one cannot speak, thereof one must be silent' — is not a ritual or theater but a logical consequence. The small theatrical component reflects only the communicative act of drawing the boundary (one must say something to indicate what cannot be said), not any excess of performance over function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is unique in the Deferential Realism framework: all perspectives produce the same classification (Mountain). There is no perspectival gap because the constraint is truly universal. The logician, the metaphysician, the linguist, and the analytical observer all see the same structure — the limit is not relative to position or power. This uniformity is itself the diagnostic signature of a genuine mountain: if one observer saw Snare and another saw Rope, or if the classification depended on the agent's exit options or time horizon, the constraint would not be a structural law but a contingent institutional arrangement. The tractarian constraint exhibits invariance across all indexical tuples: (powerless, immediate, trapped, local) produces Mountain, as does (institutional, civilizational, arbitrage, global). No observation point yields a different classification.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d ≈ 0.50-0.72 (broad middle range) because no agent is structurally positioned as a net beneficiary or net victim relative to this constraint. The constraint does not extract from anyone; it describes a universal logical structure. Directionality derivation is nearly neutral. The slight variation across perspectives (0.50 vs 0.72) reflects the observer's cognitive distance from the constraint: the pure logician is closest to the structure (d ≈ 0.50, symmetric), while the metaphysician encounters it as a boundary to their inquiry (d ≈ 0.65, moderate asymmetry in experience). But all yield f(d) values in the range 0.65-1.15, all with scope σ=1.0 (universal), producing χ values from 0.12-0.21 — all in the Mountain range (χ ≤ 0.25 by constraint definition, though the chi formula is not the primary gate; logical necessity is).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elementary_proposition_determinacy,
    'Are there determinate elementary propositions, or is the decomposition into elementary propositions observer-dependent or context-relative?',
    'Formal analysis of whether Wittgenstein''s logical atomism can specify a unique, determinate set of elementary propositions independent of measurement or conceptual scheme; examination of later Wittgenstein''s private language arguments and his rejection of logical atomism',
    'If elementary propositions are determinate: the mountain classification is fully justified — the constraint is a logical necessity. If context-relative: the constraint may be a coordinated convention (Rope) masquerading as a natural law, or a performative limit that varies across language-games (Piton). Classification stability depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elementary_proposition_determinacy, conceptual, 'Whether elementary propositions are determinate or observer-dependent').

omega_variable(
    mystical_expressibility_boundary,
    'Is the boundary between sayable and unsayable (logical vs mystical) itself sayable, or does Wittgenstein commit to what he explicitly denies: saying something about what cannot be said?',
    'Textual analysis of the Tractatus''s concluding propositions (6.4-7) and their reflexivity; examination of whether the work''s own propositions are performatively self-undermining',
    'If the boundary is sayable: the tractarian system is self-refuting, suggesting the constraint is not an immutable logical law but a contingent limitation of Wittgenstein''s formal framework (possible Piton or Tangled Rope). If unsayable: Wittgenstein must remain silent, and the constraint is a true mountain — what cannot be expressed cannot be expressed, not even the boundary itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mystical_expressibility_boundary, conceptual, 'Whether the unsayable/sayable boundary can be expressed without contradiction').

omega_variable(
    alternative_logics_escape_route,
    'Do non-classical logics (intuitionistic, paraconsistent, quantum logic, substructural logics) escape or refute the tractarian constraint, or do they exemplify it under different formal assumptions?',
    'Formal comparison of logical systems; determination of whether alternative logics relax the picture-theory constraint or work within it under different axioms',
    'If alternative logics escape: the constraint is not universal but relative to classical logic — it becomes contingent (Rope or Scaffold within classical logic, but not a Mountain across all logical systems). If they exemplify: the constraint is deeper than any particular logic, supporting the mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_logics_escape_route, empirical, 'Whether non-classical logics escape the tractarian constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tractarian_logic_limit, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tract_tr_t0, tractarian_logic_limit, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tract_tr_t50, tractarian_logic_limit, theater_ratio, 50, 0.13).
narrative_ontology:measurement(tract_tr_t100, tractarian_logic_limit, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(tract_be_t0, tractarian_logic_limit, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(tract_be_t50, tractarian_logic_limit, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(tract_be_t100, tractarian_logic_limit, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tractarian_logic_limit, information_standard).
narrative_ontology:affects_constraint(tractarian_logic_limit, godel_incompleteness).
narrative_ontology:affects_constraint(tractarian_logic_limit, halting_problem).
narrative_ontology:affects_constraint(tractarian_logic_limit, church_turing_thesis).

% DUAL FORMULATION NOTE:
% The tractarian constraint is the foundational epistemological ceiling for logical systems. Gödel's Incompleteness Theorem, the Halting Problem, and the Church-Turing Thesis are later discoveries that operate within this ceiling: they describe limits of formal proof, computation, and decidability respectively, all of which presuppose the tractarian structure of language and logic. The Tractatus is upstream; the later results are downstream instances of its constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
