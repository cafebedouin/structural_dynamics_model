% ============================================================================
% CONSTRAINT STORY: fgh_hierarchy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fgh_hierarchy_2026, []).

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
 *   constraint_id: fgh_hierarchy_2026
 *   human_readable: The Fast-Growing Hierarchy
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy (FGH) classifies the growth rates of
 *   computable functions using transfinite ordinal indexing. Functions at
 *   higher ordinal levels grow strictly faster than those below, producing a
 *   mathematical structure of unprecedented scope: FGH functions exhaust all
 *   recursively definable growth rates up to the Bachmann-Howard ordinal.
 *   This constraint exhibits the core properties of a natural law in the
 *   Deferential Realism sense: the ordinal structure is universally
 *   accessible, logically determined, and immutable across all equivalent
 *   formalisms. There are no beneficiaries or victims in the economic sense —
 *   the FGH is a neutral mathematical ordering available to all. No
 *   institution maintains it; no agent can escape it. The constraint emerges
 *   from the logical structure of recursion and transfinite ordinals, not
 *   from human choice or institutional design. The theater ratio remains low
 *   throughout the interval because exposition of FGH does not require
 *   ritualistic performance — the mathematics is transparent. The
 *   extractiveness rise from 0.08 to 0.12 reflects increased technical
 *   sophistication required to work with the hierarchy, not an increase in
 *   coercive content: as mathematical tools become more specialized, access
 *   requires more expertise, but this is complexity, not extraction.
 *
 * KEY AGENTS:
 *   - Computability Theorists: Powerless agents (analytical/analytical) — cannot challenge or modify the ordinal ordering; constrained to work within the hierarchy's structure
 *   - Research Community: Organized collective (organized/analytical) — benefits from the canonical classification system; all ordinal comparisons are determinate
 *   - Formal Systems Institutions: Institutional actors (institutional/analytical) — maintain curricula and proof systems built on FGH foundations; embedded in global mathematical infrastructure
 *   - Analytical Observer: Universal vantage (analytical/analytical) — perceives FGH as a mathematical law with civilizational scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fgh_hierarchy_2026, 0.12).
domain_priors:suppression_score(fgh_hierarchy_2026, 0.03).
domain_priors:theater_ratio(fgh_hierarchy_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, 0.12).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fgh_hierarchy_2026, mountain).
narrative_ontology:human_readable(fgh_hierarchy_2026, "The Fast-Growing Hierarchy").
narrative_ontology:topic_domain(fgh_hierarchy_2026, "mathematical/computational").

domain_priors:emerges_naturally(fgh_hierarchy_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING COMPUTABILITY THEORIST (MOUNTAIN) — The FGH ordinal indexing is an immutable structural feature of growth rate classification. A theorist cannot escape the fundamental constraint that functions with higher ordinal indices grow strictly faster than those below. This is not a choice or policy — it follows necessarily from the definition of the hierarchy itself. Zero degrees of freedom.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH COMMUNITY (MOUNTAIN) — The FGH provides the canonical ordering of recursive function growth. No alternative classification system can change the ordinal relationships — they are logically determined by the definition. Communities may invent competing hierarchies (Hardy, Wainer, etc.), but each produces equivalent orderings within their domain of application. The constraint is the mathematical structure itself, not institutional convention.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal vantage, the FGH is a mathematical law. Its ordinal structure is invariant across all formalisms (Turing, lambda calculus, post-canonical, etc.). The growth rates follow necessarily from the ordinal indexing. This constraint exhibits all hallmarks of a natural law: universal scope, logical necessity, zero alternatives, and complete accessibility to analytical scrutiny.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: FORMAL SYSTEMS COMMUNITY (MOUNTAIN) — FGH ordinal indexing is embedded in proof theory, computability theory, and reverse mathematics curricula worldwide. Institutional adoption reflects the constraint's structural inevitability — not inertia or convention. The hierarchy cannot be negotiated, weakened, or circumvented because its structure is determined by first-order logic and transfinite ordinal arithmetic.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fgh_hierarchy_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, ExtMetricName, E),
    domain_priors:suppression_score(fgh_hierarchy_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fgh_hierarchy_2026),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fgh_hierarchy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The FGH does not extract resources from any agent because it is a neutral mathematical structure. No one profits from the hierarchy's existence relative to others; the ordering is available to all researchers equally. The slight non-zero value (0.12 rather than 0.0) reflects the cognitive cost of learning the hierarchy — some prerequisite expertise is required — but this is accessibility friction, not extraction. Suppression (0.03): Negligible. The FGH cannot be suppressed because it is logically determined. Alternative hierarchies (Wainer, Hardy) exist, but each produces equivalent orderings within its domain. There is no suppression of knowledge because the mathematics is fully transparent. Theater ratio (0.15): Very low, rising slightly to 0.18. The FGH requires technical exposition but minimal ritualistic performance. Pedagogical complexity accounts for the modest theater value — explaining transfinite recursion is intrinsically sophisticated, not performative. The slight rise over the interval reflects increased specialization in the field, not growing theater.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify identically as Mountain. There is no gap. This constraint is uniform-type — the FGH structure is logically invariant across all observational positions. A computability theorist, a research community, an institutional framework, and a civilizational observer all perceive the same mathematical ordering. The uniformity itself is the diagnostic signal: natural laws do not exhibit perspectival gaps because they constrain all agents equally.
 *
 * DIRECTIONALITY LOGIC:
 *   The FGH constraint involves no directionality in the economic sense (beneficiary/victim asymmetry). All agents are in the same structural position relative to the mathematical ordering: they are subjects of the constraint, not extractors or targets. The ordinal indices apply universally. In the language of directionality, all agents have d ≈ 0.5 (neutral) because the constraint distributes neither benefits nor costs asymmetrically. This symmetry is a hallmark of mountain constraints — they constrain all equally because they express logical necessity, not institutional power. The analytical power atom with analytical exit and universal scope is the canonical perspective for accessing the constraint's true structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The FGH resolves mandatrophy by being a genuinely uniform-type mountain constraint. No attempt to reframe it as coordination (Rope) or extraction (Snare) succeeds because the mathematical structure admits no such reading. The constraint does not coordinate agents (it merely orders functions), and it does not extract from anyone (it benefits none at the expense of others). The mountain classification is the correct singular reading, not a perspective among many. The analytical observer's mountain classification is not a false summit — it is the true structure revealed by mathematical rigor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_indexing_foundation,
    'Are the ordinal indices of the FGH purely mathematical abstractions or do they correspond to actual computational distinctions in real-world algorithmic practice?',
    'Empirical analysis of algorithms in cryptography, constraint satisfaction, and optimization; mapping of real-world computational complexity to FGH levels; verification that algorithms at FGH(α) genuinely outpace FGH(β) for α > β in practice',
    'If abstract: FGH is a pure mathematical ordering without practical significance. If corresponds to real computation: FGH constrains what is computationally feasible independent of technology. The distinction determines whether FGH qualifies as a natural law of computation or merely a formal ranking system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_indexing_foundation, empirical, 'Whether FGH ordinal indices correspond to actual computational distinctions').

omega_variable(
    halting_problem_hierarchy_coupling,
    'Is the FGH hierarchy a consequence of the Halting Problem or an independent mathematical structure?',
    'Proof-theoretic analysis of whether FGH ordinal growth rates are derivable from the undecidability of the Halting Problem or whether they are autonomous mathematical facts; investigation of whether weakening undecidability assumptions would alter FGH structure',
    'If consequence: FGH inherits mountain status from Halting Problem (both immutable). If independent: FGH''s mountain status requires separate justification through accessibility_collapse and resistance metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(halting_problem_hierarchy_coupling, conceptual, 'Dependency relationship between FGH and the Halting Problem').

omega_variable(
    oracle_machine_escape,
    'Do oracle machines (hypothetical computers with access to a Halting Problem oracle) escape the FGH hierarchy, or do transfinite hierarchies of oracles simply reproduce an FGH-like ordering at a higher level?',
    'Analysis of recursive function theory with oracle extensions; construction of Turing jump hierarchies and their relationship to the Arithmetical Hierarchy; verification that no finite oracle extension breaks the hierarchy structure',
    'If oracle machines escape: FGH is contingent on classical Turing computation and not a universal constraint. If reproduce ordering: FGH structure is deeper than computational model choice — truly universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_machine_escape, conceptual, 'Whether oracle machines escape FGH structure or reproduce it at higher levels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fgh_hierarchy_2026, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fgh_tr_t0, fgh_hierarchy_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fgh_tr_t50, fgh_hierarchy_2026, theater_ratio, 50, 0.15).
narrative_ontology:measurement(fgh_tr_t100, fgh_hierarchy_2026, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(fgh_be_t0, fgh_hierarchy_2026, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fgh_be_t50, fgh_hierarchy_2026, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(fgh_be_t100, fgh_hierarchy_2026, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fgh_hierarchy_2026, information_standard).
narrative_ontology:affects_constraint(fgh_hierarchy_2026, arithmetical_hierarchy_ordering).
narrative_ontology:affects_constraint(fgh_hierarchy_2026, turing_jump_closure).
narrative_ontology:affects_constraint(fgh_hierarchy_2026, hyperarithmetical_complexity_classes).

% DUAL FORMULATION NOTE:
% The FGH is a foundational mathematical structure that constrains the landscape of computability theory. Related constraints (Arithmetical Hierarchy, Turing Jump, Hyperarithmetical classes) are downstream in the sense that their structure must be consistent with FGH ordinal ordering. The FGH does not decompose into multiple observables with different epsilon values — it is a single, unified mathematical law. No variant observable produces a different classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
