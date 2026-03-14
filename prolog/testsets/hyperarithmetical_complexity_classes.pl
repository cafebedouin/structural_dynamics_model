% ============================================================================
% CONSTRAINT STORY: hyperarithmetical_complexity_classes
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hyperarithmetical_complexity_classes, []).

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
 *   constraint_id: hyperarithmetical_complexity_classes
 *   human_readable: Hyperarithmetical Complexity Classes and Computability Limits
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   Hyperarithmetical complexity classes define the boundaries of what can be
 *   computed, proved, or decided within formal systems. The arithmetic
 *   hierarchy — stratified into Σⁿ and Πⁿ levels — establishes that certain
 *   mathematical questions are inherently undecidable at each level, and this
 *   undecidability persists no matter how the formal system is augmented with
 *   new axioms. The constraint is that no algorithm can enumerate all truths
 *   at Π¹₁ level or higher; no formal system can be simultaneously complete
 *   and consistent above Σ⁰₁; and the complexity of deciding certain
 *   predicates is irreducibly tied to their position in this hierarchy. This
 *   is not a limitation imposed by resources, technology, or institutional
 *   practice — it is a structural property of mathematics itself. The
 *   theater_ratio remains low (0.15) because the constraint involves no
 *   performative elements: the undecidability theorems state facts, not
 *   rituals. The measurements are flat because hyperarithmetical structure
 *   does not drift over time — it is static mathematical truth.
 *
 * KEY AGENTS:
 *   - Attempting Computation: Primary target (powerless/trapped) — any finite algorithm seeking to compute hyperarithmetical functions faces an inherent barrier with zero degrees of freedom
 *   - Formal System: Secondary target (moderate/constrained) — recursive formal systems cannot escape incompleteness; axiom addition is possible but constrained by Gödel's limits
 *   - Research Community: Organized observer (organized/mobile) — collective effort can understand the structure but cannot computationally transcend the hierarchy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes the constraint as fundamental mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hyperarithmetical_complexity_classes, 0.12).
domain_priors:suppression_score(hyperarithmetical_complexity_classes, 0.03).
domain_priors:theater_ratio(hyperarithmetical_complexity_classes, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, extractiveness, 0.12).
narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hyperarithmetical_complexity_classes, mountain).
narrative_ontology:human_readable(hyperarithmetical_complexity_classes, "Hyperarithmetical Complexity Classes and Computability Limits").
narrative_ontology:topic_domain(hyperarithmetical_complexity_classes, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(hyperarithmetical_complexity_classes).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATTEMPTING COMPUTATION (MOUNTAIN) — A computational process seeking to decide a Π¹₁-complete predicate faces an inherent, irrevocable barrier. No finite algorithm, no matter how sophisticated, can systematically enumerate all valid Π¹₁ sentences. The constraint is immutable from the perspective of any finitary method. This is not a limitation of current technique but a structural property of the arithmetic hierarchy itself.
constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE FORMAL SYSTEM (MOUNTAIN) — A recursive formal system (Peano Arithmetic, ZFC) cannot prove all true sentences at levels above Σ⁰₁ without external axioms or expanded language. The incompleteness follows necessarily from Gödel and the Σ-completeness theorem. Even with added axioms climbing the hierarchy, gaps persist at higher levels. The constraint is structural: completeness and consistency cannot both be maximized in powerful enough systems.
constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE RESEARCH COMMUNITY (MOUNTAIN) — Even as a coordinated collective, mathematical logic researchers cannot compute hyperarithmetical functions through any unified method. They can study the structure of these classes, prove relationships between them, and gain formal understanding — but they cannot escape the fundamental barrier that defines these classes: undecidability at higher levels of the arithmetic hierarchy. Understanding the limit is not the same as transcending it.
constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational, universal perspective, hyperarithmetical complexity classes are a provably immutable feature of formal systems and computation. This is not a contingent institutional arrangement or a limitation of current resources. It is a law of mathematical structure itself. The boundary between computable and hyperarithmetical is as fundamental as the boundary between finite and transfinite.
constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hyperarithmetical_complexity_classes_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hyperarithmetical_complexity_classes, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, ExtMetricName, E),
    domain_priors:suppression_score(hyperarithmetical_complexity_classes, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hyperarithmetical_complexity_classes),
    narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hyperarithmetical_complexity_classes, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hyperarithmetical_complexity_classes_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract value from any agent — it is a limit, not a mechanism of redistribution. No one benefits at anyone else's expense. The value is slightly above zero because stating and proving the undecidability theorems requires mathematical effort; the theorems are not self-evident. Suppression (0.03): Negligible. The constraint is not enforced through coercion or opacity — the arithmetic hierarchy is provable from first principles in basic logic. Accessibility collapse (0.92): Very high. All pathways to solving hyperarithmetical problems lead to the same barrier. There is no workaround, no hidden method, no alternative architecture that evades the hierarchy. Theater ratio (0.15): Low. Hyperarithmetical theory is expressed through pure mathematical proof, not through institutional ritual or performative claims. The theorems are either valid or invalid; there is minimal performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits remarkable uniformity across all perspectives — all four agents, from powerless to analytical, classify it as mountain. This uniformity is diagnostic: it indicates that the constraint is genuinely immutable, not just institutionally reinforced. The powerless computation cannot escape it; the formal system cannot circumvent it; the research community cannot coordinate around it; the analytical observer sees it as fundamental. Uniform mountain classification is the signature of natural law. The absence of perspectival gap (no agent perceives the constraint as coordination, extraction, or temporary) is evidence for the claim that hyperarithmetical complexity is a structural feature of mathematics, not a social construction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis is not applicable to mountain constraints. All agents are equally subject to the constraint; there is no extraction flow, no beneficiary, no victim. The arithmetic hierarchy does not privilege one cognitive agent over another — it simply marks the boundary between what is decidable and what is not. The constraint has no beneficiaries because it extracts nothing. It has no victims because it is not imposed by one actor against another. It is a law.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: Hyperarithmetical complexity classes resolve the mandatrophy by being entirely free from it. There is no risk of mislabeling them as coordination or extraction because the constraint has zero extractiveness and involves no coordination mechanism. The mandatrophy — the danger that a snare masquerades as coordination or vice versa — applies only to constraints with significant extraction or suppression. Natural laws by definition have neither. The theorem that Π¹₁-completeness entails undecidability is not a deal, an agreement, or an institutional arrangement. It is a fact of mathematical structure. The classification as mountain is not a perspectival judgment but a logical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_machine_transcendence,
    'Does providing an oracle for a Π⁰₁-complete predicate genuinely escape hyperarithmetical constraint, or does it relocate the constraint to a higher level of the transfinite hierarchy?',
    'Formal analysis of relativized computability and jump operators; demonstration that oracle machines with Π⁰₁ oracles can solve Π⁰₁ problems but face analogous barriers at Π¹₁; tracing of the constraint up the Turing degree hierarchy',
    'If oracle transcendence is illusory: hyperarithmetical constraint is truly immutable, and no informational addition at any countable level escapes it. If oracle transcendence is real: the constraint is not immutable but relocatable to higher levels, implying the universe of discourse must expand to higher-order logic or set theory to truly transcend.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_machine_transcendence, conceptual, 'Whether oracle machines escape hyperarithmetical constraint or relocate it').

omega_variable(
    physical_implementation_feasibility,
    'Could a physical computing device (quantum computer, continuous analog system, or exotic computational model) implement algorithms for hyperarithmetical functions that are uncomputable in the classical Turing-complete sense?',
    'Physical Church-Turing thesis analysis; investigation of whether quantum mechanics, continuous mathematics, or non-standard models of computation could violate classical computability bounds; cosmological constraints on energy and information density required for hyperarithmetical computation',
    'If impossible: the constraint is not merely logico-mathematical but embedded in physical law. If possible: hyperarithmetical computability could become physically realizable, and the constraint is not a law of nature but a characterization of classical discrete computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_implementation_feasibility, empirical, 'Whether physical systems can compute hyperarithmetical functions').

omega_variable(
    foundational_axiom_dependence,
    'Does the hierarchy of complexity classes depend on specific foundational axioms (e.g., choice, regularity), or is it independent of reasonable axiom sets?',
    'Comparative proof analysis across ZFC, constructive set theory (CZF), and other foundational frameworks; investigation of which complexity relationships hold in minimal logical systems',
    'If axiom-dependent: the constraint is contingent on mathematical choice, not immutable. If axiom-independent: the constraint is more robustly fundamental, approaching natural law status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_dependence, conceptual, 'Axiom dependence of complexity hierarchy structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hyperarithmetical_complexity_classes, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hype_tr_t0, hyperarithmetical_complexity_classes, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hype_tr_t50, hyperarithmetical_complexity_classes, theater_ratio, 50, 0.15).
narrative_ontology:measurement(hype_tr_t100, hyperarithmetical_complexity_classes, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hype_be_t0, hyperarithmetical_complexity_classes, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hype_be_t50, hyperarithmetical_complexity_classes, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(hype_be_t100, hyperarithmetical_complexity_classes, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hyperarithmetical_complexity_classes, information_standard).
narrative_ontology:affects_constraint(hyperarithmetical_complexity_classes, godel_incompleteness).
narrative_ontology:affects_constraint(hyperarithmetical_complexity_classes, halting_problem).
narrative_ontology:affects_constraint(hyperarithmetical_complexity_classes, rice_theorem_undecidability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
