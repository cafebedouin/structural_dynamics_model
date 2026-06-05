% ============================================================================
% CONSTRAINT STORY: power_set
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_set, []).

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
 *   constraint_id: power_set
 *   human_readable: Axiomatic Set Theory's Power Set Axiom
 *   domain: mathematical/axiomatic_set_theory
 *
 * SUMMARY:
 *   The Power Set axiom in Zermelo-Fraenkel set theory with the Axiom of
 *   Choice (ZFC) is a foundational claim: for every set X, there exists a set
 *   P(X) — the power set — containing all subsets of X. This axiom is a
 *   canonical mathematical mountain: irreducible, unchangeable, and appearing
 *   as a bedrock logical constraint rather than an institutional arrangement
 *   or extractive mechanism. The axiom is not extracting value from any
 *   agent; it is a structural feature of the formal system itself. All
 *   perspectives — constructivists, model theorists, practitioners of
 *   alternative set theories, and the mathematical practice community —
 *   classify it uniformly as a natural law. The constraint's
 *   accessibility_collapse (0.92) reflects that the power set axiom cannot be
 *   circumvented within ZFC: it is either true or false in any model, with no
 *   middle ground. The resistance (0.08) indicates minimal resistance to its
 *   adoption — mathematicians accept it as a necessary foundation. The
 *   theater_ratio (0.15) is low because the axiom's function is directly
 *   logical, not performative.
 *
 * KEY AGENTS:
 *   - Mathematical Logicians: Analytical observers — understand the axiom as a structural feature of the formal system, not as extraction
 *   - Constructivist Mathematicians: Powerless relative to the axiom — cannot construct all subsets for infinite sets but must accept their existence
 *   - Alternative Set Theory Practitioners: Organized agents with mobile exit — can adopt intuitionistic or constructive alternatives, but face the same boundary problem in different form
 *   - Mathematical Practice Community: Institutional beneficiary — uses ZFC as the standard foundation and depends on the power set axiom's expressive power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_set, 0.12).
domain_priors:suppression_score(power_set, 0.02).
domain_priors:theater_ratio(power_set, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_set, extractiveness, 0.12).
narrative_ontology:constraint_metric(power_set, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(power_set, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(power_set, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(power_set, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_set, mountain).
narrative_ontology:human_readable(power_set, "Axiomatic Set Theory's Power Set Axiom").
narrative_ontology:topic_domain(power_set, "mathematical/axiomatic_set_theory").

domain_priors:emerges_naturally(power_set).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRUCTIVIST MATHEMATICIAN (MOUNTAIN) — Views the power set axiom as an irreducible logical constraint. Cannot construct all subsets explicitly for infinite sets; the axiom asserts their existence nonetheless. The constraint appears as a bedrock limitation of what can be proven constructively versus what must be accepted as axiomatic. No exit: mathematics either adopts the axiom or loses the expressive power of ZFC.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL LOGICIAN / MODEL THEORY (MOUNTAIN) — Observes that the power set axiom is a logical consequence of the formal language of set theory and the semantics of the power set operator. The axiom is not extracting anything from any agent; it is a structural feature of the formal system itself. Classification invariant across all measurement bases: P(X) exists if and only if you are working in a model where the power set axiom holds.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ALTERNATIVE SET THEORY PRACTITIONER (MOUNTAIN) — Works in Zermelo set theory, intuitionistic set theory, or type theory where the power set axiom may not hold or may take different forms. From this perspective, the axiom remains a structural choice boundary: one can exit ZFC, but any foundation for mathematics must make some choice about subset formation. The constraint is not ZFC-specific; it is inherent to the foundational problem itself. Even exiting to mobile alternatives, the constraint reappears in different form.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL PRACTICE COMMUNITY (MOUNTAIN) — Uses ZFC as the standard foundation for mathematics. The power set axiom is not negotiable within this foundation; to do mathematics in ZFC, one must accept it. The institutional mathematical community has chosen this foundation, but the choice itself is not arbitrary — the power set axiom is necessary for ZFC's internal consistency and expressive power. Classification: mountain. The axiom is an irreducible logical commitment of the chosen system.
constraint_indexing:constraint_classification(power_set, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_set_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(power_set, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_set, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(power_set, ExtMetricName, E),
    domain_priors:suppression_score(power_set, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(power_set),
    narrative_ontology:constraint_metric(power_set, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(power_set, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(power_set_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The power set axiom does not extract value from any agent in the sense of redistribution or asymmetric benefit. It is a logical constraint that applies universally and equally — if you are working in ZFC, the axiom holds for all sets, benefiting all users equally. The low value reflects that this is not an extraction mechanism but a foundational choice. Suppression (0.02): Minimal. There is almost no suppression of alternatives within ZFC — the axiom is a stated assumption of the system. Alternative set theories exist, but they are not suppressed; they are simply different choices. Theater ratio (0.15): Low. The axiom's function is directly logical — it defines the power set operator — with minimal performative content. Accessibility collapse (0.92): Very high. Within ZFC, the axiom is inaccessible to negotiation or modification. It is either true or false in any model, with no spectrum of compromise. Resistance (0.08): Very low. Mathematicians accept the axiom without friction — it is adopted as a necessary foundation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap. All four perspectives classify the power set axiom as a mountain. The constructivist sees a logical boundary; the analytical logician sees a formal feature; the alternative set theory practitioner sees the same boundary reappear in different foundational systems; the mathematical practice community sees a necessary axiom of their chosen foundation. No agent perceives extraction, suppression, or exit options. The gap that does exist is conceptual rather than structural: is the axiom a natural law of logic itself, or a contingent choice in ZFC that alternative foundations might avoid? This is not a perspectival gap in the deferential realism sense (different structural positions), but rather an omega variable about the nature of foundational choice itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN AFFIRMATION: The power set axiom resolves the mandatrophy by being genuinely uniform across all perspectives. There is no false naturalizing of contingent institutional choice here — the axiom truly is an irreducible constraint of the formal system. The four perspectives do not disagree about classification; they provide complementary views of the same logical necessity. The only meaningful mandatrophy is at the meta-level: Is the axiom a necessity of all mathematics (truly universal mountain), or a contingent choice specific to ZFC? This is captured in the omega variables. If future foundational work shows that intuitionistic or constructive alternatives are equally viable while avoiding the power set axiom entirely, the classification would not change within ZFC — it would remain a mountain of ZFC's system. But the broader claim that the axiom is a mountain of mathematics would degrade to Rope (coordinated institutional choice of foundation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intuitionistic_alternative_status,
    'Is the power set axiom a natural logical law or a contingent choice point in foundational mathematics?',
    'Comparative analysis of intuitionistic set theory, constructive type theory, and other alternative foundations. Determination of whether they avoid or redefine the power set axiom, and what expressive cost they pay.',
    'If contingent choice: the constraint may degrade to Rope (coordination of foundational choice) or Scaffold (temporary adoption pending better alternatives). If natural law: mountain classification is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intuitionistic_alternative_status, conceptual, 'Whether the power set axiom is a natural logical constraint or contingent foundational choice').

omega_variable(
    consistency_independence_proof,
    'Can a constructive proof be given that the power set axiom is consistent with the other ZFC axioms, or does its consistency remain relative to the consistency of ZFC as a whole?',
    'Proof theory analysis; review of Gödel''s relative consistency results and whether stronger meta-theory assumptions are required for the power set axiom specifically.',
    'If provably independent: the axiom is a pure boundary choice, not a logical necessity — classification may shift toward Rope. If relative consistency only: the axiom is locked into ZFC''s foundational circularity — mountain classification affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consistency_independence_proof, conceptual, 'Whether the power set axiom''s consistency is provable or relative to ZFC').

omega_variable(
    cardinality_explosion_necessity,
    'Is the cardinality jump from a set to its power set (Cantor''s theorem) a necessary feature of any foundational system that makes mathematics work, or is it a specific artifact of ZFC?',
    'Analysis of alternative foundations (category theory, homotopy type theory, constructive mathematics) and their treatments of function spaces and subset formation. Determination of whether all viable foundational systems exhibit the power set cardinality jump.',
    'If necessary across all systems: mountain classification affirmed — the constraint is inherent to foundations. If specific to ZFC: the constraint may be institutional (Rope: coordinated choice) rather than natural (Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cardinality_explosion_necessity, empirical, 'Whether the power set cardinality jump is necessary to all mathematics or specific to ZFC').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_set, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ps_tr_t0, power_set, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ps_tr_t50, power_set, theater_ratio, 50, 0.15).
narrative_ontology:measurement(ps_tr_t100, power_set, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(ps_be_t0, power_set, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ps_be_t50, power_set, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(ps_be_t100, power_set, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_set, information_standard).
narrative_ontology:affects_constraint(power_set, cantor_diagonal_argument).
narrative_ontology:affects_constraint(power_set, godel_incompleteness_first).
narrative_ontology:affects_constraint(power_set, axiom_of_choice_dependency).

% DUAL FORMULATION NOTE:
% The power set axiom forms a constraint family with related foundational axioms (Infinity, Choice, Replacement). The power set axiom specifically governs the formation of subset collections; it is upstream of cardinal arithmetic constraints like Cantor's theorem. Alternative formulations (intuitionistic power set, predicative power set, constructive type theory function spaces) represent different boundary solutions to the same meta-problem: what entities can serve as collections in a foundational system?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
