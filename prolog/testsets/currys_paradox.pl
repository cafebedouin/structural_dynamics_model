% ============================================================================
% CONSTRAINT STORY: currys_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currys_paradox, []).

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
 *   constraint_id: currys_paradox
 *   human_readable: Curry's Paradox
 *   domain: analytical/logic
 *
 * SUMMARY:
 *   Curry's Paradox is a logical constraint that emerges from the structure
 *   of self-referential sentences combined with material conditionals. It
 *   proves that any arbitrary claim (e.g., 'The moon is made of cheese') can
 *   be derived from a single sentence of the form 'If this sentence is true,
 *   then X is true.' Unlike the Liar Paradox, Curry's Paradox does not rely
 *   on negation, making it a more fundamental threat to logics that allow
 *   unrestricted self-reference. The constraint is invariant across all
 *   perspectives — every analyst who understands the formal structure
 *   recognizes it as a brute logical fact. There are no beneficiaries or
 *   victims; there are only observers who recognize the constraint's
 *   existence and different methodological responses to it (hierarchical
 *   stratification, paraconsistency, type theory, dialogue logic). Curry's
 *   Paradox is a pure case of a Mountain constraint: it emerges naturally
 *   from the mathematical structure of language, resists all attempts at
 *   avoidance through unrestricted systems, and offers zero degrees of
 *   freedom for agents operating within classical logic.
 *
 * KEY AGENTS:
 *   - Formal Logician: Analytical observer (analytical/civilizational) — recognizes Curry's Paradox as a mathematical truth about unrestricted self-reference
 *   - Set Theorist: Analytical observer (analytical/civilizational) — recognizes the paradox as a constraint on naive set theory and unrestricted comprehension
 *   - Type Theorist: Analytical observer (analytical/civilizational) — solves the paradox through stratification but recognizes the stratification as a mandatory response, not a choice
 *   - Model Theorist: Analytical observer (analytical/civilizational) — verifies that the paradox holds across all formal models that permit self-reference
 *   - Paraconsistent Logician: Analytical observer (analytical/civilizational) — challenges whether Curry's Paradox is truly universal or domain-relative to classical logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currys_paradox, 0.12).
domain_priors:suppression_score(currys_paradox, 0.03).
domain_priors:theater_ratio(currys_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currys_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(currys_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(currys_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(currys_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currys_paradox, mountain).
narrative_ontology:human_readable(currys_paradox, "Curry's Paradox").
narrative_ontology:topic_domain(currys_paradox, "analytical/logic").

domain_priors:emerges_naturally(currys_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMAL LOGICIAN (MOUNTAIN) — Curry's Paradox is a mathematical fact about self-reference and material conditional in unrestricted logic. The paradox emerges from the structure of language itself whenever we allow sentences to quantify over their own truth-conditions without restriction. No exit, no suppression, no coordination benefit — it is a brute constraint on what formal systems can express without contradiction.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MODEL THEORIST (MOUNTAIN) — The paradox is invariant across all first-order and higher-order logical models. It does not depend on any particular model's properties — it is a universal feature of any logic that permits self-referential sentences and material conditionals. The constraint is a logical law, not a convention or a design choice.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SET THEORIST (MOUNTAIN) — Curry's Paradox shows that naive set theory (allowing any predicate to define a set) collapses into contradiction. The constraint is that unbounded self-reference in set membership cannot coexist with classical logic. This is an irreducible barrier to certain formulations — you cannot have naive set theory and consistency simultaneously.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: TYPE THEORIST (MOUNTAIN) — Type-theoretic approaches avoid Curry's Paradox by stratifying the language: a sentence cannot quantify over a domain that includes itself. This is not a restriction imposed on language; it is the natural structure language must have to avoid the paradox. The constraint is the minimal condition for logical coherence.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currys_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(currys_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(currys_paradox, ExtMetricName, E),
    domain_priors:suppression_score(currys_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(currys_paradox),
    narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(currys_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(currys_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (ε = 0.12): The paradox is a pure logical constraint with minimal extractive content — it does not extract resources from any agent, nor does it privilege any particular agent's beliefs or interests. The value 0.12 reflects the minimal 'cost' of recognizing the constraint: any formal system that aspires to unrestricted self-reference and material conditionals must either accept contradiction or abandon one of those properties. This is a genuine logical necessity, not an imposed restriction. Suppression (σ = 0.03): Suppression is near zero because the paradox offers no alternatives to suppress. Any agent working in classical logic with self-reference either encounters the paradox or imposes restrictions to avoid it. There are no suppressed exits or hidden alternatives — the logical structure is transparent. Theater Ratio (θ = 0.15): Theater is minimal. The paradox is a mathematical fact that can be verified through simple symbolic derivation. There is no performative element, no ritual, no pretense — just formal proof. The small nonzero value reflects only the minor pedagogical theater in how the paradox is presented (the 'Curry sentence' framing), but the underlying fact is all substance.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in Curry's Paradox — all four analytical observers reach the same classification: Mountain. This is the hallmark of a true logical constraint. A formal logician, a model theorist, a set theorist, and a type theorist all recognize the same fact: unrestricted self-referential sentences in classical logic derive arbitrary theorems. They may propose different solutions (stratification, paraconsistency, dialogue logic), but they all agree on the constraint's existence and universality. The absence of disagreement is not a weakness of the framework; it is the correct diagnosis: Curry's Paradox is a natural law of logic, invariant across all observation positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to Curry's Paradox. There are no beneficiaries or victims, no power asymmetries, no extraction flows. Every agent occupies the same structural position relative to the constraint: all are observers of a logical fact. The constraint does not privilege any particular agent or coalition. It does not coerce anyone into a particular position — rather, it establishes a logical boundary that any coherent system must respect. This is the defining feature of a Mountain constraint: it is indifferent to all agents and properties equally.
 *
 * MANDATROPHY ANALYSIS:
 *   Curry's Paradox is mandatrophy-resolved by its very nature. It is not a case where naive classification would confuse it with extraction (snare/tangled rope) or coordination (rope/scaffold). All perspectives immediately recognize it as a natural logical law. The constraint serves no coordination function beyond the coordination of avoiding contradiction — which is not a 'benefit' to extract, but a minimal rational condition. The paradox does not generate mandatrophy because it does not masquerade as anything other than what it is: a fundamental incompatibility in unrestricted logical systems. The framework's task here is to certify that this is indeed a Mountain (natural law) and not a degraded Piton (institutional theater pretending to be law) or a Snare (coordinated suppression pretending to be fact). The evidence is overwhelming: the paradox is universal, invariant, derives from first principles, and requires no enforcement or suppression — it simply is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paraconsistent_absorption,
    'In paraconsistent logics that tolerate contradictions, does Curry''s Paradox remain a constraint or become a mere oddity that systems can absorb?',
    'Analysis of paraconsistent and relevant logics (LP, R, et al.) to determine whether they preserve the derivation of arbitrary theorems from Curry sentences or block it through non-classical truth-value assignments or explosion prevention',
    'If absorbed: Curry''s Paradox is constraint-relative to classical logic, not universal (ε drops to 0.05, mountain becomes rope/scaffold). If not absorbed: Curry''s Paradox is universal to any logic with self-reference (ε stays 0.12, mountain confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paraconsistent_absorption, empirical, 'Whether paraconsistent logics truly avoid Curry''s Paradox or merely tolerate its consequences').

omega_variable(
    natural_language_reflection,
    'Is Curry''s Paradox a real constraint on natural language interpretation or an artifact of formalizing natural language into classical logic without preserving pragmatic context?',
    'Linguistic and pragmatic analysis of self-referential sentences in actual language use; investigation of whether speakers assign truth-values to Curry sentences or treat them as defective/pragmatically incoherent',
    'If natural language avoids the paradox through pragmatic repair: Curry''s Paradox is a constraint on formal systems, not on language itself (ε = 0.12 for formal logic domain, but 0.05 for natural language domain — two separate constraints). If natural language exhibits the same failure: Curry''s Paradox is universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_language_reflection, conceptual, 'Whether Curry''s Paradox is a constraint on language or on formal logic systems').

omega_variable(
    hierarchy_vs_universality,
    'Is the Tarski hierarchy of formal languages (L0 with metalanguage L1, L1 with metalanguage L2, etc.) a genuine solution to Curry''s Paradox or merely a way to defer it infinitely?',
    'Formal analysis of whether systems with explicit hierarchy (Tarski, Church-Turing) actually block Curry constructions or whether diagonal arguments can transcend the hierarchy; proof-theoretic comparison of consistency strength',
    'If genuine solution: hierarchies eliminate Curry''s Paradox (constraint shifts from universal to domain-relative — ε drops, type becomes rope for stratified systems). If deferral: Curry''s Paradox remains universal, and hierarchies are a coordination mechanism that does not solve the underlying problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hierarchy_vs_universality, empirical, 'Whether formal hierarchies solve Curry''s Paradox or defer it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currys_paradox, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curry_tr_t0, currys_paradox, theater_ratio, 0, 0.15).
narrative_ontology:measurement(curry_tr_t100, currys_paradox, theater_ratio, 100, 0.15).
narrative_ontology:measurement(curry_tr_t200, currys_paradox, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(curry_be_t0, currys_paradox, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(curry_be_t100, currys_paradox, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(curry_be_t200, currys_paradox, base_extractiveness, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currys_paradox, information_standard).
narrative_ontology:affects_constraint(currys_paradox, liar_paradox).
narrative_ontology:affects_constraint(currys_paradox, godel_incompleteness).
narrative_ontology:affects_constraint(currys_paradox, cantor_diagonal_argument).

% DUAL FORMULATION NOTE:
% Curry's Paradox is a member of the self-reference family, downstream of Cantor's diagonal argument and Gödel's Incompleteness. Unlike the Liar Paradox, it does not rely on negation and applies to any unrestricted logic with material conditionals. It affects set-theoretic foundations, type theory, and formal semantics independently. Each member of this family has its own ε and classification, but they are linked through shared mathematical structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
