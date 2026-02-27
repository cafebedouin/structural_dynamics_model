% ============================================================================
% CONSTRAINT STORY: liar_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liar_paradox, []).

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
 *   constraint_id: liar_paradox
 *   human_readable: The Liar Paradox (Self-Referential Inconsistency)
 *   domain: logic/epistemology
 *
 * SUMMARY:
 *   The Liar Paradox is a self-referential logical impossibility: the
 *   sentence 'This statement is false' generates an irreducible contradiction
 *   in classical logic. If the statement is true, its content asserts
 *   falsehood, making it false. If it is false, its assertion of falsehood is
 *   false, making it true. This creates a cycle with no fixed point in
 *   classical bivalent semantics. Unlike institutional constraints (markets,
 *   regulations, social hierarchies), the liar paradox does not depend on
 *   enforcement, power asymmetry, or beneficiary/victim relationships. It is
 *   a limit imposed by the axioms of formal reasoning itself. The constraint
 *   is invariant across all formal systems that include self-reference and
 *   bivalent truth assignment. It cannot be negotiated, escaped, or
 *   engineered away — only formalized, stratified, or re-axiomatized. This
 *   makes the liar paradox a canonical example of a Mountain constraint: a
 *   natural law of logical space with zero degrees of freedom for all agents.
 *
 * KEY AGENTS:
 *   - Logician: Analytical agent (analytical/analytical) — observes the paradox as a structural feature of formal language, not a problem to solve but a limit to recognize
 *   - Formal System Designer: Institutional agent (institutional/mobile) — must design around the paradox through type hierarchies, stratification, or non-classical semantics; has mobile exit options through system redesign but cannot eliminate the underlying constraint
 *   - Student/Reasoner: Moderate agent (moderate/constrained) — encounters the paradox when engaging the proposition; constrained by the necessity of the logical contradiction
 *   - Mathematical Logician: Powerful agent (powerful/arbitrage) — has institutional resources and technical sophistication but no escape from the constraint; arbitrage options in system choice do not dissolve the paradox itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liar_paradox, 0.08).
domain_priors:suppression_score(liar_paradox, 0.03).
domain_priors:theater_ratio(liar_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liar_paradox, extractiveness, 0.08).
narrative_ontology:constraint_metric(liar_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(liar_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(liar_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liar_paradox, mountain).
narrative_ontology:human_readable(liar_paradox, "The Liar Paradox (Self-Referential Inconsistency)").
narrative_ontology:topic_domain(liar_paradox, "logic/epistemology").

domain_priors:emerges_naturally(liar_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL ANALYST (MOUNTAIN) — From the perspective of formal logic and proof theory, the liar paradox is an irreducible structural impossibility. The paradox is not contingent on institutional arrangement, measurement context, or observer position. It follows necessarily from the axioms of self-reference and truth-value assignment in classical logic. No escape mechanism exists that does not fundamentally alter the logical system itself. This is a constraint imposed by the laws of formal reasoning.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL LOGICIAN (MOUNTAIN) — Even for agents with sophisticated technical resources and institutional power, the liar paradox cannot be engineered away through clever frameworks. Tarski's undefinability theorem, Gödel's incompleteness results, and the logical hierarchy (object language vs metalanguage) are formal constraints that no amount of power or sophistication can circumvent. The paradox reveals a fundamental limit to what any formal system can express about itself. Access to arbitrage options does not dissolve the underlying impossibility.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: STUDENT OF LOGIC (MOUNTAIN) — Even agents with limited power and constrained exit options cannot escape the paradox through ignorance or non-engagement. Encountering the sentence 'This statement is false' forces the same logical contradiction regardless of the agent's resources or institutional position. The constraint is imposed uniformly across all observers who engage with the proposition. No exit option exists through evasion or reinterpretation within classical logic.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: FORMAL SYSTEMS DESIGNER (MOUNTAIN) — Institutions attempting to codify logic (formal systems, proof assistants, automated theorem provers) encounter the paradox as a fundamental engineering constraint. Designers of type-theoretic systems, constructive logics, and modern proof assistants must explicitly exclude self-referential sentences or adopt stratified languages. This is not an enforcement choice — it is a mathematical necessity. The liar paradox is not suppressed through institutional will; it is avoided through architectural design that respects its inexorability.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liar_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(liar_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(liar_paradox, ExtMetricName, E),
    domain_priors:suppression_score(liar_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(liar_paradox),
    narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(liar_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(liar_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The liar paradox does not extract value from any agent or for any beneficiary. No agent benefits from the paradox; no agent is victimized by it in the sense of resource transfer. It is a structural limit, not a mechanism of exploitation. The non-zero value (0.08 rather than 0.00) reflects that encountering the paradox imposes a cognitive cost on the reasoner — the necessity of recognizing the inconsistency constitutes a minimal 'extraction' of intellectual effort. Suppression (0.03): Negligible. The paradox cannot be suppressed. Agents cannot avoid it through ignorance or non-engagement without fundamentally limiting their logical capacity. The minimal suppression value reflects that the paradox is avoided only through explicit architectural choices (stratified languages, type theories) in formal systems — but these are not suppression of the paradox itself, only its prevention through system design. Theater ratio (0.15): Minimal. The paradox has zero performative content. Its expression and recognition are entirely transparent. There is no gap between the appearance and the reality of the logical constraint. The minimal (non-zero) value reflects only that formal languages involve syntax and presentation conventions, which have trivial performative content. The paradox does not accumulate theater over time because its logical status is invariant.
 *
 * PERSPECTIVAL GAP:
 *   Unlike typical Deferential Realism constraints, the liar paradox exhibits NO perspectival gap. All four perspectives classify it as Mountain from fundamentally different starting positions: the analytical observer sees it as a universal logical limit; the powerful agent with arbitrage options still cannot escape it; the moderately-powered constrained agent is subject to the same irreducible contradiction; the institutional formal systems designer must build around it but cannot eliminate it. This uniformity of classification is diagnostic of a genuine natural law. The constraint's impossibility is not observer-relative, context-dependent, or contingent on power, time, or scope. This is precisely the opposite of constraints like the verification bottleneck, which appear as six different types from six different perspectives. The liar paradox appears as Mountain from all perspectives — not because all agents agree, but because none have any exit option whatsoever.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY ISSUE. The liar paradox cannot be mislabeled as extraction or coordination because it involves no beneficiaries, no victims, and no transfer of value. The paradox is a logical structure, not a social structure. The baseline extractiveness (0.08) is so minimal that the constraint cannot satisfy any extraction-type threshold (snare, tangled rope, piton). The suppression is so low (0.03) that suppression gates are not approached. The theater ratio is negligible (0.15). The constraint passes the Mountain gates definitively: extractiveness ≤ 0.25 (actual: 0.08), suppression ≤ 0.05 (actual: 0.03), emerges_naturally = true, accessibility_collapse ≥ 0.85 (actual: 0.92), resistance ≤ 0.15 (actual: 0.08). This is a clean mountain with no hidden extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_value_assignment,
    'Is the constraint a logical impossibility inherent to classical truth-value semantics, or does it reveal a flaw in the assumption that every proposition must have a truth value?',
    'Comparison of formal systems with and without bivalence (classical vs multi-valued logic, paraconsistent logic). Analysis of whether truth-value gaps or truth-value gluts resolve the paradox or merely relocate it.',
    'If truth-value assignment is the issue: the paradox is contingent on bivalent semantics (not a mountain). If self-reference is the issue: the paradox is fundamental (mountain). This omega determines whether the constraint is truly immutable or system-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truth_value_assignment, conceptual, 'Whether the paradox is inherent to truth-value semantics or to self-reference itself').

omega_variable(
    semantic_vs_syntactic_resolution,
    'Does the paradox reside in the semantics of truth or in the syntax of self-reference, and can these be separated?',
    'Formal analysis of Tarski''s hierarchy (object language / metalanguage separation). Examination of whether removing the ability to quantify over truth predicates eliminates the paradox or merely suppresses its expression.',
    'If semantic: paradox may be resolvable through revised truth-predicate definitions (contingent). If syntactic: paradox is unavoidable in any system with sufficient expressive power (mountain). If inseparable: paradox marks a fundamental limit to formal expression (strong mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_vs_syntactic_resolution, conceptual, 'Whether the paradox is a semantic or syntactic phenomenon').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liar_paradox, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liar_tr_t0, liar_paradox, theater_ratio, 0, 0.12).
narrative_ontology:measurement(liar_tr_t1000, liar_paradox, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(liar_tr_t2000, liar_paradox, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(liar_be_t0, liar_paradox, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(liar_be_t1000, liar_paradox, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(liar_be_t2000, liar_paradox, base_extractiveness, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liar_paradox, information_standard).
narrative_ontology:affects_constraint(liar_paradox, godels_incompleteness).
narrative_ontology:affects_constraint(liar_paradox, tarskis_undefinability).
narrative_ontology:affects_constraint(liar_paradox, self_reference_in_type_theory).

% DUAL FORMULATION NOTE:
% The liar paradox is upstream to multiple formal impossibility results (Gödel's Incompleteness, Tarski's Undefinability Theorem). These constraints share the same structural origin — the impossibility of self-reference in formal systems with sufficient expressive power — but manifest in different domains (completeness vs truth-definability). The family of constraints forms a cluster linked by the common generative principle: self-referential negation creates irreducible fixed-point problems in formal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
