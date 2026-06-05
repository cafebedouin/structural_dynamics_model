% ============================================================================
% CONSTRAINT STORY: well_ordering_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_well_ordering_theorem, []).

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
 *   constraint_id: well_ordering_theorem
 *   human_readable: Well Ordering Theorem
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   The Well Ordering Theorem is a canonical mathematical mountain: a logical
 *   entailment from the Axiom of Choice within ZFC set theory that exhibits
 *   zero degrees of freedom, no beneficiary-victim structure, and no
 *   perspectival variation. The theorem's status is not negotiated, enforced
 *   through coercion, or subject to observational interpretation. It either
 *   obtains within the formal system or it does not. The constraint
 *   exemplifies how the Deferential Realism framework handles pure logical
 *   truths — they classify as mountains because they are immutable from all
 *   structural positions. However, three irreducible uncertainties exist: (1)
 *   whether the Axiom of Choice that grounds the WOT is itself a natural law
 *   or a contingent axiomatic choice, (2) whether alternative set theories
 *   (intuitionistic, constructive) represent genuine escapes from the WOT or
 *   merely formal fragments of ZFC, and (3) whether the WOT is universally
 *   true or true only within the ZFC framework. These omegas do not affect
 *   the classification within ZFC—it remains mountain—but they bear on the
 *   deeper question of whether mathematical truths are discovered (mountains)
 *   or constructed (contingent formal choices).
 *
 * KEY AGENTS:
 *   - ZFC Set Theory Framework: The formal system in which the theorem obtains — not an agent but the context that makes the constraint immutable
 *   - Foundational Mathematicians: Agents who work with set theory and view the WOT as a logical consequence; experience no extraction or enforcement
 *   - Mathematical Community: Collective adoption of ZFC as the standard foundation; no beneficiary-victim structure emerges
 *   - Constructive Mathematicians: Agents who adopt intuitionistic logic or constructive frameworks that reject the Axiom of Choice; experience the WOT as inaccessible rather than suppressed
 *   - Analytical Observers: View the theorem from outside any particular formal system and question whether it is natural law or framework-contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(well_ordering_theorem, 0.05).
domain_priors:suppression_score(well_ordering_theorem, 0.02).
domain_priors:theater_ratio(well_ordering_theorem, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(well_ordering_theorem, extractiveness, 0.05).
narrative_ontology:constraint_metric(well_ordering_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(well_ordering_theorem, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(well_ordering_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(well_ordering_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(well_ordering_theorem, mountain).
narrative_ontology:human_readable(well_ordering_theorem, "Well Ordering Theorem").
narrative_ontology:topic_domain(well_ordering_theorem, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(well_ordering_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOUNDATIONAL MATHEMATICIAN (MOUNTAIN) — From the perspective of formal logic and set theory, the WOT is a pure logical entailment from the Axiom of Choice within ZFC. No agent enforces it, no victim bears its cost, and no social structure mediates it. The theorem is either true or false within the formal system — observer variation does not apply. The constraint emerges as a necessary logical consequence.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — Even for those with institutional power and resources to challenge foundational axioms, the WOT remains immutable within ZFC. Rejecting it would require abandoning the Axiom of Choice or the ZFC framework entirely. This is not a constraint imposed by the mathematical community — it is a feature of the formal system the community has collectively adopted. The community cannot negotiate, relax, or reinterpret the theorem without abandoning ZFC.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: WORKING MATHEMATICIAN (MOUNTAIN) — A mathematician with typical career constraints faces the WOT as an immutable feature of ZFC set theory. They may work within alternative set theories (intuitionistic logic, constructive mathematics) that reject the Axiom of Choice, but this represents choosing a different formal system, not escaping the WOT within ZFC. The constraint's accessibility collapse is total for those operating within the standard framework.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: METAMATHEMATICAL OBSERVER (MOUNTAIN) — At the level of analyzing the logical structure itself, the WOT is invariant across all observational contexts within ZFC. It has no temporal dimension within the formal system (logical truth does not accumulate or degrade), no beneficiaries or victims (logical entailment does not extract resources), and no enforcement mechanism (truth is not enforced — it obtains or fails). The constraint is maximally immutable.
constraint_indexing:constraint_classification(well_ordering_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(well_ordering_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(well_ordering_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(well_ordering_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(well_ordering_theorem, ExtMetricName, E),
    domain_priors:suppression_score(well_ordering_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(well_ordering_theorem),
    narrative_ontology:constraint_metric(well_ordering_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(well_ordering_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(well_ordering_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. The WOT generates no extraction from any agent because it is not enforced by any agent and does not concentrate resources or opportunities. No entity benefits disproportionately from the theorem's existence, and no entity bears costs from its truth. The small nonzero value reflects that within ZFC, the theorem constrains what theories can be formalized — but this constraint is not extractive; it is constitutive of the framework itself. Suppression (0.02): Minimal. The theorem is not suppressed through coercion or lack of alternatives because there is no coercive mechanism. Agents can adopt alternative set theories (intuitionistic logic, constructive frameworks) that reject the Axiom of Choice, though this requires abandoning ZFC rather than evading the WOT within ZFC. Theater ratio (0.08): Minimal. The theorem exhibits almost no performative content. Its proof is formal and verifiable; its status is not subject to interpretation or negotiation. The small nonzero value reflects minor pedagogical theater in how the theorem is presented in textbooks, but the core logical content is transparent and non-theatrical. Accessibility collapse (0.92): Near-total. Within ZFC, the theorem is accessible to any agent who learns formal logic—there is no barrier to understanding or accepting it. The collapse is not from enforcement but from the framework's internal necessity: if you accept ZFC, you logically must accept the WOT. Resistance (0.08): Minimal. No structural forces resist the theorem because no agent has incentive to deny it (unlike constraints where denial would benefit an agent). Only alternative axiom choices (rejecting AoC) generate resistance, and these are systemic choices, not localized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal by design—this is a hallmark of genuine mathematical mountains. All perspectives converge on the same classification (mountain) because the theorem's truth is invariant across observer positions within ZFC. The gap that does exist is between (1) those within ZFC (all perspectives in the analysis above) and (2) observers standing outside ZFC asking whether the theorem is universally true or true-within-this-framework. The analytical observer at the metamathematical level experiences this gap most acutely: they see that ZFC is a contingent choice, yet the WOT is immutable within that choice. This reflects the classic epistemological problem in mathematics: are logical truths discovered (mountains) or constructed (contingent formal choices)? The constraint's mountain classification answers: within ZFC, mountains are mountains. The deeper question about whether ZFC itself is a mountain or a contingent framework is a different constraint (the 'foundational status of ZFC' or similar).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis is not applicable to this constraint in the standard sense. There are no beneficiaries or victims, no asymmetric extraction, and no differentiation by power or exit options. The theorem obtains uniformly across all observer positions within ZFC. The directionality computation f(d) and scope modifiers are moot because extractiveness is zero. This is the signature of a pure natural law—the constraint's force is logical entailment, not structural extraction or resource asymmetry. If alternative set theories (intuitionistic, constructive) are taken as alternative frameworks rather than fragments of ZFC, then agents in those frameworks could be analyzed separately—but they would be separate constraints (well-ordering-in-intuitionistic-logic, etc.) with their own ε values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_of_choice_contingency,
    'Is the Axiom of Choice itself a natural law or a contingent formal choice?',
    'Metamathematical analysis: comparison of proof theoretic strength across set theories with and without AoC; examination of whether AoC is forced by foundational desiderata or represents a genuine choice point in axiomatization',
    'If AoC is contingent: the WOT remains a mountain within ZFC but is demoted to rope or tangled_rope from a constructive/intuitionistic perspective — different frameworks permit different theorems. If AoC is forced: the WOT is a mountain across all coherent foundational frameworks. Current evidence: AoC is a genuine choice point — models of ZF without AoC exist and are consistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_of_choice_contingency, empirical, 'Whether Axiom of Choice is a natural law or contingent axiomatization choice').

omega_variable(
    intuitionistic_alternative_existence,
    'Does intuitionistic logic or constructive mathematics constitute a genuinely different mathematical framework, or are they fragments of classical mathematics?',
    'Proof-theoretic analysis: examine whether intuitionistic set theory (IZF) with its rejection of AoC can serve as a complete mathematical foundation for all mathematical practice, or whether it consistently omits theorems classical mathematics requires',
    'If genuine alternative: mathematicians can exit ZFC entirely and obtain a different set of theorems (including WOT rejection) without logical contradiction. The constraint is then local to ZFC, not universal. If fragment: rejection of AoC is mathematically incoherent and WOT remains universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intuitionistic_alternative_existence, empirical, 'Whether constructive mathematics offers a coherent alternative framework').

omega_variable(
    natural_law_vs_contingent_framework,
    'Is the WOT a natural law of mathematics (true in all possible mathematical universes) or a consequence of the contingent choice to adopt ZFC as a foundational framework?',
    'Comparative axiomatics: detailed analysis of what set of foundational axioms would be forced by any coherent notion of ''set'' and ''mathematical reasoning'', versus which axioms (including AoC) are optional choices that could be replaced with alternatives',
    'If natural law: the WOT is objectively true regardless of formal system. If contingent framework: the WOT is true-within-ZFC but not universally true — it is a constraint imposed by the ZFC framework''s design, not by mathematics itself. This would reclassify the constraint from mountain to false-summit candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_framework, conceptual, 'Whether WOT is a natural law or a contingent ZFC framework consequence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(well_ordering_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wot_tr_t0, well_ordering_theorem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(wot_tr_t50, well_ordering_theorem, theater_ratio, 50, 0.08).
narrative_ontology:measurement(wot_tr_t100, well_ordering_theorem, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(wot_be_t0, well_ordering_theorem, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(wot_be_t50, well_ordering_theorem, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(wot_be_t100, well_ordering_theorem, base_extractiveness, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(well_ordering_theorem, information_standard).
narrative_ontology:affects_constraint(well_ordering_theorem, axiom_of_choice_legitimacy).
narrative_ontology:affects_constraint(well_ordering_theorem, constructive_mathematics_viability).

% DUAL FORMULATION NOTE:
% The Well Ordering Theorem's status as a mountain depends on the Axiom of Choice's status within ZFC. If AoC is itself a mountain (necessary consequence of any coherent set theory), then WOT is unambiguously a mountain. If AoC is a contingent choice (rope/tangled_rope), then WOT is framework-local, not universal. The upstream constraint 'axiom_of_choice_legitimacy' determines whether WOT is truly natural law or framework-contingent. This constraint should be classified as mountain only if the engine can establish that AoC is natural law; otherwise it should be reclassified to rope or tangled_rope framework-constrained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
