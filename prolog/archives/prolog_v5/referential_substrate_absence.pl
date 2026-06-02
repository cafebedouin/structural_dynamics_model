% ============================================================================
% CONSTRAINT STORY: referential_substrate_absence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_referential_substrate_absence, []).

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
 *   constraint_id: referential_substrate_absence
 *   human_readable: Referential Substrate Absence in Protocol Application
 *   domain: epistemic_methodology/meta_analysis/protocol_application
 *
 * SUMMARY:
 *   The referential substrate absence constraint represents a logical
 *   boundary condition in epistemic methodology: protocols designed to
 *   analyze substantive content cannot operate on inputs that contain only
 *   metadata pointers to content stored elsewhere. This is not a resource
 *   limitation, institutional barrier, or coordination failure — it is a type
 *   mismatch in the formal sense. A meta-analytic protocol requiring
 *   extractable claims, entities, or mechanisms encounters an immediate and
 *   absolute barrier when presented with text that references such content
 *   without containing it. The constraint emerges naturally from the logical
 *   structure of protocol application and exhibits zero degrees of freedom
 *   across all observer positions. No agent experiences this as extraction,
 *   coordination, or institutional dysfunction — all perspectives converge on
 *   the same classification (mountain) because the constraint is genuinely
 *   immutable within the scope of protocol-based analysis. The low
 *   extractiveness (0.08) reflects only the minimal opportunity cost of
 *   attempting to apply an inapplicable protocol; the low suppression (0.02)
 *   reflects that no coercive mechanism enforces the constraint — it is
 *   self-enforcing through logical necessity. The theater ratio (0.15) is
 *   minimal because there is little performative activity around
 *   acknowledging a type error — practitioners simply recognize the mismatch
 *   and halt application.
 *
 * KEY AGENTS:
 *   - Protocol Designer: Institutional actor (institutional/arbitrage) — defines input requirements as part of protocol specification; experiences constraint as definitional boundary
 *   - Protocol Executor: Practitioner (moderate/constrained) — attempts to apply protocol; encounters immediate hard stop when substrate is absent
 *   - Downstream Consumer: End user (powerless/trapped) — depends on protocol output; cannot proceed when protocol cannot be applied
 *   - Methodological Standards Body: Organized institutional actor (organized/mobile) — sets standards for systematic review; recognizes substrate absence as fundamental exclusion criterion
 *   - Analytical Observer: Meta-level observer (analytical/analytical) — sees constraint as logical necessity, type error in formal sense
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(referential_substrate_absence, 0.08).
domain_priors:suppression_score(referential_substrate_absence, 0.02).
domain_priors:theater_ratio(referential_substrate_absence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(referential_substrate_absence, extractiveness, 0.08).
narrative_ontology:constraint_metric(referential_substrate_absence, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(referential_substrate_absence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(referential_substrate_absence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(referential_substrate_absence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(referential_substrate_absence, mountain).
narrative_ontology:human_readable(referential_substrate_absence, "Referential Substrate Absence in Protocol Application").
narrative_ontology:topic_domain(referential_substrate_absence, "epistemic_methodology/meta_analysis/protocol_application").

domain_priors:emerges_naturally(referential_substrate_absence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint is a logical necessity: a protocol requiring substantive content cannot operate on input lacking that content. This is a type error in the formal sense — applying a function to an argument outside its domain. No amount of institutional reform, technological advancement, or perspective shift can make a metadata pointer contain the substantive claims it references.
constraint_indexing:constraint_classification(referential_substrate_absence, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROTOCOL DESIGNER (MOUNTAIN) — Designers of meta-analytic protocols experience this as an immutable precondition: the protocol's input requirements are definitional. A protocol designed to extract claims, evaluate evidence, or assess mechanisms cannot be applied to text that contains only references to such content elsewhere. The constraint is not a design choice but a logical boundary of what the protocol can coherently do.
constraint_indexing:constraint_classification(referential_substrate_absence, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL EXECUTOR (MOUNTAIN) — The practitioner attempting to apply the protocol encounters an immediate, insurmountable barrier: the required content is not present in the input. This is not a resource constraint (more time/funding would not help) or a skill constraint (more training would not help). The input structurally lacks what the protocol requires. The executor experiences this as a hard stop, not as extraction or coordination failure.
constraint_indexing:constraint_classification(referential_substrate_absence, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: DOWNSTREAM CONSUMER (MOUNTAIN) — Agents who would consume the protocol's output (systematic reviewers, policy analysts, research synthesizers) experience the constraint as an absolute barrier to their work. No amount of effort on their part can produce the required analysis from absent substrate. The constraint is not imposed by any extractive actor — it emerges from the logical structure of the task itself.
constraint_indexing:constraint_classification(referential_substrate_absence, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: METHODOLOGICAL STANDARDS BODY (MOUNTAIN) — Organizations that set standards for systematic review and meta-analysis (Cochrane, PRISMA, Campbell Collaboration) recognize substrate absence as a fundamental exclusion criterion, not a methodological choice. Standards can evolve to handle new evidence types or synthesis methods, but they cannot evolve to extract content that is not present. The constraint is invariant across all methodological frameworks.
constraint_indexing:constraint_classification(referential_substrate_absence, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(referential_substrate_absence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(referential_substrate_absence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(referential_substrate_absence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(referential_substrate_absence, ExtMetricName, E),
    domain_priors:suppression_score(referential_substrate_absence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(referential_substrate_absence),
    narrative_ontology:constraint_metric(referential_substrate_absence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(referential_substrate_absence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(referential_substrate_absence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint extracts almost nothing because it is not an institutional arrangement or coordination mechanism — it is a logical boundary. The minimal extractiveness reflects only the opportunity cost of time spent recognizing the type mismatch. No agent benefits from the constraint's existence; it is not maintained by any extractive actor. Suppression (0.02): Minimal. The constraint is self-enforcing through logical necessity. No institutional mechanism is required to prevent protocol application to absent substrate — the protocol simply cannot operate. The minimal suppression reflects only the cognitive cost of recognizing the mismatch. Theater ratio (0.15): Very low. There is minimal performative activity around this constraint. Practitioners do not engage in elaborate rituals to acknowledge a type error — they simply recognize that the input lacks required content and halt application. The small theater component reflects only the documentation overhead of recording why a protocol was not applied. Accessibility collapse (0.92): Very high. The constraint is immediately apparent to any agent attempting protocol application. The presence or absence of substantive content is directly observable. No specialized knowledge or institutional position is required to detect the mismatch. Resistance (0.08): Very low. The constraint cannot be circumvented through effort, resources, or institutional reform. It is a logical necessity that persists regardless of context. The minimal resistance reflects only the trivial possibility of misunderstanding what the protocol requires (which would be corrected upon clarification, not through overcoming the constraint).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap — all five perspectives classify as mountain. This is the diagnostic signature of a genuine natural law within the scope of protocol application. The protocol designer, executor, downstream consumer, standards body, and analytical observer all experience the constraint identically: as an immutable logical boundary. There is no institutional actor who benefits from the constraint, no powerless agent who bears disproportionate cost, no organized coalition building alternative pathways, and no degraded ritual maintaining the constraint through inertia. The uniform classification across all perspectives confirms that this is not a contingent institutional arrangement being naturalized — it is an actual logical necessity. The constraint is invariant across all observables: whether measuring by claim extraction protocols, entity recognition protocols, mechanism mapping protocols, or any other substantive analysis framework, the result is the same — absent substrate cannot be analyzed.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims because it is not an institutional arrangement or coordination mechanism. It is a logical boundary condition. All agents experience it identically as an immutable barrier. The directionality derivation chain produces d ≈ 0.50 (symmetric) for all perspectives because no agent is structurally advantaged or disadvantaged by the constraint's existence — it simply is. The effective extraction chi is uniformly low across all perspectives because the constraint extracts nothing; it merely defines the boundary of what is logically possible within protocol-based analysis. The engine will compute this as a true mountain (not a false summit) because the structural data confirms zero degrees of freedom: no perspective sees coordination, extraction, or institutional dysfunction.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint resolves the mandatrophy by demonstrating what a true mountain looks like in epistemic methodology. It is not a coordination mechanism mislabeled as natural law (no beneficiaries, no coordination function). It is not extraction naturalized as necessity (no victims, no asymmetric cost distribution). It is not a degraded institution maintained through inertia (minimal theater, no performative maintenance). It is a logical boundary condition that emerges naturally from the structure of protocol application and exhibits zero degrees of freedom across all observer positions. The constraint passes all mountain gates: base extraction ≤ 0.25 (0.08), suppression ≤ 0.05 (0.02), emerges naturally (true), accessibility collapse ≥ 0.85 (0.92), resistance ≤ 0.15 (0.08). The NL profile is complete and the certification chain succeeds. This is what the framework's mountain classification is designed to capture: constraints that are genuinely immutable within their scope, not contingent arrangements claiming immutability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(referential_substrate_absence, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(referential_substrate_absence, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. It represents a single, unified logical boundary condition with no alternative formulations that would yield different epsilon values. The constraint is invariant across all measurement methodologies because it is a type error, not an empirical claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
