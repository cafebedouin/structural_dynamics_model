% ============================================================================
% CONSTRAINT STORY: ship_of_theseus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ship_of_theseus, []).

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
 *   constraint_id: ship_of_theseus
 *   human_readable: Ship of Theseus: Identity Continuity Constraint
 *   domain: philosophical/metaphysics/ontology
 *
 * SUMMARY:
 *   The Ship of Theseus is a canonical philosophical constraint on identity
 *   continuity. The paradox arises when an object has all its parts gradually
 *   replaced: is the reconstructed whole identical to the original? The
 *   constraint is not a practical problem but a logical one. It emerges from
 *   the structure of identity itself and the requirement that identity must
 *   be reflexive, transitive, and consistent with the principle of
 *   indiscernibility of identicals. No agent can escape this constraint by
 *   stipulating a convention — any convention must itself satisfy the logical
 *   requirement of consistency. The constraint exhibits zero degrees of
 *   freedom across all indices. It is invariant across time horizons,
 *   observer positions, and measurement methodologies. Unlike extraction
 *   constraints (Snare, Tangled Rope) that vary by perspective, this
 *   constraint classifies as Mountain from every structural viewpoint because
 *   the fundamental question—'what makes something the same thing across
 *   changes?'—cannot be bypassed. The theater_ratio is low (0.15) because the
 *   constraint is not performative; the logical structure is transparent. The
 *   extractiveness is minimal (0.08) because no agent extracts value from the
 *   constraint; it is a shared structural limit on rational discourse.
 *
 * KEY AGENTS:
 *   - Logical Analyst: Analytical agent (analytical/analytical) — recognizes the constraint as stemming from the axioms of identity; no exit
 *   - Metaphysical Observer: Contemplative agent (analytical/analytical) — sees the constraint as requiring coherent identity criteria; cannot dissolve it
 *   - Practical Decider: Agent with decision-making authority (analytical/analytical) — must decide which ship is 'the' ship; bound by logical constraints even when stipulating a criterion
 *   - The Logical Structure Itself: Observer-independent (analytical/analytical) — the constraint is a feature of rational identity discourse, not of any particular observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ship_of_theseus, 0.08).
domain_priors:suppression_score(ship_of_theseus, 0.02).
domain_priors:theater_ratio(ship_of_theseus, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ship_of_theseus, extractiveness, 0.08).
narrative_ontology:constraint_metric(ship_of_theseus, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ship_of_theseus, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ship_of_theseus, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ship_of_theseus, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ship_of_theseus, mountain).
narrative_ontology:human_readable(ship_of_theseus, "Ship of Theseus: Identity Continuity Constraint").
narrative_ontology:topic_domain(ship_of_theseus, "philosophical/metaphysics/ontology").

domain_priors:emerges_naturally(ship_of_theseus).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL ANALYST (MOUNTAIN) — The constraint emerges from the logical structure of identity and continuity itself. Given the axioms of identity (reflexivity, transitivity, indiscernibility of identicals), no escape exists from the paradox. The constraint is invariant across all observation methodologies and temporal frames. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(ship_of_theseus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: METAPHYSICAL OBSERVER (MOUNTAIN) — From the standpoint of ontology, identity through time is fundamentally indeterminate unless identity criteria are stipulated. But the constraint itself—that identity criteria must be coherent and applicable—is invariant. No agent can escape the requirement to specify what makes a thing the same thing across changes. Suppression=0.02 (no coercion exists; the constraint is purely logical). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(ship_of_theseus, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: PRACTICAL DECIDER (MOUNTAIN) — Even when deciding whether the original ship or the reconstructed ship is 'the' Ship of Theseus, the decider cannot escape the logical constraint: any decision rule adopted must be internally consistent and must not violate the principle that identity is determinate. The constraint holds even when we try to dissolve it by stipulating a convention. σ=0.8 (local scope due to specific case) → χ≈0.07.
constraint_indexing:constraint_classification(ship_of_theseus, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ship_of_theseus_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ship_of_theseus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ship_of_theseus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ship_of_theseus, ExtMetricName, E),
    domain_priors:suppression_score(ship_of_theseus, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ship_of_theseus),
    narrative_ontology:constraint_metric(ship_of_theseus, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ship_of_theseus, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ship_of_theseus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No agent extracts value or coercion from the constraint. The paradox is non-extractive; all agents share the same logical burden. The value is above zero only because resolving the constraint requires stipulation of identity criteria, and different criteria may favor different interpretations (a minimal asymmetry). Suppression (0.02): Nearly zero. The constraint is transparent — no alternatives are hidden. The paradox is explicit in formal logic and metaphysics. Resistance (0.08): Very low. The constraint cannot be resisted; it is constitutive of rational identity discourse. Any attempt to 'resist' the constraint by adopting a criterion merely relocates the paradox to the level of defending the criterion itself. Theater ratio (0.15): Low. The constraint involves minimal performative activity. Philosophers present thought experiments, but the thought experiment is transparent about its logical structure. There is no hidden function or disguised purpose. The constraint is what it appears to be: a logical problem, not a social arrangement. Accessibility collapse (0.92): Very high. The constraint is immediately accessible to any rational agent. The thought experiment requires only ordinary concepts (ship, part, identity, time). There is no special expertise needed to understand the paradox. Emerges naturally (true): The constraint emerges from the logical structure of identity, not from any stipulated rule or social convention. It would emerge in any rational system that uses identity predicates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap across power levels or exit options. All perspectives classify as Mountain because the logical structure is invariant. An analyst, a practical decider, and a logical observer all encounter the same constraint: identity criteria must be coherent and applicable. The constraint does not vary with spatial scope, time horizon, or power atom because it is not a contingent social arrangement or empirical limitation. It is a feature of the logical landscape itself. The apparent universality (all perspectives yield Mountain) confirms the mountain classification — the constraint is truly invariant, not merely perceived as invariant from one position.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims exist. The constraint is not extractive, so directionality derivation is not applicable. All agents share the same logical burden equally. The constraint is non-directional by structure: it does not favor one agent over another. The analytical observer perspective (the only perspective available) yields d≈0.72 (canonical analytical d), but this reflects observer position, not structural extraction. f(d)≈1.15 and scope σ=1.0 yield χ≈0.09, which falls well below the thresholds for any non-mountain type (χ<0.25 for Mountain gate). The constraint is classified as Mountain on logical grounds, not on directionality grounds.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply. The Ship of Theseus is a pure logical constraint with no extractive mechanism. There is no risk of mislabeling coordination as extraction or vice versa because the constraint contains neither coordination nor extraction. It is a boundary condition on rational identity discourse. The mandatrophy resolution framework is designed to disambiguate cases where a constraint appears to have both a genuine coordination function and asymmetric extraction. This constraint has neither. It is invariantly Mountain across all possible observation sites and measurement methodologies. The six-type taxonomy is not under threat; the constraint simply falls into one type universally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_boundary_mereology,
    'Is identity continuity a mereological (compositional) property or a spatiotemporal continuity property, and do these two frameworks yield consistent results for all cases?',
    'Formal analysis of mereological axiom systems (classical, extensional, non-monotonic) against spatiotemporal continuity models; identification of cases where frameworks diverge',
    'If consistent: identity criteria can be unified under a single framework (reduces omega uncertainty). If divergent: the constraint reflects an irreducible tension in ontology, not a solvable paradox.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_boundary_mereology, conceptual, 'Mereological vs spatiotemporal frameworks for identity').

omega_variable(
    stipulation_adequacy,
    'Can stipulating an identity criterion (e.g., ''the ship is wherever the majority of original planks are'') eliminate the constraint, or does the constraint re-emerge at the level of defining the criterion itself?',
    'Formal proof that any identity criterion must satisfy the Leibniz law (indiscernibility of identicals); examination of whether stipulations can avoid circularity',
    'If stipulations can eliminate the constraint: it is not a mountain but a coordination problem (Rope). If stipulations always re-instantiate the constraint: the mountain classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stipulation_adequacy, conceptual, 'Whether stipulation can dissolve the constraint').

omega_variable(
    counterfactual_dependence,
    'Does identity depend on counterfactual historical facts (how the ship could have been repaired) or only on actual continuity?',
    'Analysis of counterexamples with multiple histories of modification; correlation with intuitions in metaphysical thought experiments; formalization in modal logic',
    'If counterfactual-dependent: identity criteria require possible-world semantics, increasing constraint complexity. If actual-dependent only: simpler criteria available, but may conflict with modal intuitions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_dependence, conceptual, 'Role of counterfactual facts in identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ship_of_theseus, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theseus_tr_t0, ship_of_theseus, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theseus_tr_t1, ship_of_theseus, theater_ratio, 1, 0.15).

% Extraction over time
narrative_ontology:measurement(theseus_be_t0, ship_of_theseus, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(theseus_be_t1, ship_of_theseus, base_extractiveness, 1, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ship_of_theseus, information_standard).
narrative_ontology:affects_constraint(ship_of_theseus, personal_identity_persistence).
narrative_ontology:affects_constraint(ship_of_theseus, object_identity_criterion).
narrative_ontology:affects_constraint(ship_of_theseus, sorites_paradox).

% DUAL FORMULATION NOTE:
% The Ship of Theseus is the canonical exemplar of a constraint family on identity and continuity. Related constraints include personal identity persistence (psychological continuity vs bodily continuity), object identity criteria (substance vs form), and the sorites paradox (vagueness at boundaries). All share the same logical structure: indeterminacy of identity under continuous change. The Ship of Theseus differs from the sorites paradox in that the sorites involves vagueness (borderline cases), while Theseus involves ambiguity (multiple coherent criteria conflict). Both are logical constraints (Mountains), but with different underlying structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
