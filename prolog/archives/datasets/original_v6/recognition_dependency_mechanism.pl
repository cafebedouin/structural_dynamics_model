% ============================================================================
% CONSTRAINT STORY: recognition_dependency_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_recognition_dependency_mechanism, []).

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
 *   constraint_id: recognition_dependency_mechanism
 *   human_readable: Recognition Dependency as Psychological Constraint on Agency
 *   domain: moral_philosophy/psychology_of_agency/social_epistemology
 *
 * SUMMARY:
 *   Recognition dependency — the psychological mechanism by which external
 *   validation structures an agent's choice of standards, goals, and
 *   self-concept — is a candidate natural law constraint in moral philosophy
 *   and the psychology of agency. The constraint claims that humans cannot
 *   simply choose to be indifferent to social recognition; the need for
 *   validation from others is a structural feature of social cognition that
 *   emerges from evolutionary psychology (social species require coordination
 *   mechanisms), developmental psychology (self-concept forms through
 *   mirroring and attachment), and phenomenology (self-consciousness is
 *   inherently intersubjective). This constraint is uniform-type: it
 *   classifies as Mountain from all perspectives because the mechanism
 *   operates at a level below voluntary control. Agents can learn to manage
 *   recognition dependency, redirect it toward healthier sources, or build
 *   meta-cognitive awareness of how it influences them, but they cannot
 *   eliminate the dependency itself. The constraint exhibits the natural law
 *   signature: emerges naturally (no institution creates the dependency),
 *   accessibility collapse is high (agents cannot access alternative
 *   motivational architectures), and resistance is low (attempts to simply
 *   ignore recognition needs typically fail or produce pathology).
 *
 * KEY AGENTS:
 *   - Recognition-Dependent Agent: Universal human condition (powerless/trapped at immediate horizon) — experiences validation needs as immediate and unchangeable
 *   - Self-Aware Agent: Agent with meta-cognitive capacity (moderate/constrained at biographical horizon) — can observe and manage the dependency but not eliminate it
 *   - Institutional Designer: Architects of recognition systems (powerful/mobile at generational horizon) — can channel dependency toward prosocial ends but cannot remove the underlying mechanism
 *   - Therapeutic Community: Organized interventions (organized/mobile at generational horizon) — develops practices for healthier relationship to recognition needs
 *   - Analytical Observer: Philosophical and scientific perspective (analytical/analytical at civilizational horizon) — identifies recognition dependency as a structural feature of human psychology grounded in evolutionary, developmental, and phenomenological evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(recognition_dependency_mechanism, 0.08).
domain_priors:suppression_score(recognition_dependency_mechanism, 0.03).
domain_priors:theater_ratio(recognition_dependency_mechanism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(recognition_dependency_mechanism, extractiveness, 0.08).
narrative_ontology:constraint_metric(recognition_dependency_mechanism, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(recognition_dependency_mechanism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(recognition_dependency_mechanism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(recognition_dependency_mechanism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(recognition_dependency_mechanism, mountain).
narrative_ontology:human_readable(recognition_dependency_mechanism, "Recognition Dependency as Psychological Constraint on Agency").
narrative_ontology:topic_domain(recognition_dependency_mechanism, "moral_philosophy/psychology_of_agency/social_epistemology").

domain_priors:emerges_naturally(recognition_dependency_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECOGNITION-DEPENDENT AGENT / IMMEDIATE (MOUNTAIN) — At immediate time horizons, the psychological need for external validation appears as an unchangeable constraint. The agent cannot simply decide to stop caring about recognition — the dependency is a structural feature of their motivational architecture. Zero degrees of freedom.
constraint_indexing:constraint_classification(recognition_dependency_mechanism, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SELF-AWARE AGENT / BIOGRAPHICAL (MOUNTAIN) — Even with biographical time and moderate resources for therapy or self-development, the recognition dependency remains largely immutable. Agents can learn to manage the dependency or redirect it, but the underlying psychological mechanism — that social validation structures choice — persists. The constraint can be worked with but not eliminated.
constraint_indexing:constraint_classification(recognition_dependency_mechanism, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL DESIGNER / GENERATIONAL (MOUNTAIN) — Institutions can be designed to channel recognition dependency toward prosocial ends (reputation systems, honor cultures, professional prestige hierarchies), but they cannot eliminate the dependency itself. The mechanism is a fixed constraint that institutional design must accommodate, not overcome.
constraint_indexing:constraint_classification(recognition_dependency_mechanism, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / CIVILIZATIONAL (MOUNTAIN) — From the analytical perspective, recognition dependency is a structural feature of social cognition that emerges from evolutionary psychology, developmental psychology, and the phenomenology of self-consciousness. Humans are fundamentally social beings whose sense of self is constituted through recognition by others. This is not a contingent institutional arrangement but a deep feature of human psychology that holds across all cultures and historical periods.
constraint_indexing:constraint_classification(recognition_dependency_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THERAPEUTIC COMMUNITY / GENERATIONAL (MOUNTAIN) — Organized therapeutic interventions (CBT, psychodynamic therapy, mindfulness practices) can help agents develop healthier relationships to recognition needs, but they work by redirecting or managing the dependency, not by eliminating it. The therapeutic community sees the constraint as a fixed feature of human psychology that requires accommodation and skillful navigation.
constraint_indexing:constraint_classification(recognition_dependency_mechanism, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recognition_dependency_mechanism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(recognition_dependency_mechanism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(recognition_dependency_mechanism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(recognition_dependency_mechanism, ExtMetricName, E),
    domain_priors:suppression_score(recognition_dependency_mechanism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(recognition_dependency_mechanism),
    narrative_ontology:constraint_metric(recognition_dependency_mechanism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(recognition_dependency_mechanism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(recognition_dependency_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. Recognition dependency is not an extractive mechanism — it is a psychological constraint that can be exploited by extractive systems (status hierarchies, approval-withholding manipulation, social media engagement optimization), but the dependency itself extracts nothing. The minimal extractiveness reflects the opportunity cost of attention and the vulnerability the dependency creates to exploitation, but the constraint itself is neutral. Suppression (0.03): Minimal. The constraint does not suppress alternatives through coercion — agents are free to pursue recognition from any source or to develop practices that reduce the dependency's influence. The low suppression reflects only the psychological difficulty of acting against validation needs, not institutional barriers. Theater ratio (0.15): Low. Recognition-seeking behavior is largely functional, not performative. Agents genuinely need validation; the behavior is not a ritual maintained for appearance. Some theater exists (virtue signaling, performative conformity) but these are downstream effects of the constraint, not the constraint itself. Accessibility collapse (0.92): Very high. Agents cannot access motivational architectures that are indifferent to social recognition. Even hermits and ascetics who withdraw from society are typically motivated by recognition from a specific community (religious order, philosophical tradition) or by the meta-recognition of being 'above' ordinary recognition needs. Resistance (0.08): Very low. Attempts to simply ignore recognition needs typically fail. Agents who claim complete indifference to others' opinions are usually either self-deceiving or have redirected the dependency rather than eliminated it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification — all perspectives return Mountain. The gap that does exist is in the *experience* of the constraint: powerless agents at immediate horizons experience recognition dependency as an urgent, inescapable force that structures every choice; analytical observers at civilizational horizons experience it as an abstract structural feature of human psychology that can be studied and understood. But both perspectives agree on the classification: the constraint is unchangeable. The uniformity across perspectives is itself diagnostic — it confirms the natural law status. If any perspective classified the constraint as mutable (Rope, Scaffold, or any extractive type), that would indicate the constraint is not a genuine mountain but a naturalized institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognition dependency is a uniform-type mountain constraint with no beneficiaries or victims. The constraint does not extract from anyone — it is a structural feature of human psychology that all agents experience. Directionality is not applicable because there is no extraction flow. All perspectives use the canonical fallback d values for their power atoms, but because extractiveness is so low (0.08), even the maximum f(d) for powerless agents produces negligible chi. The constraint is experienced as immutable by all agents regardless of their structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   Recognition dependency resolves the mandatrophy by being a genuine natural law rather than a mislabeled coordination or extraction mechanism. The constraint is not a Rope (it is not a coordination solution that could be replaced with an alternative), not a Snare (it does not extract from anyone), not a Tangled Rope (it has no beneficiaries or victims), not a Scaffold (it has no sunset — the dependency persists across all time horizons), and not a Piton (the behavior is functional, not theatrical). The mountain classification is correct from all perspectives because the constraint operates at a level below institutional design: it is a fixed feature of human psychology that institutions must accommodate. The key diagnostic: if recognition dependency were a contingent institutional arrangement rather than a natural law, we would expect to find cultures or historical periods where humans were genuinely indifferent to social validation. No such cultures exist. Every known human society has recognition systems (honor, prestige, reputation, status), and every individual within those societies is motivated by validation from some reference group. The universality and invariance confirm the mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(recognition_dependency_mechanism, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(recognition_dependency_mechanism, identity_coordination).

% DUAL FORMULATION NOTE:
% Recognition dependency is a foundational constraint that many extractive social systems exploit (status hierarchies, approval-withholding manipulation, platform engagement optimization), but it should not be decomposed into multiple stories because the core mechanism — psychological dependence on external validation — has a single, stable epsilon value across all contexts. The exploitation mechanisms are separate constraints with their own epsilon values and should be modeled as downstream constraints that depend on recognition_dependency_mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
