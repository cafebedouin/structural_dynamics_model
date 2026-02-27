% ============================================================================
% CONSTRAINT STORY: naming_as_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naming_as_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naming_as_control
 *   human_readable: The Ontological Hegemony
 *   domain: social/political/linguistic
 *
 * SUMMARY:
 *   This constraint occurs when a dominant institution exercises power by
 *   defining the legal and social vocabulary through which reality is
 *   interpreted. This can lead to the marginalization of alternative
 *   perspectives and the suppression of challenges to the status quo.
 *
 * KEY AGENTS:
 *   - Dominant Institution: Primary beneficiary (institutional/arbitrage) – Benefits from maintaining control over the vocabulary and narratives.
 *   - Marginalized Groups: Primary victim (powerless/trapped) – Trapped within the dominant vocabulary and narratives.
 *   - Academics and Activists: Moderate actor (moderate/constrained) – Constrained by the dominant framework but also benefit from challenging it.
 *   - Alternative Knowledge Systems: Secondary victim (powerless/trapped) - Are suppressed and made invisible by the dominant terminology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naming_as_control, 0.6).
domain_priors:suppression_score(naming_as_control, 0.7).
domain_priors:theater_ratio(naming_as_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naming_as_control, extractiveness, 0.6).
narrative_ontology:constraint_metric(naming_as_control, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naming_as_control, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naming_as_control, tangled_rope).
narrative_ontology:human_readable(naming_as_control, "The Ontological Hegemony").
narrative_ontology:topic_domain(naming_as_control, "social/political/linguistic").

domain_priors:requires_active_enforcement(naming_as_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naming_as_control, dominant_institution).
narrative_ontology:constraint_victim(naming_as_control, marginalized_groups).
narrative_ontology:constraint_victim(naming_as_control, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Marginalized groups are trapped within the dominant vocabulary and narratives, unable to effectively challenge the status quo or articulate alternative realities.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Academics and activists are constrained by the dominant framework but also benefit from challenging and deconstructing it, gaining recognition and influence. The extraction manifests as limited reach and systemic barriers.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The dominant institution benefits from maintaining its control over the vocabulary and narratives, solidifying its power and legitimacy. It can arbitrage by adapting the meanings of terms to maintain control.
constraint_indexing:constraint_classification(naming_as_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical observers recognize the tangled nature of the constraint, seeing both the coordination benefits for the dominant institution and the extractive effects on marginalized groups.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naming_as_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(naming_as_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naming_as_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(naming_as_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naming_as_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the dominant institution actively shapes the understanding of reality. Suppression is also high as alternatives are actively marginalized. The theater ratio is moderate because while there is some performative aspect to the maintenance of this hegemony, it is also substantially structural.
 *
 * PERSPECTIVAL GAP:
 *   Marginalized groups experience this as a snare because they lack the power to escape the dominant narratives. Academics and activists experience it as a tangled rope because they can challenge the hegemony, but face constraints and barriers. The dominant institution sees this as a functional coordination mechanism that serves to maintain social order.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant institution benefits by solidifying its power. Marginalized groups are harmed by having their perspectives suppressed. Academics and activists have a mixed experience, depending on their resources and strategies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_truth,
    'Is there an objective truth independent of social construction, or is all knowledge inherently shaped by power dynamics?',
    'Philosophical inquiry and historical analysis of knowledge production.',
    'If objective truth exists, the constraint is weaker. If not, the constraint is much stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_truth, conceptual, 'The degree to which truth is socially constructed vs. objective.').

omega_variable(
    accessibility_alternative_vocabularies,
    'How accessible and influential are alternative vocabularies and narratives that challenge the dominant framework?',
    'Sociological studies of language use and media consumption.',
    'Greater accessibility weakens the constraint; limited accessibility strengthens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_alternative_vocabularies, empirical, 'The availability and impact of alternative vocabularies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naming_as_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nami_tr_t0, naming_as_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nami_tr_t5, naming_as_control, theater_ratio, 5, 0.3).
narrative_ontology:measurement(nami_tr_t10, naming_as_control, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(nami_be_t0, naming_as_control, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nami_be_t5, naming_as_control, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nami_be_t10, naming_as_control, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naming_as_control, enforcement_mechanism).
narrative_ontology:affects_constraint(naming_as_control, media_narrative_control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
