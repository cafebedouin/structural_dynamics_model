% ============================================================================
% CONSTRAINT STORY: governance_overfitting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance_overfitting, []).

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
 *   constraint_id: governance_overfitting
 *   human_readable: Hyper-Specific Compliance Lock-in
 *   domain: political/technological
 *
 * SUMMARY:
 *   This constraint occurs when a governance framework is so precisely tuned
 *   to past edge cases that it becomes a rigid barrier to novel coordination.
 *   This can stifle innovation and make it difficult to adapt to new
 *   challenges. The framework benefits incumbent regulated entities and the
 *   maintainers of the framework, but it harms emerging innovators and novel
 *   coordination initiatives.
 *
 * KEY AGENTS:
 *   - Incumbent Regulated Entities: Beneficiaries of the status quo. (moderate/constrained)
 *   - Governance Framework Maintainers: Benefit from the perpetuation of the framework (institutional/constrained)
 *   - Novel Coordination Initiatives: Victims of the rigid compliance requirements. (powerless/trapped)
 *   - Emerging Innovators: Victims of the rigid compliance requirements. (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance_overfitting, 0.55).
domain_priors:suppression_score(governance_overfitting, 0.7).
domain_priors:theater_ratio(governance_overfitting, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance_overfitting, extractiveness, 0.55).
narrative_ontology:constraint_metric(governance_overfitting, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(governance_overfitting, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance_overfitting, tangled_rope).
narrative_ontology:human_readable(governance_overfitting, "Hyper-Specific Compliance Lock-in").
narrative_ontology:topic_domain(governance_overfitting, "political/technological").

domain_priors:requires_active_enforcement(governance_overfitting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance_overfitting, incumbent_regulated_entities).
narrative_ontology:constraint_beneficiary(governance_overfitting, governance_framework_maintainers).
narrative_ontology:constraint_victim(governance_overfitting, novel_coordination_initiatives).
narrative_ontology:constraint_victim(governance_overfitting, emerging_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Emerging innovators face a snare as they are trapped within the governance framework and unable to effectively coordinate new solutions due to rigid compliance requirements.
constraint_indexing:constraint_classification(governance_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Maintainers of the governance framework perceive it as a piton. They are constrained by the existing framework and find it difficult to adapt to novel situations, leading to inertia and reduced effectiveness.
constraint_indexing:constraint_classification(governance_overfitting, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Incumbent entities benefit from the status quo enforced by the overfit governance framework, which creates barriers to entry for new competitors and reduces the pressure for innovation. However, they are also constrained by the framework.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees this as a tangled rope. The overfitted framework provides some stability but also hinders progress and adaptation to new challenges, causing long-term issues.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_overfitting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance_overfitting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_overfitting, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance_overfitting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance_overfitting, TR),
    TR >= 0.70.

:- end_tests(governance_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The overfitted governance framework extracts value from novel coordination initiatives and emerging innovators, who must spend significant resources to comply with the framework. Suppression (0.70): The framework suppresses novel coordination by making it difficult to adapt to new challenges. Theater ratio (0.75): The framework's primary function is compliance, with a high degree of performative activity relative to genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   Emerging innovators perceive a snare because they are trapped within the rigid framework. Incumbent entities see it as tangled rope as they are both benefited and constrained. Framework maintainers view it as a piton, degraded and difficult to adapt. The analytical observer recognizes the framework's tangled nature as it creates a stable but stifling environment.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent entities and framework maintainers benefit from the status quo, while emerging innovators and novel initiatives bear the costs of compliance. The piton perspective reflects the inertia of the framework maintainers. The analytical perspective considers the long-term effects on societal progress.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as Tangled Rope because it provides some stability and coordination, but at the cost of stifling innovation. It is not a pure Snare because it does offer some benefits to certain actors. Resolving this mandatrophy involves finding ways to make the framework more adaptable without sacrificing its core functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptability_vs_specificity,
    'What is the optimal balance between a highly specific governance framework and a more adaptable one?',
    'Longitudinal analysis of governance frameworks and their impact on innovation and coordination.',
    'A more adaptable framework may foster innovation, while a highly specific framework may stifle it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptability_vs_specificity, empirical, 'Balancing adaptability with specificity in governance.').

omega_variable(
    incentive_alignment,
    'How can the incentives of governance framework maintainers be aligned with fostering innovation and coordination?',
    'Analysis of different incentive structures and their effectiveness in promoting innovation.',
    'Better aligned incentives may lead to a more adaptable and effective governance framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_alignment, conceptual, 'Aligning incentives to promote innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gove_tr_t0, governance_overfitting, theater_ratio, 0, 0.6).
narrative_ontology:measurement(gove_tr_t5, governance_overfitting, theater_ratio, 5, 0.7).
narrative_ontology:measurement(gove_tr_t10, governance_overfitting, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(gove_be_t0, governance_overfitting, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gove_be_t5, governance_overfitting, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gove_be_t10, governance_overfitting, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance_overfitting, enforcement_mechanism).
narrative_ontology:affects_constraint(governance_overfitting, regulatory_capture).
narrative_ontology:affects_constraint(governance_overfitting, technological_lockin).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
