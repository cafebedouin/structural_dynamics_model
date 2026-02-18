% ============================================================================
% CONSTRAINT STORY: constraint_interaction_explosion
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_interaction_explosion, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: constraint_interaction_explosion
 * human_readable: The Combinatorial Complexity Trap
 * domain: technological/social
 * * SUMMARY:
 * This constraint occurs when multiple simple coordination mechanisms (Ropes)
 * interact in a non-linear fashion, creating a "feedback explosion" of
 * emergent constraints. While each individual rule is logical, their
 * intersection creates a Snare that extracts all operational optionality
 * from the subjects trapped at the junction.
 * * KEY AGENTS:
 * - Systems Operator: Subject (Powerless)
 * - Regulatory Architect: Beneficiary (Institutional)
 * - Complexity Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) because the "explosion" of rules siphons all
% productivity into purely managing the intersections of the rules.
domain_priors:base_extractiveness(constraint_interaction_explosion, 0.81).
domain_priors:suppression_score(constraint_interaction_explosion, 0.74).
domain_priors:theater_ratio(constraint_interaction_explosion, 0.40).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(constraint_interaction_explosion, extractiveness, 0.81).
narrative_ontology:constraint_metric(constraint_interaction_explosion, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(constraint_interaction_explosion, theater_ratio, 0.40).

% The architects claim the system is for coordination, but the emergent result is different.
narrative_ontology:constraint_claim(constraint_interaction_explosion, tangled_rope).
narrative_ontology:human_readable(constraint_interaction_explosion, "The Combinatorial Complexity Trap").
narrative_ontology:topic_domain(constraint_interaction_explosion, "technological/social").

% Structural properties required for Tangled Rope classification.
domain_priors:requires_active_enforcement(constraint_interaction_explosion).
narrative_ontology:constraint_beneficiary(constraint_interaction_explosion, regulatory_architects).
narrative_ontology:constraint_victim(constraint_interaction_explosion, systems_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the operator, the interaction is a snare: they cannot move without
% violating one of the intersecting, contradictory constraints.
constraint_indexing:constraint_classification(constraint_interaction_explosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views each individual constraint as a vital Rope for safety,
% often ignoring the "explosion" caused by their sum.
constraint_indexing:constraint_classification(constraint_interaction_explosion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature where coordination intent (Ropes) leads to
% emergent predatory extraction (Snare).
constraint_indexing:constraint_classification(constraint_interaction_explosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_interaction_explosion_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(constraint_interaction_explosion, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_interaction_explosion, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(constraint_interaction_explosion, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(constraint_interaction_explosion),
    narrative_ontology:constraint_beneficiary(constraint_interaction_explosion, _),
    narrative_ontology:constraint_victim(constraint_interaction_explosion, _).

test(extraction_threshold) :-
    % Ensure high extraction (0.81) is correctly registered.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_interaction_explosion, ExtMetricName, E),
    E > 0.8.

:- end_tests(constraint_interaction_explosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects the total paralysis of the subject
 * due to combinatorial complexity. Each rule is intended as a Rope, but their sum is a Snare.
 * The suppression score (0.74) reflects the inability to opt-out of the regulatory
 * environment where these rules intersect.
 *
 * PERSPECTIVAL GAP:
 * The Operator experiences a Snare because they are the "ground" where
 * these rules collide. The Architect sees a Rope because they only monitor
 * the performance of individual rules in isolation, not their emergent interactions.
 *
 * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that the
 * "explosion" is an emergent coordination failure. By requiring the declaration
 * of beneficiaries (coordination function), victims (asymmetric extraction), and
 * active enforcement, the system correctly identifies the hybrid nature of the
 * constraint, preventing a misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_interaction_explosion,
    'Is the interaction explosion an accidental system property (Mountain) or a designed friction (Snare)?',
    'Computational simulation of rule-intersections using automated "stress-test" agents.',
    'If accidental: Mountain of Complexity. If designed: Snare of Bureaucracy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_interaction_explosion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensifies over time as more rules are added, causing the
% combinatorial explosion to worsen. Initial extraction is low, but grows non-linearly.
% Theater ratio also creeps up as managing the rules becomes an end in itself.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cie_tr_t0, constraint_interaction_explosion, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cie_tr_t5, constraint_interaction_explosion, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cie_tr_t10, constraint_interaction_explosion, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cie_ex_t0, constraint_interaction_explosion, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cie_ex_t5, constraint_interaction_explosion, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(cie_ex_t10, constraint_interaction_explosion, base_extractiveness, 10, 0.81).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint is an emergent property of multiple interacting enforcement mechanisms.
narrative_ontology:coordination_type(constraint_interaction_explosion, enforcement_mechanism).

% The combinatorial complexity trap directly harms the efficiency of any system it governs.
narrative_ontology:affects_constraint(constraint_interaction_explosion, operational_efficiency).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */