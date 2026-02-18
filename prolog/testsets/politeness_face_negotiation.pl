% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: politeness_face_negotiation
 * human_readable: The Face Negotiation Constraint (Politeness Theory)
 * domain: social
 * * SUMMARY:
 * Politeness is a universal social constraint where individuals manage a "face"—a social self-image
 * categorized into "Positive Face" (desire for acceptance) and "Negative Face"
 * (desire for autonomy). Social interactions are a constant, often coercive, negotiation of these faces,
 * coordinating social harmony at the cost of individual expression and autonomy.
 * * KEY AGENTS:
 * - The Marginalized Individual: Subject (Powerless), experiences politeness as an oppressive snare.
 * - The Diplomat: Beneficiary (Institutional), uses politeness as a critical tool for statecraft.
 * - The Sociologist: Auditor (Analytical), observes the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(politeness_face_negotiation, 0.65). % Snare extraction >= 0.46
domain_priors:suppression_score(politeness_face_negotiation, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(politeness_face_negotiation, 0.20).       % Low theater; the function is real, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(politeness_face_negotiation, extractiveness, 0.65).
narrative_ontology:constraint_metric(politeness_face_negotiation, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(politeness_face_negotiation, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Politeness presents itself as a pure coordination mechanism for social harmony.
narrative_ontology:constraint_claim(politeness_face_negotiation, tangled_rope).
narrative_ontology:human_readable(politeness_face_negotiation, "The Face Negotiation Constraint (Politeness Theory)").
narrative_ontology:topic_domain(politeness_face_negotiation, "social").

% Binary flags
domain_priors:requires_active_enforcement(politeness_face_negotiation). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, social_groups_enforcing_harmony).
narrative_ontology:constraint_victim(politeness_face_negotiation, individual_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE MARGINALIZED INDIVIDUAL (SNARE)
% For those whose identity is misread or suppressed, politeness is a Snare. The social
% requirement to "not impose" is used to silence their identity and enforce a harmful
% status quo. The extraction of their self-image is immediate and coercive.
constraint_indexing:constraint_classification(politeness_face_negotiation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE DIPLOMAT (ROPE)
% For a diplomat, politeness is a critical Rope. It is the primary tool for
% managing international relations, avoiding conflict, and building coalitions.
% Face-saving is the essence of statecraft, and the extraction is negligible.
constraint_indexing:constraint_classification(politeness_face_negotiation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The sociologist sees both functions. Politeness is a genuine coordination mechanism
% (beneficiary exists) but also imposes asymmetric costs (victim exists) and requires
% active social enforcement. The high extraction and suppression scores confirm this.
constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(politeness_face_negotiation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extraction meets the threshold for a Snare/Tangled Rope.
    domain_priors:base_extractiveness(politeness_face_negotiation, E),
    E >= 0.46.

:- end_tests(politeness_face_negotiation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file incorrectly classified the analytical perspective as a Mountain. This is inconsistent
 * with the high base extractiveness (0.65), which far exceeds the Mountain threshold of 0.15. The correct
 * analytical classification is Tangled Rope, as the constraint possesses all three required structural
 * properties: a coordination function (beneficiary), asymmetric extraction (victim), and active enforcement.
 *
 * The perspectival gap is stark: for the institutional agent (diplomat), politeness is a pure coordination
 * Rope with low perceived extraction (χ = 0.65 * π(institutional) * σ(global) = 0.65 * -0.2 * 1.2 = -0.156, felt as a benefit).
 * For the powerless agent, it is a Snare with amplified extraction (χ = 0.65 * π(powerless) * σ(local) = 0.65 * 1.5 * 0.8 = 0.78).
 * This file has been corrected to reflect the Tangled Rope classification and add the required temporal data for
 * high-extraction constraints.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification resolves the mandatrophy by acknowledging both the
 * legitimate coordination function (preventing misclassification as a pure Snare) and the significant,
 * asymmetric extraction (preventing misclassification as a pure Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Is 'face' a functional biological necessity or a culturally constructed snare?
omega_variable(
    omega_politeness_face_negotiation,
    'Is face-negotiation a biological necessity for social survival (Mountain) or a culturally constructed tool for enforcing hierarchy (Snare)?',
    'Cross-cultural audit of social/economic outcomes for face-ignoring vs. face-observing individuals.',
    'If biological, it is a Mountain. If cultural, it is a Tangled Rope or Snare depending on the power index.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(politeness_face_negotiation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (E > 0.46).
% Models a slight intensification of the constraint over the interval.

% Theater ratio over time:
narrative_ontology:measurement(pfn_tr_t0, politeness_face_negotiation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pfn_tr_t5, politeness_face_negotiation, theater_ratio, 5, 0.18).
narrative_ontology:measurement(pfn_tr_t10, politeness_face_negotiation, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pfn_ex_t0, politeness_face_negotiation, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(pfn_ex_t5, politeness_face_negotiation, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(pfn_ex_t10, politeness_face_negotiation, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Social pressure and exclusion are the primary enforcement tools.
narrative_ontology:coordination_type(politeness_face_negotiation, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */