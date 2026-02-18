% ============================================================================
% CONSTRAINT STORY: consensus_without_truth
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_consensus_without_truth, []).

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
 * * constraint_id: consensus_without_truth
 * human_readable: The Social Cohesion Mirage
 * domain: social/political/informational
 * * SUMMARY:
 * A scenario where a community achieves total agreement on a shared narrative
 * that is factually false or decoupled from physical reality. This "Rope" for
 * social coordination provides intense internal stability but acts as a "Snare"
 * for the individual, whose survival agency is liquidated as the group's
 * collective actions fail to map to the actual territory of reality,
 * leading to systemic fragility.
 * * KEY AGENTS:
 * - Dissenting Individual: Subject (Powerless)
 * - Narrative Enforcer: Beneficiary (Institutional)
 * - External Reality Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of the subject's
% epistemic agency to maintain the group's performative "unity."
domain_priors:base_extractiveness(consensus_without_truth, 0.88).
domain_priors:suppression_score(consensus_without_truth, 0.82).
domain_priors:theater_ratio(consensus_without_truth, 0.65). % High, but below Piton threshold. The coordination is still functional for the in-group, just highly performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(consensus_without_truth, extractiveness, 0.88).
narrative_ontology:constraint_metric(consensus_without_truth, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(consensus_without_truth, theater_ratio, 0.65).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(consensus_without_truth, tangled_rope).
narrative_ontology:human_readable(consensus_without_truth, "The Social Cohesion Mirage").
narrative_ontology:topic_domain(consensus_without_truth, "social/political/informational").

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(consensus_without_truth).
narrative_ontology:constraint_beneficiary(consensus_without_truth, narrative_enforcers).
narrative_ontology:constraint_victim(consensus_without_truth, dissenting_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: expressing the truth results in social death
% or expulsion, but adhering to the consensus results in physical/economic ruin.
constraint_indexing:constraint_classification(consensus_without_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The enforcer views the consensus as a Rope—the ultimate coordination tool
% for mobilizing mass action and ensuring institutional continuity.
constraint_indexing:constraint_classification(consensus_without_truth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a coordination function for beneficiaries,
% funded by high asymmetric extraction from victims, requiring active enforcement.
constraint_indexing:constraint_classification(consensus_without_truth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_without_truth_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(consensus_without_truth, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_without_truth, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(consensus_without_truth, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_properties, [nondet]) :-
    % A valid Tangled Rope requires enforcement, a beneficiary, and a victim.
    domain_priors:requires_active_enforcement(consensus_without_truth),
    narrative_ontology:constraint_beneficiary(consensus_without_truth, _),
    narrative_ontology:constraint_victim(consensus_without_truth, _).

test(extraction_threshold) :-
    % Ensures the constraint meets the high-extraction criteria for v3.4 analysis.
    domain_priors:base_extractiveness(consensus_without_truth, E),
    E >= 0.46.

:- end_tests(consensus_without_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * coordination benefit of social cohesion is achieved by the parasitic
 * liquidation of the individual's sensory and logical faculty. The theater
 * ratio is high (0.65) but kept below the Piton threshold (0.70) to reflect
 * that the coordination function, while highly performative, is still
 * effective for the in-group that benefits from it.
 *
 * * PERSPECTIVAL GAP:
 * The Dissenting Individual feels a Snare because they are forced to
 * participate in a delusion to survive. The Enforcer sees a Rope
 * because the consensus is the only coordination signal strong enough
 * to keep the group from splintering. The Analytical Observer sees a
 * Tangled Rope, recognizing both the coordination and the severe,
 * asymmetrically applied extraction.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. The system avoids misclassifying
 * the constraint as a pure Snare (ignoring its coordination function for the
 * in-group) or a pure Rope (ignoring its immense extraction from dissenters).
 * It correctly identifies the hybrid nature: a coordination mechanism for
 * beneficiaries funded by asymmetric extraction from victims, held together by
 * active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_reality_collision,
    'When the consensus fails to feed the group, does the Rope break or do they starve in unison (Snare vs Mountain)?',
    'Tracking the survival rate of insular narrative communities against resource-scarcity events.',
    'If collapse: Snare of current belief. If adaptation: Rope of Narrative resilience.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(consensus_without_truth, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint degraded over time, starting as a flawed coordination
% mechanism and intensifying into a highly extractive one.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cwt_tr_t0, consensus_without_truth, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cwt_tr_t5, consensus_without_truth, theater_ratio, 5, 0.45).
narrative_ontology:measurement(cwt_tr_t10, consensus_without_truth, theater_ratio, 10, 0.65).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cwt_ex_t0, consensus_without_truth, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cwt_ex_t5, consensus_without_truth, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(cwt_ex_t10, consensus_without_truth, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The shared narrative acts as an information standard for the group.
narrative_ontology:coordination_type(consensus_without_truth, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */