% ============================================================================
% CONSTRAINT STORY: evolutionary_mismatch_load
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_evolutionary_mismatch_load, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: evolutionary_mismatch_load
 * human_readable: The Paleolithic Circuit Break
 * domain: biological/technological/social
 * * SUMMARY:
 * This constraint represents the friction caused by human biological traits
 * that were adaptive in an ancestral environment but have become maladaptive
 * in a modern, hyper-mediated technological landscape. This mismatch functions
 * as a Snare for the individual, whose biology is "hijacked" by novel stimuli,
 * while serving as a Rope for institutions that coordinate behavior by
 * optimizing for these deep-seated evolutionary drives.
 * * KEY AGENTS:
 * - Modern Human: Subject (Powerless)
 * - Algorithmic Curator: Beneficiary (Institutional)
 * - Evolutionary Psychologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the mismatch siphons biological health, attention,
% and cognitive agency to maintain high-frequency engagement loops.
domain_priors:base_extractiveness(evolutionary_mismatch_load, 0.83).
domain_priors:suppression_score(evolutionary_mismatch_load, 0.70).
domain_priors:theater_ratio(evolutionary_mismatch_load, 0.35). % Low theater; the drives are raw and functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(evolutionary_mismatch_load, extractiveness, 0.83).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, theater_ratio, 0.35).

% Constraint self-claim: The platforms exploiting the mismatch claim to be
% providing a coordination service (connecting people, curating content).
narrative_ontology:constraint_claim(evolutionary_mismatch_load, tangled_rope).

% Binary flags
% The algorithmic systems require constant tuning and updates to maintain their
% extractive efficiency, which constitutes active enforcement.
domain_priors:requires_active_enforcement(evolutionary_mismatch_load).

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, algorithmic_curators).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, modern_humans).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped in their own biology; they cannot "opt out" of
% how their dopamine or cortisol circuits respond to modern stimuli.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Platforms view these biological "hooks" as a Rope—a way to coordinate
% billions of agents into predictable, high-fidelity engagement patterns.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the coordination function (beneficiary)
% and the severe asymmetric extraction (victim), classifying it as a Tangled Rope.
% The high extraction (0.83) and suppression (0.70) confirm this.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_mismatch_load_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Verify that the high extraction score aligns with a high-extraction classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(evolutionary_mismatch_load, ExtMetricName, E),
    E >= 0.46.

:- end_tests(evolutionary_mismatch_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the
 * species' biological foundations (attention, focus, well-being) are being
 * liquidated to drive digital engagement metrics. The suppression score (0.70)
 * represents the difficulty of escaping these systems, which are integrated
 * into modern social and professional life.
 *
 * * PERSPECTIVAL GAP:
 * The Modern Human feels a Snare because their instincts—once essential for
 * survival—now lead them toward health and focus depletion. The
 * Algorithmic Curator sees a Rope because these instincts provide the
 * most reliable coordination signal available for monetizing attention.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification resolves the Mandatrophy
 * by acknowledging both sides of the perspectival gap. It is not a pure Snare,
 * as there is a genuine (if parasitic) coordination function. It is not a Rope,
 * as the extraction is severe and asymmetric. The classification correctly
 * identifies a system where coordination is built upon the "tangle" of
 * ancestral drives and modern extractive technology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cognitive_evolution,
    'Can neuroplasticity or cultural evolution mitigate the load (Snare vs Mountain)?',
    'Longitudinal studies of "digital native" cohorts compared to legacy cohorts.',
    'If load decreases: Snare of policy/tech. If load remains: Mountain of biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(evolutionary_mismatch_load, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The exploitation of evolutionary mismatch has intensified dramatically with
% the sophistication of algorithmic curation over the last two decades.
% Extraction started low and increased as platforms optimized for engagement.
%
% Theater ratio over time:
narrative_ontology:measurement(eml_tr_t0, evolutionary_mismatch_load, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eml_tr_t5, evolutionary_mismatch_load, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eml_tr_t10, evolutionary_mismatch_load, theater_ratio, 10, 0.35).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eml_ex_t0, evolutionary_mismatch_load, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(eml_ex_t5, evolutionary_mismatch_load, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(eml_ex_t10, evolutionary_mismatch_load, base_extractiveness, 10, 0.83).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint operates as a form of global infrastructure for attention allocation.
narrative_ontology:coordination_type(evolutionary_mismatch_load, global_infrastructure).

% narrative_ontology:affects_constraint(evolutionary_mismatch_load, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */