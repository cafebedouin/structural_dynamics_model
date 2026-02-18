% ============================================================================
% CONSTRAINT STORY: institutional_inertia_lock
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_institutional_inertia_lock, []).

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
 * * constraint_id: institutional_inertia_lock
 * human_readable: The Sunk-Cost Regulatory Trap
 * domain: political/technological
 * * SUMMARY:
 * This constraint occurs when a legacy regulatory framework remains in place
 * because the cost of institutional reorganization exceeds the perceived
 * friction of the current inefficiency. It functions as a Tangled Rope that is
 * actively decaying: it still provides a coordination function for incumbents
 * and extracts value from new entrants, but its high theater ratio indicates
 * its original purpose has atrophied.
 * * KEY AGENTS:
 * - New Entrant Firms: Subject (Powerless)
 * - Incumbent Bureaucracy: Beneficiary (Institutional)
 * - Efficiency Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction (0.52) is high, indicating significant value transfer.
% Suppression (0.45) is moderate, representing procedural barriers.
% Theater ratio (0.82) is very high, indicating the system is more performative
% than functional, a sign of severe institutional decay.
domain_priors:base_extractiveness(institutional_inertia_lock, 0.52).
domain_priors:suppression_score(institutional_inertia_lock, 0.45).
domain_priors:theater_ratio(institutional_inertia_lock, 0.82).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(institutional_inertia_lock, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_inertia_lock, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(institutional_inertia_lock, theater_ratio, 0.82).

% Constraint self-claim: The bureaucracy claims it's a necessary coordination mechanism.
narrative_ontology:constraint_claim(institutional_inertia_lock, tangled_rope).
narrative_ontology:human_readable(institutional_inertia_lock, "The Sunk-Cost Regulatory Trap").
narrative_ontology:topic_domain(institutional_inertia_lock, "political/technological").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(institutional_inertia_lock).
narrative_ontology:constraint_beneficiary(institutional_inertia_lock, incumbent_bureaucracy).
narrative_ontology:constraint_victim(institutional_inertia_lock, new_entrant_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To a new entrant, the inertia is a snare that prevents market entry and extracts a 'time-tax'.
% χ = 0.52 * 1.5 (powerless) * 1.0 (national) = 0.78. This is a clear Snare.
constraint_indexing:constraint_classification(institutional_inertia_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution perceives the inertia as a 'Rope' of stability and coordination.
% χ = 0.52 * -0.2 (institutional) * 1.2 (global) = -0.12. The institution perceives a net benefit.
constraint_indexing:constraint_classification(institutional_inertia_lock, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees both the coordination function (beneficiary exists)
% and the asymmetric extraction (victim exists), classifying it as a Tangled Rope.
% The high theater ratio is a property of this decaying Tangled Rope.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_inertia_lock_tests).

test(perspectival_gap) :-
    % Verify Snare for powerless vs Rope for institutional.
    constraint_indexing:constraint_classification(institutional_inertia_lock, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_lock, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that the necessary structural facts for a Tangled Rope are present.
    domain_priors:requires_active_enforcement(institutional_inertia_lock),
    narrative_ontology:constraint_beneficiary(institutional_inertia_lock, _),
    narrative_ontology:constraint_victim(institutional_inertia_lock, _).

test(temporal_data_exists_for_high_extraction) :-
    % High extraction (>0.46) requires temporal data.
    narrative_ontology:constraint_metric(institutional_inertia_lock, extractiveness, E),
    E > 0.46,
    findall(T, narrative_ontology:measurement(_, institutional_inertia_lock, _, T, _), Times),
    length(Times, Count),
    Count >= 6.

:- end_tests(institutional_inertia_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a common form of institutional decay. It is not a Piton
 * because, despite its extremely high theater ratio (0.82), it is still highly
 * extractive (0.52). A true Piton is inert and has low extraction (<= 0.15).
 * Instead, this is a Tangled Rope in a late stage of decay, where performative
 * action has almost entirely replaced its original function, but its structure
 * continues to enable significant rent-seeking.
 *
 * The temporal data models this decay: it began as a low-extraction, functional
 * system (a Rope), but over time, extraction and theater both increased as
 * its purpose drifted and incumbents captured the mechanism.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification correctly identifies the hybrid nature of the
 * constraint. A simpler model might misclassify it as a Snare (ignoring the
 * coordination function for incumbents) or a Piton (ignoring the high, active
 * extraction). The combination of beneficiary/victim facts and temporal data
 * provides a rich picture of its lifecycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.52) > 0.46.
omega_variable(
    omega_inertia_source,
    'Is the lock caused by irreducible procedural complexity (Mountain) or active rent-seeking by incumbents (Snare component)?',
    'An external audit deploying automated process analysis to map and quantify sources of delay and friction.',
    'If friction is irreducible: Mountain. If friction is concentrated at discretionary gates controlled by incumbents: Snare component is dominant.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_inertia_lock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint started as a functional coordination mechanism (low extraction,
% low theater) and decayed over the interval into an extractive, performative trap.
% This progression is required for drift detection as base_extractiveness > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(iil_tr_t0, institutional_inertia_lock, theater_ratio, 0, 0.10).
narrative_ontology:measurement(iil_tr_t5, institutional_inertia_lock, theater_ratio, 5, 0.50).
narrative_ontology:measurement(iil_tr_t10, institutional_inertia_lock, theater_ratio, 10, 0.82).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(iil_ex_t0, institutional_inertia_lock, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(iil_ex_t5, institutional_inertia_lock, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(iil_ex_t10, institutional_inertia_lock, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The regulatory framework acts as a gatekeeping/enforcement mechanism.
narrative_ontology:coordination_type(institutional_inertia_lock, enforcement_mechanism).

% Network relationships: This type of regulatory lock often affects technology adoption rates.
% narrative_ontology:affects_constraint(institutional_inertia_lock, technology_adoption_rate).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */