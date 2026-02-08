% ============================================================================
% CONSTRAINT STORY: protocol_capture_eee
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_protocol_capture_eee, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: protocol_capture_eee
 * human_readable: The Captured Commons (Embrace, Extend, Extinguish)
 * domain: technological/economic
 * * SUMMARY:
 * A scenario where a dominant entity embraces a decentralized protocol, 
 * extends it with proprietary "features" that create lock-in, and effectively 
 * extinguishes the original open alternative. 
 * It is a Tangled Rope: mandatory for survival but predatory in nature.
 * * KEY AGENTS:
 * - The Developer: Subject (Powerless) - Dependent on the proprietary extensions.
 * - The Platform Giant: Beneficiary (Institutional) - The entity performing the capture.
 * - The Antitrust Auditor: Auditor (Analytical) - Observing the extraction signature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(protocol_capture_eee, 0.55). % High: Surplus value captured via proprietary extensions.
domain_priors:suppression_score(protocol_capture_eee, 0.85).   % Very High: Network effects make exit nearly impossible.
domain_priors:theater_ratio(protocol_capture_eee, 0.40).       % Moderate: Claims of "openness" are increasingly theatrical.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(protocol_capture_eee, extractiveness, 0.55).
narrative_ontology:constraint_metric(protocol_capture_eee, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(protocol_capture_eee, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% The platform claims its proprietary extensions are necessary for coordination and stability.
narrative_ontology:constraint_claim(protocol_capture_eee, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(protocol_capture_eee). % Legal/Technical enforcement of EULA/API terms.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(protocol_capture_eee, platform_operators).
narrative_ontology:constraint_victim(protocol_capture_eee, third_party_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE CAPTIVE DEVELOPER (SNARE)
% To the developer, the "extensions" are a trap; they cannot revert to the open standard without losing critical data/access.
constraint_indexing:constraint_classification(protocol_capture_eee, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM SHAREHOLDER (ROPE)
% To the beneficiary, this is a Rope—it provides a stable, high-performance environment for commerce and shareholder value.
constraint_indexing:constraint_classification(protocol_capture_eee, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: The system provides genuine coordination (it works) but relies on extraction (lock-in).
constraint_indexing:constraint_classification(protocol_capture_eee, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eee_capture_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(protocol_capture_eee, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(protocol_capture_eee, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical perspective correctly identifies the Tangled Rope signature.
    constraint_indexing:constraint_classification(protocol_capture_eee, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(eee_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.55) and suppression (0.85) are high, reflecting a mature "Embrace, Extend, Extinguish" strategy.
 * The platform leverages network effects (high suppression) to enforce its proprietary extensions, which extract value from
 * the ecosystem (high extraction). This creates a stark perspectival gap: developers see a Snare (trapped by dependency),
 * while platform insiders/shareholders see a Rope (a successful business model coordinating a market).
 *
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the Tangled Rope classification. A naive analysis might see only the high extraction and
 * suppression and label it a pure Snare. However, this misses the genuine coordination function the platform provides.
 * The Tangled Rope correctly identifies that the system has both a real coordination benefit (derived from the beneficiary)
 * and asymmetric extraction (derived from the victim), and requires active enforcement to maintain the imbalance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_extinguish_point,
    'At what point do the proprietary extensions render the base protocol non-functional for outsiders?',
    'Measure of protocol fork viability vs. market share of proprietary clients.',
    'If low viability: Permanent Snare; If high viability: The Tangled Rope can be untangled.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(protocol_capture_eee, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the "Embrace, Extend, Extinguish" lifecycle.
% Initially, the platform embraces the open protocol with low extraction and low theater.
% Over time, it extends the protocol, increasing extraction and making its "openness" claims more theatrical.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(protocol_capture_eee_tr_t0, protocol_capture_eee, theater_ratio, 0, 0.15).
narrative_ontology:measurement(protocol_capture_eee_tr_t5, protocol_capture_eee, theater_ratio, 5, 0.30).
narrative_ontology:measurement(protocol_capture_eee_tr_t10, protocol_capture_eee, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(protocol_capture_eee_ex_t0, protocol_capture_eee, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(protocol_capture_eee_ex_t5, protocol_capture_eee, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(protocol_capture_eee_ex_t10, protocol_capture_eee, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The captured protocol now functions as a piece of global infrastructure.
narrative_ontology:coordination_type(protocol_capture_eee, global_infrastructure).

% Network relationships (structural influence edges)
% This form of capture creates dependency, a separate constraint.
narrative_ontology:affects_constraint(protocol_capture_eee, developer_dependency_lock_in).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */