% ============================================================================
% CONSTRAINT STORY: norm_erosion_threshold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_norm_erosion_threshold, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: norm_erosion_threshold
 * human_readable: The Social Fabric Breach
 * domain: social/political
 * * SUMMARY:
 * This constraint models the non-linear breakdown of social cooperation when
 * the perceived rate of "rule-breaking" or "defection" crosses a critical
 * threshold. While high-trust norms act as a Rope for seamless coordination,
 * the erosion process creates a Snare where individuals are forced into
 * defensive, low-trust behaviors to avoid exploitation, liquidating
 * collective social capital.
 * * KEY AGENTS:
 * - High-Trust Participant: Subject (Powerless)
 * - Norm Arbitrageur / Defector: Beneficiary (Institutional)
 * - Cultural Sociologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) as the collapse of trust siphons the surplus of
% honest cooperation into the "security costs" and "arbitrage gains" of
% a low-trust environment.
domain_priors:base_extractiveness(norm_erosion_threshold, 0.85).
domain_priors:suppression_score(norm_erosion_threshold, 0.70).
domain_priors:theater_ratio(norm_erosion_threshold, 0.78). % High theater: performative adherence to dead norms.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(norm_erosion_threshold, extractiveness, 0.85).
narrative_ontology:constraint_metric(norm_erosion_threshold, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(norm_erosion_threshold, theater_ratio, 0.78).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be the original coordination norm, but its function has degraded.
narrative_ontology:constraint_claim(norm_erosion_threshold, tangled_rope).
narrative_ontology:human_readable(norm_erosion_threshold, "The Social Fabric Breach").

% Binary flags
domain_priors:requires_active_enforcement(norm_erosion_threshold). % Required for Tangled Rope

% Structural property derivation hooks:
% These facts are required for the Tangled Rope classification.
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, norm_arbitrageurs).
narrative_ontology:constraint_victim(norm_erosion_threshold, high_trust_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the honest participant, the erosion is a snare: they are trapped between
% being exploited by defectors or abandoning their values to survive.
constraint_indexing:constraint_classification(norm_erosion_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The arbitrageur views the remaining trust as a Rope—it is the coordination
% substrate they exploit to extract maximum private value before the crash.
constraint_indexing:constraint_classification(norm_erosion_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view detects the hybrid nature: a coordination
% function (beneficiary exists) with asymmetric extraction (victim exists)
% that requires active enforcement to maintain its degraded state.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% An alternative analytical view focusing on function vs performance.
% Theater ratio (0.78) > 0.70 triggers Piton: the outward symbols of the
% norm remain, but they no longer coordinate actual behavior.
constraint_indexing:constraint_classification(norm_erosion_threshold, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(norm_erosion_threshold_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional defector.
    constraint_indexing:constraint_classification(norm_erosion_threshold, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.78) triggers Piton classification.
    domain_priors:theater_ratio(norm_erosion_threshold, TR), TR > 0.7,
    constraint_indexing:constraint_classification(norm_erosion_threshold, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    % Ensure extraction and suppression meet Tangled Rope thresholds.
    domain_priors:base_extractiveness(norm_erosion_threshold, E), E >= 0.50,
    domain_priors:suppression_score(norm_erosion_threshold, S), S >= 0.40.

:- end_tests(norm_erosion_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the
 * social "coordination" is actually a parasitic liquidation of trust. The
 * suppression score (0.70) represents the high cost of either adhering to the
 * norm (and being exploited) or defecting (and facing social sanction).
 * The Tangled Rope classification is justified by the presence of both
 * beneficiaries (norm_arbitrageurs) and victims (high_trust_participants),
 * alongside the need for active enforcement to prevent total collapse.
 *
 * * PERSPECTIVAL GAP:
 * The High-Trust Participant feels a Snare because their cooperation is
 * turned against them. The Norm Arbitrageur sees a Rope because the
 * existing trust facilitates their extractive transactions. The Analytical
 * observer sees the full picture: a Tangled Rope.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the norm is no longer functional relative to its social purpose
 * (Theater 0.78 -> Piton); it is an inert spike that also functions as a
 * Tangled Rope, siphoning 0.85 of the collective surplus into private hands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_norm_recovery,
    'Can trust be restored through enforcement, or is erosion permanent (Snare vs Mountain)?',
    'Tracking the delta between enforcement cost and norm compliance over a decade.',
    'If trust returns: Snare of current policy. If trust remains low: Mountain of Social Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(norm_erosion_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of the social norm over time.
% Initially, it was a low-extraction, low-theater Rope. Over the interval,
% extraction and performative theater both increased as trust was liquidated.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(net_tr_t0, norm_erosion_threshold, theater_ratio, 0, 0.15).
narrative_ontology:measurement(net_tr_t5, norm_erosion_threshold, theater_ratio, 5, 0.55).
narrative_ontology:measurement(net_tr_t10, norm_erosion_threshold, theater_ratio, 10, 0.78).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(net_ex_t0, norm_erosion_threshold, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(net_ex_t5, norm_erosion_threshold, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(net_ex_t10, norm_erosion_threshold, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Social norms are a form of shared information standard for behavior.
narrative_ontology:coordination_type(norm_erosion_threshold, information_standard).

% The erosion of social norms has a direct structural impact on the stability
% of political systems that rely on those norms.
narrative_ontology:affects_constraint(norm_erosion_threshold, political_stability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */