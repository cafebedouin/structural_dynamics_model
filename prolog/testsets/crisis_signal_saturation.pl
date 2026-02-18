% ============================================================================
% CONSTRAINT STORY: crisis_signal_saturation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_crisis_signal_saturation, []).

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
 * * constraint_id: crisis_signal_saturation
 * human_readable: The Perpetual Alarm Fatigue
 * domain: informational/psychological/sociological
 * * SUMMARY:
 * A scenario where the "Rope" of real-time global risk monitoring and alert
 * systems (emergency broadcasts, market volatility pings, pandemic trackers)
 * reaches such high frequency and intensity that the subject's nervous system
 * habituates to "crisis" as the baseline. This coordination tool for rapid
 * response becomes a "Snare" for the subject, as their primary protective
 * agency—the ability to discern and respond to acute danger—is liquidated
 * through saturation-induced apathy, trapping them in a territory of
 * "frozen-paralysis" where they are incapable of acting even when a
 * catastrophic event is imminent.
 * * KEY AGENTS:
 * - Saturated Citizen: Subject (Powerless)
 * - Risk Management Aggregator: Beneficiary (Institutional)
 * - Neurological Load Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) reflects the liquidation of the subject's
% cognitive and emotional reserves to feed the aggregator's data-velocity metrics.
domain_priors:base_extractiveness(crisis_signal_saturation, 0.89).
domain_priors:suppression_score(crisis_signal_saturation, 0.82). % "Low-noise" life is suppressed by mandatory safety and economic connectivity.
domain_priors:theater_ratio(crisis_signal_saturation, 0.93).    % Extreme theater: "Urgency Rating" systems that performatively signal care while increasing the noise.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(crisis_signal_saturation, extractiveness, 0.89).
narrative_ontology:constraint_metric(crisis_signal_saturation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(crisis_signal_saturation, theater_ratio, 0.93).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(crisis_signal_saturation, piton).
narrative_ontology:human_readable(crisis_signal_saturation, "The Perpetual Alarm Fatigue").
narrative_ontology:topic_domain(crisis_signal_saturation, "informational/psychological/sociological").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(crisis_signal_saturation).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, risk_management_aggregator).
narrative_ontology:constraint_victim(crisis_signal_saturation, saturated_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they cannot disconnect without missing vital info,
% but staying connected liquidates their capacity for meaningful response.
constraint_indexing:constraint_classification(crisis_signal_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The aggregator views the flood as a Rope—the essential coordination
% substrate for maintaining a "Perfectly Aware" and "Responsive" global state.
constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.93) > 0.70 triggers Piton: the "Crisis Management Dashboard"
% is an inertial spike; it signals safety while the subject's agency is atrophied.
constraint_indexing:constraint_classification(crisis_signal_saturation, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) masking as essential coordination (Rope),
% supported by enforcement and asymmetric benefit/harm.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crisis_saturation_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional aggregator.
    constraint_indexing:constraint_classification(crisis_signal_saturation, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.93) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(crisis_signal_saturation, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_detection) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(crisis_saturation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a state where the "coordination"
 * benefit of total awareness is achieved by liquidating the subject's primary
 * capacity for emergency agency. The suppression score (0.82) is high because
 * opting out of the information firehose is socially and economically untenable.
 * The extreme theater ratio (0.93) points to the performative nature of the
 * alert systems, which prioritize signaling urgency over enabling effective action.
 *
 * PERSPECTIVAL GAP:
 * The Saturated Citizen feels a Snare because they are in a state of
 * permanent physiological alarm that yields zero productive output.
 * The Aggregator sees a Rope because the total data-capture coordinates
 * a perfectly legible global risk-market.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system avoids misclassifying this as a pure Snare by recognizing its
 * origins as a coordination mechanism. The Tangled Rope classification correctly
 * identifies the hybrid nature: a system with a genuine (but now degraded)
 * coordination function that has become overwhelmingly extractive. The Piton
 * classification from another analytical viewpoint further resolves this by
 * highlighting the system's functional atrophy and inertial, theatrical existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_neurological_desensitization,
    'Can "Slow-Info" protocols restore the Rope, or is saturation a biological "Mountain" (Snare vs Mountain)?',
    'Tracking the response-velocity to "Level 1" alerts in cities with high vs low notification density in 2026.',
    'If response returns: Snare of current design. If apathy persists: Mountain of Biological Habituation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(crisis_signal_saturation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a low-extraction coordination tool (a Rope) and
% degraded over time into a high-extraction, high-theater state as the volume
% of signals overwhelmed human capacity.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(css_tr_t0, crisis_signal_saturation, theater_ratio, 0, 0.20).
narrative_ontology:measurement(css_tr_t5, crisis_signal_saturation, theater_ratio, 5, 0.65).
narrative_ontology:measurement(css_tr_t10, crisis_signal_saturation, theater_ratio, 10, 0.93).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(css_ex_t0, crisis_signal_saturation, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(css_ex_t5, crisis_signal_saturation, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(css_ex_t10, crisis_signal_saturation, base_extractiveness, 10, 0.89).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system is a form of global infrastructure for disseminating information.
narrative_ontology:coordination_type(crisis_signal_saturation, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */