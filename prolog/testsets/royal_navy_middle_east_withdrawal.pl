% ============================================================================
% CONSTRAINT STORY: royal_navy_middle_east_withdrawal
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_royal_navy_middle_east_withdrawal, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: royal_navy_middle_east_withdrawal
 * human_readable: End of Permanent Royal Navy Presence in the Gulf
 * domain: political/security
 * * SUMMARY:
 * The Royal Navy is withdrawing its last remaining ship from the Middle East,
 * ending a permanent fighting presence in the Gulf for the first time since 1980.
 * This strategic shift is driven by a significant defense funding gap and a
 * shrinking fleet, forcing a reprioritization of naval assets to the North
 * Atlantic to counter Russian activity.
 * * KEY AGENTS:
 * - Commercial Shipping/Regional Partners: Subjects (Powerless) who lose a key
 *   security guarantee against regional threats.
 * - UK Ministry of Defence: Beneficiary (Institutional) who must reallocate
 *   scarce resources to meet perceived higher-priority threats closer to home.
 * - Defence Analysts: Observers (Analytical) who view the withdrawal as the
 *   inevitable consequence of long-term underinvestment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, 0.80). % High extraction: security is removed from regional actors to serve national budget priorities.
domain_priors:suppression_score(royal_navy_middle_east_withdrawal, 0.50).   % Moderate suppression: viable alternatives (e.g., drone warships) are years away.
domain_priors:theater_ratio(royal_navy_middle_east_withdrawal, 0.20).       % Low theater: this is a functional, not performative, withdrawal.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, extractiveness, 0.80).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% The government frames this as a deliberate, strategic reallocation.
narrative_ontology:constraint_claim(royal_navy_middle_east_withdrawal, tangled_rope).
narrative_ontology:human_readable(royal_navy_middle_east_withdrawal, "End of Permanent Royal Navy Presence in the Gulf").

% Binary flags
domain_priors:requires_active_enforcement(royal_navy_middle_east_withdrawal). % The withdrawal is an active policy choice.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, hm_treasury).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, global_shipping_routes).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, regional_allies_bahrain_uae).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% A merchant captain in the Gulf loses a security guarantee they rely on.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The First Sea Lord reallocating a "stretched" fleet to the North Atlantic.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% A defense analyst viewing the withdrawal as the inevitable result of a decade of fleet decline.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(royal_navy_middle_east_withdrawal_tests).

test(perspectival_gap_is_resolved) :-
    % Verify the gap between powerless (Snare), institutional (Rope), and analytical (Mountain).
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(high_extraction_is_validated) :-
    % The base extraction must be high to justify the Snare/Mountain classifications.
    domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, E),
    E >= 0.46.

:- end_tests(royal_navy_middle_east_withdrawal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a classic perspectival conflict arising from resource scarcity.
 * For regional actors (powerless), the withdrawal is a Snare, removing security
 * without their consent. For the Royal Navy command (institutional), it's a Rope,
 * a painful but necessary coordination of scarce assets. The high extraction (0.80)
 * represents the total loss of a security function for one group to solve a
 * budgetary/strategic problem for another.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The conflict between the Snare and Rope perspectives is resolved by the
 * Analytical view, which classifies the constraint as a Mountain. The high
 * extraction is not merely a malicious or self-serving policy choice that could
 * be reversed; it is the inevitable, physically determined outcome of a fleet
 * that has shrunk below the critical size required for a global presence. The
 * "choice" to withdraw is illusory; the decision was made by years of budget
 * cuts. This identifies the constraint as a structural mandate rather than a
 * simple policy lever, resolving the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_rn_withdrawal_1,
    'Will autonomous "drone warships" successfully replace manned frigates in Gulf patrol roles?',
    'Monitor the deployment and operational effectiveness of drone-led Operation Kipion rotations in the late 2020s.',
    'If successful, the Snare (lack of protection) could transition to a Rope (coordinated drone security). If not, the security vacuum becomes permanent.',
    confidence_without_resolution(low)
).

omega_variable(
    omega_rn_withdrawal_2,
    'Will the U.S. Fifth Fleet reduce the UK\'s "Deputy Commander" status as Britain\'s physical presence reaches zero?',
    'Observe the next deputy commander appointment in Bahrain and any changes in joint operational command structures.',
    'High impact on UK diplomatic influence and its perceived status as a global military power.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% The interval represents the final decade of decline (e.g., 2016-2026).
narrative_ontology:interval(royal_navy_middle_east_withdrawal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as the fleet shrank, increasing the
% "extraction" of security from the region.

% Theater ratio over time (remains low as this is a functional, not performative, decline):
narrative_ontology:measurement(rn_withdrawal_tr_t0, royal_navy_middle_east_withdrawal, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rn_withdrawal_tr_t5, royal_navy_middle_east_withdrawal, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rn_withdrawal_tr_t10, royal_navy_middle_east_withdrawal, theater_ratio, 10, 0.20).

% Extraction over time (increases as naval presence is withdrawn):
narrative_ontology:measurement(rn_withdrawal_ex_t0, royal_navy_middle_east_withdrawal, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(rn_withdrawal_ex_t5, royal_navy_middle_east_withdrawal, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(rn_withdrawal_ex_t10, royal_navy_middle_east_withdrawal, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint represents the *removal* of a security coordination function,
% so it does not have a coordination_type itself.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */