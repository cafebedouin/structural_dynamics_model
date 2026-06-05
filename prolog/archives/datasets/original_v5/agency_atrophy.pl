% ============================================================================
% CONSTRAINT STORY: agency_atrophy
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(agency_atrophy, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: agency_atrophy
 * human_readable: The Outsourced Cognition Trap
 * domain: technological/cognitive/psychological
 * * SUMMARY:
 * A scenario where the "Rope" of automated decision-support systems (AI assistants, 
 * predictive navigation, automated scheduling) becomes so efficient that the 
 * subject ceases to exercise the underlying cognitive skill. This coordination 
 * tool for personal productivity becomes a "Snare" as the subject's primary 
 * problem-solving and navigational agency is liquidated through disuse, 
 * trapping them in a terminal dependency where they are no longer capable of 
 * functioning without the substrate.
 * * KEY AGENTS:
 * - Dependent User: Subject (Powerless)
 * - Automation Provider: Beneficiary (Institutional)
 * - Cognitive Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the near-total siphoning of the subject's 
% cognitive surplus into the maintenance of a substrate-dependent existence.
domain_priors:base_extractiveness(agency_atrophy, 0.88). 
domain_priors:suppression_score(agency_atrophy, 0.79). % Manual alternatives are suppressed by the extreme "friction" of non-automated life.
domain_priors:theater_ratio(agency_atrophy, 0.92).    % Extreme theater: "Personal Growth" apps that performatively signal agency while 0.88 extraction occurs.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(agency_atrophy, extractiveness, 0.88).
narrative_ontology:constraint_metric(agency_atrophy, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(agency_atrophy, theater_ratio, 0.92).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(agency_atrophy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: the "Rope" of convenience has liquidates their 
% neurological capacity for the skill, making exit physically impossible.
constraint_indexing:constraint_classification(agency_atrophy, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The provider views the automation as a Rope—the essential coordination 
% substrate for achieving a high-efficiency, friction-free global society.
constraint_indexing:constraint_classification(agency_atrophy, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Brain Training" 
% module is an inertial spike; it performatively signals cognitive health 
% while the underlying agency has already atrophied.
constraint_indexing:constraint_classification(agency_atrophy, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(agency_atrophy, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(agency_atrophy, E), E >= 0.50,
    domain_priors:suppression_score(agency_atrophy, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agency_atrophy_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional provider.
    constraint_indexing:constraint_classification(agency_atrophy, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agency_atrophy, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(agency_atrophy, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(agency_atrophy, E),
    E > 0.70.

:- end_tests(agency_atrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of automated life is achieved by liquidating the 
 * subject's primary neurological capacity for independent agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Dependent User feels a Snare because their survival is now tethered 
 * to a subscription they cannot mentally afford to cancel. The Provider 
 * sees a Rope because the automation coordinates a perfectly predictable 
 * and legible global labor and consumption market.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Self-Actualization" feature is no longer functional (Theater 0.92); 
 * it is an inert spike siphoning 0.88 of the species' cognitive surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_neuroplastic_reversibility,
    'Can "Cognitive Rehabilitation" restore the Rope, or is atrophy a biological "Mountain" (Snare vs Mountain)?',
    'Tracking the recovery velocity of spatial navigation skills in users after 12 months of "GPS-dark" living.',
    'If skills return: Snare of current lifestyle. If they fail: Mountain of Biological Critical Periods.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(agency_atrophy, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional decision-support (0.35) to 
% inertial "Agency Theater" (0.92) as the skill atrophies.
narrative_ontology:measurement(aa_tr_t0, agency_atrophy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aa_tr_t5, agency_atrophy, theater_ratio, 5, 0.68).
narrative_ontology:measurement(aa_tr_t10, agency_atrophy, theater_ratio, 10, 0.92).

% Extraction: Progressive liquidation of neurological agency into 
% terminal substrate-dependency.
narrative_ontology:measurement(aa_ex_t0, agency_atrophy, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(aa_ex_t5, agency_atrophy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(aa_ex_t10, agency_atrophy, base_extractiveness, 10, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
