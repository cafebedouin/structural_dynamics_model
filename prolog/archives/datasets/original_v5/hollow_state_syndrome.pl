% ============================================================================
% CONSTRAINT STORY: hollow_state_syndrome
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(hollow_state_syndrome, []).

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
 * * constraint_id: hollow_state_syndrome
 * human_readable: The Shell Governance Mirage
 * domain: political/organizational
 * * SUMMARY:
 * A scenario where a state or institution maintains the formal appearance and 
 * legal authority of governance while having outsourced its core functions to 
 * private contractors or non-state actors. This "Rope" for administrative 
 * flexibility becomes a "Snare" for citizens, as the lines of accountability 
 * vanish, liquidating the subject's ability to seek redress or public service.
 * * KEY AGENTS:
 * - Public Service Recipient: Subject (Powerless)
 * - Private Prime Contractor: Beneficiary (Institutional)
 * - Administrative Law Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) because the "hollowing" siphons public funds into 
% private margins while externalizing the "failure to deliver" onto the subject.
domain_priors:base_extractiveness(hollow_state_syndrome, 0.87). 
domain_priors:suppression_score(hollow_state_syndrome, 0.73). 
domain_priors:theater_ratio(hollow_state_syndrome, 0.94). % Extreme theater: public-facing rituals with no actual capability.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hollow_state_syndrome, extractiveness, 0.87).
narrative_ontology:constraint_metric(hollow_state_syndrome, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(hollow_state_syndrome, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a loop of bureaucratic denial; the state points 
% to the contractor, and the contractor points to the contract.
constraint_indexing:constraint_classification(hollow_state_syndrome, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The contractor views the arrangement as a Rope—a highly efficient way to 
% coordinate large-scale public tasks with private-sector speed and profit.
constraint_indexing:constraint_classification(hollow_state_syndrome, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "State Capability" is 
% a performative facade masking total operational atrophy.
constraint_indexing:constraint_classification(hollow_state_syndrome, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as coordination (Rope).
constraint_indexing:constraint_classification(hollow_state_syndrome, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(hollow_state_syndrome, E), E >= 0.50,
    domain_priors:suppression_score(hollow_state_syndrome, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollow_state_syndrome_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(hollow_state_syndrome, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollow_state_syndrome, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.94) triggers Piton classification.
    constraint_indexing:constraint_classification(hollow_state_syndrome, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.87) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(hollow_state_syndrome, E),

    E > 0.70.

:- end_tests(hollow_state_syndrome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" is a parasitic liquidation of state capacity for private gain.
 
 * * PERSPECTIVAL GAP:
 * The Recipient feels a Snare because they are taxed for services that are 
 * effectively non-existent or inaccessible. The Contractor sees a Rope 
 * because the public-private partnership is their primary coordination 
 * vehicle for revenue and growth.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "state" is no longer functional relative to its charter 
 * (Theater 0.94); it is an inert spike siphoning 0.87 of the public surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_capability_reclamation,
    'Can the state re-absorb its hollowed functions, or is the knowledge lost (Snare vs Mountain)?',
    'Tracking the success rate of "insourcing" attempts for critical infrastructure.',
    'If successful: Snare of policy. If failure: Mountain of Lost Institutional Memory.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(hollow_state_syndrome, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
