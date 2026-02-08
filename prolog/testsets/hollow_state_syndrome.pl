% ============================================================================
% CONSTRAINT STORY: hollow_state_syndrome
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_hollow_state_syndrome, []).

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

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hollow_state_syndrome, extractiveness, 0.87).
narrative_ontology:constraint_metric(hollow_state_syndrome, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(hollow_state_syndrome, theater_ratio, 0.94).

% Constraint self-claim: It claims to be a more efficient coordination mechanism.
narrative_ontology:constraint_claim(hollow_state_syndrome, piton).

% Binary flags and structural properties for Tangled Rope / Piton detection.
% The contractual obligations require active enforcement to maintain.
domain_priors:requires_active_enforcement(hollow_state_syndrome).

% Structural property derivation hooks for Tangled Rope structure.
narrative_ontology:constraint_beneficiary(hollow_state_syndrome, private_prime_contractor).
narrative_ontology:constraint_victim(hollow_state_syndrome, public_service_recipient).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The extreme theater ratio (0.94 > 0.70) triggers a Piton classification.
% The "State Capability" is a performative facade masking total operational
% atrophy. The underlying structure is a Tangled Rope, but its functional
% component has degraded into pure theater.
constraint_indexing:constraint_classification(hollow_state_syndrome, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollow_state_syndrome_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(hollow_state_syndrome, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollow_state_syndrome, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hollow_state_syndrome, piton,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.94) triggers Piton classification.
    domain_priors:theater_ratio(hollow_state_syndrome, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(hollow_state_syndrome, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction is high, as expected for this type of constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hollow_state_syndrome, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_structure_present) :-
    % Verify that the structural components of a Tangled Rope are declared,
    % even though the final classification is Piton due to high theater.
    narrative_ontology:constraint_beneficiary(hollow_state_syndrome, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(hollow_state_syndrome, _),    % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(hollow_state_syndrome).

:- end_tests(hollow_state_syndrome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the
 * claimed "coordination" is a parasitic liquidation of state capacity for
 * private gain. The suppression score (0.73) reflects the legal and
 * contractual barriers preventing citizens from seeking redress.
 *
 * The key insight is the degradation path: what was likely once a Tangled Rope
 * (a mix of genuine coordination and asymmetric extraction) has decayed into a
 * Piton. The theater ratio of 0.94 indicates that the state's role is now
 * almost entirely performative, masking its inability to govern. The system
 * correctly classifies this terminal state as a Piton from an analytical view,
 * while still recognizing the underlying Tangled Rope structure through the
 * beneficiary/victim/enforcement declarations.
 *
 * * PERSPECTIVAL GAP:
 * The Recipient feels a Snare because they are taxed for services that are
 * effectively non-existent or inaccessible. The Contractor sees a Rope
 * because the public-private partnership is their primary coordination
 * vehicle for revenue and growth. The Auditor sees a Piton, an inert and
 * theatrical remnant of a functional state.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton classification. For an analytical observer, the
 * "state" is no longer functional relative to its charter (Theater 0.94);
 * it is an inert spike siphoning 0.87 of the public surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_hollow_state_syndrome,
    'Can the state re-absorb its hollowed functions, or is the institutional knowledge permanently lost (Snare vs Mountain)?',
    'Tracking the success rate of "insourcing" attempts for critical infrastructure over a decade.',
    'If successful: Snare of policy. If failure: Mountain of Lost Institutional Memory.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hollow_state_syndrome, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a process of decay. Initially, outsourcing had lower
% extraction and theater. Over time, as state capacity withered and contractor
% incentives dominated, both extraction and theater increased dramatically.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(hss_tr_t0, hollow_state_syndrome, theater_ratio, 0, 0.30).
narrative_ontology:measurement(hss_tr_t5, hollow_state_syndrome, theater_ratio, 5, 0.80).
narrative_ontology:measurement(hss_tr_t10, hollow_state_syndrome, theater_ratio, 10, 0.94).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(hss_ex_t0, hollow_state_syndrome, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(hss_ex_t5, hollow_state_syndrome, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(hss_ex_t10, hollow_state_syndrome, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The outsourcing of state functions is a form of resource allocation.
narrative_ontology:coordination_type(hollow_state_syndrome, resource_allocation).

% Network relationships: A hollowed state directly impacts its ability to
% maintain other critical infrastructure, creating a cascade of decay.
narrative_ontology:affects_constraint(hollow_state_syndrome, public_infrastructure_maintenance).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */