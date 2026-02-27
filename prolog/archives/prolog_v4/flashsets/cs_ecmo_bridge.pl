% ============================================================================
% CONSTRAINT STORY: cs_ecmo_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cs_ecmo_bridge, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cs_ecmo_bridge
 *   human_readable: ECMO Bridge to Transplant
 *   domain: technological
 *
 * SUMMARY:
 *   ECMO (Extracorporeal Membrane Oxygenation) is used as a bridge to lung
 *   transplantation, providing respiratory support to patients awaiting a
 *   suitable organ. It allows surgeons to keep a patient alive for a limited
 *   time before a lung transplant. While ECMO provides vital support, it also
 *   poses risks and raises ethical questions about access and resource
 *   allocation.
 *
 * KEY AGENTS:
 *   - Patients awaiting transplant: Primary target (powerless/trapped) - dependent on ECMO for survival
 *   - Transplant surgeons: Primary beneficiary (institutional/arbitrage) - ECMO allows for better management of patients
 *   - ECMO device manufacturers: Secondary beneficiary (powerful/arbitrage) - Profit from sales and use of ECMO devices
 *   - Patients without access to ECMO: Secondary target (powerless/trapped) - Excluded from potentially life-saving intervention
 *   - Healthcare system: Constrained actor (moderate/constrained) - Balances costs and benefits of ECMO use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cs_ecmo_bridge, 0.6).
domain_priors:suppression_score(cs_ecmo_bridge, 0.4).
domain_priors:theater_ratio(cs_ecmo_bridge, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cs_ecmo_bridge, extractiveness, 0.6).
narrative_ontology:constraint_metric(cs_ecmo_bridge, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cs_ecmo_bridge, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cs_ecmo_bridge, tangled_rope).
narrative_ontology:human_readable(cs_ecmo_bridge, "ECMO Bridge to Transplant").
narrative_ontology:topic_domain(cs_ecmo_bridge, "technological").

domain_priors:requires_active_enforcement(cs_ecmo_bridge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, transplant_surgeons).
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, ecmo_device_manufacturers).
narrative_ontology:constraint_victim(cs_ecmo_bridge, patients_awaiting_transplant).
narrative_ontology:constraint_victim(cs_ecmo_bridge, patients_without_access_to_ecmo).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients awaiting transplant, particularly those in critical condition, are trapped. ECMO is their only option, but it only buys them time, creating a snare if a suitable organ is not found in time.
constraint_indexing:constraint_classification(cs_ecmo_bridge, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Transplant surgeons benefit from ECMO as it allows them to manage the patient while waiting for a suitable organ, improving the chances of a successful transplant. ECMO provides a tool to optimize outcomes and coordinate the transplant.
constraint_indexing:constraint_classification(cs_ecmo_bridge, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The healthcare system is both constrained and benefits from ECMO. It is constrained by the high cost and limited availability of ECMO, but benefits from it as it potentially increases the number of successful transplants and improves patient outcomes. The distribution of resources presents a challenge in balancing the costs and benefits to all patients, creating a tangled rope situation.
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a global, long-term perspective, ECMO as a bridge to transplant is a tangled rope. It coordinates the allocation of scarce resources (organs, ECMO machines, skilled staff) but also extracts resources and creates potential inequities (access based on location, wealth, or health status).
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cs_ecmo_bridge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cs_ecmo_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cs_ecmo_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because while ECMO is essential for some patients, it also has inherent risks and high costs associated with its use. Suppression is moderate (0.40) as there are limited alternative treatments for patients in end-stage respiratory failure, but ECMO is not universally available. Theater ratio is low (0.20) because the performance is directly related to the functionality of the transplant success.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions of the stakeholders. Patients facing death see it as a last resort (snare), surgeons see it as a valuable tool (rope), the health care system sees both potential and constraints, and the analytical observer sees both benefits and potential issues regarding equity.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients awaiting transplant have no exit option, they are trapped, leading to a higher d value. Transplant surgeons have arbitrage via their knowledge and skill making them a beneficiary with a lower d value. Healthcare system is constrained, needing to balance cost and outcomes, resulting in a moderate d value.
 *
 * MANDATROPHY ANALYSIS:
 *   It's essential to differentiate ECMO as beneficial assistance versus a situation where it serves to extract from vulnerable patients. ECMO is a coordination mechanism to allow for life-saving transplants, not just a tool to keep someone alive for longer while accruing costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organ_availability,
    'How can organ availability be improved to reduce the time patients spend on ECMO?',
    'Increase public awareness of organ donation, improve organ preservation techniques, and expand the donor pool.',
    'Reduced time on ECMO would lessen the risks and costs associated with prolonged use, shifting the classification toward a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organ_availability, empirical, 'Improvement of organ availability.').

omega_variable(
    ecmo_access_equity,
    'How can ECMO access be made more equitable across different socioeconomic groups and geographic locations?',
    'Implement policies that prioritize equitable access, increase funding for ECMO programs in underserved areas, and develop more affordable ECMO technologies.',
    'Increased equity in access would reduce the extraction from vulnerable populations, shifting the classification towards a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecmo_access_equity, preference, 'Improve ECMO access equity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cs_ecmo_bridge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cs_e_tr_t0, cs_ecmo_bridge, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cs_e_tr_t5, cs_ecmo_bridge, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cs_e_tr_t10, cs_ecmo_bridge, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(cs_e_be_t0, cs_ecmo_bridge, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cs_e_be_t5, cs_ecmo_bridge, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cs_e_be_t10, cs_ecmo_bridge, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cs_ecmo_bridge, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
