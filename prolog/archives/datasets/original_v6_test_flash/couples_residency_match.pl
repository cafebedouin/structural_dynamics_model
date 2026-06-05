% ============================================================================
% CONSTRAINT STORY: couples_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_couples_residency_match, []).

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
 *   constraint_id: couples_residency_match
 *   human_readable: The Medical Residency Couples Match Algorithm
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Couples Match algorithm is a part of the NRMP that allows couples to
 *   link their rank order lists (ROLs). This aims to enable both partners to
 *   obtain residency positions in the same geographic location. However, it
 *   can also impose constraints on the choices available to couples,
 *   extracting concessions from both partners. It coordinates a national
 *   scale matching program, so has an inherent coordination function.
 *
 * KEY AGENTS:
 *   - Applicant Couples: Primary targets (powerless/trapped) - experience limited choices and potential career compromise.
 *   - NRMP: Primary beneficiary (institutional/arbitrage) - maintains control over residency placements and extracts fees.
 *   - Hospitals: Beneficiary (institutional/arbitrage) - fills residency positions using an established system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(couples_residency_match, 0.55).
domain_priors:suppression_score(couples_residency_match, 0.4).
domain_priors:theater_ratio(couples_residency_match, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(couples_residency_match, extractiveness, 0.55).
narrative_ontology:constraint_metric(couples_residency_match, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(couples_residency_match, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(couples_residency_match, tangled_rope).
narrative_ontology:human_readable(couples_residency_match, "The Medical Residency Couples Match Algorithm").
narrative_ontology:topic_domain(couples_residency_match, "technological/economic").

domain_priors:requires_active_enforcement(couples_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(couples_residency_match, nrmp).
narrative_ontology:constraint_beneficiary(couples_residency_match, hospitals).
narrative_ontology:constraint_victim(couples_residency_match, applicant_couples).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Applicant couples can feel trapped by the algorithm due to geographical limitations and the desire to stay together. The algorithm can extract concessions regarding program rank or specialty from each partner in a couple.
constraint_indexing:constraint_classification(couples_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The NRMP benefits from the Couples Match algorithm as it maintains its control over the residency placement system and charges fees for participation. It presents itself as providing a valuable service to applicants and residency programs alike.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the Couples Match algorithm can be seen as a tangled rope. It attempts to coordinate the preferences of couples with the needs of residency programs, but also extracts value from the applicants by limiting their options and enforcing a centralized matching process.
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Hospitals and Residency Programs also benefit from the system by filling their open slots and potentially getting a more willing cohort of residents. The labor market dynamics allow programs to extract value from residents.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(couples_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(couples_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(couples_residency_match, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(couples_residency_match, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(couples_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Couples face limitations on their choices and may need to compromise on preferred programs or specialties to stay together. NRMP maintains control over the system and charges fees. Suppression (0.40): Moderate. Couples may feel pressured to participate in the match, even if it limits their options. The centralized nature of the system suppresses alternative matching mechanisms. Theater Ratio (0.20): Low. The Couples Match algorithm is primarily functional, aiming to facilitate residency placements. The performative aspect is relatively low.
 *
 * PERSPECTIVAL GAP:
 *   Applicant couples can feel trapped by the algorithm, limiting their options and potentially extracting career compromises. The NRMP benefits from the system's control and the revenue it generates. Hospitals also benefit by filling their residency slots. The analytical observer sees a tangled rope - a mix of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the relationship between the agents and the constraint. Applicant couples are victims because they bear the cost of limited choices. The NRMP and hospitals are beneficiaries as they gain from the system's coordination function and control. This determines the extraction values experienced by each agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The Couples Match algorithm aims to coordinate couples' preferences with residency program openings, but it can also extract value from the applicants by limiting their options. Properly classified, it is a tangled rope, neither pure coordination nor pure extraction. Alternative systems are not readily available, and can often only be accessed through personal connections. The algorithm presents an equitable option, though with potentially restrictive conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_matching_algorithms,
    'Could alternative matching algorithms better address the needs of couples while minimizing constraints?',
    'Comparative analysis of different matching algorithms in simulation or real-world settings.',
    'If alternative algorithms are superior, the Couples Match could be reclassified as a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_matching_algorithms, empirical, 'Possibility of alternative, less-constraining matching algorithms.').

omega_variable(
    applicant_preference_weighting,
    'How accurately does the algorithm capture and weight the individual preferences of each partner in a couple?',
    'Surveys and interviews with couples participating in the match, combined with analysis of rank order lists.',
    'If preferences are poorly represented, extraction increases. If well-represented, coordination increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(applicant_preference_weighting, empirical, 'Accuracy of preference representation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(couples_residency_match, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coup_tr_t0, couples_residency_match, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coup_tr_t5, couples_residency_match, theater_ratio, 5, 0.15).
narrative_ontology:measurement(coup_tr_t10, couples_residency_match, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(coup_be_t0, couples_residency_match, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(coup_be_t5, couples_residency_match, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(coup_be_t10, couples_residency_match, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(couples_residency_match, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
