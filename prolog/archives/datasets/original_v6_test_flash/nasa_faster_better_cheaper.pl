% ============================================================================
% CONSTRAINT STORY: nasa_faster_better_cheaper
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nasa_faster_better_cheaper, []).

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
 *   constraint_id: nasa_faster_better_cheaper
 *   human_readable: The "Faster, Better, Cheaper" (FBC) Management Paradigm
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   "Faster, Better, Cheaper" (FBC) was a NASA management philosophy from
 *   1992-1999 designed to increase mission frequency and reduce costs by
 *   accepting higher technical risk. This constraint story examines the
 *   perspective of the NASA Administration, Private Contractors, NASA
 *   Scientists and Engineers, Mission Success Rates, and an Analytical
 *   Observer. The increased number of missions strained the existing
 *   workforce and pressured them to take short-cuts which damaged long-term
 *   mission planning and success rates. Private contractors were able to
 *   secure more contracts by offering lower bids, cutting back on testing and
 *   simplified risk-management, accepting the increased mission risk.
 *
 * KEY AGENTS:
 *   - NASA Administration: Beneficiary (institutional/arbitrage) — benefited from reduced costs and increased mission frequency
 *   - Private Contractors: Beneficiary (institutional/arbitrage) — benefited from increased contract opportunities
 *   - NASA Scientists and Engineers: Target (moderate/constrained) — constrained by reduced budgets and increased workloads, and damaged moral
 *   - Mission Success Rates: Target (powerless/trapped) — suffered from increased failure rates due to higher risk
 *   - Analytical Observer: Assesses overall impact (analytical/analytical) — considers both benefits and costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nasa_faster_better_cheaper, 0.6).
domain_priors:suppression_score(nasa_faster_better_cheaper, 0.4).
domain_priors:theater_ratio(nasa_faster_better_cheaper, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, extractiveness, 0.6).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nasa_faster_better_cheaper, tangled_rope).
narrative_ontology:human_readable(nasa_faster_better_cheaper, "The \"Faster, Better, Cheaper\" (FBC) Management Paradigm").
narrative_ontology:topic_domain(nasa_faster_better_cheaper, "political/economic/technological").

domain_priors:requires_active_enforcement(nasa_faster_better_cheaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nasa_faster_better_cheaper, nasa_administration).
narrative_ontology:constraint_beneficiary(nasa_faster_better_cheaper, private_contractors).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, mission_success_rates).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, nasa_scientists_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of overall mission success, FBC became a snare. The pressure to launch faster and cheaper led to increased risk-taking and ultimately, higher failure rates for specific missions. Mission success is trapped, unable to exit the pressure for speed and budget cuts.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% NASA scientists and engineers experienced the FBC paradigm as a tangled rope. They were constrained by reduced budgets and tighter timelines, leading to increased workloads and pressure. However, they also benefited from the increased frequency of missions and opportunities for innovation, but the pressure caused corner-cutting that increased mission failure rate and damaged morale.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From the perspective of the NASA administration, the FBC paradigm initially appeared as a rope. It allowed them to launch more missions with limited budgets, demonstrating responsiveness to political pressures for cost-effectiveness and increasing public interest, but without a true accounting for the cost of each failure.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the perspective of private contractors, the FBC paradigm appeared as a rope. It allowed them to win more contracts with lower bids, due to cost cutting measures such as reduced testing, streamlined development and simplified risk management. However, they also assumed additional risk of failure, but with the benefit of volume and access to federal resources.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective over a longer time horizon, the FBC paradigm is viewed as a tangled rope, representing a trade-off between cost, speed, and risk. The policy extracted from mission success rates (victim) while offering benefits to the NASA administration and private contractors by lowering costs and increasing mission frequency. However, this ultimately came at a cost of lower reliability and mission failures.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nasa_faster_better_cheaper_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nasa_faster_better_cheaper, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nasa_faster_better_cheaper_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60 - The FBC paradigm extracted resources from mission success rates and strained the NASA workforce. Suppression: 0.40 - While there wasn't direct suppression, the pressure to adhere to the FBC model limited alternative approaches. Theater ratio: 0.30 - Relatively low, as the paradigm was genuinely focused on efficiency, though this did not account for the real cost of mission failures in the long term.
 *
 * PERSPECTIVAL GAP:
 *   The NASA administration and private contractors initially viewed FBC as a positive development (rope), enabling more missions with limited budgets. However, scientists and engineers experienced it as a tangled rope, balancing increased opportunities with strained resources and increased risk. Mission success rates suffered, turning FBC into a snare. The analytical observer identifies the overall paradigm as a tangled rope, acknowledging both the benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic is determined by the structural relationship to the extraction flow. The NASA administration and private contractors, as beneficiaries, experience negative extractiveness. NASA scientists and engineers, and mission success rates, as targets, experience positive extractiveness. The analytical observer considers the overall balance.
 *
 * MANDATROPHY ANALYSIS:
 *   The FBC paradigm highlights the importance of considering multiple perspectives when assessing a management philosophy. What appears as a beneficial strategy from one viewpoint (NASA administration) can be detrimental from another (mission success rates). This example resolves the mandatrophy by demonstrating that a single policy can embody multiple constraint types depending on the agent's position within the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acceptable_risk_threshold,
    'What is the acceptable level of risk in space exploration, balancing cost, speed, and scientific return?',
    'Cost-benefit analysis of different mission profiles with varying risk levels, considering potential scientific discoveries and technological advancements.',
    'Determines whether the FBC paradigm was ultimately beneficial or detrimental to space exploration. A lower acceptable risk threshold would classify FBC as primarily a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_risk_threshold, preference, 'Acceptable level of risk in space exploration').

omega_variable(
    long_term_impact_of_failures,
    'What is the long-term impact of mission failures on public perception and funding for space exploration?',
    'Analysis of public opinion polls and government funding allocation trends following major mission failures.',
    'Determines whether the cost-saving benefits of FBC were offset by the negative consequences of increased failures. High long-term impact would increase the extractiveness assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_of_failures, empirical, 'Long-term impact of mission failures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nasa_faster_better_cheaper, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nasa_tr_t0, nasa_faster_better_cheaper, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nasa_tr_t3, nasa_faster_better_cheaper, theater_ratio, 3, 0.3).
narrative_ontology:measurement(nasa_tr_t7, nasa_faster_better_cheaper, theater_ratio, 7, 0.35).

% Extraction over time
narrative_ontology:measurement(nasa_be_t0, nasa_faster_better_cheaper, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nasa_be_t3, nasa_faster_better_cheaper, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(nasa_be_t7, nasa_faster_better_cheaper, base_extractiveness, 7, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nasa_faster_better_cheaper, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
