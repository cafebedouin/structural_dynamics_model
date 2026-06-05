% ============================================================================
% CONSTRAINT STORY: us_israel_faa_502b_nonenforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_israel_faa_502b_nonenforcement, []).

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
 *   constraint_id: us_israel_faa_502b_nonenforcement
 *   human_readable: Non-enforcement of US Foreign Assistance Act Section 502B for Israel
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the de facto non-enforcement of Section 502B of
 *   the US Foreign Assistance Act, which legally prohibits security
 *   assistance to countries engaged in a consistent pattern of gross human
 *   rights violations. Despite documented human rights concerns, the US
 *   continues to provide substantial military aid to Israel. This situation
 *   creates a tension between legal obligations, foreign policy goals, and
 *   human rights considerations. The non-enforcement has become
 *   institutionalized over decades, with significant implications for both
 *   the region and the credibility of US foreign policy.
 *
 * KEY AGENTS:
 *   - Israeli Government: Primary beneficiary (institutional/arbitrage)
 *   - US Defense Contractors: Secondary beneficiary (institutional/arbitrage)
 *   - Palestinian Civilians: Primary victim (powerless/trapped)
 *   - International Law: Secondary victim (moderate/constrained)
 *   - US Foreign Policy Credibility: Secondary victim (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, 0.65).
domain_priors:suppression_score(us_israel_faa_502b_nonenforcement, 0.75).
domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, theater_ratio, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_israel_faa_502b_nonenforcement, tangled_rope).
narrative_ontology:human_readable(us_israel_faa_502b_nonenforcement, "Non-enforcement of US Foreign Assistance Act Section 502B for Israel").
narrative_ontology:topic_domain(us_israel_faa_502b_nonenforcement, "geopolitical").

domain_priors:requires_active_enforcement(us_israel_faa_502b_nonenforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, israeli_government).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_defense_contractors).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, palestinian_civilians).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, international_law).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, us_foreign_policy_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Palestinian civilians experience the non-enforcement as a Snare. They are trapped within the conflict and bear the brunt of actions that might otherwise be constrained by the FAA. They have no exit option and little power to influence US policy.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% The Israeli government benefits from the non-enforcement, allowing it to pursue its security policies without the risk of losing US aid. This creates a Rope-like dynamic where they benefit from a continuous stream of resources and support.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% US defense contractors also benefit, as the continued provision of aid to Israel supports their sales and production. This perspective classifies as a Rope due to the institutional power and financial arbitrage.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees this as a Tangled Rope. There is a coordination aspect (US security interests aligned with Israel), but also asymmetric extraction (human rights violations and damage to international law).
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% From the perspective of the broader US foreign policy establishment, the non-enforcement can be seen as a Piton. The original intention of the FAA was to promote human rights, but the non-enforcement has become an entrenched practice that undermines this goal. It's maintained through inertia and political pressure, despite its functional degradation.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_israel_faa_502b_nonenforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, TR),
    TR >= 0.70.

:- end_tests(us_israel_faa_502b_nonenforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The non-enforcement allows the continuation of policies that extract a high cost from Palestinian civilians and undermine international law. Suppression (0.75): High. There is significant suppression of alternative policies due to domestic political pressure and strategic considerations. Theater Ratio (0.50): Moderate. While there are periodic statements and reports expressing concern about human rights, these are largely performative and do not lead to any substantive changes in policy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions and interests of the key agents. Palestinian civilians experience the policy as a snare, bearing the brunt of its consequences. The Israeli government and US defense contractors benefit from the continued aid, seeing it as a rope. The US foreign policy establishment is constrained by the institutionalized nature of the non-enforcement, resulting in a piton-like state. The analytical observer recognizes the tangled nature of the constraint, highlighting the interplay of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the beneficiaries and victims. The Israeli government and US defense contractors have a low 'd' value, reflecting their beneficiary status and arbitrage opportunities. Palestinian civilians have a high 'd' value due to their trapped position and victim status. The US foreign policy establishment has a moderate 'd' value, as they are constrained by the existing policy but also contribute to its maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it involves both a coordination function (US security interests aligned with Israel) and asymmetric extraction (human rights violations and damage to international law). The non-enforcement facilitates the coordination, but it also enables the extraction. Resolving the mandatrophy requires acknowledging both aspects and considering alternative policies that would minimize the extraction while preserving the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_violations,
    'What level or consistency of human rights violations would trigger enforcement of Section 502B?',
    'Analysis of past US foreign policy decisions and public statements regarding human rights violations in other countries.',
    'Determines whether the non-enforcement is a deliberate policy choice or a result of insufficient evidence of violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_violations, empirical, 'Defines the threshold of human rights violations necessary for FAA 502B enforcement.').

omega_variable(
    political_cost_tolerance,
    'What level of domestic or international political cost is the US government willing to bear to enforce Section 502B regarding Israel?',
    'Assessment of the political influence of pro-Israel lobbying groups and the potential diplomatic fallout from restricting aid.',
    'Determines the degree to which political considerations override legal obligations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_cost_tolerance, preference, 'The tolerance for political costs associated with FAA 502B enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_israel_faa_502b_nonenforcement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_i_tr_t0, us_israel_faa_502b_nonenforcement, theater_ratio, 0, 0.3).
narrative_ontology:measurement(us_i_tr_t10, us_israel_faa_502b_nonenforcement, theater_ratio, 10, 0.4).
narrative_ontology:measurement(us_i_tr_t20, us_israel_faa_502b_nonenforcement, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(us_i_be_t0, us_israel_faa_502b_nonenforcement, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_i_be_t10, us_israel_faa_502b_nonenforcement, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(us_i_be_t20, us_israel_faa_502b_nonenforcement, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_israel_faa_502b_nonenforcement, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
