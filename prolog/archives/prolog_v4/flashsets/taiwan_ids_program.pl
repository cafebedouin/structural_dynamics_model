% ============================================================================
% CONSTRAINT STORY: taiwan_ids_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_ids_program, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: taiwan_ids_program
 *   human_readable: Taiwan's Indigenous Defense Submarine (IDS) Program
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Faced with sustained geopolitical pressure from the People's Republic of
 *   China (PRC) that effectively blockades the international sale of
 *   submarines to Taiwan, the Taiwanese state initiated the Indigenous
 *   Defense Submarine (IDS) program. This program aims to enhance Taiwan's
 *   defensive capabilities and reduce reliance on foreign suppliers. However,
 *   it also increases regional tensions and poses a challenge to PRC
 *   strategic interests.
 *
 * KEY AGENTS:
 *   - Taiwanese State: Primary beneficiary (institutional/arbitrage) - Gains enhanced defense capabilities and reduces reliance on foreign suppliers.
 *   - Taiwanese Shipbuilding Industry: Secondary beneficiary (moderate/mobile) - Benefits from technology transfer and job creation.
 *   - PRC Strategic Interests: Primary victim (institutional/constrained) - The IDS program challenges PRC's strategic interests.
 *   - Regional Powers: Powerful actors (powerful/mobile) - Face increased regional tensions but can adjust their military posture.
 *   - Regional Stability: Primary victim (powerless/trapped) - Decreased due to increased tensions and arms race.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_ids_program, 0.6).
domain_priors:suppression_score(taiwan_ids_program, 0.7).
domain_priors:theater_ratio(taiwan_ids_program, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_ids_program, extractiveness, 0.6).
narrative_ontology:constraint_metric(taiwan_ids_program, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(taiwan_ids_program, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_ids_program, tangled_rope).
narrative_ontology:human_readable(taiwan_ids_program, "Taiwan's Indigenous Defense Submarine (IDS) Program").
narrative_ontology:topic_domain(taiwan_ids_program, "geopolitical/technological").

domain_priors:requires_active_enforcement(taiwan_ids_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_ids_program, taiwanese_state).
narrative_ontology:constraint_beneficiary(taiwan_ids_program, taiwanese_shipbuilding_industry).
narrative_ontology:constraint_victim(taiwan_ids_program, regional_stability).
narrative_ontology:constraint_victim(taiwan_ids_program, prc_strategic_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Taiwanese state benefits from increased defense capabilities and reduced reliance on foreign suppliers. They are mitigating geopolitical risk and supporting domestic industry.
constraint_indexing:constraint_classification(taiwan_ids_program, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The program directly challenges PRC strategic interests by enhancing Taiwan's defensive capabilities, making any potential military action more costly and difficult. PRC is heavily constrained in its response options, due to diplomatic considerations.
constraint_indexing:constraint_classification(taiwan_ids_program, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Regional powers face a complex situation. On one hand, the IDS program increases regional tensions. On the other hand, it may contribute to deterrence and stability by raising the cost of military action against Taiwan. Exit options are mobile, as they can adjust their military posture and diplomatic relationships.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% An analytical observer sees the IDS program as a mixed bag. It enhances Taiwan's defensive capabilities but increases regional tensions. The program is also a response to geopolitical pressure from the PRC, which has effectively blocked international arms sales to Taiwan.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The Taiwanese shipbuilding industry benefits from the IDS program through technology transfer, job creation, and increased revenue. While they are impacted by the challenges of building complex submarines, they are net beneficiaries.
constraint_indexing:constraint_classification(taiwan_ids_program, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Increased tensions and an arms race in the region decrease regional stability.
constraint_indexing:constraint_classification(taiwan_ids_program, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_ids_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_ids_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_ids_program, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_ids_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taiwan_ids_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The program extracts resources from the region by increasing military spending and tensions. It also extracts from PRC strategic interests. Suppression (0.7): The PRC's blockade suppresses Taiwan's access to foreign submarines, leading to the IDS program. The IDS Program itself also suppresses regional stability. Theater Ratio (0.3): The program has a relatively low theater ratio, as it is primarily focused on building real military capabilities rather than symbolic displays of force.
 *
 * PERSPECTIVAL GAP:
 *   The Taiwanese state views the program as a necessary step to ensure its security. The PRC views it as a direct challenge to its strategic interests. Regional powers see a mixed bag, with increased tensions but also potentially increased deterrence. The victims, specifically regional stability, have the clearest negative perspective. Analytical observer sees mixed coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Taiwanese state benefits from increased security and reduced reliance on foreign suppliers, resulting in a low directionality. The PRC is targeted by the program, which challenges its strategic interests and increases the cost of potential military action, resulting in a high directionality. Regional powers are both affected by the program, leading to a moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The IDS program presents a mixed picture of coordination and extraction. It coordinates resources and efforts to enhance Taiwan's defense capabilities, but it also extracts from regional stability and PRC strategic interests. The program is thus best classified as a tangled rope. The key challenge in resolving the mandatrophy is to understand the program's impact on different actors and the region as a whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prc_response_threshold,
    'At what point does the IDS program trigger a disproportionate or escalatory response from the PRC?',
    'Monitoring PRC military deployments, rhetoric, and diplomatic communications. Analyzing historical responses to similar events.',
    'If the threshold is low, the IDS program could backfire and increase the risk of conflict. If the threshold is high, the program provides a credible deterrent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prc_response_threshold, empirical, 'Threshold for PRC escalation').

omega_variable(
    technology_transfer_effectiveness,
    'How effective is the technology transfer to the Taiwanese shipbuilding industry? Can they indigenously produce advanced submarines?',
    'Assessing the performance and reliability of the IDS submarines. Evaluating the development of domestic supply chains and expertise.',
    'If technology transfer is effective, the IDS program will provide a long-term boost to Taiwan''s defense capabilities. If it is ineffective, the program will be a costly failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_effectiveness, empirical, 'Effectiveness of tech transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_ids_program, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiw_tr_t0, taiwan_ids_program, theater_ratio, 0, 0.2).
narrative_ontology:measurement(taiw_tr_t5, taiwan_ids_program, theater_ratio, 5, 0.3).
narrative_ontology:measurement(taiw_tr_t10, taiwan_ids_program, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(taiw_be_t0, taiwan_ids_program, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(taiw_be_t5, taiwan_ids_program, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(taiw_be_t10, taiwan_ids_program, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_ids_program, resource_allocation).
narrative_ontology:affects_constraint(taiwan_ids_program, prc_military_modernization).
narrative_ontology:affects_constraint(taiwan_ids_program, us_taiwan_relations).

% DUAL FORMULATION NOTE:
% The IDS program is a response to the PRC's military modernization and US-Taiwan relations but represents a distinct structural constraint. The upstream constraints have their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
