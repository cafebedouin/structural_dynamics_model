% ============================================================================
% CONSTRAINT STORY: world_factbook_sunset_2026
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_world_factbook_sunset_2026, []).

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
 *   constraint_id: world_factbook_sunset_2026
 *   human_readable: CIA World Factbook Termination
 *   domain: geopolitical/information
 *
 * SUMMARY:
 *   The CIA World Factbook termination represents a shift in information
 *   control and a redistribution of resources. While the CIA views this as a
 *   return to core missions, the termination creates a void for neutral,
 *   comprehensive information that negatively impacts policy analysts,
 *   journalists, and research institutions. At the same time, it benefits
 *   competing intelligence agencies and provides new opportunities for the
 *   open source intelligence community.
 *
 * KEY AGENTS:
 *   - CIA Management: Institutional actor (institutional/arbitrage) — benefits from resource reallocation.
 *   - Policy Analysts, Journalists, and Research Institutions: Primary victims (powerless/trapped) — lose a valuable resource.
 *   - Competing Intelligence Agencies: Powerful actors (powerful/arbitrage) — benefit from the loss of a neutral source.
 *   - Open Source Intelligence (OSINT) Community: Organized agents (organized/mobile) — benefits from increased demand.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(world_factbook_sunset_2026, 0.45).
domain_priors:suppression_score(world_factbook_sunset_2026, 0.5).
domain_priors:theater_ratio(world_factbook_sunset_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(world_factbook_sunset_2026, extractiveness, 0.45).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(world_factbook_sunset_2026, scaffold).
narrative_ontology:human_readable(world_factbook_sunset_2026, "CIA World Factbook Termination").
narrative_ontology:topic_domain(world_factbook_sunset_2026, "geopolitical/information").

domain_priors:requires_active_enforcement(world_factbook_sunset_2026).
narrative_ontology:has_sunset_clause(world_factbook_sunset_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, competing_intelligence_agencies).
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, open_source_intelligence_community).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, policy_analysts).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, journalists).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, research_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The policy analysts and journalists are trapped without a readily available, neutral source of basic information. They are the primary victims, and view this as a loss. There are alternative sources, but none with the same perceived neutrality and comprehensiveness.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% CIA Management sees this as a return to core missions, and a sunsetting of a non-core function that can be fulfilled by other entities. They benefit from resource reallocation.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Competing intelligence agencies benefit from the loss of a public resource from a rival. They can more easily control the narrative and manipulate information if there is no neutral source. They experience this as a coordination advantage.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The OSINT community benefits from the increased demand for their services. They are organized and mobile, and can quickly fill the information gap.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical observer's perspective, this is a tangled rope. There is a loss of a public resource, but also a redistribution of information control and opportunities for other entities.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(world_factbook_sunset_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(world_factbook_sunset_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The termination extracts a reliable, neutral information source from policy analysts, journalists and research institutions. The value is moderate (0.45) since alternative sources do exist, albeit with potential bias. Suppression: The termination suppresses access to a specific, high-quality source. The level of suppression is 0.50, reflecting that there are alternative (though potentially less reliable) sources. Theater Ratio: The theater ratio is low (0.30) because the CIA's motivation is primarily functional (resource reallocation), not performative.
 *
 * PERSPECTIVAL GAP:
 *   The CIA sees this as a return to core functions, a scaffold that is no longer needed. However, the policy analysts and journalists experience this as a loss, a snare that reduces access to reliable information. Competing intelligence agencies experience it as a coordination mechanism that gives them an advantage, while the OSINT community sees it as a coordination opportunity. The analytical observer sees a tangled rope, a mixture of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The CIA management benefits from reallocating resources, thus experiencing low extraction. The policy analysts and journalists are the primary victims, as they lose access to a reliable information source, thus experiencing high extraction. Competing intelligence agencies benefit from the reduced availability of neutral information, while the OSINT community benefits from increased demand for their services.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_source_neutrality,
    'Will alternative sources of information maintain the perceived neutrality of the World Factbook?',
    'Comparative analysis of bias in alternative sources.',
    'If alternative sources are biased, the classification shifts towards snare for the general public. If neutral, then the classification remains a scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_source_neutrality, empirical, 'The neutrality of alternative information sources.').

omega_variable(
    open_source_filling_the_gap,
    'Will the open source intelligence community adequately fill the information gap left by the World Factbook?',
    'Analysis of the comprehensiveness and accuracy of OSINT data.',
    'If the OSINT community can fill the gap, the sunset is less harmful. If not, the classification shifts towards snare for the general public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_filling_the_gap, empirical, 'Whether the open source intelligence community adequately fills the information gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(world_factbook_sunset_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(worl_tr_t0, world_factbook_sunset_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(worl_tr_t1, world_factbook_sunset_2026, theater_ratio, 1, 0.2).
narrative_ontology:measurement(worl_tr_t2, world_factbook_sunset_2026, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(worl_be_t0, world_factbook_sunset_2026, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(worl_be_t1, world_factbook_sunset_2026, base_extractiveness, 1, 0.32).
narrative_ontology:measurement(worl_be_t2, world_factbook_sunset_2026, base_extractiveness, 2, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(world_factbook_sunset_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
