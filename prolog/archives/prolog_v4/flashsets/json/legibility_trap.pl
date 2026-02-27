% ============================================================================
% CONSTRAINT STORY: legibility_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_trap, []).

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
 *   constraint_id: legibility_trap
 *   human_readable: The Grid-Map Displacement
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   The Grid-Map Displacement describes the scenario where a state or
 *   institution, seeking to improve governability, imposes simplified and
 *   standardized metrics (legibility) onto a complex and organic social
 *   system. While intended as a 'rope' for administrative coordination and
 *   resource allocation, this act of simplification often destroys
 *   high-resolution local knowledge and informal safety nets, effectively
 *   trapping the communities affected and becoming a 'snare'. This constraint
 *   is an example of how well-intentioned efforts to improve governance can
 *   have unintended negative consequences, particularly for vulnerable
 *   populations. The theater_ratio increases over time as the system becomes
 *   more about performative data collection than actual improvement.
 *
 * KEY AGENTS:
 *   - Local Communities: Primary victims (powerless/trapped) - lose informal safety nets and local knowledge.
 *   - State Administrators: Primary beneficiaries (institutional/constrained) - gain improved coordination but are constrained by the system.
 *   - Central Planners: Secondary beneficiaries (institutional/constrained) - implement and maintain the standardized metrics.
 *   - Analytical Observer: External observer (analytical/analytical) - analyzes the overall impact of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_trap, 0.55).
domain_priors:suppression_score(legibility_trap, 0.7).
domain_priors:theater_ratio(legibility_trap, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_trap, extractiveness, 0.55).
narrative_ontology:constraint_metric(legibility_trap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legibility_trap, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_trap, tangled_rope).
narrative_ontology:human_readable(legibility_trap, "The Grid-Map Displacement").
narrative_ontology:topic_domain(legibility_trap, "political/social/economic").

domain_priors:requires_active_enforcement(legibility_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_trap, state_administrators).
narrative_ontology:constraint_beneficiary(legibility_trap, central_planners).
narrative_ontology:constraint_victim(legibility_trap, local_communities).
narrative_ontology:constraint_victim(legibility_trap, informal_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Local communities experience the imposition of standardized metrics as a snare, as it destroys their existing informal safety nets and high-resolution local knowledge, leaving them trapped and vulnerable.
constraint_indexing:constraint_classification(legibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% State administrators initially perceive the increased legibility as a 'rope', facilitating better coordination and resource allocation. However, they are constrained by the system they created and may not fully recognize the downsides for local communities.
constraint_indexing:constraint_classification(legibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the situation as a 'tangled rope,' recognizing the benefits of improved coordination for the state but also the negative consequences for local communities and the loss of valuable local knowledge and resilience.
constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% As the initial benefits of legibility fade and the negative consequences become more apparent, central planners may find themselves stuck with a system that is difficult to change, effectively becoming a 'piton' due to institutional inertia. The theater ratio is high because the planners are now mostly performing rituals of data collection and reporting without achieving meaningful improvements in outcomes.
constraint_indexing:constraint_classification(legibility_trap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legibility_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legibility_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legibility_trap, TR),
    TR >= 0.70.

:- end_tests(legibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The imposition of standardized metrics extracts value from local communities by undermining their existing informal systems and knowledge. Suppression (0.70): The standardized metrics actively suppress alternative forms of knowledge and organization. Theater Ratio (0.75): The theater ratio is high because the planners are now mostly performing rituals of data collection and reporting without achieving meaningful improvements in outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different structural positions of the agents involved. State administrators, focused on national-level coordination, may not fully recognize the costs imposed on local communities. Local communities, trapped within the system, experience the loss of their informal safety nets directly. The analytical observer sees the overall trade-off between national coordination and local disruption.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's relationship to the extraction flow. Local communities are victims, bearing the costs of the system. State administrators and central planners are beneficiaries, gaining improved coordination. The analytical observer has a neutral perspective, analyzing the overall system dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing the initial benefits of legibility for state administrators and central planners. However, it also highlights the unintended negative consequences for local communities, preventing the system from being classified solely as a 'rope'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_quantification_error,
    'To what extent can the standardized metrics accurately capture the complex reality of local knowledge and informal economies?',
    'Ethnographic studies and quantitative analysis comparing outcomes under the standardized system vs. the previous informal systems.',
    'If the error is high, the benefits of legibility are outweighed by the loss of valuable local knowledge, shifting the classification more towards a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_quantification_error, empirical, 'The degree to which standardized metrics fail to capture local knowledge.').

omega_variable(
    community_resilience_capacity,
    'How much resilience is lost within local communities as their informal safety nets are replaced with formal systems?',
    'Analysis of community responses to crises under both systems, measuring factors like resourcefulness and mutual aid.',
    'If resilience significantly decreases, the negative impact on local communities is higher, reinforcing the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_resilience_capacity, empirical, 'The degree to which community resilience is eroded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legibility_trap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(legi_tr_t5, legibility_trap, theater_ratio, 5, 0.6).
narrative_ontology:measurement(legi_tr_t10, legibility_trap, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legibility_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t5, legibility_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(legi_be_t10, legibility_trap, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legibility_trap, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
