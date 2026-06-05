% ============================================================================
% CONSTRAINT STORY: civilizational_lifecycle_solara
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_lifecycle_solara, []).

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
 *   constraint_id: civilizational_lifecycle_solara
 *   human_readable: The Lifecycle of Solaran Civilization
 *   domain: social/political
 *
 * SUMMARY:
 *   The Solaran civilization's lifecycle is a model of unsustainable growth,
 *   resource depletion, and eventual decline. Initially, coordination
 *   mechanisms facilitated growth and prosperity. Over time, these mechanisms
 *   were subverted to serve the interests of a ruling elite, leading to
 *   increased extraction from the general population and future generations.
 *   Ideological institutions, once promoting social cohesion, became
 *   instruments of suppression. This cycle culminates in a civilizational
 *   collapse, leaving the general population trapped in a snare of scarcity
 *   and environmental degradation.
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) – bears the cost of resource depletion and suppression.
 *   - Ruling Elite: Primary beneficiary (moderate/constrained) – benefits from the system in the short term but is constrained by its eventual collapse.
 *   - Ideological Institutions: Secondary actor (institutional/constrained) – Initially provides coordination, later performs theatrical maintenance of the system.
 *   - Future Generations: Secondary victim (powerless/trapped) - suffer the long-term consequences
 *   - Analytical Observer: analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_lifecycle_solara, 0.65).
domain_priors:suppression_score(civilizational_lifecycle_solara, 0.75).
domain_priors:theater_ratio(civilizational_lifecycle_solara, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, extractiveness, 0.65).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_lifecycle_solara, tangled_rope).
narrative_ontology:human_readable(civilizational_lifecycle_solara, "The Lifecycle of Solaran Civilization").
narrative_ontology:topic_domain(civilizational_lifecycle_solara, "social/political").

domain_priors:requires_active_enforcement(civilizational_lifecycle_solara).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, ruling_elite).
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, ideological_institutions).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, general_population).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general population in the late stage of Solaran civilization experiences the lifecycle as a snare. Trapped by declining resources, environmental degradation, and rigid social structures, they lack the power to exit or change the system.  Extraction is high as the ruling elite maintains control through suppression.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% The ruling elite benefits from the current system, extracting resources and maintaining power. However, they are also constrained by the system's inherent instability and the need to suppress dissent. They perceive a tangled rope: benefit from present but are constrained by coming civilizational end.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Ideological institutions (e.g., state religion, propaganda ministries) initially served a coordination function but, in the late stage, largely engage in performative activity to maintain the status quo.  Their functional role has atrophied, but the institutions persist due to inertia.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer, studying the Solaran civilization from a distance, sees the entire lifecycle as a Tangled Rope: a system with inherent contradictions and unsustainable practices that lead to its eventual collapse.  There is a period of coordination followed by extraction and suppression.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_lifecycle_solara_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilizational_lifecycle_solara, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_lifecycle_solara, TR),
    TR >= 0.70.

:- end_tests(civilizational_lifecycle_solara_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The ruling elite extracts a significant amount of resources from the general population and the environment, leading to depletion and degradation. Suppression (0.75): High. The ruling elite actively suppresses dissent and alternative solutions, maintaining control through force and propaganda. Theater Ratio (0.30): Moderate. While there is some performative activity, the system is still largely functional in extracting resources and suppressing dissent.
 *
 * PERSPECTIVAL GAP:
 *   The general population experiences the late stage as a Snare due to their lack of power and exit options. The ruling elite sees it as a Tangled Rope because they benefit from the system but are also constrained by its inherent instability. Ideological institutions see a Piton because their functional role has atrophied, but they continue to perform theatrical rituals to maintain the status quo. The analytical observer sees the entire lifecycle as a Tangled Rope because it involves both coordination and extraction, leading to an unsustainable outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the agents' structural positions. The general population has a high d-value because they are the primary victims with no exit options. The ruling elite has a moderate d-value because they benefit from the system but are also constrained by it. The ideological institutions have a medium d-value.
 *
 * MANDATROPHY ANALYSIS:
 *   The system exhibits characteristics of both coordination and extraction. While there is some initial coordination that facilitates growth and prosperity, this is eventually subverted to serve the interests of the ruling elite, leading to increased extraction and suppression. The Mandatrophy is resolved by recognizing that the balance between coordination and extraction shifts over time, ultimately leading to an unsustainable outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_depletion_point,
    'At what point does resource depletion become irreversible, triggering civilizational decline?',
    'Historical modeling of resource consumption, environmental impact, and technological innovation.',
    'Determines the timing and severity of the snare experienced by the general population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_depletion_point, empirical, 'Timeline of resource depletion impact on Solarans').

omega_variable(
    elite_adaptability,
    'To what extent is the ruling elite capable of adapting to changing circumstances and implementing sustainable policies?',
    'Analysis of elite decision-making processes, ideological commitments, and openness to innovation.',
    'Determines whether the tangled rope experienced by the ruling elite transforms into a scaffold (successful transition) or a tighter snare (continued decline).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_adaptability, conceptual, 'Ruling Elite adaptability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_lifecycle_solara, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civilizational_lifecycle_solara, theater_ratio, 0, 0.1).
narrative_ontology:measurement(civi_tr_t50, civilizational_lifecycle_solara, theater_ratio, 50, 0.2).
narrative_ontology:measurement(civi_tr_t100, civilizational_lifecycle_solara, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civilizational_lifecycle_solara, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(civi_be_t50, civilizational_lifecycle_solara, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(civi_be_t100, civilizational_lifecycle_solara, base_extractiveness, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_lifecycle_solara, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
