% ============================================================================
% CONSTRAINT STORY: the_churn_systemic_upheaval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_churn_systemic_upheaval, []).

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
 *   constraint_id: the_churn_systemic_upheaval
 *   human_readable: The Churn (Systemic Collapse and Rebirth)
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   "The Churn" represents a period of systemic instability where established
 *   social and political rules dissolve and reconfigure. This period involves
 *   both destructive forces (collapse of old systems, increased inequality,
 *   violence) and potentially generative ones (new social movements,
 *   technological innovation, political reforms). It is characterized by high
 *   uncertainty and a struggle for power between different actors.
 *
 * KEY AGENTS:
 *   - Legacy System Dependents: Those who rely on the old system and are negatively impacted by its collapse. (powerless/trapped)
 *   - Adaptive Elites: Actors who can adapt to and exploit the new environment, often benefiting from the instability. (institutional/arbitrage)
 *   - Middle Class/Constrained Professionals: Somewhat mobile but find their careers and livelihoods disrupted. (moderate/constrained)
 *   - Rule of Law: The institutions and norms that maintain order and justice. (institutional/constrained)
 *   - Early Adopters of New Systems: Individuals or groups that adopt and develop new technologies, social structures, or political ideologies that will form the new system.(institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_churn_systemic_upheaval, 0.6).
domain_priors:suppression_score(the_churn_systemic_upheaval, 0.7).
domain_priors:theater_ratio(the_churn_systemic_upheaval, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, extractiveness, 0.6).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_churn_systemic_upheaval, tangled_rope).
narrative_ontology:human_readable(the_churn_systemic_upheaval, "The Churn (Systemic Collapse and Rebirth)").
narrative_ontology:topic_domain(the_churn_systemic_upheaval, "political/social/economic").

domain_priors:requires_active_enforcement(the_churn_systemic_upheaval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, adaptive_elites).
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, early_adopters_new_systems).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, legacy_system_dependents).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Those dependent on the old system find themselves trapped and exploited by the instability and the rise of new power structures. Their previous skills and resources are devalued.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Professionals who are somewhat mobile but find their careers and livelihoods disrupted by the changing landscape. Some benefit from new opportunities, but generally face significant disruption and risk.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Elites and global capital who can adapt to and exploit the new environment, benefiting from the instability and reconfiguration.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% International institutions and laws become weakened or disregarded as new powers rise and old ones decline. The system continues in name, but is largely performative.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analysts looking at the long-term trends of systemic collapse, recognizing both the destructive and potentially generative aspects of the churn.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_churn_systemic_upheaval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_churn_systemic_upheaval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_churn_systemic_upheaval, TR),
    TR >= 0.70.

:- end_tests(the_churn_systemic_upheaval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60. The churn creates opportunities for exploitation and extraction as old rules break down and new ones are established. Suppression: 0.70. The instability suppresses alternatives to the emerging new power structures. Theater Ratio: 0.75. As old systems decay, performative adherence to their norms increases even as functional activity shifts elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   Different actors perceive 'The Churn' differently based on their position in the system. Those dependent on the old system experience it as a snare. Adaptive elites see it as an opportunity. Analysts see a mix of destruction and creation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's capacity to exit the churn. Those trapped in failing systems experience high extraction. Those with arbitrage options benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as Tangled Rope because it includes both extraction and coordination elements. While the process extracts from vulnerable populations and systems, it also coordinates adaptation and the creation of new social orders. Mistaking the process as pure extraction would ignore the role of coordination in establishing the next system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_stability,
    'Will the new system establish stability and equity or recreate new forms of exploitation?',
    'Longitudinal analysis of social mobility, income distribution, and political participation in the new system.',
    'Determines if the ''rebirth'' aspect of the churn is realized or if it''s merely a transition to a different form of snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_stability, empirical, 'Long-term stability and equity').

omega_variable(
    adaptation_capacity,
    'How adaptable are different social groups to the changing environment?',
    'Survey data on skill acquisition, resource mobilization, and social network formation during the churn.',
    'Identifies which groups are most vulnerable and which are best positioned to benefit from the new system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_capacity, empirical, 'Adaptation capacity of social groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_churn_systemic_upheaval, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(the__tr_t0, the_churn_systemic_upheaval, theater_ratio, 0, 0.55).
narrative_ontology:measurement(the__tr_t5, the_churn_systemic_upheaval, theater_ratio, 5, 0.65).
narrative_ontology:measurement(the__tr_t10, the_churn_systemic_upheaval, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(the__be_t0, the_churn_systemic_upheaval, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(the__be_t5, the_churn_systemic_upheaval, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(the__be_t10, the_churn_systemic_upheaval, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_churn_systemic_upheaval, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
