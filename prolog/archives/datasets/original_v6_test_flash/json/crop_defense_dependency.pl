% ============================================================================
% CONSTRAINT STORY: crop_defense_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crop_defense_dependency, []).

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
 *   constraint_id: crop_defense_dependency
 *   human_readable: Dependency on External Pesticides due to Crop Biological Limitations
 *   domain: technological
 *
 * SUMMARY:
 *   Most staple food crops (e.g., corn, wheat, rice) have been bred for yield
 *   at the expense of their natural defense mechanisms, leading to a
 *   dependency on external pesticides. This creates a structural constraint
 *   where farmers and the environment are harmed, while pesticide
 *   manufacturers benefit. This dependency can be viewed from multiple
 *   perspectives. Research institutions are caught in the middle.
 *
 * KEY AGENTS:
 *   - Farmers: Primary target (powerless/trapped) - dependent on pesticides due to crop vulnerability.
 *   - Environment: Secondary target (powerless/trapped) - suffers from pesticide pollution and biodiversity loss.
 *   - Pesticide Manufacturers: Primary beneficiary (institutional/arbitrage) - profit from pesticide sales.
 *   - Agricultural Research Institutions: Secondary actor (organized/constrained) - constrained by funding but also benefit from related research.
 *   - Analytical Observer: Sees the system as a whole (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crop_defense_dependency, 0.6).
domain_priors:suppression_score(crop_defense_dependency, 0.7).
domain_priors:theater_ratio(crop_defense_dependency, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crop_defense_dependency, extractiveness, 0.6).
narrative_ontology:constraint_metric(crop_defense_dependency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(crop_defense_dependency, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crop_defense_dependency, tangled_rope).
narrative_ontology:human_readable(crop_defense_dependency, "Dependency on External Pesticides due to Crop Biological Limitations").
narrative_ontology:topic_domain(crop_defense_dependency, "technological").

domain_priors:requires_active_enforcement(crop_defense_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crop_defense_dependency, pesticide_manufacturers).
narrative_ontology:constraint_victim(crop_defense_dependency, farmers).
narrative_ontology:constraint_victim(crop_defense_dependency, environment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Farmers are trapped in a cycle of dependency, needing pesticides to protect vulnerable crops. Exit is difficult due to limited options for alternative farming methods or crop varieties.
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The environment is a victim of pesticide use, suffering from pollution and loss of biodiversity. It has no power or exit options.
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Pesticide manufacturers benefit from the continued demand for their products. They can arbitrage by developing new pesticides as pests develop resistance.
constraint_indexing:constraint_classification(crop_defense_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Agricultural research institutions face a mixed situation. They are constrained by funding priorities that often favor yield over resilience, but they also benefit from research grants related to pesticide development and resistance management. There is a coordination aspect, as they work to improve crop yields, but also an extraction aspect, as current research may perpetuate the pesticide dependency cycle
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees a tangled rope: a system of dependencies that benefits some while harming others. The observer recognizes the need to develop more resilient crops and sustainable farming methods, and that coordination between various actors could solve some but not all of the problems associated with pesticide dependence.
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crop_defense_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crop_defense_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crop_defense_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crop_defense_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(crop_defense_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Farmers are required to purchase pesticides to maintain crop yields, and the environment suffers from pesticide pollution. Suppression (0.70): High. Limited alternatives and regulatory capture suppress alternatives to pesticide usage. Theater Ratio (0.30): Low. While there is some discussion about integrated pest management and alternative practices, the system is primarily focused on direct pesticide application.
 *
 * PERSPECTIVAL GAP:
 *   Farmers and the environment experience the system as a snare due to the need for pesticides and resulting environmental damage. Pesticide manufacturers benefit from the system and see it as a rope. Agricultural research institutions see the entangled rope, as they are part of both the problem and potential solutions. The analytical observer sees that there are significant negative externalities to pesticide use which the various stakeholders can only manage through coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Farmers are victims because they are forced to purchase and apply pesticides. The environment is a victim because it is harmed by pesticide pollution. Pesticide manufacturers are beneficiaries because they profit from the sale of pesticides. Agricultural research institutions face a mixed incentive structure, as they contribute to both the problem and the solution.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is classified as a snare because the extraction and suppression are high, and the victims are largely trapped. While there are potential benefits from pesticide use, such as increased crop yields, these benefits are overshadowed by the negative externalities. The perspective of farmers, which is one of limited options, reflects the overall negative impact of pesticide dependence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crop_resilience_threshold,
    'What level of inherent pest resistance is achievable without sacrificing yield?',
    'Extensive breeding programs and genetic engineering research.',
    'If high resilience is achievable: the dependency weakens and becomes a tangled rope. If low: the dependency remains a strong snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crop_resilience_threshold, empirical, 'Defines the limit to which natural crop defense mechanisms can be improved.').

omega_variable(
    sustainable_farming_adoption_rate,
    'How quickly can sustainable farming practices be adopted at scale?',
    'Policy incentives, education programs, and demonstration projects.',
    'If adoption is rapid: the dependency weakens, and farmers gain mobility. If slow: farmers remain trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainable_farming_adoption_rate, preference, 'Defines the speed at which farmers switch to sustainable farming methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crop_defense_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crop_tr_t0, crop_defense_dependency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(crop_tr_t10, crop_defense_dependency, theater_ratio, 10, 0.3).
narrative_ontology:measurement(crop_tr_t20, crop_defense_dependency, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(crop_be_t0, crop_defense_dependency, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(crop_be_t10, crop_defense_dependency, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(crop_be_t20, crop_defense_dependency, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crop_defense_dependency, resource_allocation).
narrative_ontology:affects_constraint(crop_defense_dependency, agricultural_water_dependency).
narrative_ontology:affects_constraint(crop_defense_dependency, seed_monopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
