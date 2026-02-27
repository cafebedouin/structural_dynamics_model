% ============================================================================
% CONSTRAINT STORY: ergot_grain_poisoning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergot_grain_poisoning, []).

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
 *   constraint_id: ergot_grain_poisoning
 *   human_readable: The Ergot/Famine Dilemma
 *   domain: social/technological/biological
 *
 * SUMMARY:
 *   Ergot (Claviceps purpurea) is a fungal parasite of grains—primarily
 *   rye—that causes hallucinations, convulsions, and gangrenous death (St.
 *   Anthony's Fire). In times of famine, contaminated grain may be the only
 *   available food source, creating a tragic dilemma: consume the ergot and
 *   risk poisoning, or starve. This situation exemplifies a tangled rope
 *   constraint, where individuals are trapped between two undesirable
 *   outcomes, but some benefit from hoarding the grain. Improved food safety
 *   practices and awareness have decreased its prevalence in modern times,
 *   but it remains a threat in famine-stricken regions or areas with limited
 *   resources.
 *
 * KEY AGENTS:
 *   - Grain Consumers: Primary victim (powerless/trapped) - Forced to choose between starvation and poisoning.
 *   - Village Elder: Secondary actor (moderate/constrained) - Must decide whether to distribute the grain, balancing the risks.
 *   - Untreated Populations: Victims of outbreaks due to lack of modern medicine and knowledge.
 *   - Hoarders: Beneficiaries who extract value by controlling access to the limited resource.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergot_grain_poisoning, 0.6).
domain_priors:suppression_score(ergot_grain_poisoning, 0.7).
domain_priors:theater_ratio(ergot_grain_poisoning, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergot_grain_poisoning, extractiveness, 0.6).
narrative_ontology:constraint_metric(ergot_grain_poisoning, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ergot_grain_poisoning, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergot_grain_poisoning, tangled_rope).
narrative_ontology:human_readable(ergot_grain_poisoning, "The Ergot/Famine Dilemma").
narrative_ontology:topic_domain(ergot_grain_poisoning, "social/technological/biological").

domain_priors:requires_active_enforcement(ergot_grain_poisoning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, hoarders).
narrative_ontology:constraint_victim(ergot_grain_poisoning, grain_consumers).
narrative_ontology:constraint_victim(ergot_grain_poisoning, untreated_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STARVING PEASANT (SNARE) - With no other food source available, the peasant is trapped and forced to consume the contaminated grain, suffering the consequences of ergot poisoning. This perspective represents pure extraction, as the peasant receives no benefit and bears the full cost.
constraint_indexing:constraint_classification(ergot_grain_poisoning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE ELDER (TANGLED ROPE) - The village elder faces the dilemma of whether to distribute the contaminated grain to prevent starvation or to withhold it and risk immediate death. They are constrained by limited options, but also have some power to decide. They experience a mix of extraction (the burden of decision and the consequences) and coordination (managing the distribution, weighing risks).
constraint_indexing:constraint_classification(ergot_grain_poisoning, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE HISTORICAL RECORD (PITON) - Ergot poisoning outbreaks have been linked to historical events like the Salem Witch Trials. With improved detection and food safety, the societal fear is less salient. However, lack of awareness in some communities can lead to a delayed recognition of the problem, leading to prolonged suffering until proper diagnosis and treatment. The theater is the continued fear despite rarity.
constraint_indexing:constraint_classification(ergot_grain_poisoning, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - The observer sees the complex interplay between the biological constraint (ergot), the social constraint (famine), and the technological constraint (food safety measures). The analytical observer identifies the dilemma as a tangled rope, where actions to mitigate one problem can exacerbate another. Modern agricultural practices can minimize ergot contamination, but in situations where food security is extremely compromised or awareness is very low, it still poses a deadly risk.
constraint_indexing:constraint_classification(ergot_grain_poisoning, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergot_grain_poisoning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergot_grain_poisoning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergot_grain_poisoning, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergot_grain_poisoning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergot_grain_poisoning, TR),
    TR >= 0.70.

:- end_tests(ergot_grain_poisoning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The consumption of ergot-contaminated grain leads to severe health consequences and potentially death. The benefits are limited to mere survival in the short term for consumers, and control for hoarders. Suppression (0.7): High. The lack of alternative food sources and effective treatments leaves individuals with little to no agency. The choices are very constrained. Theater Ratio (0.75): High. The historical record shows a continued fear of ergot poisoning despite its relative rarity in modern times, representing a performative aspect of societal concern.
 *
 * PERSPECTIVAL GAP:
 *   The starving peasant experiences the constraint as a pure snare, with no viable exit option. The village elder faces a tangled rope, with the burden of decision-making and the consequences of either choice. The analytical observer frames this a problem where different perspectives each have a component of the 'true' experience, whether that be death, starvation, or the weight of a difficult decision.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's power and exit options. The peasant is trapped and powerless, bearing the full cost (high d). The village elder has some agency, leading to a lower but still significant experienced extractiveness. Hoarders benefit by controlling access to the limited resource, giving them a low d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    famine_severity_threshold,
    'At what point does the risk of starvation outweigh the risk of ergot poisoning?',
    'Empirical data on mortality rates from starvation vs. ergotism in past outbreaks.',
    'Determines whether the dilemma is primarily a coordination problem (managing risks) or a pure extraction snare (forced choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(famine_severity_threshold, empirical, 'The threshold at which famine risk outweighs ergot poisoning risk.').

omega_variable(
    detection_technology_availability,
    'How readily available and effectively deployed are ergot detection technologies in famine-stricken regions?',
    'Assessment of existing infrastructure and resource allocation for food safety.',
    'High availability shifts classification toward a coordination problem (rope). Low availability reinforces the snare dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_technology_availability, empirical, 'Availability of ergot detection technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergot_grain_poisoning, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergot_grain_poisoning, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ergo_tr_t50, ergot_grain_poisoning, theater_ratio, 50, 0.7).
narrative_ontology:measurement(ergo_tr_t100, ergot_grain_poisoning, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergot_grain_poisoning, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ergo_be_t50, ergot_grain_poisoning, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(ergo_be_t100, ergot_grain_poisoning, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergot_grain_poisoning, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
