% ============================================================================
% CONSTRAINT STORY: bedouin_sedentary_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bedouin_sedentary_transition, []).

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
 *   constraint_id: bedouin_sedentary_transition
 *   human_readable: The Bedouin Sedentary Transition
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The Bedouin sedentary transition represents the shift of nomadic Bedouin
 *   tribes from a resilient, millennium-old pastoral lifestyle in the
 *   hyper-arid Empty Quarter to a dependency-based existence in settled
 *   communities. This transition is driven by factors such as oil discovery,
 *   government policies, and technological advancements. It has significant
 *   social, economic, and cultural consequences for the Bedouin people.
 *
 * KEY AGENTS:
 *   - Bedouin Tribes: Primary target (powerless/trapped) - experiencing the loss of traditional lifestyle and increased dependency.
 *   - National Governments: Primary beneficiary (institutional/arbitrage) - gaining control over population, workforce, and resources.
 *   - Oil Companies: Secondary beneficiary (institutional/arbitrage) - requiring a settled workforce for oil extraction and production.
 *   - Second Generation Bedouin: (moderate/constrained) - experiencing mixed benefits and drawbacks of the transition.
 *   - Traditional Bedouin Culture: A victim experiencing the cultural impacts of the transition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bedouin_sedentary_transition, 0.7).
domain_priors:suppression_score(bedouin_sedentary_transition, 0.8).
domain_priors:theater_ratio(bedouin_sedentary_transition, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bedouin_sedentary_transition, extractiveness, 0.7).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bedouin_sedentary_transition, tangled_rope).
narrative_ontology:human_readable(bedouin_sedentary_transition, "The Bedouin Sedentary Transition").
narrative_ontology:topic_domain(bedouin_sedentary_transition, "social/economic/technological").

domain_priors:requires_active_enforcement(bedouin_sedentary_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, national_governments).
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, oil_companies).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, bedouin_tribes).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, traditional_bedouin_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Bedouin tribes experience this transition as a snare. Their traditional nomadic lifestyle is suppressed, and they are increasingly dependent on the government for resources. Exit options are limited due to loss of traditional skills and resources.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% National governments benefit from the transition as settled populations are easier to administer and control. It also provides a workforce for the oil industry and other sectors. The transition is seen as a form of national development and integration.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The second generation Bedouin face a tangled rope situation. They may benefit from access to education and healthcare in settled communities, but they also experience a loss of cultural identity and traditional skills. They are constrained by limited economic opportunities and dependence on government support.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The analytical observer sees the transition as a complex process with both positive and negative consequences. It represents a shift from a sustainable, traditional lifestyle to a dependence-based existence. The long-term impacts on social cohesion, cultural preservation, and environmental sustainability are uncertain.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bedouin_sedentary_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bedouin_sedentary_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bedouin_sedentary_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70): High. The Bedouin are losing their traditional lifestyle and becoming increasingly dependent on external resources. Suppression (0.80): High. Government policies and economic incentives are suppressing the nomadic lifestyle. Theater Ratio (0.60): Moderate. Government programs aimed at integrating Bedouin communities into the national fabric often have performative aspects. The transition is a tangled rope as it is not purely extractive and offers some benefits to the bedouin in terms of access to services and integration in to the national economy but at a cost to cultural identity and traditional lifeways.
 *
 * PERSPECTIVAL GAP:
 *   The Bedouin tribes experience the transition as a snare due to their loss of autonomy and increasing dependency. National governments view it as a rope, enabling better administration and resource control. Second-generation Bedouin experience the transition as a tangled rope, balancing benefits and drawbacks. An analytical observer sees a complex transition with uncertain long-term consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the structural relationships between the agents and the constraint. National governments and oil companies benefit from the transition and have arbitrage exit options, resulting in low directionality values. The Bedouin tribes are the targets of the transition and have limited exit options, resulting in high directionality values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_preservation,
    'Can traditional Bedouin culture be preserved in settled communities?',
    'Assessment of cultural programs, language preservation efforts, and intergenerational transmission of knowledge.',
    'If cultural preservation efforts are successful, the negative impacts of the transition can be mitigated. If not, the Bedouin culture may be lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_preservation, empirical, 'The long-term impact of cultural preservation efforts on Bedouin identity and traditions.').

omega_variable(
    economic_sustainability,
    'Can settled Bedouin communities achieve economic self-sufficiency?',
    'Analysis of employment rates, income levels, and access to economic opportunities in settled communities.',
    'If settled communities can achieve economic self-sufficiency, they will be less dependent on government support. If not, they may experience poverty and social unrest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_sustainability, empirical, 'The potential for economic self-sufficiency in settled Bedouin communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bedouin_sedentary_transition, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bedo_tr_t0, bedouin_sedentary_transition, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bedo_tr_t30, bedouin_sedentary_transition, theater_ratio, 30, 0.4).
narrative_ontology:measurement(bedo_tr_t60, bedouin_sedentary_transition, theater_ratio, 60, 0.6).

% Extraction over time
narrative_ontology:measurement(bedo_be_t0, bedouin_sedentary_transition, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bedo_be_t30, bedouin_sedentary_transition, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(bedo_be_t60, bedouin_sedentary_transition, base_extractiveness, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bedouin_sedentary_transition, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
