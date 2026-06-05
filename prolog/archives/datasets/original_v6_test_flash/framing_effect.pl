% ============================================================================
% CONSTRAINT STORY: framing_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_framing_effect, []).

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
 *   constraint_id: framing_effect
 *   human_readable: The Framing Effect (as applied in choice architecture)
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   The framing effect, a cognitive bias where choices are influenced by how
 *   options are presented (gain vs. loss), is utilized in choice architecture
 *   to nudge individuals towards desired behaviors. This constraint explores
 *   the ethical implications and structural dynamics of this practice,
 *   examining how it can act as both a coordination mechanism and a form of
 *   extraction. This JSON defines the framing effect as it relates to choice
 *   architecture. Choice architects use this effect to influence decisions by
 *   individuals.
 *
 * KEY AGENTS:
 *   - Choice Architects: Primary beneficiaries (institutional/arbitrage) - design and implement framing strategies.
 *   - Policy Makers: Secondary beneficiaries (institutional/constrained) - use framing to achieve policy goals.
 *   - Marketing Professionals: Secondary beneficiaries (powerful/mobile) - employ framing to increase sales and influence consumer behavior.
 *   - Individual Autonomy: Primary victim (powerless/trapped) - abstract concept representing the individual's right to self-determination.
 *   - Informed Consent: Primary victim (powerless/trapped) - abstract concept representing the requirement for choices to be made with full knowledge of relevant information.
 *   - Rational Decision Making: Primary victim (powerless/trapped) - abstract concept impacted by the framing effect.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(framing_effect, 0.55).
domain_priors:suppression_score(framing_effect, 0.65).
domain_priors:theater_ratio(framing_effect, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(framing_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(framing_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(framing_effect, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(framing_effect, tangled_rope).
narrative_ontology:human_readable(framing_effect, "The Framing Effect (as applied in choice architecture)").
narrative_ontology:topic_domain(framing_effect, "social/political/technological").

domain_priors:requires_active_enforcement(framing_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(framing_effect, choice_architects).
narrative_ontology:constraint_beneficiary(framing_effect, policy_makers).
narrative_ontology:constraint_beneficiary(framing_effect, marketing_professionals).
narrative_ontology:constraint_victim(framing_effect, individual_autonomy).
narrative_ontology:constraint_victim(framing_effect, informed_consent).
narrative_ontology:constraint_victim(framing_effect, rational_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual, often lacking awareness of the framing effect, is trapped into making decisions that may not align with their true preferences. They are the primary target of extraction.
constraint_indexing:constraint_classification(framing_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Citizens who are somewhat aware of framing, but still susceptible due to cognitive limitations and information overload. They experience a mix of coordination (potentially better choices due to nudges) and extraction (manipulation).
constraint_indexing:constraint_classification(framing_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Choice architects benefit from the framing effect by achieving desired outcomes (e.g., increased organ donation rates). They experience the constraint as a coordination mechanism to guide behavior.
constraint_indexing:constraint_classification(framing_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer focused on the ideals of libertarian paternalism views the framing effect as a potentially useful tool but worries about it being abused. They see its use gradually turning into theater, as the true motivation behind it starts to shift from helping individuals make better choices to influencing them for other purposes.
constraint_indexing:constraint_classification(framing_effect, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(framing_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(framing_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(framing_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(framing_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(framing_effect, TR),
    TR >= 0.70.

:- end_tests(framing_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The framing effect subtly extracts from individual autonomy and rational decision-making by influencing choices. This extraction is not absolute, as individuals retain some agency, but the framing does skew their preferences. Suppression (0.65): Moderate to high. The effect's subtlety makes it difficult for individuals to detect and counteract, suppressing alternative choices that might align better with their true preferences. Theater Ratio (0.30): Low. There is a functional element of trying to 'help' individuals by presenting choices in certain ways, such as increased organ donation by making it the default. However, that can easily decay if bad actors start to exploit it.
 *
 * PERSPECTIVAL GAP:
 *   The individual experiences the framing effect as a snare, being subtly manipulated into choices. Choice architects view it as a rope, a coordination mechanism to guide behavior towards desired outcomes. The partially aware citizen experiences the framing effect as a tangled rope, acknowledging it as manipulation, but also recognizing the potential for helpful nudges. An analytical observer sees its use potentially turning into theater, as the true motivation behind it shifts from helping individuals make better choices to influencing them for other purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (choice architects, policy makers) experience low directionality (d), seeing the effect as a tool to improve outcomes. Victims (individual autonomy, rational decision-making) experience high directionality, as their choices are influenced against their better judgment. A moderate directionality reflects a mixed experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by acknowledging the multifaceted nature of the framing effect. It's not simply a snare or a rope, but a tangled rope that can be used for both beneficial and manipulative purposes. The classification depends on the perspective, with the individual experiencing it as a snare, while the choice architect experiences it as a rope. It requires a deeper understanding of the context, goals, and long-term effects to classify it appropriately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_threshold_manipulation,
    'What level of framing constitutes unethical manipulation?',
    'Develop ethical guidelines through public discourse and expert consensus on acceptable framing practices.',
    'Determines whether the framing effect is a beneficial nudge or a harmful snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_threshold_manipulation, preference, 'Defining the ethical boundaries of framing techniques.').

omega_variable(
    long_term_effects_autonomy,
    'Does long-term exposure to framing erode individual autonomy and critical thinking?',
    'Conduct longitudinal studies on the cognitive and behavioral impacts of repeated framing exposure.',
    'Determines the civilizational-scale impact on individual agency and informed consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_effects_autonomy, empirical, 'Assessing the long-term effects of framing on autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(framing_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fram_tr_t0, framing_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fram_tr_t5, framing_effect, theater_ratio, 5, 0.2).
narrative_ontology:measurement(fram_tr_t10, framing_effect, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(fram_be_t0, framing_effect, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fram_be_t5, framing_effect, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fram_be_t10, framing_effect, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(framing_effect, information_standard).
narrative_ontology:affects_constraint(framing_effect, confirmation_bias).
narrative_ontology:affects_constraint(framing_effect, availability_heuristic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
