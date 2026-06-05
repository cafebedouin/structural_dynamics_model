% ============================================================================
% CONSTRAINT STORY: harm_principle_liberty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harm_principle_liberty, []).

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
 *   constraint_id: harm_principle_liberty
 *   human_readable: Mill's Harm Principle as a Social Constraint
 *   domain: political/social
 *
 * SUMMARY:
 *   John Stuart Mill's "On Liberty" proposes the Harm Principle: society may
 *   only coerce an individual to prevent harm to others. This constraint
 *   story examines the Harm Principle as a social constraint, where society
 *   (often represented by the state) seeks to balance individual liberty with
 *   the protection of others. The Harm Principle is intended to act as a
 *   rope, coordinating social interactions and preventing undue harm.
 *   However, the ambiguity of 'harm' and the potential for biased enforcement
 *   can transform the principle into a tangled rope, where individual liberty
 *   is extracted under the guise of preventing harm, or even a snare,
 *   particularly for minority groups.
 *
 * KEY AGENTS:
 *   - Individual Liberty: Primary victim (powerless/trapped) - Liberty is reduced through the application of the harm principle.
 *   - Potential Harm Targets: Primary beneficiary (moderate/constrained) - Gain protection from harm.
 *   - The State: Enforcer (institutional/constrained) - Balances liberty with order, but risks overreach.
 *   - Minority Groups: Particularly vulnerable (powerless/trapped) - The principle may be misapplied to restrict their freedoms.
 *   - The Analytical Observer: Assesses intent (analytical/analytical) - Evaluates the principle's success in achieving its goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harm_principle_liberty, 0.55).
domain_priors:suppression_score(harm_principle_liberty, 0.65).
domain_priors:theater_ratio(harm_principle_liberty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harm_principle_liberty, extractiveness, 0.55).
narrative_ontology:constraint_metric(harm_principle_liberty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(harm_principle_liberty, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harm_principle_liberty, tangled_rope).
narrative_ontology:human_readable(harm_principle_liberty, "Mill's Harm Principle as a Social Constraint").
narrative_ontology:topic_domain(harm_principle_liberty, "political/social").

domain_priors:requires_active_enforcement(harm_principle_liberty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harm_principle_liberty, potential_harm_targets).
narrative_ontology:constraint_victim(harm_principle_liberty, individual_liberty).
narrative_ontology:constraint_victim(harm_principle_liberty, minority_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL LIBERTY (SNARE) - For individuals, especially minority groups, the Harm Principle can become a snare when 'harm' is broadly defined or selectively enforced. Exit is trapped because they are subject to societal norms and legal frameworks. They experience suppression when their actions, though harmless, are restricted due to social pressures.
constraint_indexing:constraint_classification(harm_principle_liberty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: POTENTIAL HARM TARGETS (TANGLED ROPE) - Experience both benefit and constraint from the Harm Principle. They benefit from the protection it provides against harm, but they are also constrained by the potential for it to be overused or misapplied, which could lead to unnecessary limitations on individual liberty. Exit is constrained by the need for societal order and the rule of law.
constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE (TANGLED ROPE) - The state benefits from the Harm Principle by having a justification for maintaining social order and preventing chaos. However, the state is constrained by the need to balance individual liberty with the prevention of harm, which can lead to complex legal and political challenges. Exit is constrained because the state cannot simply abandon its responsibility to protect its citizens. Extraction is present as the state exerts power over individuals under the guise of preventing harm.
constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) - Sees the Harm Principle as an attempt at establishing a rule for maximizing social welfare by allowing the greatest amount of individual liberty consistent with preventing harm to others. The analytical observer understands the principle is intended to provide a coordination mechanism for societal norms, although the interpretation of 'harm' remains contested.
constraint_indexing:constraint_classification(harm_principle_liberty, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_principle_liberty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harm_principle_liberty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harm_principle_liberty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(harm_principle_liberty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The Harm Principle extracts some individual liberty, as individuals are restricted from actions that might harm others. This extractiveness is considered moderate because the principle is intended to protect society as a whole. Suppression (0.65): High. The Harm Principle suppresses individual actions that are deemed harmful, even if those actions do not directly cause harm. This suppression is considered high because it restricts a wide range of potential actions. Theater ratio (0.30): Low. The Harm Principle is not purely performative. It has a practical effect on social behavior and legal frameworks, though the interpretation and enforcement can be subject to performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing views on what constitutes 'harm' and how the Harm Principle should be enforced. Individuals, particularly minority groups, may see the principle as a snare that restricts their freedoms unnecessarily. Potential harm targets may see it as a necessary protection, though also one which could be abused. The state may see it as a vital tool for maintaining social order, while the analytical observer seeks to evaluate its effectiveness objectively.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are potential harm targets, who benefit from the protection offered by the Harm Principle. Victims are individuals, particularly minority groups, whose liberty is restricted. The state benefits by maintaining social order but also bears a cost in terms of balancing competing interests. The directionality values reflect these relationships, with potential harm targets having a low 'd' value, victims having a high 'd' value, and the state having an intermediate 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The Harm Principle risks mislabeling actions that have negative impacts as 'harmful,' potentially justifying unnecessary restrictions on liberty. It is important to consider the scale of the impact, the intent of the action, and the potential for alternative solutions before invoking the Harm Principle. Actions should be labelled as harmful if they directly and demonstrably cause significant harm to others, and when reasonable alternatives have been considered. The principle should not be invoked to suppress actions that merely cause offense or discomfort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_harm,
    'What constitutes ''harm'' in the context of the Harm Principle?',
    'Legal precedent, philosophical debate, societal norms, and empirical studies of the impact of actions on others.',
    'A broad definition can lead to excessive restrictions on liberty, while a narrow definition can fail to adequately protect vulnerable individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_harm, conceptual, 'Ambiguity in the definition of harm').

omega_variable(
    enforcement_bias,
    'Is the enforcement of the Harm Principle applied fairly across all social groups?',
    'Statistical analysis of arrest rates, legal outcomes, and policy implementation across different demographic groups.',
    'Systematic bias in enforcement can lead to disproportionate restrictions on the liberty of minority groups, transforming the Harm Principle into a tool of oppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_bias, empirical, 'Potential for biased enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harm_principle_liberty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_tr_t0, harm_principle_liberty, theater_ratio, 0, 0.15).
narrative_ontology:measurement(harm_tr_t5, harm_principle_liberty, theater_ratio, 5, 0.25).
narrative_ontology:measurement(harm_tr_t10, harm_principle_liberty, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(harm_be_t0, harm_principle_liberty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(harm_be_t5, harm_principle_liberty, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(harm_be_t10, harm_principle_liberty, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harm_principle_liberty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
