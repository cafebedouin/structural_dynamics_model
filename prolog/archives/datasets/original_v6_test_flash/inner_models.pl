% ============================================================================
% CONSTRAINT STORY: inner_models
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inner_models, []).

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
 *   constraint_id: inner_models
 *   human_readable: Confirmation Bias in Inner Model Updating
 *   domain: social
 *
 * SUMMARY:
 *   Confirmation bias in inner model updating describes the tendency of
 *   individuals to selectively gather, interpret, and remember information
 *   that confirms their existing beliefs or values. This manifests in echo
 *   chambers and filter bubbles, limiting exposure to diverse perspectives.
 *   The presence of algorithmic recommendation systems exacerbates this bias,
 *   creating challenges for epistemic reliability.
 *
 * KEY AGENTS:
 *   - Challenger Narratives: Primary victim (powerless/trapped) — faces suppression and invalidation
 *   - Dominant Narrative Holders: Primary beneficiary (institutional/arbitrage) — gains greater social cohesion and power
 *   - Epistemic Institutions: Secondary victim (moderate/constrained) — expends resources to correct bias
 *   - Out-Group Members: Secondary Victim (moderate/constrained) - Face systematic discrimination for their beliefs and values.
 *   - In-Group Members: Secondary Beneficiary (moderate/constrained) - Social cohesion and support.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inner_models, 0.55).
domain_priors:suppression_score(inner_models, 0.65).
domain_priors:theater_ratio(inner_models, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_models, extractiveness, 0.55).
narrative_ontology:constraint_metric(inner_models, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(inner_models, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inner_models, tangled_rope).
narrative_ontology:human_readable(inner_models, "Confirmation Bias in Inner Model Updating").
narrative_ontology:topic_domain(inner_models, "social").

domain_priors:requires_active_enforcement(inner_models).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inner_models, dominant_narrative_holders).
narrative_ontology:constraint_beneficiary(inner_models, in_group_members).
narrative_ontology:constraint_victim(inner_models, challenger_narratives).
narrative_ontology:constraint_victim(inner_models, out_group_members).
narrative_ontology:constraint_victim(inner_models, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An individual holding a challenger narrative finds themselves trapped within a social structure dominated by the prevailing narrative. They are actively suppressed in expressing dissenting viewpoints, and have limited ability to influence or exit the situation.
constraint_indexing:constraint_classification(inner_models, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% A dissident community that challenges the dominant narrative. This community faces suppression from the dominant narrative holders, who may act to limit their influence and prevent their narrative from gaining traction. They benefit from the narrative, if any, by its explanatory value in their setting, and are constrained because they cannot easily exit from the system without cost.
constraint_indexing:constraint_classification(inner_models, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Individuals or groups who benefit from the confirmation bias that reinforces their existing beliefs. The inner model updating is a rope as they arbitrage their position and the information landscape. The benefits accrue from greater social cohesion and continued power.
constraint_indexing:constraint_classification(inner_models, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The epistemic commons is both influenced by and tries to correct this confirmation bias. As such, scientific journals and similar institutions are extracted from, because they are required to expend resources to correct the confirmation bias.
constraint_indexing:constraint_classification(inner_models, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The traditional media ecosystem maintains theatrical support for the dominant narrative to retain their audience's attention. As such, their support is largely performative, as there is little true conviction behind their claims.
constraint_indexing:constraint_classification(inner_models, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees a mixed dynamic of extraction and coordination. The confirmation bias problem is an issue of epistemic corruption.
constraint_indexing:constraint_classification(inner_models, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_models_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inner_models, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inner_models, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inner_models, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inner_models, TR),
    TR >= 0.70.

:- end_tests(inner_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The base extractiveness (0.55) represents the degree to which the suppression of alternative narratives limits access to diverse perspectives and creates social polarization. Suppression: Suppression (0.65) is high, due to the way that information is shared through filter bubbles. Theatrical Ratio (0.3) is related to efforts for performative actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fact_check_threshold,
    'What is the threshold for fact-checking to be considered effective?',
    'Empirical studies comparing the effectiveness of different fact-checking approaches.',
    'Determines the required investment in fact-checking to counter confirmation bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fact_check_threshold, empirical, 'Defines the standard for fact-checking.').

omega_variable(
    polarization_strength,
    'To what extent does this bias contribute to social polarization?',
    'Measuring echo chamber formation and the impact of counter-narratives.',
    'Informs strategies for reducing division and fostering constructive dialogue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polarization_strength, empirical, 'Quantifies the effect of confirmation bias on polarization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inner_models, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inne_tr_t0, inner_models, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inne_tr_t5, inner_models, theater_ratio, 5, 0.25).
narrative_ontology:measurement(inne_tr_t10, inner_models, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(inne_be_t0, inner_models, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inne_be_t5, inner_models, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(inne_be_t10, inner_models, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inner_models, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
