% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanlons_razor, []).

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
 *   constraint_id: hanlons_razor
 *   human_readable: Hanlon's Razor ("Never attribute to malice that which is adequately explained by stupidity.")
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   Hanlon's Razor, a heuristic that advises against attributing to malice
 *   what can be adequately explained by stupidity, serves as a constraint on
 *   how we interpret others' actions. While it can promote efficiency in
 *   social modeling and prevent unnecessary conflict, it also risks
 *   minimizing the impact of harmful behavior and overlooking genuine
 *   malicious intent. This constraint is structurally analyzed from diverse
 *   perspectives. How individuals and institutions engage with this heuristic
 *   shapes social interactions and the enforcement of ethical standards.
 *
 * KEY AGENTS:
 *   - Accidental Offenders: Beneficiaries (moderate/mobile) - benefit from reduced culpability.
 *   - Efficient Social Modeling: Beneficiaries (institutional/arbitrage) - the heuristic streamlines social interpretations
 *   - Victims of Offense: Victims (powerless/trapped) - their experience may be dismissed.
 *   - Accurate Threat Assessment: Victims (analytical/analytical) - proper threat assessment can be undermined by applying the razor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanlons_razor, 0.3).
domain_priors:suppression_score(hanlons_razor, 0.4).
domain_priors:theater_ratio(hanlons_razor, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.3).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hanlons_razor, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanlons_razor, tangled_rope).
narrative_ontology:human_readable(hanlons_razor, "Hanlon's Razor (\"Never attribute to malice that which is adequately explained by stupidity.\")").
narrative_ontology:topic_domain(hanlons_razor, "social/cognitive").

domain_priors:requires_active_enforcement(hanlons_razor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanlons_razor, accidental_offenders).
narrative_ontology:constraint_beneficiary(hanlons_razor, efficient_social_modeling).
narrative_ontology:constraint_victim(hanlons_razor, victims_of_offense).
narrative_ontology:constraint_victim(hanlons_razor, accurate_threat_assessment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The victim of the offense often experiences Hanlon's Razor as a snare because it can minimize the impact or intentionality of the harm done to them. They are trapped in the situation and may not have the power to challenge the attribution of stupidity over malice.
constraint_indexing:constraint_classification(hanlons_razor, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: The legal system may see Hanlon's Razor as a rope. While individual intentions are hard to prove, it coordinates the system toward evidence and objective outcomes, preventing accusations of malice without substantial proof. It benefits by reducing the cost of investigations into intent.
constraint_indexing:constraint_classification(hanlons_razor, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: From an analytical viewpoint, Hanlon's Razor is a tangled rope. It facilitates efficient social modeling by promoting simpler explanations, yet it also extracts from accurate threat assessment by downplaying genuine malice. It both helps and hinders societal understanding of actions and intentions.
constraint_indexing:constraint_classification(hanlons_razor, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: For the 'accidental offender,' Hanlon's Razor provides a scaffold, offering temporary support in mitigating the consequences of their actions. This is especially true if they can convincingly demonstrate a lack of malicious intent. However, this support diminishes if the stupidity is chronic or egregious.
constraint_indexing:constraint_classification(hanlons_razor, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanlons_razor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(hanlons_razor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.30 Moderate, as there is some cost to accurate threat assessment. Suppression: 0.40. The heuristic limits full assessment. Theater Ratio: 0.20 Theatrical performances are reduced as intent is not directly investigated. The claimed type is Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is apparent between the victim, who experiences a snare due to minimized harm, and the legal system, which sees it as a useful constraint and a rope that minimizes over-prosecution based on assumptions of malice. The analytical observer recognizes the trade-off between social efficiency and accurate assessment of threats.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality arises from structural position. The victim, lacking power and trapped in their circumstance, experiences higher extraction from the application of the razor. The legal system, an institutional actor with arbitrage options, benefits from reduced costs and streamlined processes. The accidental offender benefits as they are not automatically targeted with harsh reprimands.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stupidity_vs_malice,
    'What is the threshold for distinguishing genuine stupidity from masked malice?',
    'Behavioral pattern analysis, historical context evaluation, and consideration of incentives.',
    'Overestimation of stupidity leads to increased vulnerability; underestimation leads to unwarranted accusations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stupidity_vs_malice, empirical, 'The ambiguity in differentiating stupidity from malice.').

omega_variable(
    contextual_factors,
    'How should contextual factors (power dynamics, historical grievances) influence the application of Hanlon''s Razor?',
    'Development of a weighting system that accounts for relevant contextual variables.',
    'Failure to account for context could lead to systematic bias in the application of Hanlon''s Razor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_factors, conceptual, 'The role of contextual variables.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanlons_razor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanl_tr_t0, hanlons_razor, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hanl_tr_t5, hanlons_razor, theater_ratio, 5, 0.15).
narrative_ontology:measurement(hanl_tr_t10, hanlons_razor, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(hanl_be_t0, hanlons_razor, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hanl_be_t5, hanlons_razor, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(hanl_be_t10, hanlons_razor, base_extractiveness, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanlons_razor, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
