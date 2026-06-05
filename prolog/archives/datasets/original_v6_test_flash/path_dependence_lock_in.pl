% ============================================================================
% CONSTRAINT STORY: path_dependence_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_path_dependence_lock_in, []).

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
 *   constraint_id: path_dependence_lock_in
 *   human_readable: The QWERTY Inertia Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint represents the structural inability to pivot to a
 *   superior standard because the accumulated network effects and sunk costs
 *   of the current (inferior) path make the cost of transition prohibitive.
 *   QWERTY's dominance persists despite arguably better alternatives like
 *   Dvorak because of the existing installed base, training, and
 *   manufacturing infrastructure.
 *
 * KEY AGENTS:
 *   - potential_dvorak_users: Primary target (powerless/trapped) — bears the cost of inferior ergonomics and efficiency
 *   - qwerty_keyboard_manufacturers: Primary beneficiary (institutional/arbitrage) — benefits from economies of scale and lack of disruption
 *   - qwerty_trained_typists: Secondary actor (moderate/constrained) — benefits from widespread adoption but is constrained by switching costs
 *   - ergonomic_efficiency: Analytical Observer - trapped by QWERTY even though it could be enhanced by adopting Dvorak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(path_dependence_lock_in, 0.65).
domain_priors:suppression_score(path_dependence_lock_in, 0.75).
domain_priors:theater_ratio(path_dependence_lock_in, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(path_dependence_lock_in, extractiveness, 0.65).
narrative_ontology:constraint_metric(path_dependence_lock_in, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(path_dependence_lock_in, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(path_dependence_lock_in, snare).
narrative_ontology:human_readable(path_dependence_lock_in, "The QWERTY Inertia Trap").
narrative_ontology:topic_domain(path_dependence_lock_in, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, qwerty_trained_typists).
narrative_ontology:constraint_victim(path_dependence_lock_in, potential_dvorak_users).
narrative_ontology:constraint_victim(path_dependence_lock_in, ergonomic_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Potential Dvorak Users - Trapped by the network effect and high switching costs, even if Dvorak offers ergonomic benefits.
constraint_indexing:constraint_classification(path_dependence_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: QWERTY Keyboard Manufacturers - Benefit from economies of scale and established production lines. No incentive to switch.
constraint_indexing:constraint_classification(path_dependence_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: QWERTY-trained typists - Constrained by needing to relearn a new layout, but also benefiting from current widespread adoption and job opportunities
constraint_indexing:constraint_classification(path_dependence_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4: Ergonomic efficiency - An abstract, difficult to organize victim is trapped by the QWERTY layout, where efficiency is considered in terms of the existing technology and not by any potential alternatives.
constraint_indexing:constraint_classification(path_dependence_lock_in, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(path_dependence_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(path_dependence_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(path_dependence_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(path_dependence_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(path_dependence_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The constraint extracts ergonomic and efficiency gains from users due to QWERTY's persistence. Suppression (0.75): High. Network effects and switching costs strongly suppress the adoption of superior alternatives. Theater Ratio (0.30): Low. Very little purely theatrical activity is present.
 *
 * PERSPECTIVAL GAP:
 *   Potential Dvorak users experience this as a snare because of the inability to switch, while QWERTY keyboard manufacturers see it as a rope due to a lack of disruptions. The typists face a Tangled Rope because of the initial constraint of learning something new, but the payoff of greater efficiency may be worth it to some.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship to the extraction flow. Potential Dvorak users, trapped by high switching costs, experience high d. QWERTY manufacturers, benefiting from the status quo, experience low d. Typists experience a mixed d due to both costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This is clearly not a pure coordination mechanism because there are ergonomic and efficiency costs being imposed on powerless agents, which excludes rope. There are high barriers to switching, so this is definitely a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_strength,
    'How strong are the network effects that lock users into QWERTY?',
    'Analyze user adoption rates of alternative layouts, controlled experiments',
    'Weak network effects may allow alternatives to gain traction. Strong network effects lock QWERTY in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_strength, empirical, 'Quantifying network effect strength').

omega_variable(
    switching_cost_quantification,
    'What are the true costs (time, money, effort) for users and institutions to switch?',
    'Surveys, simulations, cost-benefit analysis',
    'Low switching costs: easier to escape the lock-in. High switching costs: QWERTY remains dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_quantification, empirical, 'Measuring switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(path_dependence_lock_in, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(path_tr_t0, path_dependence_lock_in, theater_ratio, 0, 0.1).
narrative_ontology:measurement(path_tr_t50, path_dependence_lock_in, theater_ratio, 50, 0.2).
narrative_ontology:measurement(path_tr_t100, path_dependence_lock_in, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(path_be_t0, path_dependence_lock_in, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(path_be_t50, path_dependence_lock_in, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(path_be_t100, path_dependence_lock_in, base_extractiveness, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(path_dependence_lock_in, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
