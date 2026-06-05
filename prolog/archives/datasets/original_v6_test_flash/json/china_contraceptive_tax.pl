% ============================================================================
% CONSTRAINT STORY: china_contraceptive_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_contraceptive_tax, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: china_contraceptive_tax
 *   human_readable: China's Differential Tax on Contraceptives
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Chinese government's differential tax on contraceptives, imposing a
 *   17% VAT on condoms while exempting or subsidizing state-preferred methods
 *   such as IUDs and sterilization, creates a structural constraint that
 *   limits reproductive freedom and consumer choice. This policy favors
 *   government-approved contraceptive options, influencing individual
 *   decisions through economic incentives.
 *
 * KEY AGENTS:
 *   - Consumers of Condoms: Primary target (powerless/trapped) – face increased costs, limiting access.
 *   - Condom Manufacturers: Secondary target (moderate/constrained) – reduced competitiveness due to the tax.
 *   - State-Preferred Contraceptive Manufacturers: Primary beneficiary (institutional/constrained) – benefit from tax exemptions and subsidies.
 *   - Family Planning Bureau: Secondary beneficiary (institutional/arbitrage) – benefits from increased use of state-preferred methods.
 *   - Reproductive Freedom: Victim (powerless/trapped) - choice is restricted by policy intervention.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_contraceptive_tax, 0.65).
domain_priors:suppression_score(china_contraceptive_tax, 0.7).
domain_priors:theater_ratio(china_contraceptive_tax, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_contraceptive_tax, extractiveness, 0.65).
narrative_ontology:constraint_metric(china_contraceptive_tax, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(china_contraceptive_tax, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_contraceptive_tax, tangled_rope).
narrative_ontology:human_readable(china_contraceptive_tax, "China's Differential Tax on Contraceptives").
narrative_ontology:topic_domain(china_contraceptive_tax, "economic/political").

domain_priors:requires_active_enforcement(china_contraceptive_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, state_preferred_contraceptive_manufacturers).
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, family_planning_bureau).
narrative_ontology:constraint_victim(china_contraceptive_tax, consumers_of_condoms).
narrative_ontology:constraint_victim(china_contraceptive_tax, condom_manufacturers).
narrative_ontology:constraint_victim(china_contraceptive_tax, reproductive_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Consumers, especially those with limited resources, are trapped by the increased cost of condoms due to the tax, limiting their access to this method of contraception.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Condom manufacturers are constrained by the tax, which reduces their competitiveness compared to manufacturers of state-preferred contraceptives.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The Family Planning Bureau benefits from the tax as it incentivizes the use of state-preferred methods, aligning with the government's population control policies. The bureau is able to shift resources away from condom distribution towards other methods.
constraint_indexing:constraint_classification(china_contraceptive_tax, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% State-preferred contraceptive manufacturers benefit from tax exemptions and subsidies. However, they are also constrained by the state's control over the market and policy direction.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Reproductive freedom is suppressed. Individuals lack full agency to select preferred contraceptive methods without state intervention. Choice of contraceptive method is constrained by policy.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% From an analytical perspective, the tax represents a tangled rope. It extracts from condom users and manufacturers, limiting reproductive choices, while also incentivizing state-preferred methods and providing revenue for the Family Planning Bureau.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_contraceptive_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_contraceptive_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_contraceptive_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): The tax extracts value from condom users and manufacturers by increasing the cost of condoms and decreasing competitiveness. Suppression (0.70): The policy suppresses access to condoms relative to state-preferred methods by making the former more expensive. Theater Ratio (0.30): The policy does not rely heavily on performative aspects, as the main effect is a direct economic incentive.
 *
 * PERSPECTIVAL GAP:
 *   Consumers experience the tax as a snare, limiting their access to condoms. State-preferred contraceptive manufacturers experience the tax as a coordination mechanism, as it benefits them. An analytical observer would see the tax as a tangled rope, as it extracts value from some while benefiting others, all while serving a complex population control objective.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers of condoms are considered powerless because they are trapped by the tax and its effect on cost, limiting access. Manufacturers are constrained by the effect on competitiveness. The family planning bureau benefits from increased use of state-preferred methods, aligning with the state's population goals. The overall policy constrains reproductive freedom.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_effectiveness,
    'How effective is the tax in achieving the government''s population control goals?',
    'Statistical analysis of contraceptive usage rates and birth rates before and after the implementation of the tax.',
    'If highly effective, the justification for the tax is strengthened. If ineffective, the tax may be deemed unnecessary and harmful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_effectiveness, empirical, 'Assesses the effectiveness of the tax on population control.').

omega_variable(
    unintended_consequences,
    'What are the unintended consequences of the tax, such as increased rates of STIs or unwanted pregnancies?',
    'Public health surveys and statistical analysis of STI and pregnancy rates.',
    'If negative consequences are significant, the tax may be deemed harmful and require revision or repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences, empirical, 'Evaluates the unintended public health impacts.').

omega_variable(
    ethical_considerations,
    'What are the ethical implications of the tax, particularly regarding reproductive freedom and access to healthcare?',
    'Ethical analysis and public debate.',
    'The tax may be deemed unethical if it infringes on individual rights or exacerbates health inequalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_considerations, conceptual, 'Considers the ethical dimensions of the tax.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_contraceptive_tax, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_contraceptive_tax, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chin_tr_t5, china_contraceptive_tax, theater_ratio, 5, 0.25).
narrative_ontology:measurement(chin_tr_t10, china_contraceptive_tax, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_contraceptive_tax, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(chin_be_t5, china_contraceptive_tax, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(chin_be_t10, china_contraceptive_tax, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
