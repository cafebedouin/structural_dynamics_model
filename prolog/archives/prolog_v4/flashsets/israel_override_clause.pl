% ============================================================================
% CONSTRAINT STORY: israel_override_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_override_clause, []).

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
 *   constraint_id: israel_override_clause
 *   human_readable: The 61-Member Judicial Override Clause
 *   domain: political
 *
 * SUMMARY:
 *   The 61-Member Judicial Override Clause in Israel is a proposed mechanism
 *   to allow the Knesset, by a majority of 61 out of 120 MKs, to bypass
 *   Supreme Court rulings that invalidate legislation for violating Basic
 *   Laws. This clause represents a significant shift in the balance of power
 *   between the legislative and judicial branches, potentially impacting
 *   minority rights and the rule of law. The constraint is analyzed from the
 *   perspectives of different actors, revealing its multifaceted nature as
 *   both a potential tool for efficient governance and a threat to democratic
 *   principles.
 *
 * KEY AGENTS:
 *   - Ruling Coalition: Beneficiary (institutional/arbitrage) - Gains increased power and reduced judicial oversight.
 *   - Supreme Court: Victim (moderate/constrained) - Loses power to invalidate legislation, but retains interpretive role.
 *   - Minority Rights: Victim (powerless/trapped) - Increased vulnerability to the will of the majority.
 *   - Knesset: Beneficiary (institutional/arbitrage) - Overall power of the legislative branch is strengthened.
 *   - Rule of Law: Target (powerless/trapped) - Abstract principle, subject to interpretation and potential erosion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_override_clause, 0.65).
domain_priors:suppression_score(israel_override_clause, 0.7).
domain_priors:theater_ratio(israel_override_clause, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_override_clause, extractiveness, 0.65).
narrative_ontology:constraint_metric(israel_override_clause, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(israel_override_clause, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_override_clause, tangled_rope).
narrative_ontology:human_readable(israel_override_clause, "The 61-Member Judicial Override Clause").
narrative_ontology:topic_domain(israel_override_clause, "political").

domain_priors:requires_active_enforcement(israel_override_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_override_clause, ruling_coalition).
narrative_ontology:constraint_beneficiary(israel_override_clause, knesset).
narrative_ontology:constraint_victim(israel_override_clause, supreme_court).
narrative_ontology:constraint_victim(israel_override_clause, minority_rights).
narrative_ontology:constraint_victim(israel_override_clause, rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of minority rights: the override clause weakens protections against potential abuses by the majority, with limited recourse due to the override mechanism. They are trapped within the system and highly vulnerable.
constraint_indexing:constraint_classification(israel_override_clause, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of the Supreme Court: While not entirely powerless, the Court faces a constraint on its ability to check the Knesset's power. It benefits from its continued role in interpreting laws, but its power to invalidate legislation is significantly curtailed. The court is constrained rather than fully trapped due to public opinion and the need for basic law compliance by the Knesset.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the Knesset: The Knesset, particularly the ruling coalition, benefits from increased power and reduced judicial oversight, facilitating the implementation of its agenda. Members of the Knesset can arbitrage the situation by supporting legislation that aligns with their political goals, even if it is constitutionally questionable.
constraint_indexing:constraint_classification(israel_override_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees a tangled rope: a system that allows for quicker legislative action (coordination), but at the cost of potentially reduced checks and balances, and increased risk to minority rights (extraction). The long-term effects on the rule of law are uncertain.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_override_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_override_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_override_clause, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(israel_override_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Moderate-High. The clause extracts power from the judiciary and transfers it to the legislature. It also extracts protections for minority rights, making them more vulnerable to the will of the majority. Suppression (0.70): High. The override clause actively suppresses the Supreme Court's ability to act as a check on the Knesset's power. It also suppresses alternative legal interpretations that could protect minority rights. Theater Ratio (0.30): Low. The clause has a relatively low theater ratio, as its primary purpose is to directly alter the balance of power, rather than to create a superficial appearance of action.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the actors involved. The Knesset views the clause as a means to overcome judicial obstacles and implement its agenda, while the Supreme Court sees it as a threat to its independence and ability to uphold the law. Minority rights groups perceive the clause as a direct threat to their protections, while an analytical observer recognizes the potential for both benefits and risks depending on its implementation and long-term effects.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset benefits from reduced judicial oversight, granting it greater legislative authority. The Supreme Court is constrained, experiencing reduced power to check legislative actions. Minority rights are targeted, as their protections are weakened by the override clause, making them vulnerable to potential abuses of power by the majority. These declarations determine the directionality values and subsequent classifications from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the 61-member override clause is a tangled rope. It is not a pure snare, as it can facilitate governance by allowing the Knesset to implement its agenda more efficiently. However, it is not a pure rope, as it also poses a risk to minority rights and the rule of law. The various perspectives reflect the complex trade-offs inherent in this mechanism, highlighting the need for careful consideration of its potential consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_basic_laws,
    'How broadly will ''Basic Laws'' be interpreted, and how frequently will legislation be challenged under them?',
    'Tracking the number and types of laws challenged and overridden, as well as the reasoning provided by the Supreme Court and the Knesset.',
    'If interpreted narrowly and challenges are infrequent: the override clause has limited practical effect. If interpreted broadly and challenges are frequent: the override clause fundamentally alters the balance of power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_basic_laws, empirical, 'Breadth of Basic Law interpretation and frequency of challenges.').

omega_variable(
    public_opinion_backlash,
    'To what extent will public opinion react negatively to the use of the override clause, and will this affect the Knesset''s willingness to use it?',
    'Polling data and analysis of election results following high-profile uses of the override clause.',
    'Strong negative reaction: the Knesset may be hesitant to use the clause, limiting its impact. Weak or no reaction: the Knesset may use the clause more frequently, leading to significant changes in policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_opinion_backlash, empirical, 'Public opinion reaction to the use of the override clause.').

omega_variable(
    erosion_of_rule_of_law,
    'Does the override clause lead to a gradual erosion of the rule of law and the independence of the judiciary?',
    'Comparative analysis of judicial independence indices and expert assessments of the rule of law in Israel over time.',
    'Significant erosion: the democratic character of Israel is fundamentally altered. Limited erosion: the override clause is a temporary or exceptional measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(erosion_of_rule_of_law, conceptual, 'Potential erosion of the rule of law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_override_clause, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israel_override_clause, theater_ratio, 0, 0.2).
narrative_ontology:measurement(isra_tr_t5, israel_override_clause, theater_ratio, 5, 0.3).
narrative_ontology:measurement(isra_tr_t10, israel_override_clause, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israel_override_clause, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(isra_be_t5, israel_override_clause, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(isra_be_t10, israel_override_clause, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_override_clause, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
