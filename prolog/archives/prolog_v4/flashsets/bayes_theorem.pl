% ============================================================================
% CONSTRAINT STORY: bayes_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bayes_theorem, []).

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
 *   constraint_id: bayes_theorem
 *   human_readable: Bayesian Reasoning Bias (Base Rate Neglect)
 *   domain: social
 *
 * SUMMARY:
 *   This constraint describes the cognitive bias wherein individuals
 *   systematically underutilize or ignore base rate information when making
 *   judgments under uncertainty, even when provided with relevant conditional
 *   probabilities. This bias can lead to suboptimal decisions and is often
 *   exploited by those seeking to influence others.
 *
 * KEY AGENTS:
 *   - Individuals Making Decisions: Primary victim (powerless/trapped) – Subject to the bias and its consequences.
 *   - Manipulators: Primary beneficiary (powerful/arbitrage) – Exploit the bias for their advantage.
 *   - Society as a Whole: Secondary victim (moderate/constrained) – Suffers from the aggregate effects of biased decisions.
 *   - Statistical Education Institutions: Beneficiary (institutional/arbitrage) - Provide tools to mitigate the impact of the bias
 *   - Informed Citizen: Victim with some awareness (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bayes_theorem, 0.55).
domain_priors:suppression_score(bayes_theorem, 0.65).
domain_priors:theater_ratio(bayes_theorem, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bayes_theorem, extractiveness, 0.55).
narrative_ontology:constraint_metric(bayes_theorem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bayes_theorem, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bayes_theorem, tangled_rope).
narrative_ontology:human_readable(bayes_theorem, "Bayesian Reasoning Bias (Base Rate Neglect)").
narrative_ontology:topic_domain(bayes_theorem, "social").

domain_priors:requires_active_enforcement(bayes_theorem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bayes_theorem, manipulators).
narrative_ontology:constraint_beneficiary(bayes_theorem, persuaders).
narrative_ontology:constraint_victim(bayes_theorem, individuals_making_decisions).
narrative_ontology:constraint_victim(bayes_theorem, society_as_a_whole).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual, often lacking formal statistical training, is trapped in their cognitive limitations and unable to properly weigh base rates, leading to suboptimal decisions. They are the target of extraction by those who can manipulate the bias.
constraint_indexing:constraint_classification(bayes_theorem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Institutions providing statistical education benefit from the bias by having a continuous stream of students seeking to overcome this limitation. They are arbitrageurs who benefit from the persistent need for their services.
constraint_indexing:constraint_classification(bayes_theorem, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes the mixed effects of this bias. It highlights the human capacity for error, while also creating opportunities for statistical literacy and improved decision-making processes over civilizational time scales.
constraint_indexing:constraint_classification(bayes_theorem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The informed citizen is aware of the bias but is still constrained by the difficulty of consistently applying Bayesian reasoning in real-world scenarios. Benefits slightly from increased awareness but still vulnerable.
constraint_indexing:constraint_classification(bayes_theorem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The manipulator (e.g. advertiser, political campaigner) actively exploits the bias to influence individuals, seeing this as a pure coordination tool to increase their effectiveness.
constraint_indexing:constraint_classification(bayes_theorem, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bayes_theorem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bayes_theorem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bayes_theorem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bayes_theorem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bayes_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to High. Significant cost to individuals making poor decisions and society bearing the aggregate consequences. Suppression (0.65): High. Difficult to overcome due to ingrained cognitive habits and intentional manipulation. Theater Ratio (0.30): Low. While some educational efforts exist, most interactions exploiting the base rate fallacy are direct and functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The individual, as a powerless agent, experiences the bias as a snare, leading to poor decisions. The manipulator views it as a coordination tool (rope) to achieve their objectives. The analytical observer recognizes the mixed nature of the constraint (tangled rope), highlighting the cognitive limitations while acknowledging potential remedies. The informed citizen also experiences the problem as a tangled rope, because awareness is not enough to guarantee correct judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship. The individual making decisions is the target, and their trapped exit option leads to high directionality. The manipulator benefits, with the arbitrage option and powerful status resulting in low directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the exploitation of this bias as a purely coordinated activity because it acknowledges the individuals who are being harmed by the decisions they are making under the influence of this bias. The presence of victims is key to labeling it as a Tangled Rope rather than just a Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_cognitive_capacity,
    'To what extent is the base rate fallacy an irreducible feature of human cognition versus a remediable skill?',
    'Longitudinal studies of statistical training interventions; neurocognitive research on reasoning processes',
    'If irreducible: Snare classification strengthened. If remediable: Tangled Rope classification with potential for Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_cognitive_capacity, empirical, 'The extent to which base rate neglect is a fixed cognitive trait.').

omega_variable(
    effectiveness_of_debiasing,
    'How effective are debiasing techniques in mitigating the impact of base rate neglect in real-world decision-making scenarios?',
    'Randomized controlled trials of debiasing interventions in various contexts; meta-analysis of existing studies',
    'If effective: Potential for transition to Scaffold (temporary intervention). If ineffective: Reinforces Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_debiasing, empirical, 'Efficacy of debiasing strategies for overcoming base rate neglect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bayes_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(baye_tr_t0, bayes_theorem, theater_ratio, 0, 0.4).
narrative_ontology:measurement(baye_tr_t5, bayes_theorem, theater_ratio, 5, 0.35).
narrative_ontology:measurement(baye_tr_t10, bayes_theorem, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(baye_be_t0, bayes_theorem, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(baye_be_t5, bayes_theorem, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(baye_be_t10, bayes_theorem, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bayes_theorem, information_standard).
narrative_ontology:affects_constraint(bayes_theorem, confirmation_bias).
narrative_ontology:affects_constraint(bayes_theorem, availability_heuristic).

% DUAL FORMULATION NOTE:
% Bayes Theorem itself is not a constraint but a relationship that can be analyzed to show people deviate from correct answers because of base rate neglect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
