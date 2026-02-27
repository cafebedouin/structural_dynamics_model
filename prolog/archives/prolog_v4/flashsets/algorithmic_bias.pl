% ============================================================================
% CONSTRAINT STORY: algorithmic_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_bias, []).

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
 *   constraint_id: algorithmic_bias
 *   human_readable: Algorithmic Bias
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic bias occurs when machine learning systems produce
 *   systematically prejudiced results by automating historical inequities
 *   found in training data. This results in unfair outcomes for marginalized
 *   communities, while benefiting dominant tech companies and governments.
 *   This analysis assesses the dynamics of algorithmic bias, considering
 *   perspectives from various actors, including marginalized communities,
 *   individual users, dominant tech companies, regulatory bodies, and an
 *   analytical observer.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary victims (powerless/trapped) - disproportionately affected by biased outcomes and lack means to challenge them.
 *   - Individual Users: Secondary victims (moderate/constrained) - subject to biased recommendations and filtering that limit their choices.
 *   - Dominant Tech Companies: Primary beneficiaries (institutional/arbitrage) - benefit from reinforcing market power and control over data.
 *   - Governments/Surveillance Agencies: Secondary beneficiaries (institutional/constrained) - utilize algorithms for surveillance and control, often perpetuating biases.
 *   - Regulatory Bodies: Constrained actor (institutional/constrained) - struggle to monitor and mitigate bias due to limited understanding and resources.
 *   - Analytical Observer: Civilizational view (analytical/analytical) - assesses the long-term challenges to fairness and equality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_bias, 0.6).
domain_priors:suppression_score(algorithmic_bias, 0.7).
domain_priors:theater_ratio(algorithmic_bias, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_bias, extractiveness, 0.6).
narrative_ontology:constraint_metric(algorithmic_bias, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(algorithmic_bias, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_bias, tangled_rope).
narrative_ontology:human_readable(algorithmic_bias, "Algorithmic Bias").
narrative_ontology:topic_domain(algorithmic_bias, "technological/social").

domain_priors:requires_active_enforcement(algorithmic_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_bias, dominant_tech_companies).
narrative_ontology:constraint_beneficiary(algorithmic_bias, governments_surveillance_agencies).
narrative_ontology:constraint_victim(algorithmic_bias, marginalized_communities).
narrative_ontology:constraint_victim(algorithmic_bias, individual_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Marginalized communities are often trapped by algorithmic bias in areas like loan applications, job opportunities, and criminal justice. They bear the disproportionate burden of biased outcomes and have limited means to challenge or escape these systems.
constraint_indexing:constraint_classification(algorithmic_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Individual users are constrained by their dependence on algorithms for information, services, and opportunities. They benefit from some efficiencies and convenience but are also subject to biased recommendations and filtering that can limit their choices and reinforce existing inequalities.
constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Dominant tech companies benefit from algorithmic bias by reinforcing their market power and control over data. They can arbitrage this power to optimize their algorithms for profit, even if it means perpetuating inequalities. Algorithmic bias allows them to streamline processes and reduce labor costs, which benefits their bottom line.
constraint_indexing:constraint_classification(algorithmic_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulatory bodies are constrained by their limited understanding of complex algorithms and the rapid pace of technological development. They benefit from the potential for algorithms to improve efficiency and decision-making, but they struggle to effectively monitor and mitigate bias, leading to unintended consequences for society.
constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational perspective, algorithmic bias represents a long-term challenge to fairness and equality. The perpetuation of historical biases through automated systems requires careful scrutiny and ethical considerations to ensure that technology serves humanity's best interests.
constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(algorithmic_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Algorithmic bias allows for systemic extraction of opportunity and equitable treatment from marginalized groups. The extraction comes from reinforcing existing power structures and economic inequalities. Suppression (0.70): High. The increasing reliance on algorithms for decision-making suppresses alternative perspectives and reinforces biased outcomes, especially for those already disadvantaged. The lack of transparency and explainability of algorithms further suppresses the ability of victims to challenge biased outcomes. Theater ratio (0.30): Low. While some efforts are made to address algorithmic bias, they are often performative and lack meaningful impact, allowing the problem to persist and even worsen over time.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from differing experiences and structural positions. Marginalized communities experience algorithmic bias as a snare, limiting opportunities and reinforcing inequalities. Dominant tech companies experience it as a rope, benefiting from enhanced efficiency and market power. Individual users face a tangled rope, benefitting from efficiency while facing algorithmic constraints on autonomy. Regulatory bodies see tangled rope due to the difficulty of creating appropriate oversight mechanisms. The analytical observer sees a tangled rope because of the mixture of intended benefits with unintended social consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Marginalized communities are victims with limited exit options, leading to high directionality and a snare classification. Tech companies are beneficiaries with arbitrage options, resulting in low directionality and a rope classification. Individual users and regulatory bodies have moderate directionality due to constrained exit options. The analytical observer assesses overall societal impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This analysis differentiates the tangled rope classification from a pure snare by acknowledging the coordination benefits algorithms provide. While significant extraction occurs from marginalized groups, algorithms offer efficiency, and predictive power that benefit a broader population. This warrants a classification of tangled rope. Regulatory and oversight mechanisms are needed to reduce extraction, improve fairness, and avoid a slide into pure snare for certain groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_representativeness,
    'To what extent does training data accurately reflect the diversity and complexity of the real world, avoiding historical biases and skewed distributions?',
    'Rigorous audits of training data for bias, including statistical analysis, demographic breakdowns, and sensitivity testing.',
    'If data is unrepresentative, bias will be amplified and perpetuated. If data is representative, algorithmic outcomes will be more equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_representativeness, empirical, 'The representativeness of training data.').

omega_variable(
    algorithm_fairness_metrics,
    'Which fairness metrics are most appropriate for evaluating and mitigating bias in specific algorithmic applications?',
    'Comparative analysis of different fairness metrics, considering their mathematical properties, ethical implications, and suitability for different contexts.',
    'Choosing the wrong fairness metric can lead to unintended consequences and exacerbate bias. Choosing the right metric can promote more equitable outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_fairness_metrics, conceptual, 'Selection of algorithm fairness metrics.').

omega_variable(
    human_oversight_effectiveness,
    'How effective is human oversight in detecting and correcting algorithmic bias, considering the limitations of human judgment and the scale of algorithmic decision-making?',
    'Empirical studies of human-algorithm collaboration, including controlled experiments, qualitative interviews, and real-world deployments.',
    'If human oversight is ineffective, bias will persist unchecked. If human oversight is effective, algorithmic outcomes will be more equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_oversight_effectiveness, empirical, 'Effectiveness of human oversight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t0, algorithmic_bias, theater_ratio, 0, 0.1).
narrative_ontology:measurement(algo_tr_t5, algorithmic_bias, theater_ratio, 5, 0.2).
narrative_ontology:measurement(algo_tr_t10, algorithmic_bias, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(algo_be_t0, algorithmic_bias, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(algo_be_t5, algorithmic_bias, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(algo_be_t10, algorithmic_bias, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_bias, information_standard).
narrative_ontology:affects_constraint(algorithmic_bias, data_privacy).
narrative_ontology:affects_constraint(algorithmic_bias, digital_divide).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
