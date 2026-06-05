% ============================================================================
% CONSTRAINT STORY: cancer_prevention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_prevention, []).

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
 *   constraint_id: cancer_prevention
 *   human_readable: Systemic Barriers to Preventable Cancer Risk Reduction
 *   domain: social
 *
 * SUMMARY:
 *   A global report indicates that 40% of cancers are preventable through
 *   lifestyle changes and public health initiatives. However, systemic
 *   barriers hinder the implementation of these preventative measures. These
 *   barriers include corporate influence, societal norms, and inadequate
 *   access to healthcare and healthy options, creating a complex web of
 *   extraction and coordination. This constraint operates at the intersection
 *   of public health, economics, and social policy.
 *
 * KEY AGENTS:
 *   - General Public: Primary victim (powerless/trapped)
 *   - Healthcare Systems: Secondary victim (moderate/constrained)
 *   - Processed Food Industry: Primary beneficiary (institutional/arbitrage)
 *   - Tobacco Industry: Primary beneficiary (institutional/arbitrage)
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage)
 *   - Public Health Organizations: Organized actor (organized/mobile)
 *   - Outdated Regulatory Frameworks: Inertial actor (institutional/constrained)
 *   - Analytical Observer: Analytical agent (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_prevention, 0.55).
domain_priors:suppression_score(cancer_prevention, 0.65).
domain_priors:theater_ratio(cancer_prevention, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_prevention, extractiveness, 0.55).
narrative_ontology:constraint_metric(cancer_prevention, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cancer_prevention, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_prevention, tangled_rope).
narrative_ontology:human_readable(cancer_prevention, "Systemic Barriers to Preventable Cancer Risk Reduction").
narrative_ontology:topic_domain(cancer_prevention, "social").

domain_priors:requires_active_enforcement(cancer_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_prevention, processed_food_industry).
narrative_ontology:constraint_beneficiary(cancer_prevention, tobacco_industry).
narrative_ontology:constraint_beneficiary(cancer_prevention, pharmaceutical_companies).
narrative_ontology:constraint_victim(cancer_prevention, general_public).
narrative_ontology:constraint_victim(cancer_prevention, healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL PUBLIC (SNARE) - Trapped by social norms, marketing, and lack of access to healthy options.  Bears the cost of increased cancer risk due to systemic barriers. Limited ability to exit due to pervasive influences and structural factors.
constraint_indexing:constraint_classification(cancer_prevention, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HEALTHCARE SYSTEMS (TANGLED ROPE) - Constrained by reactive treatment models and funding limitations. Benefits from treating cancer, but also burdened by the high costs and increased demand due to preventable cases. Limited arbitrage, some ability to influence policy.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROCESSED FOOD/TOBACCO/PHARMA INDUSTRIES (ROPE) - Benefits from consumer demand, policy loopholes, and a focus on treatment over prevention. Arbitrage through lobbying, marketing, and product innovation. Experience the constraint as coordination of supply and demand.
constraint_indexing:constraint_classification(cancer_prevention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH ORGANIZATIONS (SCAFFOLD) - Organized actors attempting to reduce cancer risk through education, policy advocacy, and promoting healthy lifestyles. See the barriers as a temporary coordination problem with a sunset clause as policies and awareness improve.
constraint_indexing:constraint_classification(cancer_prevention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OUTDATED REGULATORY FRAMEWORKS (PITON) - Regulatory frameworks designed when less was known about cancer prevention are now largely performative or ineffective due to lobbying and shifting scientific understanding. They persist through institutional inertia.
constraint_indexing:constraint_classification(cancer_prevention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) - Sees the systemic barriers as a combination of coordination and extraction, with vested interests maintaining the status quo at the expense of public health. Analyzes long term trends and systemic effects.
constraint_indexing:constraint_classification(cancer_prevention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_prevention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_prevention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_prevention, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_prevention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_prevention, TR),
    TR >= 0.70.

:- end_tests(cancer_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate to high, reflecting the significant societal costs associated with preventable cancers. Suppression: 0.65 - High, due to the influence of vested interests and the difficulty of changing deeply ingrained habits and social norms. Theater Ratio: 0.40 - Moderate, reflecting some genuine efforts in cancer prevention campaigns alongside performative actions.
 *
 * PERSPECTIVAL GAP:
 *   The general public experiences a Snare due to limited access to healthy choices and pervasive marketing. Healthcare systems experience a Tangled Rope, managing both the costs of treatment and the potential for prevention.  Industries see a Rope, focused on coordinating supply and demand. Public health organizations see a temporary coordination failure (Scaffold), working towards systemic change. Regulatory frameworks are degraded (Piton) due to inertia and corporate influence. The Analytical Observer sees the systemic forces preventing effective cancer prevention (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Industries (processed food, tobacco, pharmaceuticals) benefit from existing systems and arbitrage regulatory loopholes. The general public bears the costs of preventable cancers and has limited exit options due to advertising, social norms, and access issues.  Healthcare systems are constrained by reactive treatment models. Public health organizations are trying to change the status quo. The existing regulatory frameworks are outdated and ineffective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a Tangled Rope because there are both coordination and extraction elements. The industries coordinate supply and demand but also extract societal costs through increased cancer rates. Public health efforts aim to coordinate resources for prevention but are often undermined by systemic barriers.  The different perspectives help clarify whether the constraint tends towards pure extraction or effective prevention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_public_health_campaigns,
    'How effective are public health campaigns in changing behavior and reducing cancer risk?',
    'Longitudinal studies, randomized controlled trials, and meta-analyses of public health interventions.',
    'If highly effective: shift towards a Rope or Scaffold classification. If ineffective: Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_public_health_campaigns, empirical, 'The efficacy of public health campaigns in reducing cancer risk.').

omega_variable(
    corporate_influence_on_policy,
    'To what extent does corporate lobbying and campaign finance influence policies related to cancer prevention?',
    'Analysis of lobbying expenditures, campaign contributions, and policy outcomes.',
    'Strong corporate influence reinforces a Snare classification; weaker influence allows for more effective prevention policies (Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_influence_on_policy, empirical, 'Corporate influence on policies related to cancer prevention.').

omega_variable(
    tradeoff_between_economic_growth_and_public_health,
    'How do societies balance the pursuit of economic growth (e.g., through industrial activity) with the need to protect public health?',
    'Comparative studies of different countries'' approaches to regulating industries known to increase cancer risk.',
    'A strong emphasis on economic growth reinforces a Snare. A strong emphasis on public health strengthens the public health perspective and allows more coordination (Rope or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradeoff_between_economic_growth_and_public_health, preference, 'Societal tradeoff between economic growth and public health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_prevention, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_tr_t0, cancer_prevention, theater_ratio, 0, 0.3).
narrative_ontology:measurement(canc_tr_t10, cancer_prevention, theater_ratio, 10, 0.4).
narrative_ontology:measurement(canc_tr_t20, cancer_prevention, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(canc_be_t0, cancer_prevention, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(canc_be_t10, cancer_prevention, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(canc_be_t20, cancer_prevention, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_prevention, enforcement_mechanism).
narrative_ontology:affects_constraint(cancer_prevention, food_deserts).
narrative_ontology:affects_constraint(cancer_prevention, tobacco_advertising).
narrative_ontology:affects_constraint(cancer_prevention, access_to_healthcare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
