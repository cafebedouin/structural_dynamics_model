% ============================================================================
% CONSTRAINT STORY: indian_ai_licensing_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_ai_licensing_regime, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indian_ai_licensing_regime
 *   human_readable: India's Sovereign AI Licensing and Data Localization Mandate
 *   domain: technological/political
 *
 * SUMMARY:
 *   Following the 2026 Delhi AI Expo, the Indian government enacted a new
 *   regulatory framework under its "AI for All" initiative. This framework
 *   mandates licensing for AI developers operating within India and enforces
 *   strict data localization policies, requiring AI systems to be trained and
 *   operated on Indian soil. The goal is to promote Indian AI sovereignty,
 *   foster domestic AI development, and ensure data security. However, this
 *   regime may also create barriers to entry for foreign AI companies and
 *   raise concerns about censorship and bias in AI systems.
 *
 * KEY AGENTS:
 *   - Indian AI Developers: Beneficiary (organized/mobile) - benefit from preferential treatment but are constrained by the licensing requirements.
 *   - Indian Government: Beneficiary (institutional/arbitrage) - benefits from control and oversight of AI development.
 *   - Foreign AI Developers: Victim (powerful/mobile) - constrained by the licensing regime.
 *   - Indian Citizens: Victim (powerless/trapped) - bear the cost of potentially censored AI systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_ai_licensing_regime, 0.6).
domain_priors:suppression_score(indian_ai_licensing_regime, 0.7).
domain_priors:theater_ratio(indian_ai_licensing_regime, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_ai_licensing_regime, extractiveness, 0.6).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_ai_licensing_regime, tangled_rope).
narrative_ontology:human_readable(indian_ai_licensing_regime, "India's Sovereign AI Licensing and Data Localization Mandate").
narrative_ontology:topic_domain(indian_ai_licensing_regime, "technological/political").

domain_priors:requires_active_enforcement(indian_ai_licensing_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, indian_ai_developers).
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, indian_government).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, foreign_ai_developers).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, indian_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Indian citizens have limited exit options and bear the cost of potentially censored or biased AI systems tailored to government interests.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Indian government benefits from greater control and oversight of AI development and deployment within its borders.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Foreign AI developers are constrained by the licensing regime but may find ways to arbitrage or partner with Indian companies.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Indian AI developers benefit from preferential treatment but are also constrained by the licensing requirements and data localization mandates.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% An analytical observer sees the licensing regime as a tangled rope, balancing national interests with innovation and global collaboration.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_ai_licensing_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_ai_licensing_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indian_ai_licensing_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Significant extraction due to barriers to entry for foreign companies and potential limitations on the use of AI for Indian citizens. Suppression (0.7): High suppression due to strict licensing requirements and data localization mandates. Theater Ratio (0.3): Low, as the regime seems genuinely focused on promoting local AI development.
 *
 * PERSPECTIVAL GAP:
 *   The Indian government sees the regime as a rope, enabling control and promoting local AI development. Foreign AI developers view it as a tangled rope, as they are constrained but still have options to operate in India. Indian citizens may experience it as a snare due to potential limitations and biases in AI systems.
 *
 * DIRECTIONALITY LOGIC:
 *   The Indian government benefits significantly, leading to a low directionality value. Foreign AI developers are constrained, resulting in a moderate directionality value. Indian citizens are the most affected, resulting in a high directionality value. The base extractiveness and suppression are influenced by these directionality values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_sovereignty_vs_innovation,
    'To what degree does data localization stifle AI innovation and global collaboration?',
    'Comparative analysis of AI development and deployment in India versus countries without data localization mandates.',
    'If data localization significantly hinders innovation, the licensing regime may need adjustments. If not, the regime could be considered successful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_sovereignty_vs_innovation, empirical, 'Balance between data sovereignty and AI innovation.').

omega_variable(
    ai_bias_and_censorship,
    'Does the licensing regime lead to biased or censored AI systems?',
    'Regular audits of AI systems deployed in India to assess bias and censorship.',
    'If significant bias or censorship is detected, the licensing regime may need to be revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_bias_and_censorship, empirical, 'Risk of AI bias and censorship under the licensing regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_ai_licensing_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, indian_ai_licensing_regime, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indi_tr_t5, indian_ai_licensing_regime, theater_ratio, 5, 0.3).
narrative_ontology:measurement(indi_tr_t10, indian_ai_licensing_regime, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, indian_ai_licensing_regime, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(indi_be_t5, indian_ai_licensing_regime, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(indi_be_t10, indian_ai_licensing_regime, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(indian_ai_licensing_regime, global_ai_governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
