% ============================================================================
% CONSTRAINT STORY: uk_graduate_visa_salary_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-11-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_graduate_visa_salary_threshold, []).

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
 *   constraint_id: uk_graduate_visa_salary_threshold
 *   human_readable: UK Graduate Visa Minimum Salary Threshold
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK government's decision to increase the minimum salary threshold for
 *   the Graduate Visa route presents a complex interplay of economic and
 *   political considerations. While intended to protect the domestic labor
 *   market, it also creates significant challenges for international
 *   graduates and could potentially harm the UK's innovation ecosystem. The
 *   threshold acts as a snare for those graduates, limiting their
 *   opportunities. This analysis considers multiple perspectives to
 *   understand the full scope of the issue.
 *
 * KEY AGENTS:
 *   - International Graduates: Primary target (powerless/trapped)
 *   - UK Economy Innovation: Secondary target (moderate/constrained)
 *   - UK Domestic Labor Market: Primary beneficiary (institutional/arbitrage)
 *   - UK Universities (Prestige): Secondary beneficiary (institutional/arbitrage)
 *   - Analytical Observer: Considers all perspectives (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, 0.65).
domain_priors:suppression_score(uk_graduate_visa_salary_threshold, 0.7).
domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, extractiveness, 0.65).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_graduate_visa_salary_threshold, snare).
narrative_ontology:human_readable(uk_graduate_visa_salary_threshold, "UK Graduate Visa Minimum Salary Threshold").
narrative_ontology:topic_domain(uk_graduate_visa_salary_threshold, "economic/political").

domain_priors:requires_active_enforcement(uk_graduate_visa_salary_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_domestic_labor_market).
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_universities_prestige).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, international_graduates).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, uk_economy_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% International graduates face significant barriers to meeting the increased salary threshold. They are often trapped due to visa sponsorship requirements and limited job options, making them vulnerable to exploitation and limiting their career prospects in the UK. d=0.95, chi=0.89
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The UK economy, while benefiting from some protections to the domestic labor market, is constrained by reduced access to international talent. The increased threshold may hinder innovation and economic growth. It benefits from reduced competition but suffers from brain drain. d=0.55, chi=0.52
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The domestic labor market benefits from reduced competition from international graduates, potentially leading to increased wages and job opportunities for UK residents. d=0.05, chi=-0.07
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% While universities may suffer from lower enrollment due to reduced attractiveness for international students, they may also see increased prestige from perceived higher standards and exclusivity. Also, they can arbitrage by focusing on higher-paying programs. d=0.05, chi = -0.07
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the salary threshold represents a tangled rope. It aims to protect the domestic labor market (coordination) but also extracts from international graduates and potentially harms the UK's innovation ecosystem (asymmetric extraction). d=0.73, chi=0.57
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_graduate_visa_salary_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(uk_graduate_visa_salary_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the salary threshold directly limits the opportunities for international graduates and makes it difficult for them to remain in the UK. Suppression (0.70) is also high due to limited alternative pathways for graduates to stay in the UK legally. Theater Ratio is relatively low (0.30), indicating the policy is more functional than performative.
 *
 * PERSPECTIVAL GAP:
 *   International graduates experience the policy as a snare due to limited exit options and potential exploitation. The UK economy experiences it as a tangled rope, balancing domestic labor market protection with potential harm to innovation. The domestic labor market views it as a rope, benefiting from reduced competition, as do universities interested in prestige.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the relationship of each agent to the constraint. International graduates are the primary targets, resulting in a high 'd' value. The domestic labor market and universities benefit, resulting in a low 'd' value. The UK economy experiences a mixed effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint could be misclassified as a pure enforcement mechanism (rope) without considering the negative impact on international graduates and the innovation economy. The analysis reveals it to be more of a snare, as it extracts significant value from a vulnerable group while providing only limited benefits to the domestic labor market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What is the long-term impact of the salary threshold on the UK''s economy and innovation?',
    'Longitudinal studies analyzing economic growth, innovation metrics, and talent retention rates.',
    'If negative: the policy may need to be revised or replaced. If positive: the policy may be expanded or maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Long-term impact of the salary threshold on the UK''s economy and innovation').

omega_variable(
    attractiveness_to_international_talent,
    'How will the salary threshold affect the UK''s attractiveness to international talent?',
    'Surveys and data analysis of international student applications and enrollment rates.',
    'If reduced: the UK may need to offer alternative pathways for attracting talent. If unchanged: the policy may be maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attractiveness_to_international_talent, empirical, 'Impact on the UK''s attractiveness to international talent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_graduate_visa_salary_threshold, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_g_tr_t0, uk_graduate_visa_salary_threshold, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uk_g_tr_t2, uk_graduate_visa_salary_threshold, theater_ratio, 2, 0.25).
narrative_ontology:measurement(uk_g_tr_t4, uk_graduate_visa_salary_threshold, theater_ratio, 4, 0.3).

% Extraction over time
narrative_ontology:measurement(uk_g_be_t0, uk_graduate_visa_salary_threshold, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(uk_g_be_t2, uk_graduate_visa_salary_threshold, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(uk_g_be_t4, uk_graduate_visa_salary_threshold, base_extractiveness, 4, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_graduate_visa_salary_threshold, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
