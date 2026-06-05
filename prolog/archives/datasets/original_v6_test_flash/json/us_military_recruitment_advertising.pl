% ============================================================================
% CONSTRAINT STORY: us_military_recruitment_advertising
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_military_recruitment_advertising, []).

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
 *   constraint_id: us_military_recruitment_advertising
 *   human_readable: US Military Recruitment Advertising
 *   domain: political
 *
 * SUMMARY:
 *   US military recruitment advertising seeks to attract new members by
 *   presenting a curated and often idealized view of military service. While
 *   providing opportunities and contributing to national defense, it also
 *   relies on asymmetric information and can disproportionately affect
 *   vulnerable populations. The type of constraint depends on perspective,
 *   with potential recruits, the military, and analytical observers all
 *   experiencing it differently.
 *
 * KEY AGENTS:
 *   - Potential Recruits: Primary target (powerless/trapped) - Subject to idealized portrayal and potential misinformation.
 *   - US Military: Primary beneficiary (institutional/arbitrage) - Uses advertising to maintain recruitment numbers.
 *   - Defense Contractors: Secondary beneficiary (institutional/arbitrage) - Profits from a larger military.
 *   - Taxpayers: Funding source (institutional/constrained) - Forced to fund military recruitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_recruitment_advertising, 0.55).
domain_priors:suppression_score(us_military_recruitment_advertising, 0.45).
domain_priors:theater_ratio(us_military_recruitment_advertising, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_recruitment_advertising, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_military_recruitment_advertising, tangled_rope).
narrative_ontology:human_readable(us_military_recruitment_advertising, "US Military Recruitment Advertising").
narrative_ontology:topic_domain(us_military_recruitment_advertising, "political").

domain_priors:requires_active_enforcement(us_military_recruitment_advertising).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, us_military).
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, defense_contractors).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, potential_recruits).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Potential recruits, especially those from disadvantaged backgrounds with limited alternatives, can be ensnared by the idealized portrayals and face significant consequences (physical, psychological, or financial) upon enlisting that were not fully presented or understood beforehand.  Limited exit options once committed.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Potential recruits who are better informed or have more options may still be influenced by the advertising but can make more reasoned decisions. Constrained by the limited availability of information, but benefits from potential career opportunities.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The US military benefits from recruitment advertising by attracting a sufficient number of recruits to maintain its force size and operational capabilities. They can shift advertising strategies to focus on different demographics to arbitrage recruitment numbers.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Taxpayers are constrained to fund the military and its recruitment efforts. The benefits (national security) are increasingly theatrical and diluted, while the costs remain, making it a piton.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, recruitment advertising serves as a tangled rope. It is a necessary function for military readiness but also relies on asymmetric information and can lead to negative outcomes for some recruits. The effectiveness and fairness are constantly evolving.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_recruitment_advertising_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_military_recruitment_advertising, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_military_recruitment_advertising, TR),
    TR >= 0.70.

:- end_tests(us_military_recruitment_advertising_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: (0.55) Moderate. Advertising extracts potential from vulnerable recruits by offering a path to social mobility. Suppression: (0.45) Moderate. The portrayal of military life and benefits suppress the negative aspects and risks associated with service. Theater Ratio: (0.70) Medium-High. While recruitment serves a genuine function of maintaining military readiness, a significant portion is dedicated to marketing and branding, projecting an idealized image.
 *
 * PERSPECTIVAL GAP:
 *   Potential recruits (trapped) see a snare due to asymmetric information and limited exit options. The US Military perceives a rope, as the ads help solve a coordination problem in finding recruits. Taxpayers view it as a piton with limited benefit. The analytical perspective is a tangled rope, balancing legitimate recruitment needs with potential exploitation and asymmetric information.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (US Military, Defense Contractors) experience the constraint as a coordination mechanism (rope), while victims (potential recruits, taxpayers) bear the costs. Directionality reflects their positions relative to the extraction flow. The degree of power and exit options influences their perceived impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_impact_on_recruits,
    'What are the long-term physical, psychological, and economic impacts on recruits who enlist due to advertising?',
    'Longitudinal studies tracking the well-being of recruits over decades.',
    'If impacts are severe, the classification shifts towards Snare. If positive, it shifts towards Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_recruits, empirical, 'Assessment of the long-term effects of military service on recruits.').

omega_variable(
    effectiveness_of_alternative_recruitment_methods,
    'How effective are alternative, less manipulative recruitment methods (e.g., scholarships, vocational training) compared to traditional advertising?',
    'Comparative analysis of recruitment numbers and recruit quality using different strategies.',
    'If alternative methods are effective, the suppression element decreases. If not, it increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_alternative_recruitment_methods, empirical, 'Evaluation of alternative recruitment strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_military_recruitment_advertising, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_m_tr_t0, us_military_recruitment_advertising, theater_ratio, 0, 0.4).
narrative_ontology:measurement(us_m_tr_t10, us_military_recruitment_advertising, theater_ratio, 10, 0.5).
narrative_ontology:measurement(us_m_tr_t20, us_military_recruitment_advertising, theater_ratio, 20, 0.7).

% Extraction over time
narrative_ontology:measurement(us_m_be_t0, us_military_recruitment_advertising, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_m_be_t10, us_military_recruitment_advertising, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(us_m_be_t20, us_military_recruitment_advertising, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_military_recruitment_advertising, enforcement_mechanism).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, military_industrial_complex).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, patriotic_narratives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
