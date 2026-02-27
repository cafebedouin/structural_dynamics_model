% ============================================================================
% CONSTRAINT STORY: prime_age_male_unwork
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_age_male_unwork, []).

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
 *   constraint_id: prime_age_male_unwork
 *   human_readable: The "New Misery" of Prime-Age Male Labor Force Exit
 *   domain: social/economic
 *
 * SUMMARY:
 *   The "New Misery" of prime-age male labor force exit describes a systemic
 *   problem where a significant portion of men aged 25-54 are neither working
 *   nor actively seeking employment. This phenomenon is driven by a complex
 *   interplay of factors including skill obsolescence, disability, addiction,
 *   criminal records, and a perceived lack of suitable opportunities. This
 *   story examines the causes and consequences of this trend.
 *
 * KEY AGENTS:
 *   - Prime-Age Males: Primary target (powerless/trapped) - suffer the most from the consequences of labor force detachment.
 *   - Social Safety Net Programs: Moderate actor (moderate/constrained) - provide support but may inadvertently disincentivize workforce participation.
 *   - Traditional Economic Models: Institutional actor (institutional/constrained) - often fail to adequately address the complex social and psychological factors at play.
 *   - Workforce Retraining Programs: Organized actor (organized/mobile) - aim to reskill and reintegrate men into the workforce, but face challenges in effectiveness.
 *   - Global Economic Productivity: Systemic victim (analytical/analytical) - the total drag on global productivity due to unemployment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_age_male_unwork, 0.55).
domain_priors:suppression_score(prime_age_male_unwork, 0.7).
domain_priors:theater_ratio(prime_age_male_unwork, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_age_male_unwork, extractiveness, 0.55).
narrative_ontology:constraint_metric(prime_age_male_unwork, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(prime_age_male_unwork, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_age_male_unwork, snare).
narrative_ontology:human_readable(prime_age_male_unwork, "The \"New Misery\" of Prime-Age Male Labor Force Exit").
narrative_ontology:topic_domain(prime_age_male_unwork, "social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_victim(prime_age_male_unwork, prime_age_males).
narrative_ontology:constraint_victim(prime_age_male_unwork, social_safety_net_sustainability).
narrative_ontology:constraint_victim(prime_age_male_unwork, economic_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Prime-Age Male (Snare) - Facing a complex web of factors (skill obsolescence, disability, addiction, criminal record, lack of opportunity), many prime-age males feel trapped with limited exit options from their unemployed state. The system extracts their potential economic and social contribution.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Social Safety Net Programs (Tangled Rope) - These programs (disability, welfare, etc.) are designed to provide support, but face the tangled reality of enabling long-term detachment from the workforce. They offer some coordination (support), but face extraction through increased burden and potential disincentives for workforce participation. The constraint lies in that the support structure constrains them to continue utilizing the service and thus does not fix the underlying problem that caused it.
constraint_indexing:constraint_classification(prime_age_male_unwork, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Traditional Economic Models (Piton) - Traditional economic models often fail to fully account for the complex social and psychological factors driving prime-age male labor force exit. The models continue to be used but are increasingly theatrical and disconnected from the reality on the ground. The inertia is continued by continued use of key performance indicators that do not accurately measure the impact of the programs.
constraint_indexing:constraint_classification(prime_age_male_unwork, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4: Workforce Retraining Programs (Tangled Rope) - Aiming to reskill and reintegrate men into the workforce, these programs offer coordination but are often underfunded and mismatched with actual labor market demands. Active enforcement via funding and incentives leads to some success, but the asymmetric extraction is seen in limited job placement rates and career progression. There is asymmetric extraction because it takes energy to improve the training programs.
constraint_indexing:constraint_classification(prime_age_male_unwork, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective 5: Global Economic Productivity (Snare) - The decline in prime-age male labor force participation represents a drag on overall global economic productivity. Loss of contribution towards innovation and other key metrics makes this appear as extraction from the economic standard of living.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 6: Economic Productivity (Snare) - The loss of prime-age male labor force participation means a drop in economic productivity. This affects quality of life as well as ability to innovate.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_age_male_unwork_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prime_age_male_unwork, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prime_age_male_unwork, TR),
    TR >= 0.70.

:- end_tests(prime_age_male_unwork_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts potential economic contributions from prime-age males, leading to decreased productivity and innovation. Suppression (0.70): High. Factors like skill obsolescence, disability, and criminal records create significant barriers to re-entering the workforce. Theater ratio (0.30): Low. While there is some performative activity (e.g., ineffective job fairs), the core issue is a real economic and social problem.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing experiences of various actors. The prime-age male feels trapped and extracted from, while social safety net programs struggle to balance support and work incentives. Traditional economic models offer limited insights, while workforce retraining programs try to address the problem but often fall short. This disparity of structural experiences can explain the failure to fully rectify this problem.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) are determined by each agent's structural position within the system. Prime-age males, as the primary victims, face high directionality. Social safety net programs face mixed directionality. Traditional economic models have moderate directionality. Workforce retraining programs face low directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This situation is a snare. The question is: Why does this persist, despite efforts? Traditional models view it as something to be explained only in the context of economic systems. However, disability, education, skill, criminal records and other structural issues all have a component. If we remove the extraction factor (provide easier pathways and fix structural incentive issues) we will likely see a major improvement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_obsolescence_vs_lack_of_opportunity,
    'To what extent is the labor force exit driven by skill obsolescence versus a genuine lack of suitable job opportunities?',
    'Longitudinal studies tracking skill sets and job market demands; surveys assessing perceived job availability among the non-working population.',
    'If skill obsolescence is dominant: retraining and education programs are crucial. If lack of opportunity is dominant: broader economic reforms and job creation initiatives are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_obsolescence_vs_lack_of_opportunity, empirical, 'Determining the primary driver of labor force exit: skill obsolescence or lack of opportunity.').

omega_variable(
    disability_definition_and_measurement,
    'How much of the rise in disability claims among prime-age males is due to genuine health issues versus strategic adaptation to economic hardship?',
    'Detailed medical evaluations and assessments; analysis of disability claim trends in relation to economic cycles and policy changes.',
    'If genuine health issues are dominant: healthcare and social support systems need strengthening. If strategic adaptation is dominant: reforms to disability programs and work incentives are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_definition_and_measurement, empirical, 'Assessing the role of disability in labor force exit, distinguishing genuine health issues from strategic adaptation.').

omega_variable(
    social_stigma_and_identity,
    'How does social stigma associated with unemployment and changing gender roles affect prime-age males'' willingness to seek and maintain employment?',
    'Qualitative research exploring the lived experiences of unemployed men; sociological studies examining the evolving meanings of work and masculinity.',
    'If social stigma is a major factor: interventions to challenge negative stereotypes and promote positive identities are needed. If not: other structural factors are likely more determinant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_stigma_and_identity, conceptual, 'Evaluating the impact of social stigma and changing gender roles on men''s labor force participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_age_male_unwork, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prim_tr_t0, prime_age_male_unwork, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prim_tr_t10, prime_age_male_unwork, theater_ratio, 10, 0.25).
narrative_ontology:measurement(prim_tr_t20, prime_age_male_unwork, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(prim_be_t0, prime_age_male_unwork, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prim_be_t10, prime_age_male_unwork, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(prim_be_t20, prime_age_male_unwork, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
