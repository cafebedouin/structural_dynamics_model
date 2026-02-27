% ============================================================================
% CONSTRAINT STORY: genetic_predisposition_mania
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_predisposition_mania, []).

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
 *   constraint_id: genetic_predisposition_mania
 *   human_readable: Genetic Blueprint for Manic Episodes in Bipolar Disorder
 *   domain: technological
 *
 * SUMMARY:
 *   The identification of a genetic blueprint for manic episodes in bipolar
 *   disorder represents a complex interaction between genetics, environment,
 *   and societal factors. While it offers the potential for targeted
 *   treatments and improved management of the condition, it also raises
 *   ethical concerns about genetic determinism and discrimination. The
 *   classification of this constraint varies depending on the perspective,
 *   ranging from a snare for affected individuals to a rope for
 *   pharmaceutical companies.
 *
 * KEY AGENTS:
 *   - Individuals with Bipolar Disorder: Primary target (powerless/trapped) - subject to genetic predisposition.
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) - benefits from drug development targets.
 *   - Research Institutions: Secondary beneficiary (institutional/constrained) - benefits from funding opportunities.
 *   - Mental Health Advocacy Groups: Organized agents (organized/mobile) - advocating for policy changes.
 *   - Analytical Observer: Analyzes the long-term impacts (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition_mania, 0.55).
domain_priors:suppression_score(genetic_predisposition_mania, 0.4).
domain_priors:theater_ratio(genetic_predisposition_mania, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition_mania, extractiveness, 0.55).
narrative_ontology:constraint_metric(genetic_predisposition_mania, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(genetic_predisposition_mania, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_predisposition_mania, tangled_rope).
narrative_ontology:human_readable(genetic_predisposition_mania, "Genetic Blueprint for Manic Episodes in Bipolar Disorder").
narrative_ontology:topic_domain(genetic_predisposition_mania, "technological").

domain_priors:requires_active_enforcement(genetic_predisposition_mania).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_predisposition_mania, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(genetic_predisposition_mania, research_institutions).
narrative_ontology:constraint_victim(genetic_predisposition_mania, individuals_bipolar_disorder).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals with bipolar disorder may feel trapped by their genetic predisposition, leading to feelings of inevitability and fatalism regarding manic episodes.  Limited exit options due to lack of immediate, effective personalized treatments.
constraint_indexing:constraint_classification(genetic_predisposition_mania, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Pharmaceutical companies benefit from the genetic blueprint as it provides a target for drug development and personalized medicine. Arbitrage exists through investment decisions and patenting of specific genetic targets.
constraint_indexing:constraint_classification(genetic_predisposition_mania, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a global, long-term perspective, the genetic blueprint for mania represents a tangled rope. It offers potential benefits through improved understanding and treatment, but also introduces the risk of genetic discrimination and the medicalization of mental health.  Extraction stems from the potential for misuse of this knowledge.
constraint_indexing:constraint_classification(genetic_predisposition_mania, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Mental health advocacy groups can use this information to lobby for increased research funding and improved access to mental health care. This can be viewed as a temporary scaffold, as the need for advocacy may diminish as personalized treatments become more widely available and societal stigma decreases.  Exit through achieving their goals.
constraint_indexing:constraint_classification(genetic_predisposition_mania, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_mania_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_predisposition_mania, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genetic_predisposition_mania_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55):  Moderate.  The genetic blueprint allows for targeted drug development, which creates value for pharmaceutical companies. The potential for genetic discrimination and the medicalization of mental health can extract value from individuals with bipolar disorder. Suppression (0.40):  Moderate.  While there are no laws directly suppressing alternative viewpoints, the focus on genetic solutions may overshadow psychosocial approaches to treatment. Theater Ratio (0.20): Low. Direct relationship to the genetic code with little performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   Individuals with bipolar disorder experience this constraint as a snare because they may feel trapped by their genetic predisposition. Pharmaceutical companies benefit as the information can be used for profit. Advocacy groups attempt to mitigate the negative effects. From a global view it represents the possibility of extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical companies benefit from the knowledge gained through this discovery allowing them to develop and market drugs. Individuals with bipolar disorder potentially face discrimination and have limited exit options. Advocacy groups can use the information to support individuals with bipolar disorder. Thus a tangled rope captures the essence of this issue.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandated trophy is resolved by considering all of the players in the system. This includes the negative perspective of genetic fatalism, which is a snare. As well as the institutional perspective of the pharmaceutical companies which is a rope. By understanding the viewpoints of the players the mandantrophy can be resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_determinism_vs_environment,
    'To what extent are manic episodes solely determined by genetics, versus environmental factors?',
    'Longitudinal studies comparing individuals with the identified genetic blueprint in different environmental conditions.',
    'If primarily genetic:  shifts classification towards a Mountain.  If significantly environmental: shifts classification towards a Rope or Scaffold (treatable/manageable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_determinism_vs_environment, empirical, 'Influence of genetics versus environment on manic episodes.').

omega_variable(
    potential_for_genetic_discrimination,
    'What is the potential for genetic discrimination based on the identified blueprint?',
    'Analysis of existing anti-discrimination laws and policies; monitoring for instances of genetic discrimination.',
    'High potential:  reinforces the Snare classification for individuals with bipolar disorder.  Low potential:  shifts classification towards Tangled Rope or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_for_genetic_discrimination, conceptual, 'Potential for genetic discrimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_predisposition_mania, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genetic_predisposition_mania, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t5, genetic_predisposition_mania, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gene_tr_t10, genetic_predisposition_mania, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genetic_predisposition_mania, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gene_be_t5, genetic_predisposition_mania, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gene_be_t10, genetic_predisposition_mania, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_predisposition_mania, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
