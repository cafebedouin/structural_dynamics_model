% ============================================================================
% CONSTRAINT STORY: sorites_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sorites_paradox, []).

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
 *   constraint_id: sorites_paradox
 *   human_readable: The Sorites Paradox (Application of Legal Cutoffs)
 *   domain: legal/social
 *
 * SUMMARY:
 *   This constraint models the *application* of arbitrary sharp boundaries
 *   (legal cutoffs) to solve the problem of vague predicates (the Sorites
 *   Paradox). While cutoffs provide clarity and enforceability, they also
 *   create injustices for individuals near the boundary and can exacerbate
 *   existing inequalities. This creates a tension between the benefits of the
 *   coordination mechanism and the extraction of the snare.
 *
 * KEY AGENTS:
 *   - Legal System: Institutional beneficiary with arbitrage (institutional/arbitrage) - gains clarity and enforceability.
 *   - Policy Makers: Benefit from the coordination mechanism to enforce laws. (powerful/mobile)
 *   - Marginalized Individuals: Bear the brunt of arbitrary boundaries (powerless/trapped) - subject to the direct negative consequences.
 *   - Affected Communities: Experience both the benefits and burdens of the cutoff. (moderate/constrained)
 *   - Analytical Observer: Can see both sides of the issue (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sorites_paradox, 0.55).
domain_priors:suppression_score(sorites_paradox, 0.7).
domain_priors:theater_ratio(sorites_paradox, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sorites_paradox, extractiveness, 0.55).
narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sorites_paradox, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sorites_paradox, tangled_rope).
narrative_ontology:human_readable(sorites_paradox, "The Sorites Paradox (Application of Legal Cutoffs)").
narrative_ontology:topic_domain(sorites_paradox, "legal/social").

domain_priors:requires_active_enforcement(sorites_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sorites_paradox, legal_system).
narrative_ontology:constraint_beneficiary(sorites_paradox, policy_makers).
narrative_ontology:constraint_victim(sorites_paradox, marginalized_individuals).
narrative_ontology:constraint_victim(sorites_paradox, affected_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual directly affected by the legal cutoff, often facing significant negative consequences with little recourse.
constraint_indexing:constraint_classification(sorites_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% A community impacted by the application of a legal cutoff, balancing some benefits with the negative consequences of exclusion or unequal treatment.
constraint_indexing:constraint_classification(sorites_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The legal system benefits from the clarity and enforceability provided by the cutoff, even if it creates some injustices, with the ability to modify cutoffs for arbitrage.
constraint_indexing:constraint_classification(sorites_paradox, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Policy makers coordinate a response for legal cutoffs to implement legal enforcement.
constraint_indexing:constraint_classification(sorites_paradox, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% An outside observer of the law can see both sides of the issue, a coordinated response with active enforcement and affected communities being extracted from by active enforcement.
constraint_indexing:constraint_classification(sorites_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sorites_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sorites_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sorites_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sorites_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sorites_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The cutoff is needed to enforce the law, but the effect will not be equally distributed. Suppression (0.70): High. The enforcement creates an environment for which affected groups have little room for recourse. Theater Ratio (0.30): Low. The legal process may not be as performative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arbitrary_threshold_justification,
    'What level of social harm justifies the use of an arbitrary cutoff?',
    'Cost-benefit analysis, ethical considerations, public discourse, and legal precedent.',
    'Determines whether the cutoff is seen as legitimate or oppressive. A clearer justification leads to reduced social friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrary_threshold_justification, preference, 'Defines what amount of public harm is just to justify legal cutoff').

omega_variable(
    definition_vague_predicate,
    'How do we best define the meaning of vague predicates?',
    'Linguistic analysis, empirical studies of language use, and theoretical frameworks in philosophy of language.',
    'The more precise the definition of a vague predicate, the less arbitrary cutoffs are required to implement laws.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vague_predicate, conceptual, 'The ability to improve vague predicates').

omega_variable(
    fairness_impact_measurement,
    'Can the fairness impacts of legal cutoffs be accurately measured?',
    'Statistical methods, social science research, and human rights assessments.',
    'The ability to measure the effects of cutoffs allows for a more robust analysis of the total cost of implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_impact_measurement, empirical, 'Measurement metrics for determining effects of cutoffs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sorites_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sori_tr_t0, sorites_paradox, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sori_tr_t5, sorites_paradox, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sori_tr_t10, sorites_paradox, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sori_be_t0, sorites_paradox, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sori_be_t5, sorites_paradox, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(sori_be_t10, sorites_paradox, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sorites_paradox, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
