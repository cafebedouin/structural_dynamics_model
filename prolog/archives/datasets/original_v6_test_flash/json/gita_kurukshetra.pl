% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra, []).

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
 *   constraint_id: gita_kurukshetra
 *   human_readable: The Duty of the Kshatriya (Warrior Caste)
 *   domain: religious/philosophical/social
 *
 * SUMMARY:
 *   The concept of the Kshatriya's duty, as presented in the Bhagavad Gita,
 *   is a complex moral and philosophical constraint. It mandates that members
 *   of the warrior caste must engage in righteous warfare, even when it
 *   involves conflict with family or loved ones. The justification lies in
 *   upholding dharma (righteousness) and maintaining social order. This often
 *   leads to significant extraction and suppression of individual desires.
 *
 * KEY AGENTS:
 *   - Individual Kshatriyas: Primary target (powerless/trapped) — bears the burden of the duty, forced to fight even against their own will.
 *   - Social Order/Divine Will: Primary beneficiary (institutional/analytical) — benefits from the maintenance of stability and adherence to dharma.
 *   - Warrior Families: Secondary actor (moderate/constrained) — experience both the benefits and costs of the Kshatriya's duty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra, 0.6).
domain_priors:suppression_score(gita_kurukshetra, 0.7).
domain_priors:theater_ratio(gita_kurukshetra, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra, extractiveness, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gita_kurukshetra, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra, "The Duty of the Kshatriya (Warrior Caste)").
narrative_ontology:topic_domain(gita_kurukshetra, "religious/philosophical/social").

domain_priors:requires_active_enforcement(gita_kurukshetra).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra, social_order).
narrative_ontology:constraint_beneficiary(gita_kurukshetra, divine_will).
narrative_ontology:constraint_victim(gita_kurukshetra, individual_kshatriyas).
narrative_ontology:constraint_victim(gita_kurukshetra, family_ties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual warrior, bound by duty, sees this as a snare. He is trapped by his caste and societal expectations, forced to fight even against his own kin.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The warrior family sees a tangled rope. They benefit from the protection and social status afforded by the warrior caste, but also bear the costs of war and potential loss of life.
constraint_indexing:constraint_classification(gita_kurukshetra, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The social order, or the divine will, perceives this as a rope. It's a necessary, albeit sometimes brutal, mechanism for maintaining stability and justice.
constraint_indexing:constraint_classification(gita_kurukshetra, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% In modern times, the concept has been degraded and used to justify aggressive nationalism and violence, losing its original philosophical nuance. Ritualistic adherence persists, but the functional justification is weak.
constraint_indexing:constraint_classification(gita_kurukshetra, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gita_kurukshetra, TR),
    TR >= 0.70.

:- end_tests(gita_kurukshetra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.6) because the individual warrior is significantly constrained, often forced to act against their personal desires. The suppression is also high (0.7) due to the strong social and religious pressures to conform to the Kshatriya's duty. The theater ratio (0.75) indicates that the performance of duty is less about outward show and more about internalized conviction, but in modern times has become more performative.
 *
 * PERSPECTIVAL GAP:
 *   The individual warrior experiences this as a snare, trapped by social and religious expectations. The family experiences a tangled rope, benefiting from the status but also bearing the costs of war. The social order views it as a rope, a necessary mechanism for stability. This difference in perspective highlights the indexical nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Social Order/Divine Will) benefit from social stability and upholding of Dharma (d=0). Victims (Individual Kshatriyas) are forced to participate in violence, even against their kin (d=1). The moderate family sits in between (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as extraction (or vice versa) by acknowledging that 'duty' has both functional (maintaining order) and performative (demonstrating piety) components. Failure to recognize both would lead to misclassification. The high theater ratio and extractiveness justify the mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_interpretation,
    'To what extent is the ''divine will'' a genuine external force, and to what extent is it a construct used to legitimize power structures?',
    'Comparative analysis of different religious and philosophical traditions; historical study of the use of religious justifications for war.',
    'If divine will is external: The duty is more immutable (mountain). If constructed: The duty is more contingent (tangled rope or scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_interpretation, conceptual, 'The nature and interpretation of ''divine will''.').

omega_variable(
    social_stability_requirement,
    'What level of social stability is truly necessary, and at what cost?',
    'Historical analysis of different societies and their levels of stability; cost-benefit analysis of different approaches to maintaining social order.',
    'If high stability required: Extraction justified (tangled rope). If lower stability acceptable: Less extraction needed (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_stability_requirement, preference, 'The necessity and value of social stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra, theater_ratio, 500, 0.4).
narrative_ontology:measurement(theater_ratio_1000, gita_kurukshetra, theater_ratio, 1000, 0.75).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(base_extractiveness_1000, gita_kurukshetra, base_extractiveness, 1000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
