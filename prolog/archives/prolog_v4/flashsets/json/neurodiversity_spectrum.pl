% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neurodiversity_spectrum, []).

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
 *   constraint_id: neurodiversity_spectrum
 *   human_readable: The Social/Medical Model of the Neurodiversity Spectrum
 *   domain: social/medical
 *
 * SUMMARY:
 *   The social/medical model of the neurodiversity spectrum reframes
 *   neurodevelopmental variation (e.g., autism, ADHD) from a binary "normal
 *   vs. abnormal" perspective to a spectrum of natural human variation. This
 *   model emphasizes both the social barriers faced by neurodivergent
 *   individuals and the biological basis of these differences. It has led to
 *   increased awareness and acceptance, but also introduces potential for
 *   extraction related to identity politics and medicalization.
 *
 * KEY AGENTS:
 *   - Neurodiversity Advocates: Benefit from increased awareness and acceptance (institutional/arbitrage).
 *   - Some Neurodivergent Individuals: Benefit from increased community and resources, but may also experience pressure to conform (moderate/constrained). Some may experience the spectrum framing and associated interventions as coercive and invalidating (powerless/trapped).
 *   - Traditional Psychiatric Establishment: Face challenges to their traditional diagnostic categories (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.45).
domain_priors:suppression_score(neurodiversity_spectrum, 0.4).
domain_priors:theater_ratio(neurodiversity_spectrum, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neurodiversity_spectrum, extractiveness, 0.45).
narrative_ontology:constraint_metric(neurodiversity_spectrum, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(neurodiversity_spectrum, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neurodiversity_spectrum, tangled_rope).
narrative_ontology:human_readable(neurodiversity_spectrum, "The Social/Medical Model of the Neurodiversity Spectrum").
narrative_ontology:topic_domain(neurodiversity_spectrum, "social/medical").

domain_priors:requires_active_enforcement(neurodiversity_spectrum).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, neurodiversity_advocates).
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, some_neurodivergent_individuals).
narrative_ontology:constraint_victim(neurodiversity_spectrum, some_neurodivergent_individuals).
narrative_ontology:constraint_victim(neurodiversity_spectrum, traditional_psychiatric_establishment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: Neurodivergent Individual with negative experiences (SNARE). Some neurodivergent individuals experience the 'spectrum' framing and associated interventions (e.g., ABA) as coercive and invalidating, suppressing their authentic selves. Limited exit options due to societal stigma and dependence on support systems that may reinforce the model.
constraint_indexing:constraint_classification(neurodiversity_spectrum, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: Neurodiversity Advocacy Organizations (ROPE). These organizations benefit from the increased awareness and acceptance of neurodiversity, which enables them to secure funding, influence policy, and provide support services. They effectively arbitrage the social and political landscape to advance their mission.
constraint_indexing:constraint_classification(neurodiversity_spectrum, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: Neurodivergent Individual with positive experiences (TANGLED ROPE). Some neurodivergent individuals benefit from the social model by gaining access to communities, resources, and self-understanding. However, they may also be constrained by the pressure to conform to certain narratives or expectations within the neurodiversity movement, and the medical model pushes for treatments that may or may not be helpful or wanted.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: Traditional Psychiatric Establishment (PITON). The neurodiversity spectrum challenges the traditional diagnostic categories and treatment approaches of psychiatry. While some within the establishment have adapted, others resist the shift, leading to a degraded, inertial state where outdated practices persist alongside newer, more inclusive approaches. They are constrained by existing diagnostic and treatment paradigms. The increased theater ratio reflects the performative adoption of neurodiversity language without fundamental changes in practice.
constraint_indexing:constraint_classification(neurodiversity_spectrum, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: Analytical Observer (TANGLED ROPE). From an analytical perspective, the neurodiversity spectrum is a tangled rope. It offers benefits of increased awareness and acceptance, but also introduces new forms of extraction related to identity politics, medicalization of difference, and the potential for performative allyship. The observer sees both the coordination and extraction functions.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodiversity_spectrum_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(neurodiversity_spectrum, TR),
    TR >= 0.70.

:- end_tests(neurodiversity_spectrum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. The neurodiversity spectrum can lead to extraction through identity politics (commodification of neurodivergent identity) and medicalization of difference (pressure to seek treatment even when not desired). Suppression (0.40): Moderate. The spectrum framing can suppress individual expression and autonomy if it is used to force conformity or medical interventions. Theater ratio (0.75): High. There is increasing performative adoption of neurodiversity language without fundamental changes in practice, especially within the traditional psychiatric establishment.
 *
 * PERSPECTIVAL GAP:
 *   The neurodiversity spectrum is viewed differently by different actors. Advocates see it as a positive force for change (Rope). Some neurodivergent individuals benefit from it, while others experience it as coercive (Snare). The traditional psychiatric establishment may view it as a challenge to their authority (Piton). An analytical observer sees the tangled rope nature of it all.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocacy organizations benefit (low d). Some neurodivergent individuals benefit but are also constrained (moderate d). Some experience it as purely extractive (high d). The psychiatric establishment is also constrained, as their authority is being challenged. The analytical observer sees both positive and negative aspects.
 *
 * MANDATROPHY ANALYSIS:
 *   The neurodiversity spectrum is a complex issue that cannot be reduced to a single type. It has elements of both coordination (increased awareness, community building) and extraction (identity politics, medicalization). By considering multiple perspectives, we can avoid misclassifying it as either purely beneficial or purely harmful. This model emphasizes the inherent tension of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essential_vs_constructed,
    'To what extent is neurodiversity an essential, biologically-rooted characteristic vs. a socially-constructed category?',
    'Longitudinal studies tracing the developmental origins of neurodivergent traits; cross-cultural comparisons of diagnostic criteria and social perceptions.',
    'If essential: interventions should focus on accommodation and support. If constructed: interventions should focus on dismantling oppressive social structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_vs_constructed, empirical, 'The nature of neurodiversity: essential vs. constructed').

omega_variable(
    diagnostic_threshold_validity,
    'Are current diagnostic thresholds for neurodevelopmental conditions valid and reliable, or do they inadvertently pathologize normal variation?',
    'Large-scale studies correlating diagnostic status with functional outcomes and subjective well-being; receiver operating characteristic (ROC) analysis to determine optimal diagnostic cutoffs.',
    'If valid: current diagnostic practices are justified. If invalid: diagnostic criteria need revision to reduce false positives and negatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_threshold_validity, empirical, 'Validity of diagnostic thresholds').

omega_variable(
    intervention_efficacy_harm,
    'What is the long-term efficacy and potential harm of interventions marketed to neurodivergent individuals (e.g., ABA, social skills training, medication)?',
    'Randomized controlled trials with long-term follow-up; qualitative studies capturing the lived experiences of individuals undergoing these interventions.',
    'If efficacious and safe: interventions should be widely accessible. If harmful or ineffective: interventions should be discouraged or modified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_efficacy_harm, empirical, 'Efficacy and harm of neurodivergent interventions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neurodiversity_spectrum, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neur_tr_t0, neurodiversity_spectrum, theater_ratio, 0, 0.5).
narrative_ontology:measurement(neur_tr_t5, neurodiversity_spectrum, theater_ratio, 5, 0.6).
narrative_ontology:measurement(neur_tr_t10, neurodiversity_spectrum, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(neur_be_t0, neurodiversity_spectrum, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(neur_be_t5, neurodiversity_spectrum, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(neur_be_t10, neurodiversity_spectrum, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neurodiversity_spectrum, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
