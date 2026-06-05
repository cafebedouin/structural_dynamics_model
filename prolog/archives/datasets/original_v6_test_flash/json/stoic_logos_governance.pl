% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stoic_logos_governance, []).

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
 *   constraint_id: stoic_logos_governance
 *   human_readable: The Stoic Logos as an Imperial Governance Framework
 *   domain: philosophical/political
 *
 * SUMMARY:
 *   The Stoic framework, as practiced by Emperor Marcus Aurelius, represents
 *   a constraint where the ruler submits his personal will to the Logos
 *   (divine reason/natural law) for the good of the state. This submission,
 *   however, creates a framework which is both a source of stability and also
 *   potentially a tool for manipulation. The long-term impacts and
 *   interpretation have broad civilizational ramifications.
 *
 * KEY AGENTS:
 *   - The Emperor (Marcus Aurelius): Primary implementer (institutional/constrained)
 *   - Roman Citizenry: Beneficiaries (moderate/mobile)
 *   - Future Philosophers: Analyzers and Adaptors (analytical/analytical)
 *   - Later Roman Emperors: Potential Degenerators (institutional/constrained)
 *   - Individual Autonomy: Potential Victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stoic_logos_governance, 0.35).
domain_priors:suppression_score(stoic_logos_governance, 0.25).
domain_priors:theater_ratio(stoic_logos_governance, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, 0.35).
narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(stoic_logos_governance, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stoic_logos_governance, piton).
narrative_ontology:human_readable(stoic_logos_governance, "The Stoic Logos as an Imperial Governance Framework").
narrative_ontology:topic_domain(stoic_logos_governance, "philosophical/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stoic_logos_governance, roman_citizens).
narrative_ontology:constraint_beneficiary(stoic_logos_governance, future_philosophers).
narrative_ontology:constraint_victim(stoic_logos_governance, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Emperor, as a Stoic, benefits from increased legitimacy and stability by adhering to the Logos, even though it constrains his personal desires. The constraint is a rope, coordinating his actions with the perceived natural order. While he is powerful, he is also constrained by the expectation to follow Stoic principles.
constraint_indexing:constraint_classification(stoic_logos_governance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Roman citizenry benefits from a ruler who is perceived as just and rational, leading to social stability and reduced arbitrary rule. While they have limited direct power, they are mobile in the sense that dissatisfaction can lead to unrest or migration, constraining the Emperor's actions. The Logos provides a framework for expecting certain conduct.
constraint_indexing:constraint_classification(stoic_logos_governance, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Future philosophers can analyze and adapt the Stoic framework for governance, drawing lessons from its successes and failures. The constraint acts as a temporary support, providing a model for ethical leadership. The scaffold is in place until better models are discovered. The perspective is analytical and universal.
constraint_indexing:constraint_classification(stoic_logos_governance, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Later emperors, who adopted the theater of Stoicism without the substance, found it a degraded framework. The performance of adhering to the Logos became a means of manipulating public opinion, rather than genuine guidance. The Logos becomes a Piton — institutional inertia without function.
constraint_indexing:constraint_classification(stoic_logos_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% For individuals who value personal autonomy above all else, the Stoic Logos can be perceived as a snare, trapping them within a rigid framework of expectations and suppressing their individual desires. They are powerless to change the overarching philosophical framework and trapped within its influence.
constraint_indexing:constraint_classification(stoic_logos_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stoic_logos_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stoic_logos_governance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(stoic_logos_governance, TR),
    TR >= 0.70.

:- end_tests(stoic_logos_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the fact that the Emperor does relinquish personal desires, but does so for personal gain (legitimacy). Suppression (0.25) is relatively low, as there are alternative governance frameworks. Theater Ratio (0.75) reflects the idea that for later emperors, there was a disparity between performance and reality, as they adopted the theater of Stoicism without the substance.
 *
 * PERSPECTIVAL GAP:
 *   The gap stems from the different positions relative to the constraint. Aurelius sees a source of strength and legitimacy. Citizens benefit from perceived just governance. Later emperors see the framework as a tool to manipulate public opinion while philosophers see something to build off of. Individuals who value autonomy may see the Logos as a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the benefit received from the constraint. The Emperor benefits from legitimacy, citizens from stability. Philosophers from the lessons learned while emperors see it as a tool. Individuals may see it as a constraint on their freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint can be misclassified if the performance is mistaken for the genuine adherence to the Logos (Stoic principles). This can result in classifying it as pure coordination when it involves extracting from the Emperor's personal will. Mandatrophy is resolved by analyzing the intent and sincerity of the adherence. The addition of the 'individual autonomy' perspective helps clarify the potential for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincerity_of_adherence,
    'To what extent was the adherence to the Logos genuine versus performative?',
    'Historical analysis of the Emperor''s actions and writings, comparing them to Stoic principles.',
    'If genuine, the constraint is closer to a rope. If performative, it is closer to a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_of_adherence, empirical, 'The degree to which the Emperor''s adherence to the Logos was genuine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stoic_logos_governance, 161, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stoi_tr_t0, stoic_logos_governance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stoi_tr_t5, stoic_logos_governance, theater_ratio, 5, 0.3).
narrative_ontology:measurement(stoi_tr_t10, stoic_logos_governance, theater_ratio, 10, 0.5).
narrative_ontology:measurement(stoi_tr_t15, stoic_logos_governance, theater_ratio, 15, 0.7).
narrative_ontology:measurement(stoi_tr_t19, stoic_logos_governance, theater_ratio, 19, 0.75).

% Extraction over time
narrative_ontology:measurement(stoi_be_t0, stoic_logos_governance, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(stoi_be_t5, stoic_logos_governance, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(stoi_be_t10, stoic_logos_governance, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(stoi_be_t15, stoic_logos_governance, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(stoi_be_t19, stoic_logos_governance, base_extractiveness, 19, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stoic_logos_governance, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
