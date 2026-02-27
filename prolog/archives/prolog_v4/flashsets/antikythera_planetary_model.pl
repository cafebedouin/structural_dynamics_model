% ============================================================================
% CONSTRAINT STORY: antikythera_planetary_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antikythera_planetary_model, []).

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
 *   constraint_id: antikythera_planetary_model
 *   human_readable: Antikythera Mechanism's Geocentric Planetary Model
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the complex, epicycle-based geocentric model
 *   of planetary motion encoded in the Antikythera Mechanism. It highlights
 *   the trade-offs between technological innovation, accuracy, and the
 *   suppression of alternative scientific theories.
 *
 * KEY AGENTS:
 *   - Antikythera Mechanism Makers: Beneficiary (institutional/arbitrage) - Gained prestige and resources through creation.
 *   - Astrology Practitioners: Beneficiary (institutional/constrained) - Used the model for astrological predictions, but limited by the inaccuracy.
 *   - Heliocentric Model Proponents: Victim (powerless/trapped) - Their theories were suppressed by the dominant geocentric view.
 *   - Astronomical Accuracy: Victim (moderate/constrained) - The model's inherent limitations restricted the precision of astronomical predictions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_planetary_model, 0.6).
domain_priors:suppression_score(antikythera_planetary_model, 0.7).
domain_priors:theater_ratio(antikythera_planetary_model, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_planetary_model, extractiveness, 0.6).
narrative_ontology:constraint_metric(antikythera_planetary_model, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(antikythera_planetary_model, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antikythera_planetary_model, tangled_rope).
narrative_ontology:human_readable(antikythera_planetary_model, "Antikythera Mechanism's Geocentric Planetary Model").
narrative_ontology:topic_domain(antikythera_planetary_model, "technological/scientific").

domain_priors:requires_active_enforcement(antikythera_planetary_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antikythera_planetary_model, antikythera_mechanism_makers).
narrative_ontology:constraint_beneficiary(antikythera_planetary_model, astrology_practitioners).
narrative_ontology:constraint_victim(antikythera_planetary_model, heliocentric_model_proponents).
narrative_ontology:constraint_victim(antikythera_planetary_model, astronomical_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Heliocentric model proponents - The geocentric model, when dominant, suppressed the development and acceptance of heliocentric alternatives. They are trapped due to lack of computational power at the time, and societal beliefs.
constraint_indexing:constraint_classification(antikythera_planetary_model, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Astronomical accuracy - The geocentric model, even with epicycles, provides only an approximation of planetary positions. Improved accuracy is constrained by the model's inherent limitations, but benefit came in form of advancements in gear mechanism.
constraint_indexing:constraint_classification(antikythera_planetary_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Antikythera Mechanism Makers - Benefitted directly from the geocentric model as it provided the basis for designing and constructing the mechanism. They had ability to arbitrage by using existing knowledge to create device.
constraint_indexing:constraint_classification(antikythera_planetary_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Perspective 4: Astrology Practitioners - Relied upon planetary positions for astrological predictions. The geocentric model provided a framework, although less accurate than modern models. Became a piton as better models for astrology came about.
constraint_indexing:constraint_classification(antikythera_planetary_model, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analytical Observer - Sees that the model represents a significant advancement in mechanical computing and astronomical understanding for its time, but also hindered the development of more accurate, heliocentric models.
constraint_indexing:constraint_classification(antikythera_planetary_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_planetary_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antikythera_planetary_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_planetary_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antikythera_planetary_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antikythera_planetary_model, TR),
    TR >= 0.70.

:- end_tests(antikythera_planetary_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) due to the suppression of alternative models. Suppression is also high (0.7) due to societal beliefs and technological limitations. The theater ratio is moderate (0.3) since the model provided a functional approximation, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap lies in the model's varying impact. Makers and astrology benefited, while scientific accuracy and heliocentric thinking were suppressed. The analytical observer sees the mechanism's technological achievement and the constraints of the geocentric paradigm.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship of agents to the constraint. Makers and astrology benefit, resulting in low d values and lower extraction; suppressed models experienced high d values and high extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_complexity,
    'To what extent did the computational complexity of heliocentric models limit their adoption?',
    'Historical analysis of scientific literature, computational capabilities, and model adoption rates.',
    'Determines whether geocentrism was primarily a technological limitation or a conceptual choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_complexity, empirical, 'Omega variable: The effect of computational complexity.').

omega_variable(
    societal_acceptance,
    'How did prevailing societal beliefs and philosophical frameworks influence the acceptance of geocentrism?',
    'Analysis of philosophical texts, religious doctrines, and societal norms from the relevant historical period.',
    'Highlights the role of cultural and ideological factors in scientific progress.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(societal_acceptance, conceptual, 'Omega variable: The effect of societal acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_planetary_model, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antikythera_planetary_model, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t50, antikythera_planetary_model, theater_ratio, 50, 0.2).
narrative_ontology:measurement(anti_tr_t100, antikythera_planetary_model, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antikythera_planetary_model, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(anti_be_t50, antikythera_planetary_model, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(anti_be_t100, antikythera_planetary_model, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antikythera_planetary_model, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
