% ============================================================================
% CONSTRAINT STORY: straight_coercion_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_straight_coercion_2025, []).

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
 *   constraint_id: straight_coercion_2025
 *   human_readable: Normalized Taiwan Strait Military Coercion
 *   domain: political/military
 *
 * SUMMARY:
 *   By 2025, Chinese military activity around Taiwan has decoupled from
 *   external political triggers (signaling) and shifted to internal readiness
 *   cycles and training schedules (preparation). This 'normalization' of
 *   coercion poses a challenge for regional stability. The increased
 *   frequency of military exercises, patrols, and airspace incursions by the
 *   People's Liberation Army (PLA) serves to both intimidate Taiwan and
 *   improve the PLA's operational capabilities. This normalized coercion aims
 *   to erode Taiwanese public confidence, strain Taiwan's defense resources,
 *   and create a fait accompli scenario where military pressure becomes the
 *   new normal.
 *
 * KEY AGENTS:
 *   - PLA High Command: Primary beneficiary (institutional/arbitrage) - enhances readiness and justifies resource allocation.
 *   - Taiwanese Population: Primary victim (powerless/trapped) - experiences constant military pressure with limited exit options.
 *   - Taiwan Defense Capabilities: Secondary victim (moderate/constrained) - constrained by coercion but also adapts and improves.
 *   - Regional Stability: Undermined actor (institutional/constrained) - facade maintained for diplomacy.
 *   - Chinese Nationalist Sentiment: Beneficiary (organized/constrained) - benefits from perceived strength/resolve shown by the PLA.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(straight_coercion_2025, 0.6).
domain_priors:suppression_score(straight_coercion_2025, 0.7).
domain_priors:theater_ratio(straight_coercion_2025, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(straight_coercion_2025, extractiveness, 0.6).
narrative_ontology:constraint_metric(straight_coercion_2025, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(straight_coercion_2025, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(straight_coercion_2025, snare).
narrative_ontology:human_readable(straight_coercion_2025, "Normalized Taiwan Strait Military Coercion").
narrative_ontology:topic_domain(straight_coercion_2025, "political/military").

domain_priors:requires_active_enforcement(straight_coercion_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(straight_coercion_2025, pla_high_command).
narrative_ontology:constraint_beneficiary(straight_coercion_2025, chinese_nationalist_sentiment).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwanese_population).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_defense_capabilities).
narrative_ontology:constraint_victim(straight_coercion_2025, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taiwanese population experiences constant military pressure as a snare, with limited exit options and vulnerability to coercion.
constraint_indexing:constraint_classification(straight_coercion_2025, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Taiwan's defense capabilities are constrained by the coercion, but also adapt and improve, resulting in a Tangled Rope classification. They have some constrained agency to respond, but cannot fully escape the extraction.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The PLA High Command benefits from normalized coercion by enhancing readiness and justifying resource allocation, experiencing it as a form of coordination (Rope).
constraint_indexing:constraint_classification(straight_coercion_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Regional stability is undermined, but the facade of 'peaceful development' is maintained for diplomatic reasons, creating a piton effect where the stated goal is undermined by the actions.
constraint_indexing:constraint_classification(straight_coercion_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% From an analytical perspective, the situation is a tangled rope: China exerts coercive pressure (extraction), but this also serves a coordination function for internal military readiness and nationalist sentiment.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(straight_coercion_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(straight_coercion_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(straight_coercion_2025, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(straight_coercion_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(straight_coercion_2025, TR),
    TR >= 0.70.

:- end_tests(straight_coercion_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is 0.60 because the coercion extracts a significant cost from Taiwan in terms of defense resources, public confidence, and political autonomy. The suppression is 0.70 because the constant military pressure limits Taiwan's options and suppresses its ability to exercise self-determination. The theater ratio is 0.30 because while there is some performative aspect to the exercises, the primary purpose is to improve military readiness and exert real pressure.
 *
 * PERSPECTIVAL GAP:
 *   The Taiwanese population experiences a snare due to their limited exit options and vulnerability. Taiwan's defense capabilities see a tangled rope because they are both constrained and adaptive. The PLA high command views the situation as a rope, as it serves a coordination function for military readiness. The regional stability perspective views it as a piton because the stated goal of peaceful development is undermined by the coercive actions. From an analytical perspective, the coercion serves a coordination function (military readiness) but is also deeply extractive for Taiwan and the region.
 *
 * DIRECTIONALITY LOGIC:
 *   The PLA high command benefits from the enhanced readiness and justification for resource allocation, leading to a low directionality value. The Taiwanese population bears the cost of the military pressure, resulting in a high directionality value. Taiwan's defense capabilities are moderately affected, resulting in an intermediate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging the coordination function the coercion serves for the PLA in terms of military readiness and resource allocation. However, the significant extractive cost to Taiwan and the region justifies the overall classification as a snare or tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_conventionality,
    'At what point does increased PLA activity become normalized to the point of being indistinguishable from routine training exercises?',
    'Analysis of frequency, location, and type of PLA military activities, compared against historical baselines and stated training objectives.',
    'If normalization is high, the coercion is more effective in suppressing Taiwanese autonomy. If normalization is low, the coercion serves only as a signaling mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_conventionality, empirical, 'Threshold where coercion blends into routine activity.').

omega_variable(
    taiwans_adaptive_capacity,
    'How effectively can Taiwan adapt its defense capabilities and resilience to counter the normalized coercion?',
    'Assessment of Taiwanese defense spending, military modernization efforts, civil defense preparedness, and international partnerships.',
    'Higher adaptive capacity shifts the classification towards a tangled rope. Lower adaptive capacity reinforces the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taiwans_adaptive_capacity, empirical, 'Taiwan''s ability to resist/adapt to the coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(straight_coercion_2025, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stra_tr_t0, straight_coercion_2025, theater_ratio, 0, 0.5).
narrative_ontology:measurement(stra_tr_t2, straight_coercion_2025, theater_ratio, 2, 0.4).
narrative_ontology:measurement(stra_tr_t5, straight_coercion_2025, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(stra_be_t0, straight_coercion_2025, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stra_be_t2, straight_coercion_2025, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(stra_be_t5, straight_coercion_2025, base_extractiveness, 5, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(straight_coercion_2025, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
