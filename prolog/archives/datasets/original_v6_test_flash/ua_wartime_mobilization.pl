% ============================================================================
% CONSTRAINT STORY: ua_wartime_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ua_wartime_mobilization, []).

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
 *   constraint_id: ua_wartime_mobilization
 *   human_readable: Wartime Mobilization Law in Ukraine
 *   domain: political/military
 *
 * SUMMARY:
 *   In response to the full-scale Russian invasion, the Ukrainian state
 *   enacted and enforces a general mobilization law. This law mandates the
 *   conscription of eligible male citizens (typically 18-60) into the armed
 *   forces, with limited exceptions. While necessary for national defense,
 *   the mobilization law also entails significant costs for individuals, the
 *   economy, and society.
 *
 * KEY AGENTS:
 *   - Ukrainian Male Population: Primary target (powerless/trapped) - bears the cost of conscription, potential injury or death, and disruption of life.
 *   - Ukrainian Armed Forces: Primary beneficiary (institutional/arbitrage) - receives manpower to defend the country.
 *   - Ukrainian State: Secondary beneficiary (institutional/constrained) - benefits from enhanced military strength but constrained by economic and social costs.
 *   - Ukrainian Economy: Secondary target (moderate/constrained) - suffers from labor shortages and economic disruption, but benefits from territorial defense.
 *   - Analytical Observer: Sees the mobilization law as a necessary but costly measure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_wartime_mobilization, 0.7).
domain_priors:suppression_score(ua_wartime_mobilization, 0.8).
domain_priors:theater_ratio(ua_wartime_mobilization, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_wartime_mobilization, extractiveness, 0.7).
narrative_ontology:constraint_metric(ua_wartime_mobilization, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ua_wartime_mobilization, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ua_wartime_mobilization, tangled_rope).
narrative_ontology:human_readable(ua_wartime_mobilization, "Wartime Mobilization Law in Ukraine").
narrative_ontology:topic_domain(ua_wartime_mobilization, "political/military").

domain_priors:requires_active_enforcement(ua_wartime_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, ukrainian_armed_forces).
narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, ukrainian_state).
narrative_ontology:constraint_victim(ua_wartime_mobilization, ukrainian_male_population).
narrative_ontology:constraint_victim(ua_wartime_mobilization, ukrainian_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Individual Ukrainian Male (Snare) - Limited exit options (legally trapped within the country), high suppression of alternatives (mandatory conscription), and high extraction (risk of death/injury, loss of income, disruption of life).
constraint_indexing:constraint_classification(ua_wartime_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: The Ukrainian Economy (Tangled Rope) - Constrained by the loss of labor, but benefits from the defense of its territory and sovereignty. Experiences both extraction (economic disruption) and coordination (defense against aggression).
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: The Ukrainian Armed Forces (Rope) - Benefits from the influx of manpower, enabling it to defend the country. Experiences the constraint as coordination – solving a collective action problem of national defense.
constraint_indexing:constraint_classification(ua_wartime_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: The Ukrainian State (Tangled Rope) - Benefits from increased military strength and territorial defense, but constrained by the economic costs and social disruption of mobilization. Active enforcement is required to maintain conscription.
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: The Analytical Observer (Tangled Rope) - Sees the mobilization law as a necessary but costly measure for national survival. Recognizes both the coordination benefits (national defense) and the extraction costs (individual and economic).
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_wartime_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ua_wartime_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ua_wartime_mobilization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ua_wartime_mobilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ua_wartime_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70): High. The mobilization law imposes significant costs on individuals, including the risk of death or injury, loss of income, and disruption of life. Suppression (0.80): Very High. The law mandates conscription with limited exceptions, effectively suppressing alternative options for eligible citizens. Theater Ratio (0.30): Low. The mobilization is primarily functional, focused on generating military manpower for national defense, with relatively little performative or theatrical activity.
 *
 * PERSPECTIVAL GAP:
 *   The individual Ukrainian male experiences the mobilization law as a snare, with limited exit options and high extraction. The Ukrainian Armed Forces experience it as a rope, enabling them to defend the country. The Ukrainian state and economy experience it as a tangled rope, with both benefits and costs. The analytical observer sees the overall picture and recognizes the trade-offs involved.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural relationship of each agent to the mobilization law. Individuals targeted for conscription experience high extraction, while the armed forces that receive manpower benefit. The state and economy experience both benefits and costs, resulting in intermediate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by recognizing the complex interplay of coordination and extraction. While the mobilization law is necessary for national defense (coordination), it also imposes significant costs on individuals and the economy (extraction). A pure coordination or extraction classification would fail to capture this complexity. The tangled rope classification reflects the inherent trade-offs and the active enforcement required to maintain the mobilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What will be the long-term economic impact of mass mobilization on Ukraine''s future economic prospects?',
    'Economic modeling and analysis of post-war recovery scenarios, considering factors such as labor force participation, investment, and technological innovation.',
    'If the economic impact is severe, it could lead to long-term decline and social instability, requiring significant international aid and reconstruction efforts. If manageable, Ukraine can rebuild its economy and integrate with European markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Long-term economic impact of mobilization').

omega_variable(
    social_and_demographic_consequences,
    'What will be the social and demographic consequences of mass mobilization, including potential brain drain, gender imbalances, and psychological trauma?',
    'Sociological studies and demographic analysis of post-war population trends, considering migration patterns, birth rates, and mental health outcomes.',
    'If the social and demographic consequences are severe, it could undermine Ukraine''s social fabric and future development. If relatively mild, Ukraine can address these challenges through targeted social programs and reconciliation efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_and_demographic_consequences, empirical, 'Social and demographic consequences of mobilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ua_wartime_mobilization, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ua_w_tr_t0, ua_wartime_mobilization, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ua_w_tr_t6, ua_wartime_mobilization, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ua_w_tr_t12, ua_wartime_mobilization, theater_ratio, 12, 0.3).

% Extraction over time
narrative_ontology:measurement(ua_w_be_t0, ua_wartime_mobilization, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ua_w_be_t6, ua_wartime_mobilization, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ua_w_be_t12, ua_wartime_mobilization, base_extractiveness, 12, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ua_wartime_mobilization, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
