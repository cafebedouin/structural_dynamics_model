% ============================================================================
% CONSTRAINT STORY: china_export_led_growth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_export_led_growth, []).

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
 *   constraint_id: china_export_led_growth
 *   human_readable: China's State-Directed Export-Led Growth Model
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's state-directed industrial policy prioritizes massive investment
 *   in manufacturing and export-led growth. This model has led to significant
 *   economic growth but also generates trade imbalances and internal social
 *   pressures. State subsidies and market access advantages are provided to
 *   Chinese SOEs, while foreign competitors face suppressed prices and unfair
 *   competition. Chinese labor benefits from job creation but is constrained
 *   by low wages and limited labor rights.
 *
 * KEY AGENTS:
 *   - Chinese State-Owned Enterprises: Primary beneficiaries (institutional/arbitrage)
 *   - Chinese Communist Party: Beneficiary (institutional/constrained)
 *   - Foreign Competitors: Primary victims (powerless/trapped)
 *   - Chinese Labor: Victims (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_export_led_growth, 0.65).
domain_priors:suppression_score(china_export_led_growth, 0.7).
domain_priors:theater_ratio(china_export_led_growth, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_export_led_growth, extractiveness, 0.65).
narrative_ontology:constraint_metric(china_export_led_growth, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(china_export_led_growth, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_export_led_growth, tangled_rope).
narrative_ontology:human_readable(china_export_led_growth, "China's State-Directed Export-Led Growth Model").
narrative_ontology:topic_domain(china_export_led_growth, "economic/political").

domain_priors:requires_active_enforcement(china_export_led_growth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_export_led_growth, chinese_state_owned_enterprises).
narrative_ontology:constraint_beneficiary(china_export_led_growth, chinese_communist_party).
narrative_ontology:constraint_victim(china_export_led_growth, foreign_competitors).
narrative_ontology:constraint_victim(china_export_led_growth, chinese_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Foreign competitors facing suppressed prices and unfair competition in the global market. Trapped as they are unable to match the state subsidized prices.
constraint_indexing:constraint_classification(china_export_led_growth, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Chinese labor benefits from job creation but are constrained by low wages and limited labor rights. They have limited ability to exit due to hukou system and limited alternative employment.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Chinese SOEs benefit from state subsidies and market access. They have arbitrage opportunities due to preferential treatment.
constraint_indexing:constraint_classification(china_export_led_growth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: The CCP benefits from economic growth, maintaining political legitimacy. They are constrained by the need to deliver continued growth and stability.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical perspective viewing the long-term consequences of the export-led growth model, including trade imbalances and environmental degradation. Sees both coordination and extraction elements.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_export_led_growth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_export_led_growth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_export_led_growth, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_export_led_growth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_export_led_growth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High, reflecting state control and the pressure on labor and foreign companies. Suppression (0.70): High, indicating limited competition and constraints on labor rights. Theater Ratio (0.30): Relatively low, as the focus is primarily on tangible economic outputs rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Foreign competitors experience the system as a snare, with limited options to compete. Chinese labor experiences a tangled rope, benefiting from jobs but constrained by limited rights. Chinese SOEs benefit significantly. The CCP views it as a way to maintain power and stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Chinese SOEs receive state support and preferential treatment (low d). Victims: Foreign companies face unfair competition (high d), and Chinese workers face low wages and limited rights (high d). The CCP's relationship is more complex, balancing economic growth with social and political stability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_sustainability,
    'Can the export-led growth model be sustained given increasing global trade tensions and domestic economic challenges?',
    'Analysis of China''s trade policies, technological innovation, and domestic consumption patterns.',
    'If unsustainable: economic instability and political upheaval. If sustainable: continued global trade imbalances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_sustainability, empirical, 'Sustainability of export-led growth.').

omega_variable(
    labor_rights_reform,
    'Will China implement meaningful labor rights reforms to address the exploitation of workers?',
    'Monitoring of labor laws, enforcement mechanisms, and worker organization activities.',
    'If yes: reduced extraction from labor. If no: continued suppression and potential social unrest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_rights_reform, preference, 'Future labor reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_export_led_growth, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_export_led_growth, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chin_tr_t10, china_export_led_growth, theater_ratio, 10, 0.25).
narrative_ontology:measurement(chin_tr_t20, china_export_led_growth, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_export_led_growth, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(chin_be_t10, china_export_led_growth, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(chin_be_t20, china_export_led_growth, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_export_led_growth, resource_allocation).
narrative_ontology:affects_constraint(china_export_led_growth, global_trade_imbalances).
narrative_ontology:affects_constraint(china_export_led_growth, intellectual_property_theft_china).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
