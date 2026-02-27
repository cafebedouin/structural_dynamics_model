% ============================================================================
% CONSTRAINT STORY: us_ai_chip_export_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_ai_chip_export_controls, []).

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
 *   constraint_id: us_ai_chip_export_controls
 *   human_readable: US Export Controls on Advanced AI Chips to China
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The US export controls on advanced AI chips to China represent a complex
 *   geopolitical and technological constraint. The controls aim to limit
 *   China's access to cutting-edge AI technology, impacting various actors
 *   including US chip manufacturers, Chinese AI companies, and the global
 *   chip supply chain.
 *
 * KEY AGENTS:
 *   - US Chip Manufacturers: Beneficiaries (institutional/arbitrage)
 *   - Chinese AI Companies: Victims (powerless/trapped)
 *   - Global Chip Supply Chain: Constrained Actor (institutional/constrained)
 *   - US National Security: Institutional Actor (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_ai_chip_export_controls, 0.6).
domain_priors:suppression_score(us_ai_chip_export_controls, 0.7).
domain_priors:theater_ratio(us_ai_chip_export_controls, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_ai_chip_export_controls, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_ai_chip_export_controls, tangled_rope).
narrative_ontology:human_readable(us_ai_chip_export_controls, "US Export Controls on Advanced AI Chips to China").
narrative_ontology:topic_domain(us_ai_chip_export_controls, "geopolitical/technological").

domain_priors:requires_active_enforcement(us_ai_chip_export_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_chip_manufacturers).
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_national_security).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chinese_ai_companies).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, global_chip_supply_chain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of Chinese AI companies: Trapped by the export controls, hindering their access to advanced AI chips necessary for training large models.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of US chip manufacturers: Benefit from the controls, as they limit competition from Chinese companies and potentially increase demand for their products from other regions. They also have arbitrage options through selling to non-controlled entities and regions.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of the global chip supply chain: Constrained by the export controls, which disrupt established supply chains and create uncertainty, but also benefits from potential diversification of supply sources. Experiences both extraction and coordination, reflecting its hybrid nature.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical perspective: the US attempts to maintain national security interests in the face of potentially threatening technological development. However, this also creates the tangled-rope effect of constraining the global economy while hoping to ensure US security in the long run.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_ai_chip_export_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_ai_chip_export_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_ai_chip_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.6 - Moderate. The export controls extract significant capabilities from the Chinese AI sector by limiting access to advanced chips. Suppression: 0.7 - High. The controls actively suppress China's ability to develop cutting-edge AI, creating high barriers for innovation. Theater Ratio: 0.3 - Low. The enforcement is less performative and more structural as the controls are actively enforced and monitored.
 *
 * PERSPECTIVAL GAP:
 *   Chinese AI companies experience the constraint as a snare, as they are directly and negatively impacted. US chip manufacturers benefit from reduced competition, experiencing it as a rope. The global chip supply chain experiences a mixed effect (tangled rope) due to disruptions and potential diversification. The US national security establishment sees it as a tool to limit a rival's technological prowess.
 *
 * DIRECTIONALITY LOGIC:
 *   US chip manufacturers and the US are considered beneficiaries with low 'd' values. Chinese companies are victims with high 'd' values. The global chip supply chain has a moderate 'd' value, reflecting its mixed position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_domestic_production,
    'How quickly can China develop domestic capabilities to produce advanced AI chips?',
    'Monitor Chinese semiconductor industry investments and technological advancements.',
    'If China achieves self-sufficiency, the export controls become less effective. If not, they maintain their impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_domestic_production, empirical, 'China''s potential to develop domestic advanced AI chip production.').

omega_variable(
    circumvention_methods,
    'To what extent can Chinese companies circumvent the export controls through third countries or other means?',
    'Track trade flows and investigate potential illicit transactions.',
    'If circumvention is widespread, the effectiveness of the export controls is significantly reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circumvention_methods, empirical, 'The extent to which Chinese companies can circumvent export controls.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_ai_chip_export_controls, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_a_tr_t0, us_ai_chip_export_controls, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_a_tr_t5, us_ai_chip_export_controls, theater_ratio, 5, 0.3).
narrative_ontology:measurement(us_a_tr_t10, us_ai_chip_export_controls, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(us_a_be_t0, us_ai_chip_export_controls, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_a_be_t5, us_ai_chip_export_controls, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(us_a_be_t10, us_ai_chip_export_controls, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_ai_chip_export_controls, enforcement_mechanism).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, global_semiconductor_supply_chain).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, us_china_trade_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
