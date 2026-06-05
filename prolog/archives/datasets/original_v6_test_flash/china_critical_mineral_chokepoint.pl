% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_critical_mineral_chokepoint, []).

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
 *   constraint_id: china_critical_mineral_chokepoint
 *   human_readable: China's Strategic Chokepoint in Critical Mineral Processing
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   China's strategic dominance over the processing and supply of critical
 *   minerals (e.g., rare earths, gallium, germanium) creates a global
 *   chokepoint. This dominance allows China to exert significant economic and
 *   political influence over nations dependent on these minerals for
 *   manufacturing and technological advancement. This chokepoint is
 *   reinforced by a combination of factors, including state subsidies,
 *   environmental policies that favor domestic processing, and the
 *   accumulation of technological expertise.
 *
 * KEY AGENTS:
 *   - Chinese Mineral Processing Industry: Primary beneficiary (institutional/arbitrage) – Benefits from economies of scale, government subsidies, and technological expertise.
 *   - China State-Owned Enterprises: Secondary beneficiary (institutional/arbitrage) – Benefits from government support and strategic control over mineral resources.
 *   - Downstream Manufacturing Industries: Primary victim (powerless/trapped) – Trapped due to reliance on processed minerals from China with limited alternatives.
 *   - Mineral Exporting Nations: Secondary victim (moderate/constrained) – Constrained by price controls and lack of diversification.
 *   - Geopolitical Rivals: Tertiary victim (powerful/constrained) – constrained due to dependence on China for strategic resources.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.75).
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.8).
domain_priors:theater_ratio(china_critical_mineral_chokepoint, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, extractiveness, 0.75).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_critical_mineral_chokepoint, snare).
narrative_ontology:human_readable(china_critical_mineral_chokepoint, "China's Strategic Chokepoint in Critical Mineral Processing").
narrative_ontology:topic_domain(china_critical_mineral_chokepoint, "economic/political/technological").

domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, chinese_mineral_processing_industry).
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, china_state_owned_enterprises).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, downstream_manufacturing_industries).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, mineral_exporting_nations).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, geopolitical_rivals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream industries (e.g., electric vehicles, electronics) are trapped due to reliance on processed minerals from China. Limited alternatives and high switching costs make them vulnerable to supply disruptions and price manipulation.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Mineral exporting nations are constrained. They benefit from exporting raw materials to China, but are vulnerable to price controls and lack diversification in processing capabilities.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Chinese mineral processing benefits from economies of scale, government subsidies, and technological expertise, allowing them to dictate global supply and pricing.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Geopolitical rivals are heavily constrained by dependence on China. Diversification is possible in the long run, but they are strategically vulnerable in the short and medium term to political manipulation of mineral supplies.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observers recognize the strategic importance and the potential for long-term instability if diversification measures are not actively taken by other countries.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_critical_mineral_chokepoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_critical_mineral_chokepoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_critical_mineral_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. China extracts significant value from its control over mineral processing. This extraction is reflected in its ability to set prices, control supply, and influence downstream industries. Suppression (0.80): High. Limited alternatives for processing and the high costs of establishing competing facilities create significant barriers to entry and suppress competition. Theater ratio (0.30): Low. While some rhetoric exists about 'win-win' cooperation, the dominant role of China is quite clear.
 *
 * PERSPECTIVAL GAP:
 *   Downstream industries experience the situation as a snare. They have limited options and are highly vulnerable. Mineral exporting nations experience it as a Tangled Rope. They have some agency but are largely dependent on China's processing capacity. China's industry views its position as a Rope, enabling efficient resource allocation and supply. The Chokepoint exerts asymmetric influence on Geopolitical Rivals. China is the center of a coercive trade regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream industries (victims with trapped exit options) bear the full cost. China’s state-owned enterprises (beneficiaries with arbitrage exit options) extract the benefit. Other actors experience a mixed or constrained position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling as simple coordination. Although China provides a service by processing the minerals, the terms are dictated by them, generating a structural vulnerability for other states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_processing_viability,
    'How viable are alternative processing technologies outside of China, considering environmental regulations, costs, and scalability?',
    'Technological and economic feasibility studies, pilot plant projects in other countries, comparative cost analysis',
    'If viable: the chokepoint can be bypassed. If not viable: dependence will continue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_processing_viability, empirical, 'Viability of alternative mineral processing technologies').

omega_variable(
    resource_nationalization,
    'Will mineral-rich nations outside China invest in domestic processing capabilities or continue exporting raw materials?',
    'Political analysis, government policy tracking, investment trends in processing facilities',
    'If domestic processing increases: China''s market share diminishes. If raw material exports continue: chokepoint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_nationalization, preference, 'Investment in domestic processing by mineral-rich nations').

omega_variable(
    substitution_potential,
    'To what extent can substitute materials or alternative technologies reduce the demand for critical minerals?',
    'Material science research, technology forecasting, market analysis of substitute materials',
    'If significant substitution is possible: the strategic importance of these minerals declines. If substitution is limited: dependence will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_potential, empirical, 'Potential for substitute materials or alternative technologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_critical_mineral_chokepoint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_critical_mineral_chokepoint, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chin_tr_t5, china_critical_mineral_chokepoint, theater_ratio, 5, 0.25).
narrative_ontology:measurement(chin_tr_t10, china_critical_mineral_chokepoint, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_critical_mineral_chokepoint, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(chin_be_t5, china_critical_mineral_chokepoint, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(chin_be_t10, china_critical_mineral_chokepoint, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_critical_mineral_chokepoint, resource_allocation).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, rare_earth_element_monopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
