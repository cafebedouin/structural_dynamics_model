% ============================================================================
% CONSTRAINT STORY: ai_superpowers_race_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_superpowers_race_2026, []).

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
 *   constraint_id: ai_superpowers_race_2026
 *   human_readable: The Sino-American AI Implementation Gap
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   The Sino-American AI competition is structured by a fundamental gap: US
 *   leadership in foundational, high-compute 'AGI Moonshots' versus China's
 *   rapid, tenacious deployment of 'Consumer AI' at scale. This constraint is
 *   not a simple race but a complex geopolitical and technological dynamic.
 *   It functions as a global coordination mechanism for advancing AI, but
 *   this function is deeply entangled with extractive practices like data
 *   harvesting, intellectual property control, and the use of state power
 *   (e.g., export controls) to suppress competition. The high theater ratio
 *   reflects the intense public and political narrative-crafting that often
 *   outpaces tangible, deployed capabilities.
 *
 * KEY AGENTS:
 *   - US Frontier AI Labs (e.g., OpenAI): Primary beneficiary (institutional/arbitrage) - leverages compute and capital advantages.
 *   - Chinese Implementation Firms (e.g., 01.ai, Zhipu): Beneficiary/Victim (organized/mobile) - benefits from state support and data access, but targeted by US sanctions.
 *   - Global South Developers: Primary victim (powerless/trapped) - forced to choose between expensive, closed US platforms and politically complex Chinese open-source alternatives.
 *   - US Government (Export Controls): Institutional actor (institutional/constrained) - views its suppressive actions as a temporary scaffold for strategic advantage.
 *   - Legacy Enterprise Adopters: Secondary victim (powerful/constrained) - struggles to distinguish functional AI from performative hype, experiencing the dynamic as a Piton.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_superpowers_race_2026, 0.55).
domain_priors:suppression_score(ai_superpowers_race_2026, 0.7).
domain_priors:theater_ratio(ai_superpowers_race_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_superpowers_race_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_superpowers_race_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_superpowers_race_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_superpowers_race_2026, tangled_rope).
narrative_ontology:human_readable(ai_superpowers_race_2026, "The Sino-American AI Implementation Gap").
narrative_ontology:topic_domain(ai_superpowers_race_2026, "technological/geopolitical").

domain_priors:requires_active_enforcement(ai_superpowers_race_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_superpowers_race_2026, us_frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_superpowers_race_2026, chinese_implementation_firms).
narrative_ontology:constraint_beneficiary(ai_superpowers_race_2026, us_chip_designers).
narrative_ontology:constraint_victim(ai_superpowers_race_2026, global_south_developers).
narrative_ontology:constraint_victim(ai_superpowers_race_2026, chinese_consumers_data_privacy).
narrative_ontology:constraint_victim(ai_superpowers_race_2026, new_market_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH DEVELOPER (SNARE) — Trapped between two ecosystems. The US closed-source model is extractive via high API costs, while China's open-source alternatives come with geopolitical and data privacy risks. They are rule-takers with no exit. High suppression (chip controls, platform lock-in) and high extraction (cost, data) make this a snare. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US FRONTIER AI LAB (ROPE) — Experiences the dynamic as a pure coordination race to achieve AGI. They benefit from access to capital, talent, and superior compute, and see competition as a driver of innovation, not extraction. As a primary beneficiary with arbitrage, their effective extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE IMPLEMENTATION FIRM (TANGLED ROPE) — Experiences both coordination (state support, vast domestic data, engineering talent) and extraction (US sanctions, intense domestic competition, state surveillance demands). They are mobile within their national ecosystem but constrained geopolitically. This mixed role as both beneficiary and victim defines the tangled rope. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: US EXPORT CONTROL REGIME (SCAFFOLD) — From the perspective of the sanctioning body, the high-suppression export controls are a temporary measure to slow a competitor and allow the domestic industry to secure a long-term advantage. The 'sunset clause' is the point at which the strategic lead is deemed secure, making this a scaffold, not a permanent snare. The high suppression is tolerated for a strategic goal.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ENTERPRISE ADOPTER (PITON) — A non-AI-native corporation sees the endless cycle of benchmark-chasing and AGI hype as largely performative. Their own internal AI initiatives are high on theatrical announcements but low on functional, integrated value. The constraint's high theater_ratio (0.75) meets the piton gate, reflecting the gap between marketing claims and real-world utility.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees the complete structure: a genuine coordination function (global technological advancement) intertwined with severe, asymmetric extraction (geopolitical power plays, data harvesting, suppression of competition via sanctions and market barriers). This is the canonical definition of a tangled rope.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_superpowers_race_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_superpowers_race_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_superpowers_race_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_superpowers_race_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_superpowers_race_2026, TR),
    TR >= 0.70.

:- end_tests(ai_superpowers_race_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. Extraction occurs through multiple channels: high API costs from closed US models, mass data collection in China, and the diversion of national resources into a zero-sum geopolitical contest. Suppression (0.70): High. US export controls on advanced semiconductors are a direct and powerful tool of suppression. This is compounded by China's 'Great Firewall' and the immense capital costs required to compete at the frontier, which suppresses new entrants. Theater Ratio (0.75): High. The public discourse is dominated by AGI hype, nationalistic rhetoric, and benchmark supremacy claims that are often disconnected from real-world, deployed value, making the constraint highly performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. A US frontier lab sees a Rope, a pure race for innovation. A Chinese firm sees a Tangled Rope, navigating both state support and foreign sanctions. A developer in a non-aligned nation sees a Snare, trapped between two costly and controlling ecosystems. A US policymaker sees a temporary Scaffold. A legacy company trying to adopt AI sees a Piton of performative hype. The analytical view confirms the Tangled Rope, recognizing that the coordination and extraction elements are inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (US labs, Chinese firms) have low to moderate directionality (d), experiencing the constraint as coordination or a mixed system. Victims (Global South developers, data subjects) have high directionality (d), experiencing it as pure extraction. The US government, as an enforcer, has a unique institutional perspective where its own suppressive actions are framed as a temporary, goal-oriented scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by demonstrating that a single structural reality can be simultaneously a Rope, Snare, Scaffold, Piton, and Tangled Rope depending on the observer's index. The system correctly classifies the analytical perspective as Tangled Rope, acknowledging both the genuine technological coordination and the severe, asymmetric extraction inherent in the geopolitical competition. It resists collapsing the complex dynamic into a simplistic 'good' (Rope) or 'bad' (Snare) classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_parity,
    'Can China''s open-source, application-focused model achieve performance parity with the US''s high-compute, closed-source ''moonshot'' model?',
    'Longitudinal tracking of benchmark performance (e.g., MMLU, HELM) and real-world application capabilities of leading models from both ecosystems.',
    'If parity is achieved, the US compute advantage becomes less decisive, shifting the constraint towards a pure Rope (coordination on open standards). If a persistent gap remains, the US model remains a Snare for those who cannot afford access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_parity, empirical, 'Whether open-source models can reach performance parity with high-compute closed models.').

omega_variable(
    sanction_effectiveness,
    'Are US export controls a temporary ''scaffold'' that secures a long-term lead, or a permanent ''snare'' that accelerates Chinese technological self-sufficiency?',
    'Analysis of China''s domestic chip manufacturing capabilities and software optimization progress over a 5-10 year horizon.',
    'If effective, the US perspective (Scaffold) is validated. If they accelerate self-sufficiency, they backfire and intensify the Tangled Rope by creating two fully independent, competing stacks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanction_effectiveness, empirical, 'The long-term strategic effect of US semiconductor export controls on China''s AI industry.').

omega_variable(
    innovation_philosophy,
    'Is the ''implementation gap'' a temporary lag or a fundamental, persistent difference in innovation philosophy (frontier research vs. mass-market application)?',
    'Comparative analysis of R&D investment allocation, patent filings, and commercialization patterns in both countries.',
    'If it''s a temporary lag, the race is a single-axis competition. If it''s a philosophical difference, the two nations are running different races, and the constraint may resolve into two separate, loosely coupled systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_philosophy, conceptual, 'Whether the US/China AI gap is a temporary lag or a fundamental strategic divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_superpowers_race_2026, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_superpowers_race_2026, theater_ratio, 0, 0.6).
narrative_ontology:measurement(ai_s_tr_t4, ai_superpowers_race_2026, theater_ratio, 4, 0.7).
narrative_ontology:measurement(ai_s_tr_t8, ai_superpowers_race_2026, theater_ratio, 8, 0.75).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_superpowers_race_2026, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_s_be_t4, ai_superpowers_race_2026, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(ai_s_be_t8, ai_superpowers_race_2026, base_extractiveness, 8, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_superpowers_race_2026, global_infrastructure).
narrative_ontology:affects_constraint(ai_superpowers_race_2026, semiconductor_supply_chain).
narrative_ontology:affects_constraint(ai_superpowers_race_2026, global_data_privacy_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
