% ============================================================================
% CONSTRAINT STORY: brazilian_regional_leadership
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazilian_regional_leadership, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazilian_regional_leadership
 *   human_readable: Brazilian Regional Leadership Constraint
 *   domain: geopolitical/regional_hegemon/south_american_dynamics
 *
 * SUMMARY:
 *   Brazilian regional leadership within South America represents a Tangled
 *   Rope constraint: genuine regional coordination functions (MERCOSUR trade
 *   integration, currency stabilization, infrastructure networks, diplomatic
 *   alliance) coexist with asymmetric extraction mechanisms (terms-of-trade
 *   imbalance, institutional capture of supranational bodies, pressure on
 *   smaller states' policy autonomy). Brazil's regional dominance is
 *   structurally enabled by geography, demographic scale, and economic size,
 *   but is actively maintained through enforcement mechanisms including trade
 *   conditionality, diplomatic pressure, and blocking alternative regional
 *   governance models. The constraint has intensified since 2000
 *   (extractiveness rising from 0.42 to 0.58) as Brazil's institutional
 *   capacity for regional management has grown and as peripheral states' exit
 *   options have narrowed. The theater_ratio increase reflects rising
 *   performative activity in regional institutions (UNASUR declarations,
 *   PROSUR meetings, OAS resolutions) while real bargaining power
 *   concentrates in bilateral Brazil-state relationships. Peripheral states
 *   face suppression mechanisms including geographic proximity (no arbitrage
 *   options), currency dependence, infrastructure lock-in, and asymmetric
 *   diplomatic leverage. Organized actors (regional coalitions, integration
 *   movements) see this as a temporary constraint with sunset pathways
 *   (distributed governance models, Chinese alignment, alternative trade
 *   arrangements), but structural power asymmetries limit these agents'
 *   actual agency.
 *
 * KEY AGENTS:
 *   - Brazilian State Apparatus: Primary beneficiary (institutional/arbitrage) — captures term-of-trade advantage, institutional influence, and regional diplomatic leadership; retains arbitrage options toward Atlantic, Lusophone, or Asian alignment
 *   - Brasilia Economic Elite: Primary beneficiary (powerful/arbitrage) — agribusiness, mining, and finance sectors benefit from regional market access and protected market position
 *   - Peripheral States (Paraguay, Bolivia, Uruguay): Primary victims (powerless/trapped) — geographic proximity eliminates arbitrage; dependent on Brazilian market access and currency stability; face retaliation for regional autonomy moves
 *   - Intermediate Powers (Colombia, Ecuador, Venezuela): Secondary victims (moderate/constrained) — constrained by exit costs but retaining some diplomatic agency; divided loyalties between Brazilian and US alignment
 *   - Regional Integration Coalitions (UNASUR, ALBA, PROSUR): Organized agents (organized/constrained) — build alternative governance pathways; see sunset mechanism in distributed supranational bodies; constrained by Brazilian institutional dominance
 *   - OAS and Multilateral Institutions: Institutional theater (institutional/arbitrage) — maintain façade of neutral regional governance while Brazilian/US influence shapes outcomes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks reading Brazilian dominance as geographic inevitability rather than contingent political-economic arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazilian_regional_leadership, 0.58).
domain_priors:suppression_score(brazilian_regional_leadership, 0.65).
domain_priors:theater_ratio(brazilian_regional_leadership, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazilian_regional_leadership, extractiveness, 0.58).
narrative_ontology:constraint_metric(brazilian_regional_leadership, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brazilian_regional_leadership, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazilian_regional_leadership, tangled_rope).
narrative_ontology:human_readable(brazilian_regional_leadership, "Brazilian Regional Leadership Constraint").
narrative_ontology:topic_domain(brazilian_regional_leadership, "geopolitical/regional_hegemon/south_american_dynamics").

domain_priors:requires_active_enforcement(brazilian_regional_leadership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazilian_regional_leadership, brazilian_state_apparatus).
narrative_ontology:constraint_beneficiary(brazilian_regional_leadership, brasilia_economic_elite).
narrative_ontology:constraint_victim(brazilian_regional_leadership, peripheral_south_american_states).
narrative_ontology:constraint_victim(brazilian_regional_leadership, regional_coordination_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL SOUTH AMERICAN STATE (SNARE) — Structurally dependent on Brazilian market access, infrastructure, and currency stability. Geographic proximity eliminates arbitrage options. Exit attempts (regional trade blocs, Chinese alignment) face Brazilian economic pressure and diplomatic retaliation. Maximum extraction with minimal coordination benefit for the trapped agent.
constraint_indexing:constraint_classification(brazilian_regional_leadership, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERMEDIATE REGIONAL POWER (TANGLED ROPE) — Derives genuine coordination benefits from Brazilian economic integration (trade, infrastructure investment, currency coordination), but constrained by asymmetric bargaining power. Can exit toward US alignment or non-aligned postures but at significant cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(brazilian_regional_leadership, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAZILIAN STATE APPARATUS (ROPE) — Benefits from regional coordination function (MERCOSUR, infrastructure linkages, currency area stabilization). Experiences leadership role as mutually beneficial coordination mechanism with exit options (can shift focus to Atlantic or Lusophone alignment). Net beneficiary, but genuine coordination function exists.
constraint_indexing:constraint_classification(brazilian_regional_leadership, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL INTEGRATION COALITIONS (SCAFFOLD) — UNASUR, ALBA, and PROSUR represent organized attempts to build alternative regional coordination bypassing Brazilian dominance. Low extraction because organized agents have agency and see a sunset: distributed governance models and autonomous regional institutions would reduce Brazilian leverage. Temporary constraint with transitional institutional design.
constraint_indexing:constraint_classification(brazilian_regional_leadership, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: OAS AND PAN-AMERICAN INSTITUTIONS (PITON) — These multilateral bodies maintain a theater of regional autonomy and neutral coordination while Brazilian and US influence shape outcomes. The institutional machinery persists (reports, committees, declarations) but the real bargaining power flows through bilateral relationships. Theatrical maintenance of regional governance without corresponding functional autonomy.
constraint_indexing:constraint_classification(brazilian_regional_leadership, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOGRAPHIC NATURALIZATION (MOUNTAIN) — From civilizational scale, Brazilian regional leadership appears as geographic inevitability: Brazil's size, population, and resource endowment make regional dominance a natural law of geography and economics. This perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will flag this as naturalization of historical/political choices as immutable laws.
constraint_indexing:constraint_classification(brazilian_regional_leadership, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazilian_regional_leadership_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazilian_regional_leadership, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazilian_regional_leadership, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazilian_regional_leadership, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazilian_regional_leadership, TR),
    TR >= 0.70.

:- end_tests(brazilian_regional_leadership_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Brazil captures asymmetric benefits from regional integration — favorable terms of trade in manufactured goods, institutional control of MERCOSUR supranational bodies, and preservation of policy autonomy while constraining peripheral states. However, extractiveness is not maximal (0.70+) because genuine coordination functions exist: MERCOSUR has produced sustained trade integration, currency linkages enable macroeconomic stability for smaller economies, and infrastructure projects provide real public goods. The extraction exists alongside coordination, not instead of it. Suppression (0.65): High. Peripheral states face multiple suppression mechanisms: geographic proximity eliminates exit options, economic dependence on Brazilian markets and currency creates lock-in, institutional rules of MERCOSUR preserve Brazilian dominance, and asymmetric diplomatic leverage enables pressure. However, suppression is not total — regional coalitions (UNASUR, ALBA) represent organized resistance, and external alternatives (US, China) reduce total suppression below 0.80. Theater ratio (0.62): Moderate-high. Regional institutions perform autonomy and neutral governance while real bargaining power flows through bilateral relationships. OAS resolutions and MERCOSUR declarations maintain the appearance of collective decision-making, but the outcomes track Brazilian preferences. The rise in theater_ratio over the interval reflects increasing performative activity relative to institutional capacity — regional bodies have proliferated but their functional role in constraint distribution has declined.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence between the beneficiary's experience (Rope — genuine coordination) and the trapped agent's experience (Snare — pure extraction). The Brazilian state experiences regional leadership as solving coordination problems: aligning exchange rates, enabling trade integration, providing infrastructure links. Peripheral states experience the same arrangement as coercive subordination: facing market domination, policy constraints, and blocked alternatives. Intermediate powers see Tangled Rope — real economic benefits alongside real constraints. Organized regional coalitions see Scaffold — a temporary arrangement being bypassed through alternative governance designs. The OAS sees Piton — maintaining theatrical autonomy without corresponding institutional function. The analytical observer risks Mountain — seeing geography as destiny rather than institutional choice. The perspectival gap reveals that the same constraint is coordination from the perspective of agents with exit options and extraction from the perspective of agents without them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the extraction flow. The Brazilian state apparatus occupies beneficiary status with high exit options (arbitrage) — it can redirect regional focus toward Atlantic, BRICS, or bilateral relationships if regional extraction becomes unprofitable. This produces low d (≈0.15), yielding low/negative f(d) and minimal experienced chi despite moderate base extractiveness. Peripheral states occupy victim status with trapped exit options — no geographic alternatives, economic dependence, and retaliation threats mean they cannot exit even if extraction increases. This produces high d (≈0.92), yielding high f(d) and maximum experienced chi. Intermediate powers have moderate exit options (US alignment, Chinese trade) but face cost, producing d ≈0.55 and moderate experienced chi. The institutional theater (OAS, MERCOSUR bodies) has arbitrage-like options but reduced practical leverage, producing d ≈0.25 and piton-level classification despite coordination-type institutional status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how regional dominance distributes differently across the observation site. From Brazil's institutional perspective, regional leadership solves genuine coordination problems (Rope). From peripheral states' perspective, the same arrangement enforces subordination (Snare). Neither reading is wrong — they measure different extraction vectors from different positions in the power asymmetry. The mandatrophy resolves when we recognize that the constraint's classification depends on observer position and exit options. Saying 'Brazilian regional leadership IS a tangled rope' risks naturalizing the beneficiary's reading as universal. The engine prevents this by requiring perspectival measurement from both beneficiary and victim contexts — if they diverge (Rope vs Snare), the tangled rope classification from the analytical view captures the hybrid truth: this arrangement coordinates for some agents while extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependent_development_vs_structural_subordination,
    'Does Brazilian regional leadership enable development (dependency theory variant) or enforce structural subordination that precludes autonomous industrialization?',
    'Long-term comparative development trajectories of peripheral vs non-Brazilian-aligned regional states; measurement of autonomous export diversification and technological capacity building within vs outside the Brazilian sphere',
    'If development-enabling: constraint classifies as Rope from more perspectives. If structurally subordinating: constraint remains Snare for peripheral states with no genuine alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependent_development_vs_structural_subordination, conceptual, 'Dependent development versus structural subordination mechanism').

omega_variable(
    mercosur_asymmetry_source,
    'Is MERCOSUR''s asymmetry inherent to scale differentials or actively enforced through Brazilian institutional capture of supranational bodies?',
    'Institutional analysis of MERCOSUR decision-making; comparison of decision outcomes to Brazilian explicit preferences; counterfactual analysis of alternative institutional designs',
    'If institutional capture: manipulation and enforcement mechanisms are visible and targetable for reform. If scale-inherent: asymmetry persists regardless of institutional design choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mercosur_asymmetry_source, empirical, 'Source of MERCOSUR asymmetry').

omega_variable(
    chinese_alignment_viability,
    'Do Chinese trade and infrastructure investments (Belt and Road) offer genuine exit from Brazilian-dominated regional constraints, or do they create new asymmetric dependencies?',
    'Medium-term (10-20 year) comparison of peripheral state autonomy and domestic capacity under Chinese vs Brazilian alignment; measurement of technology transfer, skill development, and supply-chain control',
    'If genuine exit: peripheral states have alternative leverage and Brazilian constraint weakens. If new dependency: Chinese alignment substitutes one snare for another, increasing overall suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_alignment_viability, empirical, 'Chinese alignment as alternative or substitution').

omega_variable(
    us_counterbalance_credibility,
    'Does US alignment offer peripheral states genuine leverage against Brazilian dominance or merely substitute Atlantic dominance for regional dominance?',
    'Historical analysis of US policy toward Brazilian-aligned peripheral states; measurement of actual US support for autonomy vs support for Brazil-compatible outcomes',
    'If credible counterbalance: exit option for peripheral states upgrades from trapped to constrained. If substitution: suppression remains high but source shifts from Brazilian to Atlantic pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_counterbalance_credibility, empirical, 'US counterbalance credibility against Brazilian dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazilian_regional_leadership, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brl_tr_t0, brazilian_regional_leadership, theater_ratio, 0, 0.48).
narrative_ontology:measurement(brl_tr_t10, brazilian_regional_leadership, theater_ratio, 10, 0.58).
narrative_ontology:measurement(brl_tr_t20, brazilian_regional_leadership, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(brl_be_t0, brazilian_regional_leadership, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(brl_be_t10, brazilian_regional_leadership, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(brl_be_t20, brazilian_regional_leadership, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazilian_regional_leadership, resource_allocation).
narrative_ontology:affects_constraint(brazilian_regional_leadership, mercosur_institutional_asymmetry).
narrative_ontology:affects_constraint(brazilian_regional_leadership, south_american_supply_chain_dependency).
narrative_ontology:affects_constraint(brazilian_regional_leadership, peripheral_state_policy_autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brazilian_regional_leadership, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
