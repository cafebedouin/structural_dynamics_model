% ============================================================================
% CONSTRAINT STORY: china_africa_zero_tariff_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_africa_zero_tariff_2026, []).

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
 *   constraint_id: china_africa_zero_tariff_2026
 *   human_readable: China-Africa Zero-Tariff Trade Framework
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The China-Africa Zero-Tariff Trade Framework, effective May 2026, offers
 *   preferential market access to 53 African nations. While presented as a
 *   mechanism for shared development (a Rope), it operates within a
 *   pre-existing and growing structural trade imbalance where Africa
 *   primarily exports raw materials and imports higher-value manufactured
 *   goods from China. This creates a core tension between its coordination
 *   function (facilitating trade) and its extractive function (reinforcing
 *   dependency and providing geopolitical leverage). The framework's
 *   structure also includes explicit political conditionality, with Eswatini
 *   excluded due to its diplomatic ties with Taiwan.
 *
 * KEY AGENTS:
 *   - Chinese State & Industry: Primary beneficiary (institutional/arbitrage) - Gains market access, resource security, and geopolitical influence.
 *   - Participating African Nations: Secondary beneficiaries/victims (organized/constrained) - Gain market access but risk deeper economic dependency.
 *   - Eswatini Government: Primary victim (organized/trapped) - Excluded on political grounds, bearing the full coercive force of the framework's conditionality.
 *   - Western Trade Blocs (USA, EU): Structural victims (institutional/constrained) - Face diminished market share and geopolitical leverage in Africa.
 *   - Uncompetitive African Domestic Industries: Indirect victims (powerless/trapped) - May struggle to compete with the influx of Chinese goods facilitated by the deepened trade relationship.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_africa_zero_tariff_2026, 0.55).
domain_priors:suppression_score(china_africa_zero_tariff_2026, 0.65).
domain_priors:theater_ratio(china_africa_zero_tariff_2026, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_africa_zero_tariff_2026, tangled_rope).
narrative_ontology:human_readable(china_africa_zero_tariff_2026, "China-Africa Zero-Tariff Trade Framework").
narrative_ontology:topic_domain(china_africa_zero_tariff_2026, "economic/geopolitical").

domain_priors:requires_active_enforcement(china_africa_zero_tariff_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, chinese_state_and_industry).
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, participating_african_exporters).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, eswatini_government).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, western_trade_blocs).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, uncompetitive_african_domestic_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESWATINI (SNARE) - As a nation-state, Eswatini is 'organized' but is 'trapped' outside this framework due to political conditionality (Taiwan relations). The constraint acts as pure coercive extraction of political alignment, offering no coordination benefit and suppressing its access relative to its neighbors. d is derived high from victim+trapped status, leading to high χ and a Snare classification.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CHINA (ROPE) - As the architect, China has 'arbitrage' exit and experiences the framework as a pure coordination mechanism to secure resources, expand markets, and build geopolitical influence. The structural trade imbalance is viewed as a feature, not a bug. d is derived low from beneficiary+arbitrage status, yielding negative χ (net benefit).
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PARTICIPATING AFRICAN NATION (TANGLED ROPE) - Experiences both a genuine coordination benefit (tariff-free market access) and asymmetric extraction (risk of dependency, exacerbation of trade imbalance). Exit is 'constrained' by the high economic and diplomatic costs of withdrawal. This mixed experience is the canonical Tangled Rope.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WESTERN POWERS (TANGLED ROPE) - View the framework as a hybrid of legitimate trade coordination and an extractive geopolitical tool that undermines their own leverage and market access. Their ability to offer a competitive alternative is 'constrained'. They are a structural victim of the shift in influence.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - The system's default view. It recognizes the genuine coordination function (market access) but also the high base extraction (ε=0.55) from the structural trade imbalance and the high suppression (0.65) from political conditionality and lack of alternatives. This confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_africa_zero_tariff_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_africa_zero_tariff_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_africa_zero_tariff_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. This score reflects the significant, persistent trade deficit Africa runs with China, which this framework may exacerbate. The extraction is not a tariff but the structural value transfer from a raw material exporter to a manufacturing superpower. Suppression (0.65): High. The political exclusion of Eswatini is a direct act of suppression. For participants, the deal is so advantageous compared to alternatives (often laden with Western political conditionality) that it suppresses the formation of a unified negotiating bloc or the pursuit of other partnerships. Theater (0.40): Moderate. The 'win-win' and 'South-South cooperation' narrative is a key part of the framework's political maintenance, but it partially masks the underlying extractive imbalance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. China, the architect, sees a beneficial coordination Rope. Eswatini, the excluded target, sees a coercive political Snare. Participating African nations, caught in the middle, experience the quintessential Tangled Rope: a genuine benefit (market access) tangled with a significant structural cost (dependency and trade imbalance). Western competitors also see a Tangled Rope, recognizing the mix of legitimate trade and strategic extraction of influence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the perspectival classifications. China (beneficiary, arbitrage exit) has its effective extraction (χ) driven negative, classifying as Rope. Eswatini (victim, trapped exit) has its χ amplified to the maximum, classifying as Snare. Participating nations (beneficiary/victim, constrained exit) have a moderately high χ, landing squarely in the Tangled Rope category. This demonstrates the system's ability to derive nuanced classifications from structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for resolving mandatrophy. A simplistic analysis might label it a purely benevolent Rope (China's narrative) or a purely predatory Snare (a critical perspective). The Deferential Realism framework, by using indexical classification, shows that both are incomplete. The analytical classification of Tangled Rope correctly identifies the dual nature of the constraint: it possesses both a genuine coordination function and a significant, asymmetric extractive component. The perspectival analysis further clarifies that Rope and Snare are indeed the experienced realities for agents at the structural extremes of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imbalance_entrenchment,
    'Will the zero-tariff framework reduce Africa''s trade deficit with China, or will it entrench the existing pattern of raw material exports vs. manufactured goods imports?',
    'Longitudinal analysis of trade data post-2026, tracking the value-add of African exports to China.',
    'If the deficit shrinks and export composition diversifies, the constraint''s extractiveness (ε) is lower than estimated. If the deficit grows, ε is higher, and the constraint trends towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imbalance_entrenchment, empirical, 'Whether the framework reduces or entrenches the China-Africa trade imbalance.').

omega_variable(
    industrialization_catalyst,
    'Can African nations leverage the preferential market access to build domestic manufacturing and industrial capacity?',
    'Tracking metrics of industrialization (e.g., manufacturing as % of GDP, employment in secondary sectors) in participating countries.',
    'Successful industrialization would indicate a strong coordination function, classifying the constraint as a Scaffold from a developmental perspective. Failure would confirm the extractive Tangled Rope/Snare dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrialization_catalyst, empirical, 'Whether market access translates into genuine industrial capacity building.').

omega_variable(
    geopolitical_vs_economic_driver,
    'Is the primary function of the framework economic development for Africa or securing geopolitical alignment and resource access for China?',
    'Analysis of Chinese state documents, voting patterns of participating African nations in international forums (e.g., UN), and terms of associated infrastructure loans.',
    'If primarily geopolitical, the ''coordination'' function is largely theater, and the constraint is more accurately a Snare. If economic drivers are primary, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_vs_economic_driver, conceptual, 'The primary driver of the framework: economic development vs. geopolitical leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_africa_zero_tariff_2026, 2024, 2036).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t2024, china_africa_zero_tariff_2026, theater_ratio, 2024, 0.5).
narrative_ontology:measurement(chin_tr_t2030, china_africa_zero_tariff_2026, theater_ratio, 2030, 0.45).
narrative_ontology:measurement(chin_tr_t2036, china_africa_zero_tariff_2026, theater_ratio, 2036, 0.4).

% Extraction over time
narrative_ontology:measurement(chin_be_t2024, china_africa_zero_tariff_2026, base_extractiveness, 2024, 0.48).
narrative_ontology:measurement(chin_be_t2030, china_africa_zero_tariff_2026, base_extractiveness, 2030, 0.52).
narrative_ontology:measurement(chin_be_t2036, china_africa_zero_tariff_2026, base_extractiveness, 2036, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_africa_zero_tariff_2026, resource_allocation).
narrative_ontology:affects_constraint(china_africa_zero_tariff_2026, belt_and_road_initiative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
