% ============================================================================
% CONSTRAINT STORY: chinese_overcapacity_offloading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chinese_overcapacity_offloading, []).

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
 *   constraint_id: chinese_overcapacity_offloading
 *   human_readable: Chinese Overcapacity Offloading to Developing Markets
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Chinese overcapacity offloading represents a structural extraction
 *   mechanism operating through financial intermediation and market
 *   saturation. Chinese manufacturers with excess production capacity,
 *   subsidized by state policy and export credit, export products and finance
 *   infrastructure projects to developing economies at terms that appear
 *   mutually beneficial in the short term but generate long-term debt
 *   obligations and deindustrialization in recipient countries. The
 *   constraint exhibits hybrid coordination-extraction dynamics: genuine
 *   coordination function (Chinese firms solve overcapacity; recipient
 *   countries gain infrastructure) coupled with asymmetric extraction (debt
 *   cycles, technology lock-in, suppression of domestic industrial capacity).
 *   The theater ratio indicates that this mechanism operates with moderate
 *   performativity — formal development narratives (South-South cooperation,
 *   mutual benefit) mask the underlying extraction structure, but not
 *   entirely; debt dynamics and deindustrialization are increasingly visible.
 *
 * KEY AGENTS:
 *   - Chinese Manufacturers and State Actors: Primary beneficiary (institutional/arbitrage) — offload excess capacity and capture long-term market share in developing economies; exit optionality through capital reallocation or market switching
 *   - Domestic Industries in Recipient Countries: Primary victim (powerless/trapped) — cannot compete against state-subsidized Chinese production; face market elimination or subordination to Chinese supply chains
 *   - Recipient Country Governments: Secondary victim/partial beneficiary (moderate/constrained) — gain infrastructure and short-term employment but incur debt obligations that constrain future fiscal space and industrial policy
 *   - Regional Trade Organizations and Alternative Financing: Organized agents (organized/mobile) — building parallel capacity and financing structures that reduce Chinese dependency over generational timescales
 *   - Multilateral Trade Architecture: Institutional observer (institutional/arbitrage) — maintains formal anti-dumping and trade dispute mechanisms but lacks enforcement capacity against state-backed financing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-driven extraction as comparative advantage or natural capital flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chinese_overcapacity_offloading, 0.58).
domain_priors:suppression_score(chinese_overcapacity_offloading, 0.65).
domain_priors:theater_ratio(chinese_overcapacity_offloading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chinese_overcapacity_offloading, extractiveness, 0.58).
narrative_ontology:constraint_metric(chinese_overcapacity_offloading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(chinese_overcapacity_offloading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chinese_overcapacity_offloading, tangled_rope).
narrative_ontology:human_readable(chinese_overcapacity_offloading, "Chinese Overcapacity Offloading to Developing Markets").
narrative_ontology:topic_domain(chinese_overcapacity_offloading, "economic/geopolitical").

domain_priors:requires_active_enforcement(chinese_overcapacity_offloading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chinese_overcapacity_offloading, chinese_manufacturers).
narrative_ontology:constraint_beneficiary(chinese_overcapacity_offloading, chinese_state_actors).
narrative_ontology:constraint_victim(chinese_overcapacity_offloading, developing_market_industries).
narrative_ontology:constraint_victim(chinese_overcapacity_offloading, domestic_labor_markets_recipient_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC MANUFACTURERS IN RECIPIENT COUNTRIES (SNARE) — Trapped by product dumping, predatory pricing, and debt-financed infrastructure that locks them into long-term Chinese supply dependencies. Local firms cannot exit the market or compete on price without matching unsustainable capital injection. Maximum extraction experienced; suppression through debt cycles and market saturation.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HOST GOVERNMENT / RECIPIENT COUNTRY (TANGLED ROPE) — Benefits from immediate infrastructure and employment, but incurs long-term debt obligations and deindustrialization risk. Constrained by limited capital alternatives and IMF-driven fiscal pressures. Genuine coordination function (financing gap closure) coupled with asymmetric extraction (debt servicing, technology lock-in, loss of industrial base). Active enforcement through debt conditionality.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHINESE STATE AND MANUFACTURING SECTOR (ROPE) — Pure coordination from this angle: excess capacity is destructive domestically; offloading to emerging markets solves both the Chinese firm and the recipient country's capacity gap. Experiences the constraint as mutually beneficial coordination. High exit optionality (can redirect capital to other markets or domestic reallocation). Net beneficiary with minimal coercion experienced from their position.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL TRADE ORGANIZATIONS AND DEVELOPMENT ALTERNATIVES (SCAFFOLD) — Organized actors (ASEAN, African Union, alternative financing through development banks) are building parallel infrastructure financing and manufacturing capacity models. See the overcapacity offloading as a temporary coordination gap that will be solved by diversified financing and regional capacity development. Sunset clause: as regional manufacturing and alternative financing mature (10-15 years), dependency on Chinese offloading diminishes. Mobile exit through cooperation and scale.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO AND MULTILATERAL TRADE ARCHITECTURE (PITON) — Traditional trade dispute mechanisms and anti-dumping protocols persist in form but have degraded in function. Theater ratio 0.48 reflects that formal anti-dumping cases are filed but enforcement is weak against state-backed financing. The multilateral system maintains rules and procedures while actual trade flows operate through geopolitical and financial channels that bypass these mechanisms. Institutional inertia rather than functional capacity.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE VIEW (MOUNTAIN) — From a long-run equilibrium perspective, capital flows from high-savings to capital-scarce economies as a natural feature of global financial markets. Overcapacity offloading appears as an immutable consequence of differential savings rates and demographic structures. However, the structural data contradicts this naturalization — suppression is 0.65 and active enforcement is required; this suggests the constraint is contingent on policy choices (Chinese industrial policy, recipient country debt servicing obligations) rather than natural economic law.
constraint_indexing:constraint_classification(chinese_overcapacity_offloading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chinese_overcapacity_offloading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chinese_overcapacity_offloading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chinese_overcapacity_offloading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chinese_overcapacity_offloading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chinese_overcapacity_offloading, TR),
    TR >= 0.70.

:- end_tests(chinese_overcapacity_offloading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the measurement interval. Initial value 0.35 reflects that early Chinese investments appeared largely beneficial (infrastructure gap closure). Current value 0.58 reflects growing evidence of debt trap dynamics, market saturation, and deindustrialization in recipient countries. The trajectory shows extraction mechanism strengthening as debt service obligations consume fiscal space and lock-in dependencies mature. Suppression (0.65): Moderate-high. Recipient countries face barriers to exit including: limited alternative financing sources, debt conditionality forcing continued Chinese engagement, infrastructure that requires Chinese expertise/parts, political pressure from both Chinese actors and Western institutions (IMF) limiting autonomy. Suppression is structural, not primarily internalized, though recipient governments sometimes justify dependency through development framing. Theater ratio (0.48): Moderate. The narrative framing of Chinese engagement emphasizes mutual benefit, South-South cooperation, and development partnership (theater). But debt dynamics are increasingly transparent in policy discussions and media coverage. The theater is present but not dominant — unlike pure pitons (theater > 0.70), the extraction mechanism is not primarily maintained through performative mystification. Theater increased from 0.32 to 0.48 as projects matured and debt obligations became visible.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) perspectives is acute. Chinese actors see a coordination solution — excess capacity matched to capital-scarce markets, mutual benefit. Domestic manufacturers see pure extraction — market elimination through subsidized competition, no path to retain industrial capacity. Recipient governments see mixed dynamics — infrastructure gain coupled with debt burden and deindustrialization risk. The organized coalition (regional trade bodies) sees a temporary gap that will be solved as alternative financing matures. The analytical observer who naturalizes this as comparative advantage or market equilibrium is committing a false summit — the suppression value (0.65) and active enforcement requirement reveal policy choices, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by beneficiary/victim status and exit capacity. Chinese state/manufacturers are beneficiaries with high exit optionality (arbitrage) — they can reallocate capital, redirect exports, or access alternative markets. This produces low d and low/negative chi from their perspective, confirming Rope classification (coordination benefit). Domestic manufacturers in recipient countries are victims with no exit (trapped) — they cannot compete against subsidized imports or relocate their industries. This produces high d and high chi, confirming Snare classification. Recipient governments are partial beneficiaries (infrastructure gain) and partial victims (debt obligation) with constrained but non-zero exit capacity — they could theoretically refuse Chinese financing or default on debt, but face retaliation and loss of infrastructure. This produces moderate d and moderate chi, confirming Tangled Rope classification. The organized coalition has exit capacity through alternative financing and capacity development, producing lower chi despite moderate victimization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy through hybrid coordination-extraction structure. The genuine coordination function (solving both Chinese overcapacity and recipient capital scarcity) justifies Rope or Scaffold classifications from beneficiary perspectives. The asymmetric extraction (debt lock-in, deindustrialization, technology suppression) justifies Snare and Tangled Rope classifications from victim perspectives. The resolution is perspectival: the constraint IS both coordination and extraction simultaneously. The mandatrophy is resolved by recognizing that Tangled Rope is the analytically coherent classification from the neutral observer position — it explicitly contains both functions. The beneficiary Rope and victim Snare are partial truths that suppress the other agent's structural reality. The Piton classification (degraded multilateral trade architecture) is diagnostically important because it shows why the extraction persists — the institutional mechanisms designed to constrain it (anti-dumping rules, trade disputes) have atrophied under geopolitical pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_sustainability_threshold,
    'At what debt-to-GDP ratio does Chinese-financed infrastructure transition from productive investment to extraction mechanism?',
    'Longitudinal analysis of recipient country fiscal trajectories; correlation between infrastructure debt service and domestic public investment capacity loss',
    'If threshold < 40% DTG: many projects are already extractive. If threshold > 70% DTG: extraction mechanism remains covert until fiscal crisis emerges. Classification shifts from Tangled Rope toward Snare as debt burden increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt sustainability threshold for infrastructure extraction').

omega_variable(
    technology_transfer_degree,
    'What degree of genuine technology transfer occurs in Chinese manufacturing projects versus supplier lock-in masquerading as partnership?',
    'Analysis of intellectual property ownership, independent operation capacity, and post-project local workforce technical capability',
    'If transfer is genuine: coordination function is stronger, Rope classification more valid. If lock-in is primary: extraction component is disguised, Snare classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_degree, empirical, 'Degree of technology transfer versus supplier lock-in').

omega_variable(
    domestic_reindustrialization_capacity,
    'Can recipient countries build alternative manufacturing capacity fast enough to compete with Chinese offloading before debt servicing consumes fiscal space?',
    'Comparative trajectory analysis: manufacturing employment, FDI diversification, debt service burden across recipient countries with different industrial policies',
    'If yes (timeframe < 10 years): Scaffold sunset is realistic, constraint weakens. If no (timeframe > 20 years): Snare becomes locked in across generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_reindustrialization_capacity, empirical, 'Domestic reindustrialization capacity relative to debt dynamics').

omega_variable(
    state_subsidy_attribution,
    'What portion of Chinese export pricing advantage derives from state subsidies (export credit, industrial policy) versus genuine efficiency or comparative advantage?',
    'Forensic analysis of export financing terms, domestic supply chain subsidies, and counterfactual pricing under market-rate capital costs',
    'If state subsidy > 60%: offloading is policy-driven extraction, requires active enforcement (validates Tangled Rope). If state subsidy < 20%: constraint approaches natural market process (Rope toward Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_subsidy_attribution, empirical, 'State subsidy attribution in Chinese export pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chinese_overcapacity_offloading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coc_tr_t0, chinese_overcapacity_offloading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(coc_tr_t5, chinese_overcapacity_offloading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(coc_tr_t10, chinese_overcapacity_offloading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(coc_be_t0, chinese_overcapacity_offloading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coc_be_t5, chinese_overcapacity_offloading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(coc_be_t10, chinese_overcapacity_offloading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chinese_overcapacity_offloading, resource_allocation).
narrative_ontology:affects_constraint(chinese_overcapacity_offloading, debt_trap_dynamics).
narrative_ontology:affects_constraint(chinese_overcapacity_offloading, industrial_policy_lock_in).
narrative_ontology:affects_constraint(chinese_overcapacity_offloading, developing_market_deindustrialization).

% DUAL FORMULATION NOTE:
% This constraint is downstream of Chinese domestic industrial policy (excess capacity production) but upstream of specific recipient country debt dynamics. The overcapacity offloading mechanism is the structural intermediary converting policy-driven production excess into extraction dependencies in recipient markets. Decomposition is possible: separate stories for Chinese industrial policy (ε ≈ 0.42, Tangled Rope) and recipient country debt servicing (ε ≈ 0.72, Snare) exist as distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
