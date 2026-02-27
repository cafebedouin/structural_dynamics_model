% ============================================================================
% CONSTRAINT STORY: global_economic_anxiety_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_economic_anxiety_2026, []).

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
 *   constraint_id: global_economic_anxiety_2026
 *   human_readable: The Global Economic Anxiety Snare
 *   domain: economic/social
 *
 * SUMMARY:
 *   Global economic anxiety indexed at 23-26% concern in 107 countries (2026
 *   Gallup) represents a structural extraction mechanism, not a psychological
 *   anomaly or temporary downturn. The constraint operates through multiple
 *   interlocking extractive pathways: wage suppression in precariat labor
 *   markets, debt service obligations for education and housing, financial
 *   sector rent extraction through interest rate spreads, and political
 *   gatekeeping that constrains redistributive policy alternatives. The
 *   snare's core mechanism is suppression of exit options: precariat workers
 *   cannot exit labor markets without starvation; young adults cannot avoid
 *   debt through education avoidance (credentialing barrier); small
 *   businesses cannot escape high interest rates; climate-displaced
 *   populations have no exit to stable employment. Simultaneously, incumbent
 *   beneficiaries (financial institutions, political gatekeepers) maintain
 *   arbitrage options through capital mobility, regulatory capture, and
 *   policy influence. The theater ratio (0.58) reflects that policy responses
 *   (inflation targeting, forward guidance, stimulus announcements) maintain
 *   performative appearance of addressing anxiety while leaving extraction
 *   mechanisms intact. Progressive coalitions (unions, climate movements,
 *   housing advocates) perceive the constraint as temporary — offering policy
 *   frameworks (UBI, debt jubilee, housing-as-right) that would open exit
 *   pathways. But these frameworks face organized opposition from beneficiary
 *   institutions and remain politically blocked. The macroeconomic policy
 *   establishment treats anxiety as a communication and measurement problem,
 *   maintaining theater through stress testing, financial inclusion metrics,
 *   and policy communication while avoiding structural policy change. From a
 *   civilizational analytical perspective, the risk is naturalizing the snare
 *   as an inherent feature of market economies rather than recognizing it as
 *   a contingent institutional arrangement vulnerable to policy disruption.
 *
 * KEY AGENTS:
 *   - Precariat Workers: Primary victim (powerless/trapped) — gig workers, casual laborers, informal economy. Cannot exit labor markets; face wage suppression, unstable hours, zero benefits. Bear maximum extraction through low wages and high precarity.
 *   - Young Adults Entering Labor Market: Primary victim (powerless/trapped) — student debt, housing unaffordability, compressed wage expectations. Trapped by educational requirement and credit system. Extraction deferred through debt servicing.
 *   - Climate-Displaced Populations: Primary victim (powerless/trapped) — losing livelihoods to climate impacts; face forced migration, labor market discrimination, asset seizure. No exit options; extraction through forced downward mobility.
 *   - Small Business Operators: Secondary victim (moderate/constrained) — depend on consumer demand, face high interest rates and supplier concentration. Constrained exit: cannot easily liquidate or relocate. Extraction through capital costs and supply constraints.
 *   - Financial Sector Institutions: Primary beneficiary (institutional/arbitrage) — extract through interest spreads, debt issuance, capital appreciation. Maximum arbitrage: capital mobility, central bank access, regulatory influence. Experience constraint as profit opportunity.
 *   - Incumbent Political Gatekeepers: Primary beneficiary (institutional/arbitrage) — maintain policy capture, block redistributive alternatives, benefit from status quo. High exit options through revolving-door employment and policy continuity.
 *   - Progressive Policy Coalition: Organized agents (organized/constrained) — unions, climate movements, housing advocates. See snare as solvable through policy; building alternative frameworks; constrained by political opposition.
 *   - Macroeconomic Policy Establishment: Institutional theater (institutional/arbitrage) — central banks, finance ministries. Maintain policy performance without structural change; see anxiety as communication problem. Theater maintained through inertia.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent arrangements as inevitable features of market economies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_economic_anxiety_2026, 0.68).
domain_priors:suppression_score(global_economic_anxiety_2026, 0.72).
domain_priors:theater_ratio(global_economic_anxiety_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_economic_anxiety_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_economic_anxiety_2026, snare).
narrative_ontology:human_readable(global_economic_anxiety_2026, "The Global Economic Anxiety Snare").
narrative_ontology:topic_domain(global_economic_anxiety_2026, "economic/social").

domain_priors:requires_active_enforcement(global_economic_anxiety_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_economic_anxiety_2026, financial_sector_extractors).
narrative_ontology:constraint_beneficiary(global_economic_anxiety_2026, incumbent_political_gatekeepers).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, precariat_workers).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, small_business_operators).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, climate_displaced_populations).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, young_adults_entering_labor_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIAT WORKER (SNARE) — Trapped in gig economy, unstable housing, declining purchasing power. Cannot exit labor markets, cannot avoid debt cycles. Experiences maximum extraction through wage suppression, housing cost inflation, and financial services predation. No alternatives available; anxiety is rational response to structural constraint.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: YOUNG ADULT ENTERING LABOR MARKET (SNARE) — Faces student debt, housing unaffordability, precarious first-job market. Trapped by educational requirements, geographic immobility due to debt, and compressed wage expectations. Extraction through debt servicing and delayed household formation. No generational precedent for exit pathway.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL BUSINESS OPERATORS (TANGLED ROPE) — Benefit from consumer demand coordination and supply chain infrastructure. Simultaneously extracted through high interest rates, supplier concentration, and regulatory compliance costs. Constrained exit: cannot easily liquidate assets or relocate operations. Mixed experience of coordination benefit and extraction burden.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL SECTOR INSTITUTIONS (ROPE) — Extract through interest rate spreads, debt issuance, and risk concentration. Maintain access to unlimited central bank liquidity and regulatory forbearance. Experience constraint as coordination mechanism for wealth concentration. High exit optionality through capital mobility and policy influence.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRESSIVE POLICY COALITION (SCAFFOLD) — Organized actors (labor unions, climate movements, housing advocates) perceive the anxiety as a temporary coordination failure with explicit sunset: universal basic income, debt jubilees, housing-as-right frameworks offer structural exits. See the snare as politically solvable through redistributive policy. Sunset clause: 10-15 years if policy architecture shifts.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MACROECONOMIC POLICY ESTABLISHMENT (PITON) — Central banks and finance ministries treat anxiety as a measurement and communication problem rather than a structural extraction mechanism. Policy theater: inflation targeting, forward guidance, stress testing. Actual function degraded — monetary policy cannot address wage suppression or housing unaffordability. Maintained through institutional inertia and ideological lock-in, not because framework works.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — Risk naturalizing the snare as inevitable: 'inequality is a natural feature of market economies,' 'anxiety is a psychological constant,' 'precarity is inherent to globalization.' This perspective mistakes a contingent institutional arrangement for a law of nature. The engine's false summit detector will identify this as naturalization of extractive policy architecture, not discovery of natural limits.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_economic_anxiety_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_economic_anxiety_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_economic_anxiety_2026, TR),
    TR >= 0.70.

:- end_tests(global_economic_anxiety_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint shows sustained extraction mechanisms across multiple pathways — wage suppression, debt service obligations, financial rent extraction, and political gatekeeping. The 23-26% Gallup anxiety level is not marginal concern; it indicates mainstream awareness of systematic extraction. Measurement trajectory shows increasing extractiveness from 0.52 (baseline) to 0.68 (current), reflecting accumulating debt burdens, housing cost inflation, and credential inflation. This is not a temporary recession but a structural shift in labor market structure and wealth distribution. Suppression (0.72): High. Exit options are systematically blocked: labor market exit requires income (impossible for precariat); geographic exit requires housing capital (impossible when housing is inflated); credential exit requires accepting lower status (real option but carries large cost); political exit requires coalition formation (constrained by atomization of precariat). The suppression is maintained through institutional design (debt structures, labor market regulation, housing policy) rather than explicit coercion. Theater ratio (0.58): Moderate. Policy responses maintain visible action (inflation targeting, stimulus, financial inclusion metrics) while leaving extraction mechanisms intact. Theater has increased from 0.42 to 0.58 over the measurement interval, indicating growing gap between policy performance and structural outcomes. Policy theater masks that monetary policy cannot address wage suppression or housing unaffordability.
 *
 * PERSPECTIVAL GAP:
 *   The snare exhibits five distinct perspectival readings from the same structural data: (1) Precariat perceives pure extraction (Snare) — their structural reality is trapped vulnerability. (2) Small business perceives mixed extraction-coordination (Tangled Rope) — they benefit from some mechanisms while being extracted from others. (3) Financial institutions perceive coordination (Rope) — they experience the snare as a system that solves the collective action problem of concentrating wealth. (4) Progressive coalition perceives temporary problem with solvable exit (Scaffold) — policy frameworks offer genuine alternative pathways if political opposition can be overcome. (5) Policy establishment perceives communication problem (Piton) — maintains theater that anxiety is being addressed through updated policies that actually leave mechanisms intact. (6) Civilizational observer risks perceiving natural law (Mountain) — treating 'inequality is inevitable' or 'precarity is inherent to markets' as natural limits rather than contingent institutional outcomes. The perspectival gap is largest between precariat (Snare) and financial sector (Rope) — they perceive fundamentally different constraint types because they occupy opposite structural positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position in the extraction flow. Powerless/trapped agents (precariat, young adults, displaced populations) occupy positions of maximum extraction extraction — they have no exit options and bear full cost of anxiety-generating mechanisms. Their d values derive from trapped exit + victim status, producing high f(d) and high experienced chi. Moderate/constrained agents (small business operators) experience mixed extraction — they benefit from some coordination functions (supply chains, consumer markets) but face significant extraction burden (capital costs, supplier concentration). Their d values reflect constrained exit + mixed beneficiary/victim status, producing moderate chi. Institutional/arbitrage agents (financial sector, political gatekeepers) experience negative effective extraction — they benefit from the snare's mechanisms and maintain escape options through capital mobility and policy influence. Their d values derive from arbitrage exit + beneficiary status, producing low or negative f(d) and negative chi. The theater ratio reflects that policy establishment treats anxiety as a communication problem (piton perspective) rather than as evidence of extraction mechanism requiring structural policy change.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is a genuine Snare (extractiveness 0.68, suppression 0.72, high victim populations with trapped exit) with multiple institutional overlays that create false classification temptations. The piton perspective (policy theater) is real but secondary — it describes the mechanisms by which the snare is maintained, not its fundamental nature. The scaffold perspective (progressive policy alternatives) is genuine but blocked — it offers real exit pathways that are politically suppressed by beneficiary institutions. The mountain perspective (natural law) is a false summit — the constraint is entirely contingent on institutional arrangements (debt structures, labor market regulation, housing policy) that could be reorganized. The tangled rope classification for small business operators is accurate — they genuinely experience both coordination benefit and extraction burden simultaneously. The rope classification for financial institutions is accurate for their position — they experience the snare as pure coordination solution. None of these perspectival readings is incorrect; they are all structurally accurate from their vantage points. The mandatrophy resolution is: the constraint IS a snare from the perspective of trapped populations, and this is the dominant structural fact. The alternative perspectives capture real institutional complexity, but they do not overturn the primary classification because they describe secondary institutional layers, not the core extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anxiety_causation_chain,
    'Is the 23-26% economic anxiety citing primarily driven by actual income decline, by comparison-based status anxiety, or by information cascades amplifying concern beyond structural economic change?',
    'Multivariate regression: correlation of anxiety levels with objective economic indicators (real wages, housing affordability, employment stability) vs subjective comparison metrics (relative status, social media exposure, media diet); panel analysis of anxiety trajectory before/after major policy announcements',
    'If objective-driven: extraction is real and measurable; snare classification robust. If comparison-driven: snare persists through psychological mechanisms; policy solutions require coordination frame-shift, not only redistribution. If information-cascade-driven: snare is partly self-reinforcing through attention dynamics; circuit-breaker policies (information diet, platform redesign) become co-equal with economic policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anxiety_causation_chain, empirical, 'Whether anxiety is driven by objective economic change, comparison, or information cascades').

omega_variable(
    exit_pathway_feasibility,
    'Do policy frameworks currently proposed by progressive coalitions (UBI, debt jubilee, housing-as-right) actually open exit pathways at scale, or do they represent aspirational scaffold logic that cannot survive political economy constraints?',
    'Pilot program evaluation: Kenyan UBI (GiveDirectly), German housing policies, debt forgiveness experiments; simulation of fiscal capacity under different tax regimes; analysis of political coalition dynamics required to sustain redistributive policy against financial sector opposition',
    'If feasible: scaffold classification is structural; sunset is real and achievable within 10-15 years. If infeasible: scaffold perspective is aspirational theater; snare persists indefinitely because exit pathways remain blocked by institutional gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_pathway_feasibility, empirical, 'Whether proposed policy frameworks can open genuine exit pathways').

omega_variable(
    global_coordination_bind,
    'Can individual nations implement redistributive exit-pathway policies while globally integrated financial systems maintain capital exit optionality and regulatory arbitrage?',
    'Analysis of capital flight response to wealth taxes (France, Spain case studies); simulation of policy effectiveness under varying capital mobility; assessment of international coordination capacity for financial regulation harmonization',
    'If nations can act unilaterally: scaffold is structurally achievable; snare can be escaped through policy. If global coordination is necessary: snare persists until international institutions shift — much lower probability and longer timescale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_coordination_bind, empirical, 'Whether unilateral national policy can overcome global financial system constraints').

omega_variable(
    precariat_coalition_formation,
    'Can the dispersed precariat (gig workers, migrants, young adults, small businesses) form durable coalitions capable of political pressure, or does the atomization built into precarity prevent collective action?',
    'Historical analysis of precariat-led movements (gilets jaunes, platform worker organizing, youth climate movements); network analysis of social movement capacity in different precarity contexts; simulation of strike/slowdown effectiveness under different employment contract structures',
    'If coalition formation succeeds: precariat can upgrade from powerless to organized; snare''s effectiveness declines as victims develop exit capacity. If atomization persists: snare persists because victims remain isolated and uncoordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precariat_coalition_formation, empirical, 'Whether precariat populations can form durable political coalitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_economic_anxiety_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geax_tr_t0, global_economic_anxiety_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(geax_tr_t3, global_economic_anxiety_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(geax_tr_t6, global_economic_anxiety_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(geax_be_t0, global_economic_anxiety_2026, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(geax_be_t3, global_economic_anxiety_2026, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(geax_be_t6, global_economic_anxiety_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_economic_anxiety_2026, resource_allocation).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, housing_affordability_crisis).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, student_debt_accumulation).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, labor_precarity_expansion).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, political_capture_finance).

% DUAL FORMULATION NOTE:
% The global economic anxiety snare is structurally downstream of multiple specific policy constraints (housing policy, education financing, labor regulation, financial deregulation) but represents a distinct constraint in its own right. Individual policy constraints have different ε values and mechanisms; the anxiety snare captures the emergent extraction effect when multiple mechanisms combine to suppress exit options at scale. The network links show causal dependencies: housing policy feeds into anxiety through affordability; education policy feeds through debt burdens; labor policy feeds through precarity; financial policy feeds through extraction mechanisms. Each upstream constraint contributes to the overall snare structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_economic_anxiety_2026, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
