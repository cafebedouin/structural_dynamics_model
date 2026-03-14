% ============================================================================
% CONSTRAINT STORY: digital_divide_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_divide_expansion, []).

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
 *   constraint_id: digital_divide_expansion
 *   human_readable: Digital Divide Expansion Through Infrastructure Asymmetry
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The digital divide expansion represents a structural constraint
 *   maintaining and amplifying inequality through asymmetric infrastructure
 *   access, pricing mechanisms, and data extraction. Unlike the narrative of
 *   inevitable technological diffusion (which naturalizes the divide as a
 *   temporary lag), the actual constraint combines genuine coordination
 *   functions (infrastructure deployment solves legitimate collective action
 *   problems) with extractive mechanisms (market-driven prioritization of
 *   profitable regions, monopoly pricing, data harvesting). The divide has
 *   expanded despite increases in absolute internet penetration because gains
 *   concentrate in wealthy populations while unconnected populations face
 *   escalating barriers: device costs rise, service prices reflect market
 *   consolidation, and the skill requirements for digital participation
 *   increase. The constraint manifests differently across geographies: in
 *   developed nations, the divide is primarily about low-income exclusion
 *   through pricing; in developing nations, it reflects colonial
 *   infrastructure patterns and foreign monopoly control. Measurements show
 *   increasing theater ratio as digital equity initiatives proliferate while
 *   remaining performative (grant-funded device distribution without
 *   sustained connectivity; training programs without structural employment
 *   pathways) and extractiveness increases as data collection and targeted
 *   pricing become more sophisticated.
 *
 * KEY AGENTS:
 *   - Rural unconnected populations: Primary victim (powerless/trapped) — geographically isolated from infrastructure rollout; no exit from constraint; subsidize urban expansion through taxes while receiving zero digital services
 *   - Low-income urban households: Secondary victim (moderate/constrained) — infrastructure exists but pricing excludes; face high device costs, recurring subscription extraction, and data harvesting; can theoretically relocate or work around constraints but at prohibitive cost
 *   - Technology infrastructure companies: Primary beneficiary (institutional/arbitrage) — profit from market-driven prioritization of wealthy markets; capture value through monopoly pricing and data monetization; coordinate legitimate infrastructure expansion but extract through pricing strategies
 *   - Government regulators: Hybrid actor (powerful/mobile) — coordinate broadband policy and spectrum allocation; captured by industry lobbying; prioritize profitable deployment over universal coverage; maintain extractive pricing through regulatory passivity
 *   - Developing nations: Powerful but structurally dependent (powerful/arbitrage) — benefit from infrastructure access but through foreign-controlled systems; face digital colonialism and data extraction; arbitrage possible but costly
 *   - Community broadband initiatives: Degraded coordination (organized/constrained) — original function of grassroots digital access increasingly performative; grant-funded theater without sustained funding; outcompeted by incumbent scale advantage
 *   - Analytical observer: Risk of naturalizing the divide as inevitable technological lag rather than contingent institutional arrangement (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_divide_expansion, 0.58).
domain_priors:suppression_score(digital_divide_expansion, 0.65).
domain_priors:theater_ratio(digital_divide_expansion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_divide_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_divide_expansion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(digital_divide_expansion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_divide_expansion, tangled_rope).
narrative_ontology:human_readable(digital_divide_expansion, "Digital Divide Expansion Through Infrastructure Asymmetry").
narrative_ontology:topic_domain(digital_divide_expansion, "economic/social/technological").

domain_priors:requires_active_enforcement(digital_divide_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_divide_expansion, technology_incumbents).
narrative_ontology:constraint_beneficiary(digital_divide_expansion, high_bandwidth_regions).
narrative_ontology:constraint_victim(digital_divide_expansion, rural_unconnected_populations).
narrative_ontology:constraint_victim(digital_divide_expansion, low_income_urban_households).
narrative_ontology:constraint_victim(digital_divide_expansion, developing_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL UNCONNECTED HOUSEHOLDS (SNARE) — Geographically trapped without access infrastructure; face insurmountable barriers to digital participation. Cannot exit the constraint; bears full extraction cost. Infrastructure rollout focuses on profitable urban markets; rural populations subsidize through tax contributions that fund urban expansion, receiving nothing in return. Zero degrees of freedom within biographical horizon.
constraint_indexing:constraint_classification(digital_divide_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOW-INCOME URBAN POPULATIONS (TANGLED ROPE) — Infrastructure exists but device costs and service subscriptions extract significant income share. Genuine coordination benefit: internet access enables employment search, educational access, civic participation. But extraction overlay: service providers practice price discrimination; data harvesting monetizes customer profiles; surveillance through usage tracking. Constrained exit — could theoretically relocate or work around constraints, but at high cost.
constraint_indexing:constraint_classification(digital_divide_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY INFRASTRUCTURE COMPANIES (ROPE) — Experience the digital divide as a pure coordination mechanism. Building fiber networks, 5G coverage, and data centers solves collective action problems: users want connectivity, companies want subscribers, governments want economic development. Profitable markets (dense urban centers, wealthy nations) get served through market mechanisms. Extraction flows toward these actors as net beneficiaries, but the coordination function is genuine.
constraint_indexing:constraint_classification(digital_divide_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT REGULATORY INSTITUTIONS (TANGLED ROPE) — Coordinate broadband policy and universal service obligations. Genuine function: set standards, allocate spectrum, incentivize infrastructure investment. Asymmetric extraction: captured by industry preferences through lobbying and revolving-door hiring; prioritize profitable expansion over universal coverage; establish price floors that exclude low-income users. Mobile exit-in-principle (could implement different policies) but constrained in practice by industry influence and budgetary pressure.
constraint_indexing:constraint_classification(digital_divide_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY BROADBAND AND DIGITAL EQUITY INITIATIVES (PITON) — Formed to address the divide through cooperative networks, municipal fiber, and device distribution programs. Original function: coordinate grassroots digital access. Degraded through institutional inertia: many initiatives operate as performative theater — grant-funded device giveaways without sustained connectivity, training programs without job placement infrastructure, community networks competing with subsidized incumbent providers. Theater ratio high because the initiatives exist to signal commitment to equity while the structural constraint (profitable market incentives) remains unchanged. Exit constrained by funding dependence and lack of scale to challenge incumbent power.
constraint_indexing:constraint_classification(digital_divide_expansion, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPING NATIONS AND THEIR POPULATIONS (TANGLED ROPE) — Coordination benefit: mobile internet leapfrogs fixed-line infrastructure, enabling access to markets, education, financial services, and government services. Extraction overlay: foreign companies control infrastructure and extract data/capital; pricing reflects international standards rather than local income; digital colonialism perpetuates dependency on Western platforms and services. Powerful (at state level) but with high extraction through structural dependence; arbitrage possible (build competing infrastructure) but costly and politically difficult.
constraint_indexing:constraint_classification(digital_divide_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing the divide as inherent to technology diffusion: innovation starts in wealthy, dense markets and gradually expands; digital access follows natural adoption curves; lag is a law of technological progress. This framing masks the contingent institutional arrangements (infrastructure investment prioritizes profit over universality, spectrum allocation concentrates power, service pricing reflects extractive maximization). The analytical observer must actively resist this naturalizing frame.
constraint_indexing:constraint_classification(digital_divide_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_divide_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_divide_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_divide_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_divide_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_divide_expansion, TR),
    TR >= 0.70.

:- end_tests(digital_divide_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple channels: infrastructure investment prioritizes profitable markets, extracting opportunity from unconnected regions; monopoly pricing extracts income from low-income users in areas with infrastructure; data harvesting extracts behavioral data from digitized populations. The coefficient reflects that substantial coordination benefit exists (internet access genuinely enables economic participation, education, civic engagement) alongside systematic extraction through market structuring. Suppression (0.65): High. Multiple barriers prevent exit: geographic isolation from infrastructure, device costs averaging 5-20% of monthly income for unconnected populations, subscription prices consuming 10-30% of income for low-income users, skill barriers requiring digital literacy investments, and regulatory barriers (spectrum is allocated to incumbent companies, municipal broadband is often legally restricted). These are not absolute—some alternatives exist—but collectively create very high suppression. Theater ratio (0.48): Moderate. The constraint has real coordination content (infrastructure deployment is genuinely difficult and expensive; investment incentives matter). Theater appears in digital equity initiatives (performative device distribution, training without employment pathways, public WiFi without sustained funding) and in regulatory framing (universal service commitments that are not enforced; bridge-the-divide rhetoric without structural policy change). The theater is not dominant—coordination function is primary—but it is substantial and increasing.
 *
 * PERSPECTIVAL GAP:
 *   The structural gap between powerless and institutional perspectives is maximal. The same technological infrastructure—fiber optic cables, wireless networks, data centers—appears as pure extraction to unconnected rural populations (Snare) and pure coordination to technology companies (Rope). This gap is not a matter of different values or interpretations; it is structural. The rural household truly receives no benefit and has no exit. The company truly solves the coordination problem of connecting distributed users and devices. The gap reflects that market-driven infrastructure investment creates genuine winners and losers. Intermediate perspectives (moderate/constrained, powerful/mobile, organized/constrained) show blended classifications reflecting their mixed structural positions. The piton perspective (community initiatives) shows how original coordination intent degrades into theater. The mountain perspective reveals the risk of naturalizing this contingent arrangement as inevitable technological diffusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: Technology incumbents declared as primary beneficiary. Their power is institutional, exit is arbitrage (can shift markets, invest in different geographies, change business models). Victim declaration: rural unconnected populations, low-income urban households, developing nations. Rural populations are powerless/trapped (no geographic arbitrage, no substitute infrastructure). Low-income urban households are moderate/constrained (can theoretically relocate or work around, but costs are prohibitive). Derivation chain produces: beneficiaries with arbitrage exit → low d (0.1-0.2) → negative or low f(d) → low effective extraction experienced by them. Victims with trapped/constrained exit → high d (0.70-1.0) → high f(d) → high effective extraction experienced by them. Regulatory capture implicit in government perspective: institutional power but constrained by industry influence; declared as victim in terms of policy capture but beneficiary in terms of governance authority. Deriving d for regulators from captured position + victim status in terms of autonomy → d around 0.40-0.50, intermediate experienced extraction. No directionality overrides needed; the structural data produces coherent perspectival differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that multiple types are legitimate perspectival readings. Tangled Rope is the base classification because the constraint has both genuine coordination (infrastructure deployment solves collective action problems) and systematic asymmetric extraction (market prioritization extracts opportunity from unconnected regions; pricing extracts from low-income users; data extraction from all digitized populations). Snare is the victim's perspective because from the powerless/trapped position, only extraction is visible and experienced. Rope is the beneficiary's perspective because from institutional position with arbitrage options, the coordination function is primary and extraction is rationalized as market efficiency. Mountain is a false summit that must be actively resisted—the analytical observer risks naturalizing the divide as inevitable technological lag rather than seeing the contingent institutional choices (regulatory capture, monopoly pricing, infrastructure investment prioritization) that maintain it. The mandatrophy is resolved by showing that the classification space itself is the answer: the divide is maintained by institutional choice, not technological law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_cost_ceiling,
    'What is the true infrastructure cost to achieve universal broadband access at bandwidth standards necessary for full economic participation?',
    'Engineering analysis of last-mile deployment costs; comparative study of countries with near-universal coverage (Japan, South Korea, Finland); cost decomposition by geography and terrain',
    'If true cost < 5% of government digital budgets: divide is maintained by choice rather than constraint (classification shifts toward Snare). If true cost > 20%: divide is partially structural, justifying stronger Tangled Rope reading and reducing extraction attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_cost_ceiling, empirical, 'True infrastructure cost ceiling for universal access').

omega_variable(
    market_viability_boundary,
    'Below what population density or household income threshold is market-driven infrastructure provision economically impossible?',
    'Analysis of profitability models for rural/low-income deployment; identification of density/income breakpoints where service provision requires subsidy; comparison across regulatory regimes',
    'If boundary is clearly defined and universal: permits legitimate infrastructure constraint (Mountain or Rope). If boundary is artificially inflated by extractive pricing: reveals pricing as the real constraint, shifting classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_viability_boundary, empirical, 'Market viability boundary for infrastructure investment').

omega_variable(
    alternative_infrastructure_feasibility,
    'Can low-cost alternatives (satellite, mesh networks, community fiber) close the divide at competitive quality and cost, or are economies of scale so dominant that incumbent infrastructure is structurally necessary?',
    'Deployment case studies of satellite broadband (Starlink, Kuiper), mesh networks, and municipal fiber; cost-per-subscriber analysis; latency and reliability benchmarking; adoption curves in competitive markets',
    'If alternatives are technologically viable: divide maintenance becomes a matter of market structure and pricing strategy (Snare/Tangled Rope attribution increases). If alternatives structurally inferior: legitimizes incumbent monopoly (Rope/Tangled Rope attribution increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_feasibility, empirical, 'Feasibility of alternative infrastructure to break incumbent lock-in').

omega_variable(
    pricing_extraction_decomposition,
    'What portion of observed digital divide is attributable to infrastructure scarcity versus pricing extraction by service providers in areas where infrastructure exists?',
    'Comparison of adoption rates in areas with competing providers versus monopoly providers at equivalent income levels; analysis of price-demand elasticity; case studies of price regulation outcomes',
    'If scarcity dominates (>70%): divide is coordination problem requiring infrastructure investment (Rope/Tangled Rope). If pricing dominates (>50%): divide is extraction mechanism maintained by monopoly power (Snare/Tangled Rope with high extraction coefficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_extraction_decomposition, empirical, 'Decomposition of divide causes: scarcity versus pricing extraction').

omega_variable(
    digital_equity_program_effectiveness,
    'Do subsidized device programs and public WiFi without sustained subscription funding achieve meaningful digital equity, or are they performative theater that masks structural constraints?',
    'Longitudinal tracking of program beneficiaries: sustained usage rates, economic outcomes, employment gains; comparison of device subsidy outcomes versus full-cost-recovery programs; analysis of why beneficiaries remain connected or drop out',
    'If programs create lasting access: piton classification is partially incorrect; initiatives have real function (Rope/Tangled Rope). If programs show high dropout rates: piton confirmed; theater ratio is even higher than measured, and structural barriers remain unaddressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_equity_program_effectiveness, empirical, 'Whether digital equity programs create sustained access or serve as theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_divide_expansion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digdiv_tr_t0, digital_divide_expansion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(digdiv_tr_t5, digital_divide_expansion, theater_ratio, 5, 0.38).
narrative_ontology:measurement(digdiv_tr_t10, digital_divide_expansion, theater_ratio, 10, 0.48).
narrative_ontology:measurement(digdiv_tr_t15, digital_divide_expansion, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(digdiv_be_t0, digital_divide_expansion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(digdiv_be_t5, digital_divide_expansion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(digdiv_be_t10, digital_divide_expansion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(digdiv_be_t15, digital_divide_expansion, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_divide_expansion, global_infrastructure).
narrative_ontology:affects_constraint(digital_divide_expansion, data_colonialism_in_developing_nations).
narrative_ontology:affects_constraint(digital_divide_expansion, broadband_monopoly_pricing).
narrative_ontology:affects_constraint(digital_divide_expansion, digital_skills_access_barrier).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_divide_expansion, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
