% ============================================================================
% CONSTRAINT STORY: digital_access_equity_global
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_access_equity_global, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_access_equity_global
 *   human_readable: Digital Access Equity: Global Infrastructure Coordination and Asymmetric Extraction
 *   domain: digital_infrastructure/equity/global_development
 *
 * SUMMARY:
 *   Digital access equity presents a global constraint that combines genuine
 *   coordination (network effects require scale, interoperability requires
 *   standards) with asymmetric extraction (platform market concentration,
 *   data harvesting, attention capture, regulatory arbitrage). The constraint
 *   is simultaneously solving the legitimate problem of connecting dispersed
 *   populations and extracting value from the newly connected through data,
 *   algorithmic steering, and financial exclusion mechanisms. The base
 *   extractiveness (0.58) reflects that the extraction component is
 *   significant and growing but not total — genuine coordination benefits
 *   exist (markets expand, education access improves, digital payments enable
 *   financial inclusion). Suppression (0.62) is high: capital requirements
 *   for independent infrastructure, patent regimes restricting technology
 *   transfer, linguistic and literacy barriers, and regulatory capture by
 *   platform corporations all constrain exit options. The constraint's
 *   theater ratio (0.54) reflects the performative layer: universal service
 *   obligations, sustainable development goals, and digital equity narratives
 *   mask the concentration of extraction benefits toward wealthy nations and
 *   platform corporations. The structure exhibits all characteristics of
 *   Tangled Rope: genuine coordination function (network effects, standards
 *   enabling interoperability) combined with asymmetric extraction flows
 *   (data, labor, regulatory arbitrage, attention capture) and active
 *   enforcement (intellectual property regimes, infrastructure investment
 *   conditionality, platform terms of service).
 *
 * KEY AGENTS:
 *   - Unconnected Populations: Primary victims (powerless/trapped) — face material barriers to access; bear costs of exclusion (financial, educational, social)
 *   - Low-Income Nations: Structural victims (powerless/trapped) — capital constraints and external dependencies prevent independent infrastructure development
 *   - Middle-Income Nations: Secondary actors (moderate/constrained) — achieve partial access with mixed coordination and extraction benefits
 *   - Infrastructure Investors: Primary beneficiaries (institutional/arbitrage) — capture returns on capital, enjoy network effects, arbitrage regulatory differences
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — expand markets, extract data, achieve lock-in through network effects
 *   - High-Income Nations: Secondary beneficiaries (institutional/arbitrage) — citizens and corporations benefit from investor returns and network scale
 *   - Telecommunications Regulators: Institutional mediators (organized/constrained) — balance coordination and capture dynamics; implement uneven enforcement
 *   - Digital Rights Coalition: Organized alternatives (organized/constrained) — develop decentralized alternatives; see sunset trajectory for platform dependence
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies inseparability of coordination and extraction under current arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_access_equity_global, 0.58).
domain_priors:suppression_score(digital_access_equity_global, 0.62).
domain_priors:theater_ratio(digital_access_equity_global, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_access_equity_global, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_access_equity_global, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(digital_access_equity_global, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_access_equity_global, tangled_rope).
narrative_ontology:human_readable(digital_access_equity_global, "Digital Access Equity: Global Infrastructure Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(digital_access_equity_global, "digital_infrastructure/equity/global_development").

domain_priors:requires_active_enforcement(digital_access_equity_global).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_access_equity_global, infrastructure_investors).
narrative_ontology:constraint_beneficiary(digital_access_equity_global, platform_corporations).
narrative_ontology:constraint_beneficiary(digital_access_equity_global, high_income_nations).
narrative_ontology:constraint_victim(digital_access_equity_global, unconnected_populations).
narrative_ontology:constraint_victim(digital_access_equity_global, low_income_nations).
narrative_ontology:constraint_victim(digital_access_equity_global, epistemic_access_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCONNECTED POPULATION (SNARE) — Faces material barriers to digital access: lack of infrastructure, cost prohibitive to household income, geographic isolation, language/literacy barriers. Trapped without exit options. Bears full cost of digital exclusion while the constraint's coordination benefits flow to others. Maximum extraction experienced.
constraint_indexing:constraint_classification(digital_access_equity_global, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME NATIONS - SYSTEMIC VIEW (SNARE) — National governments face structural barriers to building independent digital infrastructure: capital requirements exceed available resources, licensing/patent regimes restrict technology transfer, debt servicing leaves no budget for digital investment. Trapped in dependency on foreign infrastructure providers. Extraction flows toward debt service and licensing fees; coordination benefits are minimal.
constraint_indexing:constraint_classification(digital_access_equity_global, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MIDDLE-INCOME NATION DIGITAL STRATEGIST (TANGLED ROPE) — Nations with partial infrastructure capacity (India, Brazil, Indonesia) experience both coordination and extraction. Genuine coordination benefit: internet access enables market participation, education access, digital payment systems. Asymmetric extraction: revenue flows to platform corporations and foreign investors; labor exploitation in content moderation and data annotation; regulatory capture by platforms. Constrained by debt dependency and limited sovereign capacity, but not trapped.
constraint_indexing:constraint_classification(digital_access_equity_global, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TELECOMMUNICATIONS REGULATOR - CAPTURE VIEW (TANGLED ROPE) — Regulators achieve genuine coordination: spectrum allocation, infrastructure standards, consumer protection. Also exhibit capture dynamics: platform corporations influence regulatory design, foreign investors shape infrastructure policy, career paths lead to industry positions. Constrained by institutional dependencies and global capital flows, but retain some sovereign authority. Experience is mixed: some genuine function, some extraction.
constraint_indexing:constraint_classification(digital_access_equity_global, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PLATFORM CORPORATIONS & INFRASTRUCTURE INVESTORS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination. Market expansion is framed as solving access inequity. Network effects, data extraction, and market capture are secondary to the coordination value proposition: connecting markets, enabling commerce, expanding user bases. Arbitrage options (alternative markets, regulatory arbitrage across jurisdictions). Net extraction flows toward these actors; they perceive the constraint as enabling coordination.
constraint_indexing:constraint_classification(digital_access_equity_global, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-INCOME NATIONS & CITIZENS (ROPE) — Primary beneficiaries via investor returns, platform profits, and network effects. Digital infrastructure in wealthy nations is robust and subsidized; extraction is minimal. The constraint functions primarily as coordination for these agents — solving problems of market efficiency and network connectivity. Arbitrage options available: domestic alternatives, regulatory options, capital mobility. Extraction flows toward this agent; they see the constraint as legitimate.
constraint_indexing:constraint_classification(digital_access_equity_global, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized agents (community networks, open-source movements, international NGOs) see digital access as a temporary coordination problem being solved by alternative infrastructures: mesh networks, open-source platforms, community radio, satellite internet. Constrained by funding and coordination challenges, but sees a sunset: decentralized infrastructure and open protocols reducing dependence on platform corporations. Extraction is temporary because alternatives are being built.
constraint_indexing:constraint_classification(digital_access_equity_global, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: UNIVERSAL SERVICE OBLIGATION (PITON) — Regulatory mandate to provide access to underserved populations has become largely performative. Operators comply through minimal-cost implementations (satellite coverage, slow speeds, inadequate maintenance) while claiming universal service compliance. The theater persists through institutional inertia despite low functional access. Beneficiaries (operators) maintain the fiction to satisfy regulators; the framework's actual function has atrophied.
constraint_indexing:constraint_classification(digital_access_equity_global, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, digital access is both a genuine coordination problem (network effects require scale, interoperability requires standards) and an asymmetric extraction mechanism (ownership concentration, data extraction, algorithmic capture, attention hijacking, regulatory arbitrage). The constraint operates simultaneously as a solution to network externalities and as a vehicle for wealth extraction. Both functions are real and inseparable under current institutional arrangements.
constraint_indexing:constraint_classification(digital_access_equity_global, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_access_equity_global_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_access_equity_global, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_access_equity_global, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_access_equity_global, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_access_equity_global, TR),
    TR >= 0.70.

:- end_tests(digital_access_equity_global_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high, reflecting significant but not total extraction flows. Infrastructure provision (roads, power, communications) is a coordination problem requiring scale and investment. Digital access is real and valuable to those gaining it — markets expand, income opportunities open, educational access improves. The extracted value (data, attention, financial margin) is substantial but not the sole function. The measurement trajectory shows extractiveness increasing from 0.32 to 0.58 over the interval, indicating that extraction mechanisms (data harvesting, algorithmic capture, platform dependence) are accumulating faster than access expansion. Suppression (0.62): High. Capital barriers (typical broadband rollout costs $800-1500 per household in rural areas) exceed household income in low-income regions. Patent regimes restrict technology transfer (essential medicines model shows how IP creates artificial scarcity; similar mechanisms apply to digital infrastructure). Regulatory capture: platform corporations shape digital policy in their favor through lobbying, revolving-door employment, and regulatory arbitrage. Literacy barriers (300+ million adults lack basic digital skills) are structural and persistent. Theater ratio (0.54): Moderate, indicating substantial performative content. Universal service obligations exist in many jurisdictions but are minimally implemented (satellite coverage meeting the letter while violating the spirit of adequate service). Sustainable development goals (SDG 9: digital infrastructure) frame access as a development priority, yet implementation remains extraction-focused (telecoms prioritize profitable urban markets, not unconnected rural areas). Digital equity narratives emphasize access while obscuring ownership concentration and data flows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single coordination-extraction hybrid appears as different types from different structural positions. The trapped see a Snare because they bear extraction cost without coordination benefit (they cannot participate in the network effects that justify the extraction). The beneficiaries see a Rope because coordination benefits (market expansion, network scale) are salient and extraction flows toward them. The analytical observer sees Tangled Rope because both functions are structurally present and inseparable under current arrangements. The gap is not observational (different facts) but perspectival (same facts, different structural positions). The constraint's true type is determined by whether the coordination function could exist without the extraction, or whether they are institutionally inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline works as follows: (1) Infrastructure investors are declared as beneficiaries — they capture returns on capital and network effects. The engine derives d from beneficiary status + arbitrage exit options → d ≈ 0.10, producing negative or minimal f(d). (2) Unconnected populations are declared as victims — they lack access and pay extraction costs (data, attention, financial margin). The engine derives d from victim status + trapped exit → d ≈ 0.95, producing high f(d) ≈ 1.42. (3) Low-income nations are declared as victims — structural barriers prevent independent infrastructure. Engine derives d from victim status + trapped exit (capital constraints) → d ≈ 0.92. (4) Middle-income nations are declared as both beneficiary (market expansion) and victim (data extraction, regulatory capture). The engine resolves this through constrained exit (high cost but possible) → d ≈ 0.70, producing moderate f(d). The directionality overrides are not needed — the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION BY PERSPECTIVAL DECOMPOSITION: The mandatrophy (potential confusion of coordination with extraction) is resolved by the multiple perspectives. If only the beneficiary perspective were examined, the constraint would be misclassified as pure Rope (coordination). If only the victim perspectives were examined, it would be misclassified as pure Snare (extraction). The true type — Tangled Rope — is revealed only by the presence of both beneficiaries and victims, both coordination function and asymmetric extraction. The nine perspectives together show why neither pure type is adequate: (1) the constraint genuinely solves coordination problems (network effects require scale), (2) the constraint genuinely extracts value asymmetrically (data, attention, financial margins flow to beneficiaries), (3) both mechanisms operate simultaneously and are institutionally inseparable under current ownership and regulatory models. The constraint could theoretically be decomposed into a pure-coordination story (network protocol standards) and a pure-extraction story (platform market concentration and data harvesting), but in practice they are coupled: the coordination function enables the extraction, and the extraction mechanisms prevent alternatives. Therefore Tangled Rope is the only adequate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'What portion of digital access inequality is inherent coordination cost versus what is contingent institutional extraction?',
    'Comparative analysis of open-source/community-owned vs corporate-owned digital infrastructure; cost-to-serve analysis in equivalent contexts; longitudinal data on how markup over infrastructure cost scales with market power',
    'If high coordination cost: classification shifts toward Rope (pure coordination). If extraction dominates: classification solidifies as Snare. Current assessment (0.58) assumes mixed model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Inherent coordination cost versus extractive markup in digital access').

omega_variable(
    alternative_infrastructure_viability,
    'Can decentralized, open-source, and community-owned digital infrastructure provide functionally equivalent access at significantly lower extraction rates?',
    'Case study comparison: mesh networks vs cellular, open-source platforms vs proprietary, community radio vs commercial broadcast. Metrics: user satisfaction, cost per user, data privacy, sustainability, scalability',
    'If viable at scale: scaffold perspective confirmed — sunset is structural. If marginal: scaffold is aspirational, and platform dependence is more structural than contingent. High confidence would validate the ecosystem transition narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_viability, empirical, 'Viability of decentralized infrastructure alternatives').

omega_variable(
    epistemic_access_rights_definition,
    'Is ''epistemic access'' (knowledge production, information literacy, algorithmic transparency) a structural component of digital equity or a separable layer?',
    'Measurement framework: distinguish between connectivity access, functionality access, literacy access, and epistemic access. Identify which dimensions correlate with extraction flows and which are orthogonal',
    'If integrated: constraint must address epistemic dimensions (algorithmic accountability, data sovereignty). If separable: two distinct constraints (infrastructure access and epistemic access) require separate stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_access_rights_definition, conceptual, 'Whether epistemic access is structural component of digital equity').

omega_variable(
    data_extraction_metrics,
    'How do we measure the magnitude of data extraction flows (behavioral data, attention capture, algorithmic steering) relative to infrastructure provision benefits?',
    'Framework for monetizing data externalities; economic analysis of attention economy; longitudinal studies of algorithmic recommendation impact on user outcomes',
    'If extractiveness of data flows is high: base_extractiveness should be revised upward (to 0.65+). If data extraction is diffuse and non-binding: current 0.58 is accurate. Current value assumes data extraction is significant but not determinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_extraction_metrics, empirical, 'Quantification of data extraction relative to infrastructure benefits').

omega_variable(
    sovereignty_vs_efficiency_tradeoff,
    'How much infrastructure efficiency (speed, cost, network effects) must low-income nations sacrifice to achieve digital sovereignty and reduce extraction?',
    'Cost-benefit analysis of fragmented vs integrated digital infrastructure; measurement of ''digital sovereignty premium'' (price of local control); case studies of countries choosing partial independence (Russia, Iran, Cuba digital strategies)',
    'If sovereignty premium is low: alternative pathways are realistic (affects classification of victim perspectives). If premium is high: structural dependence becomes partially unavoidable, shifting classification toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_efficiency_tradeoff, preference, 'Efficiency cost of digital sovereignty for low-income nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_access_equity_global, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digac_tr_t0, digital_access_equity_global, theater_ratio, 0, 0.38).
narrative_ontology:measurement(digac_tr_t8, digital_access_equity_global, theater_ratio, 8, 0.46).
narrative_ontology:measurement(digac_tr_t16, digital_access_equity_global, theater_ratio, 16, 0.54).
narrative_ontology:measurement(digac_tr_t4, digital_access_equity_global, theater_ratio, 4, 0.42).
narrative_ontology:measurement(digac_tr_t12, digital_access_equity_global, theater_ratio, 12, 0.5).

% Extraction over time
narrative_ontology:measurement(digac_be_t0, digital_access_equity_global, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(digac_be_t8, digital_access_equity_global, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(digac_be_t16, digital_access_equity_global, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(digac_be_t4, digital_access_equity_global, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(digac_be_t12, digital_access_equity_global, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_access_equity_global, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_access_equity_global, 0.18).
narrative_ontology:affects_constraint(digital_access_equity_global, data_extraction_asymmetry).
narrative_ontology:affects_constraint(digital_access_equity_global, platform_network_lock_in).
narrative_ontology:affects_constraint(digital_access_equity_global, regulatory_capture_telecommunications).

% DUAL FORMULATION NOTE:
% Digital access equity decomposes into three structurally distinct constraints with different ε values. This story addresses the coordination-extraction hybrid (global_infrastructure coordination with asymmetric extraction, ε=0.58). Upstream constraint: data_extraction_asymmetry (ε=0.72, platform-specific data flows and algorithmic capture) — affects this constraint by providing mechanism for extraction. Downstream constraints: regulatory_capture_telecommunications (ε=0.65, institutional dynamics in spectrum and infrastructure regulation) and platform_network_lock_in (ε=0.64, switching costs and vendor lock-in) — both are enabled by this constraint. Each story has different beneficiary/victim structure and different intervention points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
