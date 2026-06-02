% ============================================================================
% CONSTRAINT STORY: regional_economic_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_economic_inequality, []).

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
 *   constraint_id: regional_economic_inequality
 *   human_readable: Regional Economic Inequality and Capital Concentration
 *   domain: economic_geography/political_economy
 *
 * SUMMARY:
 *   Regional economic inequality operates as a structural constraint where
 *   core financial and industrial centers extract from peripheral regions
 *   through differential wage capture, infrastructure investment
 *   concentration, and labor mobility barriers. The constraint exhibits all
 *   six DR types from different perspectives. From the perspective of trapped
 *   peripheral workers, it is a snare — immobility is enforced by cost, skill
 *   mismatch, and credential non-recognition. From the perspective of
 *   secondary city professionals, it is tangled rope — genuine regional
 *   coordination (universities, professional networks) coexists with
 *   systematic extraction through tax base drain and capital flight. From
 *   core financial centers, it appears as rope — a coordination mechanism
 *   solving complex operational and financial network requirements. From
 *   organized labor and regional development actors, it appears as a scaffold
 *   — a temporary problem being solved through union coordination and digital
 *   infrastructure. From the policy apparatus, it appears as piton — regional
 *   development policy is largely performative ritual. The constraint's
 *   theater ratio (0.62) reflects that policy announcements and development
 *   programs create an appearance of action while core extraction mechanisms
 *   (capital's mobility and labor's immobility) remain unchanged. The
 *   extractiveness has increased over the measurement interval (0.42 → 0.58)
 *   as agglomeration dynamics have intensified and remote work promises have
 *   not yet materially decentralized capital. The suppression level (0.68)
 *   reflects multiple, overlapping barriers to exit: geographic cost,
 *   educational concentration in cores, credential non-recognition across
 *   jurisdictions, and informal network dependencies that disadvantage
 *   outsiders.
 *
 * KEY AGENTS:
 *   - Peripheral Region Workers: Primary victims (powerless/trapped) — face insurmountable barriers to migration and constrained local job markets
 *   - Core Financial Centers: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains from concentration and can credibly threaten relocation
 *   - Secondary City Professionals: Mixed agents (moderate/constrained) — benefit from regional coordination but systematically extract from through brain drain and capital flight
 *   - Real Estate Investors: Structural beneficiaries (institutional/arbitrage) — profit from core appreciation and peripheral disinvestment
 *   - Organized Labor: Organized victims (organized/constrained) — coordinating function is systematically weaker in periphery, enabling wage suppression
 *   - Regional Development Coalitions: Organized reformers (organized/constrained) — see temporary problem solvable through intervention
 *   - Policy Apparatus: Institutional maintainers (institutional/arbitrage) — performs regional development ritual without addressing core mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_economic_inequality, 0.58).
domain_priors:suppression_score(regional_economic_inequality, 0.68).
domain_priors:theater_ratio(regional_economic_inequality, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_economic_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_economic_inequality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regional_economic_inequality, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_economic_inequality, tangled_rope).
narrative_ontology:human_readable(regional_economic_inequality, "Regional Economic Inequality and Capital Concentration").
narrative_ontology:topic_domain(regional_economic_inequality, "economic_geography/political_economy").

domain_priors:requires_active_enforcement(regional_economic_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_economic_inequality, core_financial_centers).
narrative_ontology:constraint_beneficiary(regional_economic_inequality, multinational_corporations).
narrative_ontology:constraint_beneficiary(regional_economic_inequality, real_estate_investors).
narrative_ontology:constraint_victim(regional_economic_inequality, peripheral_regions).
narrative_ontology:constraint_victim(regional_economic_inequality, rural_labor_force).
narrative_ontology:constraint_victim(regional_economic_inequality, resource_extraction_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL REGION WORKER (SNARE) — Trapped by geography, lack of skill-match for urban jobs, and costs of relocation. Educational opportunities are concentrated in core cities; migrating requires capital and social networks the worker lacks. Suppression is structural: deteriorating local services create push factors, but exit barriers remain insurmountable. Bears full extraction cost with no exit capacity.
constraint_indexing:constraint_classification(regional_economic_inequality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECONDARY CITY MIDDLE CLASS (TANGLED ROPE) — Constrained by regional employment concentration and housing costs that absorb local wage premium. Benefits from coordination of educational institutions and professional networks within the region, which provides genuine coordination service. However, faces systematic extraction through tax base drain (wealthier residents migrate), infrastructure underinvestment, and wage suppression from capital's ability to shift operations.
constraint_indexing:constraint_classification(regional_economic_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORE FINANCIAL CENTER (ROPE) — Experiences inequality constraint as coordination mechanism: labor concentration, supply chain efficiency, and network effects are genuine benefits. Arbitrage option exists — capital can move anywhere (and threatens to move as leverage). Net beneficiary of the constraint; extracts through wage differentials and property values, but also genuinely solves coordination problems of complex financial operations.
constraint_indexing:constraint_classification(regional_economic_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT COALITION (SCAFFOLD) — Organized actors (local government, NGOs, regional universities, cooperative enterprises) see inequality as a temporary problem solvable through targeted intervention: remote work, broadband infrastructure, skill-matching programs, anchor institution strategies. Sunset clause implicit in digital infrastructure buildout and labor market decentralization. Coalition has agency and sees exit path, constraining extraction through coordination alternatives.
constraint_indexing:constraint_classification(regional_economic_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGIONAL POLICY APPARATUS (PITON) — Regional development policy (development zones, tax incentives, infrastructure investment) is largely performative ritual: policy announcements create theater of action without addressing core extraction mechanisms (capital's exit option and labor immobility). Theater ratio 0.62 reflects that much policy activity is symbolic gesture rather than effective restructuring. The apparatus persists through institutional inertia and political face-saving despite limited track record of equalization.
constraint_indexing:constraint_classification(regional_economic_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED LABOR MOVEMENT (TANGLED ROPE) — Coordinating function: unions provide genuine collective voice that negotiates both within and across regions, pooling risk and knowledge. Extraction: labor movement is systematically weaker in peripheral regions where density is lower, and capital uses regional bifurcation as leverage to suppress wages nationally. Constrained exit — deunionization creates new extraction layer on top of coordination.
constraint_indexing:constraint_classification(regional_economic_inequality, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, agglomeration effects and increasing returns to scale in complex economies are inherent properties of modern production. Regional inequality is a natural law of economic development — core accumulation is structural. However, this perspective risks naturalizing what are actually contingent institutional arrangements (patent systems, infrastructure investment patterns, migration barriers). The engine's false summit detector will flag this as misidentification of contingent structures as natural laws.
constraint_indexing:constraint_classification(regional_economic_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_economic_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_economic_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_economic_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_economic_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_economic_inequality, TR),
    TR >= 0.70.

:- end_tests(regional_economic_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Core regions capture wage and asset value premiums through multiple mechanisms: agglomeration efficiency (genuine coordination gain ~0.15-0.20 of differential), labor market power (capital's mobility enabling wage suppression ~0.20-0.25), and rent extraction through property and financial assets (~0.15-0.18). The measurement progression shows extractiveness increasing as agglomeration deepens and remote work promises fail to materialize. Suppression (0.68): Moderate-high. Barriers to exit are multiple: (1) structural — relocation costs, skill mismatch with core jobs, credential non-recognition; (2) social — network effects that disadvantage outsiders in core labor markets; (3) institutional — infrastructure and educational investment concentrated in cores. Suppression is not total — some mobility occurs through internal migration and some core-periphery integration happens — but is substantial enough to trap the majority. Theater ratio (0.62): Moderate-high. Regional development policy creates appearance of action (development zones, tax incentives, subsidy programs) without addressing core mechanisms. Policy announcements and ribbon-cuttings are performative; actual capital reallocation remains negligible. Theater has increased over the interval as policy became more elaborate while inequality worsened, indicating growing gap between policy activity and real effect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows dramatic perspectival divergence. The same structural mechanism (agglomeration + labor immobility + capital mobility) classifies as Snare from the trapped worker perspective, Rope from the core beneficiary perspective, Tangled Rope from the moderate and organized perspectives, Scaffold from the reform coalition perspective, and Piton from the policy perspective. The mountain perspective—'regional inequality is inherent to modern economies'—is a false summit that naturalizes contingent institutional arrangements (patent systems, credential non-portability, infrastructure investment patterns, labor mobility barriers). These gaps are not conflicting reports of the same thing — they are legitimate readings of how different agents structurally experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) value is derived from their structural position relative to the extraction flow. Trapped workers with no exit capacity experience high d (0.85+) — maximum vulnerability, maximum f(d), maximum experienced extraction (chi). Constrained secondary city professionals experience moderate-high d (0.55-0.70) — they have costs to exit but not absolute barriers, enabling them to capture some coordination rent. Core financial center beneficiaries experience very low d (0.10-0.20) — they benefit from the constraint and have arbitrage option to exit, so extraction flows toward them rather than from them. Organized labor experiences d around 0.50-0.65 depending on regional union density — coordinating across inequality regimes while being systematically weakened by periphery dynamics. The derived directionality values feed into the chi formula (χ = ε × f(d) × σ(S)) and produce the observed classification spread: snare for high d + global scope, rope for low d + institutional power, tangled rope for moderate d + organized/moderate power, scaffold for moderate d + organized power with sunset.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that regional inequality is genuinely both a coordination mechanism and an extraction mechanism. The coordination function (agglomeration efficiency, network effects, labor market matching) is real but modest (~20-30% of observed wage differentials). The extraction function (wage suppression, capital flight, rent capture, barrier enforcement) dominates (~50-60% of observed differentials). The Tangled Rope classification captures this hybrid: genuine coordination coexists with asymmetric extraction, and the asymmetry is actively maintained through institutional enforcement (credential non-recognition, infrastructure underinvestment, labor migration restriction). The constraint is not mislabeled extraction-as-coordination (which would be Snare) or mislabeled coordination-as-extraction (which would be Rope). Rather, it is correctly identified as both, with the extraction component dominant and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agglomeration_vs_extraction_boundary,
    'Are observed wage and development differentials between regions due to genuine agglomeration efficiency gains or to extractive rent-capture by core capitals?',
    'Comparative analysis of regional wage premiums vs productivity differentials; cross-country comparison of inequality in different institutional regimes (federal transfer systems, union density, labor mobility); decomposition of wage gaps into human capital, job mix, and residual extraction.',
    'If 70%+ differential is agglomeration: core regions have genuine coordination advantage and constraint is primarily Rope. If 70%+ is extraction: constraint is primarily Snare with theatrical efficiency claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agglomeration_vs_extraction_boundary, empirical, 'Proportion of regional wage differentials attributable to agglomeration gains vs extractive rents').

omega_variable(
    labor_mobility_structural_vs_choice,
    'Do peripheral region residents face structural barriers to migration (cost, skills mismatch, credential non-recognition) or is immobility a preference revealed through cost-benefit analysis?',
    'Comparative study of migration patterns when barriers are explicitly reduced (visa liberalization, relocation subsidies, credential mutual recognition); survey of non-migrants about counterfactual scenarios; analysis of return migration rates and success outcomes.',
    'If structural: suppression classification is correct and exit_options are trapped/constrained. If choice-based: exit_options shift toward mobile/arbitrage and constraint appears less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_mobility_structural_vs_choice, empirical, 'Whether labor immobility reflects structural barriers or revealed preferences').

omega_variable(
    remote_work_sunset_credibility,
    'Will remote work infrastructure and digital labor markets actually decentralize capital and employment, or will they preserve and amplify core-periphery dynamics through different mechanisms?',
    'Longitudinal tracking of employment distribution and wages in jurisdictions with high remote work adoption; analysis of whether remote work reduces wage suppression for peripheral workers or merely substitutes location-independent low-wage labor.',
    'If genuine decentralization: scaffold sunset is real and constraint will degrade. If mechanism substitution: apparent decentralization masks continued extraction, and scaffold classification is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remote_work_sunset_credibility, empirical, 'Whether remote work infrastructure will actually decentralize capital and employment').

omega_variable(
    federal_transfer_effectiveness,
    'Do equalization transfers and regional development funds actually reduce underlying extraction mechanisms or merely compensate victims while preserving core-periphery capital flows?',
    'Analysis of regional inequality trends before/after major transfer system reforms; comparison of transfer-recipient regions that succeeded in diversification vs those that remained dependent; examination of whether transfers reduce or entench capital dependency.',
    'If effective: transfers reduce extraction and constraint moves toward Rope. If compensatory: transfers maintain suppression and mask extraction, and constraint remains Snare at base level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_transfer_effectiveness, empirical, 'Whether federal transfers reduce extraction or merely compensate victims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_economic_inequality, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regecon_tr_t0, regional_economic_inequality, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regecon_tr_t10, regional_economic_inequality, theater_ratio, 10, 0.58).
narrative_ontology:measurement(regecon_tr_t20, regional_economic_inequality, theater_ratio, 20, 0.62).
narrative_ontology:measurement(regecon_tr_t5, regional_economic_inequality, theater_ratio, 5, 0.53).
narrative_ontology:measurement(regecon_tr_t15, regional_economic_inequality, theater_ratio, 15, 0.6).

% Extraction over time
narrative_ontology:measurement(regecon_be_t0, regional_economic_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(regecon_be_t10, regional_economic_inequality, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(regecon_be_t20, regional_economic_inequality, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(regecon_be_t5, regional_economic_inequality, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regecon_be_t15, regional_economic_inequality, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_economic_inequality, resource_allocation).
narrative_ontology:affects_constraint(regional_economic_inequality, labor_market_sorting_by_credential).
narrative_ontology:affects_constraint(regional_economic_inequality, housing_affordability_crisis).
narrative_ontology:affects_constraint(regional_economic_inequality, infrastructure_investment_concentration).

% DUAL FORMULATION NOTE:
% Regional inequality decomposes into distinct structural claims: (1) labor_market_sorting_by_credential — wage gaps due to educational mismatch and credential concentration (ε~0.35, primarily Rope with Tangled Rope undertones); (2) housing_affordability_crisis — property values as extraction mechanism independent of job market (ε~0.50, Tangled Rope); (3) infrastructure_investment_concentration — differential public investment as reinforcement mechanism (ε~0.40, Tangled Rope with Piton theater). The master constraint (regional_economic_inequality) represents the systemic integration of these three. Decomposition enables precise measurement of which mechanism dominates in specific regions and time periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_economic_inequality, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
