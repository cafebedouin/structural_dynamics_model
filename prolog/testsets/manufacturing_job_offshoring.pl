% ============================================================================
% CONSTRAINT STORY: manufacturing_job_offshoring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manufacturing_job_offshoring, []).

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
 *   constraint_id: manufacturing_job_offshoring
 *   human_readable: Manufacturing Job Offshoring and Domestic Labor Market Extraction
 *   domain: economic/labor/geopolitical
 *
 * SUMMARY:
 *   Manufacturing job offshoring represents a structural tension between
 *   capital mobility, labor immobility, and unequal distribution of gains and
 *   losses. Since the 1980s, multinational corporations have relocated
 *   production from high-wage developed economies (primarily North America
 *   and Western Europe) to lower-wage jurisdictions (initially Asia, later
 *   globally). The constraint creates genuine coordination benefits (lower
 *   consumer prices, industrial development in host nations) alongside severe
 *   extraction (displacement of domestic workers, community degradation, wage
 *   suppression in manufacturing globally). The extractiveness metric has
 *   increased from 0.32 to 0.58 over the measurement interval because: (1)
 *   wage arbitrage has intensified rather than equilibrating, (2) automation
 *   in offshored production reduces employment gains in host nations, and (3)
 *   policy responses (trade agreements, subsidy structures, labor regulation)
 *   have enabled extraction rather than constraining it. The theater ratio
 *   has increased from 0.38 to 0.52 because trade policy rhetoric emphasizes
 *   'free trade' and 'comparative advantage' while actual mechanisms are
 *   shaped by tax treaties, capital mobility policy, intellectual property
 *   enforcement, and subsidy structures designed to enable corporate
 *   arbitrage. This is not a natural law but a contingent institutional
 *   arrangement.
 *
 * KEY AGENTS:
 *   - Multinational Corporations: Primary beneficiary (institutional/arbitrage) — capture wage differentials, avoid unionized labor, extract tax subsidies from competing jurisdictions
 *   - Manufacturing Workers (Domestic): Primary victim (powerless/trapped) — face job loss with minimal transition support, geographic and skill-based immobility traps
 *   - Manufacturing Communities (Rustbelt): Secondary victim (moderate/constrained) — bear stranded infrastructure costs, population decline, fiscal deterioration; constrained by geographic anchoring and political fragmentation
 *   - Foreign Manufacturing Host Nations: Secondary beneficiary (institutional/arbitrage) — gain employment and FDI but face wage suppression and lost policy autonomy
 *   - Consumers (Developed Markets): Moderate beneficiary with extraction exposure (powerful/mobile) — benefit from lower prices but bear systemic wage and labor-market degradation
 *   - Global Manufacturing Labor Force: Tertiary victim (moderate/constrained) — locked into global wage competition through jurisdictional arbitrage
 *   - Trade Policy Regime: Institutional actor maintaining piton structure (institutional/constrained) — enforces trade agreements and capital mobility while transition programs degrade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manufacturing_job_offshoring, 0.58).
domain_priors:suppression_score(manufacturing_job_offshoring, 0.65).
domain_priors:theater_ratio(manufacturing_job_offshoring, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manufacturing_job_offshoring, extractiveness, 0.58).
narrative_ontology:constraint_metric(manufacturing_job_offshoring, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(manufacturing_job_offshoring, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manufacturing_job_offshoring, tangled_rope).
narrative_ontology:human_readable(manufacturing_job_offshoring, "Manufacturing Job Offshoring and Domestic Labor Market Extraction").
narrative_ontology:topic_domain(manufacturing_job_offshoring, "economic/labor/geopolitical").

domain_priors:requires_active_enforcement(manufacturing_job_offshoring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manufacturing_job_offshoring, multinational_corporations).
narrative_ontology:constraint_beneficiary(manufacturing_job_offshoring, capital_investors).
narrative_ontology:constraint_beneficiary(manufacturing_job_offshoring, consumer_markets_developed).
narrative_ontology:constraint_victim(manufacturing_job_offshoring, manufacturing_workers_domestic).
narrative_ontology:constraint_victim(manufacturing_job_offshoring, manufacturing_communities_rustbelt).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED MANUFACTURING WORKER (SNARE) — Faces immobility: factory closing eliminates primary income source with no equivalent retraining pathway. Geographic relocation barriers (housing costs, family ties, spousal employment) prevent exit to other regional labor markets. Age discrimination and skill mismatch lock older workers out of service-sector alternatives. Suppression is near-total: legal frameworks protect corporate decision-making but offer minimal worker transition support. Maximum experienced extraction — the worker bears full cost of the offshoring decision with zero agency.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MANUFACTURING COMMUNITY (TANGLED ROPE) — Local governments coordinate tax-base maintenance, workforce development, and infrastructure alongside corporations seeking cost advantages. Real coordination exists: communities invest in workforce training and industrial parks in exchange for plant location commitments. But extraction is asymmetric: when corporations relocate, communities bear stranded infrastructure costs and population decline. Exit is costly (retooling economy, emigration) but not impossible. Generational time horizon reflects that community recovery takes 20-30 years.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATION (ROPE) — Experiences offshoring as solving a coordination problem: aligning labor costs with production efficiency to serve global markets competitively. No suppression from the corporation's perspective — it has full legal and operational freedom to move production. High arbitrage: can relocate to lowest-cost jurisdictions, renegotiate tax rates, or threaten plant closure to extract local subsidies. Experiences minimal extraction — the constraint enables rather than constrains their action.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOREIGN MANUFACTURING HOST NATION (ROPE) — Coordinates industrial development, workforce expansion, and FDI incentives. Gains employment, technology transfer, and export revenue. Has arbitrage option: can renegotiate labor regulations, tax rates, or environmental standards to attract investment. Experiences no suppression — policy levers are available. Net beneficiary of the offshoring constraint. However, coordination function is real: the nation must actually develop workforce capacity and industrial infrastructure.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPED-MARKET CONSUMER (TANGLED ROPE) — Benefits from lower manufacturing costs passed through as lower consumer prices (coordination function realized). But also bears extraction: local labor-market degradation, reduced union wages, and eroded middle-class stability reduce their own bargaining power and wage growth. Exit is mobile — consumers can choose products with domestic labor certification or relocate to labor-market regions with stronger manufacturing. But exit carries costs (price premiums, geographic constraints). Moderate extraction with genuine benefits — mixed perspectival experience.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADE POLICY REGIME (PITON) — Institutions designed to manage industrial transition (tariffs, trade adjustment assistance, regional development programs) persist despite degraded function. Theater ratio (0.52) reflects: formal trade policy negotiations show 'level playing field' rhetoric while actual terms enable offshoring; worker transition programs are underfunded and performative; regional development subsidies go to corporate incentive packages rather than community capacity. The regime is inertial — maintained because alternatives require coordinated multilateral renegotiation, not because the current mechanisms effectively manage offshoring's consequences.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: GLOBAL MANUFACTURING LABOR FORCE / ORGANIZED PRECARIAT (SNARE) — At global scale, manufacturing workers across all jurisdictions are locked into competition for offshored jobs through wage arbitrage. Multinational corporations play jurisdictions against each other, extracting concessions from all. Individual workers have constrained exit: lack of global labor mobility, language barriers, visa restrictions, and skill-specificity lock them into local labor markets. Globally organized (through ILO, union networks, NGOs), but coordination is constrained by free-rider problems and weak enforcement. The global labor market is structured to suppress wages and working conditions across all manufacturing jurisdictions simultaneously.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE (MOUNTAIN-ASPIRATION) — From the canonical economics view, offshoring is an immutable consequence of comparative advantage and capital mobility: firms will always move to lowest-cost jurisdictions, and any attempt to prevent offshoring fights economic law. This perspective treats offshoring as a natural law of markets rather than a contingent institutional arrangement. However, the structural data contradicts this — the constraint is shaped by trade agreements, capital mobility policy, tax treaties, labor regulation, and subsidy structures. The mountain classification is a false summit: it naturalizes policy choices as law of nature.
constraint_indexing:constraint_classification(manufacturing_job_offshoring, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manufacturing_job_offshoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manufacturing_job_offshoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manufacturing_job_offshoring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manufacturing_job_offshoring, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(manufacturing_job_offshoring, TR),
    TR >= 0.70.

:- end_tests(manufacturing_job_offshoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially from domestic manufacturing workers and communities through wage loss, job displacement, and infrastructure decay. But extraction is not maximal (0.70+) because: (1) genuine coordination benefits exist (lower consumer prices, industrial development), (2) some workers successfully transition to service sectors or gain skills, (3) some communities diversify or reverse-offshore. The extraction is real and concentrated but not total. The upward trajectory (0.32 → 0.58) reflects that: wage arbitrage has intensified rather than equilibrating; automation in host nations is reducing employment gains; and successive trade rounds have deepened capital mobility while weakening labor protections. Suppression (0.65): Moderate-high. Barriers to worker exit are substantial: geographic immobility (housing costs, family ties, low portability of manufacturing skills), age discrimination in service sectors, regional concentration of manufacturing (when the primary employer leaves, regional labor markets collapse), and absence of effective transition support. But suppression is not total (0.85+) because: federal programs (Trade Adjustment Assistance, retraining subsidies) exist (though underfunded); some workers do relocate; some manufacturing persists in other sectors. The suppression is structural (external barriers) rather than purely internalized (workers have not generally identity-locked to factory work). Theater ratio (0.52): Moderate. Trade policy contains significant theater: negotiations emphasize 'level playing field' and 'rules-based order' while actual mechanisms are shaped by corporate lobbying, tax treaty design, and capital flow management. Transition programs are performative — they exist as policy theater but are underfunded relative to the scale of displacement. However, the constraint is not primarily theatrical — the actual extraction mechanism (wage arbitrage enabled by capital mobility) is real and functional. Unlike pitons, the theater here is supplement to functional extraction, not substitute for it.
 *
 * PERSPECTIVAL GAP:
 *   The multipolar gap reveals that offshoring is experienced as coordination by beneficiaries (corporations, host nations) and as extraction by victims (domestic workers, communities, global labor forces). From beneficiaries' positions, the constraint solves a genuine problem (cost competitiveness) with minimal coercion — they experience it as Rope. From victims' positions, the constraint is extractive with few alternatives — they experience it as Snare or Tangled Rope. The consumer gap (Tangled Rope) is particularly diagnostic: consumers benefit from lower prices but are also workers/community members who bear wage and labor-market degradation. The piton classification of the trade regime reveals institutional inertia: agreements remain in force despite degraded function (labor standards are not enforced, transition programs are underfunded, capital mobility is unrestricted while labor mobility is blocked). The false-summit mountain classification reveals the risk of naturalizing contingent arrangements: 'comparative advantage is law' naturalizes policy choices (trade agreements, capital mobility frameworks, labor regulation) as immutable economic principle.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are required. The default derivation chain (beneficiary/victim declarations + exit options → d → f(d) → χ) correctly differentiates perspectives. Multinational corporations are beneficiaries + arbitrage → low d → low χ. Domestic workers are victims + trapped → high d → high χ. Manufacturing communities are victims + constrained → moderate-high d → moderate-high χ. The perspectival gap is generated by structural differentiation (who benefits, who bears costs, how mobile each agent is) rather than by override correction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY ASYMMETRIC GAIN/LOSS DISTRIBUTION: The mandatrophy is resolved by documenting that offshoring produces real coordination benefits (lower manufacturing costs, industrial development in host nations) alongside asymmetric extraction (concentrated job losses in developed economies, wage suppression globally). The constraint is not 'coordination masquerading as extraction' but 'genuine coordination with asymmetric distribution enabled by unequal power.' Snare classification for the domestic worker is not contradicted by Rope classification for the corporation — they are the same constraint experienced from opposite positions. The Piton classification of the trade regime reveals that institutional responses (transition programs, regional development, labor standards enforcement) have degraded to theater, creating a two-layer mandatrophy: the underlying constraint is Tangled Rope (coordination + extraction), but institutional responses are degraded (Piton), leaving extraction exposed. Resolution requires institutional reform (enforcement of labor standards in trade agreements, funded transition support, capital mobility restrictions tied to labor mobility, sectoral bargaining frameworks) rather than denial that extraction exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_incidence,
    'Who actually bears the true economic cost of offshoring when all effects are counted: consumers, workers, communities, and future fiscal obligations?',
    'Comprehensive cost accounting: wage losses + benefits forgone + increased crime/health costs + fiscal transfers + consumer price reductions + corporate profit increases. Cross-country longitudinal analysis of manufacturing regions pre- and post-offshoring with matched controls.',
    'If total cost exceeds aggregate benefits: offshoring is pure extraction masked by unequal distribution. If benefits exceed costs but are concentrated: confirms tangled rope asymmetry. If properly accounted benefits flow to all groups: reclassifies as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_incidence, empirical, 'True total economic cost incidence of offshoring across all stakeholders').

omega_variable(
    counterfactual_domestic_productivity,
    'What would have been the productivity and wage trajectory of domestic manufacturing in the absence of offshoring pressure?',
    'Comparison of wage/productivity trends in industries with offshoring exposure vs protected/reshoring industries; econometric estimation of displaced manufacturing sectors'' counterfactual growth rates; case studies of firms that maintained domestic production.',
    'If domestic productivity would have stagnated anyway: offshoring is coordination response to inevitable decline. If domestic productivity could have recovered with different investment: offshoring is extraction enabled by policy choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_domestic_productivity, conceptual, 'Counterfactual productivity trajectory without offshoring pressure').

omega_variable(
    labor_arbitrage_sustainability,
    'Is wage arbitrage between jurisdictions a temporary coordination problem (bridging toward equilibrium) or a sustainable extraction mechanism?',
    'Analysis of wage convergence rates across manufacturing jurisdictions over 30-year periods; measurement of whether offshoring pressure drives convergence upward (equilibrium) or indefinite divergence (extraction). Cross-border labor mobility constraints documentation.',
    'If convergent: temporary coordination problem with scaffold characteristics. If divergent despite development: sustainable extraction enabled by labor immobility barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_arbitrage_sustainability, empirical, 'Whether wage arbitrage drives toward or away from equilibrium').

omega_variable(
    alternative_coordination_arrangements,
    'What policy structures would enable coordination between manufacturers and labor without the offshoring extraction mechanism?',
    'Comparative institutional analysis: trade agreements with labor standards enforcement, sectoral bargaining frameworks, capital mobility restrictions, tax treaties conditioning investment on wage floors, reshoring subsidies. Identification of which policy levers are binding constraints vs theater.',
    'If robust alternatives exist: current offshoring pattern is choice, not necessity. If alternatives are unworkable: constraint is closer to mountain than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_arrangements, preference, 'Feasibility of alternative coordination arrangements without offshoring').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manufacturing_job_offshoring, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfg_offshore_tr_t0, manufacturing_job_offshoring, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mfg_offshore_tr_t5, manufacturing_job_offshoring, theater_ratio, 5, 0.45).
narrative_ontology:measurement(mfg_offshore_tr_t10, manufacturing_job_offshoring, theater_ratio, 10, 0.52).
narrative_ontology:measurement(mfg_offshore_tr_t15, manufacturing_job_offshoring, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(mfg_offshore_be_t0, manufacturing_job_offshoring, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mfg_offshore_be_t5, manufacturing_job_offshoring, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mfg_offshore_be_t10, manufacturing_job_offshoring, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mfg_offshore_be_t15, manufacturing_job_offshoring, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manufacturing_job_offshoring, resource_allocation).
narrative_ontology:affects_constraint(manufacturing_job_offshoring, trade_agreement_enforcement).
narrative_ontology:affects_constraint(manufacturing_job_offshoring, labor_standard_enforcement).
narrative_ontology:affects_constraint(manufacturing_job_offshoring, capital_mobility_regime).
narrative_ontology:affects_constraint(manufacturing_job_offshoring, domestic_manufacturing_decline).

% DUAL FORMULATION NOTE:
% Manufacturing job offshoring decomposes into structurally distinct constraints: (1) wage_arbitrage (ε≈0.35, coordination problem in global labor markets), (2) trade_agreement_enforcement (ε≈0.42, institutional constraint on labor standards), (3) capital_mobility_regime (ε≈0.48, policy constraint enabling offshoring), (4) domestic_manufacturing_decline (ε≈0.52, path-dependent regional economic failure). The offshoring constraint is downstream of these enabling mechanisms — fixing the upstream constraints would alter offshoring dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
