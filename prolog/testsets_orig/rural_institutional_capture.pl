% ============================================================================
% CONSTRAINT STORY: rural_institutional_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_institutional_capture, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rural_institutional_capture
 *   human_readable: Rural Institutional Capture
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Rural institutional capture describes the structural entanglement of
 *   local government, credit systems, market infrastructure, and land tenure
 *   arrangements in ways that systematically extract surplus from small
 *   farmers and agricultural workers while providing concentrated benefits to
 *   landed gentry and extractive industries. Unlike a simple coercive snare,
 *   institutional capture involves genuine coordination functions: local
 *   governments do provide some services, credit cooperatives do facilitate
 *   transactions, and market infrastructure does enable trade. But all these
 *   institutions are captured — designed and operated to serve landed
 *   interests first, with small farmer benefit as a secondary effect if at
 *   all. The constraint exhibits all the signatures of a tangled rope hybrid:
 *   substantive extraction (χ ≈ 0.58), significant suppression (barriers to
 *   exit and alternative institutions), and genuine coordination functions
 *   that make abolishing the institutions counterproductive. The key
 *   perspective on resolution is the organized farmer movement perspective
 *   (scaffold): agricultural unions and cooperatives are attempting to build
 *   institutional alternatives that have built-in sunset logic — if they
 *   scale, the captured local institutions lose their monopoly and extraction
 *   mechanisms weaken.
 *
 * KEY AGENTS:
 *   - Rural Agricultural Workers: Primary victims (powerless/trapped) — no exit options, complete dependency on captured institutions for employment and survival
 *   - Small Farmers: Secondary victims (powerless/constrained) — structurally mobile but face high exit costs from land sale, social disruption, and regulatory hostility
 *   - Landed Gentry and Extractive Industries: Primary beneficiaries (institutional/arbitrage) — design and benefit from captured institutional arrangement, high mobility (can relocate if capture fails)
 *   - Local Government Officials: Captured institutional actors (moderate/constrained) — constrained by landowner patronage dependency, also benefit from side payments and career advancement through patron networks
 *   - Agricultural Extension Services: Nominal state agents (institutional/constrained) — tasked with agricultural development but constrained by operating through captured local structures
 *   - Agricultural Unions and Farmer Cooperatives: Organized agents attempting alternatives (organized/constrained) — face regulatory hostility and credit embargoes but represent potential sunset pathway
 *   - Central State/National Government: Macro-level institutional actor (institutional/constrained) — benefits from stable rural political control, constrained by dependence on local enforcement capacity, tolerates capture to maintain rural stability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing capture as inevitable or culturally normative feature of rural life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_institutional_capture, 0.58).
domain_priors:suppression_score(rural_institutional_capture, 0.65).
domain_priors:theater_ratio(rural_institutional_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_institutional_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(rural_institutional_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rural_institutional_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_institutional_capture, tangled_rope).
narrative_ontology:human_readable(rural_institutional_capture, "Rural Institutional Capture").
narrative_ontology:topic_domain(rural_institutional_capture, "political_economy/governance").

domain_priors:requires_active_enforcement(rural_institutional_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_institutional_capture, landed_gentry).
narrative_ontology:constraint_beneficiary(rural_institutional_capture, extractive_industries).
narrative_ontology:constraint_beneficiary(rural_institutional_capture, centralized_financial_intermediaries).
narrative_ontology:constraint_victim(rural_institutional_capture, rural_agricultural_workers).
narrative_ontology:constraint_victim(rural_institutional_capture, small_farmer_communities).
narrative_ontology:constraint_victim(rural_institutional_capture, rural_public_goods_provision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AGRICULTURAL WORKER (SNARE) — Trapped by geographic isolation, lack of alternative employment, dependency on land access through captured local institutions, and limited mobility. The local government, agricultural extension services, credit cooperatives, and market infrastructure are all captured by landowners. No exit available without relocation. Maximum extraction: labor is undercompensated, credit terms are predatory, market prices are suppressed through monopsony power, and public services are diverted to benefit the landed.
constraint_indexing:constraint_classification(rural_institutional_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL FARMER (SNARE) — Structurally mobile (can relocate) but constrained by high costs: sale of land at suppressed prices, loss of social networks, educational disruption for children, and psychological attachment to inherited property. The captured local government enforces regulations (zoning, environmental rules) selectively against small farmers. Credit from captured institutions is predatory. Output marketing is controlled by buyers with monopsony power. Exit is technically possible but prohibitively expensive.
constraint_indexing:constraint_classification(rural_institutional_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOCAL GOVERNMENT OFFICIAL (TANGLED ROPE) — Structurally constrained by dependence on landowners for tax revenue, campaign support, and informal payment flows. Also benefits from the captured system: career advancement through landowner patronage networks, side payments, and employment for family members. The constraint is both extractive (career depends on serving landowners, not constituents) and coordinative (genuine provision of some local services that serve all parties, including small farmers, though unequally). Moderate extraction because the official has some discretion and some potential for exit (transfer to a less-captured jurisdiction), though at career cost.
constraint_indexing:constraint_classification(rural_institutional_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LANDED GENTRY AND EXTRACTIVE INDUSTRIES (ROPE) — Primary beneficiaries. Experiences the constraint as pure coordination: organizing the local government, credit system, and market infrastructure to serve their interests while maintaining just enough public legitimacy to prevent uprisings or external intervention. The coordination function is genuine (they must organize labor, manage risk, negotiate with state agencies). Extraction runs toward this agent. Exit is costless (can arbitrage to other jurisdictions or nations). Beneficiary perspective with full mobility and high institutional power.
constraint_indexing:constraint_classification(rural_institutional_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AGRICULTURAL UNION AND FARMER COOPERATIVES (SCAFFOLD) — Organized agents attempting to build alternative coordination structures (credit unions, direct marketing cooperatives, collective bargaining). See the institutional capture as a temporary problem solvable through institutional competition and scale. The cooperative movement has partial exit: members can access alternative credit, better prices through volume, and political voice through the union. Suppression (hostile regulation by captured local government, credit embargoes, market access denial) is high but not total. The sunset logic: if cooperatives scale to regional or national scope, they can bypass the captured local institutions. Moderate extractiveness because the organized alternative is growing.
constraint_indexing:constraint_classification(rural_institutional_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE AGRICULTURAL DEPARTMENT (PITON) — Nominally responsible for agricultural extension, technology transfer, and rural development. In practice, much of its activity is performative: extension agents visit but cannot override captured local institutions, technology transfer programs reach only cooperative farmers, and development projects are implemented through captured local governments and thus capture rents themselves. The department maintains legitimacy by staging rural development activities while the core institutional capture persists. Theater ratio high because the department's resource inputs do not match its output effectiveness — the constraint persists through inertia (bureaucratic tradition) rather than current function.
constraint_indexing:constraint_classification(rural_institutional_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: CENTRAL STATE REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by limited field capacity and dependence on local government for enforcement of national policies (land reform, labor standards, antitrust). Also benefits from rural institutional capture: it reduces the state's own service delivery burden and creates stable, manageable local power structures with whom the state can coordinate at national scale. The constraint is hybrid: the center coordinates with captured local governments (genuine coordination function) while implicitly accepting rural extraction (extractive asymmetry). Effective extraction runs from periphery (small farmers) through captured local institutions to center (state captures tax revenue and political stability). Constraint-relative power is high (the state could theoretically override capture through federal intervention), but exit is constrained by the coordination benefits the capture provides.
constraint_indexing:constraint_classification(rural_institutional_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, rural institutional capture is presented as inevitable given information asymmetries, geographic isolation, limited state capacity, and collective action barriers that make alternatives unachievable. The frame claims: dispersed rural populations cannot organize against concentrated landed interests; the state lacks capacity to extend oversight; communication technology is insufficient to enable coordination; informal institutions (reputation, patronage) are inherent to low-capacity contexts. But the structural data reveals this as a false summit: capture is maintained through active enforcement (coercion, selective regulation), not structural inevitability. The analytics will flag this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(rural_institutional_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_institutional_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_institutional_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_institutional_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_institutional_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_institutional_capture, TR),
    TR >= 0.70.

:- end_tests(rural_institutional_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant but not absolute extraction. Base calculation: the landed gentry capture an outsized share of agricultural output through monopsony pricing, credit terms, and land rental arrangements. But extraction is not total — small farmers retain some surplus (farming remains viable if marginal), and some public goods are genuinely provided. The trajectory shows increasing extractiveness over the interval (0.42 → 0.58), driven by intensification of agricultural consolidation and mechanization that reduces labor demand while increasing capital dependency (credit dependency). Suppression (0.65): High. Multiple suppression mechanisms operate: geographic isolation limits alternative employment; lack of credit access outside landowner networks prevents asset accumulation and exit; regulatory hostility toward farmer organizations (unions, cooperatives) blocks institutional alternatives; limited information about fair-market terms creates information asymmetry. Suppression is structural (material barriers) rather than purely coercive. Theater ratio (0.68): High and increasing. State agricultural agencies, cooperative development programs, and rural development initiatives maintain legitimacy through visible activity (extension visits, training programs, development projects) that does not significantly change institutional capture. The theatrical activity masks that core extraction mechanisms remain intact. Theater increases as the central state invests in rural development rhetoric to maintain political stability while tolerating capture at local level.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because beneficiaries (landowners, captured institutions) perceive the arrangement as coordination (organizing rural production efficiently) while victims perceive extraction (surplus capture with coercion). The local official perspective reveals the hybrid: they genuinely coordinate some public goods while extracting through selective enforcement. The scaffold perspective reveals that the gap can be closed through institutional competition — alternatives that provide coordination without extraction. The false summit naturalizes capture as inevitable, but the structural data (identity-locked officials, enforced suppression of alternatives, theater in state agricultural activity) reveals capture is maintained through active institutional work, not structural inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the captured local official is complex: they appear as institutional actor with moderate power, but they are constrained by patron dependency and face potential career risk if they deviate from landowner preferences. The official's d value reflects this constraint — they are neither full beneficiary nor full target, but a hybrid: they benefit from side payments and patronage advancement while being constrained by dependence on continuing patron support. This is a case where directionality override may be appropriate if the canonical derivation (institutional + constrained + beneficiary signal) produces d ≈ 0.20 but the actual structural position is d ≈ 0.40 (constrained by patron dependency). The override documents that institutional actors can be captured even when they appear to have power. Similarly, the central state's directionality is derived from receiving political stability benefit (beneficiary signal, d ≈ 0.15) but being constrained by enforcement dependency (d ≈ 0.35). No override needed — the derivation captures the constraint relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled rope classification is structurally correct: the institutional arrangement DOES provide genuine coordination (rural production, market clearing, some public goods) while ALSO sustaining extraction (surplus capture, selective regulation, suppression of alternatives). Neither pure Rope (which would ignore extraction) nor pure Snare (which would ignore coordination) accurately captures the structure. The mandatrophy resolution requires that perspectives show this hybrid across agents: beneficiaries see Rope (coordination), victims see Snare (extraction), moderate agents see Tangled Rope (both). The scaffold perspective shows organized alternatives exist — this prevents false naturalization as Mountain. The piton perspective (state agricultural department) reveals performative activity that masks persistent capture. All eight perspectives together prevent misclassification: cannot collapse to single type without losing structural accuracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_versus_intent,
    'Does rural institutional capture persist because the state lacks capacity to prevent it, or because the state chooses not to intervene?',
    'Comparative analysis: do states with equal capacity show different rural capture patterns based on anti-capture policy commitment? Do high-capacity states show lower rural capture rates? Do states with explicit decentralization policies show different outcomes than states with ostensible centralization?',
    'If capacity: rural reform requires state capacity investment and institutional infrastructure (field agents, courts, communication). If intent: rural reform requires changing central state incentives (remove benefits from capture tolerance). Classification shift: higher capacity but same capture suggests snare is correctly identified; capacity without intent changes makes capture more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_versus_intent, empirical, 'Whether capture persists due to state capacity limits or state tolerance').

omega_variable(
    alternative_institution_viability,
    'Can farmer cooperatives and agricultural unions genuinely scale to bypass captured local institutions, or are they structurally constrained to remain marginal?',
    'Historical tracking of cooperative growth rates across regions; comparison of cooperative-served vs non-cooperative-served small farmers on income, asset accumulation, and exit mobility over 10-20 year periods; analysis of institutional hostility toward cooperatives (litigation, regulatory obstruction) as function of cooperative size and threat to landowner monopoly.',
    'If scalable: scaffold perspective confirmed, sunset is real, crisis timeline is 15-25 years. If structurally marginal: scaffold is aspirational, organizational alternatives cannot overcome suppression, crisis deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institution_viability, empirical, 'Whether cooperative movements can achieve structural alternatives').

omega_variable(
    extraction_versus_coordination_balance,
    'What proportion of institutional activity represents genuine coordination (enabling rural production) versus pure extraction (capturing surplus)?',
    'Analysis of institutional service flows: credit provided at fair-market rates vs predatory rates; extension services reaching non-owner farmers vs only landowners; public goods investment (roads, schools, water) proportional to population vs concentrated in landowner-benefiting zones; regulatory enforcement even-handed vs selective.',
    'If coordination-heavy (>60% of activity): tangled rope classification confirmed, capture coexists with real service provision. If extraction-heavy (<40% coordination): snare classification more appropriate, capture involves minimal public goods provision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_versus_coordination_balance, empirical, 'Balance between genuine coordination and pure extraction in captured institutions').

omega_variable(
    identity_lock_mechanism,
    'To what extent is rural institutional capture maintained by identity fusion (locals view landowners as legitimate leaders, see capture as natural/cultural) versus structural coercion (exit barriers, resource dependency)?',
    'Qualitative interview data on whether captured community members perceive the system as changeable; comparison of identity narratives in high-capture vs low-capture regions; analysis of whether anti-capture organizing focuses on institutional alternatives or on identity reframing; tracking whether identity shifts precede or follow institutional changes.',
    'If identity-locked (consensus legitimacy of capture): institutional reform alone will fail; requires identity reframing or generational turnover. If coercion-based: institutional alternatives can overcome capture without identity change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether suppression is identity-based or coercion-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_institutional_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ric_tr_t0, rural_institutional_capture, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ric_tr_t10, rural_institutional_capture, theater_ratio, 10, 0.62).
narrative_ontology:measurement(ric_tr_t20, rural_institutional_capture, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ric_be_t0, rural_institutional_capture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ric_be_t10, rural_institutional_capture, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ric_be_t20, rural_institutional_capture, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_institutional_capture, resource_allocation).
narrative_ontology:boltzmann_floor_override(rural_institutional_capture, 0.18).
narrative_ontology:affects_constraint(rural_institutional_capture, agricultural_debt_trap).
narrative_ontology:affects_constraint(rural_institutional_capture, land_tenure_insecurity).
narrative_ontology:affects_constraint(rural_institutional_capture, market_monopsony).

% DUAL FORMULATION NOTE:
% Rural institutional capture is upstream of multiple specific extractive mechanisms (agricultural debt traps, land insecurity, monopsony pricing). Each downstream constraint has its own ε value reflecting specific institutional details, but all are structurally downstream of the institutional capture that enables them. Decomposition: capture (this story) is the institutional infrastructure; debt trap, land insecurity, and monopsony are the extraction mechanisms operating through that infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rural_institutional_capture, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
