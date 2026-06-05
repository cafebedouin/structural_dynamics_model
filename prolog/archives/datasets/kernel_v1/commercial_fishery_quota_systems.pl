% ============================================================================
% CONSTRAINT STORY: commercial_fishery_quota_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commercial_fishery_quota_systems, []).

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
 *   constraint_id: commercial_fishery_quota_systems
 *   human_readable: Commercial Fishery Quota Systems
 *   domain: environmental_regulation/resource_extraction
 *
 * SUMMARY:
 *   Commercial fishery quota systems represent a layered institutional
 *   mechanism that solves a genuine commons tragedy (overfishing and stock
 *   collapse) while simultaneously enabling rent extraction and equity
 *   displacement from small-scale fishers. The constraint operates globally
 *   through international agreements (United Nations Convention on the Law of
 *   the Sea, regional fisheries management organizations) and is implemented
 *   through national licensing and catch monitoring systems. The core
 *   coordination function is real: without managed access, competitive
 *   fishing depletes stocks below sustainable levels within years. However,
 *   the implementation mechanism — Transferable Catch Quota (TCQ) or
 *   Individual Transferable Quota (ITQ) systems — has evolved to concentrate
 *   fishing rights toward large industrial operators through quota trading
 *   and capital barriers to entry. Small-scale artisanal fishers, who
 *   historically accessed open-access commons, are economically or legally
 *   excluded, creating asymmetric extraction that persists under the cover of
 *   conservation necessity. The measurements show increasing extractiveness
 *   (0.35→0.52) and rising theater ratio (0.32→0.48) over the 20-year
 *   interval, indicating that the constraint's performance has drifted from
 *   conservation coordination toward rent consolidation and performance
 *   theater.
 *
 * KEY AGENTS:
 *   - Incumbent Industrial Fleets: Primary beneficiary (institutional/arbitrage) — received substantial initial allocations and benefit from quota trading consolidation. Experience the system as coordination.
 *   - Coastal Fishing Communities: Primary victim (powerless/trapped) — artisanal and small-scale fishers excluded from quota access; bear career displacement without compensation.
 *   - Fish Stocks / Ecosystem Health: Secondary victim (powerless/trapped) — abstract collective that bears cost when enforcement fails or extraction exceeds sustainable limits.
 *   - Regional Fishing Cooperatives: Mixed agent (organized/constrained) — have some organizational capacity and voice but remain constrained by consolidation dynamics.
 *   - Fishery Management Authority: Institutional coordinator (institutional/arbitrage) — solves commons tragedy mandate while facing political pressure from incumbent operators and displaced communities.
 *   - Coastal State Government: Institutional mediator (institutional/constrained) — benefits from licensing revenue and employment maintenance but constrained by competing stakeholder demands and international agreements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commercial_fishery_quota_systems, 0.52).
domain_priors:suppression_score(commercial_fishery_quota_systems, 0.58).
domain_priors:theater_ratio(commercial_fishery_quota_systems, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, extractiveness, 0.52).
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(commercial_fishery_quota_systems, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commercial_fishery_quota_systems, tangled_rope).
narrative_ontology:human_readable(commercial_fishery_quota_systems, "Commercial Fishery Quota Systems").
narrative_ontology:topic_domain(commercial_fishery_quota_systems, "environmental_regulation/resource_extraction").

domain_priors:requires_active_enforcement(commercial_fishery_quota_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commercial_fishery_quota_systems, incumbent_fishing_fleets).
narrative_ontology:constraint_beneficiary(commercial_fishery_quota_systems, quota_trading_operators).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, coastal_fishing_communities).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, fish_stock_sustainability).
narrative_ontology:constraint_victim(commercial_fishery_quota_systems, small_scale_fishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL FISHING COMMUNITY (SNARE) — Small-scale artisanal fishers are excluded from quota allocation or allocated subsistence-level permits. They face legal barriers to fishing (quota enforcement), economic barriers to quota purchase (prohibitive cost), and geographic barriers to relocation. The constraint extracts their historical fishing rights while providing no compensation or alternative livelihood. Maximum perceived extraction with no exit path.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDEPENDENT FISHING BOAT OWNER (SNARE) — Owns a single vessel but cannot access quota without purchasing permits at escalating market prices or inheriting historical allocation. Faces suppression through permit costs (economic barrier), catch monitoring (enforcement), and quota trading concentration that favors large operators. Can theoretically exit (sell vessel, change occupation) but at severe career and financial cost. High extraction with constrained exit.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL FISHING COOPERATIVE (TANGLED ROPE) — Organized groups benefit from collective quota allocation and can negotiate with regulators, but remain constrained by centralized catch limits and quota trading mechanics that favor consolidation. The quota system coordinates actual fish stock management (genuine coordination function) while extracting economic value from smaller operators (asymmetric extraction). Organized exit options and some agency, but caught between sustainability imperatives and economic displacement.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT INDUSTRIAL FLEET (ROPE) — Large-scale operators received substantial initial quota allocations (historical catch-based grandfathering) and benefit from quota trading infrastructure that consolidates fishing rights toward large players. They experience the quota system as pure coordination: managing overall catch prevents stock collapse, which would eliminate their fishing grounds entirely. Exit options (investing in alternative fisheries, relocating to new regions, quota arbitrage) are available. Net beneficiary experiencing coordination rather than extraction.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FISHERY MANAGEMENT AUTHORITY (ROPE) — Regulatory agency benefits from quota systems as a coordination mechanism: total allowable catch (TAC) limits prevent stock collapse and maintain fishery viability over time. The system solves their core mandate (prevent tragedy of the commons) with relatively low administrative overhead compared to effort-based management. Exit options (switching to alternative management schemes, ceding authority to fishing industry self-regulation) are theoretically available but politically constrained. Sees quota as effective coordination.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COASTAL STATE GOVERNMENT (TANGLED ROPE) — Benefits from quota systems by extracting licensing revenue and maintaining fishery employment (political stability), while also constrained by international fishing agreements, competing stakeholder demands (environmental groups, fishing industry, indigenous rights), and the genuine need for stock management. Experiences the system as hybrid: legitimate coordination function (prevents stock collapse) plus asymmetric extraction (consolidates fishing rights toward industrial operators, displaces small-scale fishers).
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FISH STOCK AND ECOSYSTEM HEALTH (SNARE) — The constraint's nominal target (fish stock management) functions only to the extent that TAC limits are set below maximum sustainable yield and enforced. However, quota trading, subsidy capture, and illegal unreported unregulated (IUU) fishing mean the nominal constraint is regularly violated. The fish stock cannot exit and bears the cost of extraction whenever actual catch exceeds sustainable limits. Abstract collective victim with no organizational capacity.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DEGRADATION VIEW (PITON) — The quota system has evolved from a conservation mechanism (1970s-1990s: strict TAC enforcement, stock recovery) into a theater for rent extraction and equity displacement (2000s-present: quota consolidation, IUU fishing, subsidy-driven overcapacity masking true stock status). The core conservation function persists but is overshadowed by performative compliance and hidden extraction. Theater ratio is rising as quota trading becomes the dominant mechanism and catch monitoring becomes ritualized.
constraint_indexing:constraint_classification(commercial_fishery_quota_systems, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commercial_fishery_quota_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commercial_fishery_quota_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commercial_fishery_quota_systems, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commercial_fishery_quota_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commercial_fishery_quota_systems, TR),
    TR >= 0.70.

:- end_tests(commercial_fishery_quota_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The quota system extracts fishing rights from historical open-access commons and redistributes them through market mechanisms that favor capital-rich operators. The extraction is not maximal (0.70+) because the system does maintain genuine stock sustainability and the efficiency gains from quota trading are partially real — catch is reallocated toward operators with lower costs and better technology. However, the extraction mechanism is substantial: entry barriers (quota cost), exclusionary allocation (historical grandfathering), and consolidation dynamics all concentrate rents. Suppression (0.58): Moderate-high. Enforcement mechanisms include catch monitoring (port inspections, observer programs), license revocation, vessel impoundment, and fines. These are substantial but not absolute — enforcement varies by region and nation, and IUU fishing is persistent. Small operators face barriers through permit costs (economic suppression) and legal exclusion (regulatory suppression). Theater ratio (0.48): Moderate. The quota system has functional conservation components (TAC-setting based on stock assessments, catch monitoring) but increasingly performative elements (quota trading theater that obscures actual conservation outcomes, compliance reporting that masks IUU fishing). The rising trajectory (0.32→0.48) reflects increasing mismatch between the system's nominal conservation function and its actual rent-extraction function.
 *
 * PERSPECTIVAL GAP:
 *   The clearest gap is between the incumbent fleet's Rope classification and the small-scale fisher's Snare classification. Both are experiencing the same quota system. The incumbent perceives coordination (preventing stock collapse) because they benefit from the allocation and can arbitrage within the system. The small-scale fisher perceives extraction (losing historical rights) because they are excluded and trapped. The management authority's Rope classification (solves the commons tragedy) rests on the assumption that stock sustainability is achieved through quota enforcement. The piton perspective challenges this: if enforcement is degraded (high IUU, subsidy-driven overcapacity) and theater is rising, the coordination function may be nominal while extraction persists. The delta between perspectives reflects whether the quota system's true function is conservation (rope/tangled rope) or rent consolidation masked as conservation (piton/false summit).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. The incumbent industrial fleet benefits from quota allocation and trading; they have arbitrage options (invest in different fisheries, relocate, acquire more quota). Their d≈0.05-0.15, producing negative or minimal f(d), so χ is low/negative — they experience the quota system as a coordination benefit. The small-scale fisher is trapped (no capital to buy quota, limited geographic mobility, culturally dependent on fishing); they have no exit options. Their d≈0.90-0.95, producing maximum f(d)≈1.42, so χ is high — they experience maximum extraction. The regional cooperative is organized but constrained by consolidation mechanics; their d≈0.55-0.65, producing moderate f(d)≈0.75-1.00, so χ is moderate. The scope modifier σ(S) also applies: regional scope (σ=0.9) slightly dampens extractiveness, while global scope (σ=1.2) amplifies it. The global scale at which quota trading operates means that consolidation is not local friction but a systematic transnational mechanism, amplifying χ for those affected.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is the apparent necessity of choosing between commons collapse (no quota system) and equity displacement (quota consolidation toward large operators). The system narrative presents this as unavoidable: 'without transferable quotas, small fishers cannot access fish because they lack capital to buy quota; but without quota trading, the system cannot achieve conservation.' This forces small fishers into a false binary: accept exclusion or tolerate stock collapse. The omega variables suggest this is resolvable: alternative allocation mechanisms (equal division per community, protection of small-fisher baseline allocation, spatial management instead of catch quotas) could achieve conservation outcomes without consolidation. The mandatrophy resolves by recognizing that the tangled rope classification is correct — genuine coordination function exists alongside asymmetric extraction — but the asymmetry is not necessary for coordination. The extracted rents (quota consolidation toward large operators) could be redistributed without destroying the conservation function. The constraint remains tangled rope (both functions present) but the resolution pathway is equity-preserving quota redesign rather than false choice between conservation and fairness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tac_setting_accuracy,
    'Are Total Allowable Catch limits set scientifically based on actual stock biomass, or are they politically negotiated values that systematically underestimate sustainable capacity to favor incumbent operators?',
    'Comparison of TAC recommendations from stock assessment scientists vs. final regulatory TAC values; correlation of TAC-setting patterns with incumbent fleet interests; historical analysis of stock recovery vs. TAC adherence',
    'If scientifically set: quota system is legitimate coordination (stronger Rope classification). If politically negotiated: TAC is a cover story for consolidation (stronger Snare classification for victims). Affects whether the beneficiary is receiving legitimate coordination benefit or extractive rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tac_setting_accuracy, empirical, 'Whether TAC limits reflect scientific consensus or political negotiation favoring incumbents').

omega_variable(
    quota_trading_function,
    'Does quota trading enable efficient reallocation of catch rights to operators who can fish most sustainably, or does it concentrate rights toward large operators regardless of efficiency or sustainability outcomes?',
    'Analysis of quota trading volume and directionality over time; correlation between quota concentration and catch-per-unit-effort; comparison of fishing practices between quota owners and quota lessees',
    'If trading is efficiency-driven: tangled rope classification is correct (genuine coordination + asymmetric extraction). If trading concentrates rights without efficiency gains: system is closer to pure snare for small operators. Affects whether asymmetry reflects legitimate economic sorting or rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quota_trading_function, empirical, 'Whether quota trading improves efficiency or concentrates rents').

omega_variable(
    iuu_fishing_scale,
    'What proportion of actual catch is Illegal, Unreported, Unregulated (IUU) fishing, and does this represent a constraint-level failure of enforcement (rendering the nominal quota meaningless) or a survivable margin of system noise?',
    'Port-level catch monitoring; at-sea observer programs; scientific stock assessment residuals (gap between predicted catch and observed stock decline); comparison of IUU estimates across regions and time periods',
    'If IUU ≥ 30% of nominal TAC: enforcement is structurally degraded (piton classification is correct — theater persists despite functional failure). If IUU ≤ 10%: enforcement is working (tangled rope classification holds). Affects whether the constraint is a functional mechanism or a performative ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iuu_fishing_scale, empirical, 'Proportion of total catch that is Illegal, Unreported, or Unregulated').

omega_variable(
    small_scale_fisher_alternatives,
    'Do excluded small-scale fishers have viable economic alternatives (aquaculture, alternative employment, subsistence food security without commercial fishing) or are they genuinely trapped in a zero-sum choice between illegal fishing and economic destitution?',
    'Comparative analysis of coastal communities with/without quota access; longitudinal tracking of excluded fisher livelihoods; assessment of alternative livelihood viability in low-income coastal regions',
    'If alternatives exist: powerless/trapped classification may be overstated (could reclassify toward constrained). If no alternatives exist: trapped classification is correct and snare extraction is severe. Affects assessment of suppression mechanism (structural vs. lack of alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_scale_fisher_alternatives, empirical, 'Whether excluded fishers have viable economic alternatives').

omega_variable(
    stock_recovery_causation,
    'When fish stocks have recovered (e.g., Atlantic cod rebuilding, some Pacific rockfish recovery), is recovery causally attributable to quota enforcement or to other factors (fishing technology changes, marine spatial planning, pollution reduction, ocean conditions)?',
    'Counterfactual analysis: comparison of recovery patterns between regions with strong quota enforcement vs. regions with weak enforcement; temporal correlation analysis (does recovery begin after quota implementation or earlier?); ecosystem modeling isolating quota impact from confounders',
    'If quota is causal: coordination function is real (rope/tangled rope classification confirmed). If confounded: quota may be performative cover for stock recovery driven by other mechanisms (piton classification more likely). Affects the fundamental legitimacy of the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stock_recovery_causation, empirical, 'Whether quota enforcement causally drives fish stock recovery').

omega_variable(
    equity_vs_sustainability_tradeoff,
    'Is the quota system''s consolidation toward large operators a necessary condition for stock sustainability (i.e., does equity necessarily degrade conservation outcomes), or is this a contingent design choice that could be restructured with different allocation and enforcement mechanisms?',
    'Comparative analysis of quota systems with different allocation rules (equal division, community-based allocation, historical small-fisher protection); assessment of conservation outcomes across allocation regimes; case studies of alternative management approaches (spatial closures, effort limits, customary tenure)',
    'If necessary tradeoff: snare extraction for small fishers is the cost of preventing stock collapse (justifies but clarifies the constraint). If contingent: alternative designs could achieve conservation without consolidation (suggests piton or false-summit dynamics where equity displacement is presented as natural rather than chosen).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equity_vs_sustainability_tradeoff, conceptual, 'Whether quota consolidation is necessary for conservation or a contingent design choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commercial_fishery_quota_systems, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfqs_tr_t0, commercial_fishery_quota_systems, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cfqs_tr_t10, commercial_fishery_quota_systems, theater_ratio, 10, 0.42).
narrative_ontology:measurement(cfqs_tr_t20, commercial_fishery_quota_systems, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(cfqs_be_t0, commercial_fishery_quota_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cfqs_be_t10, commercial_fishery_quota_systems, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cfqs_be_t20, commercial_fishery_quota_systems, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cfqs_su_t0, commercial_fishery_quota_systems, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cfqs_su_t10, commercial_fishery_quota_systems, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(cfqs_su_t20, commercial_fishery_quota_systems, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commercial_fishery_quota_systems, resource_allocation).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, marine_protected_area_enforcement).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, subsidy_driven_fishing_capacity).
narrative_ontology:affects_constraint(commercial_fishery_quota_systems, iuu_fishing_supply_chain).

% DUAL FORMULATION NOTE:
% The quota system's conservation function and its rent-extraction function are structurally entangled but analytically separable. The conservation function (preventing tragedy of the commons through catch limits) is upstream; the allocation and trading mechanics are downstream mechanisms that implement conservation but also enable extraction. Subsidy-driven fishing capacity undercuts the system's conservation efficacy by masking true stock status. IUU fishing represents extraction that bypasses the constraint entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commercial_fishery_quota_systems, powerless, 0.92).
constraint_indexing:directionality_override(commercial_fishery_quota_systems, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
