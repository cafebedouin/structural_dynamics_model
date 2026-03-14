% ============================================================================
% CONSTRAINT STORY: enterprise_ai_integration_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_enterprise_ai_integration_lock_in, []).

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
 *   constraint_id: enterprise_ai_integration_lock_in
 *   human_readable: Enterprise AI Integration Lock-In
 *   domain: technology/business/economics
 *
 * SUMMARY:
 *   Enterprise AI integration lock-in represents a structural constraint
 *   where vendors and IT departments coordinate legitimate AI adoption while
 *   systematically extracting from enterprises through switching cost
 *   accumulation. Initial adoption appears to solve genuine operational
 *   problems (coordination function), but integration depth creates
 *   technical, contractual, and organizational dependencies that raise exit
 *   costs monotonically. This constraint exhibits the full spectrum of DR
 *   classifications: large enterprises experience coordination (Rope);
 *   mid-market enterprises experience pure extraction (Snare); IT departments
 *   experience mixed coordination-extraction (Tangled Rope); open standards
 *   coalitions see a temporary problem being solved (Scaffold); legacy
 *   infrastructure persists through inertia (Piton). The constraint's
 *   extractiveness has grown from 0.28 at initial adoption to 0.58 as
 *   integrations deepen, indicating systematic rent-seeking layered onto
 *   coordination functions. Theater ratio (0.55) reflects the performative
 *   elements of enterprise AI governance: vendor-mandated compliance reviews,
 *   internal approval rituals, and integration governance that largely
 *   duplicate vendor capabilities.
 *
 * KEY AGENTS:
 *   - AI Vendor Ecosystem: Primary beneficiary (institutional/arbitrage) — coordinates enterprise adoption while extracting through multi-year contracts, ecosystem expansion, and vendor lock-in
 *   - Mid-Market Enterprises: Primary victim (powerless/trapped) — face prohibitive switching costs once integrated; constrained by technical, contractual, and organizational dependencies
 *   - Enterprise IT Departments: Secondary beneficiary/victim (moderate/constrained) — gain operational efficiency but lose negotiating power; face career risk from admission of failed implementation
 *   - Open Standards Coalition: Organized agents (organized/constrained) — building open-source AI, data portability standards, and vendor-agnostic platforms as alternative pathways
 *   - Large Hyperscale Enterprises: Powerful actor (powerful/mobile) — negotiate custom agreements and maintain multi-vendor relationships; extract value through negotiating leverage
 *   - Legacy Enterprise Architecture: Institutional actor (institutional/arbitrage) — maintains governance theater; persists through inertia despite duplication of vendor capabilities
 *   - Analytical Observer: Civilizational position (analytical/analytical) — identifies structure as systematic extraction masked by coordination narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(enterprise_ai_integration_lock_in, 0.58).
domain_priors:suppression_score(enterprise_ai_integration_lock_in, 0.68).
domain_priors:theater_ratio(enterprise_ai_integration_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(enterprise_ai_integration_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(enterprise_ai_integration_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(enterprise_ai_integration_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(enterprise_ai_integration_lock_in, tangled_rope).
narrative_ontology:human_readable(enterprise_ai_integration_lock_in, "Enterprise AI Integration Lock-In").
narrative_ontology:topic_domain(enterprise_ai_integration_lock_in, "technology/business/economics").

domain_priors:requires_active_enforcement(enterprise_ai_integration_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(enterprise_ai_integration_lock_in, ai_vendor_ecosystem).
narrative_ontology:constraint_beneficiary(enterprise_ai_integration_lock_in, enterprise_it_departments).
narrative_ontology:constraint_victim(enterprise_ai_integration_lock_in, mid_market_enterprises).
narrative_ontology:constraint_victim(enterprise_ai_integration_lock_in, downstream_operational_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MID-MARKET ENTERPRISE OPERATIONS (SNARE) — Once integrated into vendor-specific AI infrastructure, exit is prohibitively expensive. Technical integration creates switching costs (retraining, data migration, workflow redesign); contractual lock-in (multi-year commitments, penalty clauses) creates legal barriers; organizational lock-in (staff trained on vendor tools, processes built around vendor APIs) creates human capital barriers. No meaningful alternatives once committed.
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE IT DEPARTMENT (TANGLED ROPE) — Experiences both coordination benefit and extraction. AI integration genuinely improves workflow efficiency and decision support (coordination function), but IT departments become dependent on vendor support, face pressure to adopt additional vendor products (upsell extraction), and lose negotiating power as switching costs accumulate. Constrained exit due to career risk (admitting failed implementation) and operational disruption.
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI VENDOR ECOSYSTEM (ROPE) — Primary beneficiary. Vendors coordinate enterprise AI deployment through standardized platforms, APIs, and integrations. The constraint functions as a coordination mechanism from their perspective: they solve the legitimate problem of enterprise AI adoption at scale. Arbitrage exit enables (can move between vendors if terms are unfavorable) but sticky customer base produces revenue predictability. Net positive extraction position.
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Organized agents (open-source communities, standards bodies, regulatory bodies) are building interoperable AI frameworks, vendor-agnostic platforms, and data portability requirements that create alternative pathways. EU AI Act, open-weight models, and containerization standards represent sunset mechanisms. Lock-in is being reduced by regulatory and technical innovation, but processes are slow (5-10 year timescale).
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ENTERPRISE ARCHITECTURE (PITON) — Traditional on-premises infrastructure and internal IT governance structures are increasingly theatrical. Vendors coordinate around cloud-native AI while enterprises maintain elaborate internal review processes, data governance rituals, and infrastructure that largely duplicate vendor capabilities. The constraint persists through institutional inertia — enterprises have invested in governance layers that now exist primarily to justify their own existence rather than serving essential functions.
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE HYPERSCALE ENTERPRISES (TANGLED ROPE) — Tech giants and large enterprises have sufficient scale and technical capacity to negotiate custom agreements, build hybrid solutions, or maintain multiple vendor relationships. They experience genuine coordination benefit (AI platforms solve real problems) but also extract value through negotiating power and multi-vendor strategies. Mobile exit options give them agency that mid-market lacks. Mixed extraction reflects their intermediate structural position.
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE AT SCALE) — From a civilizational perspective, the lock-in constraint systematically extracts from smaller enterprises while benefiting vendors and large enterprises. The structure resembles a financial snare: initial low switching costs lure adoption; subsequent integrations raise switching costs monotonically; suppliers exploit the accumulated lock-in through price increases, forced upgrades, and ecosystem expansion. The constraint is maintained through active enforcement (contract design, API deprecation, vendor bundle strategies).
constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(enterprise_ai_integration_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(enterprise_ai_integration_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(enterprise_ai_integration_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(enterprise_ai_integration_lock_in, TR),
    TR >= 0.70.

:- end_tests(enterprise_ai_integration_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. Initial adoption (t=0, ε=0.28) involves genuine coordination benefit — vendors solve real enterprise AI deployment challenges. But extractiveness increases monotonically as switching costs accumulate (t=2 to t=6 shows clear trend: 0.28 → 0.38 → 0.48 → 0.58). This trajectory reflects systematic rent-seeking: vendors lock in customers through multi-year contracts, mandatory upgrades, ecosystem dependencies, and API changes that force ongoing investment. The increase is not accidental — it reflects active enforcement (vendors intentionally raise switching costs through contract design and technical strategies). Suppression (0.68): High. Multiple barriers to exit: technical switching costs (data migration, workflow redesign, retraining), contractual penalties (early termination fees), organizational lock-in (staff trained on vendor tools), and opportunity costs (disruption during transition). Enterprises face realistic options of: (a) pay vendor terms, (b) absorb massive switching costs, or (c) maintain status quo. None are attractive. Theater ratio (0.55): Moderate. Enterprise AI governance involves performative elements — compliance reviews, integration approvals, and architecture governance that largely duplicate vendor-provided functionality. But theater is not dominant; much of the governance is genuinely required. The ratio reflects that vendor strategies increasingly use theater (staged product releases, mandatory certifications) to normalize escalating lock-in.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival disagreement. Vendors perceive Rope — they solve a legitimate coordination problem (enterprise AI adoption at scale). IT departments perceive Tangled Rope — they gain efficiency but lose autonomy as vendor dependency grows. Mid-market enterprises perceive Snare — they are locked in with no effective exit. Large enterprises perceive Tangled Rope — they have enough scale to negotiate and maintain alternatives. Open standards coalitions perceive Scaffold — regulatory and technical innovations (data portability, open-source alternatives) are creating sunset pathways. Legacy infrastructure perceives Piton — governance processes persist through inertia. The analytical observer perceives Snare at scale — systematic extraction targeting smaller enterprises while benefiting vendors and large actors. The perspectival gap reflects real structural differences: enterprises of different sizes experience fundamentally different constraints from identical vendor behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural positions within the constraint. Vendors with arbitrage exit options (can shift between customers, can compete with alternatives) experience low d ≈ 0.15, producing negative effective extraction through f(d). Mid-market enterprises trapped by switching costs experience high d ≈ 0.92, producing maximum f(d) ≈ 1.40, making extraction painfully visible to them. IT departments constrained by career risk and operational disruption experience intermediate d ≈ 0.65, producing f(d) ≈ 1.00. The analytical observer at universal scope experiences d ≈ 0.72 (observing the systematic extraction), producing f(d) ≈ 1.15. These values are derived from the beneficiary/victim declarations and exit options without additional override — the directionality structure correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE vs EXTRACTION AMBIGUITY: The core mandatrophy question is whether enterprises MANDATE vendors (adopt them because they solve necessary problems) or whether vendors EXTRACT from enterprises (lock them in to exploit accumulated dependency). The classification reveals that both are true simultaneously. Early adoption (t=0, ε=0.28) appears as pure mandate — enterprises adopt AI because they need it. But extractiveness increases monotonically (trajectory: 0.28 → 0.58) while the coordination function remains constant. This reveals that vendors are actively layering extraction onto genuine mandate. The constraint resolves mandatrophy by distinguishing temporal phases: Phase 1 (t=0-2): Genuine mandate (enterprises adopt for real operational benefit). Phase 2 (t=2-4): Mandate + extraction begins (vendors introduce lock-in mechanisms). Phase 3 (t=4-6): Extraction dominates (switching costs exceed operational benefits, but enterprises cannot exit). The Tangled Rope classification captures this hybrid nature: the constraint remains functional (coordination) while becoming increasingly extractive (lock-in). Mid-market enterprises experience this as Snare (pure extraction) because they lack the scale to negotiate; large enterprises experience it as Tangled Rope (mixed) because they can maintain leverage. The mandatrophy is not resolved by choosing one type, but by recognizing that the type varies across observational positions and time horizons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_displacement_timeline,
    'Will open-source AI frameworks (Hugging Face, LLaMA, open-weight models) displace vendor lock-in before enterprises reach critical switching cost thresholds?',
    'Market adoption curves for open-source AI vs proprietary; enterprise switching events; competitive pricing dynamics; capability parity timeline',
    'If open-source displaces within 3-5 years: lock-in constraint reverts to temporary coordination (Scaffold). If vendors maintain technical advantage for 10+ years: lock-in entrenches as persistent Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_displacement_timeline, empirical, 'Whether open-source AI will provide competitive alternative before lock-in entrenches').

omega_variable(
    data_portability_enforcement,
    'Will regulatory data portability requirements (EU AI Act, DMA, equivalent regulations) create enforceable exit mechanisms before switching costs accumulate irreversibly?',
    'Regulatory timeline for enforcement; compliance costs vs switching costs; legal challenges to portability requirements; vendor technical response to portability mandates',
    'If enforcement precedes critical lock-in: regulatory scaffold creates exit pathway, reducing Snare risk. If enforcement lags: vendors achieve lock-in before regulatory intervention becomes effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_enforcement, empirical, 'Whether regulatory data portability will enable exit before lock-in becomes irreversible').

omega_variable(
    multi_vendor_fragmentation_cost,
    'Does maintaining multi-vendor AI ecosystems (hedge against lock-in) consume enough operational overhead that it recreates the lock-in problem at a higher level of complexity?',
    'Cost accounting for multi-vendor integration; staff overhead; coordination complexity; enterprise case studies comparing single-vendor vs multi-vendor approaches',
    'If multi-vendor overhead exceeds single-vendor lock-in costs: enterprises cannot hedge effectively, and lock-in constraint becomes inescapable (high Snare). If multi-vendor remains cheaper: enterprises have real exit option, reducing Snare severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_vendor_fragmentation_cost, empirical, 'Whether multi-vendor strategy is cost-effective as lock-in hedge').

omega_variable(
    vendor_service_dependency_substitution,
    'Can in-house technical capacity or third-party service providers (systems integrators, consultants) substitute for vendor lock-in, or does vendor technical support become irreplaceable as AI systems scale?',
    'Enterprise capability development timelines; consultant/integrator market competence surveys; vendor support necessity analysis; internal staff expertise scaling',
    'If substitutable: enterprises can reduce dependency and escape lock-in (exits closer to mobile/constrained). If irreplaceable: vendor becomes infrastructure, lock-in becomes permanent (trapped exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_service_dependency_substitution, empirical, 'Whether vendor support can be substituted by internal capacity or third parties').

omega_variable(
    switching_cost_trajectory_nonlinearity,
    'Do switching costs accumulate linearly with integration depth (predictable) or do they exhibit threshold effects and discontinuities (catastrophic lock-in)?',
    'Historical case studies of enterprise exits; cost measurement across integration stages; identification of nonlinear breakpoints (e.g., when internal tools become irreplaceable)',
    'If linear: enterprises have predictable decision points and can manage lock-in risk. If nonlinear: sudden cost explosions trap enterprises unexpectedly, shifting classification toward Snare from entire spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_trajectory_nonlinearity, empirical, 'Whether switching costs accumulate linearly or exhibit catastrophic threshold effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(enterprise_ai_integration_lock_in, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eai_tr_t0, enterprise_ai_integration_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(eai_tr_t2, enterprise_ai_integration_lock_in, theater_ratio, 2, 0.4).
narrative_ontology:measurement(eai_tr_t4, enterprise_ai_integration_lock_in, theater_ratio, 4, 0.5).
narrative_ontology:measurement(eai_tr_t6, enterprise_ai_integration_lock_in, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(eai_be_t0, enterprise_ai_integration_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eai_be_t2, enterprise_ai_integration_lock_in, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(eai_be_t4, enterprise_ai_integration_lock_in, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(eai_be_t6, enterprise_ai_integration_lock_in, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(enterprise_ai_integration_lock_in, resource_allocation).
narrative_ontology:boltzmann_floor_override(enterprise_ai_integration_lock_in, 0.12).
narrative_ontology:affects_constraint(enterprise_ai_integration_lock_in, vendor_ecosystem_concentration).
narrative_ontology:affects_constraint(enterprise_ai_integration_lock_in, enterprise_technical_debt_accumulation).
narrative_ontology:affects_constraint(enterprise_ai_integration_lock_in, ai_governance_theater_escalation).

% DUAL FORMULATION NOTE:
% Enterprise AI integration lock-in is upstream of specific vendor relationships and downstream of broader enterprise digital transformation dynamics. Vendor concentration, technical debt, and governance theater are structurally related constraints that influence and are influenced by lock-in mechanisms. The family has three members: vendor_ecosystem_concentration (structural why lock-in is possible), enterprise_ai_integration_lock_in (mechanism), and enterprise_technical_debt_accumulation (consequence). All three should be analyzed in ecosystem context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(enterprise_ai_integration_lock_in, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
