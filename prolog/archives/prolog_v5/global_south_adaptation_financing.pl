% ============================================================================
% CONSTRAINT STORY: global_south_adaptation_financing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_south_adaptation_financing, []).

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
 *   constraint_id: global_south_adaptation_financing
 *   human_readable: Global South Adaptation Financing Constraint
 *   domain: climate/finance/development
 *
 * SUMMARY:
 *   Global adaptation financing represents a structural constraint where
 *   climate-vulnerable nations require capital to address impacts they did
 *   not cause, while capital flows through institutional architectures
 *   controlled by wealthy nations and financial institutions. The constraint
 *   exhibits Tangled Rope structure: genuine coordination function exists
 *   (mobilizing capital for climate adaptation reduces risk for all actors)
 *   alongside asymmetric extraction (vulnerable nations bear adaptation costs
 *   while Global North institutions capture financial returns). The
 *   extractiveness (0.58) reflects moderate extraction—higher than pure
 *   coordination but lower than pure capture. Suppression (0.68) is high,
 *   driven by institutional requirements (fiduciary standards, grant-writing
 *   capacity, environmental impact assessments) that function as gatekeeping
 *   mechanisms. Theater ratio (0.65) reflects moderate performative content:
 *   adaptation finance is frequently marketed as development aid while
 *   functioning partially as commercial finance or migration containment. The
 *   constraint's temporal trajectory shows increasing extractiveness and
 *   theater from t=0 to t=15, suggesting institutional capture has
 *   intensified—adaptation finance architecture is becoming more explicitly
 *   commercialized and less concessional.
 *
 * KEY AGENTS:
 *   - Vulnerable populations in climate-exposed regions: Primary victim (powerless/trapped) — face adaptation costs with no financing access; bear full climate risk
 *   - Least developed countries: Secondary victim (powerless/trapped) — excluded from adaptation finance by institutional capacity requirements; trapped by climate impacts regardless of financing barriers
 *   - Middle-income countries: Mixed victim/target (moderate/constrained) — can access financing but face high-interest debt burdens; some exit optionality but constrained by capital scarcity
 *   - Global North financial institutions and climate tech vendors: Primary beneficiary (institutional/arbitrage) — capture origination fees, interest spreads, and technology sales; full exit options
 *   - Global North wealthy populations: Secondary beneficiary (powerful/mobile) — benefit from adaptation finance as migration containment and finance generation; face shared climate impacts creating coordination incentive
 *   - Climate justice and development NGO coalition: Organized advocate (organized/constrained) — perceive sunset mechanism through norm change toward grant-based financing and loss-and-damage reparations; constrained by Global North veto power
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional structures as inherent features of climate economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_south_adaptation_financing, 0.58).
domain_priors:suppression_score(global_south_adaptation_financing, 0.68).
domain_priors:theater_ratio(global_south_adaptation_financing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_south_adaptation_financing, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_south_adaptation_financing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_south_adaptation_financing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_south_adaptation_financing, tangled_rope).
narrative_ontology:human_readable(global_south_adaptation_financing, "Global South Adaptation Financing Constraint").
narrative_ontology:topic_domain(global_south_adaptation_financing, "climate/finance/development").

domain_priors:requires_active_enforcement(global_south_adaptation_financing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_south_adaptation_financing, global_north_financial_institutions).
narrative_ontology:constraint_beneficiary(global_south_adaptation_financing, northern_climate_tech_vendors).
narrative_ontology:constraint_victim(global_south_adaptation_financing, vulnerable_populations_global_south).
narrative_ontology:constraint_victim(global_south_adaptation_financing, least_developed_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Trapped by rising climate impacts with no genuine exit option. Face adaptation costs (infrastructure, relocation, livelihood transformation) but lack financing mechanisms accessible to them. Suppression is structural: extreme poverty, geographic isolation, political marginalization. The constraint extracts through forced exposure to climate hazards while adaptation resources flow to institutional actors.
constraint_indexing:constraint_classification(global_south_adaptation_financing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEAST DEVELOPED COUNTRIES (SNARE) — Trapped by financing architecture that requires high-capacity grant writing, complex fiduciary standards, and institutional capacity. Many LDCs lack treasury systems, environmental impact assessment infrastructure, or project management capacity to access adaptation finance. Exit is trapped: climate impacts accelerate regardless of financing access barriers. Suppression derives from institutional requirements that wealthy nations use as gatekeeping.
constraint_indexing:constraint_classification(global_south_adaptation_financing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MIDDLE-INCOME COUNTRIES (TANGLED ROPE) — Constrained by high-interest debt requirements for adaptation financing. These nations can access loans but at commercial terms, creating debt servicing burdens. Genuine coordination function exists: multilateral banks do provide capital where markets fail. But asymmetric extraction occurs: concessional terms go to poorest nations; middle-income countries pay near-market rates while bearing adaptation costs. Some exit optionality through regional cooperation and national resource mobilization, but constrained by capital scarcity.
constraint_indexing:constraint_classification(global_south_adaptation_financing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GLOBAL NORTH FINANCIAL INSTITUTIONS (ROPE) — Institutional beneficiary with full arbitrage options. Experiences the constraint as a coordination mechanism: mobilizing capital to address climate risks expands markets and creates finance flows. Benefits include origination fees, interest spreads, technical assistance contracts, and export markets for northern climate technologies. Net beneficiary experiencing the constraint as manageable coordination with asymmetric gain.
constraint_indexing:constraint_classification(global_south_adaptation_financing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL NORTH WEALTHY POPULATIONS (TANGLED ROPE) — Benefit from adaptation finance architecture through exported climate technology sales, finance origination, and containment of climate migration. Also face shared climate impacts (extreme weather, supply chain disruption) that motivation creates coordination incentive around adaptation financing. But extraction asymmetry is real: Global South bears disproportionate climate risk while Global North extracts financial returns. Exit option exists (domestic climate investments) but geopolitical and economic incentives maintain the constraint.
constraint_indexing:constraint_classification(global_south_adaptation_financing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE JUSTICE COALITION (SCAFFOLD) — Organized actors (climate justice movements, development NGOs, vulnerable country alliances) perceive the constraint as temporary and solvable through norm-shift. Coalition advocacy for loss-and-damage financing, grant-based adaptation funding (not loans), and technology transfer represents sunset mechanisms. Suppression is moderate (they have some political voice and technical capacity) but constrained (Global North has structural veto power). The coalition's exit path: international norm change establishing adaptation finance as grant-based reparations rather than loans. Estimated sunset: 10-20 years for norms to mature around loss-and-damage obligations.
constraint_indexing:constraint_classification(global_south_adaptation_financing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CLIMATE PHYSICS VIEW (MOUNTAIN) — From a civilizational perspective, some financing gap is inherent to global-scale climate adaptation: adaptation costs are genuinely massive (trillions annually), and global capital allocation always favors lower-risk/higher-return investments. This view naturalizes adaptation financing constraints as immutable features of climate economics. However, structural data (suppression via institutional gatekeeping, theater via concessional term rates obscuring commercial extraction, beneficiary/victim asymmetry) contradicts the mountain framing. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(global_south_adaptation_financing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_south_adaptation_financing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_south_adaptation_financing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_south_adaptation_financing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_south_adaptation_financing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_south_adaptation_financing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Adaptation finance involves genuine capital provision (coordination function) but at terms that systematically favor Global North institutions. The extraction is not maximal because some real capital flows to Global South and some adaptation actually occurs. The temporal increase from 0.42 to 0.58 reflects intensifying commercialization: initial adaptation finance was more grant-based; current finance is increasingly loan-based with commercial interest rates. Suppression (0.68): High and persistent. Multiple mechanisms enforce suppression: (1) institutional gatekeeping (fiduciary requirements, capacity standards favor wealthy-nation actors); (2) information asymmetry (Global South nations lack technical capacity for negotiations); (3) political subordination (Global North sets agenda through G7, IMF, World Bank governance); (4) climate urgency (vulnerable nations cannot wait for better terms). Suppression has remained relatively stable across the interval—institutional structures persist despite rhetoric of increasing concessional financing. Theater ratio (0.65): Moderate and increasing. Concessional terms are marketed as development aid while often approximating commercial rates. Capacity-building investments are presented as solutions while creating consultant dependencies. Loss-and-damage financing is announced with great fanfare while remaining underfunded. The theater has increased over the interval as the gap between rhetorical commitments and actual financing has widened—this is classic Goodhart drift, where performance on publicized metrics (climate finance pledges) diverges from actual constraint function (accessible capital for vulnerable nations).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Vulnerable populations see a Snare (pure extraction with no coordination benefit to them). LDCs see a Snare (trapped by capacity requirements and climate impacts). Middle-income countries see Tangled Rope (some financing available but at high cost; mixed coordination and extraction). Global North institutions see Rope (genuine coordination problem being solved; capital provision enabling risk reduction). Global North wealthy see Tangled Rope (coordination incentive around climate stability alongside extraction benefit through finance flows and migration containment). The climate justice coalition sees Scaffold (temporary constraint being resolved through norm change toward grant-based reparations). The analytical observer risks seeing Mountain (climate economics naturally produce unequal adaptation financing as an immutable feature of massive capital requirements and market logic). The perspectival gap is driven entirely by structural position—who captures returns, who bears costs, what exit options are available, and whether the constraint's primary function is coordination or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) follow from agent position in the extraction flow and exit optionality. Vulnerable populations: trapped victims with no exit = d ≈ 0.95 (maximum extraction experienced). LDCs: trapped victims but with some institutional voice = d ≈ 0.88. Middle-income countries: victims with constrained exit options and some coordination benefits = d ≈ 0.62 (both costs and benefits, constrained mobility). Global North financial institutions: beneficiaries with arbitrage options = d ≈ 0.08 (minimal extraction, maximal benefit). Global North wealthy: beneficiaries with mobile exit options but also facing shared climate impacts = d ≈ 0.45 (mixed). Climate justice coalition: organized advocates with constrained exit options = d ≈ 0.58 (moderate experienced extraction of their energy and political capital). Each agent's directionality feeds into the sigmoid f(d) to produce their experienced effective extractiveness (χ), which combined with scope modifier σ(S) gives their classification.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy through the structural requirement that it be both coordination AND extraction. Coordination function: genuine. Adaptation financing mobilizes capital for climate risk reduction that market mechanisms alone would not provide; all agents (including Global North) benefit from climate stability. Asymmetric extraction: genuine. Global North institutions capture financial returns (fees, interest spreads, technology sales) while vulnerable nations bear adaptation costs; Global North benefits from migration containment while claiming development motives. Active enforcement: genuine. The constraint requires continuous institutional maintenance—fiduciary standards, capacity assessments, governance structures—to sustain the financing architecture. This is NOT a passive coordination arrangement like standard-setting; it requires active institutional work to maintain the extraction mechanisms (institutional gatekeeping, technology licensing, loan-based financing structure). These three requirements are simultaneously true: coordination exists (genuine capital mobilization for climate adaptation), extraction exists (asymmetric return capture), and active enforcement exists (institutional mechanisms sustaining the architecture). The mandatrophy dissolves: Tangled Rope is the correct classification because all three defining properties hold. The apparent contradiction (coordination + extraction, voluntary + enforced) reflects the actual structural tension: adaptation financing both solves a genuine coordination problem AND extracts asymmetric returns through the mechanism that solves it. This is not a false classification but the accurate representation of a mixed-function constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concessional_rate_definition,
    'What interest rate and repayment term threshold distinguishes genuinely concessional adaptation financing from commercial extraction disguised by development rhetoric?',
    'Comparative analysis of actual offered terms vs capital-weighted cost of funds; temporal tracking of whether concessional terms expand or contract over the measurement interval',
    'If concessional threshold is strict (negative real interest rates): many ''adaptation'' loans are actually commercial extraction (snare classification for moderate-income countries moves toward higher d). If threshold is permissive (positive real interest): adaptation financing appears as functional development coordination (rope classification becomes more defensible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(concessional_rate_definition, empirical, 'Definition of concessional terms vs commercial extraction in adaptation finance').

omega_variable(
    capacity_building_effectiveness,
    'Do institutional capacity-building investments (grant writing support, treasury system upgrades, fiduciary training) actually enable LDC access to adaptation finance or primarily entrench dependence on external consultants?',
    'Tracking of successful grant application rates pre- and post-capacity building; measurement of cost per successful adaptation project; longitudinal data on consultant billable hours as share of project budgets',
    'If capacity-building effective: suppression mechanism is partly addressable (Snare classification for LDCs moves toward Tangled Rope). If ineffective: suppression is structural and permanent (Mountain view of adaptation financing as an inherently unequal arrangement gains credibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_building_effectiveness, empirical, 'Whether capacity-building investments enable or entrench LDC dependence').

omega_variable(
    loss_and_damage_financing_transition,
    'Will emerging loss-and-damage financing mechanisms create a genuine exit pathway for the scaffold coalition, or will they be incorporated into the existing loan-based architecture?',
    'Tracking of loss-and-damage funding growth vs adaptation loan growth; monitoring of whether loss-and-damage finance takes grant vs loan form; assessment of whether Global North adopts reparations-based vs risk-transfer framing',
    'If grant-based and expanding: scaffold sunset is real (constraint classification shifts toward Scaffold from beneficiary/moderate perspectives). If loan-based or stalled: Global North is absorbing loss-and-damage into existing extraction architecture (constraint remains Snare/Tangled Rope despite rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_and_damage_financing_transition, empirical, 'Whether loss-and-damage financing becomes genuine reparations or loan-based extraction').

omega_variable(
    climate_migration_extraction,
    'Does adaptation financing in lower-risk Global South regions prevent climate-induced migration to Global North, thereby creating an extractive mechanism where Global North benefits from preventing migration it has partly caused?',
    'Comparative analysis of migration patterns in regions with vs without adaptation financing; econometric assessment of whether adaptation spending correlates with reduced outbound migration; measurement of Global North''s implicit WTP for migration prevention vs explicit development aid commitments',
    'If migration prevention is primary extraction mechanism: constraint is actually more extractive than base_extractiveness (0.58) suggests (true extractiveness may be 0.72+). If primary mechanism is genuine climate risk reduction: extractiveness assessment is approximately correct (Tangled Rope classification holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_migration_extraction, empirical, 'Whether adaptation financing functions as migration prevention extraction').

omega_variable(
    technology_transfer_capture,
    'Do Global North climate technology vendors capture adaptation finance flows through licensing restrictions, proprietary designs, and patent enforcement, preventing genuine technology transfer despite development finance intent?',
    'Tracking of open-source vs proprietary technology deployment in adaptation projects; measurement of IP licensing costs as share of adaptation budgets; longitudinal assessment of whether Global South develops indigenous adaptation technology capacity or remains dependent on northern vendors',
    'If technology transfer is captured: adaptation finance becomes a mechanism for entrenching market dependencies (extractiveness increases; Snare classification becomes more accurate). If technology transfer succeeds: adaptation finance enables genuine capacity building (Rope or Tangled Rope with bounded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_capture, empirical, 'Whether proprietary technology licensing captures adaptation finance flows').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_south_adaptation_financing, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gsaf_tr_t0, global_south_adaptation_financing, theater_ratio, 0, 0.5).
narrative_ontology:measurement(gsaf_tr_t7, global_south_adaptation_financing, theater_ratio, 7, 0.58).
narrative_ontology:measurement(gsaf_tr_t15, global_south_adaptation_financing, theater_ratio, 15, 0.65).
narrative_ontology:measurement(gsaf_tr_t10, global_south_adaptation_financing, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(gsaf_be_t0, global_south_adaptation_financing, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gsaf_be_t7, global_south_adaptation_financing, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(gsaf_be_t15, global_south_adaptation_financing, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(gsaf_be_t10, global_south_adaptation_financing, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_south_adaptation_financing, resource_allocation).
narrative_ontology:affects_constraint(global_south_adaptation_financing, climate_loss_and_damage).
narrative_ontology:affects_constraint(global_south_adaptation_financing, technology_transfer_ip_capture).
narrative_ontology:affects_constraint(global_south_adaptation_financing, climate_migration_pressures).

% DUAL FORMULATION NOTE:
% Global adaptation financing decomposes into structurally distinct constraints: (1) adaptation_finance_mobilization (coordination function, moderate ε) — the genuine problem of raising capital for climate risk reduction; (2) adaptation_finance_extraction (extraction function, higher ε) — the institutional capture and commercial structuring of that capital. This story focuses on the combined constraint (Tangled Rope) that unifies both functions. The upstream constraint is climate_loss_and_damage (the unmet financing gap for non-adaptation climate impacts); the downstream constraint is technology_transfer_ip_capture (how adaptation finance flows get recaptured through intellectual property mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
