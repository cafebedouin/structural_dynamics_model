% ============================================================================
% CONSTRAINT STORY: great_mongolian_road_economic_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_great_mongolian_road_economic_dependency, []).

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
 *   constraint_id: great_mongolian_road_economic_dependency
 *   human_readable: Economic Dependency via Infrastructure Development (Great Mongolian Road)
 *   domain: economic/development_infrastructure
 *
 * SUMMARY:
 *   The Great Mongolian Road initiative represents a classic
 *   infrastructure-led development strategy: Japan funds large-scale road and
 *   connectivity projects in Mongolia, marketed as mutual benefit and
 *   regional coordination. The structural reality is more complex. The
 *   constraint exhibits the full spectrum of DR classifications depending on
 *   observer position, revealing the tension between legitimate development
 *   coordination and extractive dependency mechanisms. Japanese corporations
 *   and capital benefit immediately and unambiguously (institutional
 *   perspective: Rope). Mongolian political leadership at the central level
 *   benefits from infrastructure but accepts debt and policy constraints
 *   (organized perspective: Tangled Rope, bordering on regulatory capture).
 *   Mongolian rural labor forces lose traditional livelihoods without
 *   equivalent alternative employment and face wage suppression (powerless
 *   perspective: Snare). Regional governments gain connectivity but lose
 *   operational autonomy (moderate perspective: Tangled Rope). International
 *   development organizations frame the constraint as temporary and solvable
 *   through capacity building (organized perspective: Scaffold with weak
 *   sunset enforcement). The post-Soviet development paradigm naturalizes
 *   infrastructure investment as a universal solution despite mounting
 *   evidence that it routinely enables extraction (institutional perspective:
 *   Piton, performative maintenance of failed model). The analytical observer
 *   risks seeing landlocked geography as a natural law that 'forces'
 *   dependency, when the dependency is actually a contingent choice to rely
 *   on external capital and operational control (analytical perspective:
 *   false Mountain). The constraint is fundamentally a tangled rope: roads
 *   genuinely improve connectivity and enable regional trade (coordination
 *   function), but access to this benefit is conditional on accepting debt
 *   obligations, technology lock-in, and operational control by foreign
 *   actors (extraction function). The extraction has increased over the
 *   interval (extractiveness rising from 0.35 to 0.52) as the debt burden
 *   accumulates and policy constraints tighten, while the theater ratio has
 *   risen (from 0.42 to 0.58) as initial development enthusiasm masks growing
 *   concerns about sustainability.
 *
 * KEY AGENTS:
 *   - Japanese Government and Corporations: Primary beneficiary (institutional/arbitrage) — captures infrastructure contracts, trading corridor access, resource extraction benefits
 *   - Mongolian Rural Labor Force: Primary victim (powerless/trapped) — displaced from traditional livelihoods, face wage suppression, cannot exit region or pursue autonomous economic activity
 *   - Mongolian Central Government: Secondary beneficiary and captured party (organized/mobile) — benefits from infrastructure investment and resource access corridors, but accepts debt obligations and policy constraints that limit autonomy
 *   - Mongolian Regional Governments: Mixed actor (moderate/constrained) — gain road access and property value increases, but lose operational control and face constrained fiscal autonomy due to central government debt service
 *   - International Development Organizations: Oversight actor (organized/mobile) — frame constraint as temporary and solvable through conditions and capacity building; possess mobility to enforce sunset mechanisms but exercise weakly
 *   - Post-Soviet Development Paradigm: Institutional discourse (institutional/arbitrage) — naturalizes infrastructure-led dependency as universal development model; maintains through academic research, policy consensus, and institutional inertia despite mounting failures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, 0.52).
domain_priors:suppression_score(great_mongolian_road_economic_dependency, 0.65).
domain_priors:theater_ratio(great_mongolian_road_economic_dependency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(great_mongolian_road_economic_dependency, tangled_rope).
narrative_ontology:human_readable(great_mongolian_road_economic_dependency, "Economic Dependency via Infrastructure Development (Great Mongolian Road)").
narrative_ontology:topic_domain(great_mongolian_road_economic_dependency, "economic/development_infrastructure").

domain_priors:requires_active_enforcement(great_mongolian_road_economic_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, japanese_corporations).
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, mongolian_political_leadership).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_labor_market).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_sovereign_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONGOLIAN RURAL LABOR FORCE (SNARE) — Structurally trapped. Infrastructure development displaces traditional economic activity (herding, local trade) without creating equivalent employment. Workers cannot exit the labor-depressed region; capital flight is beyond their capacity. The constraint extracts through wage suppression and forced dependence on external employment sources controlled by foreign operators. Maximum suppression: no alternative livelihoods, no exit options, no voice in project design.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MONGOLIAN REGIONAL GOVERNMENTS (TANGLED ROPE) — Structurally mixed. Infrastructure investment does provide genuine coordination benefit: roads improve local trade access, reduce isolation, and increase property values. But access to the coordination benefit is conditional on compliance with Japanese operational control, technology procurement requirements, and debt-servicing obligations. Exit is constrained by the debt obligation structure and lack of alternative infrastructure capital. Beneficiary (roads improve connectivity) and victim (lose sovereign control over infrastructure decisions) simultaneously.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: JAPANESE GOVERNMENT AND CORPORATIONS (ROPE) — Experiences the constraint as pure coordination. Road projects solve legitimate international development challenges: improving Mongolia's connectivity increases market access for regional trade, benefits Japanese trading companies through new corridors, and generates contract revenue. The Japanese institutional actor has arbitrage options (invest elsewhere) and experiences the constraint as beneficial coordination with low extraction cost to them. Net beneficiary with high institutional optionality.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MONGOLIAN CENTRAL GOVERNMENT (TANGLED ROPE) — Complex dual position. As political leadership with organized power, benefits from infrastructure investment: roads improve tax base, enable resource extraction (mining corridors), and secure geopolitical positioning between China and Russia. But accepts extraction in the form of debt obligations, technology lock-in, and reduced policy autonomy. Central government has some exit options (renegotiate terms, seek alternative lenders) but exercises them weakly due to capital constraints. This perspective reveals institutional regulatory capture: the central government is both beneficiary and victim, with coordinated benefit structurally dependent on accepting extraction.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT ORGANIZATIONS (SCAFFOLD) — Sees the constraint as a temporary coordination failure with a sunset clause. Projects like Great Mongolian Road are justified as transitional infrastructure that builds capacity for Mongolia's eventual self-sufficient development. Once roads are complete and local expertise develops, the dependency structure (capital, technology, operational control) is supposed to transfer to Mongolian entities. High theater ratio (development mission rhetoric) masks extraction, but organized actors (World Bank, ADB) possess oversight authority and mobility to enforce conditions. Sunset mechanism is weak but present — projects have completion dates; transfer of ownership/control is nominally planned.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-SOVIET DEVELOPMENT PARADIGM (PITON) — Infrastructure-led development (roads, ports, telecommunications) is treated as universally beneficial, a folk theorem of development economics. The paradigm persists institutionally despite mounting evidence that infrastructure alone does not build autonomous economies: roads without complementary industrial policy, local capital formation, and institutional capacity primarily serve extraction (resource corridors for foreign firms, labor cost reduction). The institutional maintenance of this paradigm (World Bank endorsed, bilateral projects replicated across Central Asia) is largely performative — it appears to solve development problems while routinizing dependency. Theater is high because the narrative of 'development through infrastructure' is compelling and masks extraction mechanisms.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, landlocked geography imposes real constraints: Mongolia lacks sea access, faces transportation costs for any export, and requires external capital for infrastructure investment. The constraint of geography is immutable. However, the classification as mountain must pass the natural law gate: accessibility_collapse >= 0.85 (Mongolia CAN access sea via China, Russia, or multimodal routes) and resistance <= 0.15 (resistance to landlocked status is not minimal—alternative routes exist). The engine will classify this as a false summit: geography is a legitimate structural fact, but the dependency on Japanese capital and operational control is a contingent institutional choice, not a natural law.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(great_mongolian_road_economic_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(great_mongolian_road_economic_dependency, TR),
    TR >= 0.70.

:- end_tests(great_mongolian_road_economic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The initial extractiveness (0.35) reflects that roads do provide genuine coordination benefits: improved connectivity, reduced trade costs, property value increases. But extractiveness has risen to 0.52 over the interval as the debt burden accumulates. Debt service obligations increasingly constrain Mongolia's ability to invest in complementary development (education, industrial policy, local capital formation), shifting the net effect from mixed benefit toward net extraction. The rise in extractiveness reflects a classic debt-trap dynamic: initial benefits are real, but debt-servicing costs compound faster than development benefits accrue. Suppression (0.65): High. Multiple suppression mechanisms operate simultaneously: (1) Mongolian labor cannot easily exit regions affected by infrastructure displacement; (2) alternatives to Japanese capital and technology are limited due to geopolitical positioning and fiscal constraints; (3) policy constraints embedded in loan conditions reduce Mongolia's range of response options; (4) international development consensus treats infrastructure investment as obviously beneficial, suppressing critical evaluation. Theater ratio (0.58): Moderate-high and rising. The 'development' framing of infrastructure projects is performative—projects are presented as pure coordination mechanism ('mutual benefit,' 'regional integration') while the extraction mechanisms (debt terms, technology lock-in, operational control) are obscured in technical details. The rise in theater from 0.42 to 0.58 reflects increasing reliance on development narrative as debt burden becomes harder to justify. The fact that projects require constant rhetorical maintenance suggests low genuine function—true coordination mechanisms require less performative narrative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is extraordinarily wide, capturing the core tension between coordination and extraction narratives. Japanese actors see pure coordination (Rope): roads improve regional trade, both parties gain. Mongolian powerless actors see pure extraction (Snare): their livelihoods are displaced, they bear costs, and they have no exit. Mongolian organized leadership sees mixed benefit (Tangled Rope): roads improve strategic position and economic capacity, but at the cost of policy autonomy and debt service. International development organizations see a temporary problem being solved (Scaffold): infrastructure builds capacity; the sunset is the technology transfer and eventual Mongolian ownership. The post-Soviet paradigm sees a proven development model (Piton): infrastructure investment is an institutional script that persists despite weak empirical support. The analytical observer risks seeing an immutable constraint (Mountain): landlocked geography 'forces' infrastructure dependency. The core insight: the same infrastructure project appears as coordination or extraction depending entirely on the observer's structural position and exit options. Beneficiaries with arbitrage see coordination. Trapped agents see extraction. Organized agents see mixed strategies. The framework's job is to make this perspectival gap measurable and predictive—agents in snare positions will eventually resist; agents in rope positions will defend the arrangement; tangled_rope positions generate internal contradiction and potential coalition-forming.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position relative to the extraction and coordination flows. Japanese institutional actors are beneficiaries with arbitrage options—they can invest elsewhere if Mongolian terms become unfavorable. This yields low d (0.15–0.25 range), producing negative f(d) and low/negative χ from their perspective. Mongolian powerless labor are victims with trapped exit—they cannot relocate easily or form alternative economic structures. This yields high d (0.90+), producing f(d) ≈ 1.40 and high χ from their perspective. Mongolian central government occupies a complex position: beneficiary (receives roads, resource corridor access) but also victim (accepts debt and policy constraints). Organized power with mobile exit options means d ≈ 0.45–0.55, producing f(d) ≈ 0.65, consistent with the tangled_rope classification. Regional governments have moderate power and constrained exit, yielding d ≈ 0.60–0.65, also tangled_rope. The scaffold perspective (development organizations) has organized power, mobile exit, and sees beneficiaries (Mongolia eventually becomes self-sufficient), yielding low d despite the current extraction—but the sunset clause is the decisive feature, not the directionality value alone. The piton perspective treats the constraint as institutional inertia maintained through narrative, not through current extraction mechanics, so directionality becomes secondary to the theater gate (theater_ratio >= 0.70 is the gate; current value 0.58 is below threshold, so piton requires future assessment).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy resolution because extractiveness (0.52) > 0.46, crossing the threshold for detailed classification scrutiny. The mandatrophy is resolved by the tangled_rope classification: Mongolia genuinely receives coordination benefits (improved roads, reduced trade costs, property value increases, mining corridor access) AND genuinely bears extraction costs (debt service, policy constraints, technology lock-in, labor wage suppression). The constraint is not misclassified as pure coordination (Rope) nor as pure extraction (Snare)—it is authentically hybrid. The mandatrophy resolution requires: (1) explicit beneficiary declaration (japanese_corporations, mongolian_political_leadership) and victim declaration (mongolian_labor_market, mongolian_sovereign_autonomy), (2) active enforcement mechanism (loan conditionality, technology transfer conditions, debt obligations), (3) mixed power positioning (beneficiaries have arbitrage; victims have trapped or constrained exit). All three conditions are met. The resolution also requires confronting the false Mountain perspective: landlocked geography is real, but it is not what drives the dependency structure. Alternative responses to landlocked geography exist (e.g., Mongolia could pursue South-South development partnerships, regional infrastructure ownership consortiums, or domestic financing through resource revenue—all technically feasible but politically constrained). The dependency structure is a contingent institutional choice to rely on Japanese capital and external operational control, not a natural law. The false summit detection prevents naturalizing the extraction by framing it as geography.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_timing,
    'At what point does genuine technology and capacity transfer to Mongolian entities occur? Is the sunset clause enforced?',
    'Comparative analysis of infrastructure projects: (1) completion timelines vs operational control transfer timelines, (2) Mongolian ownership of road maintenance and toll revenue, (3) technical staff training and certification rates, (4) local procurement percentage growth over project lifecycle',
    'If transfer occurs on schedule: scaffold classification confirmed—temporary extraction with real sunset. If transfer is indefinitely delayed: constraint becomes permanent snare/tangled_rope—the development narrative masks chronic dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_timing, empirical, 'Whether technology transfer to Mongolian entities actually occurs per sunset clause').

omega_variable(
    local_employment_vs_displacement,
    'Do infrastructure projects create net local employment, or do they displace traditional livelihoods faster than new employment emerges?',
    'Labor market analysis: (1) employment created during construction vs permanent operational jobs, (2) wage rates for local workers vs Japanese expatriate workers, (3) traditional sector employment change in affected regions, (4) migration rates post-project completion',
    'If net employment is positive and wages are competitive: tangled_rope classification sustained—genuine coordination benefit alongside extraction. If displacement exceeds new jobs: snare classification from labor perspective becomes dominant—pure extraction masked by development rhetoric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_employment_vs_displacement, empirical, 'Whether infrastructure creates net local employment or displaces livelihoods').

omega_variable(
    debt_sustainability_threshold,
    'What debt-to-GDP ratio indicates the shift from beneficial infrastructure investment to extractive debt servicing that constrains policy autonomy?',
    'Fiscal analysis: (1) Mongolia''s debt burden from Great Mongolian Road and similar projects, (2) fiscal space available for education, health, local development after debt service, (3) correlation between debt levels and policy autonomy constraints, (4) comparison with other infrastructure-dependent economies (Laos, Sri Lanka, Kenya)',
    'If debt ratio remains below 60% and policy autonomy is maintained: tangled_rope (mixed benefit). If debt ratio exceeds 80% or drives austerity: permanent snare classification—infrastructure becomes an extraction mechanism rather than development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt sustainability threshold for infrastructure dependency').

omega_variable(
    corridor_utilization_vs_resource_extraction,
    'Do infrastructure corridors primarily serve bilateral regional trade (mutually beneficial coordination), or do they primarily serve resource extraction corridors (foreign extraction)?',
    'Trade flow analysis: (1) commodity composition of traffic on roads/corridors, (2) directionality of flows (raw materials out, finished goods in vs balanced trade), (3) value capture: tariff revenue to Mongolia vs value accruing to foreign operators, (4) competing corridor analysis—if multiple routes exist, do roads attract new trade or canibalize existing routes?',
    'If corridors attract genuine bilateral regional trade: rope/tangled_rope with authentic coordination benefit. If corridors primarily serve resource extraction (mining access): snare classification—infrastructure becomes an extraction tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corridor_utilization_vs_resource_extraction, empirical, 'Whether corridors serve bilateral trade or resource extraction').

omega_variable(
    institutional_autonomy_measurement,
    'How much does Mongolia''s policy autonomy degrade due to infrastructure debt obligations and technology lock-in? Are major policy decisions (resource policy, labor standards, environmental standards) constrained by lender conditions?',
    'Institutional analysis: (1) loan conditionality requirements and their scope, (2) policy changes that conflict with lender requirements and their frequency, (3) Mongolia''s negotiating power in subsequent rounds of infrastructure investment, (4) independence of regulatory decisions from lender signaling',
    'If autonomy degradation is minimal and Mongolia retains policy space: tangled_rope classification confirmed. If autonomy is substantially constrained: snare classification—the infrastructure becomes a mechanism for controlling policy, pure extraction of autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autonomy_measurement, empirical, 'Degree of policy autonomy loss due to infrastructure dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(great_mongolian_road_economic_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gmr_tr_t0, great_mongolian_road_economic_dependency, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gmr_tr_t5, great_mongolian_road_economic_dependency, theater_ratio, 5, 0.54).
narrative_ontology:measurement(gmr_tr_t10, great_mongolian_road_economic_dependency, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gmr_be_t0, great_mongolian_road_economic_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gmr_be_t5, great_mongolian_road_economic_dependency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gmr_be_t10, great_mongolian_road_economic_dependency, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(great_mongolian_road_economic_dependency, global_infrastructure).
narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, mongolian_resource_extraction_sovereignty).
narrative_ontology:affects_constraint(great_mongolian_road_economic_dependency, asian_infrastructure_debt_trap).

% DUAL FORMULATION NOTE:
% The Great Mongolian Road is decomposed from broader infrastructure-led development patterns. Upstream constraint (global_infrastructure_development_paradigm, epsilon=0.42) establishes institutional consensus treating infrastructure as universal development solution. This story (epsilon=0.52) instantiates that paradigm in Mongolia's specific case with higher extractiveness due to geopolitical constraints and debt-specific terms. Downstream constraints (resource_extraction_sovereignty, epsilon=0.68) model how infrastructure corridors enable resource extraction that further entrenches dependency. Each story has distinct epsilon reflecting different structural mechanisms: paradigm consensus vs project-specific extraction vs resource extraction coupling. Network links establish causal chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(great_mongolian_road_economic_dependency, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
