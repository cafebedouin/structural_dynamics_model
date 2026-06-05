% ============================================================================
% CONSTRAINT STORY: sotu_1949_truman_inflation_control_authorities
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1949_truman_inflation_control_authorities, []).

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
 *   constraint_id: sotu_1949_truman_inflation_control_authorities
 *   human_readable: Truman's 1949 Inflation Control Authorities
 *   domain: regulatory/economic_policy
 *
 * SUMMARY:
 *   President Truman's 1949 State of the Union proposal for expanded
 *   inflation control authorities represents a consolidation of emergency
 *   economic powers in executive and congressional hands to intervene at
 *   'critical points' in the economy. The constraint exhibits genuine
 *   coordination functions (stabilizing essential prices for consumers,
 *   preventing commodity speculation, ensuring production continuity)
 *   alongside asymmetric extraction mechanisms (wage control that prevents
 *   workers from capturing productivity gains, material allocations that
 *   constrain business autonomy, credit controls that subordinate financial
 *   institutions to government priorities). The theater ratio reflects that
 *   implementation relies on voluntary business compliance and periodic OPA
 *   monitoring rather than continuous enforcement, creating a gap between the
 *   regulatory facade and actual capacity. The constraint's extractiveness
 *   increased over the proposed interval (0.42 to 0.58) as ongoing inflation
 *   despite peacetime production expansion forced authorities to expand
 *   coverage and enforcement intensity.
 *
 * KEY AGENTS:
 *   - Workers Seeking Wage Increases: Primary victims (powerless/trapped) — wage adjustment limits prevent capturing inflation-adjusted productivity gains; no exit from labor markets
 *   - Regulated Businesses: Primary victims (powerless/trapped) — material allocations, transportation priorities, price ceilings constrain production and profit; sunk capital prevents exit
 *   - Consumers and Renters: Primary beneficiaries (institutional/arbitrage) — rent controls, price ceilings, commodity speculation limits stabilize essential expenses; benefit from supply predictability
 *   - Government Authorities (Executive, Congress): Secondary beneficiary (institutional/arbitrage) — consolidate emergency powers, project crisis management competence, can trigger/suspend authorities
 *   - Labor Union Leadership: Tertiary actor (organized/constrained) — experience mixed coordination (wage stabilization prevents destructive competition) and extraction (negotiating power legislatively capped)
 *   - Price Control Administration (OPA and successor agencies): Institutional actor (institutional/arbitrage) — implement controls through performative monitoring; benefit from budget and authority expansion
 *   - Agricultural Producers and Commodity Handlers: Mixed actor (moderate/constrained) — benefit from price stabilization but constrained by export controls and allocations
 *   - Inflation Skeptics and Market Advocates: Counter-coalition (organized/constrained) — advocate for decontrol; constrained by political processes; see authorities as temporary scaffolding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1949_truman_inflation_control_authorities, 0.58).
domain_priors:suppression_score(sotu_1949_truman_inflation_control_authorities, 0.62).
domain_priors:theater_ratio(sotu_1949_truman_inflation_control_authorities, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1949_truman_inflation_control_authorities, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1949_truman_inflation_control_authorities, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1949_truman_inflation_control_authorities, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1949_truman_inflation_control_authorities, tangled_rope).
narrative_ontology:human_readable(sotu_1949_truman_inflation_control_authorities, "Truman's 1949 Inflation Control Authorities").
narrative_ontology:topic_domain(sotu_1949_truman_inflation_control_authorities, "regulatory/economic_policy").

domain_priors:requires_active_enforcement(sotu_1949_truman_inflation_control_authorities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1949_truman_inflation_control_authorities, consumers_of_essential_goods).
narrative_ontology:constraint_beneficiary(sotu_1949_truman_inflation_control_authorities, renters).
narrative_ontology:constraint_beneficiary(sotu_1949_truman_inflation_control_authorities, stabilized_production_sectors).
narrative_ontology:constraint_victim(sotu_1949_truman_inflation_control_authorities, workers_seeking_wage_increases).
narrative_ontology:constraint_victim(sotu_1949_truman_inflation_control_authorities, businesses_in_regulated_sectors).
narrative_ontology:constraint_victim(sotu_1949_truman_inflation_control_authorities, commodity_speculators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED WORKER (SNARE) — Workers subject to wage adjustment limits cannot exit labor markets or negotiate freely. Suppression is structural: legal prohibition on wage increases, no collective leverage, inflation erodes real wages while controls persist. Extraction is maximal from this agent's position — bearable costs with minimal coordination benefit.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATED BUSINESS (SNARE) — Small and medium producers subject to material allocation, transportation priority, and price ceiling controls cannot redirect resources or exit regulated sectors. Exit barriers include sunk capital, supplier dependencies, market licensing. Extraction: direct government seizure of allocation and pricing authority.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSUMER PROTECTION COALITION (ROPE) — Consumers and housing advocates benefit directly from rent controls, price ceilings on essential goods, and commodity speculation limits. These groups experience the constraint as pure coordination: stabilizing essential prices enables predictable household budgeting. Arbitrage exit: groups can adapt budget allocation to official price structures; minimal extraction experienced.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION LEADERSHIP (TANGLED ROPE) — Unions experience both coordination and extraction. Coordination benefit: wage controls prevent ruinous competition and stabilize negotiated rates across sectors. Extraction cost: union negotiating power is legislatively capped; leadership must enforce wage discipline on membership or face government intervention. Constrained exit: unions could strike (costly, risky) but cannot freely negotiate wages. Active enforcement required: government monitors compliance and unions police membership.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE AND CONGRESSIONAL LEADERSHIP (ROPE) — Government authorities consolidate emergency economic powers and experience these as coordination mechanisms: credit controls prevent speculative runs, material allocations ensure production continuity, export controls preserve domestic supplies, transportation priorities direct resources to critical sectors. Arbitrage exit: leadership can trigger or suspend emergency authority; minimal experienced extraction. Benefits from public perception of crisis management.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PRICE CONTROL ADMINISTRATION (PITON) — The Office of Price Administration (OPA) and related agencies implement controls through performative compliance monitoring and reporting rather than continuous enforcement. Theater ratio (0.54) reflects that price surveillance relies on voluntary business reporting and spot-check verification; actual enforcement coverage is limited. The apparatus persists through institutional inertia (World War II legacy) and political commitment, not through functional necessity at the proposed scale.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: AGRICULTURAL PRODUCERS (TANGLED ROPE) — Commodity speculation limits prevent runaway food prices, which benefits agricultural sectors through stabilized input and output markets. But export controls and material allocations constrain production choices and revenue. Constrained exit: producers must comply or lose market access; cartel-like coordination among farmers to maintain price support requires government backstopping. Active enforcement: USDA monitors allocation compliance.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: INFLATION SKEPTICS (SCAFFOLD) — Economists and business advocates who reject the inflation emergency and advocate for price decontrol see these authorities as temporary scaffolding: the constraint has an implied sunset (when inflation moderates, authorities expire). Constrained exit: advocates must work within political processes to argue for decontrol; no direct exit mechanism. Theater: arguments over whether inflation is real or policy-induced occupy much political space, separate from price control mechanics.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, some economic controls are inherent to complex monetary systems: credit creation always requires some constraint, allocation of scarce resources always involves priority-setting, price stability always requires some intervention. This perspective sees Truman's authorities as discovering natural laws of economic management rather than constructing policy. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to all complex economies' framing naturalizes what is actually a contingent postwar policy choice.
constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1949_truman_inflation_control_authorities_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1949_truman_inflation_control_authorities, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1949_truman_inflation_control_authorities, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1949_truman_inflation_control_authorities, TR),
    TR >= 0.70.

:- end_tests(sotu_1949_truman_inflation_control_authorities_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint consolidates power over critical economic functions (credit, commodities, wages, materials, transportation, rents, prices) in government hands. The extraction is not maximal (0.66+) because the coordination benefits are genuine and significant — stabilized food prices, protected housing costs, and controlled speculation have real welfare effects for constrained populations. But the asymmetry is substantial: workers and regulated businesses bear concentrated costs while benefits disperse across consumer populations and government authority. The upward trajectory (0.42→0.58) reflects the escalating enforcement intensity as inflation persisted despite peacetime production. Suppression (0.62): Moderately high. Structural barriers to exit include legal prohibition on wage negotiation above ceilings, capital immobility in regulated sectors, and credit control constraints that limit financial flexibility. But suppression is not maximal because some exit routes exist: workers can reduce labor effort or seek unregulated sectors; businesses can lobby for allocation increases or negotiate with agencies; consumers can accept rationing or black market purchases. Theater ratio (0.54): Moderate. Price controls and credit monitoring rely substantially on voluntary compliance and periodic auditing rather than continuous surveillance, creating a gap between the regulatory facade and actual enforcement capacity. The theater increased over the interval as the scale of controls expanded — more controls require more reporting, creating more opportunities for performative compliance. Claimed type: Tangled Rope reflects the genuine coordination function (price stabilization benefits) combined with asymmetric extraction (wage suppression, business constraint) and the requirement for active government enforcement to maintain the coordination against agents' incentives to exit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. Workers experience pure Snare — concentrated extraction with no coordination benefit for them personally. Consumers experience pure Rope — price stabilization solves their coordination problem without perceived extraction. Regulated businesses experience Snare with trapped exit. Government authorities experience Rope with arbitrage exit. Labor unions experience Tangled Rope with their dual position as both beneficiaries (wage stabilization) and victims (negotiating power capped). The OPA apparatus experiences Piton — performative compliance monitoring sustains the control structure through theater rather than functional necessity. Inflation skeptics experience Scaffold — viewing authorities as temporary emergency measures that will sunset when inflation moderates. The analytical observer at civilizational scale risks Mountain — naturalizing postwar policy choices as inherent to complex economies. The perspectival gap reveals that 'inflation control authorities' is not a single constraint but a bundle of distinct extraction/coordination mechanisms that affect different agents asymmetrically. No single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Workers and regulated businesses are victims (d ≈ 0.85-0.95) experiencing high extraction and suppression with minimal coordination benefit. Consumers and government authorities are beneficiaries (d ≈ 0.10-0.20) experiencing coordination and power gain with minimal extraction. Union leadership occupies a hybrid position (d ≈ 0.50) — they are nominally beneficiaries of wage stabilization but also victims of capped negotiating power. The analytical observer at universal/civilizational scope risks d ≈ 0.65 (treating the constraint as a natural law of economic management), but the structural data reveals this as a false summit: the directionality values derive from specific postwar political choices, not from economic necessity. The scaffold perspective (inflation skeptics) has constrained exit (d ≈ 0.55) — they can advocate for decontrol but cannot directly exit the regulated economy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the Tangled Rope classification holds the center of analytical gravity. The coordination function is genuine and substantial (price stabilization for essential goods is a real public good problem). The extraction is equally genuine and substantial (wage controls and business constraints impose concentrated costs on identified victims). The constraint requires active enforcement (government must continuously monitor compliance and adjust allocations) because agents' incentives are misaligned — workers want wage increases above ceilings, businesses want production autonomy above allocations, commodity traders want price discovery above ceilings. The true mandatrophy question is not 'is this really coordination or really extraction?' but 'at what ratio do these combine, and for how long can the ratio be sustained?' The rising theater ratio (0.38→0.54) suggests that as enforcement intensity increases, more of the constraint's power comes from performative compliance and regulatory theater than from agents' genuine preference alignment. If theater continues rising above 0.70, the classification will drift toward Piton — the coordination function would persist through institutional inertia rather than functional necessity, suggesting the constraint has become a zombie institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_reality_vs_perception,
    'Is the 1949 inflation emergency an objective monetary/supply problem or a perceptual/political construction by price control advocates?',
    'Historical analysis of actual inflation rates, money supply growth, and commodity price movements 1945-1950; comparison with contemporaneous academic and policy disagreement about inflation severity',
    'If objective crisis: constraint is genuine coordination response to real problem (Rope from beneficiary perspective). If perceptual construction: constraint is opportunistic power consolidation (Snare from worker/business perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_reality_vs_perception, empirical, 'Whether postwar inflation was objective crisis or political construction').

omega_variable(
    wage_control_necessity,
    'Are direct wage adjustment limits necessary to control inflation, or do they primarily extract rents from workers while addressing credit/commodity problems separately?',
    'Econometric analysis of wage-price transmission in periods with/without wage controls; comparison of inflation control effectiveness across countries with different wage policy approaches',
    'If necessary: Tangled Rope classification holds — wage controls are part of integrated coordination. If unnecessary: wage controls are pure extraction (Snare component rises relative to coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_control_necessity, empirical, 'Whether wage controls are necessary for inflation management').

omega_variable(
    executive_power_perpetuation,
    'Are emergency economic authorities genuinely temporary scaffolding with self-liquidating logic, or do they create institutional constituencies (OPA, price administrators, regulatory agencies) with incentives to perpetuate emergency framing?',
    'Historical tracking of when authorities were actually suspended; analysis of OPA closure politics 1946-1947; examination of successor agencies and regulatory institutions that inherited emergency authorities',
    'If truly temporary: Scaffold classification confirmed. If perpetuated: constraint slides toward Tangled Rope or Snare as theater increases and beneficiary-victim gap widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_power_perpetuation, empirical, 'Whether emergency authorities are genuinely temporary or perpetuated through institutional interests').

omega_variable(
    black_market_displacement,
    'Do price controls displace supply into black markets and informal economy, making actual inflation worse for those relying on formal markets?',
    'Historical documentation of black market activity 1946-1950; price comparison (official vs underground markets) for controlled goods; supply availability before and after controls',
    'If displacement significant: constraint increases suppression by forcing formal-market reliance on constrained suppliers, increasing effective extraction. If minimal: theater-ratio concern is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_displacement, empirical, 'Whether price controls displace supply to black markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1949_truman_inflation_control_authorities, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu49_tr_t0, sotu_1949_truman_inflation_control_authorities, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sotu49_tr_t2, sotu_1949_truman_inflation_control_authorities, theater_ratio, 2, 0.48).
narrative_ontology:measurement(sotu49_tr_t4, sotu_1949_truman_inflation_control_authorities, theater_ratio, 4, 0.54).

% Extraction over time
narrative_ontology:measurement(sotu49_be_t0, sotu_1949_truman_inflation_control_authorities, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu49_be_t2, sotu_1949_truman_inflation_control_authorities, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(sotu49_be_t4, sotu_1949_truman_inflation_control_authorities, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1949_truman_inflation_control_authorities, resource_allocation).
narrative_ontology:affects_constraint(sotu_1949_truman_inflation_control_authorities, stagflation_wage_price_spiral_1970s).
narrative_ontology:affects_constraint(sotu_1949_truman_inflation_control_authorities, bretton_woods_external_balance_constraint).
narrative_ontology:affects_constraint(sotu_1949_truman_inflation_control_authorities, post_war_labor_management_accord).

% DUAL FORMULATION NOTE:
% The Truman inflation control authorities represent a specific institutional solution to the general problem of inflation management under full employment. Downstream constraints (stagflation dynamics, Bretton Woods balance-of-payments pressures) inherit structural features of this control framework: the wage-price coupling established by these authorities, the expectation that government can/should intervene at economic 'critical points,' and the accumulated institutional apparatus (regulatory agencies, monitoring infrastructure) created to enforce controls. The upstream constraint is bretton_woods_external_balance_constraint (fixed exchange rates create pressure for domestic price stability, motivating the control authorities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1949_truman_inflation_control_authorities, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
