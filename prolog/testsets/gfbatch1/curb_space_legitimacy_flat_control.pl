% ============================================================================
% CONSTRAINT STORY: curb_space_legitimacy_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_curb_space_legitimacy_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: curb_space_legitimacy_flat_control
 *   human_readable: Municipal Curb Space as Public Resource Requiring Legitimate Allocation
 *   domain: urban_planning/public_resource_allocation/transportation_policy
 *
 * SUMMARY:
 *   The shared commitment that municipal curb space is a public resource
 *   requiring legitimate allocation rules emerged in the mid-20th century as
 *   car ownership expanded and street parking became contested. This
 *   constraint coordinates access to scarce curb space through permit
 *   systems, metered parking, time limits, and designated loading zones, but
 *   it also embeds extraction: permit fees and citation revenue fund
 *   municipal budgets, enforcement falls asymmetrically on transient users
 *   (delivery workers, ride-share drivers, visitors) who cannot obtain
 *   residential permits, and allocation rules ossify around incumbent uses
 *   (residential parking) even as transportation patterns shift toward
 *   multimodal access. The constraint exhibits perspectival variation: the
 *   municipal parking authority experiences it as coordination (allocating
 *   scarce space among competing uses), residential permit holders experience
 *   it as mixed coordination and extraction (guaranteed parking but with fees
 *   and zone restrictions), and delivery workers experience it as pure
 *   extraction (trapped by job requirements in a system that excludes their
 *   use case). The theater_ratio (0.38) reflects that enforcement in
 *   low-density areas where curb scarcity is minimal has become substantially
 *   performative — meter readers patrol to meet revenue targets rather than
 *   to solve coordination problems. The constraint's extractiveness and
 *   suppression have increased over the 30-year interval as permit systems
 *   expanded, citation rates rose, and enforcement intensified, even as
 *   actual curb scarcity in many neighborhoods remained stable or declined
 *   due to reduced car ownership among younger residents.
 *
 * KEY AGENTS:
 *   - Municipal Parking Authority: Primary beneficiary (institutional/arbitrage) — sets allocation rules, collects permit revenue and citation fees, experiences constraint as coordination
 *   - Residential Permit Holders: Mixed position (moderate/constrained) — benefit from guaranteed parking but pay fees and face zone restrictions; genuine coordination with embedded extraction
 *   - Commercial Loading Permit Holders: Mixed position (moderate/constrained) — benefit from designated loading zones but pay permit fees and face time restrictions
 *   - Delivery Workers: Primary victim (powerless/trapped) — excluded from permit systems, bear enforcement costs, cannot exit job requirements that force curb access
 *   - Ride-Share Drivers: Secondary victim (powerless/constrained) — face enforcement for stopping in no-parking zones, cannot obtain permits, experience system as extraction
 *   - Non-Permit Street Users: Secondary victim (moderate/constrained) — visitors, shoppers, service workers who face metered parking costs and time limits
 *   - Transportation Reform Coalition: Organized agents (organized/mobile) — transit agencies, bike advocates, micromobility companies building alternative curb uses with sunset logic
 *   - Parking Enforcement Division: Institutional actor (institutional/arbitrage) — maintains enforcement ritual that has become partly theatrical in low-scarcity areas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(curb_space_legitimacy_flat_control, 0.32).
domain_priors:suppression_score(curb_space_legitimacy_flat_control, 0.48).
domain_priors:theater_ratio(curb_space_legitimacy_flat_control, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(curb_space_legitimacy_flat_control, extractiveness, 0.32).
narrative_ontology:constraint_metric(curb_space_legitimacy_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(curb_space_legitimacy_flat_control, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(curb_space_legitimacy_flat_control, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(curb_space_legitimacy_flat_control, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(curb_space_legitimacy_flat_control, tangled_rope).
narrative_ontology:human_readable(curb_space_legitimacy_flat_control, "Municipal Curb Space as Public Resource Requiring Legitimate Allocation").
narrative_ontology:topic_domain(curb_space_legitimacy_flat_control, "urban_planning/public_resource_allocation/transportation_policy").

domain_priors:requires_active_enforcement(curb_space_legitimacy_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(curb_space_legitimacy_flat_control, '99c654b0-b726-4dc0-8237-ea69780363e2').
narrative_ontology:cs_kernel_codification('99c654b0-b726-4dc0-8237-ea69780363e2', formalized).
narrative_ontology:cs_authority_grounding('99c654b0-b726-4dc0-8237-ea69780363e2', practice).
narrative_ontology:cs_interpretation_layer_present('99c654b0-b726-4dc0-8237-ea69780363e2').
narrative_ontology:cs_created_at('99c654b0-b726-4dc0-8237-ea69780363e2', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(curb_space_legitimacy_flat_control, curb_space_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(curb_space_legitimacy_flat_control, municipal_parking_authority).
narrative_ontology:constraint_beneficiary(curb_space_legitimacy_flat_control, commercial_loading_permit_holders).
narrative_ontology:constraint_beneficiary(curb_space_legitimacy_flat_control, residential_permit_holders).
narrative_ontology:constraint_victim(curb_space_legitimacy_flat_control, non_permit_street_users).
narrative_ontology:constraint_victim(curb_space_legitimacy_flat_control, delivery_workers).
narrative_ontology:constraint_victim(curb_space_legitimacy_flat_control, ride_share_drivers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(curb_space_legitimacy_flat_control, parking_enforcement_division).
narrative_ontology:constraint_vindicates(curb_space_legitimacy_flat_control, public_resource_doctrine).
narrative_ontology:constraint_vindicates(curb_space_legitimacy_flat_control, municipal_allocation_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curb allocation rules, administers permit systems, collects revenue from permits and citations. Experiences the constraint as coordination — allocating scarce curb space among competing uses (residential parking, commercial loading, transit stops, bike lanes). Captures permit fees, meter income, and citation revenue. Can adjust rules and enforcement intensity. Low exit costs — the authority defines the system.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, municipal_parking_authority, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Pay annual permit fees to guarantee parking near home. Benefit from reduced competition for curb space in permit zones but face enforcement if permit lapses and cannot park outside home zone without paying meters. Exit options: move to non-permit area, give up car, pay for off-street parking. Constrained by housing costs and car dependency.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, residential_permit_holders, beneficiary,
    moderate, biographical, constrained, local).

% Pay commercial permit fees for designated loading zone access during business hours. Benefit from guaranteed loading space but face time restrictions and enforcement outside permitted hours. Exit options: relocate business, build private loading dock, absorb double-parking citations. Constrained by lease terms and capital costs.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, commercial_loading_permit_holders, beneficiary,
    moderate, biographical, constrained, local).

% Must access curb space to complete deliveries but are excluded from residential and commercial permit systems. Bear citation costs when no legal parking is available, lose time searching for spots, face employer discipline for late deliveries. Cannot refuse deliveries to permit zones, cannot obtain permits themselves. Trapped by job requirements and gig economy precarity.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, delivery_workers, payer,
    powerless, immediate, trapped, local).

% Must stop at curb to pick up and drop off passengers but face enforcement for stopping in no-parking zones, blocking bike lanes, or occupying metered spaces without payment. Bear citation costs, lose income during enforcement interactions. Cannot obtain permits for transient stops. Exit options: quit gig work, absorb citations as cost of business. Constrained by income needs and platform algorithms.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, ride_share_drivers, payer,
    powerless, immediate, constrained, local).

% Visitors, shoppers, service workers who need temporary curb access. Pay meter fees, face time limits, risk citations if meters expire. Exit options: use transit, park in commercial lots, avoid permit zones. Mobile but face costs and inconvenience.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, non_permit_street_users, payer,
    moderate, immediate, mobile, local).

% Transit agencies, bike coalitions, pedestrian advocates, micromobility companies building alternative curb uses. Advocate for reallocating curb space from private vehicle storage to transit lanes, bike infrastructure, parklets, and dynamic loading zones. See current permit systems as transitional — justified by the shift to multimodal transportation, not by steady-state car dominance. Organized across cities, can pilot alternatives, have political influence.
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, transportation_reform_coalition, observer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(curb_space_legitimacy_flat_control, transportation_reform_coalition, agenda_setter).

% Enforces permit zones, meters, and time limits. Employment and budget depend on citation revenue. In low-density areas where curb scarcity is minimal, enforcement has become substantially theatrical — meter readers patrol to meet revenue targets rather than to solve coordination problems. In high-density areas, enforcement remains functional. Dual role: sets enforcement priorities (agenda_setter) and benefits from citation revenue (beneficiary).
narrative_ontology:constraint_stakeholder(curb_space_legitimacy_flat_control, parking_enforcement_division, agenda_setter,
    institutional, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(curb_space_legitimacy_flat_control, parking_enforcement_division, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(curb_space_legitimacy_flat_control, municipal_parking_authority).
narrative_ontology:fixing_cost_class(curb_space_legitimacy_flat_control, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents tragedy of the commons in high-demand curb space by allocating access among competing uses: residential parking, commercial loading, transit stops, bike lanes, metered short-term parking. Without allocation rules, curb space would be claimed first-come-first-served, creating coordination failures (double-parking, blocked loading zones, residential streets clogged with commuter parking).
% TRANSFER_FUNCTION: Moves curb access from transient users (delivery workers, ride-share drivers, visitors) to permit holders (residents, businesses). Moves money from all street users to municipal parking authority via permit fees, meter payments, and citations. Moves enforcement costs onto powerless users who cannot obtain permits.
% ABSENT_VOICES: Delivery workers and gig economy drivers are largely absent from curb allocation policy-making. They bear enforcement costs and are excluded from permit systems, but they have no organized representation in municipal transportation planning. Also absent: future residents who will rely on transit and micromobility rather than private cars, whose curb space needs (bike parking, scooter corrals, bus lanes) are under-weighted in allocation decisions dominated by current car owners.
% DISAPPEARANCE_RATIONALE: If the legitimacy commitment disappeared overnight — if curb space reverted to unregulated first-come-first-served access — high-demand areas would experience immediate coordination failures: double-parking, blocked driveways, commercial loading zones claimed by commuters, residential streets clogged with all-day parkers. Low-demand areas would see little change. The world rearranges in proportion to actual curb scarcity, which varies widely by neighborhood density and transit access.
% FOUNDING_PROBLEM: Mid-20th century explosion in car ownership created curb space scarcity in urban areas. Before mass car ownership, street parking was unregulated because demand was low. As car ownership expanded, unregulated curb access created coordination failures: residential streets filled with commuter parking, commercial loading zones blocked, double-parking obstructed traffic. Municipal parking authorities were created to allocate curb space and prevent these coordination failures.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (curb scarcity from mass car ownership) is corroborated by transportation historians and urban planners outside the parking authority beneficiary set. However, the problem's current status is contested: transportation reform advocates argue that curb scarcity is now partly constructed by minimum parking requirements and car-centric zoning, and that the founding problem is dissolving as younger residents shift to transit and micromobility. Parking authorities and residential permit holders argue the problem remains live in high-density areas. The contest is over whether the problem is natural (density-driven) or policy-constructed (car-centric land use).
narrative_ontology:disappearance_verdict(curb_space_legitimacy_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(curb_space_legitimacy_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DELIVERY WORKER (SNARE) — Trapped by job requirements to access curb space but excluded from permit systems designed for residents and businesses. Bears enforcement costs (tickets, time searching for legal spots) while the coordination story (orderly allocation) does not include their use case. Maximum extraction with no exit — cannot refuse deliveries to permit zones, cannot obtain permits themselves.
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Benefits from loading zone access via commercial permits but pays permit fees, faces enforcement when customers cannot park, and bears costs of restricted residential parking that limits foot traffic. Genuine coordination (designated loading times) mixed with extraction (permit costs, enforcement asymmetry favoring large chains with dedicated loading infrastructure).
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: MUNICIPAL PARKING AUTHORITY (ROPE) — Primary beneficiary. Collects permit revenue, parking meter income, and citation fees. Experiences the constraint as coordination: allocating scarce curb space among competing uses (residential parking, commercial loading, transit stops, bike lanes, outdoor dining). Low effective extraction because the authority sets the rules and captures the revenue stream.
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RESIDENTIAL PERMIT HOLDER (TANGLED ROPE) — Benefits from guaranteed parking near home via permit system but pays annual fees, faces enforcement if permit lapses, and experiences reduced mobility (permit valid only in home zone). Genuine coordination (neighbors share scarce street parking) mixed with extraction (fees, zone restrictions, enforcement theater when actual scarcity is low).
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: TRANSPORTATION REFORM COALITION (SCAFFOLD) — Organized advocates (transit agencies, bike coalitions, pedestrian groups, micromobility companies) see curb allocation as a temporary coordination problem with a sunset: as cities shift from car-centric to multimodal transportation, curb space will be reallocated from private vehicle storage to transit, bike lanes, parklets, and dynamic loading zones. The current permit system is transitional infrastructure justified by the shift, not by steady-state car dominance.
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PARKING ENFORCEMENT DIVISION (PITON) — Enforcement of residential permit zones and time-limited commercial loading has become substantially theatrical in low-density areas where actual curb scarcity is minimal. Meter readers patrol and cite violations to meet revenue targets rather than to solve coordination problems. The enforcement ritual persists through institutional inertia and budget dependence, not because it solves the allocation problem it was designed for.
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The commitment that curb space is a public resource requiring allocation rules solves a genuine coordination problem (preventing tragedy of the commons in high-demand areas) but embeds asymmetric extraction (permit systems favor long-term residents over transient users, enforcement revenue creates perverse incentives, allocation rules ossify around incumbent uses). The constraint is not a natural law — curb scarcity is partly constructed by minimum parking requirements and car-centric zoning — but it is also not pure extraction. Tangled rope at the analytical level.
constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(curb_space_legitimacy_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(curb_space_legitimacy_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(curb_space_legitimacy_flat_control, TR),
    TR >= 0.70.

:- end_tests(curb_space_legitimacy_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint solves a genuine coordination problem in high-demand areas (preventing curb space tragedy of the commons) but embeds real extraction: permit fees and citation revenue create perverse incentives, enforcement falls asymmetrically on transient users who cannot obtain permits, and allocation rules favor incumbent uses over emerging transportation patterns. The extraction is not as severe as pure rent-seeking (no monopoly control, alternatives exist via transit or off-street parking) but it is substantial and rising. Suppression (0.48): Moderate-high. Significant barriers to non-compliance include citation costs, towing risk, and job requirements that force curb access (delivery workers cannot refuse to enter permit zones). But suppression is not total — users can choose off-street parking, transit, or relocation to areas with lower enforcement. The suppression has increased over the interval as enforcement intensified and citation rates rose. Theater ratio (0.38): Moderate. Enforcement in low-density areas where actual curb scarcity is minimal has become substantially performative — meter readers patrol to meet revenue targets rather than to solve coordination problems. But enforcement in high-density areas remains functional (prevents double-parking, keeps loading zones clear for commercial access). The theater has increased as permit systems expanded into lower-density neighborhoods. Accessibility collapse (0.55): Moderate. Once the commitment that curb space requires allocation rules is accepted, some alternatives collapse (free-for-all parking creates coordination failures) but others remain viable (off-street parking, transit, bike/walk, relocation). The collapse is partial, not total. Resistance (0.42): Moderate. The constraint meets real resistance from delivery workers, ride-share drivers, and transportation reform advocates who contest the allocation rules, but it also has broad acceptance among residential permit holders and municipal authorities who benefit from it.
 *
 * PERSPECTIVAL GAP:
 *   The municipal parking authority sees coordination (Rope) — allocating scarce curb space among competing uses is a genuine public resource management problem. Residential permit holders see mixed coordination and extraction (Tangled Rope) — the system solves the neighbor parking problem but extracts fees and restricts mobility. Delivery workers see pure extraction (Snare) — they are trapped by job requirements in a system that excludes their use case and enforces against them. The transportation reform coalition sees a temporary problem with a sunset (Scaffold) — as cities shift to multimodal transportation, curb space will be reallocated away from private vehicle storage. The parking enforcement division sees its own degraded ritual (Piton) — enforcement in low-scarcity areas is performative, maintained for revenue rather than coordination. The analytical observer sees tangled rope — genuine coordination mixed with asymmetric extraction, neither pure coordination nor pure rent-seeking. The perspectival gap is wide: the same allocation rules appear as legitimate public resource management, necessary but costly coordination, extractive exclusion, transitional infrastructure, or performative ritual depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The municipal parking authority is the primary beneficiary — it sets the allocation rules, collects permit revenue and citation fees, and experiences the constraint as coordination rather than extraction. Directionality for this agent is low (near 0.2), producing low or negative effective extraction. Residential and commercial permit holders are mixed — they benefit from guaranteed curb access but pay fees and face restrictions. Directionality for these agents is moderate (around 0.4-0.5), producing moderate effective extraction. Delivery workers and ride-share drivers are primary victims — they are excluded from permit systems, bear enforcement costs, and cannot exit job requirements that force curb access. Directionality for these agents is high (near 0.8-0.9), producing high effective extraction. The transportation reform coalition has organized power and mobile exit options — they are building alternative curb uses and see the current system as transitional. Directionality for this agent is low-moderate (around 0.3), producing low effective extraction. The parking enforcement division is an institutional actor with arbitrage exit — it maintains the enforcement ritual and captures employment and budget benefits. Directionality for this agent is low (near 0.2).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the coordination function (preventing curb space tragedy of the commons) and the extraction function (permit fees, citation revenue, enforcement asymmetry) are genuinely intertwined rather than separable. The system is not mislabeled pure extraction (it does solve a real coordination problem in high-demand areas) and it is not mislabeled pure coordination (it does extract asymmetrically from transient users and create perverse revenue incentives). The tangled rope classification at the analytical level captures this irreducible mixture. The constraint also demonstrates how a coordination mechanism can degrade toward piton (enforcement becomes theatrical in low-scarcity areas) and how organized agents can build scaffold alternatives (dynamic pricing, multimodal curb allocation) that route around the extraction while preserving the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_construction_vs_natural,
    'Is curb scarcity a natural feature of urban density or a constructed outcome of car-centric land use policy?',
    'Cross-city comparison of curb demand in cities with different minimum parking requirements, transit investment levels, and land use density. Historical analysis of curb scarcity before and after parking mandates.',
    'If constructed: the legitimacy claim naturalizes a policy choice, and the constraint is closer to snare from more perspectives. If natural: the coordination function is genuine across all density levels, and rope/scaffold perspectives are vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_construction_vs_natural, empirical, 'Whether curb scarcity is natural or policy-constructed').

omega_variable(
    permit_system_equity,
    'Do residential permit systems allocate curb space equitably or do they entrench incumbent advantage at the expense of transient users?',
    'Demographic analysis of permit holders vs non-permit street users; income and tenure correlation with permit access; measurement of enforcement burden on delivery workers, ride-share drivers, and visitors.',
    'If equitable: the tangled rope classification understates the coordination function. If inequitable: the snare classification from powerless perspectives is vindicated, and the system is more extractive than moderate perspectives perceive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permit_system_equity, empirical, 'Equity of permit system allocation').

omega_variable(
    enforcement_revenue_dependence,
    'Is parking enforcement primarily a coordination mechanism or a municipal revenue source?',
    'Budget analysis: proportion of municipal revenue from parking citations and permits; correlation between citation rates and budget shortfalls; comparison of enforcement intensity in high-scarcity vs low-scarcity areas.',
    'If revenue-driven: the piton perspective is vindicated and theater_ratio should be higher. If coordination-driven: enforcement is functional rather than performative, and the rope/tangled_rope perspectives are more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_revenue_dependence, empirical, 'Whether enforcement is coordination or revenue extraction').

omega_variable(
    dynamic_pricing_alternative,
    'Would dynamic curb pricing (demand-responsive rates) solve the coordination problem with lower extraction than permit systems?',
    'Pilot program comparison: cities with dynamic pricing (SF, LA) vs traditional permit systems; measurement of curb utilization efficiency, enforcement costs, and equity impacts across user groups.',
    'If dynamic pricing is superior: the scaffold perspective is vindicated — current permit systems are transitional. If permit systems are superior: the coordination function is more robust than reform advocates claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_pricing_alternative, empirical, 'Comparative efficiency of dynamic pricing vs permits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(curb_space_legitimacy_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curb_theater_1990, curb_space_legitimacy_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(curb_theater_2000, curb_space_legitimacy_flat_control, theater_ratio, 10, 0.3).
narrative_ontology:measurement(curb_theater_2010, curb_space_legitimacy_flat_control, theater_ratio, 20, 0.35).
narrative_ontology:measurement(curb_theater_2020, curb_space_legitimacy_flat_control, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(curb_extract_1990, curb_space_legitimacy_flat_control, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(curb_extract_2000, curb_space_legitimacy_flat_control, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(curb_extract_2010, curb_space_legitimacy_flat_control, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(curb_extract_2020, curb_space_legitimacy_flat_control, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(curb_suppress_1990, curb_space_legitimacy_flat_control, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(curb_suppress_2000, curb_space_legitimacy_flat_control, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(curb_suppress_2010, curb_space_legitimacy_flat_control, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(curb_suppress_2020, curb_space_legitimacy_flat_control, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(curb_space_legitimacy_flat_control, resource_allocation).
narrative_ontology:affects_constraint(curb_space_legitimacy_flat_control, minimum_parking_requirements).
narrative_ontology:affects_constraint(curb_space_legitimacy_flat_control, transit_priority_lanes).
narrative_ontology:affects_constraint(curb_space_legitimacy_flat_control, bike_lane_allocation).

% DUAL FORMULATION NOTE:
% The curb space legitimacy constraint is upstream of specific allocation mechanisms (residential permits, metered parking, loading zones) but represents a distinct structural commitment. The downstream constraints have their own extractiveness values reflecting the specific rules and enforcement patterns; the legitimacy constraint has its own extractiveness reflecting the shared commitment that curb space is a public resource requiring municipal allocation authority rather than first-come-first-served or market-based access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
