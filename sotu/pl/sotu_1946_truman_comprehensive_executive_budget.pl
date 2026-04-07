% ============================================================================
% CONSTRAINT STORY: sotu_1946_truman_comprehensive_executive_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1946_truman_comprehensive_executive_budget, []).

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
 *   constraint_id: sotu_1946_truman_comprehensive_executive_budget
 *   human_readable: Comprehensive Executive Budget System (Truman Administration, 1946+)
 *   domain: governance/budget_administration
 *
 * SUMMARY:
 *   The comprehensive executive budget system, institutionalized under
 *   President Truman in 1946, created a structural requirement that all
 *   departmental and agency financial programs be consolidated into a single,
 *   unified budget submission to Congress. This innovation solved a real
 *   coordination problem: prior to 1946, departments submitted separate
 *   budget requests directly to Congressional committees, creating
 *   fragmentation, duplication, and inability to optimize resource allocation
 *   across government. The unified submission enabled the executive branch to
 *   coordinate priorities, eliminate redundancies, and present a coherent
 *   strategic vision. However, the system simultaneously created an
 *   extraction mechanism: by centralizing budget coordination at the
 *   presidential level, it reduced departmental autonomy in priority-setting,
 *   restricted Congressional legislative initiative on individual programs,
 *   and created suppression barriers for agencies seeking to pursue missions
 *   that did not align with presidential priorities. The constraint exhibits
 *   the classic tangled-rope signature: genuine coordination function
 *   (improved government-wide planning) layered with asymmetric extraction
 *   (presidential authority over departmental resources).
 *
 * KEY AGENTS:
 *   - Presidential Administration: Primary beneficiary (institutional/arbitrage) — consolidates budgetary authority and enables efficient resource allocation; can structure all departmental requests according to strategic vision
 *   - Departmental/Agency Directors: Primary victims (powerless/trapped and organized/constrained) — lose autonomous channels to Congress; must operate within presidentially-approved budget framework; cannot advocate directly for mission-specific priorities
 *   - Congressional Appropriations Committees: Secondary beneficiary and victim (organized/constrained) — benefit from comprehensive oversight capability; constrained by presidential pre-structuring of budget alternatives
 *   - Budget Bureau / OMB: Institutional implementer (institutional/arbitrage) — maintains coordination machinery; experiences constraint as pure coordination function from immediate perspective
 *   - Individual Congressional Representatives: Tertiary actors (powerful/mobile) — lose traditional pork-barrel authority but retain amendment capacity; experience constraint as coordination problem to solve through committee work
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — views constraint as institutional innovation solving coordination problem while creating extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1946_truman_comprehensive_executive_budget, 0.28).
domain_priors:suppression_score(sotu_1946_truman_comprehensive_executive_budget, 0.32).
domain_priors:theater_ratio(sotu_1946_truman_comprehensive_executive_budget, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1946_truman_comprehensive_executive_budget, extractiveness, 0.28).
narrative_ontology:constraint_metric(sotu_1946_truman_comprehensive_executive_budget, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(sotu_1946_truman_comprehensive_executive_budget, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1946_truman_comprehensive_executive_budget, tangled_rope).
narrative_ontology:human_readable(sotu_1946_truman_comprehensive_executive_budget, "Comprehensive Executive Budget System (Truman Administration, 1946+)").
narrative_ontology:topic_domain(sotu_1946_truman_comprehensive_executive_budget, "governance/budget_administration").

domain_priors:requires_active_enforcement(sotu_1946_truman_comprehensive_executive_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1946_truman_comprehensive_executive_budget, executive_branch_leadership).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_comprehensive_executive_budget, congressional_appropriations_committees).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_comprehensive_executive_budget, budget_coordination_specialists).
narrative_ontology:constraint_victim(sotu_1946_truman_comprehensive_executive_budget, departmental_autonomy).
narrative_ontology:constraint_victim(sotu_1946_truman_comprehensive_executive_budget, legislative_branch_individual_initiatives).
narrative_ontology:constraint_victim(sotu_1946_truman_comprehensive_executive_budget, specialized_agency_missions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED AGENCY DIRECTOR (SNARE) — A department head cannot escape the unified budget requirement; they must submit program requests through the presidential coordination apparatus or face budget rejection. No alternative channel exists for direct Congressional appeal. Suppression is high: the agency director has lost the historical ability to advocate directly to Congress for their mission. The constraint appears as pure extraction — the president's unified authority extracts compliance from agencies in exchange for minimal coordination benefit to their specific operations.
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPARTMENTAL SECRETARY (TANGLED ROPE) — Cabinet-level executives are constrained by the unified budget process but also benefit from guaranteed review and priority-setting. They have organizational resources and some ability to negotiate within the executive coordination process, but exit is costly (defying the President carries political and career consequences). The constraint has both coordination function (ensures all departments are reviewed at executive level) and extraction (the President consolidates budgetary authority, reducing departmental independence).
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRESIDENTIAL ADMINISTRATION (ROPE) — The President experiences the comprehensive budget requirement as pure coordination: consolidating all departments into a single submission solves the executive coordination problem, enabling efficient resource allocation and strategic planning. The President has full arbitrage capacity — they can override, reorganize, or reallocate budgets across departments at will. The constraint is coordination mechanism from this perspective, with minimal suppression or extraction cost.
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL APPROPRIATIONS COMMITTEES (TANGLED ROPE) — Committees benefit from the unified budget (single submission enables comprehensive oversight rather than piecemeal consideration), but are constrained by the President's power to structure priorities before reaching Congress. The committees cannot originate separate departmental budgets; they must work within the executive-proposed framework. Exit option is constrained: Congress could theoretically reject the entire unified budget, but this creates chaos and political cost. The constraint has coordination function (comprehensive oversight) and extraction (executive pre-structuring limits legislative alternatives).
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INDIVIDUAL CONGRESSIONAL REPRESENTATIVES (ROPE) — Individual members have mobile exit options (they can challenge the unified budget, propose amendments, or shift party alignment). The comprehensive budget constraint does reduce their traditional pork-barrel authority (individual earmarks are now subject to departmental and presidential review), but this is experienced as a coordination problem they solve through committee membership and caucusing. The constraint is coordination rather than extraction from this perspective — members collectively benefit from the oversight structure.
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: BUDGET BUREAU / OMB APPARATUS (PITON) — The institutional machinery that implements the comprehensive budget requirement has become substantially performative over time. The Bureau of the Budget (later OMB) maintains extensive review processes, forms, and coordination rituals that are often theater: departments submit requests; OMB reviews them according to presidential priorities; the President approves a unified submission. The theater persists because it works at coordination — it structures the conversation. But the mechanism's original function (discovering optimal allocation) has degraded into ritual enforcement of pre-established priorities. The apparatus persists through institutional inertia.
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the comprehensive executive budget is a genuine institutional innovation that solves a real coordination problem (how to coordinate budget requests across dozens of competing departments and agencies) while simultaneously creating an extraction mechanism (presidential centralization of budgetary authority at the expense of departmental autonomy and Congressional legislative initiative). The constraint is structurally hybrid: coordination function is real; extraction mechanism is real. Neither dominates completely, making the tangled rope classification appropriate.
constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1946_truman_comprehensive_executive_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1946_truman_comprehensive_executive_budget, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1946_truman_comprehensive_executive_budget, TR),
    TR >= 0.70.

:- end_tests(sotu_1946_truman_comprehensive_executive_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The unified budget system extracts presidential authority from departmental autonomy and legislative initiative, but the extraction is not severe because (a) departments retain operational autonomy within approved budgets, (b) Congress retains amendment power, and (c) the coordination function is genuine and produces real efficiency gains. The metric reflects that the system is fundamentally about coordination with embedded asymmetry, not pure extraction. The measured increase from 0.15 to 0.28 over the interval reflects gradual presidential habituation to centralized control: initial implementation (1946-1950) was experimental and less coercive; by the 1960s-1970s, presidential budget authority became routinized and extraction increased. Suppression (0.32): Moderate. Barriers to exit from the unified system include: (a) legal requirement for unified submission (departments cannot bypass the system), (b) career consequences for defying presidential budget authority (Cabinet secretaries who openly challenge president's priorities face removal), (c) Congressional barriers to direct departmental appeals (committees have normalized receiving requests through presidential channels), (d) public expectations that the President presents a unified budget (circumventing this creates scandal). However, suppression is not high because: (a) departments can negotiate within the system, (b) Congress can amend the presidential budget substantially, (c) agencies have some flexibility in implementation. Theater ratio (0.48): Moderate. The unified budget system combines genuine function (government-wide planning) with performative elements (extensive review processes, coordination rituals, formal submission ceremonies). The theater has increased over time as the mechanism matured—initial implementation required genuine deliberation; by the 1970s-1980s, much of the review process was ritual. The theater serves a coordination function (it structures the conversation, signals priorities) but increasingly obscures actual decision-making.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across institutional levels. The Presidential Administration sees Rope (pure coordination; they are solving the legitimate government-wide planning problem). The Budget Bureau sees Rope (they implement coordination machinery without experiencing extraction). The Departmental Directors see Snare (pure extraction; they have lost autonomy with minimal coordination benefit to their specific operations). Congressional Committees see Tangled Rope (genuine mixed experience of oversight capability and constraint). Individual Congressional Representatives see Rope (they experience the unified budget as a coordination problem they solve through committee participation and amendment). The Budget Bureau over time (piton perspective) sees the system as increasingly performative—coordination machinery persists through institutional inertia even when genuine deliberation has become ritual. The perspectival gaps are driven by asymmetric power relationships: those who benefit from consolidation see coordination; those who lose autonomy see extraction; those with institutional responsibilities see a mix; those with mobile exit options see a coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to the extraction flow. The Presidential Administration sits at d=0.0 (full beneficiary): the unified budget requirement consolidates authority in their hands, enabling efficient resource allocation and strategic control. They have arbitrage capacity—they can work around the budget system by executive order, reorganization, or emergency funding. Departmental directors sit at d=0.95 (nearly full targets): they lose autonomous budgeting authority and must operate within presidentially-approved frameworks. Their trapped exit option reflects that they cannot formally exit the unified budget requirement; the only exit is resignation or defiance (both costly). Congressional Appropriations Committees sit at d=0.50 (symmetric): they benefit from comprehensive oversight (coordination function) but are constrained by presidential pre-structuring (extraction mechanism). Their constrained exit option reflects that they could theoretically reject the entire budget, but this is rare and high-cost. Individual Congressional Representatives sit at d=0.35 (modest beneficiary in the aggregate, though frustrated individually): they lose individual earmark authority but gain from collective oversight capabilities; their mobile exit option reflects that members can challenge the system, shift party alignment, or propose amendments. Budget Bureau/OMB sits at d=0.10 (weak beneficiary): the apparatus implements the unified system but experiences no extraction—only coordination function. The Analytical Observer sits at d=0.72 (moderate target): the civilizational view requires acknowledging both extraction and coordination, so the observer experiences both mechanisms proportionally.
 *
 * MANDATROPHY ANALYSIS:
 *   The comprehensive executive budget system resolves mandatrophy by demonstrating that the tangled-rope classification is structurally justified: the constraint has both genuine coordination function (government-wide budget planning, elimination of duplicates, strategic resource allocation) and genuine extraction mechanism (presidential centralization of authority, loss of departmental autonomy, constraint on legislative initiative). No single-type classification captures both. The Rope perspective (Presidential Administration, Budget Bureau) is legitimate—they genuinely experience coordination. The Snare perspective (Departmental Directors) is legitimate—they genuinely experience extraction. The Tangled Rope perspective (Congressional Committees, Analytical Observer) is the correct synthesizing classification. The mandatrophy is resolved by recognizing that the system's design intentionally trades departmental autonomy for government-wide coordination. Whether this tradeoff is wise is a values question (empirical omega variables can measure its effects, but normative judgment requires policy choice). The Piton perspective (institutional system over time) suggests that the theater ratio is increasing—the original coordination function may be atrophying while the extraction mechanism persists. This is a warning signal for FSM-type degradation: if the system has been captured to serve presidential extraction at the expense of genuine government-wide planning, it should eventually reclassify from Tangled Rope (mixed function) to Snare (extraction-dominant).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_centralization_boundary,
    'To what degree does the unified budget system represent genuine coordination improvement versus presidential power consolidation disguised as administrative efficiency?',
    'Historical counterfactual: compare actual budget outcomes under unified system with hypothetical outcomes under agency-direct-petition system; measure diversity of departmental funding outcomes, volatility of budget allocations, and departmental satisfaction with review process',
    'If coordination dominates: the constraint should be classified as Rope from more perspectives, with suppression understood as necessary overhead. If centralization dominates: more perspectives should see Snare, with suppression understood as extractive coercion. The empirical ratio determines mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_centralization_boundary, empirical, 'Boundary between genuine coordination and power consolidation').

omega_variable(
    alternative_coordination_mechanisms,
    'Could the same coordination function (comprehensive government planning) be achieved through decentralized negotiation between departments and Congressional committees without presidential unification requirement?',
    'Case study comparison: UK Parliament''s departmental budget review process, multi-committee coordination in other democracies; analysis of whether agency autonomy and legislative initiative improve outcomes without sacrificing government-wide planning',
    'If decentralized alternatives work: the suppression metric should be reassessed upward (the unified system imposes barriers that are not inevitable). If decentralized alternatives fail: suppression is justified as necessary overhead, and the constraint should be viewed as less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether decentralized alternatives achieve same coordination without suppression').

omega_variable(
    departmental_innovation_cost,
    'Does the unified budget requirement suppress departmental innovation and mission-drift-correction by requiring all new initiatives to survive presidential review and integrated priority-setting?',
    'Comparative analysis of new departmental programs approved under unified budget vs hypothetical approval rates under direct-petition system; measurement of time-lag from departmental proposal to Congressional consideration; assessment of how often presidential priorities overrule departmental expertise in resource allocation',
    'If innovation cost is high: the victim classification ''departmental autonomy'' is well-founded, extractiveness should be reassessed upward, and the constraint should show Snare from departmental perspectives more strongly. If innovation cost is low: extractiveness is justified, and the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(departmental_innovation_cost, empirical, 'Whether unified budget suppresses departmental innovation').

omega_variable(
    legislative_amendment_capacity_retention,
    'Do Congressional amendments to the unified presidential budget maintain effective legislative initiative, or does presidential pre-structuring reduce amendment capacity below historical baselines?',
    'Quantitative analysis: ratio of Congressional amendments to presidential budget submission vs ratio of amendments to individual agency budget requests under pre-1946 system; measurement of amendment success rates and magnitude of budget changes achieved through amendment',
    'If amendment capacity is fully retained: Congressional Appropriations Committees experience the constraint as pure coordination (Rope classification justified). If amendment capacity is substantially reduced: Committees experience extraction, supporting Tangled Rope classification and higher suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_amendment_capacity_retention, empirical, 'Whether Congressional amendment capacity survives presidential pre-structuring').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1946_truman_comprehensive_executive_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1946_truman_comprehensive_executive_budget, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t5, sotu_1946_truman_comprehensive_executive_budget, theater_ratio, 5, 0.42).
narrative_ontology:measurement(sotu_tr_t10, sotu_1946_truman_comprehensive_executive_budget, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1946_truman_comprehensive_executive_budget, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sotu_be_t5, sotu_1946_truman_comprehensive_executive_budget, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(sotu_be_t10, sotu_1946_truman_comprehensive_executive_budget, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1946_truman_comprehensive_executive_budget, resource_allocation).
narrative_ontology:affects_constraint(sotu_1946_truman_comprehensive_executive_budget, presidential_appointments_authority).
narrative_ontology:affects_constraint(sotu_1946_truman_comprehensive_executive_budget, executive_reorganization_power).
narrative_ontology:affects_constraint(sotu_1946_truman_comprehensive_executive_budget, impoundment_control_authority).

% DUAL FORMULATION NOTE:
% The comprehensive executive budget is the structural upstream constraint enabling presidential control of resource allocation. Downstream constraints (appointments authority, reorganization power, impoundment control) are aspects of how presidents exercise budgetary authority once consolidated. These are causally linked: the unified budget system creates the infrastructure through which other presidential powers operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
