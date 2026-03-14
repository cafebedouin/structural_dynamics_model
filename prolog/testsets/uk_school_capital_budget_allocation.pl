% ============================================================================
% CONSTRAINT STORY: uk_school_capital_budget_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_school_capital_budget_allocation, []).

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
 *   constraint_id: uk_school_capital_budget_allocation
 *   human_readable: UK School Capital Budget Allocation System
 *   domain: education_policy/public_finance
 *
 * SUMMARY:
 *   The UK school capital budget allocation system distributes scarce
 *   infrastructure investment across thousands of schools through a
 *   competitive bidding and prioritization framework. This constraint
 *   exhibits the classic tangled_rope structure: it coordinates legitimate
 *   capital planning across multiple institutional actors while
 *   simultaneously embedding mechanisms that extract disproportionate
 *   resources toward already-advantaged schools. The system serves a genuine
 *   coordination function — preventing duplicate investments, allocating
 *   limited capital strategically, coordinating multi-school infrastructure
 *   projects — but this coordination coexists with systematic extraction
 *   favoring institutional capacity-rich actors. Affluent local authorities
 *   and well-connected academy chains with professional bidding capacity,
 *   existing infrastructure quality, and political access navigate the system
 *   more effectively than deprived schools, rural networks, and institutions
 *   without professional support. The constraint's theater_ratio (0.68)
 *   reflects that competitive bidding processes, scoring matrices, and
 *   prioritization criteria create substantial performative activity:
 *   evaluation committees spend significant effort scoring bids according to
 *   rubrics with weak correlation to stated pedagogical priorities
 *   (need-based allocation, equity, closure prevention). Yet the bureaucratic
 *   apparatus persists because it provides legitimating language for
 *   allocation decisions actually driven by institutional capacity and
 *   pre-existing advantage.
 *
 * KEY AGENTS:
 *   - Deprived School Community: Primary victim (powerless/trapped) — lack professional bidding capacity, poor existing infrastructure, no independent capital access; bears disproportionate cost of undersupply
 *   - Rural School Network: Secondary victim (moderate/constrained) — can aggregate negotiating power through federation but disadvantaged in competitive bidding; can exit via academy conversion at cost to network stability
 *   - Well-Connected Academy Chain: Primary beneficiary (institutional/arbitrage) — possesses professional bidding expertise, capital planning staff, political relationships; extracts disproportionate allocation; can exit via independent capital markets
 *   - Professional Capital Bidding Consultant: Secondary beneficiary (powerful/arbitrage) — provides genuine service while concentrating value capture among schools that can afford consultation fees; enables further extraction through professionalization of access
 *   - Capital Allocation Bureaucracy: Institutional actor (institutional/arbitrage) — maintains bidding process theater; allocation decisions driven by pre-existing advantage despite meritocratic evaluation criteria
 *   - Department for Education Policy: Overseer (analytical/analytical) — sets allocation framework; risks naturalizing structural inequality as meritocratic outcome of competitive process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_school_capital_budget_allocation, 0.58).
domain_priors:suppression_score(uk_school_capital_budget_allocation, 0.62).
domain_priors:theater_ratio(uk_school_capital_budget_allocation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_school_capital_budget_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_school_capital_budget_allocation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(uk_school_capital_budget_allocation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_school_capital_budget_allocation, tangled_rope).
narrative_ontology:human_readable(uk_school_capital_budget_allocation, "UK School Capital Budget Allocation System").
narrative_ontology:topic_domain(uk_school_capital_budget_allocation, "education_policy/public_finance").

domain_priors:requires_active_enforcement(uk_school_capital_budget_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_school_capital_budget_allocation, affluent_local_authorities).
narrative_ontology:constraint_beneficiary(uk_school_capital_budget_allocation, well_connected_academy_chains).
narrative_ontology:constraint_beneficiary(uk_school_capital_budget_allocation, professional_capital_bidding_consultants).
narrative_ontology:constraint_victim(uk_school_capital_budget_allocation, deprived_school_communities).
narrative_ontology:constraint_victim(uk_school_capital_budget_allocation, rural_school_networks).
narrative_ontology:constraint_victim(uk_school_capital_budget_allocation, schools_without_professional_support).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPRIVED SCHOOL COMMUNITY (SNARE) — Trapped by lack of professional bidding capacity, poor initial infrastructure requiring higher maintenance costs, and dependence on central allocation with no exit option. Bears full cost of capital undersupply while more affluent schools extract disproportionate resources. Geographic and economic barriers prevent relocation or independent capital sourcing.
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL SCHOOL NETWORK (TANGLED ROPE) — Experiences genuine coordination benefits from shared capital planning (economies of scale on repairs, shared procurement) alongside extraction through competitive bidding disadvantage relative to urban clusters. Can exit via academy conversion at cost to federation stability. Some agency but significant extraction.
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WELL-CONNECTED ACADEMY CHAIN (ROPE) — Experiences the constraint as coordination mechanism. Has professional bidding capacity, capital planning expertise, and political access. Extracts resource surplus through superior navigation of bureaucracy. Net beneficiary. Can exit via independent capital markets if needed (arbitrage option).
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROFESSIONAL CAPITAL BIDDING CONSULTANT (TANGLED ROPE) — Provides genuine service (helping schools navigate complex bidding processes) while extracting fees/retainers that concentrate value capture among schools that can afford consultancy. Coordinates capital planning while ensuring that superior advice flows disproportionately to well-resourced institutions. Can exit via private sector work (arbitrage). Benefits the system while restructuring it to require their services.
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPITAL BIDDING BUREAUCRACY (PITON) — The competitive bidding and prioritization processes are largely performative theater. Capital allocation is driven by pre-existing regional inequality, academy conversion incentives, and institutional capacity rather than pedagogical need or closure-prevention logic. The bureaucracy maintains the appearance of meritocratic allocation while channeling resources by structural advantage. Theater ratio (0.68) reflects that evaluation criteria, scoring matrices, and feedback mechanisms persist despite weak correlation with stated pedagogical priorities.
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint coordinates legitimate capital planning across multiple institutional actors while simultaneously embedding structural extraction favoring already-advantaged schools. Genuine coordination (avoiding duplication, preventing closure cascades) coexists with asymmetric extraction (capital flowing to capacity-rich institutions). The system cannot be decomposed into pure coordination or pure extraction — both functions are structural and inseparable.
constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_school_capital_budget_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_school_capital_budget_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_school_capital_budget_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_school_capital_budget_allocation, TR),
    TR >= 0.70.

:- end_tests(uk_school_capital_budget_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system extracts disproportionate resources toward capacity-rich institutions over a 5–10 year capital planning cycle. The extraction is not total (some deprived schools do receive capital allocations) and is justified as meritocratic (capital flows to schools with strongest bids and clearest need). But structural analysis reveals that bidding capacity, existing infrastructure quality, political access, and organizational sophistication drive allocation more strongly than pedagogical need. The extraction increased from 0.42 to 0.58 over the interval as academy conversion incentives became embedded in capital prioritization and professional bidding consultancy matured as a fee-extracting service. Suppression (0.62): High. Schools trapped in capital undersupply face substantial barriers: no independent borrowing capacity, no profitable private partnerships for school infrastructure, no exit option via geographic relocation, professional bidding services unaffordable to resource-poor schools, political access limited. However, suppression is not absolute — some schools do mobilize alternative funding through charitable foundations, community fundraising, and academy chain leverage. Theater ratio (0.68): Moderately high. Bidding processes, evaluation criteria, and needs assessments create substantial performative activity. Rubrics for scoring bids exist but have weak correlation with stated pedagogical priorities (closure prevention, need-based equity). Evaluation committees spend significant time reviewing bids according to matrices that appear data-driven but reflect pre-existing institutional advantage (Is this school well-organized enough to navigate bureaucracy? Does it have capital planning expertise?). Yet the theater persists because competitive evaluation provides legitimating narrative for allocation decisions actually driven by capacity and advantage.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how coordination language masks extraction mechanisms. The institutional beneficiary (well-connected academy chain) perceives the system as Rope — coordination that enables their capital planning and allocation success. They experience extraction flowing toward them, which they frame as meritocratic reward for superior planning. The powerless victim (deprived school community) perceives the system as Snare — pure extraction with no coordination benefit. They lack bidding capacity, face resource barriers, and have no exit. The rural network perceives Tangled Rope — genuine value from aggregated planning and procurement alongside extraction from competitive bidding disadvantage relative to larger urban clusters. The piton perspective reveals that the system's evaluation apparatus is performative — committees spend significant effort scoring bids according to rubrics with weak correlation to stated allocation priorities. The gap between Rope (beneficiary) and Snare (victim) perspectives reveals the constraint's core structure: the coordination function (legitimate capital planning) is REAL, but it is asymmetrically distributed. Well-resourced institutions coordinate effectively and receive resources; poorly resourced institutions fail at coordination and receive undersupply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with strong institutional capacity and political access experience low effective extraction (d ≈ 0.15–0.25); the system enables their navigation and allocates resources toward them. Victims trapped in capital undersupply and professional bidding incapacity experience high extraction (d ≈ 0.85–0.95); the system concentrates resources away from them. Moderate-power actors (rural networks, some academy chains) experience constrained exit (d ≈ 0.55–0.65); they can navigate the system better than trapped actors but worse than well-connected institutions. The analytical observer sees the system as fundamentally extractive despite coordination language (d ≈ 0.70–0.75). The directionality derives from: (1) beneficiary status and institutional capacity enabling effective navigation, (2) victim status and resource poverty creating navigation barriers, (3) exit options differentiating constrained from trapped agents, (4) political access and professional expertise enabling arbitrage between competitive bidding and private capital markets.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled_rope is the analytically correct classification despite strong pressure to classify as either pure rope (coordination) or pure snare (extraction). The system genuinely coordinates capital planning across multiple actors and prevents duplicate investments and cascade failures — removing the coordination function would harm all schools. But the same system asymmetrically distributes this coordination benefit: institutions with capacity benefit, institutions without capacity are harmed. The constraint cannot be reframed as pure coordination (rope) because the extraction is real and systematic — deprived schools lose access to resources relative to what a need-based allocation would provide. But it also cannot be classified as pure extraction (snare) because a significant portion of capital allocation does follow needs-based logic and the bidding process does enable some redistribution (albeit captured at the margins). The tangled_rope classification reflects the true structure: coordination and extraction are structurally inseparable in this system. The bidding process coordinates capital while extracting through institutional advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    need_vs_capacity_visibility,
    'How much of observed capital allocation inequality reflects genuine pedagogical need versus extraction of capacity-rich schools?',
    'Comparative analysis: capital invested per pupil per infrastructure deficiency versus capital invested per administrative capacity; regression of allocation on need metrics controlling for institutional capacity',
    'High need-correlation: extraction component ≈ 0.20–0.30 (allocation is mostly needs-driven). Low need-correlation: extraction component ≈ 0.50+ (capacity determines allocation, need is noise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(need_vs_capacity_visibility, empirical, 'How much allocation inequality reflects need versus institutional capacity').

omega_variable(
    academy_conversion_incentive_mix,
    'Is academy conversion preferential in capital allocation a design feature for school improvement or an extraction mechanism favoring chains with conversion leverage?',
    'Longitudinal tracking: Do converted academies receive disproportionate capital before or after conversion? Do capital promises drive conversion or follow from conversion? Comparison of capital allocation growth for converting vs non-converting schools.',
    'If design feature: extraction recedes; tangled_rope classification confirmed. If mechanism: extraction component rises; snare classification becomes more accurate for non-converting schools.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academy_conversion_incentive_mix, empirical, 'Whether academy conversion preference in capital allocation drives beneficial restructuring or extracts via conversion leverage').

omega_variable(
    bidding_process_barrier_magnitude,
    'How much of capital allocation inequality is due to bidding process complexity versus pre-existing resource inequality in schools?',
    'Intervention trial: simplified bidding process / dedicated bidding support for deprived schools; measurement of allocation change. Structural decomposition: isolation of bidding-barrier component versus network-effect component.',
    'If bidding is barrier (70%+ of gap): suppression moderate (0.40–0.55), constraint is solvable (tangled_rope). If pre-existing inequality is driver (70%+): suppression high (0.65+), constraint is structural (snare/scaffold depending on exit feasibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidding_process_barrier_magnitude, empirical, 'Whether capital allocation inequality is driven by bidding process complexity or pre-existing school inequality').

omega_variable(
    escape_valve_feasibility,
    'Can schools realistically fund capital from alternative sources (borrowing, private partnerships, fundraising) or is central allocation truly the only exit?',
    'Inventory of alternative capital sources; cost comparison (interest rates, risk exposure). Tracking of schools attempting private capital pathways; success/failure rates. Policy analysis of barriers to alternative sourcing.',
    'If alternatives exist: exit options escalate from trapped→constrained; snare classification weakens. If alternatives are inaccessible: trapped status confirmed; snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_valve_feasibility, empirical, 'Whether schools have realistic access to alternative capital sources beyond central allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_school_capital_budget_allocation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukscb_tr_t0, uk_school_capital_budget_allocation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ukscb_tr_t5, uk_school_capital_budget_allocation, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ukscb_tr_t10, uk_school_capital_budget_allocation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ukscb_be_t0, uk_school_capital_budget_allocation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ukscb_be_t5, uk_school_capital_budget_allocation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ukscb_be_t10, uk_school_capital_budget_allocation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_school_capital_budget_allocation, resource_allocation).
narrative_ontology:boltzmann_floor_override(uk_school_capital_budget_allocation, 0.18).
narrative_ontology:affects_constraint(uk_school_capital_budget_allocation, academy_conversion_incentive_coupling).
narrative_ontology:affects_constraint(uk_school_capital_budget_allocation, local_authority_fiscal_stress_spiral).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of related structures in UK education finance. The academy conversion incentive coupling (separate story) creates preferential capital access for converting schools. The local authority fiscal stress spiral (separate story) shows how capital undersupply feeds operating budget pressure. This story focuses on the capital allocation mechanism itself; decomposition into bidding-process barriers versus pre-existing inequality is handled via omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_school_capital_budget_allocation, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
