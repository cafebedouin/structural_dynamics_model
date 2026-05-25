% ============================================================================
% CONSTRAINT STORY: collective_action_as_leverage_conversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_as_leverage_conversion, []).

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
 *   constraint_id: collective_action_as_leverage_conversion
 *   human_readable: Collective Action as Leverage Conversion Mechanism
 *   domain: organizational_dynamics/labor_relations/institutional_power
 *
 * SUMMARY:
 *   Collective action in labor relations represents a coordination technology
 *   that converts individual powerlessness into collective bargaining
 *   leverage. The constraint exhibits strong perspectival variation: isolated
 *   workers experience maximum extraction (Snare), organizing workers
 *   experience mixed coordination and extraction (Tangled Rope), established
 *   unions and labor movement infrastructure experience pure coordination
 *   (Rope), and sectoral bargaining coalitions see a path to
 *   institutionalization (Scaffold). The analytical observer classifies the
 *   mechanism as Rope — a genuine solution to the bilateral bargaining
 *   asymmetry inherent to labor markets. The constraint's extractiveness
 *   (0.38) reflects real coordination costs: organizing labor, free-rider
 *   problems, retaliation risk, and coordination overhead. These costs are
 *   not zero, but they are substantially lower than the extraction workers
 *   face in the absence of collective organization. The theater_ratio (0.35)
 *   reflects moderate performative content: some collective action rituals
 *   (symbolic strikes, performative negotiations, organizational maintenance
 *   activities) serve signaling functions rather than direct coordination,
 *   but the core mechanism (credible threat capacity through coordinated
 *   withdrawal of labor) is functional. The measurements show gradual
 *   increase in both theater and extractiveness over the interval, consistent
 *   with organizational maturity and potential early-stage institutional
 *   capture dynamics.
 *
 * KEY AGENTS:
 *   - Isolated Worker: Primary victim in absence of organization (powerless/trapped) — faces maximum extraction from bilateral bargaining asymmetry; collective action mechanism is inaccessible
 *   - Organizing Worker: Mixed position (moderate/constrained) — experiences both coordination benefit and extraction cost; bears organizing labor and retaliation risk
 *   - Established Union: Primary beneficiary of mature coordination (organized/mobile) — has institutional capacity, financial reserves, and exit options; experiences low effective extraction
 *   - Labor Movement Infrastructure: Institutional beneficiary (institutional/arbitrage) — federations, legal funds, organizing networks that provide coordination technology and institutional memory
 *   - Marginal Worker: Secondary victim (moderate/constrained) — excluded from organizing effort due to employment precarity; benefits from wage floor effects but bears coordination costs without full protection
 *   - Sectoral Bargaining Coalition: Organized agents building institutional sunset (organized/mobile) — sees path to encoding coordination function into legal structures, reducing need for sustained mobilization
 *   - Coordination Defectors: Victims of free-rider problem — workers who benefit from collective bargaining without bearing organizing costs; create extraction asymmetry for active organizers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees collective action as coordination technology solving bilateral bargaining asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_as_leverage_conversion, 0.38).
domain_priors:suppression_score(collective_action_as_leverage_conversion, 0.42).
domain_priors:theater_ratio(collective_action_as_leverage_conversion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_as_leverage_conversion, extractiveness, 0.38).
narrative_ontology:constraint_metric(collective_action_as_leverage_conversion, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(collective_action_as_leverage_conversion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_as_leverage_conversion, rope).
narrative_ontology:human_readable(collective_action_as_leverage_conversion, "Collective Action as Leverage Conversion Mechanism").
narrative_ontology:topic_domain(collective_action_as_leverage_conversion, "organizational_dynamics/labor_relations/institutional_power").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_as_leverage_conversion, organized_workers).
narrative_ontology:constraint_beneficiary(collective_action_as_leverage_conversion, labor_movement_infrastructure).
narrative_ontology:constraint_beneficiary(collective_action_as_leverage_conversion, policy_reform_beneficiaries).
narrative_ontology:constraint_victim(collective_action_as_leverage_conversion, coordination_defectors).
narrative_ontology:constraint_victim(collective_action_as_leverage_conversion, marginal_workers_excluded_from_organizing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED WORKER (SNARE) — Individual worker without collective organization faces maximum extraction. Cannot exit employment without severe economic penalty, has no bargaining power, and bears full cost of employer discretion. The collective action mechanism is invisible or inaccessible from this position — the worker is trapped in bilateral asymmetry.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZING WORKER (TANGLED ROPE) — Worker participating in collective action experiences both coordination benefit (increased bargaining power, mutual aid, shared risk) and extraction cost (time investment, retaliation risk, free-rider problem, coordination overhead). Can exit organizing effort but at cost of losing collective protection. Mixed experience: genuine coordination function with embedded extraction from coordination defectors and organizational maintenance burden.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED UNION (ROPE) — Mature labor organization with institutional capacity experiences collective action as pure coordination mechanism. Has exit options (can shift organizing focus, has financial reserves, can form coalitions), operates at generational time horizon where short-term coordination costs are amortized. Coordination function is clear: converting dispersed individual powerlessness into concentrated collective leverage. Low effective extraction because the organization has agency and resources.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT INFRASTRUCTURE (ROPE) — Institutional actors (labor federations, legal defense funds, organizing networks) experience collective action as coordination technology. Have arbitrage exit options (can shift resources between campaigns, can operate in multiple jurisdictions), benefit from network effects and institutional memory. See the constraint as a solution to the bilateral bargaining asymmetry problem — a mechanism that solves a genuine coordination failure.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MARGINAL WORKER (TANGLED ROPE) — Worker excluded from organizing effort (part-time, gig economy, undocumented, high turnover sector) experiences collective action as simultaneously beneficial (raises wage floor, improves working conditions broadly) and extractive (organizing resources flow to stable workforces, marginal workers bear coordination costs without full protection). Constrained exit: can leave specific job but not labor market position. Coordination function exists but extraction is asymmetric.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: SECTORAL BARGAINING COALITION (SCAFFOLD) — Organized coalition building sectoral bargaining frameworks (industry-wide agreements, co-determination structures, works councils) sees collective action as temporary coordination mechanism with sunset logic. Once sectoral institutions are established, the need for sustained mobilization decreases — the coordination function is encoded into legal/institutional structures. Low extraction because organized agents have exit options and see path to institutionalization.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From civilizational perspective, collective action is a coordination technology that solves the bilateral bargaining asymmetry inherent to labor markets. Converts individual powerlessness (trapped agents with no exit) into collective leverage (organized agents with credible threat capacity). The mechanism has coordination overhead (organizing costs, free-rider problems, coordination failures) but this is inherent to any solution to the collective action problem, not extractive rent-seeking. Analytical classification: Rope.
constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_as_leverage_conversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_as_leverage_conversion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(collective_action_as_leverage_conversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Collective action has real coordination costs — organizing labor (estimated 5-15% of workforce time during active campaigns), free-rider problems (non-union workers capturing 60-80% of union wage premium in some sectors), retaliation risk (organizers face 2-3x higher termination rates), and coordination overhead (dues, meetings, strike funds). However, these costs are substantially lower than the extraction workers face without collective organization. The bilateral bargaining asymmetry in labor markets (employers have structural power advantage due to workers' need for income, limited savings, and job search costs) creates extraction in the range of 0.60-0.80 for isolated workers. Collective action reduces this to 0.38 by converting individual powerlessness into collective leverage. The extractiveness is not zero because coordination is not free, but it represents a net reduction in total extraction. Suppression (0.42): Moderate. Barriers to collective action include legal restrictions (right-to-work laws, limitations on secondary boycotts, restrictions on public sector bargaining), employer retaliation (termination, blacklisting, surveillance), coordination costs (organizing requires sustained effort across dispersed workforce), and free-rider problems (individual incentive to defect from collective action). However, suppression is not total — workers can and do organize successfully, and legal protections (NLRA, sector-specific bargaining rights) reduce some barriers. Theater ratio (0.35): Moderate. Some collective action activities are performative — symbolic strikes that are not intended to impose economic costs, ritualized negotiation processes, organizational maintenance activities that serve internal legitimacy rather than external coordination. However, the core mechanism (credible threat of coordinated work stoppage) is functional, not theatrical. The theater has increased over the interval as organizations mature and some activities shift from direct coordination to organizational maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates strong perspectival variation driven by structural position. Isolated workers see Snare — they are trapped in bilateral bargaining asymmetry with no access to collective action. Organizing workers see Tangled Rope — they experience both coordination benefit and extraction cost, with asymmetric burden on active organizers. Established unions see Rope — they have institutional capacity and experience the mechanism as pure coordination technology. Sectoral bargaining coalitions see Scaffold — they are building institutional structures that will reduce the need for sustained mobilization. The analytical observer sees Rope — collective action is a coordination technology that solves the bilateral bargaining asymmetry, and coordination costs are inherent to the solution. The perspectival gap is not a disagreement about facts but a difference in structural position: what you see depends on whether you are trapped outside the coordination mechanism, bearing organizing costs within it, benefiting from institutional maturity, or observing from analytical distance. The gap is diagnostically meaningful — it reveals that the 'same' constraint (collective action) is experienced as extraction, mixed coordination-extraction, pure coordination, or temporary coordination depending on the observer's power, exit options, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the collective action mechanism. Isolated workers are victims of the bilateral bargaining asymmetry that collective action solves — they have no access to the coordination mechanism and bear maximum extraction. Organizing workers are mixed: they benefit from collective bargaining power but bear asymmetric organizing costs and retaliation risk. Established unions and labor movement infrastructure are beneficiaries — they have institutional capacity, exit options, and experience the mechanism as pure coordination. Marginal workers excluded from organizing are victims of coordination boundary effects — they benefit from spillover effects (wage floor, working conditions improvements) but bear costs without full protection. Coordination defectors (free-riders) are victims in the sense that active organizers bear disproportionate costs, creating extraction asymmetry within the collective. The analytical observer sees the mechanism as coordination technology — it solves a genuine collective action problem, and the coordination costs are inherent to any solution, not extractive rent-seeking. Directionality values are derived from beneficiary/victim declarations and exit options: trapped isolated workers have high d (victim, no exit), constrained organizing workers have moderate d (mixed position, costly exit), mobile established unions have low d (beneficiary, exit options), analytical observer has moderate d (standard analytical position).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that collective action is primarily a coordination mechanism (Rope from analytical and institutional perspectives) with embedded extraction costs that vary by structural position. The coordination function is genuine: collective action solves the bilateral bargaining asymmetry inherent to labor markets by converting individual powerlessness into collective leverage. The extraction is real but secondary: organizing costs, free-rider problems, and retaliation risk create asymmetric burdens, particularly for active organizers and marginal workers excluded from organizing. The classification as Rope (claimed type) is validated from the analytical perspective and from the perspective of established unions with institutional capacity. The Tangled Rope classification from organizing workers and marginal workers reflects that coordination costs are not evenly distributed — some agents bear disproportionate burdens. The Snare classification from isolated workers reflects that the coordination mechanism is inaccessible to them — they are trapped in the bilateral asymmetry that collective action solves. The Scaffold classification from sectoral bargaining coalitions reflects a real structural feature: as coordination functions are encoded into legal and institutional structures, the need for sustained mobilization decreases. The mandatrophy is resolved by recognizing that all classifications are legitimate perspectival readings of the same structural data, and the perspectival variation is itself diagnostic information about how coordination costs and benefits are distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohesion_maintenance_cost_threshold,
    'At what point does the cost of maintaining collective cohesion (dues, meeting time, strike fund contributions, retaliation risk) exceed the benefit of collective bargaining power for marginal participants?',
    'Longitudinal analysis of union participation rates vs wage premium; exit interviews with workers who leave organizing efforts; cost-benefit analysis at different income levels and job security positions',
    'If threshold is low (cohesion costs exceed benefits for >30% of potential members): collective action is extractive for marginal workers, classification shifts toward Tangled Rope or Snare from more perspectives. If threshold is high (benefits exceed costs for >70%): collective action is genuine coordination, Rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohesion_maintenance_cost_threshold, empirical, 'Threshold where cohesion maintenance costs exceed collective bargaining benefits').

omega_variable(
    free_rider_extraction_magnitude,
    'How much of the coordination overhead is borne by active organizers vs distributed across all beneficiaries? Does the free-rider problem constitute extraction from organizers or necessary coordination cost?',
    'Time-use studies of organizing vs non-organizing workers who benefit from collective bargaining; analysis of who bears retaliation risk vs who captures wage gains; measurement of organizing labor as proportion of total workforce',
    'If free-rider burden is concentrated (organizers bear >3x the cost per unit benefit): extraction is asymmetric, Tangled Rope classification strengthens. If burden is distributed (costs scale with benefits): coordination overhead is symmetric, Rope classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_rider_extraction_magnitude, empirical, 'Whether free-rider problem constitutes extraction or coordination cost').

omega_variable(
    institutional_capture_timeline,
    'At what point does established labor organization shift from representing worker interests to maintaining organizational survival? Does institutional maturity reduce or increase extractiveness?',
    'Historical analysis of union responsiveness to member preferences vs organizational imperatives; comparison of wage gains in newly organized vs long-established bargaining units; analysis of union democracy metrics over organizational lifecycle',
    'If capture occurs rapidly (<10 years): Scaffold perspective is correct — collective action should sunset into formal institutions. If capture is rare or slow (>30 years): Rope perspective is correct — mature organizations maintain coordination function. If capture is inevitable and rapid: Piton classification emerges for established unions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_timeline, empirical, 'Timeline and inevitability of institutional capture in labor organizations').

omega_variable(
    sectoral_vs_enterprise_bargaining_efficiency,
    'Does sectoral bargaining (industry-wide coordination) reduce extraction relative to enterprise bargaining (firm-level coordination) by distributing costs more broadly, or does it increase extraction by creating larger coordination overhead and more opportunities for capture?',
    'Cross-national comparison of wage inequality, worker voice, and organizing costs under different bargaining regimes; analysis of coordination overhead as function of bargaining unit size; measurement of worker satisfaction and exit rates under different structures',
    'If sectoral bargaining reduces extraction: Scaffold perspective is validated — path to institutionalization reduces coordination costs. If sectoral bargaining increases extraction: enterprise-level organizing is more efficient coordination mechanism, and sectoral structures are Tangled Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_vs_enterprise_bargaining_efficiency, empirical, 'Comparative efficiency of sectoral vs enterprise bargaining structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_as_leverage_conversion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_act_theater_t0, collective_action_as_leverage_conversion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coll_act_theater_t3, collective_action_as_leverage_conversion, theater_ratio, 3, 0.25).
narrative_ontology:measurement(coll_act_theater_t6, collective_action_as_leverage_conversion, theater_ratio, 6, 0.3).
narrative_ontology:measurement(coll_act_theater_t10, collective_action_as_leverage_conversion, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(coll_act_extract_t0, collective_action_as_leverage_conversion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(coll_act_extract_t3, collective_action_as_leverage_conversion, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(coll_act_extract_t6, collective_action_as_leverage_conversion, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(coll_act_extract_t10, collective_action_as_leverage_conversion, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_as_leverage_conversion, resource_allocation).
narrative_ontology:affects_constraint(collective_action_as_leverage_conversion, architectural_constraint_as_dual_substrate).

% DUAL FORMULATION NOTE:
% Collective action is downstream of architectural constraints that structure labor market bargaining. The bilateral bargaining asymmetry (architectural_constraint_as_dual_substrate) creates the coordination problem that collective action solves. The upstream constraint is classified as Mountain (immutable structural feature of labor markets under capitalism), while the downstream constraint (collective action) is classified as Rope (coordination technology that mitigates but does not eliminate the upstream asymmetry). The network relationship is causal: the existence and severity of the bilateral bargaining asymmetry determines the necessity and effectiveness of collective action as a coordination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
