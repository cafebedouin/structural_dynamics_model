% ============================================================================
% CONSTRAINT STORY: political_schedule_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_schedule_pressure, []).

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
 *   constraint_id: political_schedule_pressure
 *   human_readable: Political Schedule Pressure
 *   domain: political_governance/institutional_constraint
 *
 * SUMMARY:
 *   Political schedule pressure constrains deliberation through electoral and
 *   fiscal calendars that compress decision-making into narrow windows. This
 *   constraint exhibits a structural hybrid: it provides genuine coordination
 *   (synchronizing legislative action, forcing prioritization, preventing
 *   indefinite obstruction) while simultaneously extracting through
 *   suppression of substantive deliberation, minority voice exclusion, and
 *   delegation to executive summary review. The constraint is maintained
 *   through multiple enforcement mechanisms — formal procedural rules
 *   (suspension of rules, expedited passage procedures), informal political
 *   costs (labeling delays as obstruction), and implicit media/market
 *   expectations around deadline-driven governance. Theater has increased
 *   over the measurement interval as formal deliberative procedures
 *   (committee hearings, floor debate, amendment markup) have become
 *   increasingly disconnected from actual decision points (which occur in
 *   leadership offices before floor votes). The constraint affects different
 *   agents asymmetrically: executive and party leadership benefit from
 *   compression; career legislators experience mixed costs and benefits;
 *   policy deliberation quality and legislative minorities bear extraction;
 *   the long-term planning capacity of government is structurally degraded.
 *
 * KEY AGENTS:
 *   - Executive Leadership & Party Leadership: Primary beneficiary (institutional/arbitrage) — controls schedule, uses pressure to concentrate power, benefits from reduced deliberation time
 *   - Legislative Minorities: Primary victim (powerless/trapped) — cannot exit compressed timelines, cannot slow process without political delegitimation, lack forum for substantive opposition
 *   - Policy Deliberation Quality: Secondary victim (powerless/trapped) — abstract collective good; cannot organize; compressed timelines prevent adequate analysis of complex policy implications
 *   - Long-Term Planning Capacity: Secondary victim (powerless/trapped) — government becomes reactive to immediate schedule demands; strategic planning displaced by crisis-driven governance
 *   - Career Legislators & Policy Staff: Mixed (moderate/constrained) — benefit from productivity metrics tied to legislation passed, constrained by reduced analysis time, gain from opponents' reduced capacity to mobilize
 *   - Advocacy Organizations: Organized agent (organized/constrained) — can mobilize around predictable schedules but extraction occurs when bills advance faster than advocacy capacity allows participation
 *   - Legislative Procedure & Committee System: Institutional inertia (institutional/arbitrage) — maintains performative apparatus while real decisions occur outside formal procedures
 *   - Reform Movements: Powerful challenger (powerful/mobile) — building alternative deliberative pathways (ballot measures, participatory budgeting) with potential sunset mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_schedule_pressure, 0.58).
domain_priors:suppression_score(political_schedule_pressure, 0.62).
domain_priors:theater_ratio(political_schedule_pressure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_schedule_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_schedule_pressure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(political_schedule_pressure, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_schedule_pressure, tangled_rope).
narrative_ontology:human_readable(political_schedule_pressure, "Political Schedule Pressure").
narrative_ontology:topic_domain(political_schedule_pressure, "political_governance/institutional_constraint").

domain_priors:requires_active_enforcement(political_schedule_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_schedule_pressure, executive_leadership).
narrative_ontology:constraint_beneficiary(political_schedule_pressure, media_cycle_actors).
narrative_ontology:constraint_victim(political_schedule_pressure, policy_deliberation_quality).
narrative_ontology:constraint_victim(political_schedule_pressure, legislative_minority).
narrative_ontology:constraint_victim(political_schedule_pressure, long_term_planning_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATIVE MINORITY (SNARE) — Trapped by the electoral cycle. Minority factions have no exit from pressure to vote on abbreviated timelines; cannot slow deliberation without being labeled obstructionist. Bears extraction of having their concerns heard only through performative opposition. Maximum suppression: procedural rules prevent extended debate on controversial bills; political cost of delay is delegitimation.
constraint_indexing:constraint_classification(political_schedule_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER LEGISLATORS & POLICY STAFF (TANGLED ROPE) — Constrained by electoral calendars and campaign schedules, but also benefit from accelerated passage (reduced time for opponents to mobilize, opportunity for major legislative accomplishment in compressed window). Experience genuine coordination (scheduling shared votes, synchronizing floor time) alongside extraction (forced votes on incomplete analysis). Career advancement depends on legislative productivity metrics tied to schedule pressure.
constraint_indexing:constraint_classification(political_schedule_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE & PARTY LEADERSHIP (ROPE) — Benefits from schedule compression as a coordination mechanism: accelerated timelines force coherence around leadership priorities, reduce floor delays, and concentrate decision-making power at the top. Leadership experiences this as effective coordination (getting legislation passed), not extraction. Has exit options: can set the schedule, negotiate timelines, prioritize bills.
constraint_indexing:constraint_classification(political_schedule_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents with some constrained exit. Benefit from predictable legislative calendars (can plan campaigns) and from schedule pressure reducing influence windows (forces mobilization). Extraction occurs when bills advance faster than advocacy capacity allows comment or testimony. Experience both coordination (predictable schedule) and extraction (time compression limits effective participation).
constraint_indexing:constraint_classification(political_schedule_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE PROCEDURE (PITON) — Committee review, hearings, and formal debate are increasingly performative: bills bypass committees through suspension of rules; hearings are scheduled for theater rather than genuine deliberation; markup sessions are choreographed around predetermined outcomes. The procedural apparatus persists through institutional inertia despite reduced functional verification of policy claims. Theater ratio reflects that formal processes are maintained but decisions are made outside them.
constraint_indexing:constraint_classification(political_schedule_pressure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM MOVEMENTS (SCAFFOLD) — Powerful actors (citizen initiatives, ballot measure movements, participatory budgeting pilots) are creating alternative decision pathways with longer timelines: ballot measures require extended public comment windows; participatory budgeting uses deliberative formats. These represent potential sunset mechanisms — distributed decision-making could reduce dependency on compressed legislative schedules. Sees schedule pressure as a solvable coordination problem with architectural solutions.
constraint_indexing:constraint_classification(political_schedule_pressure, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, electoral cycles are fixed structural features of representative democracy: decisions must be made within defined periods, and the constraint between decision capacity and calendar is immutable. This perspective naturalizes schedule pressure as inherent to democratic cycles. However, the structural data contradicts this — the measured suppression and theater ratio reveal that the constraint is institutional (procedural rules, campaign finance dependencies, leadership gatekeeping) rather than natural. False summit detection applies.
constraint_indexing:constraint_classification(political_schedule_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_schedule_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_schedule_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_schedule_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_schedule_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_schedule_pressure, TR),
    TR >= 0.70.

:- end_tests(political_schedule_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing over time. Schedule pressure extracts by compressing deliberation, but extraction is not maximal because some actors (executives, party leaders, organized interests with resources) retain capacity to participate. The upward trend (0.38→0.58 over 20-year interval) reflects acceleration of deadline-driven governance and reduction of substantive legislative review. Suppression (0.62): Moderate-high. Formal procedural barriers (suspension of rules, expedited passage) are high; informal political costs of delay are severe (delegitimation as obstructionist); but not total — determined minorities can still obstruct through floor filibuster and procedural delay tactics. Structural barriers to exit are severe but not absolute. Theater ratio (0.65): Moderate-high and increasing. Committee hearings, floor debate, and amendment markup processes persist as formal procedures but are increasingly disconnected from actual decision points. Decisions are predetermined by leadership; formal procedures serve primarily communicative/legitimation function. The rising theater ratio reflects increasing disconnect between procedural form and substantive function.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspective (executive/institutional) sees Rope: schedule pressure is a coordination mechanism that forces coherence and prevents endless obstruction. The trapped minority perspective sees Snare: no exit, no voice, pure extraction. The moderate perspective sees Tangled Rope: genuine coordination benefits mixed with significant extraction costs. The reform perspective sees Scaffold: alternative deliberative pathways with sunset potential. The natural law perspective sees Mountain: electoral cycles are immutable constraints — but this is a false summit, as the measured theater ratio and suppression data reveal that the constraint is institutional (procedure, custom, expectation) rather than physical. The key gap is between the beneficiary's experience (this works) and the victim's experience (this excludes me) — exactly the signature of a Tangled Rope where both coordination and extraction are real but asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent follows from power level, exit options, and structural relationship. Executive leadership (institutional power, arbitrage exit): low d → negative chi → benefits from constraint. Legislative minorities (powerless, trapped): high d → high f(d) → maximum experienced extraction. Career legislators (moderate power, constrained exit): mid-range d → moderate f(d) → mixed experience. The sigmoid f(d) amplifies the extraction experienced by trapped agents while compressing the burden on those with exit options. The beneficiary's experience of Rope (coordination) is accurate within their structural position; the victim's experience of Snare (extraction) is equally accurate within theirs. The perspectival gap is not a classification error but a sign that Tangled Rope is the appropriate type — both coordination and asymmetric extraction are structural.
 *
 * MANDATROPHY ANALYSIS:
 *   Schedule pressure resolves mandatrophy by showing that the constraint is legitimately hybrid: genuine coordination function exists (forcing prioritization, preventing indefinite obstruction, synchronizing action) alongside genuine asymmetric extraction (suppressing deliberation, excluding minority voice, degrading long-term planning). The classification resists collapse to pure extraction (Snare) because the beneficiaries genuinely experience this as solving a coordination problem. The classification resists collapse to pure coordination (Rope) because suppression is severe and asymmetrically distributed. The mandatrophy is resolved by accepting that the constraint is structurally both: it solves a real coordination problem (how to make timely decisions in a divided legislature) while extracting asymmetrically from those without power to control the schedule. The false summit (natural law mountain) is rejected because the measurement data shows that theater ratio and suppression are institutional features (procedural rules, political cost expectations, leadership gatekeeping) that could be altered, not immutable features of electoral cycles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberation_quality_measurement,
    'How is deliberation quality measured, and does schedule pressure reduce it or merely compress it temporally?',
    'Content analysis of legislative debate: linguistic markers of substantive engagement vs performative positioning; tracking of amendments and substantive floor modifications; comparison of bills pre-passage revision rates across time horizons',
    'If quality actually degrades: extraction mechanism confirmed. If quality is preserved but compressed: schedule pressure is coordination solution rather than extraction mechanism. Affects victim group classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_quality_measurement, empirical, 'Whether schedule pressure reduces deliberation quality or merely compresses it').

omega_variable(
    alternative_legislative_timeline_feasibility,
    'Could extended legislative calendars achieve equivalent policy outcomes at lower suppression cost?',
    'Comparative analysis of outcomes in legislative systems with different session lengths (US states, European parliaments); historical periods with different schedule pressure (pre-1920s US with longer sessions); experimental deliberative legislatures with extended timelines',
    'If extended timelines produce equivalent outcomes: schedule pressure is pure extraction mechanism (Snare). If extended timelines degrade outcomes: schedule pressure is genuine coordination necessity (Rope). If mixed: confirms Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legislative_timeline_feasibility, empirical, 'Whether alternative legislative timelines could achieve equivalent outcomes').

omega_variable(
    executive_schedule_autonomy,
    'Does executive leadership genuinely control the legislative schedule, or is the executive also constrained by implicit pressure cycles (media, campaign, financial market expectations)?',
    'Tracking of executive schedule proposals vs actual passage timing; analysis of emergency declarations and deadline-driven legislation; comparison of schedule pressure across legislative and executive branches',
    'If executives control schedule: extraction runs from executive to other agents (confirms leadership beneficiary classification). If schedule is autonomous constraint: all agents including executives are victims (pyramid collapses, Snare from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(executive_schedule_autonomy, empirical, 'Whether executives genuinely control or are also constrained by schedule pressure').

omega_variable(
    suppression_mechanism_breakdown,
    'Is measured suppression primarily procedural (formal rules limiting debate) or internalized (actors self-censor due to political cost expectations)?',
    'Tracking of formal procedure violations and suspensions vs self-imposed debate constraints; interviews and statements from legislative staff about decision calculus; historical comparison of suppression across periods with different political polarization',
    'If primarily procedural: suppression can be reduced through rule changes. If primarily internalized: deeper political realignment required. Affects reform pathway credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_breakdown, empirical, 'Whether suppression is primarily procedural or internalized political cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_schedule_pressure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psp_tr_t0, political_schedule_pressure, theater_ratio, 0, 0.45).
narrative_ontology:measurement(psp_tr_t10, political_schedule_pressure, theater_ratio, 10, 0.55).
narrative_ontology:measurement(psp_tr_t20, political_schedule_pressure, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(psp_be_t0, political_schedule_pressure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(psp_be_t10, political_schedule_pressure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(psp_be_t20, political_schedule_pressure, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_schedule_pressure, enforcement_mechanism).
narrative_ontology:affects_constraint(political_schedule_pressure, legislative_minority_exclusion).
narrative_ontology:affects_constraint(political_schedule_pressure, executive_power_concentration).
narrative_ontology:affects_constraint(political_schedule_pressure, long_term_policy_planning_horizon).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_schedule_pressure, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
