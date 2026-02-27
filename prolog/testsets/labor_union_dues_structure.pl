% ============================================================================
% CONSTRAINT STORY: labor_union_dues_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_union_dues_structure, []).

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
 *   constraint_id: labor_union_dues_structure
 *   human_readable: Mandatory Union Dues-for-Grievance Mechanism
 *   domain: economic/political
 *
 * SUMMARY:
 *   The mandatory union dues structure creates a structural tension between
 *   legitimate collective protection (grievance handling, legal defense, wage
 *   negotiation) and extractive enforcement mechanisms that trap workers
 *   without exit options. The constraint exhibits markedly different
 *   classifications across perspectives depending on the agent's structural
 *   position. A worker with an active grievance perceives genuine
 *   coordination and protection (Tangled Rope); a worker paying dues who
 *   never needs grievance services perceives pure extraction (Snare); union
 *   leadership perceives coordination that enables their bargaining power
 *   (Rope); multinational employers perceive a declining extraction mechanism
 *   as automation reduces union power (Scaffold); and analytical observers
 *   risk naturalizing mandatory dues as an immutable solution to collective
 *   action (Mountain). The theater_ratio (0.48) reflects moderate
 *   performative content: grievance handling includes genuine dispute
 *   resolution, but union dues collection is partly ritualistic
 *   (seniority-based allocation, bureaucratic grievance denial, enforcement
 *   against whistleblowers). Extractiveness (0.52) reflects moderate
 *   asymmetry: dues provide real benefits (job protection, wage floors) but
 *   also fund union apparatus that resists internal reform and may extract
 *   rents through corruption or misaligned incentives.
 *
 * KEY AGENTS:
 *   - Union Leadership: Primary beneficiary (organized/mobile) — controls dues collection, allocates grievance resources, captures surplus through administrative apparatus
 *   - Worker With Grievance: Secondary beneficiary (moderate/constrained) — receives real protection from employment coercion; benefits from collective bargaining power
 *   - Dues-Paying Worker Without Grievance: Primary victim (powerless/trapped) — pays mandatory dues without accessing grievance mechanism; cannot exit without employment loss
 *   - Non-Union Competitor: Secondary victim (powerless/trapped) — experiences union wage floors as extracted externality; trapped by market structure
 *   - Multinational Employer: Powerful actor (powerful/mobile) — perceives union dues as declining constraint as offshoring/automation reduce union leverage; can exit through relocation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing mandatory dues as inevitable to solve free-rider problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_union_dues_structure, 0.52).
domain_priors:suppression_score(labor_union_dues_structure, 0.65).
domain_priors:theater_ratio(labor_union_dues_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_union_dues_structure, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_union_dues_structure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_union_dues_structure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_union_dues_structure, tangled_rope).
narrative_ontology:human_readable(labor_union_dues_structure, "Mandatory Union Dues-for-Grievance Mechanism").
narrative_ontology:topic_domain(labor_union_dues_structure, "economic/political").

domain_priors:requires_active_enforcement(labor_union_dues_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_union_dues_structure, union_leadership).
narrative_ontology:constraint_beneficiary(labor_union_dues_structure, protected_workers).
narrative_ontology:constraint_victim(labor_union_dues_structure, free_riders).
narrative_ontology:constraint_victim(labor_union_dues_structure, non_union_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUES-PAYING WORKER WITHOUT GRIEVANCE (SNARE) — Worker with few transferable skills, bound to union shop by need for employment. Pays dues regularly but has no access to grievance mechanism without union authorization. Cannot exit without losing job; cannot modify terms. Extraction is coercive and lacks coordination benefit for this agent.
constraint_indexing:constraint_classification(labor_union_dues_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER WITH GRIEVANCE (TANGLED ROPE) — Worker who benefits from grievance mechanism (real protection against termination, wage theft, unsafe conditions). Pays dues and receives service. Constrained exit: can move to non-union shop but sacrifices protection. Mixed experience: significant benefit but also structural dependency on union leadership.
constraint_indexing:constraint_classification(labor_union_dues_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNION LEADERSHIP (ROPE) — Primary beneficiary. Collects dues, allocates resources, controls grievance process. Mobile exit (can restructure union, change dues formulas). Sees constraint as pure coordination mechanism: dues enable collective bargaining, strike funds, legal defense. Net positive experience; extraction flows toward this agent.
constraint_indexing:constraint_classification(labor_union_dues_structure, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-UNION COMPETITOR (SNARE) — Small non-union firm in union-dominated industry. Trapped by market structure: union firms set wages/conditions, non-union firms cannot undercut labor costs without triggering strikes or boycotts. Experiences union dues structure as extraction mechanism that inflates competitor wages, raising market floor. No genuine exit.
constraint_indexing:constraint_classification(labor_union_dues_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: GLOBAL WORKFORCE COORDINATOR / MULTINATIONAL EMPLOYER (SCAFFOLD) — Large firm with leverage to negotiate union contracts. Sees mandatory dues as temporary coordination mechanism with sunset logic: as offshoring and automation mature, union bargaining power declines. Can exit by relocating production. Experiences extraction as constrained and declining; sees sunset as built-in as union density falls.
constraint_indexing:constraint_classification(labor_union_dues_structure, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, mandatory dues solve an immutable collective action problem: individual workers cannot negotiate with large employers without coordination; free-riding destroys the collective. The dues mechanism is viewed as inevitable to overcoming rational defection. However, this perspective risks naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(labor_union_dues_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_union_dues_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_union_dues_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_union_dues_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_union_dues_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(labor_union_dues_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the dual nature of the constraint. Base extraction is the mandatory nature of dues collection — workers cannot opt out without losing employment. However, extraction is not maximal because a significant fraction of dues genuinely funds grievance mechanisms that protect workers from termination and wage theft. The extractiveness has increased over the interval (from 0.35 to 0.52) as union administrations have grown relative to grievance-handling capacity, and as union leadership has used dues collection to fund political activities diverging from worker interests. Suppression (0.65): Moderate-high. Workers face significant barriers to exit: employment termination for union non-membership in union security agreements, inability to work in unionized shops without paying dues, and social pressure within shop floors. Suppression is not total because some workers can exit to non-union shops (though often at wage penalty), and some jurisdictions (right-to-work states) have reduced suppression. Theater ratio (0.48): Moderate. Genuine grievance handling occurs, but union bureaucracy introduces performative elements: grievance denial based on seniority rather than merit, dues collection rituals that feel disconnected from direct benefit, and union elections/governance structures that serve administrative visibility rather than worker agency.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. Union leadership sees a pure coordination mechanism (Rope) — dues enable collective bargaining and legal defense. A worker receiving grievance services sees a mixed mechanism (Tangled Rope) — real protection but also structured dependency on union apparatus that controls access. A worker paying dues without ever needing grievance services sees pure extraction (Snare) — coercive payment for a service they do not use, with no alternative. A non-union competitor sees an externality-imposing constraint (Snare) — union-set wages and benefits inflate market floor, trapping non-union firms. A multinational can see the constraint as temporary (Scaffold) — union density is declining, automation is rising, so the extraction mechanism has a built-in sunset. The analytical observer risks seeing natural law (Mountain) — free-riding would destroy any voluntary system — but this naturalization conceals that many systems operate with voluntary or opt-in participation (Germany, Sweden), suggesting the mountain classification is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position. Union leadership: beneficiary with mobile exit → d ≈ 0.10 → low/negative f(d) → they experience low extraction. Dues-paying worker without grievance: victim with trapped exit → d ≈ 0.92 → high f(d) → they experience high extraction. Worker with grievance: beneficiary AND victim with constrained exit → d ≈ 0.55 → moderate f(d) → they experience moderate extraction. Non-union competitor: victim with trapped exit (cannot escape market structure) → d ≈ 0.88 → high f(d) → high extraction. Multinational employer: powerful with mobile exit (can offshore) → d ≈ 0.45 → moderate f(d) → they experience moderate/declining extraction. These directionality values explain why the same base properties (ε=0.52, suppression=0.65) produce different classification types from different perspectives: the engine's sigmoid f(d) transforms beneficiary/victim relationships and exit options into differential experienced extractiveness, which then determines classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that mandatory dues solve a real collective action problem (free-rider defection) but also enable rent extraction by union leadership. The classification as Tangled Rope reflects this hybrid: genuine coordination (grievance handling, strike funds, legal defense) AND asymmetric extraction (dues collection that funds administrative apparatus, political spending, and bureaucratic gatekeeping of grievance access). The false summit (mountain perspective) naturalizes mandatory dues as inevitable to solving free-rider problems, but empirical comparison with voluntary/opt-in systems (Germany, Sweden) shows that the free-rider problem is not immutable — it is contingent on system design. The Scaffold perspective (multinational employer) shows that the constraint has built-in sunset logic as union density declines and automation reduces bargaining leverage. The Snare perspective (trapped worker) shows that the extractive component is severe for those who pay dues without benefiting from grievance services. The resolution: the constraint is genuinely Tangled Rope at the analytical level, with legitimate coordination function and asymmetric extraction both present. No single perspective 'wins' — the heterogeneity of experiences is structural to the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_mechanism_necessity,
    'Would the grievance mechanism function with voluntary dues (opt-in contributions)?',
    'Comparative analysis of voluntary union systems (Germany co-determination boards, Sweden union presence in firms), empirical measurement of participation rates and enforcement efficacy',
    'If voluntary system works: mandatory dues are extraction, not coordination. If voluntary fails: dues are necessary coordination mechanism (Rope from all perspectives). If partial success: Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grievance_mechanism_necessity, empirical, 'Whether grievance mechanism requires mandatory funding').

omega_variable(
    dues_allocation_opacity,
    'How much of collected dues goes to grievance/legal services vs administrative overhead, political contributions, and leadership salaries?',
    'Financial audit of union accounting; comparison of grievance handling costs to dues collected; longitudinal tracking of allocation ratios',
    'If grievance services consume >70% of dues: Rope classification justified. If <40%: Snare classification from worker perspective justified. If 40-70%: Tangled Rope classification supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dues_allocation_opacity, empirical, 'Transparency of dues allocation').

omega_variable(
    free_rider_capacity,
    'What percentage of union benefits (workplace safety standards, wage floors, grievance precedent) accrue to non-paying workers in non-union shops?',
    'Comparative wage analysis between union and non-union shops in same industry; measurement of safety standard adoption rates; survey of non-union workers'' awareness of union-established norms',
    'If high free-riding (>40%): justifies suppression gate and mandatory dues. If low (<20%): benefits are excludable and voluntary system becomes feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_capacity, empirical, 'Scale of free-rider problem').

omega_variable(
    worker_exit_feasibility,
    'In tight labor markets, can workers realistically switch to non-union employment or negotiate individual exit from union shop membership?',
    'Labor market transition data; measurement of wage penalty for union-to-non-union switches; policy variation experiments (right-to-work vs union security laws)',
    'If exit is realistic (low wage penalty): exit_options upgrade to ''mobile'' and snare classification becomes less defensible. If exit is costly (>15% wage penalty): trapped classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_exit_feasibility, empirical, 'Feasibility of worker exit from union dues').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_union_dues_structure, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labor_union_tr_t0, labor_union_dues_structure, theater_ratio, 0, 0.32).
narrative_ontology:measurement(labor_union_tr_t20, labor_union_dues_structure, theater_ratio, 20, 0.4).
narrative_ontology:measurement(labor_union_tr_t40, labor_union_dues_structure, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(labor_union_be_t0, labor_union_dues_structure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(labor_union_be_t20, labor_union_dues_structure, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(labor_union_be_t40, labor_union_dues_structure, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_union_dues_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(labor_union_dues_structure, right_to_work_state_dynamics).
narrative_ontology:affects_constraint(labor_union_dues_structure, union_density_decline).
narrative_ontology:affects_constraint(labor_union_dues_structure, workplace_safety_standards).

% DUAL FORMULATION NOTE:
% The mandatory dues structure can be decomposed into two related constraints: (1) the collective action problem of funding grievance mechanisms (Rope/coordination) and (2) the extraction of surplus through union apparatus (Snare/extraction). These are linked: the coordination function justifies mandatory dues, but the extraction emerges from how leadership allocates those dues. Both perspectives are necessary to capture the full structural picture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_union_dues_structure, powerless, 0.92).
constraint_indexing:directionality_override(labor_union_dues_structure, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
