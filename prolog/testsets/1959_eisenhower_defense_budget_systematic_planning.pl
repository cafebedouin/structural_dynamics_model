% ============================================================================
% CONSTRAINT STORY: 1959_eisenhower_defense_budget_systematic_planning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1959_eisenhower_defense_budget_systematic_planning, []).

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
 *   constraint_id: 1959_eisenhower_defense_budget_systematic_planning
 *   human_readable: Eisenhower Defense Budget Systematic Planning Constraint
 *   domain: economics/military/governance
 *
 * SUMMARY:
 *   The Eisenhower administration's institutionalization of systematic
 *   defense budgeting represents a deliberate shift from episodic,
 *   crisis-driven military spending to a planned, rationalized process.
 *   Eisenhower's 'New Look' doctrine embedded budget discipline directly into
 *   strategic thinking, treating defense spending as a continuous, managed
 *   process prioritizing efficiency and long-term economic stability over
 *   rapid reaction to perceived threat surges. This constraint creates a
 *   structural tension: it benefits civilian government (protected budgets,
 *   predictable resource allocation) and long-term economic stability but
 *   imposes costs on military command's ability to respond rapidly to threat
 *   perception spikes. The constraint exhibits coordination function
 *   (systematic planning reduces waste and duplication) alongside asymmetric
 *   extraction (military readiness doctrine is subordinated to fiscal
 *   discipline). The theater ratio (0.35) reflects that while the planning
 *   process has real coordinative function, it also includes significant
 *   ritual justification and bureaucratic performance—budget hearings, threat
 *   assessments, and platform rationalization reviews generate substantial
 *   procedural overhead relative to the actual decision variance.
 *
 * KEY AGENTS:
 *   - Military Command and Services: Primary victim (powerless/trapped) — faces hard ceiling on defense spending regardless of threat perception; cannot exit the budgetary constraint; bears full cost of deferred modernization and force structure limitations
 *   - Treasury Department and Civilian Government: Primary beneficiary (institutional/arbitrage) — benefits from predictable defense spending, protected civilian budgets, and prevention of fiscal hemorrhaging during military-driven crises
 *   - Defense Industrial Base and Contractors: Secondary victim/beneficiary (moderate/constrained) — gains from elimination of wasteful duplication and platform standardization but bears suppressed profit margins relative to fear-driven procurement surges
 *   - Alliance Partners (NATO, Japan, Korea): Secondary actor (organized/constrained) — see systematic budget as commitment device demonstrating deterrent resolve; but constrained during acute threat perception escalation
 *   - Congress: Institutional enforcer (institutional/arbitrage) — maintains the budgetary discipline framework; can adjust parameters but not escape the constraint logic
 *   - Strategic Planning Institutions: Institutional maintainer (institutional/arbitrage) — perpetuates systematic budgeting framework through institutional inertia and career incentives in planning bureaucracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1959_eisenhower_defense_budget_systematic_planning, 0.52).
domain_priors:suppression_score(1959_eisenhower_defense_budget_systematic_planning, 0.48).
domain_priors:theater_ratio(1959_eisenhower_defense_budget_systematic_planning, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1959_eisenhower_defense_budget_systematic_planning, extractiveness, 0.52).
narrative_ontology:constraint_metric(1959_eisenhower_defense_budget_systematic_planning, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1959_eisenhower_defense_budget_systematic_planning, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1959_eisenhower_defense_budget_systematic_planning, tangled_rope).
narrative_ontology:human_readable(1959_eisenhower_defense_budget_systematic_planning, "Eisenhower Defense Budget Systematic Planning Constraint").
narrative_ontology:topic_domain(1959_eisenhower_defense_budget_systematic_planning, "economics/military/governance").

domain_priors:requires_active_enforcement(1959_eisenhower_defense_budget_systematic_planning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1959_eisenhower_defense_budget_systematic_planning, long_term_economic_stability).
narrative_ontology:constraint_beneficiary(1959_eisenhower_defense_budget_systematic_planning, civilian_budget_protection).
narrative_ontology:constraint_beneficiary(1959_eisenhower_defense_budget_systematic_planning, industrial_efficiency).
narrative_ontology:constraint_victim(1959_eisenhower_defense_budget_systematic_planning, rapid_military_response_capacity).
narrative_ontology:constraint_victim(1959_eisenhower_defense_budget_systematic_planning, threat_response_flexibility).
narrative_ontology:constraint_victim(1959_eisenhower_defense_budget_systematic_planning, military_readiness_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILITARY COMMAND (SNARE) — Faces absolute upper bound on defense spending regardless of threat perception. Cannot exit the budgetary ceiling; bears full cost of deferred modernization and constrained force readiness during perceived threat escalation. Maximum experienced extraction from the military's perspective: the constraint directly suppresses their stated needs.
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENSE CONTRACTORS (TANGLED ROPE) — Constrained by predictable budget cycles (planning horizon benefits) but also suppressed relative to fear-driven procurement surges. Benefits from elimination of wasteful duplication and platform rationalization; bears costs of slower obsolescence replacement rates. Coordination function present (efficient platform management); asymmetric extraction present (systematic planning caps profit margins on crisis-response margins).
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY AND CIVILIAN GOVERNMENT (ROPE) — Benefits from predictable defense spending, protected civilian budgets, and prevention of crisis-driven fiscal hemorrhaging. Experiences the constraint as enabling coordination: systematic planning allows rational resource allocation across defense, infrastructure, education, and social programs. Net beneficiary with full arbitrage option (can adjust spending parameters within the systematic framework).
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALLIANCE PARTNERS (SCAFFOLD) — See the systematic budget as a commitment device: predictable, planned military spending demonstrates deterrent resolve without the risk of crisis-driven overreaction. But faces constraint during acute threat perception (cannot escalate spending quickly). The coalition perspective sees a sunset: as strategic environment changes, the fixed budget envelope either updates via deliberate policy (no sunset) or becomes increasingly inadequate (effective sunset as allies diverge). Current status: constrained but stable; sunset depends on geopolitical drift.
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC PLANNING INSTITUTIONS (PITON) — The budgetary mechanism persists through institutional inertia: the 'New Look' doctrine and systematic planning were rational responses to nuclear-age uncertainty and fiscal pressure in the 1950s. By the Cold War's later phases, the constraint became partly theatrical—maintained because the alternative (ad-hoc crisis response) was politically costly, not because it optimally matched strategic needs. The planning process has degraded into ritual budget justification rather than genuine strategic evaluation.
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FISCAL REALITY (MOUNTAIN) — From a civilizational perspective, some constraint on total military spending is inherent to economic sustainability: resources allocated to defense are unavailable for other purposes, and unlimited defense spending is empirically impossible. This perspective risks naturalizing the specific Eisenhower mechanism as inevitable, when in fact the systematic planning discipline is a contingent institutional choice. The false summit detection will flag this: the constraint appears natural but has identifiable beneficiaries (civilian government, economic stability advocates) who benefit from naturalizing it.
constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1959_eisenhower_defense_budget_systematic_planning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1959_eisenhower_defense_budget_systematic_planning, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1959_eisenhower_defense_budget_systematic_planning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1959_eisenhower_defense_budget_systematic_planning, TR),
    TR >= 0.70.

:- end_tests(1959_eisenhower_defense_budget_systematic_planning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward. The constraint begins with modest extractiveness (0.28) when introduced—genuine operational benefits from platform rationalization and efficiency gains are real. But over the decade, as threat perception rises (Korean War escalation, Soviet nuclear capability, Sputnik) and the military experiences the binding nature of the constraint, extractiveness increases. The final value (0.52) reflects that the constraint is now materially suppressing military readiness expansion even as threats (from the military's perspective) are escalating. This is not pure extraction—the beneficiary group (civilian government, economic stability) is genuinely coordinating resource allocation—but it is asymmetric: the extraction flow runs from military command toward civilian priorities. Suppression (0.48): Moderate. The budget ceiling is not absolute (emergency supplementals and reprogramming authority exist), but it is binding in normal operations. Military command cannot freely allocate resources to rapid threat response; must work within predictable envelope. The suppression is institutional rather than coercive—enforced through budget authority and congressional appropriations, not force. Theater ratio (0.35): Moderate-low, trending upward. The systematic planning process has genuine coordinative function—eliminating duplicate platforms, rationalizing logistics, improving long-term industrial efficiency. But significant procedural theater exists: budget justifications, threat assessments, platform reviews generate substantial overhead. The theater increases over time as the constraint becomes institutionalized and ritualized—the planning process generates more justificatory narrative relative to actual decision variance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. Military command (powerless/trapped) sees pure extraction (Snare)—a hard ceiling preventing rapid threat response. Treasury (institutional/arbitrage) sees pure coordination (Rope)—rational resource allocation preventing fiscal crisis. Defense contractors (moderate/constrained) see hybrid (Tangled Rope)—benefits from efficiency and platform standardization but constrained profit margins. Alliance partners (organized/constrained) see temporary coordination device (Scaffold)—commitment mechanism with sunset if geopolitical environment changes. Strategic planning institutions (institutional/arbitrage) see degraded ritual (Piton)—process persists through inertia, no longer optimally matching strategic needs. The analytical observer risks seeing fiscal immutability (Mountain)—but structural data reveals this as false summit: civilian government benefits from naturalizing military constraint as inevitable fiscal reality. The perspectival gap here is not a gap in perception quality—military command is not misperceiving the constraint—but a genuine disagreement about the constraint's function. Is it extraction or coordination? Both interpretations are structurally valid from their positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declaration drives directionality computation. Military command and military readiness doctrine are declared victims—they bear costs of suppressed force expansion. Civilian budgets, economic stability, and industrial efficiency are declared beneficiaries—they receive protection or benefit from rationalization. The suppression metric (0.48) reflects that while the ceiling is binding in practice, emergency supplementals and reprogramming authority create a narrow flexibility band. The extractiveness metric (0.52) reflects mixed coordination (real efficiency gains from platform standardization) and extraction (asymmetric resource allocation favoring civilian priorities over military expansion). The constraint requires active enforcement (congressional budget authority, Pentagon budget discipline, congressional oversight) to maintain, confirming Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope is the correct classification precisely because it exhibits both genuine coordination function AND asymmetric extraction. The military command's Snare perspective is not the 'true' classification—it is the experience of the victim. The treasury's Rope perspective is not the 'true' classification—it is the experience of the beneficiary. The Tangled Rope classification from the moderate actor's perspective is the structural reality: the constraint simultaneously coordinates resource allocation (eliminates duplication, enables long-term industrial planning, protects civilian budgets from crisis-driven raiding) and extracts military flexibility (subordinates readiness doctrine to fiscal discipline, suppresses rapid response capability). The mandatrophy is resolved by recognizing that the constraint's function is BOTH—it is extractive coordination, not pure coordination mislabeled as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_vs_objective_threat,
    'How much of the military command''s perceived need for higher spending reflects objective threat escalation versus internal organizational pressure for resource expansion?',
    'Comparison of military spending requests against contemporaneous threat assessments from independent intelligence; historical analysis of deterrent capability requirements during periods of similar objective threat',
    'If objective threats high: military constraints are extractive (Snare classification correct). If organizational pressure dominant: the constraint may be beneficial coordination (Rope classification gains support). If mixed: Tangled Rope classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_vs_objective_threat, empirical, 'Objective versus organizational threat perception').

omega_variable(
    systematic_planning_flexibility_band,
    'What range of contingency spending (emergency supplementals, reprogramming authority) actually exists within the systematic framework versus the stated rigid ceiling?',
    'Audit of emergency supplemental appropriations, reprogramming authorities, and contingency fund access during Eisenhower era; comparison of ''formal ceiling'' versus ''actual maximum spending'' across budget cycles',
    'If flexibility band is wide: suppression metric is overstated (0.48 should be lower), constraint is more Rope-like than Snare-like. If band is narrow: suppression metric confirmed, constraint is genuinely restrictive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systematic_planning_flexibility_band, empirical, 'Actual flexibility within systematic budget ceiling').

omega_variable(
    industrial_efficiency_gains_magnitude,
    'How much real efficiency improvement and waste reduction resulted from systematic platform rationalization versus how much was marketing/myth?',
    'Cost-per-unit analysis pre and post systematic planning; comparison of platform consolidation outcomes versus fragmented procurement in defense industrial base; adjustment for inflation and technical capability improvements',
    'If large efficiency gains: beneficiary group (industrial base, treasury) genuinely benefits from coordination function—strengthens Rope/Tangled Rope classification. If minimal: efficiency claim is theater masking extraction—Snare classification gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrial_efficiency_gains_magnitude, empirical, 'Real versus claimed industrial efficiency gains').

omega_variable(
    crisis_response_cost_of_constraint,
    'During actual crises (Korean War escalation, Berlin crisis, Sputnik response), how much did the systematic budget constraint materially impair military response capability versus how much was symbolic constraint with practical workarounds?',
    'Historical analysis of actual budget reallocation and emergency supplementals during crisis periods; comparison of stated readiness versus achieved operational capability; interviews/declassified documents on constraint experience',
    'If large material impairment: suppression is real, Snare classification for military command is correct. If workarounds available: suppression is overstated, constraint is more governance theater than hard ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_response_cost_of_constraint, empirical, 'Material impact of budget constraint during actual crises').

omega_variable(
    civilian_budget_protection_counterfactual,
    'Would civilian budgets have been smaller in the absence of the systematic defense budget constraint, or would systematic spending discipline have occurred regardless due to fiscal conservatism?',
    'Comparison with other post-war states that lacked systematic defense budgeting; analysis of civilian budget trends in alternative budget scenarios; counterfactual modeling of congressional spending behavior',
    'If constraint was causal: benefits to civilian budgets are real, beneficiary group genuinely captured. If budgeting discipline was overdetermined: constraint is less significant, coordination function claim is weakened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_budget_protection_counterfactual, conceptual, 'Causal role of systematic budgeting in civilian budget protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1959_eisenhower_defense_budget_systematic_planning, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eisen_tr_t0, 1959_eisenhower_defense_budget_systematic_planning, theater_ratio, 0, 0.22).
narrative_ontology:measurement(eisen_tr_t3, 1959_eisenhower_defense_budget_systematic_planning, theater_ratio, 3, 0.28).
narrative_ontology:measurement(eisen_tr_t6, 1959_eisenhower_defense_budget_systematic_planning, theater_ratio, 6, 0.33).
narrative_ontology:measurement(eisen_tr_t10, 1959_eisenhower_defense_budget_systematic_planning, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(eisen_be_t0, 1959_eisenhower_defense_budget_systematic_planning, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eisen_be_t3, 1959_eisenhower_defense_budget_systematic_planning, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(eisen_be_t6, 1959_eisenhower_defense_budget_systematic_planning, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(eisen_be_t10, 1959_eisenhower_defense_budget_systematic_planning, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1959_eisenhower_defense_budget_systematic_planning, resource_allocation).
narrative_ontology:affects_constraint(1959_eisenhower_defense_budget_systematic_planning, cold_war_military_industrial_complex).
narrative_ontology:affects_constraint(1959_eisenhower_defense_budget_systematic_planning, nuclear_deterrence_doctrine_credibility).
narrative_ontology:affects_constraint(1959_eisenhower_defense_budget_systematic_planning, defense_innovation_rate_constraint).

% DUAL FORMULATION NOTE:
% The systematic budgeting constraint is upstream of specific military capability constraints and doctrinal commitments. The budget discipline shapes which defense innovations, force structures, and strategic commitments are feasible. Separate stories model the downstream consequences (nuclear deterrence, industrial innovation rates) with distinct ε values reflecting different observable domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1959_eisenhower_defense_budget_systematic_planning, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
