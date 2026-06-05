% ============================================================================
% CONSTRAINT STORY: 2001_bush_Emergency_appropriations_40billion_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_2001_bush_Emergency_appropriations_40billion_mobilization, []).

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
 *   constraint_id: 2001_bush_Emergency_appropriations_40billion_mobilization
 *   human_readable: $40 Billion Emergency Appropriation (2001)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The $40 billion emergency appropriation enacted by Congress in response
 *   to the September 11, 2001 attacks represents a structural pivot point in
 *   fiscal governance: the deliberate, transparent bypass of normal
 *   appropriations process under crisis conditions. The constraint exhibits
 *   the full DR classification range because it simultaneously solves a
 *   coordination problem (rapid federal response to catastrophic damage),
 *   centralizes executive authority (military mobilization), suspends
 *   oversight mechanisms (budget committees lose review authority), and
 *   establishes a precedent that reshapes future crisis response. The
 *   constraint's extractiveness (0.38) reflects that genuine coordination
 *   benefits (reconstruction funding, military readiness) coexist with
 *   asymmetric loss of fiscal oversight authority and precedent risk. Theater
 *   ratio (0.62) indicates that the emergency appropriations process
 *   maintains Congressional legitimacy through continued procedural activity
 *   (committee meetings, floor votes) while actual allocative authority
 *   migrates to executive branch during crisis window. The constraint
 *   demonstrates how constitutional emergency powers, when invoked, create a
 *   structural gap between formal authority (Congress) and operational
 *   authority (Executive) that persists long after the acute crisis phase.
 *
 * KEY AGENTS:
 *   - Executive Branch Military Command: Primary beneficiary (institutional/arbitrage) — gains rapid mobilization authority and budget flexibility during crisis period
 *   - Affected Communities: Secondary beneficiary (moderate/constrained) — receive federal reconstruction funding but subject to federal conditions and coordination requirements
 *   - Congressional Budget Committees: Primary victim (powerless/trapped) — normal review authority suspended by emergency declaration; cannot exit (opposing emergency relief politically impossible)
 *   - Congress (as Institution): Organized victim (organized/constrained) — delegates its own authority to emergency framework; constrained by political pressure and constitutional emergency doctrine
 *   - State and Local Governments: Constrained beneficiaries (moderate/constrained) — receive federal funds but lose fiscal autonomy during crisis period
 *   - Civil Society Watchdogs: Organized reformers (organized/constrained) — observe constraint as temporary (scaffold view); constrain executive overreach through subsequent legislation and monitoring
 *   - Normal Appropriations Process: Institutional actor (institutional/arbitrage) — experiences functional degradation (piton view); maintained through inertia despite reduced authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent emergency authority as constitutional inevitability (false summit mountain view)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(2001_bush_Emergency_appropriations_40billion_mobilization, 0.38).
domain_priors:suppression_score(2001_bush_Emergency_appropriations_40billion_mobilization, 0.48).
domain_priors:theater_ratio(2001_bush_Emergency_appropriations_40billion_mobilization, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(2001_bush_Emergency_appropriations_40billion_mobilization, extractiveness, 0.38).
narrative_ontology:constraint_metric(2001_bush_Emergency_appropriations_40billion_mobilization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(2001_bush_Emergency_appropriations_40billion_mobilization, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(2001_bush_Emergency_appropriations_40billion_mobilization, tangled_rope).
narrative_ontology:human_readable(2001_bush_Emergency_appropriations_40billion_mobilization, "$40 Billion Emergency Appropriation (2001)").
narrative_ontology:topic_domain(2001_bush_Emergency_appropriations_40billion_mobilization, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(2001_bush_Emergency_appropriations_40billion_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(2001_bush_Emergency_appropriations_40billion_mobilization, executive_branch_military).
narrative_ontology:constraint_beneficiary(2001_bush_Emergency_appropriations_40billion_mobilization, affected_communities_reconstruction).
narrative_ontology:constraint_victim(2001_bush_Emergency_appropriations_40billion_mobilization, normal_appropriations_process).
narrative_ontology:constraint_victim(2001_bush_Emergency_appropriations_40billion_mobilization, fiscal_oversight_authority).
narrative_ontology:constraint_victim(2001_bush_Emergency_appropriations_40billion_mobilization, future_budget_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISCAL OVERSIGHT AUTHORITY (SNARE) — Congressional budget committees and GAO face structural inability to exercise normal review authority during crisis declaration. Emergency appropriations bypass standard review cycles. Trapped by constitutional authority (Congress itself declared emergency), trapped by political pressure (opposing emergency relief is electorally catastrophic), trapped by information asymmetry (executive claims about military needs unverifiable during mobilization). Zero exit options. Maximum experienced extraction of oversight authority.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE AND LOCAL GOVERNMENTS (TANGLED ROPE) — Benefit from federal reconstruction funding (coordination function: rapid disaster recovery), but constrained by federal spending conditions, audit requirements, and loss of local appropriations authority during crisis period. Can exit through refusing federal funds (politically impossible), can exit through state-level appropriations (insufficient scale). Moderate extraction: genuine benefit exists alongside asymmetric loss of fiscal autonomy.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE BRANCH MILITARY COMMAND (ROPE) — Primary beneficiary. Experiences constraint as pure coordination: emergency authority enables rapid mobilization without normal budgetary delays. Can arbitrage: shift funds between accounts, accelerate procurement timelines, consolidate authorities. Extraction runs toward this agent. Net positive experienced benefit — low effective extraction because institutional power and exit options are high.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESS (AS INSTITUTION) (TANGLED ROPE) — Congress delegates emergency authority to itself (tautological benefit) while losing the ability to coordinate normal oversight processes. Organized agents (committees, caucuses) can constrain executive overreach through subsequent legislation, but during the crisis window, Congress experiences its own authority as extracted — spent on emergency declaration rather than available for deliberate appropriation. Theater ratio high: ceremonial emergency sessions create performative legitimacy without substantive debate. Active enforcement required (budget rules suspended). Moderate extraction because Congress retains long-term authority to restructure the emergency framework.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITIONS AND CIVIL SOCIETY (SCAFFOLD) — Organized civil society observers (audit groups, budget watchdogs, transparency advocates) see emergency appropriations as a temporary coordination failure with built-in sunset: emergency declarations have constitutional time limits, subsequent Congresses retain authority to restructure budget rules, and crisis-driven centralization creates coalitional incentives for decentralized accountability mechanisms post-crisis. Theater ratio decline visible as emergency period ends and normal appropriations process is (partially) restored. Scaffold classification derives from sunset logic: the extractive asymmetry is temporary by constitutional design.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NORMAL APPROPRIATIONS PROCESS (PITON) — The formal budget process (committee review, markup sessions, floor debate, scoring rules) becomes substantially performative during emergency declaration. The process is maintained through institutional inertia — committees still meet, staff still prepare analyses — but the actual allocation decisions are made through emergency authority channels outside normal process. Theater ratio is high (0.62): the appropriations committees maintain legitimacy through continued procedural activity despite reduced functional authority. This is institutional degradation from continued process maintenance without corresponding decision authority.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL VIEW (MOUNTAIN) — From civilizational/universal perspective, emergency fiscal authority is a structural feature of constitutional separation of powers: no complex nation can survive if it must debate appropriations during existential crisis. Some bypass of normal process is inherent to crisis response. From this view, the constraint appears as a natural law of political organization. However, this perspective risks false summitry — it naturalizes what is actually a contingent institutional arrangement (when do we declare emergency? who decides? how long does it persist?). The 2001 appropriation demonstrates that natural-law language ('we have to act') masks distributed choices about crisis definition, authorization scope, and executive discretion limits.
constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(2001_bush_Emergency_appropriations_40billion_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(2001_bush_Emergency_appropriations_40billion_mobilization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(2001_bush_Emergency_appropriations_40billion_mobilization, TR),
    TR >= 0.70.

:- end_tests(2001_bush_Emergency_appropriations_40billion_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The appropriation delivers genuine coordination benefits — federal reconstruction funding and military mobilization capacity that markets cannot provide at crisis speed. However, extractiveness rises from 0.15 to 0.38 across the interval, indicating that the initial crisis response (time 0) becomes entrenched as permanent budget authority (time 12). The rise reflects that emergency fiscal authority was not sunset but incorporated into ongoing budgetary authority, converting temporary coordination into persistent extraction mechanism. Suppression (0.48): Moderate. Multiple barriers prevent normal appropriations authority: constitutional emergency doctrine prohibits floor debate on appropriations during declared emergency; political cost of opposing emergency relief suppresses opposition; information asymmetry (military claims unverifiable during crisis); time compression (must vote within days). However, suppression is not total — subsequent Congresses can legislatively restructure emergency authority, and civil society retains monitoring capacity. Theater ratio (0.62): Moderate-high. Congressional emergency sessions maintain performative legitimacy (floor debate, committee markup) despite reduced functional authority over actual allocations. The ratio increases over time as emergency period extends and becomes normalized — the ceremonial aspects of appropriations process persist while real authority migrates to executive branch. The constraint requires active enforcement (rules suspension) to maintain the emergency authority framework against normal procedural defaults.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural event (emergency appropriation) produces divergent classifications from different observer positions. The executive branch sees coordination (Rope) — solving the problem of rapid mobilization. Civil society sees temporary coordination failure with institutional learning (Scaffold) — emergency authority creates incentives for reform. Congress sees its own authority fragmented between coordination benefit and extraction cost (Tangled Rope) — delegates to emergency framework while losing oversight. Budget committees see pure extraction (Snare) — suspended authority with no exit. The normal appropriations process sees its own degradation (Piton) — maintained through ceremonial activity despite reduced function. The civilizational observer risks false summitry (Mountain) — naturalizing emergency authority as inevitable feature of constitutional governance rather than contingent institutional choice. The perspectival gap reveals that 'emergency appropriation' is not a single phenomenon but a cluster of structural relationships differentiated by power, exit options, and temporal horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural relationship to extraction flow. Executive branch military (institutional/arbitrage) benefits maximally and can reallocate funds across accounts — low d (around 0.15), experiences negative effective extraction (benefits from constraint). Affected communities (moderate/constrained) receive reconstruction funding but face federal conditions and lose local appropriations authority — moderate d (around 0.55), experiences moderate extraction. Congressional committees (powerless/trapped) lose all review authority during emergency period — high d (around 0.92), experience maximum extraction of oversight function. The constraint's chi varies by perspective: military experiences low χ (beneficiary with arbitrage), committees experience high χ (victim with trapped exit), communities experience moderate χ (mixed beneficiary/victim with constrained exit). The piton classification of normal appropriations process derives from theater ratio (0.62) exceeding function threshold — the process continues through institutional inertia despite reduced allocative authority.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the coordination-vs-extraction ambiguity by showing that both are structurally present and correctly classified from different perspectives. The coordination function (reconstruction funding, military mobilization) is genuine — this is NOT false positive extraction. The extraction mechanism (loss of oversight authority, precedent entrenchment) is also genuine — this is NOT coordination masquerading as extraction. The tangled_rope classification captures this hybrid: the constraint genuinely coordinates rapid federal response while simultaneously extracting oversight authority from deliberative institutions. The mandatrophy is resolved by the perspectival decomposition: beneficiaries experience rope (pure coordination), victims experience snare (pure extraction), the institution experiences the hybrid directly (tangled_rope). The false summit risk is highest in the civilizational/analytical perspective, which may naturalize emergency authority as inherent to politics. This risk is flagged by the existence of strong beneficiary declarations in a constraint that claims universal applicability — the FSM engine would identify this as a candidate false summit if beneficiaries were listed on a mountain-classified constraint. The empirical validation comes from omega variables tracking emergency duration and precedent entrenchment: if emergency authority becomes permanent and is repeatedly invoked for non-catastrophic crises, the constraint has degraded from scaffold (temporary coordination bypass) to snare (permanent extraction mechanism), and the mountain view is revealed as false summitry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_duration_definition,
    'Where is the boundary between legitimate crisis-driven emergency authority and institutional capture of emergency powers as permanent governance mode?',
    'Historical tracking: compare duration of declared emergencies vs actual threat persistence; identify which emergency appropriations become permanent entitlements vs sunset; measure subsequent Congress reauthorization patterns',
    'If boundary is < 2 years: many legitimate crises misclassified as overreach. If boundary is > 10 years: emergency authority becomes de facto permanent, reclassifying from scaffold to snare. The 2001 appropriation''s descendants remain in effect decades later — evidence of capture rather than sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_duration_definition, empirical, 'Definition of emergency duration boundary').

omega_variable(
    executive_discretion_bounds,
    'How much of the $40 billion was actually constrained by Congressional conditions vs. available to executive branch for discretionary reallocation?',
    'GAO audit analysis; tracking of obligated vs. appropriated funds; measurement of executive authority to transfer funds between accounts and purposes; comparison of stated Congressional intent vs. actual allocation',
    'If discretion is < 20%: Congress retained meaningful control (tangled_rope confirmed). If discretion is > 50%: Congress delegated extraction authority to executive (upgrades snare perspective). If all funds were reappropriated for non-emergency purposes within 5 years: institutional capture confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(executive_discretion_bounds, empirical, 'Scope of executive discretion within emergency appropriation').

omega_variable(
    precedent_entrenchment_mechanism,
    'Did the 2001 emergency appropriation establish a precedent that reduced friction for subsequent emergency declarations and budget bypass?',
    'Comparative analysis: time from crisis onset to Congressional emergency vote before vs. after 2001; measurement of debate intensity (floor speeches, amendment counts) in emergency appropriations pre-2001 vs. post-2001; tracking of invocation frequency',
    'If precedent reduces friction: the constraint''s extractive mechanism persists through normalization (reclassifies to piton from organizational perspective — maintained through inertia rather than function). If subsequent emergencies face higher scrutiny: the scaffold sunset logic is operating (restraint through institutional learning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_entrenchment_mechanism, empirical, 'Whether emergency precedent became entrenchment mechanism').

omega_variable(
    fiscal_reconstruction_validity,
    'Did the $40 billion actually flow to stated reconstruction purposes, or was significant portion redirected to military expansion unrelated to response to attacks?',
    'Fund tracking analysis: obligated amounts by budget category; comparison of stated purpose (reconstruction, military response) vs. actual allocation; measurement of funds spent within first 2 years vs. shifted to ongoing military budgets; audit of affected communities'' actual infrastructure spending',
    'If > 80% reaches stated purpose: reconstruction coordination function is genuine (tangled_rope). If < 50% reaches stated purpose: the appropriation''s coordination framing masks military expansion extraction (upgrades snare, downgrades rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_reconstruction_validity, empirical, 'Validation of reconstruction vs. military spending split').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(2001_bush_Emergency_appropriations_40billion_mobilization, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, 2001_bush_Emergency_appropriations_40billion_mobilization, theater_ratio, 0, 0.48).
narrative_ontology:measurement(emerg_tr_t6, 2001_bush_Emergency_appropriations_40billion_mobilization, theater_ratio, 6, 0.58).
narrative_ontology:measurement(emerg_tr_t12, 2001_bush_Emergency_appropriations_40billion_mobilization, theater_ratio, 12, 0.62).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, 2001_bush_Emergency_appropriations_40billion_mobilization, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(emerg_be_t6, 2001_bush_Emergency_appropriations_40billion_mobilization, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(emerg_be_t12, 2001_bush_Emergency_appropriations_40billion_mobilization, base_extractiveness, 12, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(2001_bush_Emergency_appropriations_40billion_mobilization, resource_allocation).
narrative_ontology:affects_constraint(2001_bush_Emergency_appropriations_40billion_mobilization, military_industrial_permanent_mobilization).
narrative_ontology:affects_constraint(2001_bush_Emergency_appropriations_40billion_mobilization, congressional_war_powers_delegation).
narrative_ontology:affects_constraint(2001_bush_Emergency_appropriations_40billion_mobilization, executive_budget_discretion_expansion).

% DUAL FORMULATION NOTE:
% The $40 billion appropriation is upstream to subsequent military budget entitlements and executive discretion expansion. The initial emergency response (0-6 months, extractiveness 0.15) is distinct from the entrenched emergency authority (6+ months, extractiveness rising to 0.38). If decomposing, write separate constraint for (a) legitimate crisis coordination response, and (b) institutional capture of emergency mechanism. This story focuses on the hybrid (tangled_rope) that emerges when temporary measures become permanent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(2001_bush_Emergency_appropriations_40billion_mobilization, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
