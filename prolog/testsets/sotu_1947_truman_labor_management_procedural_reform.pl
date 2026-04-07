% ============================================================================
% CONSTRAINT STORY: sotu_1947_truman_labor_management_procedural_reform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1947_truman_labor_management_procedural_reform, []).

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
 *   constraint_id: sotu_1947_truman_labor_management_procedural_reform
 *   human_readable: Truman's 1947 Labor-Management Procedural Reform (Joint Responsibility Model)
 *   domain: labor_relations/institutional_governance
 *
 * SUMMARY:
 *   Truman's 1947 State of the Union address proposes a labor-management
 *   procedural reform framework in direct response to the 1946 strike surge,
 *   which generated pressure for punitive anti-labor legislation. The reform
 *   explicitly rejects Taft-Hartley-style restrictions on worker rights while
 *   assigning joint responsibility for negotiation failures to both
 *   management and unions. This creates a structural constraint that operates
 *   as both coordination mechanism and extraction system depending on the
 *   observer's position. Rank-and-file workers experience it as pure
 *   coordination (their rights are protected). Union leadership experiences
 *   it as tangled rope (benefits from legitimacy but constrained by
 *   procedural requirements). Management experiences it as snare (forced into
 *   good-faith negotiation). The federal infrastructure experiences it as
 *   scaffold (designed to prevent worse legislation). Over time, the
 *   machinery becomes ritualistically performative (piton). The constraint's
 *   genius is that it prevents worse outcomes for all parties while
 *   distributing the constraint burden asymmetrically — no party wins
 *   unilaterally, which is exactly why all parties accept it.
 *
 * KEY AGENTS:
 *   - Rank-and-File Workers: Primary beneficiary (powerless/mobile) — explicit protection of strike rights; freedom expanded by preventing punitive legislation
 *   - Labor Union Leadership: Mixed victim-beneficiary (organized/constrained) — gains legitimacy and recognition; bears reputational cost if procedural exhaustion fails
 *   - Capital / Management Interests: Primary victim (organized/constrained) — forced into good-faith negotiation and joint responsibility; cannot unilaterally shift accountability to labor
 *   - Federal Mediation Infrastructure: Institutional actor (institutional/arbitrage) — builds machinery designed to prevent legislative escalation; benefits from procedural legitimacy
 *   - Congress / Legislative Branch: Latent beneficiary (institutional/mobile) — pressure for punitive legislation is defused by procedural framework
 *   - Analytical Observer: Sees both genuine coordination and asymmetric constraint embedded in procedural parity language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1947_truman_labor_management_procedural_reform, 0.38).
domain_priors:suppression_score(sotu_1947_truman_labor_management_procedural_reform, 0.32).
domain_priors:theater_ratio(sotu_1947_truman_labor_management_procedural_reform, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1947_truman_labor_management_procedural_reform, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1947_truman_labor_management_procedural_reform, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(sotu_1947_truman_labor_management_procedural_reform, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1947_truman_labor_management_procedural_reform, tangled_rope).
narrative_ontology:human_readable(sotu_1947_truman_labor_management_procedural_reform, "Truman's 1947 Labor-Management Procedural Reform (Joint Responsibility Model)").
narrative_ontology:topic_domain(sotu_1947_truman_labor_management_procedural_reform, "labor_relations/institutional_governance").

domain_priors:requires_active_enforcement(sotu_1947_truman_labor_management_procedural_reform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1947_truman_labor_management_procedural_reform, rank_and_file_workers).
narrative_ontology:constraint_beneficiary(sotu_1947_truman_labor_management_procedural_reform, labor_unions).
narrative_ontology:constraint_beneficiary(sotu_1947_truman_labor_management_procedural_reform, management_discipline).
narrative_ontology:constraint_victim(sotu_1947_truman_labor_management_procedural_reform, punitive_legislation_preventability).
narrative_ontology:constraint_victim(sotu_1947_truman_labor_management_procedural_reform, negotiation_delays).
narrative_ontology:constraint_victim(sotu_1947_truman_labor_management_procedural_reform, capital_accumulation_during_disputes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE WORKER (ROPE) — The procedural reform explicitly protects strike rights and rejects punitive restrictions. Workers retain mobility (can organize, strike, or exit). The constraint solves a coordination problem: formalized dispute resolution prevents unilateral employer crackdown and legislative backlash that would have produced genuinely restrictive legislation (Taft-Hartley was passed despite Truman's resistance). This agent experiences the constraint as pure coordination — their freedom is actually expanded by the procedural framework.
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNION LEADERSHIP (TANGLED ROPE) — Unions benefit from legitimacy and recognition through procedural participation, but are constrained by the requirement to participate in joint responsibility mechanisms. Extractive component: the procedural framework delays strike action (cooling-off periods, negotiation sequences) and exposes union leadership to internal pressure if they exhaust procedures without delivering gains. Coordination component: the framework prevents worse legislation and establishes labor's seat at the governance table. Mixed experience — genuine benefit (recognition, prevention of Taft-Hartley) coupled with asymmetric constraint (unions bear reputational cost if procedures fail).
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL / MANAGEMENT INTERESTS (SNARE) — Management is trapped by joint responsibility language: the framework assigns equal blame for negotiation failures and explicitly rejects one-sided punitive restrictions on labor. Management cannot simply lock out workers and let emergency legislation do the work (as the 1946 recession created pressure for). The constraint forces management to negotiate in good faith and share responsibility for dispute outcomes. High extraction (from management's preferred position of shifting all accountability to labor) coupled with low alternatives — exit would require rejecting the entire procedural framework, which is the only thing preventing worse legislation.
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL MEDIATION INFRASTRUCTURE (SCAFFOLD) — The procedural framework creates institutional machinery (fact-finding boards, mediation services, cooling-off periods) designed as temporary structure to prevent legislative escalation. Theater is moderate (0.55) because the machinery is partly performative: fact-finding reports are often non-binding, cooling-off periods sometimes just delay inevitable conflict. But the framework has genuine sunset logic — it is explicitly designed to prevent worse legislation. As labor-management relations stabilize and norms of negotiation become embedded, the formal procedural requirements become less necessary. This perspective is optimistic about institutional capacity and believes the framework bridges to a more stable long-term equilibrium.
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STRIKE-PREVENTION RITUAL SYSTEM (PITON) — Over decades, cooling-off periods, fact-finding boards, and mandatory negotiation sequences become ritualistic. The machinery persists through institutional inertia even when the original threat (legislative backlash) fades. Workers learn the ritual, unions plan around cooling-off timelines, management prepares for the predictable sequence. The theater_ratio increases as the original coordinating function (preventing Taft-Hartley) recedes and the ritual itself becomes the constraint. Piton classification reflects that the original function has partially atrophied while institutional maintenance keeps the structure alive.
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint is a hybrid: genuine coordination function (formalizing dispute resolution prevents worse outcomes for all parties) coupled with asymmetric extraction (the procedural framework imposes greater constraints on labor's ability to strike immediately while management retains various lock-out options). The constraint works because both parties benefit from avoiding legislative escalation more than they benefit from unilateral victory in disputes. But the framework's asymmetries (management can threaten capital flight and relocation; labor is geographically rooted) embed power imbalances into the procedural structure itself.
constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1947_truman_labor_management_procedural_reform_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1947_truman_labor_management_procedural_reform, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1947_truman_labor_management_procedural_reform, TR),
    TR >= 0.70.

:- end_tests(sotu_1947_truman_labor_management_procedural_reform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The constraint imposes procedural costs on all parties (cooling-off periods, mandatory negotiation, fact-finding delays) but these are genuine coordination costs, not pure extraction. The asymmetry comes from management's ability to prepare for strikes during cooling-off periods while labor loses momentum. Extractiveness is lower than the 1946 crisis baseline (0.55) because the procedural framework reduces legislative backlash risk — avoiding Taft-Hartley is worth procedural costs. Suppression (0.32): Moderate-low. Workers retain mobility and strike rights. The constraint does not eliminate alternatives; it structures them sequentially. Suppression would be higher if strike rights were actually restricted, but Truman's reform explicitly protects them. Theater ratio (0.55): Moderate. The procedural machinery (fact-finding boards, cooling-off periods) has genuine function in 1947-1955 period (preventing escalation). But as labor-management norms stabilize, the machinery becomes more ritual than function. Theater increases over the interval as original threat recedes and institution persists through inertia.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the constraint's structural genius: the same framework appears as pure coordination (rope) to workers who gain protection, mixed constraint (tangled_rope) to union leadership, pure extraction (snare) to management, and temporary problem (scaffold) to federal mediators. No single perspective sees the full structure. The rank-and-file worker sees their freedom protected and misses the asymmetry of management's relocation threat. Management sees itself trapped and misses that procedural participation prevents worse legislation. Union leadership sees both coordination and constraint accurately. The analytical observer sees all four and recognizes the framework as redistribution of constraint burden away from legislation and into procedural structure — replacing external legal threat with internal procedural mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Rank-and-file workers (powerless/mobile): d ≈ 0.20. They are structurally mobile (can strike, organize, exit employment) and the procedural framework explicitly protects their rights. No beneficiary extraction from this agent; constraint actually subsidizes them by preventing worse legislation. Derived from: victim status removed (workers benefit from legislative prevention) + mobile exit → low d. Union leadership (organized/constrained): d ≈ 0.55. Constrained by procedural requirements (cooling-off delays, mandatory participation) but benefit from institutional recognition. Mixed exposure. Management (organized/constrained): d ≈ 0.75. Forced into joint responsibility (high extraction from preferred unilateral position) with limited exit (rejecting framework invites legislation). Trapped in the coordination system. Federal infrastructure (institutional/arbitrage): d ≈ 0.15. Benefits from procedural architecture; can arbitrage between labor and capital by controlling negotiation sequences. Low extraction experienced. Analytical observer: d ≈ 0.60. Sees coordination function (preventing escalation) as genuine but recognizes asymmetric embedding of capital's relocation threat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by operating as genuine coordination that happens to asymmetrically benefit labor. The extraction is not hidden — it is explicit in the joint responsibility language (management cannot deflect blame to labor unilaterally). The framework prevents a worse outcome (legislative restriction of strike rights) while simultaneously constraining management's ability to bypass labor with law-backed restrictions. All parties accept this because the alternative (escalating legislative backlash) is worse. Mandatrophy is not resolved by showing this is 'really' pure coordination or 'really' extraction — it is resolved by showing that the constraint's value is precisely that it prevents a worse constraint from emerging. The procedural framework is a Pareto improvement over the 1946-1947 alternative (Taft-Hartley or worse), which is why mandatrophy does not trigger: the constraint solves a real problem for all parties, even though it redistributes costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_backlash_counterfactual,
    'Would genuinely restrictive anti-labor legislation (e.g., Taft-Hartley without Truman''s modifications) have passed without the procedural reform framework?',
    'Comparative legislative history: analysis of Truman''s veto message and Congressional override dynamics; counterfactual modeling based on voting bloc pressure in 1946-1947 environment',
    'If yes: the procedural framework prevented worse extraction (classification confirmed). If no: the reform is less valuable than framed — legislation would have been restricted regardless (classification downgrades to rope, less extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_backlash_counterfactual, empirical, 'Whether legislative backlash was genuinely preventable via procedural reform').

omega_variable(
    management_negotiation_bad_faith_prevalence,
    'What fraction of labor-management disputes involved management refusal to negotiate in good faith vs. genuine negotiation breakdown?',
    'Analysis of fact-finding board reports, mediator assessments, and union grievance documentation; comparison of disputes by negotiation outcome categories',
    'If high bad faith: joint responsibility language is extractive cover (management avoids accountability). If low: framework reflects genuine coordination problem. Affects directionality and experienced chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_negotiation_bad_faith_prevalence, empirical, 'Prevalence of management bad-faith negotiation vs. genuine breakdown').

omega_variable(
    cooling_off_period_strike_deferral_effect,
    'Do cooling-off periods defer strikes (compressing them later at higher intensity) or genuinely reduce strike frequency?',
    'Time-series analysis of strike frequency and magnitude before/after procedural implementation; comparison of deferred vs. prevented strikes in fact-finding outcomes',
    'If deferral: suppression is higher than measured (delayed strikes with accumulated grievances). If prevention: suppression is real coordination benefit. Affects whether extractiveness should be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cooling_off_period_strike_deferral_effect, empirical, 'Whether cooling-off periods defer or prevent strikes').

omega_variable(
    union_leadership_cooptation_risk,
    'Does participation in federal procedural frameworks coopts union leadership into accepting suboptimal settlements?',
    'Comparison of settlement wages and conditions under procedural framework vs. immediate-strike outcomes; analysis of internal union democracy and rank-and-file pressure on leadership',
    'If yes: extractiveness is higher than measured (leadership captured). If no: framework preserves genuine negotiating power. Affects whether rope or tangled_rope is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_leadership_cooptation_risk, empirical, 'Whether procedural participation coopts union leadership').

omega_variable(
    capital_relocation_threat_embedding,
    'Does the procedural framework embed capital''s relocation threat as an asymmetric constraint on negotiation, even if not explicitly invoked?',
    'Analysis of corporate relocation patterns during disputes; documentation of implicit threats in negotiation records; comparison of settlement outcomes when firms have/lack relocation options',
    'If yes: management has hidden exit option (arbitrage), creating structural asymmetry beneath procedural parity language. If no: framework genuinely equalizes positions. Affects directionality and whether management is truly trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_relocation_threat_embedding, conceptual, 'Whether capital relocation threat is embedded in procedural constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1947_truman_labor_management_procedural_reform, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(truman47_tr_t0, sotu_1947_truman_labor_management_procedural_reform, theater_ratio, 0, 0.35).
narrative_ontology:measurement(truman47_tr_t5, sotu_1947_truman_labor_management_procedural_reform, theater_ratio, 5, 0.48).
narrative_ontology:measurement(truman47_tr_t10, sotu_1947_truman_labor_management_procedural_reform, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(truman47_be_t0, sotu_1947_truman_labor_management_procedural_reform, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(truman47_be_t5, sotu_1947_truman_labor_management_procedural_reform, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(truman47_be_t10, sotu_1947_truman_labor_management_procedural_reform, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1947_truman_labor_management_procedural_reform, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1947_truman_labor_management_procedural_reform, taft_hartley_act_restrictiveness).
narrative_ontology:affects_constraint(sotu_1947_truman_labor_management_procedural_reform, strike_wave_legislative_response).
narrative_ontology:affects_constraint(sotu_1947_truman_labor_management_procedural_reform, cold_war_labor_discipline).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the 1946 strike wave and upstream of the Taft-Hartley Act (which passed despite Truman's resistance and modified the procedural framework). The procedural reform represents the coordination-oriented alternative to Taft-Hartley's extraction-oriented approach. Network links show how the constraint prevents worse institutional outcomes while being modified by subsequent legislation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1947_truman_labor_management_procedural_reform, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
