% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotation_seven_kubo_ranking, []).

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
 *   constraint_id: rotation_seven_kubo_ranking
 *   human_readable: R7 Kubo Credit and Ranking System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Kubo Credit and Ranking System on Rotation Seven is a
 *   multi-generational labor-extraction mechanism disguised as meritocratic
 *   gamification. Crew members are assigned birthright credit scores at
 *   registration; these scores determine access to resources, work
 *   assignments, and social status. The system employs psychological
 *   compliance mechanisms (leaderboards, rank badges, advancement narratives)
 *   and systematic suppression (opaque algorithms, credit decay,
 *   intergenerational debt heredity, administrative penalties for
 *   complaints). The ship administration uses Kubo to solve a genuine
 *   coordination problem — allocating scarce labor and resources across a
 *   closed population — but the solution is extractive: it concentrates
 *   benefit among high-ranking families and forces the underclass into
 *   perpetual debt service. Theater pervades the system: the 'merit'
 *   narrative obscures that credit assignments are path-dependent
 *   (inheritance matters more than ability); the gamification creates the
 *   illusion of fairness through visible ranking mechanics; crew narratives
 *   blame individual failure rather than algorithmic bias. The constraint
 *   exhibits snare characteristics at all perspectives except the
 *   administration (which sees coordination benefit) and a small elite
 *   officer class (which experiences mixed benefit). Mandatrophy is resolved:
 *   this is not a coordination failure misclassified as extraction, nor an
 *   extraction failure misclassified as coordination. The Kubo system
 *   genuinely solves the allocation problem AND genuinely extracts from the
 *   underclass simultaneously.
 *
 * KEY AGENTS:
 *   - Crew Underclass (powerless/trapped): Born into low credit; face maximum extraction through labor demands and resource rationing; cannot exit; generations accumulate debt
 *   - Middle-Tier Workers (moderate/constrained): Attempt upward mobility through Kubo accumulation; systematically blocked by credit decay and administrative penalties; no real exit option
 *   - Ship Administration (institutional/arbitrage): Designers and operators of Kubo system; benefit from labor allocation efficiency and reduced direct coercion overhead; experience as coordination tool
 *   - Elite Officer Class (powerful/mobile): Inherit high credit; enjoy resource preference and prestige; can be reassigned or exempted from ranking ladder; see mixed benefit
 *   - Formal Resistance Movement (organized/constrained): Multi-generational opposition documented in ship records; propose alternatives (participatory budgeting, needs-based allocation); persist despite suppression
 *   - Analytical Observer (analytical/analytical): Views from civilizational perspective; recognizes extraction mechanism beneath merit narrative; identifies theater (58% of legitimacy is performative)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.68).
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.72).
domain_priors:theater_ratio(rotation_seven_kubo_ranking, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, extractiveness, 0.68).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_kubo_ranking, snare).
narrative_ontology:human_readable(rotation_seven_kubo_ranking, "R7 Kubo Credit and Ranking System").
narrative_ontology:topic_domain(rotation_seven_kubo_ranking, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, crew_underclass).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, low_ranking_households).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, intergenerational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREW UNDERCLASS (SNARE) — Trapped in generational dependency on Kubo rankings. Cannot exit the ship; ranking algorithm is opaque; career trajectory locked by birthright credit assignments. d≈0.98, f(d)≈1.40, σ=0.8 → χ≈0.76.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE-TIER WORKER (SNARE) — Can attempt upward mobility through Kubo accumulation but faces systematic barriers. Credit decay and administrative penalties ensure constrained exit. d≈0.82, f(d)≈1.18, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: SHIP ADMINISTRATION (ROPE) — Benefits from labor scheduling and allocation via Kubo. Experiences system as coordination tool: gamification drives participation, ranking automates resource distribution, credit mechanics reduce direct coercion overhead. d≈0.08, f(d)≈-0.11, σ=0.8 → χ≈-0.06.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: FORMAL RESISTANCE (PITON) — Organized opposition has documented Kubo exploitation for three generations; proposals for alternatives exist (needs-based allocation, participatory budgeting). But the system persists through institutional inertia: ship code treats Kubo as immutable, captain's office resists alternatives as 'unproven,' crew socialization normalizes rankings as natural. theater_ratio=0.58 reflects that much Kubo legitimacy comes from narrative (meritocracy myth) rather than functional necessity. d≈0.45, f(d)≈0.45, σ=0.8 → χ≈0.21.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ELITE OFFICER CLASS (TANGLED ROPE) — Senior officers benefit from Kubo (prestige, resource allocation preference), but also experience it as coordination necessity. They see the system as both justified (merit should reward labor) and extractive (they inherit high credit). Mobile exit: officers can be reassigned, promoted off the ranking ladder, or claim institutional exemptions. Mixed perception: net beneficiary but with genuine coordination rationale. d≈0.35, f(d)≈0.33, σ=0.8 → χ≈0.18.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the Kubo system is a pure extraction mechanism: it creates psychological compliance (gamification), systematic suppression of alternatives (ranking naturalized as merit), and intergenerational debt bondage (credit heredity). Base extraction ε=0.68 and suppression σ=0.72 meet snare thresholds. χ≈0.68 at this scope. The 'merit' narrative is theater (ratio=0.58), not function.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_kubo_ranking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotation_seven_kubo_ranking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotation_seven_kubo_ranking, TR),
    TR >= 0.70.

:- end_tests(rotation_seven_kubo_ranking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The Kubo system transfers labor value from the underclass to administrators and beneficiary families. Initial extractiveness (~0.42 at generation 1) reflected genuine coordination benefit when alternatives were truly unavailable; but as the system matured and the administration actively suppressed alternatives (killing participatory budgeting proposals, dismissing lottery schemes), the extraction component intensified. Current value (0.68) reflects that the system now primarily serves rent-seeking rather than coordination. Suppression (0.72): High and structural. Multiple barriers prevent exit: (a) algorithmic opacity (decay formula not publicly audited), (b) credit inheritance (born into debt), (c) administrative penalties (complaints trigger credit penalties), (d) narrative capture (crew socialization treats Kubo as natural/fair), (e) absence of alternatives (institutional resistance to proposals). Theater ratio (0.58): Moderate-high and rising. Theater comes from: (a) merit narrative (ranks presented as reflecting ability; actually reflect inheritance + bias), (b) leaderboard gamification (visible rankings create illusion of fairness through transparency), (c) advancement stories (individual success narratives obscure systemic barriers). Rising over time because as extraction intensified, theater had to increase to maintain legitimacy (the system requires belief in its fairness to function).
 *
 * PERSPECTIVAL GAP:
 *   The crew underclass perceives pure extraction (Snare) with d≈0.98, trapped in perpetual low ranking with no path to exit. The middle tier perceives similar extraction (d≈0.82) but with slightly more agency — they attempt mobility even if systematically blocked. The administration perceives coordination (Rope, d≈0.08) — Kubo genuinely allocates labor and resources. The elite officer class perceives mixed benefit (Tangled Rope, d≈0.35) — they benefit from rankings but also feel constrained by the system's logic. The resistance movement perceives inertial degradation (Piton) — the system persists through narrative maintenance, not functional necessity. The analytical observer perceives extraction (Snare, d≈0.72 in universal scope) — the theater and suppression mechanisms are fully visible at civilizational scale. This six-way perspectival gap is the diagnostic signature of the Kubo system: different structural positions reveal radically different mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   The crew underclass is the primary victim (high extraction, high suppression). They bear the full cost of labor allocation without proportional benefit. Directionality d≈0.98 follows from: (a) victim classification (they are explicitly targeted by debt mechanics), (b) trapped exit (cannot leave the ship, cannot exit the ranking system without death or mutiny), (c) highest f(d) function (f(0.98)≈1.40). The middle tier is also victim (d≈0.82) but with slightly lower d because they have constrained mobility — some rare individuals do escape low rankings. The administration is beneficiary (d≈0.08) because they solve coordination problems without bearing allocation costs. Their arbitrage exit (they can modify Kubo rules unilaterally) results in low d and negative effective extraction from their perspective. The elite officer class occupies ambiguous position (d≈0.35) — they benefit from rankings but are also constrained by them (cannot simply ignore merit optics). The resistance movement has moderate directionality (d≈0.45) because while they oppose extraction, they remain embedded in the system (organized agents, but constrained in their ability to change outcomes). The analytical observer sees the full extraction from maximum distance (d≈0.72 in universal scope), treating Kubo as a pure mechanism independent of legitimate coordination rationale.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVES MANDATROPHY: The critical distinction is whether Kubo's coordination function justifies its extraction. Mandatrophy resolution: Yes, Kubo genuinely solves labor allocation (no pure-coordination alternative exists that the administration has genuinely tested), BUT the current extraction level (0.68) far exceeds what coordination would require. Historical analysis shows: (a) Generation 1: ε≈0.42, genuine coordination necessity, suppression moderate (allocation under scarcity), theater low (crew understood tradeoffs). (b) Generation 3: ε≈0.55, administration begins suppressing alternatives (kills participatory budgeting pilot), theater rises (merit narrative hardens), extraction increases. (c) Generation 6: ε≈0.68, alternatives actively blocked, theater entrenched (crew socialized into Kubo belief), extraction dominant. The mandatrophy is NOT that Kubo is mislabeled — it is correctly labeled Snare because extraction now exceeds coordination necessity. The system represents a drift from legitimate coordination constraint (would have been Rope or Scaffold) into pure extraction mechanism. The snare classification certifies that the extraction is structural and systemic, not a bug in an otherwise fair system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_decay_algorithm_opacity,
    'Is the credit decay formula (the ''drift'') a mathematical necessity for fiscal sustainability or an intentional suppression mechanism to perpetuate debt cycles?',
    'Cryptographic audit of the decay formula; comparison with alternative allocation algorithms (needs-based, participatory, lottery); analysis of who benefits from decay patterns over 10+ generations',
    'If mathematical necessity: Snare classification stands but with lower suppression score (~0.60). If intentional mechanism: Suppression confirmed as deliberate extraction, snare classification hardened (suppression→0.85).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credit_decay_algorithm_opacity, empirical, 'Whether credit decay is structurally necessary or deliberately suppressive').

omega_variable(
    alternative_allocation_viability,
    'Could the ship''s resource distribution and labor scheduling operate effectively without Kubo gamification (e.g., via lottery, rotation, needs-based allocation)?',
    'Simulation of alternative systems with same labor constraints; trial periods with partial opt-out zones; ethnographic study of motivation under non-gamified allocation in ship archives',
    'If viable: Kubo is pure extraction theater (Snare classification confirmed, theater_ratio→0.70+). If necessary: Some coordination rationale exists (reclassify to Tangled Rope with lower extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_viability, empirical, 'Whether non-gamified labor allocation could replace Kubo').

omega_variable(
    intergenerational_credit_mobility,
    'What percentage of crew members born into bottom-decile credit actually exit that decile within their lifetime, versus how many remain trapped?',
    'Longitudinal birth-cohort analysis from ship census; correlation between birth credit rank and lifetime earnings rank; identification of mobility barriers (algorithm bias, administrative penalties, network effects)',
    'If mobility < 15%: Snare extraction confirmed (intergenerational debt bondage). If mobility > 50%: System has genuine meritocratic function (reclassify toward Tangled Rope). Current suspicion: ~8% mobility suggests structural trap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_credit_mobility, empirical, 'Intergenerational credit mobility rates and mobility barriers').

omega_variable(
    captain_dependency_legitimacy,
    'Does the Kubo system''s legitimacy depend primarily on the ship captain''s personal authority or on perceived fairness of the ranking algorithm?',
    'Captain turnover analysis: do legitimacy crises occur at leadership transitions? Survey data on crew perception of ranking fairness vs. institutional authority; correlation with captain charisma/competence vs. algorithm transparency',
    'If primarily personal authority: System is piton (theatrical maintenance via captain myth). If primarily algorithm legitimacy: Snare has deeper roots (belief in merit naturalizes extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(captain_dependency_legitimacy, conceptual, 'Source of Kubo system legitimacy: personal captain authority versus algorithmic fairness perception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_kubo_ranking, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rota_tr_t0, rotation_seven_kubo_ranking, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rota_tr_t3, rotation_seven_kubo_ranking, theater_ratio, 3, 0.46).
narrative_ontology:measurement(rota_tr_t6, rotation_seven_kubo_ranking, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(rota_be_t0, rotation_seven_kubo_ranking, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rota_be_t3, rotation_seven_kubo_ranking, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(rota_be_t6, rotation_seven_kubo_ranking, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_kubo_ranking, resource_allocation).
narrative_ontology:affects_constraint(rotation_seven_kubo_ranking, generation_ship_closed_loop_psychology).
narrative_ontology:affects_constraint(rotation_seven_kubo_ranking, shipboard_succession_legitimacy).

% DUAL FORMULATION NOTE:
% The Kubo system is downstream of the closed-loop constraint (generational ship cannot exit) and upstream of succession legitimacy (ranking determines who captains next rotation). The three constraints form a family: the closed-loop makes extraction impossible to resist; Kubo extracts through labor allocation; succession legitimacy ensures offspring of high-Kubo families inherit power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotation_seven_kubo_ranking, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
