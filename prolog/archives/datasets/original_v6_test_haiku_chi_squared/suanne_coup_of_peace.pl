% ============================================================================
% CONSTRAINT STORY: suanne_coup_of_peace
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suanne_coup_of_peace, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: suanne_coup_of_peace
 *   human_readable: The Hostile Social Environment at the Lead Basketball Game
 *   domain: social/cultural
 *
 * SUMMARY:
 *   In Ian Frazier's 'On the Rez,' the Pine Ridge Lakota basketball team
 *   travels to Lead, South Dakota for a game and encounters a hostile,
 *   explicitly racist social environment — jeers, slurs, and aggressive crowd
 *   behavior designed to intimidate and distract. This constraint models that
 *   environment as a structural extraction mechanism: the home crowd uses
 *   control of public space (the gym) and social dominance to extract
 *   dignity, focus, and psychological confidence from the visiting team. The
 *   extraction serves no coordination function — it is pure coercion
 *   maintaining racial hierarchy. The constraint is enforced through
 *   collective action (crowd behavior) and institutional passivity (school
 *   administration enforces no meaningful conduct standards). The hostility
 *   exhibits low theater ratio (0.38) because it is functionally direct:
 *   jeers and slurs accomplish their extraction goal without elaborate ritual
 *   or performative cover, unlike many pitons or snares that hide behind
 *   formal procedures.
 *
 * KEY AGENTS:
 *   - Pine Ridge Basketball Team: Primary victim (powerless/trapped) — must play in hostile environment; cannot withdraw without forfeiting; bears direct extraction through psychological intimidation
 *   - Individual Lakota Players: Secondary victim (moderate/constrained) — face personal racial attack; technically can leave but face team/family/career pressure
 *   - Lead Community White Residents: Primary beneficiary (organized/arbitrage) — benefit from signaling racial dominance and in-group solidarity through control of public space; experience as coordination within their own group
 *   - South Dakota Education/Sports Administration: Institutional actor (institutional/constrained) — nominally enforces conduct codes but performs tolerance while avoiding enforcement; maintains piton-level performative compliance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as pure structural racism with no legitimate coordination function; no sunset clause or alternative pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suanne_coup_of_peace, 0.58).
domain_priors:suppression_score(suanne_coup_of_peace, 0.72).
domain_priors:theater_ratio(suanne_coup_of_peace, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suanne_coup_of_peace, extractiveness, 0.58).
narrative_ontology:constraint_metric(suanne_coup_of_peace, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(suanne_coup_of_peace, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suanne_coup_of_peace, snare).
narrative_ontology:human_readable(suanne_coup_of_peace, "The Hostile Social Environment at the Lead Basketball Game").
narrative_ontology:topic_domain(suanne_coup_of_peace, "social/cultural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suanne_coup_of_peace, lead_community_white_residents).
narrative_ontology:constraint_victim(suanne_coup_of_peace, pine_ridge_basketball_team).
narrative_ontology:constraint_victim(suanne_coup_of_peace, lakota_dignity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PINE RIDGE BASKETBALL TEAM (SNARE) — Trapped in a hostile environment with no exit. Must play in Lead; cannot withdraw without forfeiting. Bears full extraction cost: psychological intimidation, racial slurs, jeers designed to disrupt focus and dignity. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL LAKOTA PLAYERS (SNARE) — Constrained by career and team obligation. Can technically exit (quit team, move) but face social/family pressure and loss of opportunity. Experience the constraint as direct personal attack on dignity and safety. d≈0.85, f(d)≈1.18, σ=0.8 → χ≈0.54.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: LEAD COMMUNITY WHITE RESIDENTS (ROPE) — Organized as implicit collective. Benefit from maintenance of racial hierarchy and local dominance signaling through control of public space (the gym). Experience the constraint as coordination mechanism: rally support, reinforce in-group solidarity through opposition to out-group. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.06. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(suanne_coup_of_peace, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE FROM CIVILIZATIONAL VIEW) — From outside, the hostility appears as pure extraction mechanism: a coercive system designed to suppress Lakota dignity and economic/social advancement. The constraint serves no coordination function; it is maintained entirely through suppression and cultural dominance. No sunset clause. d≈0.88, f(d)≈1.30, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(suanne_coup_of_peace, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

% PERSPECTIVE 5: SD EDUCATION/SPORTS ADMINISTRATION (PITON) — Institutional actors nominally enforce 'fair play' rules but perform tolerance while avoiding enforcement against clear violations of conduct codes. Theater ratio high: statements about 'sportsmanship' and 'respect' paired with minimal sanctions against racial harassment. The administrative constraint persists through inertia and cowardice, not legitimate function. theater_ratio=0.38 suggests lower theater than typical piton, but the performative tolerance (rules on books, unenforced in Lead) fits the piton pattern. d≈0.65, f(d)≈0.98, σ=0.9 → χ≈0.56.
constraint_indexing:constraint_classification(suanne_coup_of_peace, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_coup_of_peace_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_coup_of_peace, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suanne_coup_of_peace, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(suanne_coup_of_peace, TR),
    TR >= 0.70.

:- end_tests(suanne_coup_of_peace_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts measurable costs from Pine Ridge players: cognitive load from managing hostile environment during competition, loss of dignity and psychological safety, potential impact on athletic performance. However, it does not reach the highest extraction levels (0.80+) because the extraction is one-dimensional (psychological/social) rather than multi-faceted, and the constraint is geographically/temporally bounded (occurs only during away games in Lead, not continuously). Suppression (0.72): High. Significant barriers to exit and alternative pathways. Players cannot walk away without forfeit; school cannot easily relocate the game; state athletic authority does not intervene; institutional norms normalize the behavior as acceptable 'home court advantage.' The hostility is backed by collective enforcement (crowd) and institutional passivity (administrators). Theater ratio (0.38): Low-moderate. The constraint operates with minimal performative cover. The jeers and slurs are functionally direct — they accomplish their extraction goal (distraction, demoralization) without elaborate ritual or false justification. The low theater reflects that this is not a degraded institution (piton) but an actively functional extraction mechanism. The theatrical element that IS present comes from the institutional side: administrators perform 'concern' while avoiding action.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary group experiences the constraint as in-group coordination and dominance signaling; the victim group experiences it as extraction and degradation. Pine Ridge players see a snare with no exit. Lead residents see a rope (coordination mechanism within their own group, no sense of extraction). The analytical observer sees pure snare and structural racism. The institutional administration sees a performative balance between tolerance and enforcement, neither of which it actually delivers (piton perspective). This perspectival gap is maximal: no group agrees on what the constraint is.
 *
 * DIRECTIONALITY LOGIC:
 *   Pine Ridge basketball team: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction directionality. Individual Lakota players: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction but with some agency. Lead white residents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Beneficiaries with freedom to participate or not. Their 'exit option' is arbitrage — they can use the space for dominance signaling or not, but face no cost either way. South Dakota administration: Institutional + constrained (not arbitrage, because institutional actors are bound by enforcement obligations they nominally accept) → d≈0.65, f(d)≈0.98. High extraction from the constraint's perspective; the admin is being extracted from (coerced into passivity). Analytical observer: analytical → d≈0.88, f(d)≈1.30. Observer sees structural racism; high extraction from the constraint's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy because it is a pure snare (no coordination function detected from any perspective). The beneficiary group experiences in-group coordination, but that coordination would occur with or without the Pine Ridge constraint — the coordination is among Lead residents regardless of the victim's presence. The constraint adds nothing to the beneficiary's coordination; it purely extracts from the victim. There is no hybrid element here that would create the mandatrophy tension between 'is this coordination or extraction?' The extraction is primary; any coordination is orthogonal. The institutional admin perspective (piton) does not resolve mandatrophy either — the theater is from the admin side, not structural to the constraint itself. This is a clear snare case: designed, enforced, and beneficiary-serving extraction mechanism with zero coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'Does the hostility require explicit racial motivation from Lead residents, or does systemic indifference to racial impact constitute sufficient extraction intent?',
    'Testimony from Lead residents and team members; analysis of whether similar hostility directed at other visiting teams (non-Lakota); comparison to conduct standards applied at other schools',
    'If explicit intent required: some hostility reclassifiable as coordination (in-group bonding) rather than extraction. If systemic indifference sufficient: hostility remains extraction even if not explicitly motivated by anti-Lakota ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_threshold, conceptual, 'Whether extraction requires explicit racial motivation or systemic indifference suffices').

omega_variable(
    alternative_venue_feasibility,
    'Could the constraint be bypassed by Pine Ridge declining away games in Lead, or by state athletic authority mandating neutral venues?',
    'Historical analysis of other schools'' venue selections; cost comparison of neutral venue vs Lead; political feasibility of state intervention against Lead school district',
    'If feasible: exit option upgrades from ''trapped'' to ''constrained''/''mobile'' for team; classification shifts toward tangled_rope. If infeasible: trapped status confirmed, snare classification solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_venue_feasibility, empirical, 'Whether alternative venue arrangement could bypass the constraint').

omega_variable(
    generational_persistence,
    'Is the hostility sustained by active enforcement by each generation of Lead residents, or by institutional inertia and normalized practice that persists without active will?',
    'Longitudinal interviews with Lead residents across age cohorts; analysis of whether youth hostility matches parent/grandparent cohort; examination of when hostility escalates vs plateaus',
    'If active enforcement: snare classification confirmed across generations. If inertia-driven: potential piton degradation — could be addressed by breaking the cycle of normalized practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_persistence, empirical, 'Whether hostility is actively sustained or institutionally inherited').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suanne_coup_of_peace, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suanne_tr_t0, suanne_coup_of_peace, theater_ratio, 0, 0.35).
narrative_ontology:measurement(suanne_tr_t10, suanne_coup_of_peace, theater_ratio, 10, 0.38).
narrative_ontology:measurement(suanne_tr_t20, suanne_coup_of_peace, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(suanne_be_t0, suanne_coup_of_peace, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(suanne_be_t10, suanne_coup_of_peace, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(suanne_be_t20, suanne_coup_of_peace, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(suanne_coup_of_peace, lakota_economic_marginalization).
narrative_ontology:affects_constraint(suanne_coup_of_peace, reservation_school_funding_disparity).

% DUAL FORMULATION NOTE:
% The Lead basketball game hostility is a local manifestation of the larger constraint of structural racism in regional South Dakota. It is downstream of systemic inequalities (economic, educational, institutional) and upstream of the team's broader confrontation with social dominance systems. Linked as a specific extraction mechanism within the regional network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
