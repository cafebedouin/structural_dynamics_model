% ============================================================================
% CONSTRAINT STORY: arms_race_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arms_race_dynamics, []).

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
 *   constraint_id: arms_race_dynamics
 *   human_readable: Arms Race Dynamics: Competitive Escalation Trap
 *   domain: geopolitical/military/security
 *
 * SUMMARY:
 *   Arms race dynamics represent a structural constraint where competitive
 *   security logic produces outcomes that no individual actor desires but all
 *   actors rationally pursue. The constraint operates at the intersection of
 *   military strategy, economic resource allocation, and geopolitical
 *   stability. Civilian populations bear the extraction cost (resource
 *   diversion, economic opportunity cost, heightened existential risk).
 *   Military and defense institutions benefit through budget allocation,
 *   technological advancement opportunities, and organizational expansion.
 *   The constraint exhibits high suppression (0.72) because the coordination
 *   function of mutual deterrence masks the extractive mechanism — actors
 *   justify armament as defensive necessity, not as exploitation. Theater
 *   ratio (0.58) indicates moderate performative content: threat narratives,
 *   capability demonstrations, and strategic signaling often exceed what
 *   functional deterrence requires. Extractiveness (0.68) reflects
 *   significant real costs: global military expenditure exceeds $2 trillion
 *   annually, with opportunity costs in healthcare, education, and climate
 *   adaptation. The constraint has accumulated over the post-WWII period,
 *   accelerating through the Cold War and persisting despite its formal end.
 *   The measurement trajectory shows increasing extractiveness and theater
 *   ratio — the constraint has degraded toward pure extraction as
 *   verification mechanisms have decayed and strategic ambiguity has
 *   increased.
 *
 * KEY AGENTS:
 *   - Civilian populations and economic base: Primary victim (powerless/trapped) — bear resource diversion and existential risk with no exit mechanism
 *   - Military strategists and defense planners: Secondary victim/actor (moderate/constrained) — structurally locked into rational escalation logic despite recognizing collective inefficiency
 *   - Defense contractors and military-industrial complex: Primary beneficiary (institutional/arbitrage) — profit from escalation cycles; can exit individual competitions but benefit from system persistence
 *   - Nuclear deterrence theorists: Powerful institutional voice (powerful/constrained) — provide coordination rationale (MAD doctrine) while also contributing to extraction mechanism
 *   - Arms control coalitions: Organized agents (organized/mobile) — see institutional solutions with sunset logic through verification and transparency agreements
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks treating Prisoner's Dilemma as natural law rather than contingent institutional outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arms_race_dynamics, 0.68).
domain_priors:suppression_score(arms_race_dynamics, 0.72).
domain_priors:theater_ratio(arms_race_dynamics, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arms_race_dynamics, extractiveness, 0.68).
narrative_ontology:constraint_metric(arms_race_dynamics, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(arms_race_dynamics, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arms_race_dynamics, snare).
narrative_ontology:human_readable(arms_race_dynamics, "Arms Race Dynamics: Competitive Escalation Trap").
narrative_ontology:topic_domain(arms_race_dynamics, "geopolitical/military/security").

domain_priors:requires_active_enforcement(arms_race_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arms_race_dynamics, defense_contractors).
narrative_ontology:constraint_beneficiary(arms_race_dynamics, military_bureaucracies).
narrative_ontology:constraint_victim(arms_race_dynamics, civilian_populations).
narrative_ontology:constraint_victim(arms_race_dynamics, economic_resources).
narrative_ontology:constraint_victim(arms_race_dynamics, strategic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS & ECONOMIC RESOURCES (SNARE) — Trapped in escalation cycle. No individual exit option; resources diverted to weapons production regardless of consent. Experience maximum extraction: taxation, labor mobilization, economic opportunity cost. Cannot opt out of threat environment created by competitors' arms buildup.
constraint_indexing:constraint_classification(arms_race_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER MILITARY STRATEGIST (SNARE) — Constrained by structural logic: unilateral disarmament appears suicidal when adversaries arm. Each actor rationally arms given others' armament; collective outcome is economically destructive and strategically unstable. Exit means risking national security position; constrained rather than trapped but extraction is severe.
constraint_indexing:constraint_classification(arms_race_dynamics, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR DETERRENCE THEORIST (TANGLED ROPE) — Powerful institutional voice. Arms race has genuine coordination function: mutual vulnerability creates incentive not to attack (MAD doctrine). But also asymmetric extraction: weapons proliferation increases risk of accident, miscalculation, unauthorized use. Genuine coordination function alongside extractive mechanism.
constraint_indexing:constraint_classification(arms_race_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE CONTRACTOR (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as pure coordination mechanism: arming signals commitment, establishes deterrence credibility, generates contracts. Can arbitrage between multiple national buyers. Minimal extraction from their perspective; maximum benefit.
constraint_indexing:constraint_classification(arms_race_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARMS CONTROL COALITION (SCAFFOLD) — Organized agents (disarmament movements, treaty bodies, verification regimes) see the escalation as solvable through institutional frameworks with sunset logic: nuclear non-proliferation treaties, verification protocols, confidence-building measures. High suppression under the constraint (hard to implement verification); but coalition sees an achievable exit path through agreements. Sunset: as verification infrastructure matures and trust-building occurs, the extraction mechanism loses force.
constraint_indexing:constraint_classification(arms_race_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GAME THEORY VIEW (MOUNTAIN) — From a civilizational perspective, arms race escalation appears as an immutable law of competitive dynamics: the Prisoner's Dilemma structure is inherent to security competition when trust is absent and verification is incomplete. Each rational actor's best response to others' arming is to arm. The constraint is natural law. However, structural data suggests this is a false summit — the 'immutability' rests on assumptions about information asymmetry, verification capability, and institutional capacity that are contingent, not natural.
constraint_indexing:constraint_classification(arms_race_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arms_race_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arms_race_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arms_race_dynamics, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arms_race_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arms_race_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. The measurable cost includes direct military spending (~2.4% of global GDP), opportunity cost of foregone civilian investment, and risk premium from heightened existential danger. The value reflects genuine resource extraction from civilian economies. Measurement trajectory shows increase from 0.35 to 0.68 over the interval, indicating degradation from managed competition toward uncontrolled escalation. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) Information asymmetry about actual threat levels allows threat inflation; (2) Verification difficulty makes unilateral disarmament appear suicidal; (3) Institutional inertia in defense bureaucracies prevents policy reversal; (4) Domestic political constraints (military-industrial influence) limit alternatives. Theater ratio (0.58): Moderate and increasing. Strategic signaling, capability demonstrations, and threat narratives often exceed functional deterrence requirements. Accumulation of redundant systems, parade displays, and rhetorical escalation indicates rising performative content. The theater serves the extraction mechanism — it justifies continued spending to civilian populations.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification from the powerless/trapped perspective and the rope classification from the institutional/arbitrage perspective represent maximum divergence. The same structural phenomenon (competitive armament) appears as pure extraction to those who bear costs and pure coordination to those who benefit. The snare classification is confirmed across powerless and moderate perspectives; the rope classification appears only for the primary beneficiary. The tangled rope at powerful/constrained reflects that institutional actors managing deterrence genuinely coordinate while also enabling extraction. The scaffold at organized/mobile reflects that organized coalitions perceive institutional solutions. The mountain at analytical/civilizational represents the naturalization risk — treating contingent institutional outcomes as immutable laws. This perspectival structure is stable: it would persist across time horizons and scopes because the underlying structural position (beneficiary vs victim) is stable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit capacity. Defense contractors (beneficiary + arbitrage) have d ≈ 0.05 (full beneficiary, can exit individual competitions while profiting from system persistence). Civilian populations (victim + trapped) have d ≈ 0.95 (full target, no exit mechanism). Military strategists (victim + constrained) have d ≈ 0.85 (bear costs through rational escalation but have marginal agency through policy influence). Deterrence theorists (mixed position, constrained) have d ≈ 0.50 (benefit from institutional authority but also constrained by strategic logic). Arms control advocates (organized, mobile) have d ≈ 0.55 (can organize coalitions and perceive exit paths). The analytical observer (analytical exit) has d ≈ 0.72 (sees the full structure but risks naturalizing contingent arrangements). The chi formula χ = ε × f(d) × σ(S) produces: for powerless trapped agents χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (maximum effective extraction at global scope); for institutional arbitrage beneficiaries χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative extraction — subsidy from the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by identifying the coordination function AND the asymmetric extraction. The coordination function is genuine: mutual vulnerability creates deterrent stability, reducing risk of war through rational calculation. This is not a cover story — mutual assured destruction does prevent certain attack vectors. However, the asymmetric extraction is also genuine: civilian populations bear costs (resource diversion, existential risk) that are not symmetrically distributed with the coordination benefits. The snare classification (not tangled rope) is correct because the suppression (0.72) is severe — civilian populations cannot opt out of the threat environment even if they benefit marginally from reduced war probability. The beneficiary (defense contractors) can arbitrage away; the victim cannot. The constraint maintains extraction through narrative framing (security necessity) and institutional lock-in (military-industrial complex). The false summit risk occurs when the mountain perspective naturalizes this as inherent to competition, rather than recognizing it as a contingent institutional outcome dependent on verification capability, information transparency, and institutional design choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prisoner_dilemma_inevitability,
    'Is the Prisoner''s Dilemma structure of arms competition inherent to security dynamics or dependent on specific institutional/informational context?',
    'Historical comparison of arms races with vs without verification regimes, transparency mechanisms, third-party monitoring. Analysis of whether cooperative equilibria become stable when information asymmetry is reduced.',
    'If inherent: mountain classification valid — arms race is structural law. If contingent: false summit — the extraction mechanism depends on institutional choices, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prisoner_dilemma_inevitability, conceptual, 'Whether Prisoner''s Dilemma structure is inherent or contingent on institutions').

omega_variable(
    verification_sufficiency_threshold,
    'What level of verification and transparency would break the escalation cycle? Is perfect verification required, or does partial transparency suffice to enable cooperative equilibrium?',
    'Analysis of Cold War arms control treaties (SALT I/II, START, INF); examination of which verification mechanisms correlated with reduced escalation rates; game-theoretic modeling of repeated games with imperfect information.',
    'If perfect verification required: arms race is near-inevitable given technological uncertainty. If partial transparency sufficient: scaffold perspective is realistic — institutional agreements can establish exit path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_sufficiency_threshold, empirical, 'Verification and transparency threshold for breaking escalation').

omega_variable(
    technological_lock_in,
    'Does weapons technology development follow its own momentum independent of strategic intent, creating path dependency that prevents de-escalation even when political actors desire it?',
    'Historical case analysis: instances where military institutions resisted arms reduction despite political leadership support; examination of sunk costs, industrial capacity, technical expertise inertia.',
    'If strong lock-in: even with political will, institutional inertia maintains extraction. If lock-in is weak: political intent can redirect technological development. Affects whether snare classification includes institutional capture dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in, empirical, 'Whether weapons technology development creates irreversible path dependency').

omega_variable(
    extraction_beneficiary_opacity,
    'How much of the arms race''s continuation depends on deliberate obfuscation by defense contractors vs genuine strategic uncertainty among political decision-makers?',
    'Analysis of defense budget justifications, threat inflation in official documents, comparison with declassified intelligence assessments of actual threat levels; examination of defense industry influence on strategic perception.',
    'If primarily obfuscation: snare structure includes deliberate information suppression — extraction is maintained through theater. If genuine uncertainty: snare is driven by structural incentive misalignment, not conspiracy. Affects whether beneficiaries are actively suppressing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_opacity, empirical, 'Whether extraction continuation depends on deliberate obfuscation vs genuine uncertainty').

omega_variable(
    multipolar_instability,
    'Does transition from bipolar to multipolar arms dynamics increase or decrease extraction severity? Does the addition of more competitors entrench the snare or create coalition opportunities?',
    'Comparison of extraction metrics (resource diversion, economic cost, risk levels) during bipolar Cold War period vs post-Cold War multipolar period. Analysis of whether multiple actors increases or decreases verification difficulty and coalition-building possibility.',
    'If multipolar increases severity: snare classification is robust across polarity structures. If multipolar enables coalition: scaffold perspective becomes more realistic as organized coalitions have greater bargaining power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multipolar_instability, empirical, 'Effect of bipolarity vs multipolarity on arms race extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arms_race_dynamics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arm_tr_t0, arms_race_dynamics, theater_ratio, 0, 0.42).
narrative_ontology:measurement(arm_tr_t10, arms_race_dynamics, theater_ratio, 10, 0.5).
narrative_ontology:measurement(arm_tr_t20, arms_race_dynamics, theater_ratio, 20, 0.58).
narrative_ontology:measurement(arm_tr_t5, arms_race_dynamics, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(arm_be_t0, arms_race_dynamics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arm_be_t10, arms_race_dynamics, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(arm_be_t20, arms_race_dynamics, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(arm_be_t5, arms_race_dynamics, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arms_race_dynamics, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(arms_race_dynamics, 0.12).
narrative_ontology:affects_constraint(arms_race_dynamics, military_industrial_complex).
narrative_ontology:affects_constraint(arms_race_dynamics, security_dilemma).
narrative_ontology:affects_constraint(arms_race_dynamics, nuclear_proliferation_incentive).

% DUAL FORMULATION NOTE:
% Arms race dynamics decompose into three structurally distinct constraints sharing common drivers. This story focuses on the macro-level escalation cycle (arms race proper). Upstream: security_dilemma (the fundamental strategic logic). Downstream: military_industrial_complex (institutional capture that prevents de-escalation). nuclear_proliferation_incentive (specific instance of arms race logic applied to WMD). All three share high suppression and theater ratios but differ in extractiveness and beneficiary specificity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arms_race_dynamics, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
