% ============================================================================
% CONSTRAINT STORY: arms_race_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arms_race_escalation, []).

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
 *   constraint_id: arms_race_escalation
 *   human_readable: Arms Race Escalation Dynamic
 *   domain: geopolitical/military/security
 *
 * SUMMARY:
 *   The arms race escalation dynamic represents a structural trap where
 *   rational individual actor choices (military establishments securing
 *   deterrence capability, defense contractors meeting demand, states
 *   protecting sovereignty) collectively produce irrational aggregate
 *   outcomes (resource diversion, instability increase, accident risk). The
 *   constraint exhibits dual extraction pathways: direct (funding capture by
 *   military-industrial complex) and indirect (suppression of alternative
 *   security models through threat inflation and identity lock). The
 *   extractiveness score (0.68) reflects sustained institutional capture of
 *   government resources, but with some reversibility — historical examples
 *   (Cold War de-escalation, arms control treaties) demonstrate that
 *   escalation is not immutable. Theater ratio (0.58) indicates that
 *   significant portions of defense spending constitute performative
 *   signaling (doctrine rehearsal, threat communication, strategic ambiguity
 *   maintenance) rather than functional capability. The constraint multiplies
 *   across multiple time horizons: biographical (immediate threat perception
 *   drives citizen compliance), generational (military industrial complex
 *   becomes institutionalized), civilizational (international anarchy framing
 *   naturalizes escalation as inevitable). Perspectives diverge sharply:
 *   beneficiaries (defense contractors, military establishments) experience
 *   rope or tangled rope (coordination with extraction); victims (civilian
 *   populations, global stability) experience snare (pure extraction with no
 *   exit); analytical observers risk mountain classification (security
 *   dilemma as natural law) which may be a false summit.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victims (powerless/trapped) — bear cost of military spending diversion, nuclear risk, and resource scarcity with no exit option
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) — capture sustained demand and government funding; can exit if political conditions change
 *   - Military Establishments: Secondary beneficiary (organized/constrained) — gain budgetary increase and geopolitical relevance; constrained by alliance obligations and threat perception
 *   - Adversary State Apparatus: Mixed actor (organized/identity_locked) — experiences coordination (genuine deterrence) and extraction (institutional autonomy); cannot exit without identity redefinition
 *   - Global Stability Commons: Victim (powerless/trapped) — abstract collective good bearing cost of increased unpredictability and accident risk
 *   - Analytical Observer: Risk naturalization (analytical/analytical) — security dilemma framing risks treating contingent institutional choices as inevitable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arms_race_escalation, 0.68).
domain_priors:suppression_score(arms_race_escalation, 0.72).
domain_priors:theater_ratio(arms_race_escalation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arms_race_escalation, extractiveness, 0.68).
narrative_ontology:constraint_metric(arms_race_escalation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(arms_race_escalation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arms_race_escalation, snare).
narrative_ontology:human_readable(arms_race_escalation, "Arms Race Escalation Dynamic").
narrative_ontology:topic_domain(arms_race_escalation, "geopolitical/military/security").

domain_priors:requires_active_enforcement(arms_race_escalation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arms_race_escalation, defense_contractors).
narrative_ontology:constraint_beneficiary(arms_race_escalation, military_establishments).
narrative_ontology:constraint_victim(arms_race_escalation, civilian_populations).
narrative_ontology:constraint_victim(arms_race_escalation, economic_development_capacity).
narrative_ontology:constraint_victim(arms_race_escalation, global_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Bears full cost of escalation through military spending diversion, nuclear risk, and resource scarcity. No exit option; cannot opt out of living in a militarizing world. Maximum extraction with suppression via threat perception and geopolitical framing.
constraint_indexing:constraint_classification(arms_race_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL STABILITY COMMONS (SNARE) — Abstract collective good with no agent to represent it; no exit from the escalation logic. Bears cost of reduced predictability, increased accident risk, and resource depletion. Trapped on a civilizational timescale.
constraint_indexing:constraint_classification(arms_race_escalation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED MILITARY ESTABLISHMENT (TANGLED ROPE) — Simultaneously experiences coordination (deterrence function) and extraction (funding, institutional power, autonomy). Exit is constrained by treaty obligations and threat perception. Benefits from arms race through budgetary increase and geopolitical relevance, but also bears cost of maintaining readiness and risk escalation.
constraint_indexing:constraint_classification(arms_race_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Primary beneficiary; experiences the constraint as pure coordination. Solves the problem of supplying military capability; extracts value through contracts and sustained demand. Arbitrage exit option — can shift production to non-military supply if political conditions change. Net beneficiary position.
constraint_indexing:constraint_classification(arms_race_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ADVERSARY STATE APPARATUS (TANGLED ROPE, IDENTITY-LOCKED) — Experiences both coordination (genuine security deterrence against perceived threats) and extraction (institutional autonomy, diversion of resources, geopolitical status). Exit is constrained not by material barriers but by identity fusion: the state's legitimacy narrative is constituted through security positioning. Cannot exit escalation without redefining state identity.
constraint_indexing:constraint_classification(arms_race_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: MAD DOCTRINE (PITON) — Cold War-era strategic logic that persists through institutional inertia despite reduced empirical basis (more actors, more instability, more accident risk). Theater ratio high (0.58) — doctrinal rehearsal, strategic communication, and threat narratives constitute much of the constraint's functional content. Genuine deterrence function degraded; institutional maintenance remains.
constraint_indexing:constraint_classification(arms_race_escalation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SECURITY DILEMMA NATURAL LAW (MOUNTAIN) — From civilizational scale, the arms race escalation appears as an immutable feature of international anarchy: without a central authority, actors cannot credibly commit to non-escalation, so mutual escalation becomes rational. This perspective risks naturalizing what is actually a contingent institutional-political arrangement. The engine's false summit detector will identify whether this classification survives structural scrutiny.
constraint_indexing:constraint_classification(arms_race_escalation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arms_race_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arms_race_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arms_race_escalation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arms_race_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arms_race_escalation, TR),
    TR >= 0.70.

:- end_tests(arms_race_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Base extraction reflects sustained institutional capture of government resources by defense sector and military establishments. The measurement trajectory (0.35 → 0.68 over 30 years) shows accumulation consistent with institutional drift — as military-industrial complex becomes more entrenched, more spending becomes structurally justified. The extracted value flows to defense contractors (contracts), military establishments (budget increase, autonomy), and state apparatus (geopolitical positioning). Civilian populations and economic development capacity bear the cost. Suppression (0.72): Very high. Escalation logic is maintained through multiple suppression mechanisms: (1) threat narrative inflation by state apparatus and military establishments, (2) identity lock preventing alternative security models from being imagined, (3) technological determinism framing (acceleration as inevitable), (4) rationality justification (security dilemma makes escalation 'rational'), (5) temporal distribution (immediate threat perception vs long-term instability cost). Theater ratio (0.58): Moderate-high. Significant portions of military spending constitute performative activity: doctrine rehearsal, strategic communication, threat signaling, military posturing. But some genuine deterrence function exists, preventing full piton classification. The rising theater trajectory (0.42 → 0.58) indicates increasing performative content relative to functional capability — doctrinal theater substitutes for actual force readiness in signaling deterrence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. Defense contractors (institutional/arbitrage) see Rope — solving the coordination problem of supplying deterrence capability. Military establishments (organized/constrained) see Tangled Rope — genuine deterrence coordination mixed with institutional capture. Adversary state apparatus (organized/identity_locked) also see Tangled Rope but with critical difference: their exit is blocked not by material barriers but by identity fusion with security positioning; their classification at identity_locked differs from the military establishment's constrained exit. Civilian populations (powerless/trapped) and global stability (powerless/trapped) see Snare — pure extraction with no exit option. The analytical observer at civilizational scale risks seeing Mountain (security dilemma as inevitable natural law) — but this is a false summit if empirically, historical de-escalation and institutional reform are feasible. The perspectival gap reveals how the same structural phenomenon (military capability building) is experienced as coordination by beneficiaries, mixed coordination-extraction by military establishments with institutional interests, and pure extraction by victims. The identity_locked classification for adversary state is critical: this actor cannot exit through arbitrage (like contractors) because their state identity is constituted through security positioning.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural relationship to the constraint: beneficiary status (low d), victim status (high d), and exit options. Defense contractors are beneficiaries with arbitrage exit (d ≈ 0.05) — they experience negative effective extraction (constraint subsidizes them). Military establishments are beneficiary-victims with constrained exit (d ≈ 0.45) — they benefit from institutional autonomy and budget, but are partially trapped by alliance obligations and threat perception (moderate extraction). Adversary state apparatus is identity-locked; structural derivation would place them as constrained/mixed (d ≈ 0.55), but identity lock prevents exercising arbitrage exit — they cannot redefine state security position without identity dissolution. Civilian populations are victims with trapped exit (d ≈ 0.95) — maximum extraction, maximum suppression. The piton classification derives not from high d but from theater_ratio gate (0.58 ≥ 0.70 threshold not met, but close); MAD doctrine persists through institutional inertia despite degraded deterrence function.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE AS DOMINANT CLASSIFICATION: The constraint resolves mandatrophy by identifying defense contractors as primary beneficiaries with genuine arbitrage exit (not trapped), and civilian populations as primary victims with no exit. This satisfies snare gates: extractiveness (0.68 ≥ 0.46), suppression (0.72 ≥ 0.60), χ projected ≥ 0.66. The tangled rope perspectives (military establishments, adversary state) represent secondary positions with mixed extraction-coordination functions; these do not override the snare classification because snare gates are satisfied from the primary victim perspective. IDENTITY LOCK DIAGNOSIS: The adversary state apparatus classification at identity_locked (rather than constrained) is diagnostically critical. If the state were merely constrained, they could exit through arbitrage — negotiated de-escalation, arms control treaties, diplomatic settlement. But identity_locked indicates the state's legitimacy narrative cannot tolerate appearing to back down or accept reduced security positioning. This makes de-escalation coordination difficult (requires identity reframing, not just cost-benefit recalculation). INSTITUTIONAL CAPTURE: The military-industrial complex satisfies tangled rope criteria from the military establishment perspective (coordination + extraction + enforcement), not rope, because significant extraction occurs and suppression is high. From defense contractor perspective, the constraint is pure rope (no enforcement needed, voluntary participation). PITON RISK: The theater_ratio (0.58) is approaching piton threshold (0.70). If doctrine theater rises above 0.70 while actual deterrence function declines, the constraint would reclassify as piton (institutional inertia maintaining degraded ritual). Historical precedent: Cold War MAD doctrine became increasingly performative in final years as Soviet capacity to maintain credible deterrence declined, yet escalation continued through doctrinal inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_dilemma_empirical_necessity,
    'Is escalation an inevitable consequence of international anarchy, or a choice enabled by institutional structures that could be reformed?',
    'Comparative historical analysis of de-escalation successes (Cold War end, arms control agreements); identification of structural conditions that permit mutual restraint vs those that force escalation',
    'If inevitable (natural law): mountain classification sustained; policy optimization focuses on stability within escalation. If contingent: snare classification from analytical perspective; policy optimization focuses on structural reform to permit de-escalation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_empirical_necessity, conceptual, 'Whether arms race escalation is structurally inevitable or contingent on institutional choice').

omega_variable(
    defense_spending_fungibility,
    'What proportion of military spending diversion from civilian sectors is extractive overhead vs legitimate security investment?',
    'Cost-benefit analysis of actual military capabilities vs stated deterrence functions; measurement of security improvement per dollar spent; comparison to civilian infrastructure ROI',
    'If primarily extractive (>70%): victimization of civilian populations confirmed, snare classification strengthened. If primarily legitimate (>50% genuine security): tangled rope classification more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_spending_fungibility, empirical, 'What fraction of military spending is extractive overhead vs legitimate security').

omega_variable(
    adversary_perception_calibration,
    'To what degree does escalation reflect accurate threat perception vs identity-locked threat inflation by state apparatus?',
    'Intelligence assessment of actual adversary capabilities vs stated threat levels; measurement of threat narrative consistency with classified intelligence; analysis of domestic political incentives for threat inflation',
    'If accurate perception: tangled rope with genuine coordination function. If substantially inflated: snare with suppression via manufactured fear; identity_locked classification gains empirical support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adversary_perception_calibration, empirical, 'Whether threat perception driving escalation is accurate or inflated by institutional interests').

omega_variable(
    de_escalation_coordination_feasibility,
    'Can mutual de-escalation be achieved through institutional mechanisms without unilateral vulnerability, or is escalation irreversible once initiated?',
    'Game-theoretic analysis of de-escalation pathways; historical precedent analysis (Soviet collapse, INF Treaty); negotiation simulation under asymmetric information',
    'If feasible: scaffold perspective gains reality (sunset is possible); policy recommendation shifts to de-escalation coordination. If infeasible: snare classification confirmed; only mitigation strategies available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_escalation_coordination_feasibility, conceptual, 'Whether de-escalation is institutionally achievable or irreversible').

omega_variable(
    technological_acceleration_floor,
    'Is there a technological advancement speed threshold beyond which escalation dynamics become deterministic rather than choice-based?',
    'Analysis of AI, autonomous weapons, and precision strike acceleration rates; measurement of decision timescales vs technological timescales; identification of critical threshold where human control is effectively lost',
    'If threshold approaching: mountain classification from technological perspective (immutable physics of acceleration). If threshold distant: snare classification sustained (political choice remains).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_acceleration_floor, empirical, 'Whether technological acceleration creates deterministic escalation threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arms_race_escalation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arms_tr_t0, arms_race_escalation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(arms_tr_t15, arms_race_escalation, theater_ratio, 15, 0.51).
narrative_ontology:measurement(arms_tr_t30, arms_race_escalation, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(arms_be_t0, arms_race_escalation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arms_be_t15, arms_race_escalation, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(arms_be_t30, arms_race_escalation, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arms_race_escalation, enforcement_mechanism).
narrative_ontology:affects_constraint(arms_race_escalation, military_industrial_complex_capture).
narrative_ontology:affects_constraint(arms_race_escalation, nuclear_proliferation_dynamics).
narrative_ontology:affects_constraint(arms_race_escalation, defense_budget_fungibility_constraint).

% DUAL FORMULATION NOTE:
% Arms race escalation decomposes into three structurally distinct constraints: (1) military-industrial complex institutional capture (ε ≈ 0.65, snare from contractor perspective, tangled rope from military perspective), (2) nuclear proliferation as separate escalation driver (ε ≈ 0.58, with different beneficiary structure), (3) defense budget competition with civilian development (ε ≈ 0.60, zero-sum resource allocation). This constraint story focuses on the systemic escalation dynamic; network links to the domain-specific instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arms_race_escalation, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
