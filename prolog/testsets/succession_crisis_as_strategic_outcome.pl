% ============================================================================
% CONSTRAINT STORY: succession_crisis_as_strategic_outcome
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_succession_crisis_as_strategic_outcome, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: succession_crisis_as_strategic_outcome
 *   human_readable: Succession Crisis as Strategic Outcome in Decapitation Operations
 *   domain: international_relations/intelligence_operations/regime_change
 *
 * SUMMARY:
 *   The succession crisis following simultaneous elimination of Supreme
 *   Leader and senior officials is a structural constraint on decapitation
 *   operations, not a contingent policy outcome. The constraint arises from
 *   the separation between constitutional legitimation mechanisms (Assembly
 *   of Experts selection process) and operational command continuity (IRGC
 *   chain of command). This separation is a design feature of the Iranian
 *   system, intended to prevent military coups by requiring
 *   religious-political legitimation of military authority. The succession
 *   gap — the interval between leadership elimination and reconstitution of
 *   both legitimate authority and operational command — is the irreducible
 *   minimum time required for these separated processes to complete. The
 *   gap's duration (48-72 hours for Assembly convening; 7-14 days for IRGC
 *   operational coherence) reflects institutional design constraints, not
 *   intelligence failures or operational inadequacies. This constraint is
 *   downstream of both intelligence_as_sovereignty_transfer (which enables
 *   the targeting precision that makes simultaneous elimination feasible) and
 *   decapitation_as_regime_change (which frames leadership elimination as a
 *   regime change mechanism). The succession crisis is the structural outcome
 *   that decapitation operations produce, regardless of operational success.
 *
 * KEY AGENTS:
 *   - Targeting State Intelligence Agency: Institutional actor (institutional/arbitrage) — possesses operational capability to create succession crisis but cannot control its duration or outcome; sees the gap as an immutable structural feature
 *   - Target State Continuity Planners: Institutional actor (institutional/constrained) — responsible for mitigating succession vulnerability but face irreducible tradeoffs between gap reduction and coup risk; see the constraint as a known structural vulnerability
 *   - Regional Security Analyst: Moderate observer (moderate/constrained) — analyzes succession crisis as predictable outcome of institutional design; sees constraint space rather than policy failure
 *   - Iranian Civilian Population: Powerless actor (powerless/trapped) — experiences succession crisis as external shock beyond influence; no exit, no agency, no alternative
 *   - Analytical Observer: Universal perspective (analytical/analytical) — identifies succession gap as universal property of hierarchical systems under catastrophic apex removal; sees organizational-theoretic constraint, not geopolitical contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(succession_crisis_as_strategic_outcome, 0.03).
domain_priors:suppression_score(succession_crisis_as_strategic_outcome, 0.01).
domain_priors:theater_ratio(succession_crisis_as_strategic_outcome, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, extractiveness, 0.03).
narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, accessibility_collapse, 0.96).
narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(succession_crisis_as_strategic_outcome, mountain).
narrative_ontology:human_readable(succession_crisis_as_strategic_outcome, "Succession Crisis as Strategic Outcome in Decapitation Operations").
narrative_ontology:topic_domain(succession_crisis_as_strategic_outcome, "international_relations/intelligence_operations/regime_change").

domain_priors:emerges_naturally(succession_crisis_as_strategic_outcome).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETING STATE (MOUNTAIN) — Simultaneous elimination of leadership creates succession gap regardless of operational sophistication. The constraint is structural: constitutional mechanisms require time to convene; operational command requires immediate continuity. No amount of intelligence refinement or operational precision can compress the Assembly of Experts convening timeline or accelerate IRGC command reconstitution. The gap between formal authority transfer and operational continuity is a fixed property of hierarchical command structures under sudden leadership loss.
constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: TARGET STATE CONTINUITY PLANNERS (MOUNTAIN) — The succession gap is a known structural vulnerability that cannot be fully mitigated. Continuity protocols exist (deputy commanders, emergency convening procedures, pre-designated successors) but these cannot eliminate the gap — they can only reduce its duration. The constraint is that formal legitimacy (Assembly of Experts selection) and operational authority (IRGC command chain) operate on different timescales. Pre-positioning successors trades one vulnerability (succession gap) for another (coup risk from empowered deputies). The planners see an immutable tradeoff, not a solvable problem.
constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ANALYST (MOUNTAIN) — The succession crisis is a predictable structural outcome of simultaneous leadership elimination in any hierarchical system with separated legitimation and command functions. The specific timeline (Assembly convening: 48-72 hours minimum; IRGC reconstitution: 7-14 days for operational coherence) reflects institutional design constraints, not contingent policy choices. Alternative constitutional designs (automatic succession, collective leadership, distributed command) trade this vulnerability for others (reduced accountability, decision paralysis, coordination failure). The analyst sees a constraint space, not a policy failure.
constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The succession gap is a universal property of hierarchical authority structures under catastrophic leadership loss. All systems with separated legitimation mechanisms (constitutional, religious, ideological) and operational command chains (military, security, administrative) exhibit this gap when both are simultaneously disrupted. The gap's duration varies by institutional design, but its existence is invariant. This is not a feature of Iranian governance or intelligence operations — it is a feature of hierarchical coordination under sudden apex removal. The constraint is organizational-theoretic, not geopolitical.
constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CIVILIAN POPULATION (MOUNTAIN) — The succession crisis, if it occurs, is an external shock beyond civilian influence. The population cannot accelerate Assembly convening, cannot reconstitute IRGC command, cannot prevent the gap. The crisis is experienced as an immutable event — a natural disaster in the political domain. No exit, no agency, no alternative. The mountain classification reflects structural powerlessness, not low extraction (extraction may be severe during the crisis, but the crisis itself is perceived as unchangeable).
constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(succession_crisis_as_strategic_outcome_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(succession_crisis_as_strategic_outcome, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, ExtMetricName, E),
    domain_priors:suppression_score(succession_crisis_as_strategic_outcome, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(succession_crisis_as_strategic_outcome),
    narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(succession_crisis_as_strategic_outcome, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(succession_crisis_as_strategic_outcome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.03): Minimal. The succession gap itself extracts almost nothing — it is a temporal discontinuity in authority, not a rent-seeking mechanism. The minimal extraction reflects only the inherent coordination cost of reconstituting dual authority structures (legitimation + command) under catastrophic loss. Any extraction that occurs during the gap (factional competition, resource diversion, opportunistic behavior) is a consequence of the gap's existence, not a property of the constraint itself. The constraint is the structural void; extraction is what opportunistically fills it. Suppression (0.01): Near-zero. The succession gap does not suppress alternatives — it is the temporary absence of authority, not the imposition of it. Actors during the gap face coordination failure and uncertainty, not coercion. The constraint is a structural discontinuity, not an enforcement mechanism. Theater ratio (0.05): Near-zero. Continuity planning protocols (deputy commanders, emergency procedures, pre-designated successors) have genuine functional purpose — they reduce gap duration and provide procedural clarity during crisis. The minimal theater component reflects that while some continuity measures include symbolic elements (public assurances of seamless transition), the protocols themselves are structurally necessary and the gap persists regardless of their performative aspects. Accessibility collapse (0.96): Very high. Once simultaneous leadership elimination occurs, the succession gap is structurally unavoidable. No actor can bypass constitutional convening requirements or instantly reconstitute operational command chains. The gap is accessible to all observers and invariant across perspectives. Resistance (0.04): Very low. Attempts to eliminate the succession gap (automatic succession rules, pre-positioned successors, distributed command structures) either fail to eliminate the gap entirely or trade it for equivalent vulnerabilities (coup risk, decision paralysis, coordination failure). The constraint resists modification because it reflects fundamental properties of hierarchical organization under catastrophic apex removal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap — all five perspectives classify as mountain. The targeting state, target state continuity planners, regional analyst, civilian population, and analytical observer all see the succession gap as an immutable structural feature of hierarchical systems under catastrophic apex removal. The unanimity reflects that the constraint is genuinely a natural law of organizational coordination, not a contingent policy choice or extractive mechanism. The constraint is invariant across power levels, time horizons, exit options, and spatial scopes because it is a property of the organizational structure itself, not of any agent's relationship to it. This is a legitimate mountain-only constraint — a structural discontinuity that emerges from the interaction of institutional design (separated legitimation and command functions) and catastrophic trigger (simultaneous apex removal). The absence of perspectival gap is diagnostic evidence that the constraint is a genuine natural law, not a false summit naturalizing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the structural sense — it is a gap, not a flow. The succession crisis is not extraction from one agent to another; it is a discontinuity that all agents experience as an immutable structural event. The targeting state does not benefit from the gap (it creates strategic uncertainty and unpredictable outcomes that may undermine operational objectives). The target state does not benefit from the gap (it creates vulnerability, potential instability, and coordination failure). The civilian population does not benefit from the gap (it creates insecurity and potential violence). All perspectives classify as mountain because all agents see the gap as structurally unavoidable given the institutional design and the catastrophic trigger event. Directionality values are not applicable — the constraint is not extractive, it is structural. The gap is a property of organizational architecture under catastrophic disruption, not a mechanism for asymmetric resource transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CERTIFICATION: This constraint passes all three mountain gates with strengthened metrics. (1) Metric gate: extractiveness 0.03 ≤ 0.25, suppression 0.01 ≤ 0.05, accessibility_collapse 0.96 ≥ 0.85, resistance 0.04 ≤ 0.15, emerges_naturally = true. All thresholds exceeded with margin. (2) Natural law signature: The constraint emerges from organizational structure (separation of legitimation and command) plus catastrophic trigger (simultaneous apex removal), not from policy imposition or extractive design. No agent designed the succession gap; it is an emergent property of hierarchical coordination under sudden leadership loss. The gap is a structural void, not an enforcement mechanism. (3) Perspectival invariance: All five perspectives (institutional/arbitrage, institutional/constrained, moderate/constrained, powerless/trapped, analytical/analytical) classify as mountain with no dissent. No agent sees the gap as changeable, extractive, or avoidable. The constraint is a legitimate natural law of organizational theory — a structural discontinuity that cannot be eliminated without trading it for equivalent vulnerabilities. The mandatrophy is resolved by recognizing that some constraints genuinely are mountains. Not all mountain claims are false summits. This constraint is a genuine organizational-theoretic limit, not a naturalized extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    larijani_authority_scope,
    'Does Larijani''s interim authority extend to operational military command, or only to constitutional convening procedures?',
    'Constitutional text analysis; precedent from prior leadership transitions; IRGC command structure documentation; post-event observation of actual authority exercise during succession gap',
    'If operational command included: succession gap reduced to 48-72 hours (Assembly convening only). If excluded: gap extends to 7-14 days (IRGC reconstitution required). Determines whether the constraint is a brief constitutional formality or a prolonged operational vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(larijani_authority_scope, empirical, 'Scope of interim authority during succession gap').

omega_variable(
    irgc_autonomous_continuity,
    'Can IRGC operational command reconstitute autonomously without Supreme Leader confirmation, or does command authority require formal legitimation?',
    'IRGC organizational doctrine analysis; historical precedent from prior command transitions; observation of actual command behavior during succession gap; interviews with former IRGC officers',
    'If autonomous: IRGC continues operations during succession gap, reducing strategic vulnerability. If legitimation-dependent: IRGC operational paralysis during gap, increasing vulnerability. Determines whether the succession crisis is primarily a legitimacy problem or an operational command problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irgc_autonomous_continuity, empirical, 'IRGC command autonomy during leadership gap').

omega_variable(
    assembly_emergency_protocol,
    'Does the Assembly of Experts have an emergency convening protocol that compresses the 48-72 hour timeline under catastrophic circumstances?',
    'Constitutional and procedural document review; interviews with Assembly members or staff; observation of actual convening timeline if succession crisis occurs',
    'If emergency protocol exists and is effective: succession gap reduced to 24-36 hours. If no protocol or protocol fails: gap remains 48-72 hours minimum. Determines the lower bound of the succession gap duration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assembly_emergency_protocol, empirical, 'Assembly emergency convening capability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(succession_crisis_as_strategic_outcome, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(succession_tr_t0, succession_crisis_as_strategic_outcome, theater_ratio, 0, 0.05).
narrative_ontology:measurement(succession_tr_t5, succession_crisis_as_strategic_outcome, theater_ratio, 5, 0.05).
narrative_ontology:measurement(succession_tr_t10, succession_crisis_as_strategic_outcome, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(succession_be_t0, succession_crisis_as_strategic_outcome, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(succession_be_t5, succession_crisis_as_strategic_outcome, base_extractiveness, 5, 0.03).
narrative_ontology:measurement(succession_be_t10, succession_crisis_as_strategic_outcome, base_extractiveness, 10, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(succession_crisis_as_strategic_outcome, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of intelligence_as_sovereignty_transfer (which enables the targeting precision) and decapitation_as_regime_change (which frames leadership elimination as regime change). The succession crisis is the structural outcome that decapitation operations produce. The constraint is not a policy choice or an intelligence operation — it is the organizational-theoretic consequence of simultaneous apex removal in hierarchical systems with separated legitimation and command functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
