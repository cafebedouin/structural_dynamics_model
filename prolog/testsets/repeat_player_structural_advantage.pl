% ============================================================================
% CONSTRAINT STORY: repeat_player_structural_advantage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_repeat_player_structural_advantage, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: repeat_player_structural_advantage
 *   human_readable: Repeat Player Structural Advantage in Arbitration
 *   domain: labor_law/dispute_resolution/corporate_governance
 *
 * SUMMARY:
 *   The repeat player structural advantage in employment arbitration is a
 *   mathematical consequence of asymmetric case volume between employers and
 *   workers. Employers interact with the same arbitrators repeatedly across
 *   hundreds of cases; individual workers have typically one case in their
 *   lifetime. This frequency asymmetry creates financial and reputational
 *   dependencies that systematically favor the repeat player. The empirical
 *   signature is a 16.6% decline in award amounts per employer-arbitrator
 *   pairing — a measurable drift toward the repeat player's interests as the
 *   relationship deepens. This constraint is classified as a mountain because
 *   it emerges naturally from the structure of market-based dispute
 *   resolution with asymmetric participation rates. No policy created it; no
 *   single actor enforces it; it persists across jurisdictions and legal
 *   frameworks. Marc Galanter's 1974 'Why the Haves Come Out Ahead'
 *   formalized this as a general property of legal systems: repeat players
 *   develop expertise, can play for rules rather than outcomes, have lower
 *   per-case stakes, and create informal relationships with adjudicators. The
 *   arbitration context makes the effect measurable because arbitrator
 *   selection is observable and award amounts are quantifiable. The
 *   constraint has near-zero suppression (0.03) because no active enforcement
 *   is required — the advantage emerges from voluntary arbitrator selection
 *   and implicit incentive alignment. Accessibility collapse is high (0.92)
 *   because the frequency asymmetry is baked into the employer-worker
 *   relationship: employers will always have more cases than individual
 *   workers. Resistance is low (0.08) because attempts to eliminate the
 *   advantage (randomized arbitrator assignment, prohibiting repeat pairings,
 *   public adjudication) require abandoning the private arbitration system
 *   entirely.
 *
 * KEY AGENTS:
 *   - Individual Worker: One-shot player (powerless/trapped) — cannot replicate employer's repeat interaction advantage; perceives advantage as immutable system feature
 *   - Employer Organization: Repeat player (institutional/arbitrage) — benefits from frequency asymmetry but experiences it as natural market dynamic, not designed extraction
 *   - Arbitrator: Market participant (moderate/constrained) — financially dependent on repeat business; implicit bias emerges from rational incentive response
 *   - Labor Union: Organized collective (organized/mobile) — can route disputes away from individual arbitration but cannot eliminate the structural advantage within it
 *   - Analytical Observer: System theorist (analytical/analytical) — sees repeat player advantage as mathematical consequence of asymmetric participation in market-based adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(repeat_player_structural_advantage, 0.22).
domain_priors:suppression_score(repeat_player_structural_advantage, 0.03).
domain_priors:theater_ratio(repeat_player_structural_advantage, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(repeat_player_structural_advantage, extractiveness, 0.22).
narrative_ontology:constraint_metric(repeat_player_structural_advantage, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(repeat_player_structural_advantage, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(repeat_player_structural_advantage, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(repeat_player_structural_advantage, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(repeat_player_structural_advantage, mountain).
narrative_ontology:human_readable(repeat_player_structural_advantage, "Repeat Player Structural Advantage in Arbitration").
narrative_ontology:topic_domain(repeat_player_structural_advantage, "labor_law/dispute_resolution/corporate_governance").

domain_priors:emerges_naturally(repeat_player_structural_advantage).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (MOUNTAIN) — The repeat player advantage appears as an immutable structural feature of any dispute resolution system where one party has higher case volume than the other. The worker cannot change the frequency asymmetry — they have one case, the employer has hundreds. This is perceived as a law of the system, not a policy choice.
constraint_indexing:constraint_classification(repeat_player_structural_advantage, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYER ORGANIZATION (MOUNTAIN) — The employer experiences the repeat player effect as an inevitable consequence of scale. Large organizations naturally interact with the same arbitrators more frequently than individual workers. The advantage emerges from the structure of the arbitration market, not from deliberate design. Perceived as unchangeable market dynamics.
constraint_indexing:constraint_classification(repeat_player_structural_advantage, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ARBITRATOR (MOUNTAIN) — The arbitrator experiences financial dependency on repeat business as a structural feature of private dispute resolution markets. The 16.6% award decline per pairing is not conscious bias but an emergent property of implicit incentive alignment. The arbitrator cannot unilaterally exit this dynamic without leaving the profession.
constraint_indexing:constraint_classification(repeat_player_structural_advantage, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical perspective, the repeat player advantage is a mathematical consequence of asymmetric case volume in any market-based adjudication system. Marc Galanter's 1974 formalization showed this is a structural property of legal systems, not specific to arbitration. The 16.6% decline per pairing is an empirical measurement of a theoretically predicted effect. This is a genuine mountain — the constraint emerges from the interaction of rational actors in an asymmetric information environment.
constraint_indexing:constraint_classification(repeat_player_structural_advantage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: LABOR UNION (MOUNTAIN) — Even organized labor with collective bargaining power cannot eliminate the repeat player advantage in individual arbitration — they can only route disputes through different mechanisms (union grievance procedures, collective arbitration). The structural asymmetry persists in any one-shot vs repeat player interaction. Unions see this as a fundamental reason to avoid individual arbitration, not as a fixable feature of it.
constraint_indexing:constraint_classification(repeat_player_structural_advantage, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(repeat_player_structural_advantage_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(repeat_player_structural_advantage, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(repeat_player_structural_advantage, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(repeat_player_structural_advantage, ExtMetricName, E),
    domain_priors:suppression_score(repeat_player_structural_advantage, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(repeat_player_structural_advantage),
    narrative_ontology:constraint_metric(repeat_player_structural_advantage, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(repeat_player_structural_advantage, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(repeat_player_structural_advantage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low but non-zero. The 16.6% award decline per pairing represents real extraction — workers receive systematically lower awards as employer-arbitrator relationships deepen. However, the extraction is not the primary function of the arbitration system (which exists to resolve disputes), and the magnitude is moderate compared to pure extraction mechanisms. The value reflects that this is an emergent side effect of a coordination system, not a designed extraction mechanism. Suppression (0.03): Near-zero. No active enforcement is required. The advantage emerges from voluntary arbitrator selection and rational response to financial incentives. Workers are not prevented from accessing arbitration; they simply face a structural disadvantage within it. Theater ratio (0.15): Low. The arbitration process is functional — disputes are resolved, awards are issued, decisions are binding. The repeat player advantage is a bias in outcomes, not a replacement of function with performance. Accessibility collapse (0.92): Very high. The frequency asymmetry is inherent to the employer-worker relationship. Employers will always have more cases than individual workers because employers interact with many workers while workers interact with one employer. This asymmetry cannot be eliminated without fundamentally changing the structure of employment relationships. Resistance (0.08): Very low. Attempts to eliminate the advantage within private arbitration (randomized assignment, repeat pairing limits) face strong resistance from arbitration providers and employers, and may be legally constrained by arbitration agreements. The only effective resistance is to abandon private arbitration entirely in favor of public adjudication, which faces different structural and political barriers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap — all agents classify it as mountain. The uniformity is diagnostically significant: it demonstrates that some structural advantages are genuinely immutable within a given institutional framework. The worker, employer, arbitrator, union, and analytical observer all perceive the repeat player advantage as an unchangeable consequence of asymmetric case volume. The gap that does exist is between the analytical mountain classification and the false summit detector: if an agent claims this is a mountain of nature (inherent to all dispute resolution) rather than a mountain of institutional design (specific to private, market-based arbitration), the detector should flag it. The advantage is a genuine mountain within the private arbitration framework, but the framework itself is a policy choice. The distinction matters: you cannot eliminate the repeat player advantage while keeping private arbitration, but you can eliminate it by moving to public adjudication with randomly assigned judges. The mountain is real, but its domain is bounded.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no declared beneficiaries or victims because the repeat player advantage emerges naturally from the structure of the system rather than being designed to benefit or harm specific groups. All perspectives classify as mountain because all agents — workers, employers, arbitrators, unions, and analytical observers — perceive the frequency asymmetry as an immutable structural feature. The worker cannot increase their case volume to match the employer. The employer cannot reduce their case volume without ceasing to be a large organization. The arbitrator cannot eliminate financial dependency on repeat business without leaving the private arbitration market. The union can route disputes to different mechanisms but cannot eliminate the advantage within individual arbitration. The analytical observer sees the advantage as a mathematical consequence of asymmetric participation rates in any market-based adjudication system. Because this is a uniform-type constraint (mountain from all perspectives), no beneficiary/victim declarations are needed, and directionality values are not computed. The constraint's low extractiveness (0.22) reflects that the repeat player advantage is a side effect of a coordination mechanism (dispute resolution) rather than a designed extraction system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low extractiveness (0.22) combined with natural emergence and high accessibility collapse produces a genuine mountain classification. The repeat player advantage is not mislabeled coordination (it does extract — 16.6% award decline is real harm to workers) and it is not mislabeled extraction (it emerges naturally from market structure, not from designed coercion). It is a structural feature of asymmetric participation in market-based adjudication — a mathematical consequence that no single actor can eliminate. The mountain classification is justified by: (1) Natural emergence — no policy created the advantage; it arises from rational actors responding to incentives in an asymmetric information environment. (2) High accessibility collapse — the frequency asymmetry cannot be eliminated without changing the fundamental structure of employment (employers will always have more cases than individual workers). (3) Low resistance — attempts to eliminate the advantage within private arbitration face strong barriers; the only effective solution is to abandon the framework entirely. (4) Low suppression — no active enforcement is required; the advantage emerges from voluntary choices. The constraint demonstrates that mountains can have non-zero extractiveness when the extraction is an emergent property of an immutable structure rather than a designed mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(repeat_player_structural_advantage, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(repeat_player_structural_advantage, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The repeat player advantage is a general structural property formalized by Galanter (1974). The employment arbitration context is one instantiation. Other instantiations include: consumer arbitration (credit card companies vs cardholders), medical malpractice (hospitals vs patients), insurance disputes (insurers vs policyholders). Each context could be modeled as a separate constraint story with its own empirical measurements, but all share the same underlying mountain structure: asymmetric case volume creates systematic advantage for the repeat player in any market-based adjudication system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
