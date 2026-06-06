% ============================================================================
% CONSTRAINT STORY: supreme_emergency_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supreme_emergency_threshold, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supreme_emergency_threshold
 *   human_readable: Supreme Emergency Threshold in Just War Theory
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   Michael Walzer's supreme emergency doctrine underwent a dramatic
 *   narrowing between 1973 (Just and Unjust Wars) and 2004 (Arguing About
 *   War). The 1973 formulation permitted emergency overrides for cases like
 *   torture to prevent hundreds of deaths, bribery of enemy officials, or
 *   civilian bombing to shorten wars. The 2004 formulation restricted supreme
 *   emergency to existential threats: communal death or destruction of a way
 *   of life. This narrowing is the constraint's defining structural feature.
 *   The piton hypothesis: the narrowing reveals functional atrophy rather
 *   than moral progress. As the threshold became more extreme, it retreated
 *   from governing actual cases to an unfalsifiable position that preserves
 *   just war theory's legitimacy without constraining state practice. The
 *   theater ratio (0.68) reflects that the threshold's primary function is
 *   now theoretical maintenance rather than practical constraint. The
 *   declining extractiveness (0.42→0.28) reflects that the narrowing reduced
 *   the threshold's operational scope — fewer cases fall under it, so less
 *   extraction occurs through its invocation. But the high theater ratio
 *   reveals this is not moral progress: the constraint simply stopped
 *   governing. The declining suppression (0.58→0.42) reflects that the
 *   narrowed threshold imposes less coercive force on decision-makers because
 *   it applies to fewer cases, but this is coupled with rising theater — the
 *   constraint's grip weakened not because alternatives emerged but because
 *   the threshold retreated to irrelevance.
 *
 * KEY AGENTS:
 *   - Civilian Populations Under Emergency Claims: Primary victim (powerless/trapped) — bear costs of violence legitimated by emergency threshold; no exit from emergency declarations
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures legitimacy benefits from threshold's ambiguity; can invoke or ignore as needed
 *   - Military Officer Corps: Mixed position (moderate/constrained) — constrained by professional ethics but benefits from legitimating function; bears career risk for emergency decisions
 *   - Human Rights Advocacy Coalition: Organized agents (organized/mobile) — building alternative frameworks (R2P, ICC) that bypass just war theory; see threshold as transitional
 *   - Just War Theory Tradition: Institutional actor (institutional/arbitrage) — maintains threshold theatrically to preserve tradition's legitimacy; sees own constraint as degraded
 *   - Moral Constraint Credibility: Abstract victim (powerless/trapped) — the credibility of moral limits on violence is eroded by unfalsifiable emergency exceptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supreme_emergency_threshold, 0.28).
domain_priors:suppression_score(supreme_emergency_threshold, 0.42).
domain_priors:theater_ratio(supreme_emergency_threshold, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supreme_emergency_threshold, extractiveness, 0.28).
narrative_ontology:constraint_metric(supreme_emergency_threshold, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(supreme_emergency_threshold, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supreme_emergency_threshold, piton).
narrative_ontology:human_readable(supreme_emergency_threshold, "Supreme Emergency Threshold in Just War Theory").
narrative_ontology:topic_domain(supreme_emergency_threshold, "political_philosophy/normative_ethics/applied_ethics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supreme_emergency_threshold, just_war_theorists).
narrative_ontology:constraint_beneficiary(supreme_emergency_threshold, state_security_apparatus).
narrative_ontology:constraint_victim(supreme_emergency_threshold, civilian_populations_under_emergency_claims).
narrative_ontology:constraint_victim(supreme_emergency_threshold, moral_constraint_credibility).
narrative_ontology:constraint_vindicates(supreme_emergency_threshold, moral_absolutism_untenable).
narrative_ontology:constraint_vindicates(supreme_emergency_threshold, communal_survival_trumps_individual_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (PITON) — Trapped under emergency declarations with no exit. The threshold's dramatic narrowing (1973→2004) reveals atrophy: what was once a functional constraint on state violence became a theatrical performance maintained to preserve just war theory's legitimacy. The narrowing is not strengthening — it is the constraint retreating to a position so extreme it can never be falsified. Experiences high theater ratio because the constraint no longer governs actual state practice.
constraint_indexing:constraint_classification(supreme_emergency_threshold, piton,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STATE SECURITY APPARATUS (ROPE) — Benefits from the threshold's ambiguity and narrowing. The 1973 formulation was constraining (torture to prevent hundreds of deaths is a testable standard); the 2004 formulation is permissive (communal death/way of life destruction is unfalsifiable in real time). Experiences the constraint as coordination: the threshold provides legitimating language for security decisions while imposing no real operational limits. Arbitrage exit: can invoke or ignore the threshold as needed.
constraint_indexing:constraint_classification(supreme_emergency_threshold, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY OFFICER CORPS (TANGLED ROPE) — Constrained by professional ethics and legal frameworks but also benefits from the threshold's legitimating function. The narrowing creates genuine coordination (clearer guidance on when normal rules suspend) but also extraction (officers bear career risk for emergency decisions while political leadership captures legitimacy benefits). Mixed experience: the threshold both enables and constrains professional judgment.
constraint_indexing:constraint_classification(supreme_emergency_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HUMAN RIGHTS COALITION (SCAFFOLD) — Organized agents (Amnesty, HRW, ICC) see the supreme emergency threshold as a temporary legitimation device that will be superseded by absolute prohibitions on torture and civilian targeting. The narrowing from 1973 to 2004 is interpreted as the constraint's sunset trajectory: as the threshold becomes more extreme and less applicable, it loses practical force and will eventually be replaced by categorical norms. Mobile exit: can build alternative normative frameworks (R2P, ICC statutes) that bypass just war theory entirely.
constraint_indexing:constraint_classification(supreme_emergency_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: JUST WAR THEORY TRADITION (PITON) — The supreme emergency threshold is maintained theatrically to preserve the tradition's claim to moral realism against absolutist critics. The 1973→2004 narrowing reveals functional atrophy: the threshold retreated from governing actual cases (torture, bribery, civilian bombing) to an unfalsifiable extreme (communal death) that can never be invoked without retrospective judgment. The tradition sees its own constraint as degraded — kept alive through philosophical performance rather than practical application. High theater ratio: the threshold's primary function is now preserving just war theory's legitimacy, not constraining state violence.
constraint_indexing:constraint_classification(supreme_emergency_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The threshold exhibits genuine coordination (provides shared language for emergency ethics) and genuine extraction (legitimates state violence while imposing no falsifiable constraint). The 1973→2004 trajectory is diagnostic: the narrowing is not moral progress but strategic retreat to an unfalsifiable position. The constraint's primary function has shifted from governing practice to preserving theoretical coherence. Analytical classification: tangled rope, not piton, because the coordination function (shared normative vocabulary) remains active even as the constraining function atrophies.
constraint_indexing:constraint_classification(supreme_emergency_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supreme_emergency_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supreme_emergency_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supreme_emergency_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(supreme_emergency_threshold, TR),
    TR >= 0.70.

:- end_tests(supreme_emergency_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The threshold legitimates state violence and provides cover for security decisions, but the 2004 narrowing reduced its operational scope dramatically. The 1973 formulation was substantially extractive (torture, bribery, civilian bombing all fell under it); the 2004 formulation applies to almost no real cases. The declining trajectory (0.42→0.28) reflects the narrowing's effect: less extraction because the threshold governs fewer cases. But this is not moral progress — it is functional atrophy. Suppression (0.42): Moderate. The threshold suppresses alternative moral frameworks (absolute prohibitions, civilian immunity) by claiming they are unrealistic in extremis. But suppression has declined (0.58→0.42) because the narrowed threshold applies to fewer cases and thus constrains fewer decisions. The declining suppression is coupled with rising theater — the constraint's grip weakened not because alternatives emerged but because the threshold retreated. Theater ratio (0.68): High. The threshold's primary function is now preserving just war theory's legitimacy against absolutist critics, not constraining state violence. The 1973→2004 narrowing is diagnostic: the threshold retreated from testable cases (torture to prevent hundreds of deaths) to an unfalsifiable extreme (communal death) that can never be invoked without retrospective judgment. The rising trajectory (0.35→0.68) tracks the constraint's transformation from operational standard to theoretical performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full piton signature from multiple perspectives but computes as tangled rope from the analytical position. Civilian populations and the just war theory tradition both see piton (degraded function maintained theatrically), but for opposite reasons: civilians see a constraint that no longer protects them; theorists see a constraint maintained to protect the theory. The state security apparatus sees rope (pure coordination/legitimation with no real constraint). Military officers see tangled rope (genuine coordination mixed with extraction). Human rights coalitions see scaffold (transitional device being superseded by absolute prohibitions). The analytical observer sees tangled rope rather than piton because the coordination function (providing shared language for emergency ethics) remains genuine even as the constraining function atrophies. The perspectival gap reveals that the 1973→2004 narrowing is not moral progress but strategic retreat: the threshold retreated to an unfalsifiable position to preserve theoretical coherence while abandoning practical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are full victims (d→1.0): trapped under emergency declarations with no exit, bearing maximum extraction. The state security apparatus is a full beneficiary (d→0.0): institutional power with arbitrage exit, capturing legitimacy benefits while facing no real constraint. Military officers are mixed (d≈0.4): constrained by professional ethics and legal frameworks but also benefiting from the threshold's legitimating function; the constraint both enables and extracts. Human rights coalitions are low-extraction (d≈0.2): organized agents with mobile exit, building alternative frameworks that bypass the threshold entirely. Just war theorists are beneficiaries (d→0.0): institutional actors maintaining the threshold theatrically to preserve the tradition's legitimacy. The analytical observer computes tangled rope rather than piton because the coordination function (shared normative vocabulary for emergency ethics) remains active even as the constraining function atrophies.
 *
 * MANDATROPHY ANALYSIS:
 *   The supreme emergency threshold exhibits mandatrophy in its purest form: the constraint's original mandate (providing falsifiable criteria for when normal moral rules may be suspended) has been abandoned, but the constraint persists because just war theory requires it for theoretical coherence. The 1973 formulation was operationally meaningful: torture to prevent hundreds of deaths is a testable standard that could govern actual decisions. The 2004 formulation is unfalsifiable: communal death or destruction of a way of life can only be judged retrospectively, if at all. The narrowing is not strengthening — it is the constraint retreating to a position where it can never be falsified and thus never constrain. The mandate has outlived its function: the threshold no longer governs state practice but is maintained to preserve just war theory's claim to moral realism. The piton classification from multiple perspectives (civilian populations, just war tradition) captures this mandatrophy directly. The analytical tangled rope classification reveals that some coordination function persists (shared normative vocabulary) even as the constraining function atrophies, but this residual coordination is insufficient to prevent the constraint's primary function from becoming theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    falsifiability_threshold,
    'At what point does the supreme emergency threshold become so narrow that it ceases to be a falsifiable constraint on state action?',
    'Historical analysis of invoked emergencies vs. retrospective judgments; identification of cases where the threshold was claimed but later rejected; measurement of time lag between claim and falsification',
    'If threshold is falsifiable in real time: constraint retains governing function (tangled rope). If falsifiable only retrospectively: constraint is theatrical (piton). If unfalsifiable even retrospectively: constraint is pure legitimation (snare from victim perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(falsifiability_threshold, empirical, 'Whether the threshold can be falsified in real time or only retrospectively').

omega_variable(
    narrowing_trajectory_endpoint,
    'Does the 1973→2004 narrowing trajectory continue toward absolute prohibition, or stabilize at the 2004 formulation?',
    'Longitudinal analysis of Walzer''s subsequent work and just war theory literature post-2004; tracking of invoked emergencies and their retrospective evaluation; measurement of threshold drift in academic and policy discourse',
    'If trajectory continues toward prohibition: scaffold perspective confirmed (sunset is real). If stabilizes at 2004 level: piton perspective confirmed (theatrical maintenance at unfalsifiable extreme). If widens again: reveals the narrowing was strategic rather than principled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrowing_trajectory_endpoint, empirical, 'Whether the narrowing trajectory continues, stabilizes, or reverses').

omega_variable(
    communal_death_operationalization,
    'Can ''communal death or destruction of a way of life'' be operationalized into falsifiable criteria, or is it inherently a retrospective judgment?',
    'Analysis of attempted operationalizations in policy and legal contexts; comparison of ex ante emergency claims vs. ex post evaluations; identification of cases where communal survival was genuinely at stake vs. cases where the claim was strategic',
    'If operationalizable ex ante: the 2004 threshold retains constraining function. If operationalizable only ex post: the threshold is theatrical. If inherently non-operationalizable: the threshold is pure legitimation device.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communal_death_operationalization, conceptual, 'Whether communal death can be defined in falsifiable terms').

omega_variable(
    alternative_framework_sufficiency,
    'Do alternative normative frameworks (absolute prohibitions, R2P, ICC statutes) provide sufficient guidance for emergency ethics, or does the supreme emergency threshold fill a genuine conceptual gap?',
    'Comparative analysis of emergency decision-making under different normative frameworks; identification of cases where absolute prohibitions failed to provide guidance vs. cases where supreme emergency threshold was invoked; assessment of whether the threshold adds decision-relevant information',
    'If alternatives sufficient: scaffold perspective confirmed (threshold is transitional). If threshold fills genuine gap: rope or tangled rope from more perspectives (coordination function is real). If neither sufficient: reveals deeper conceptual problem in emergency ethics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_sufficiency, conceptual, 'Whether alternative frameworks make the threshold obsolete').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supreme_emergency_threshold, 0, 31).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supreme_emerg_theater_1973, supreme_emergency_threshold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(supreme_emerg_theater_1983, supreme_emergency_threshold, theater_ratio, 10, 0.48).
narrative_ontology:measurement(supreme_emerg_theater_1993, supreme_emergency_threshold, theater_ratio, 20, 0.58).
narrative_ontology:measurement(supreme_emerg_theater_2004, supreme_emergency_threshold, theater_ratio, 31, 0.68).

% Extraction over time
narrative_ontology:measurement(supreme_emerg_extract_1973, supreme_emergency_threshold, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(supreme_emerg_extract_1983, supreme_emergency_threshold, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(supreme_emerg_extract_1993, supreme_emergency_threshold, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(supreme_emerg_extract_2004, supreme_emergency_threshold, base_extractiveness, 31, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(supreme_emerg_suppress_1973, supreme_emergency_threshold, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(supreme_emerg_suppress_2004, supreme_emergency_threshold, suppression_requirement, 31, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supreme_emergency_threshold, identity_coordination).
narrative_ontology:affects_constraint(supreme_emergency_threshold, necessity_ambiguity).

% DUAL FORMULATION NOTE:
% The supreme emergency threshold is downstream of necessity_ambiguity (the general problem of defining when necessity justifies moral override) but represents a distinct structural constraint. The upstream constraint (necessity_ambiguity) has its own extractiveness reflecting the conceptual ambiguity in necessity claims; the supreme emergency threshold has its own extractiveness reflecting the specific legitimation function in just war theory. The 1973→2004 narrowing is a response to the upstream ambiguity: by making the threshold more extreme, Walzer attempted to reduce the ambiguity, but the result was functional atrophy rather than clarification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
