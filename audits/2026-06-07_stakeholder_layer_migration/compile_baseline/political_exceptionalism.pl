% ============================================================================
% CONSTRAINT STORY: political_exceptionalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_exceptionalism, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: political_exceptionalism
 *   human_readable: Political Exceptionalism: Differential Moral Prerogatives for State Actors
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   Political exceptionalism is the doctrine that state actors possess moral
 *   prerogatives unavailable to non-state actors, particularly in emergency
 *   scenarios. The constraint is most visible in just war theory's
 *   differential treatment of state terrorism (sometimes permissible under
 *   supreme emergency) versus non-state terrorism (categorically
 *   impermissible). Michael Walzer's framework exemplifies this: he accepts
 *   that democratic states facing existential threats may target enemy
 *   civilians (British bombing of German cities in WWII) while simultaneously
 *   rejecting any parallel justification for non-state actors. The constraint
 *   operates through a claimed coordination function (providing systematic
 *   guidance for state action under emergency) that masks an extraction
 *   mechanism (asymmetric moral permissions that advantage institutional
 *   power holders). The theater_ratio (0.58) reflects that much contemporary
 *   invocation of exceptionalist logic is performative: states claim
 *   emergency status to license actions they would take anyway, and
 *   international institutions ritually condemn violations while
 *   systematically failing to constrain powerful states. The constraint has
 *   intensified over the measured interval (Westphalian origin to post-Cold
 *   War era) as states have accumulated more sophisticated justificatory
 *   frameworks while actual constraints on state violence have weakened.
 *
 * KEY AGENTS:
 *   - State Actors: Primary beneficiaries (institutional/arbitrage) — possess asymmetric moral permissions to act under emergency; can invoke or decline exceptionalist logic strategically
 *   - Non-State Actors: Primary victims (powerless/trapped) — denied the same moral justifications available to states for identical acts; trapped in asymmetric evaluation framework
 *   - Civilian Populations Under Emergency Rule: Direct victims (powerless/trapped) — subject to violence that exceptionalist logic licenses for states but would prohibit for non-state actors
 *   - International Legal Order: Mixed position (institutional/constrained) — benefits from coordination function (states accept international law because it preserves their prerogatives) but corrupted by the exception (undermines universality of human rights)
 *   - Just War Theorists: Moderate actors (moderate/constrained) — benefit from systematic framework but bear reputational costs when internal contradictions are exposed
 *   - Cosmopolitan Human Rights Coalition: Organized opposition (organized/mobile) — building alternative frameworks that reject state exceptionalism; sees constraint as temporary
 *   - UN Security Council: Institutional embodiment (institutional/constrained) — exceptionalist structure has atrophied into theater; veto power and Chapter VII authority now primarily performative
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees pure extraction mechanism masked by coordination rhetoric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_exceptionalism, 0.68).
domain_priors:suppression_score(political_exceptionalism, 0.72).
domain_priors:theater_ratio(political_exceptionalism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_exceptionalism, extractiveness, 0.68).
narrative_ontology:constraint_metric(political_exceptionalism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(political_exceptionalism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_exceptionalism, rope).
narrative_ontology:human_readable(political_exceptionalism, "Political Exceptionalism: Differential Moral Prerogatives for State Actors").
narrative_ontology:topic_domain(political_exceptionalism, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(political_exceptionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_exceptionalism, state_actors).
narrative_ontology:constraint_beneficiary(political_exceptionalism, political_leaders).
narrative_ontology:constraint_beneficiary(political_exceptionalism, institutional_violence_monopolists).
narrative_ontology:constraint_victim(political_exceptionalism, non_state_actors).
narrative_ontology:constraint_victim(political_exceptionalism, civilian_populations_under_emergency_rule).
narrative_ontology:constraint_victim(political_exceptionalism, insurgent_groups).
narrative_ontology:constraint_victim(political_exceptionalism, moral_consistency_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STATE ACTOR (SNARE) — Trapped within a moral framework that denies them the same justifications available to state actors. Cannot exit the asymmetric moral evaluation: identical acts (targeting civilians under supreme emergency) are terrorism when performed by non-state actors but permissible state action when performed by political leaders. Bears full extraction of the double standard with no recourse.
constraint_indexing:constraint_classification(political_exceptionalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVILIAN UNDER EMERGENCY RULE (SNARE) — Immediate victim of the exceptionalist logic. Trapped in the zone where normal moral constraints are suspended for state actors. The constraint licenses violence against them that would be impermissible if performed by non-state actors. No exit from the emergency zone; no appeal to the moral principles that protect them in non-emergency contexts.
constraint_indexing:constraint_classification(political_exceptionalism, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STATE ACTOR IN EMERGENCY (ROPE) — Primary beneficiary. Experiences the constraint as coordination: the exceptionalist framework solves the genuine problem of how to act under supreme emergency when normal moral rules produce catastrophic outcomes. Arbitrage-level exit: can invoke or decline the exception as strategically optimal. Net beneficiary of the asymmetric moral permission structure.
constraint_indexing:constraint_classification(political_exceptionalism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL ORDER (TANGLED ROPE) — Constrained by the need to maintain state sovereignty norms while also constraining state violence. Benefits from the coordination function (states accept international law because it preserves their exceptionalist prerogatives) but also bears costs (the exception undermines the universality of human rights law). Mixed extraction: the system both enables and is corrupted by the exceptionalist logic.
constraint_indexing:constraint_classification(political_exceptionalism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: JUST WAR THEORIST (TANGLED ROPE) — Moderate power, constrained exit. Benefits from the exceptionalist framework's coordination function (provides a systematic account of permissible state violence) but also bears reputational and epistemic costs when the framework's internal contradictions are exposed. Walzer's acceptance of state terrorism under supreme emergency while rejecting non-state terrorism reveals the extraction: the theory serves state interests rather than moral consistency.
constraint_indexing:constraint_classification(political_exceptionalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COSMOPOLITAN HUMAN RIGHTS COALITION (SCAFFOLD) — Organized agents building alternative frameworks that reject state exceptionalism. Sees the constraint as temporary: the Westphalian state system and its moral privileges are eroding under pressure from international criminal law, universal jurisdiction, and cosmopolitan norms. The sunset is the transition from state-centric to individual-centric moral and legal frameworks. Mobile exit: can operate in transnational networks that bypass state sovereignty.
constraint_indexing:constraint_classification(political_exceptionalism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: UN SECURITY COUNCIL (PITON) — The institutional embodiment of state exceptionalism has atrophied into theater. The veto power and Chapter VII authority were designed to coordinate great power action under genuine emergency, but now function primarily as performative legitimation for actions states would take anyway. The constraint persists through institutional inertia and great power investment in the status quo, not because it effectively coordinates international security.
constraint_indexing:constraint_classification(political_exceptionalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a universal civilizational perspective, political exceptionalism is a pure extraction mechanism. The claimed coordination function (solving supreme emergency dilemmas) is cover for asymmetric moral permission that systematically advantages state actors. The analytical observer sees that identical acts receive opposite moral evaluations based solely on the actor's institutional status, not on morally relevant features of the act itself. This is the structural signature of extraction, not coordination.
constraint_indexing:constraint_classification(political_exceptionalism, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_exceptionalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_exceptionalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_exceptionalism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_exceptionalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_exceptionalism, TR),
    TR >= 0.70.

:- end_tests(political_exceptionalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. State actors capture substantial asymmetric moral permissions that non-state actors are denied. The extraction is not total (some constraints on state violence persist, and some non-state actors gain legitimacy through other pathways) but is severe and systematic. The value reflects that identical acts receive opposite moral evaluations based solely on institutional status. Suppression (0.72): High. Non-state actors face severe barriers to challenging the asymmetric framework: they lack institutional platforms, their moral claims are dismissed as self-serving, and attempts to invoke parallel justifications are treated as evidence of moral depravity rather than as legitimate arguments. The suppression has increased over the interval as states have professionalized the justificatory apparatus (legal advisors, ethics boards, humanitarian law compliance theater). Theater ratio (0.58): Moderate-high. Much contemporary invocation of exceptionalist logic is performative: states claim emergency status to license actions they would take anyway (War on Terror, humanitarian intervention, responsibility to protect), and international institutions ritually condemn violations while systematically failing to constrain powerful states. The theater has increased as the gap between formal constraints and actual state behavior has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. State actors experience it as coordination (Rope) — the exceptionalist framework solves the genuine problem of how to act under supreme emergency. Non-state actors experience it as pure extraction (Snare) — identical acts are terrorism when they perform them but permissible state action when states perform them. The international legal order experiences it as mixed (Tangled Rope) — the system both enables and is corrupted by the exceptionalist logic. The cosmopolitan coalition sees it as temporary (Scaffold) — Westphalian sovereignty is eroding. The UN Security Council sees its own degraded ritual (Piton) — the institutional embodiment persists through inertia. The analytical observer sees pure extraction (Snare) — the coordination story is cover for asymmetric moral permissions that advantage institutional power. The gap between the state actor's Rope and the non-state actor's Snare is the structural signature of the constraint's extractive core.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors are primary beneficiaries with arbitrage-level exit options — they can invoke or decline exceptionalist logic as strategically optimal, and they capture the asymmetric moral permissions the framework provides. The engine derives low d (beneficiary + arbitrage) → low or negative chi (experienced as coordination). Non-state actors are primary victims with trapped exit options — they cannot exit the asymmetric evaluation framework and bear the full cost of the double standard. The engine derives high d (victim + trapped) → high chi (experienced as pure extraction). The international legal order is both beneficiary (states accept international law because it preserves their prerogatives) and victim (the exception undermines universality), with constrained exit — moderate d → moderate chi (experienced as tangled rope). Just war theorists are moderate actors with constrained exit (reputational investment in the framework) — moderate d → moderate chi. The cosmopolitan coalition is organized with mobile exit (can operate in transnational networks) — low d → low chi (experienced as scaffold with sunset). The UN Security Council is institutional with constrained exit (great power investment in status quo) — moderate d but high theater → piton classification from theater gate rather than chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by revealing that the claimed coordination function (solving supreme emergency dilemmas) and the extraction mechanism (asymmetric moral permissions for state actors) are structurally inseparable in the current framework. The coordination story is not pure cover — there are genuine supreme emergency dilemmas where normal moral rules produce catastrophic outcomes. But the solution is not neutral coordination; it is coordination that systematically advantages institutional power holders. The Walzerian framework exemplifies this: it provides systematic guidance for state action (coordination function) while denying parallel justifications to non-state actors (extraction mechanism). The mandatrophy is not 'is this coordination or extraction?' but 'how much of the coordination function could be preserved in a non-exceptionalist framework?' The analytical observer's Snare classification suggests: very little of the current coordination function is genuinely necessary; most of it is naturalization of state power. The cosmopolitan coalition's Scaffold classification suggests: the coordination function can be achieved through alternative frameworks (cosmopolitan ethics, international criminal law, universal jurisdiction) that do not require asymmetric moral permissions. The constraint is a tangled rope at minimum, and likely a snare from most non-state perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supreme_emergency_threshold,
    'What constitutes a genuine supreme emergency that would justify suspending normal moral constraints, and who has epistemic authority to make that determination?',
    'Historical analysis of claimed emergencies vs. actual existential threats; correlation between emergency declarations and state interests; comparative analysis of state vs. non-state emergency claims',
    'If threshold is objective and verifiable: exceptionalism might be legitimate coordination (Rope from more perspectives). If threshold is subjective and state-determined: exceptionalism is extraction mechanism (Snare from more perspectives). Current evidence suggests states systematically over-claim emergency status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supreme_emergency_threshold, empirical, 'Threshold and epistemic authority for genuine supreme emergency').

omega_variable(
    moral_relevance_of_institutional_status,
    'Is there a morally relevant difference between state and non-state actors that justifies differential moral permissions, or is institutional status morally arbitrary?',
    'Philosophical analysis of proposed justifications (democratic legitimacy, monopoly on violence, responsibility to protect); examination of whether these features track morally relevant properties or merely institutional power',
    'If institutional status tracks morally relevant features: exceptionalism is coordination (Rope). If institutional status is morally arbitrary: exceptionalism is extraction (Snare). The Walzerian framework assumes the former but provides no independent argument for it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_relevance_of_institutional_status, conceptual, 'Whether institutional status is morally relevant or arbitrary').

omega_variable(
    coordination_vs_naturalization,
    'Does political exceptionalism solve a genuine coordination problem (how to act under supreme emergency) or does it naturalize state power by presenting contingent institutional arrangements as moral necessities?',
    'Comparison of exceptionalist frameworks with non-exceptionalist alternatives (cosmopolitan ethics, anarchist ethics, pacifism); assessment of whether the coordination function could be achieved without asymmetric moral permissions',
    'If coordination function requires exceptionalism: Tangled Rope or Rope from more perspectives. If coordination function is achievable without exceptionalism: current framework is Snare (naturalizes state power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_naturalization, conceptual, 'Whether exceptionalism solves coordination or naturalizes power').

omega_variable(
    sunset_timeline_for_westphalian_sovereignty,
    'Is the erosion of state sovereignty and its moral privileges a genuine structural trend or aspirational rhetoric?',
    'Longitudinal analysis of international criminal prosecutions, universal jurisdiction cases, humanitarian intervention patterns, and state compliance with cosmopolitan norms; measurement of whether state exceptionalist claims are becoming less defensible over time',
    'If erosion is real and accelerating: Scaffold perspective confirmed (sunset within 50-100 years). If erosion is stalled or reversing: Scaffold perspective is aspirational, and the constraint is stable Snare or Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_timeline_for_westphalian_sovereignty, empirical, 'Timeline for erosion of Westphalian sovereignty norms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_exceptionalism, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polexcept_theater_westphalian_origin, political_exceptionalism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(polexcept_theater_post_wwii, political_exceptionalism, theater_ratio, 100, 0.45).
narrative_ontology:measurement(polexcept_theater_cold_war, political_exceptionalism, theater_ratio, 200, 0.52).
narrative_ontology:measurement(polexcept_theater_post_cold_war, political_exceptionalism, theater_ratio, 300, 0.58).

% Extraction over time
narrative_ontology:measurement(polexcept_extract_westphalian_origin, political_exceptionalism, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(polexcept_extract_post_wwii, political_exceptionalism, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(polexcept_extract_cold_war, political_exceptionalism, base_extractiveness, 200, 0.65).
narrative_ontology:measurement(polexcept_extract_post_cold_war, political_exceptionalism, base_extractiveness, 300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(polexcept_suppress_westphalian_origin, political_exceptionalism, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(polexcept_suppress_post_wwii, political_exceptionalism, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(polexcept_suppress_cold_war, political_exceptionalism, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(polexcept_suppress_post_cold_war, political_exceptionalism, suppression_requirement, 300, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_exceptionalism, enforcement_mechanism).
narrative_ontology:affects_constraint(political_exceptionalism, state_sovereignty_doctrine).
narrative_ontology:affects_constraint(political_exceptionalism, monopoly_on_legitimate_violence).
narrative_ontology:affects_constraint(political_exceptionalism, just_war_theory_framework).
narrative_ontology:affects_constraint(political_exceptionalism, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% Political exceptionalism is the meta-constraint that grounds multiple specific doctrines (state sovereignty, monopoly on violence, just war theory, humanitarian intervention). Each downstream constraint inherits the exceptionalist logic but has its own extractiveness value reflecting its specific domain. The upstream constraint (political exceptionalism) has high extractiveness reflecting the general asymmetric moral permission structure; downstream constraints may have lower extractiveness if they impose genuine constraints on state action within the exceptionalist framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
