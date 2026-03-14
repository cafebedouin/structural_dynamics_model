% ============================================================================
% CONSTRAINT STORY: governance_latency_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance_latency_tradeoff, []).

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
 *   constraint_id: governance_latency_tradeoff
 *   human_readable: Governance Latency Tradeoff: Speed vs. Legitimacy
 *   domain: political_economy/institutional_design
 *
 * SUMMARY:
 *   The governance latency tradeoff represents a fundamental structural
 *   tension in institutional design: decision-making speed and stakeholder
 *   legitimacy appear to vary inversely. Rapid crisis response requires
 *   concentrated authority and accelerated procedures, which exclude
 *   deliberative input. Inclusive deliberation requires time for multiple
 *   stakeholders to participate, which delays response. This constraint
 *   exhibits all six classification types depending on structural position,
 *   and reveals how the same governance mechanism appears as natural law to
 *   one observer, coordination solution to another, and pure extraction to a
 *   third. The extractiveness value has risen over the interval (0.35 → 0.58)
 *   as crisis declarations have become more frequent and less temporally
 *   bounded, while theater ratio has increased (0.48 → 0.71) as procedural
 *   justifications have multiplied to cover increasingly routine uses of
 *   emergency authority. The constraint is a tangled rope: genuine
 *   coordination function (crisis response requires speed) exists alongside
 *   genuine extraction (stakeholders bear costs of exclusion with no exit
 *   option). The scaffold perspective identifies a structural sunset:
 *   deliberative procedures can be materially accelerated, emergency powers
 *   can be explicitly time-limited, and post-crisis review can restore
 *   legitimacy. The piton perspective identifies degradation: emergency
 *   procedures designed for exceptional threats have become permanent
 *   institutional features, maintained through recurring invocation of new
 *   crises.
 *
 * KEY AGENTS:
 *   - Executive Authority: Primary beneficiary (institutional/arbitrage) — captures decision authority and speed advantage during latency window; can arbitrage between rapid action and deliberative consensus
 *   - Crisis Response Apparatus: Secondary beneficiary (organized/mobile) — justifies existence and budget through emergency framework; benefits from expanded authority scope
 *   - Excluded Stakeholders: Primary victim (powerless/trapped) — face coercive imposition of policies designed without their input; no exit from governance decisions affecting them
 *   - Deliberative Legitimacy: Secondary victim (powerless/trapped) — abstract institutional value that cannot organize; systematically degraded by speed-prioritizing design
 *   - Procedural Reform Coalition: Organized agents (organized/constrained) — civil society, legal advocates, legislative reformers building alternative procedures with real acceleration capacity and explicit sunset logic
 *   - Institutional Apparatus for Perpetual Emergency: Institutional actor (institutional/arbitrage) — sees emergency procedures as normalization; maintains theatrical justifications to cover routine uses of exceptional authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional choice as immutable tradeoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance_latency_tradeoff, 0.52).
domain_priors:suppression_score(governance_latency_tradeoff, 0.58).
domain_priors:theater_ratio(governance_latency_tradeoff, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance_latency_tradeoff, extractiveness, 0.52).
narrative_ontology:constraint_metric(governance_latency_tradeoff, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(governance_latency_tradeoff, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance_latency_tradeoff, tangled_rope).
narrative_ontology:human_readable(governance_latency_tradeoff, "Governance Latency Tradeoff: Speed vs. Legitimacy").
narrative_ontology:topic_domain(governance_latency_tradeoff, "political_economy/institutional_design").

domain_priors:requires_active_enforcement(governance_latency_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance_latency_tradeoff, executive_authority).
narrative_ontology:constraint_beneficiary(governance_latency_tradeoff, crisis_response_apparatus).
narrative_ontology:constraint_victim(governance_latency_tradeoff, deliberative_legitimacy).
narrative_ontology:constraint_victim(governance_latency_tradeoff, minority_stakeholder_voice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED STAKEHOLDER (SNARE) — Stakeholders without seat at decision table face latency extraction: rapid executive action to resolve crisis excludes deliberative input. No exit option from policies that affect them. Experience maximum extraction — coercive imposition under time pressure justifies bypassing consent mechanisms. Suppression maintained through emergency framing.
constraint_indexing:constraint_classification(governance_latency_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED PUBLIC (TANGLED ROPE) — Constrained by the governance process itself but benefits from crisis resolution. Faces costs (exclusion, imposed policies) alongside benefits (stability, functional response). Medium extraction with genuine coordination requirement — the speed constraint solves real coordination problems but at asymmetric cost. Biographical horizon shows higher extraction; generational view accommodates longer-term benefits.
constraint_indexing:constraint_classification(governance_latency_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE AUTHORITY (ROPE) — Beneficiary. Experiences latency tradeoff as pure coordination solution: rapid decision-making enables response to crisis. Can arbitrage between faster executive action and slower deliberative consensus. Faces no meaningful constraints on exit — authority to bypass consensus is the mechanism's core function. Low experienced extraction — extraction runs toward this agent.
constraint_indexing:constraint_classification(governance_latency_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROCEDURAL REFORM COALITION (SCAFFOLD) — Organized actors (civil society groups, legal advocates, legislative reformers) see the latency tradeoff as a temporary institutional design failure with a sunset: emergency procedures can be time-limited, deliberative mechanisms can be accelerated without full abandonment, and post-crisis review processes can restore legitimacy deficit. This perspective requires has_sunset_clause: true — the emergency authorization must include explicit expiration and review triggers.
constraint_indexing:constraint_classification(governance_latency_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PERPETUAL EMERGENCY APPARATUS (PITON) — Institutional mechanisms designed as temporary (emergency powers, executive orders, crisis protocols) persist beyond their original crisis due to inertia, recurring threats, or political convenience. Theater ratio high: much procedural activity around 'managing' emergency legitimacy (oversight hearings, impact statements, review processes) performs legitimacy without changing underlying speed prioritization. The apparatus has become degraded — maintained because permanent replacement structures haven't emerged, not because it functions as designed.
constraint_indexing:constraint_classification(governance_latency_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, governance faces an apparent mathematical tradeoff: the time required for democratic deliberation cannot be compressed below some threshold without degrading legitimacy or stakeholder input capacity. This appears as an immutable constraint of collective decision-making — a natural law of politics. However, the structural data reveals this as a false summit: the tradeoff is contingent on institutional design choices (voting procedures, information access, representation rules) and crisis framing authority, not on any fundamental law.
constraint_indexing:constraint_classification(governance_latency_tradeoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_latency_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance_latency_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_latency_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance_latency_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance_latency_tradeoff, TR),
    TR >= 0.70.

:- end_tests(governance_latency_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The speed-legitimacy tradeoff enables executive authority to bypass deliberative consensus, capturing benefits of rapid response. But the extraction is not maximal (0.70+) because genuine coordination problems exist — crises do require faster response than standard procedures allow, and the constraint solves this real problem. The rise over the interval reflects not increased legitimacy of the tradeoff but increased scope creep: emergency procedures invoked for recurring, foreseeable problems that don't justify speed-legitimacy tradeoff. Suppression (0.58): Moderate-high. Stakeholders face significant barriers to exit from governance decisions. Barriers include lack of representation in emergency procedures, legal prohibition on refusing imposed policies, institutional unavailability of alternative governance structures, and information asymmetry (executives control crisis framing). Suppression is not total because some stakeholders can organize resistance (civil society, legislative action), and procedural workarounds exist (emergency review hearings, appeal processes). Theater ratio (0.64): Moderate-high. Significant procedural activity performs legitimacy without changing underlying speed prioritization. Review hearings, impact assessments, stakeholder notification procedures, and post-crisis inquiries create appearance of inclusion while decisions remain executive-driven. Theater has increased as the apparatus has normalized — more justification required to cover routine invocation of exception status.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how identical structural data produces divergent classifications from different positions. The executive authority sees pure coordination — latency is a real problem that deliberation cannot solve, speed is a necessary tool. The excluded stakeholder sees pure extraction — they bear costs of exclusion with no benefit and no voice. The affected public sees tangled rope — they benefit from crisis resolution but also bear legitimacy costs from exclusion. The reform coalition sees a temporary problem with fixable institutional design — latency is a choice, not a law. The perpetual emergency apparatus sees its own degradation — the procedures persist through inertia and routine crisis invocation, not because they work as designed. The analytical observer sees an immutable natural law — governance speed and legitimacy cannot be simultaneously maximized — but the structural data reveals this as false summit: actual constraints are institutional design choices (voting procedures, representation structures, crisis definition authority), not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective flows from structural position relative to extraction. Executive authority benefits during latency window (d ≈ 0.10, low d, arbitrage exit produces negative effective extraction). Excluded stakeholders bear all costs with no exit (d ≈ 0.95, high d, trapped exit produces maximum experienced extraction). Affected public caught in middle — benefits from crisis response but excluded from deliberation (d ≈ 0.65, moderate d, constrained exit produces moderate experienced extraction). Reform coalition has agency and exit paths through procedural change (d ≈ 0.50, neutral d, constrained but organized exit produces moderate extraction). Perpetual emergency apparatus benefits from perpetuation (d ≈ 0.15, low d, arbitrage exit through continued crisis invocation produces low extraction). Analytical observer occupies measurement position, not structural position (d ≈ 0.72 by canonical value). The engine derives these d values from beneficiary/victim declarations plus exit options; commentary confirms structural logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the 'latency tradeoff' is not an immutable law but a contingent institutional choice with real alternatives. The mountain perspective (natural law of politics) is a false summit, naturalized by language ('you cannot do both speed and legitimacy'). The tangled rope classification is structurally correct — genuine coordination function (speed solves crisis problems) coexists with genuine extraction (stakeholders bear costs of exclusion). The scaffold perspective identifies the structural exit: accelerated deliberative procedures, explicit emergency sunset clauses, and standing crisis consultation bodies can materially reduce the apparent tradeoff. The piton perspective identifies degradation: emergency procedures designed for exceptional circumstances have become normalized, maintained through recurring invocation of new crises rather than through justifiable emergency need. The snare perspective (powerless excluded stakeholder) identifies the asymmetric extraction that makes this tangled rope rather than pure rope. Mandatrophy is resolved by mapping the false law (immutable speed-legitimacy tradeoff) to its institutional contingencies (crisis definition authority, representation structure, sunset clause design), revealing it as a choice that could be redesigned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_legitimacy_threshold,
    'What response time is necessary for a governance action to qualify as ''crisis response'' vs. political opportunism disguised as urgency?',
    'Comparative analysis of crisis definitions across jurisdictions; post-crisis assessment of whether speed was empirically necessary or politically convenient; tracking false urgency claims in historical retrospectives',
    'If threshold is low (hours): legitimate emergency powers expand dramatically, enabling extraction via false urgency. If threshold is high (weeks): genuine crises may be mishandled by deliberative delays. Classification shifts from Snare toward Tangled Rope as legitimacy criteria tighten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_legitimacy_threshold, empirical, 'Crisis legitimacy threshold for urgency claims').

omega_variable(
    deliberative_acceleration_feasibility,
    'Can deliberative procedures be materially accelerated (parallel input streams, pre-drafted alternatives, standing advisory bodies) without degrading legitimacy or stakeholder capacity to participate meaningfully?',
    'Case studies of accelerated procedures (expedited legislation, emergency consultation processes); measurement of stakeholder comprehension and participation rates; comparison of legitimacy deficits between accelerated and bypassed procedures',
    'If feasible: latency tradeoff is not fundamental but a choice to privilege speed over legitimacy. Scaffold perspective confirmed. If infeasible: speed-legitimacy tradeoff is structural. Snare classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberative_acceleration_feasibility, empirical, 'Feasibility of accelerating deliberative procedures without legitimacy loss').

omega_variable(
    crisis_recurrence_pattern,
    'Are invoked ''emergency'' powers actually used only for unprecedented threats, or do they accumulate around recurring, foreseeable problems that could be solved through normal governance?',
    'Longitudinal tracking of emergency declarations; categorization by crisis type (novel threat vs recurring pattern); measurement of time gap between crisis resolution and sunset clause activation',
    'If mostly novel threats: Piton classification is incorrect — temporary powers remain truly temporary. If mostly recurring: Piton classification confirmed — emergency authorization persists due to institutional inertia around foreseeable problems. Reveals whether apparatus is degraded or functioning as extended tool.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crisis_recurrence_pattern, empirical, 'Pattern of emergency power use: novel threats vs recurring problems').

omega_variable(
    stakeholder_substitution_mechanism,
    'In deliberative exclusions, are affected stakeholders'' interests represented by proxy (elected officials, administrative bodies, advocacy groups) or are they genuinely absent from decision-making?',
    'Analysis of proxy representation structures; measurement of correlation between proxy positions and excluded stakeholder preferences (surveys, retrospective assessment); evidence of proxy capture by other interests',
    'If proxy representation is accurate: extraction is reduced — Tangled Rope classification holds. If proxy capture occurs: victims'' interests diverge from proxy positions — Snare classification strengthened. Determines whether constraint is hybrid coordination-extraction or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_substitution_mechanism, empirical, 'Accuracy of proxy representation for excluded stakeholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance_latency_tradeoff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(govlat_tr_t0, governance_latency_tradeoff, theater_ratio, 0, 0.48).
narrative_ontology:measurement(govlat_tr_t3, governance_latency_tradeoff, theater_ratio, 3, 0.57).
narrative_ontology:measurement(govlat_tr_t6, governance_latency_tradeoff, theater_ratio, 6, 0.64).
narrative_ontology:measurement(govlat_tr_t9, governance_latency_tradeoff, theater_ratio, 9, 0.71).

% Extraction over time
narrative_ontology:measurement(govlat_be_t0, governance_latency_tradeoff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(govlat_be_t3, governance_latency_tradeoff, base_extractiveness, 3, 0.43).
narrative_ontology:measurement(govlat_be_t6, governance_latency_tradeoff, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(govlat_be_t9, governance_latency_tradeoff, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance_latency_tradeoff, enforcement_mechanism).
narrative_ontology:affects_constraint(governance_latency_tradeoff, democratic_legitimacy_deficit).
narrative_ontology:affects_constraint(governance_latency_tradeoff, executive_power_expansion).
narrative_ontology:affects_constraint(governance_latency_tradeoff, stakeholder_representation_access).

% DUAL FORMULATION NOTE:
% The governance latency tradeoff is analytically decomposable into three structurally distinct constraints: (1) crisis response speed requirement (coordination problem, ε ≈ 0.20, Rope), (2) deliberative exclusion extraction (asymmetric cost imposition, ε ≈ 0.60, Snare), and (3) emergency procedure normalization (institutional degradation, ε ≈ 0.35, Piton). This story treats the integrated phenomenon; decomposed stories model each mechanism separately. The network links show downstream constraints that inherit the latency tradeoff's structural tensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
