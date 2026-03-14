% ============================================================================
% CONSTRAINT STORY: sunk_cost_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sunk_cost_escalation, []).

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
 *   constraint_id: sunk_cost_escalation
 *   human_readable: Sunk Cost Escalation (Commitment Trap)
 *   domain: behavioral_economics/decision_theory
 *
 * SUMMARY:
 *   Sunk cost escalation is a commitment trap where agents continue investing
 *   in projects, relationships, or decisions to justify prior investments,
 *   even when rational cost-benefit analysis would recommend exit. The trap
 *   operates through psychological and structural mechanisms: escalating
 *   commitment to maintain identity coherence, social reputation, and
 *   perceived rationality; organizational theater that performs commitment
 *   review while legitimizing further escalation; and suppression of exit
 *   options through internalized narratives ('we're too far to stop now') and
 *   external costs (admitting failure, losing face). The constraint exhibits
 *   strong mandatrophy dynamics — it appears as an immutable law of human
 *   psychology (mountain/false summit), as extraction pure and simple
 *   (snare), as organizational theater (piton), and as mixed
 *   coordination-extraction in institutional contexts (tangled rope). The
 *   measurements show consistent escalation over a 6-unit interval: both
 *   extractiveness and theater increase monotonically, indicating that as
 *   agents become more committed, the constraint's extraction mechanism
 *   intensifies. This is opposite to learning dynamics — in genuine
 *   multi-stage investment, extractiveness should decline as information
 *   improves project viability estimates. The monotonic rise indicates that
 *   escalation is psychology-driven rather than information-driven.
 *
 * KEY AGENTS:
 *   - Escalating Agent: Primary victim (powerless/trapped or moderate/constrained) — bears full cost of continued commitment; trapped by sunk costs and identity fusion
 *   - System Resources: Victim (abstract) — capital, labor, and opportunity costs allocated to failing projects or relationships
 *   - Organizational Leadership: Secondary beneficiary/coordinator (organized/constrained) — benefits from narrative of progress and continued commitment; coordinates escalation through hierarchy
 *   - Institutional Review Gates: Performative actor (institutional/mobile) — maintain theater of decision-making while legitimizing escalation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing as immutable law what is contingent on psychology and institutional frames
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sunk_cost_escalation, 0.62).
domain_priors:suppression_score(sunk_cost_escalation, 0.58).
domain_priors:theater_ratio(sunk_cost_escalation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sunk_cost_escalation, extractiveness, 0.62).
narrative_ontology:constraint_metric(sunk_cost_escalation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sunk_cost_escalation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sunk_cost_escalation, snare).
narrative_ontology:human_readable(sunk_cost_escalation, "Sunk Cost Escalation (Commitment Trap)").
narrative_ontology:topic_domain(sunk_cost_escalation, "behavioral_economics/decision_theory").

% --- Structural relationships ---
narrative_ontology:constraint_victim(sunk_cost_escalation, escalating_agent).
narrative_ontology:constraint_victim(sunk_cost_escalation, system_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESCALATING AGENT (SNARE) — Agent is trapped by their own prior commitment. Each additional investment increases psychological commitment and sunk costs, reducing perceived exit options. The constraint extracts continued commitment and resources from the agent who perceives themselves as having no good exit. No coordination benefit — pure extraction of time and resources to justify prior choices.
constraint_indexing:constraint_classification(sunk_cost_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ESCALATING AGENT ALTERNATIVE (SNARE, CONSTRAINED VIEW) — From a slightly less trapped position, exit is possible but costly: admitting failure, losing sunk investments, social reputation damage, and identity disruption ('I am someone who abandons projects'). High cost to exit makes continuing feel like the only rational choice. Moderate power due to some agency in choosing escalation. Effective extraction remains high.
constraint_indexing:constraint_classification(sunk_cost_escalation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, sunk cost escalation appears to be an immutable psychological law: rational agents making decisions based on past commitments to justify investments. The constraint seems to emerge naturally from how human cognition processes commitment, accountability, and loss. However, this is a FALSE SUMMIT — the engine will flag this as naturalization of a behavioral trap that is contingent on institutional and cognitive frames.
constraint_indexing:constraint_classification(sunk_cost_escalation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ORGANIZATIONAL SYSTEM (PITON) — Organizations and institutions maintain performative escalation commitments (project reviews, milestone gates, continued funding justifications) long after rational cost-benefit analysis would recommend exit. The institutional machinery continues the ritual of escalation review despite degraded function. Theater is high (0.65) because the escalation decision point has become primarily a performance of commitment rather than genuine resource optimization.
constraint_indexing:constraint_classification(sunk_cost_escalation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZATIONAL LEADERSHIP (TANGLED ROPE) — Leadership can coordinate multiple escalating agents through investment announcements, resource commitments, and narrative construction ('we're nearly there'). Genuine coordination function exists (aligning effort toward completion). But asymmetric extraction also occurs: leadership benefits from perceived progress, subordinates bear the escalation costs. Requires active enforcement through hierarchy.
constraint_indexing:constraint_classification(sunk_cost_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INFORMED OBSERVER WITH EXIT OPTIONS (ROPE) — An agent with sufficient power, information, and exit options perceives escalation as a pure coordination problem with low extraction. Can exit if cost-benefit tilts negative. The constraint operates for them as a rational commitment mechanism enabling long-term projects. Sees escalation as manageable — low suppression, genuine coordination function.
constraint_indexing:constraint_classification(sunk_cost_escalation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sunk_cost_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_escalation, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sunk_cost_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sunk_cost_escalation, TR),
    TR >= 0.70.

:- end_tests(sunk_cost_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and escalating. Measured as the proportion of ongoing commitment driven by sunk cost justification rather than forward-looking cost-benefit analysis. Starting at 0.35 (many projects have genuine remaining potential), escalating to 0.62 as identity fusion and psychological commitment deepens. The agent is extracted from in the sense that they continue bearing costs beyond rational exit point. Suppression (0.58): Moderate-high. Multiple suppression mechanisms: (1) psychological—internalized narratives that make exit feel like identity failure or irrationality; (2) structural—career/reputation costs of abandoning projects, difficulty finding alternative uses for specialized investments, sunk human capital; (3) institutional—organizational theater that performs decision-making while legitimizing escalation. Theater ratio (0.65): High and increasing. Escalation decisions are increasingly performative: project reviews, milestone announcements, continuation justifications serve primarily to demonstrate commitment rather than to optimize resource allocation. At t=0, theater is lower (genuine learning). At t=6, theater dominates — the decision machinery is sustained primarily to legitimize continuation rather than to question it. Mandatrophy resolution: This constraint resolves mandatrophy by showing that all six classifications are legitimate perspectival readings. The mountain (natural law view) is a FALSE SUMMIT that naturalizes contingent psychology. The snare is the victim's reality. The piton is the organizational system's degraded review ritual. The tangled rope is leadership's perspective. The rope is the informed observer's coordination experience. The mandatrophy resolves by rejecting the mountain as naturalization.
 *
 * PERSPECTIVAL GAP:
 *   The escalating agent (powerless/trapped) sees pure extraction (Snare) — continuing investment feels mandatory, with no good exit. They experience high suppression as psychological lock-in and identity fusion. The organizational system (institutional/mobile) sees its own review process as largely theatrical (Piton) — escalation gates exist but primarily legitimize continuation rather than genuinely question it. Leadership (organized/constrained) sees mixed coordination and extraction (Tangled Rope) — they can coordinate multiple agents toward completion and perceive genuine benefit from progress narrative, but subordinates bear asymmetric cost. The informed observer with real exit options (powerful/mobile) sees coordination (Rope) — escalation is a rational commitment mechanism enabling long-term projects; exit is possible if truly necessary. The analytical observer at civilizational scope (analytical/analytical) risks seeing a natural law (Mountain) — 'sunk costs always drive human behavior' — but this is a false summit naturalizing what is actually a contingent feature of institutional and cognitive frames. The perspectival gap reveals that the constraint's type depends on the agent's position and exit options; powerless agents experience it as Snare, while agents with real exit options experience it as Rope or manageable coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the escalating agent's position: trapped agents (d ≈ 0.95) experience maximum extraction; constrained agents (d ≈ 0.75) experience high extraction with some perceived agency; informed agents with mobile exit (d ≈ 0.40) experience lower extraction. The escalation agent bears costs (investment time, resources, emotional labor, opportunity costs) while organizational leadership captures benefits (narrative of progress, resource commitment, perceived competence). No genuine coordination benefit accrues to trapped agents — the constraint serves pure extraction. For constrained or mobile agents, some coordination benefit exists (commitment enables long-term projects), producing lower χ and different classifications. The victim group is 'escalating_agent' and 'system_resources' (abstract collective) — there are no beneficiaries in the pure snare reading, though leadership has secondary benefits in tangled rope contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   Sunk cost escalation demonstrates mandatrophy resolution through perspectival analysis. The mountain (natural law) classification appears from a civilizational/analytical view that treats escalation as an inherent feature of human psychology. But the structural data reveals this as FALSE SUMMIT naturalization: the constraint is behavioral and institutional, not physical or logical. The snare classification is the victim's reality — trapped by prior commitment and identity fusion. The piton classification is organizational — the review ritual is degraded, existing primarily to legitimize escalation rather than to question it. The tangled rope is leadership's mixed experience — genuine coordination exists (aligning effort) but extraction is asymmetric (subordinates bear costs). The rope perspective is the informed observer with real exit options — for them, escalation is a rational commitment mechanism, low-suppression coordination. All these are coherent readings of the same constraint from different positions. Mandatrophy is resolved by recognizing that the constraint TYPE depends on the observer's structural position and exit capacity, not on discovering a single 'true' type. The false summit (mountain) is rejected as naturalization; all other types are legitimate perspectival readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_psychology_structural,
    'Is sunk cost escalation a structural feature of how rational agents make decisions under commitment, or is it a cognitive bias remediable by information and framing?',
    'Empirical tests: do agents with explicit sunk cost information and reframing still escalate? Does institutional structure (sunburst accounting, zero-based budgeting) reduce escalation? Comparison across decision-making frameworks.',
    'If structural/rational: classification remains Snare across most contexts. If remediable bias: classification shifts toward Rope or Scaffold in informed/designed contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_psychology_structural, empirical, 'Whether escalation is structural or remediable through information').

omega_variable(
    identity_lock_binding,
    'How much of the escalation trap is due to identity fusion (''I am invested in this project'') versus material sunk costs (money, time already spent)?',
    'Controlled experiments varying identity salience while holding sunk costs constant. Post-project interviews about self-concept entanglement. Longitudinal tracking of agents who leave projects — does identity damage persist?',
    'If identity-locked: exit_options should be identity_locked rather than trapped/constrained for many agents. Classification remains Snare but binding mechanism is cognitive-internal rather than structural-external.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_binding, empirical, 'Role of identity fusion in escalation entrapment').

omega_variable(
    institutional_escalation_suppression,
    'Is escalation suppressed by institutional structure (gates, reviews, performance metrics) or amplified? Are formal decision points reducing escalation or legitimizing it?',
    'Comparison of escalation rates across organizational structures: flat vs hierarchical, with/without formal gates, with/without zero-based budgeting. Archival analysis of project termination decisions.',
    'If suppressed by institutional structure: suppression metric should be higher; organizational constraints prevent some escalation. If amplified: suppression metric reflects psychological suppression only; institutional theater masks escalation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_escalation_suppression, empirical, 'Whether institutional structures suppress or amplify escalation').

omega_variable(
    escalation_definition_boundary,
    'What distinguishes legitimate multi-stage investment (learning-driven, discovering true costs iteratively) from extractive escalation (justifying prior choices)?',
    'Analysis of decision records: are resource allocations driven by new information about project viability, or by framing of sunk costs? Do agents show evidence of Bayesian updating on the true probability of project success?',
    'If boundary is epistemic (information-driven vs. psychology-driven): extractiveness should be lower for learning-driven projects. If boundary is blurred: extractiveness should be higher throughout — learning becomes a cover story for escalation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_definition_boundary, empirical, 'Distinguishing legitimate investment iteration from extractive escalation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sunk_cost_escalation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sunkcost_tr_t0, sunk_cost_escalation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sunkcost_tr_t2, sunk_cost_escalation, theater_ratio, 2, 0.48).
narrative_ontology:measurement(sunkcost_tr_t4, sunk_cost_escalation, theater_ratio, 4, 0.6).
narrative_ontology:measurement(sunkcost_tr_t6, sunk_cost_escalation, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(sunkcost_be_t0, sunk_cost_escalation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sunkcost_be_t2, sunk_cost_escalation, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sunkcost_be_t4, sunk_cost_escalation, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(sunkcost_be_t6, sunk_cost_escalation, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sunk_cost_escalation, resource_allocation).
narrative_ontology:affects_constraint(sunk_cost_escalation, commitment_escalation_cycle).
narrative_ontology:affects_constraint(sunk_cost_escalation, sunk_cost_bias_institutional).
narrative_ontology:affects_constraint(sunk_cost_escalation, project_termination_delay).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
