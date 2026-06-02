% ============================================================================
% CONSTRAINT STORY: institutional_messaging_discipline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_messaging_discipline, []).

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
 *   constraint_id: institutional_messaging_discipline
 *   human_readable: Institutional Messaging Discipline
 *   domain: institutional_governance/communication_control
 *
 * SUMMARY:
 *   Institutional messaging discipline is the enforcement of uniform,
 *   pre-approved communication by organizational members. It manifests as:
 *   approval workflows for public statements, prohibition on independent
 *   media engagement, script adherence in customer interactions, and
 *   organizational consequences for 'off-message' communication. The
 *   constraint solves a genuine coordination problem — distributed
 *   institutional actors need some alignment to avoid conflicting narratives
 *   that undermine institutional credibility. However, it simultaneously
 *   suppresses information flow, constrains adaptive response to novel
 *   situations, and creates systematic vulnerability to rapid event cycles
 *   that outpace approval processes. The extractiveness has increased over a
 *   decade (0.35→0.58) as digital communication acceleration has created
 *   tension between real-time information environments and approval-based
 *   message control. The theater_ratio has risen (0.42→0.68), indicating that
 *   enforcement activity has become increasingly performative — communication
 *   audits, approval workflows, and messaging cascades consume resources
 *   without improving actual coordination outcomes, particularly when crises
 *   make the suppression mechanism visible.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — controls narrative, manages reputation risk, maintains brand coherence; experiences discipline as solution
 *   - Frontline Staff: Primary victim (powerless/trapped) — face employment risk for unauthorized communication; cannot exit framework without leaving institution
 *   - Internal Epistemic Diversity: Structural victim (powerless/trapped) — organization loses access to distributed knowledge, early warning signals, and diverse perspectives on problems
 *   - Communications Department: Beneficiary (institutional/arbitrage) — gains authority over narrative definition and organizational voice; captures strategic communication function
 *   - Middle Managers: Secondary victim (moderate/constrained) — enforce discipline while managing team autonomy; face competing demands
 *   - External Stakeholders: Variable (powerful/mobile to powerless/trapped depending on leverage) — receive controlled information; can exit relationship or apply countervailing pressure
 *   - Real-Time Responsiveness: Structural victim (powerless/trapped) — organization cannot adapt quickly to fast-moving events within approval timelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_messaging_discipline, 0.58).
domain_priors:suppression_score(institutional_messaging_discipline, 0.65).
domain_priors:theater_ratio(institutional_messaging_discipline, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_messaging_discipline, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_messaging_discipline, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_messaging_discipline, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_messaging_discipline, tangled_rope).
narrative_ontology:human_readable(institutional_messaging_discipline, "Institutional Messaging Discipline").
narrative_ontology:topic_domain(institutional_messaging_discipline, "institutional_governance/communication_control").

domain_priors:requires_active_enforcement(institutional_messaging_discipline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_messaging_discipline, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_messaging_discipline, brand_coherence_function).
narrative_ontology:constraint_victim(institutional_messaging_discipline, frontline_staff_autonomy).
narrative_ontology:constraint_victim(institutional_messaging_discipline, internal_epistemic_diversity).
narrative_ontology:constraint_victim(institutional_messaging_discipline, real_time_responsiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Employees bound by messaging discipline face high suppression: violation risks employment termination, public reprimand, or institutional blacklisting. No exit option beyond leaving the institution entirely. The constraint extracts compliance through fear. Maximum experienced extraction because the worker cannot negotiate, cannot speak truthfully without cost, and cannot exit the binding framework without abandoning their position and often their professional network.
constraint_indexing:constraint_classification(institutional_messaging_discipline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Managers coordinate team communication (genuine coordination function) while enforcing messaging discipline from above. They experience mixed extraction: enforcement burden constrains their autonomy, but they also benefit from brand coherence and reduced reputational risk. Career advancement depends on compliance. Significant extraction but not total — some agency through interpretation of guidance and selective enforcement.
constraint_indexing:constraint_classification(institutional_messaging_discipline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMUNICATIONS DEPARTMENT (ROPE) — Messaging discipline solves the genuine coordination problem of maintaining brand coherence across distributed agents. Communications teams benefit from first-mover definition of messaging strategy, internal authority over narrative, and protection from conflicting external signals. Experiences the constraint as coordination: enabling coherent organizational voice. Net beneficiary.
constraint_indexing:constraint_classification(institutional_messaging_discipline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXTERNAL STAKEHOLDER WITH LEVERAGE (TANGLED ROPE) — Customers, regulators, or media entities with significant bargaining power experience mixed extraction. The institution enforces messaging discipline that protects its interests (asymmetric extraction — stakeholders get controlled information), but the stakeholder can exit: choose competitors, publicize institutional silence, or apply regulatory pressure. Moderate extracted value because the stakeholder has mobility and outside options.
constraint_indexing:constraint_classification(institutional_messaging_discipline, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INSTITUTIONAL RITUAL (PITON) — Messaging discipline as a historical practice persists through institutional inertia despite degraded function. Crisis communication and social media have created environments where strict messaging discipline reduces credibility and increases perception of deception. The theater_ratio reflects that much messaging discipline activity (approval workflows, communication audits, controlled messaging cascades) is performative — visible enforcement of conformity rather than effective risk management. The constraint is maintained because alternatives haven't fully replaced it, not because it functions well.
constraint_indexing:constraint_classification(institutional_messaging_discipline, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (CIVILIZATIONAL VIEW) — Messaging discipline embeds genuine coordination function (brand coherence is necessary for distributed teams) alongside clear asymmetric extraction (information suppression constrains adaptive responsiveness). The constraint is structurally hybrid: it solves a real problem while creating systematic blind spots. Modern information environments make the suppression function increasingly visible (social media acceleration, employee voice platforms, crisis escalation) — the extraction mechanism that worked in slower media cycles fails in real-time media.
constraint_indexing:constraint_classification(institutional_messaging_discipline, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_messaging_discipline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_messaging_discipline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_messaging_discipline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_messaging_discipline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_messaging_discipline, TR),
    TR >= 0.70.

:- end_tests(institutional_messaging_discipline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value through information suppression and labor compliance (staff effort spent on coordination that could be autonomous) and through reputation management that primarily protects institutional leadership rather than frontline staff. However, it is not as extreme as snare-level (≥0.66) because genuine coordination benefits (brand coherence, reduced conflicting signals) are real and measurable. The increase from 0.35→0.58 over the interval reflects that the suppression cost has become more visible as media acceleration has made approval delays costly. Suppression (0.65): High. Institutional barriers to unauthorized communication are substantial: career risk, formal disciplinary mechanisms, cultural enforcement, and asymmetric information (staff don't know what they're 'allowed' to say until they violate it). These barriers are structural (external enforcement) not purely internalized. Theater ratio (0.68): High and rising. Much messaging discipline enforcement is performative: approval processes that provide little actual coordination value, communication audits that detect risk but don't prevent problems, and messaging cascades that replicate approved talking points rather than coordinate real communication. The rise reflects that the ritual persistence (piton dynamic) is increasingly visible — the constraint is maintained through institutional inertia and fear of reputational catastrophe, not because it functions well in high-velocity information environments.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence across power and exit combinations. Institutional leadership with arbitrage options (Communications Department) sees coordination and brand protection — the constraint solves a real problem at acceptable cost. Frontline workers with no exit see pure extraction and coercion — they bear the suppression cost with no benefit. Middle managers see the hybrid: coordination function is real but enforcement burden extracts from their autonomy. The analytical observer sees both: the coordination is genuine but the suppression mechanism has become dysfunctional in accelerated information environments. The piton perspective reveals institutional inertia — legacy communication control practices persist despite degraded function in media contexts where transparency is often more credible than managed silence. The gap between the beneficiary view (we need discipline to stay coherent) and the victim view (discipline makes us fragile and untrustworthy) reflects genuine structural asymmetry: the burden of maintaining the approved narrative falls on those without authority to define it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation combines power level, exit options, and beneficiary/victim status. Leadership with institutional power and arbitrage options (can move to different institution, can change policy, can engage external communication freely) experience low d (they benefit from the constraint). Frontline staff with no power and trapped exit (cannot leave without losing income and network, cannot communicate outside framework without employment risk) experience high d (full target of the constraint). Middle managers occupy intermediate space: constrained exit (could leave but at career cost) and moderate power (can interpret discipline, can selectively enforce) produce moderate d. This derivation directly produces the perspectival gap: the same constraint appears as rope (beneficiary), tangled_rope (manager), and snare (worker) — not because classification is subjective, but because directionality objectively differs across the power/exit/relationship matrix.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification is empirically justified: the constraint has both genuine coordination function (brand coherence, reduced reputational risk, distributed message alignment) AND significant asymmetric extraction (information suppression, labor coercion, constraint on adaptive response). The coordination is not incidental to the extraction — it is precisely what enables the extraction. By enforcing brand coherence, the constraint makes suppression more effective (contradictory signals leak information, so enforced uniformity prevents leaks). By controlling narrative, it protects leadership at the cost of staff autonomy. The mandatrophy is resolved by recognizing that these are not competing types but structurally linked functions: the same mechanism that coordinates is the mechanism that extracts. The rise in theater_ratio (0.42→0.68) reveals degradation of the coordination function (approval processes no longer produce coherent response in accelerated environments) while the extraction mechanism persists through institutional inertia — the piton dynamic where the constraint is maintained not because it works but because the alternative (dismantling control infrastructure) appears more risky to leadership.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_control_boundary,
    'Where does legitimate brand coherence coordination end and information suppression control begin?',
    'Comparative analysis: institutions with high messaging discipline vs. high autonomy; measurement of crisis response speed, accuracy of internal knowledge capture, and stakeholder trust trajectory',
    'If boundary is empirically clear: messaging discipline can be calibrated to retain coordination without suppression. If boundary is fuzzy: the constraint will always have asymmetric extraction embedded in apparent coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_control_boundary, empirical, 'Boundary between legitimate coordination and information suppression').

omega_variable(
    crisis_responsiveness_cost,
    'What is the empirical cost (reputational damage, lost stakeholder trust, regulatory penalty) of messaging discipline delays during institutional crises?',
    'Case study analysis of institutional responses to unexpected events (product failures, data breaches, leadership scandals); comparison of rapid transparent response vs. delayed controlled messaging outcomes',
    'If cost is high: messaging discipline suppression creates systemic fragility (high chi despite coordination function). If cost is acceptable: discipline provides net benefit over responsiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_responsiveness_cost, empirical, 'Reputational and operational costs of messaging discipline delays').

omega_variable(
    internal_epistemic_cost,
    'To what extent does suppression of internal staff communication reduce organizational learning capacity and situational awareness?',
    'Measurement of error detection speed, innovation pipeline effectiveness, and crisis forewarning lag in high-discipline vs. low-discipline organizational units',
    'If cost is substantial: the constraint reduces institutional adaptive capacity (extraction mechanism actively harmful to beneficiary). If cost is minimal: discipline extracts without degrading coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_epistemic_cost, empirical, 'Organizational learning impact of messaging discipline suppression').

omega_variable(
    identity_lock_mechanism,
    'Do institutional actors (particularly leadership) experience messaging discipline as a natural requirement of organizational coherence, or as a contingent policy choice?',
    'Stakeholder interviews and behavioral observation; measurement of resistance to messaging relaxation during low-crisis periods; comparison with peer institutions using different disciplines',
    'If identity-locked: the constraint persists even when empirical evidence suggests relaxation would improve outcomes (cognitive capture). If perceived as contingent: messaging discipline can be renegotiated in response to changed circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether messaging discipline is naturalized or seen as policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_messaging_discipline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msgdisc_tr_t0, institutional_messaging_discipline, theater_ratio, 0, 0.42).
narrative_ontology:measurement(msgdisc_tr_t5, institutional_messaging_discipline, theater_ratio, 5, 0.58).
narrative_ontology:measurement(msgdisc_tr_t10, institutional_messaging_discipline, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(msgdisc_be_t0, institutional_messaging_discipline, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(msgdisc_be_t5, institutional_messaging_discipline, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(msgdisc_be_t10, institutional_messaging_discipline, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_messaging_discipline, information_standard).
narrative_ontology:boltzmann_floor_override(institutional_messaging_discipline, 0.12).
narrative_ontology:affects_constraint(institutional_messaging_discipline, organizational_learning_capacity).
narrative_ontology:affects_constraint(institutional_messaging_discipline, crisis_response_velocity).

% DUAL FORMULATION NOTE:
% Messaging discipline overlaps with constraint_organizational_identity_coherence (identity-level coordination). This story focuses on the communication control mechanism; the related constraint addresses the identity/culture aspects. Messaging discipline enables institutional identity coherence by controlling information flow that could contradict the official narrative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_messaging_discipline, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
