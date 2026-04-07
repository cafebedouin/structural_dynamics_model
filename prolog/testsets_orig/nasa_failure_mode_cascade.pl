% ============================================================================
% CONSTRAINT STORY: nasa_failure_mode_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nasa_failure_mode_cascade, []).

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
 *   constraint_id: nasa_failure_mode_cascade
 *   human_readable: NASA Failure Mode Cascade and Risk Normalization
 *   domain: organizational/safety/aerospace_engineering
 *
 * SUMMARY:
 *   The NASA failure mode cascade represents a structural constraint where
 *   institutional hierarchy, schedule pressure, and success-breeds-confidence
 *   dynamics combine to suppress safety-critical information and normalize
 *   acceptance of increasing risk margins. The constraint exhibits the
 *   characteristic signature of a tangled rope: a genuine coordination
 *   function (aggregating complex subsystem data into mission feasibility
 *   assessment) coupled with asymmetric extraction (risks externalized onto
 *   powerless frontline engineers and astronauts while benefits flow to
 *   schedule maintainers and mission leadership). The theater ratio of 0.68
 *   reflects that post-Challenger quantified risk assessment procedures are
 *   substantially performative: they produce documentation and decision
 *   trails that satisfy stakeholder confidence requirements but correlate
 *   poorly with actual mission risk. The cascade is not a simple snare
 *   because some actors (program managers, safety engineers) genuinely
 *   coordinate complex integration. It is not pure rope because the
 *   coordination is embedded within an extraction mechanism that suppresses
 *   the very dissent necessary for course correction. The constraint's
 *   suppression value (0.65) reflects multiple overlapping mechanisms:
 *   hierarchical authority structure that makes dissent costly, professional
 *   identity fusion for astronauts that makes risk questions appear disloyal,
 *   retaliation risk for engineers who escalate concerns outside approved
 *   channels, and the organizational epistemic closure that accumulating
 *   success produces (previous narrow margins were acceptable, therefore
 *   current narrower margins are acceptable).
 *
 * KEY AGENTS:
 *   - Frontline Engineers: Primary victims (powerless/trapped) — bear costs of safety degradation while having no institutional mechanism to prevent it; face career retaliation for escalating concerns
 *   - Astronauts: Primary victims (powerless/identity_locked) — professional identity fused with willingness to accept mission risk; structurally mobile (could resign) but identity-locked against exercising that option
 *   - Safety Engineering Function: Organized secondary victims (organized/constrained) — possess expertise and formal authority for safety review but constrained by embeddedness in mission hierarchy; resources and attention allocated away from verification under schedule pressure
 *   - Mid-Level Program Managers: Mixed position (moderate/constrained) — genuine coordination role but also extraction: schedule pressure from above, suppression of bad news, incentive misalignment between career advancement and safety outcomes
 *   - Mission Success Narrative: Primary beneficiary (institutional/arbitrage) — captures political and budgetary benefits of schedule maintenance while externalities (risk normalization) are borne by others
 *   - Risk Management Theater: Institutional system (institutional/arbitrage) — maintains performative procedures that satisfy stakeholder confidence while low functional verification; perpetuated through post-Challenger institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional failures as immutable properties of complex systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nasa_failure_mode_cascade, 0.58).
domain_priors:suppression_score(nasa_failure_mode_cascade, 0.65).
domain_priors:theater_ratio(nasa_failure_mode_cascade, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nasa_failure_mode_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(nasa_failure_mode_cascade, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nasa_failure_mode_cascade, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nasa_failure_mode_cascade, tangled_rope).
narrative_ontology:human_readable(nasa_failure_mode_cascade, "NASA Failure Mode Cascade and Risk Normalization").
narrative_ontology:topic_domain(nasa_failure_mode_cascade, "organizational/safety/aerospace_engineering").

domain_priors:requires_active_enforcement(nasa_failure_mode_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nasa_failure_mode_cascade, mission_schedule_pressure).
narrative_ontology:constraint_beneficiary(nasa_failure_mode_cascade, bureaucratic_risk_management).
narrative_ontology:constraint_victim(nasa_failure_mode_cascade, safety_culture).
narrative_ontology:constraint_victim(nasa_failure_mode_cascade, frontline_engineers).
narrative_ontology:constraint_victim(nasa_failure_mode_cascade, astronauts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE ENGINEER (SNARE) — Structurally trapped within hierarchical reporting lines that suppress dissent. Career consequences for escalating safety concerns; no mechanisms for independent safety review; retaliation implicit in institutional culture. Maximum extraction: bears costs of safety degradation while having no power to prevent it. The constraint extracts obedience while suppressing the very information needed for course correction.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ASTRONAUT (SNARE) — Professional commitment to mission (identity_locked component) combined with asymmetric information about risk. Mission timeline pressures dominate risk assessment. No structural exit: professional identity fused with willingness to accept calculated risk. Trapped within the cascade because the institutional narrative frames mission success as value-transcendent, making dissent appear cowardly or insufficient faith in the machine.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-LEVEL PROGRAM MANAGER (TANGLED ROPE) — Experiences genuine coordination function: aggregating subsystem data, risk analysis, resource allocation across complex integration. Also experiences extraction: schedule pressure from above, suppression of bad news, incentive misalignment between career advancement and actual safety outcomes. Constrained exit because program continuity depends on their institutional knowledge; leaving damages their career and the program simultaneously.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MISSION SUCCESS NARRATIVE (ROPE) — Benefits from the cascade through coordination of political, budgetary, and technical requirements into a coherent story of progress. Arbitrage position: can selectively highlight successes, defer failures, reframe risks as managed uncertainties. Low extraction experienced because the narrative apparatus captures the benefits of constraint maintenance while externalities (risk normalization, culture degradation) are borne by frontline actors.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY ENGINEERING FUNCTION (TANGLED ROPE) — Organized group with genuine safety coordination role (risk analysis, failure mode identification, integration testing). Also experiences extraction: safety concerns deprioritized against schedule pressure, resources allocated to mission critical path rather than verification, escalation mechanisms bypassed in crisis conditions. Constrained exit because safety engineering legitimacy depends on being embedded in mission structure; independence would isolate them from decision data.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RISK MANAGEMENT THEATER (PITON) — Post-Challenger, NASA implemented elaborate quantified risk assessment (QRA) procedures: probability estimates, failure mode tables, sign-off documentation. The theater persists despite low functional verification: QRA accuracy correlates poorly with actual failure rates, risk numbers are negotiable under schedule pressure, and the ritual primarily serves stakeholder confidence rather than risk reduction. Performative system maintained through institutional inertia despite known limitations.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some risk normalization is inherent to large-scale engineering projects: the more experience accumulated without failure, the more compelling the argument that current margins are sufficient. This perspective risks naturalizing what is actually a contingent institutional failure: the cascade is not an immutable property of complex systems but a specific organizational pathology where status hierarchy suppresses dissent and schedule pressure overrides safety deliberation.
constraint_indexing:constraint_classification(nasa_failure_mode_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nasa_failure_mode_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nasa_failure_mode_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nasa_failure_mode_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nasa_failure_mode_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nasa_failure_mode_cascade, TR),
    TR >= 0.70.

:- end_tests(nasa_failure_mode_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. Initial value (0.32) reflects early post-Challenger safety culture emphasis. Middle value (0.45) reflects gradual schedule pressure increasing as program maturity normalizes risk margins. Final value (0.58) reflects systematic risk elevation in pursuit of increasingly ambitious mission schedules. The trajectory shows cumulative extraction rather than stable mechanism — each successful mission with narrow margin justifies accepting even narrower margins for the next mission. Suppression (0.65): High. Multiple overlapping suppression mechanisms: hierarchical authority structure that concentrates information and suppresses dissent, identity fusion (astronauts cannot exit without abandoning professional identity), professional retaliation risk (engineers who escalate safety concerns face career damage), and organizational epistemic closure (success breeding confidence in current margins). Theater ratio (0.68): Moderate-high, rising over the interval. Post-Challenger quantified risk assessment produces extensive documentation and decision trails but low functional verification. Risk numbers are negotiable under schedule pressure; QRA accuracy correlates poorly with actual failure modes. The ritual serves stakeholder confidence (demonstrating 'rigor') rather than actual risk reduction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why single-perspective analysis fails to capture institutional pathologies. The mission success narrative sees coordination (rope) — they are solving a legitimate problem of aggregating complex requirements. Frontline engineers see pure extraction (snare) — their voice is systematically suppressed and their risk bears without recourse. Safety engineers see mixed coordination and extraction (tangled_rope) — they have legitimate integration role but constrained authority and resource starvation. Program managers see constraint (tangled_rope) — both coordinating complexity and bearing pressure from above/below. The institutional risk management system sees its own ritual as degraded (piton) — post-Challenger procedures exist through inertia rather than effective function. The civilizational analytical observer risks seeing natural law (mountain) — complexity breeds risk, margins erode, success breeds confidence. But the structural data reveals this as naturalization of contingent organizational failure: the cascade is not an inherent property of complex systems but an extractive mechanism that uses technical coordination as cover for risk externalization.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary of the constraint is the mission success narrative and schedule maintainers, who capture political and budgetary benefits of on-time, on-budget execution. Their directionality is low (full beneficiary position) because they experience the constraint as coordination: aggregating complex technical requirements into feasible schedules. The primary victims are frontline engineers and astronauts, who bear extraction through suppressed voice and externalized risk. Engineers are trapped (directionality high) because hierarchical authority and retaliation risk prevent exit. Astronauts are identity_locked (directionality high-moderate) because professional identity is fused with willingness to accept mission risk, making dissent appear disloyal even absent institutional retaliation. Mid-level program managers occupy a hybrid position: constrained exit because institutional embeddedness is career-defining, yet some coordination function legitimacy. The piton classification derives from the theater ratio — performative risk management procedures persist through institutional inertia despite known limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate readings of the same structural phenomenon, but the preponderance of snare/tangled_rope classifications (4 out of 7 perspectives) reveals the constraint's true character: it is fundamentally extractive, with coordination function embedded as justification rather than primary purpose. The piton classification (degraded ritual) is diagnostic — it identifies that post-Challenger risk management theater serves institutional legitimacy rather than safety verification. The mountain classification is a false summit (naturalization). The rope classification (mission narrative) reflects beneficiary perspective rather than structural reality. The tangled_rope and snare classifications accurately capture the constraint's mechanism: legitimate coordination problems (system integration, risk aggregation) are the vehicle through which risk is extracted and suppressed. The constraint persists not because coordination is impossible without it, but because the extraction mechanism benefits powerful actors (mission leadership, schedule maintainers) while costs are borne by powerless ones (engineers, astronauts).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_normalization_threshold,
    'At what point does accumulated mission success without failure transition from reasonable confidence margin adjustment to pathological risk normalization?',
    'Historical failure-rate data correlated with quantified risk estimates at time of mission; identification of inflection points where QRA predictions systematically underestimated actual failure rates',
    'If threshold < 50 consecutive success flights: current NASA practices may already exceed safe risk envelope. If threshold > 200 flights: normalization is more gradual than data suggests. Determines whether cascade represents active extraction or passive drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_normalization_threshold, empirical, 'Threshold for risk normalization vs. reasonable confidence adjustment').

omega_variable(
    hierarchy_suppression_mechanism,
    'Is risk information suppression primarily a deliberate institutional strategy or an emergent outcome of hierarchical reporting structures under schedule pressure?',
    'Analysis of pre-launch communications, decision meeting transcripts, escalation attempt documentation; comparison of suppression rates in crisis vs. nominal conditions; organizational analysis of incentive structures vs. explicit directives',
    'If deliberate strategy: requires systemic intervention in decision authority and accountability. If emergent from structure: can be addressed through transparency mechanisms, cross-functional review, and schedule protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_suppression_mechanism, conceptual, 'Whether suppression is deliberate institutional strategy or structural emergence').

omega_variable(
    safety_culture_recovery_pathway,
    'Can safety culture be restored within the current hierarchical mission structure, or does the constraint require reorganization to separate safety verification from schedule authority?',
    'Comparison of safety culture metrics (incident reporting rates, near-miss escalation) in different organizational models; longitudinal tracking of proposed structural changes and their correlation with safety outcomes',
    'If recovery possible within current structure: the tangled_rope classification holds and coordination function can be restored. If structural separation required: the constraint is a snare that cannot be reformed by better processes alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_culture_recovery_pathway, preference, 'Whether safety culture recovery requires structural reorganization').

omega_variable(
    identity_locked_astronaut_exit,
    'For astronauts experiencing identity_locked exit options, how would dissent from mission manifest in practice? What percentage of astronauts would voice safety concerns if retaliation mechanisms were removed?',
    'Anonymized safety concern surveys in parallel reporting structure; post-flight interviews using explicit protection against retaliation; comparison of concern rates before/after implementation of independent safety escalation channels',
    'If high percentage would dissent: identity lock is secondary to suppression (treat as trapped rather than identity_locked). If low percentage: identity fusion with mission is genuine constraint even absent institutional retaliation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_astronaut_exit, empirical, 'Extent of identity lock vs. structural suppression for astronauts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nasa_failure_mode_cascade, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nasa_tr_t0, nasa_failure_mode_cascade, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nasa_tr_t15, nasa_failure_mode_cascade, theater_ratio, 15, 0.56).
narrative_ontology:measurement(nasa_tr_t30, nasa_failure_mode_cascade, theater_ratio, 30, 0.68).
narrative_ontology:measurement(nasa_tr_t45, nasa_failure_mode_cascade, theater_ratio, 45, 0.72).

% Extraction over time
narrative_ontology:measurement(nasa_be_t0, nasa_failure_mode_cascade, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nasa_be_t15, nasa_failure_mode_cascade, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(nasa_be_t30, nasa_failure_mode_cascade, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(nasa_be_t45, nasa_failure_mode_cascade, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nasa_failure_mode_cascade, enforcement_mechanism).
narrative_ontology:affects_constraint(nasa_failure_mode_cascade, organizational_culture_suppression).
narrative_ontology:affects_constraint(nasa_failure_mode_cascade, schedule_driven_risk_elevation).
narrative_ontology:affects_constraint(nasa_failure_mode_cascade, astronaut_autonomy_erosion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nasa_failure_mode_cascade, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
