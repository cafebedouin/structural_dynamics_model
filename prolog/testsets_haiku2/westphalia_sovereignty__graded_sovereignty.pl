% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The Westphalian ideal of absolute sovereignty—each state is sole
 *   authority within its borders, external interference per se
 *   illegitimate—has been decomposed by the graded-sovereignty reading into a
 *   scalar framework: territorial authority exists on a spectrum from 'full'
 *   (Western democracies with strong institutions and rule of law) to
 *   'nominal' (failed states, post-conflict zones, ungoverned spaces).
 *   Intervention legitimacy is calibrated to measured capacity deficits: weak
 *   administrative reach, lack of monopoly on violence, fiscal extraction
 *   failure, weak rule of law, low democratic representation. This reading
 *   justifies external administration, military intervention, conditional
 *   aid, and IMF restructuring as capacity-building rather than imperialism.
 *   The core tension: the frame presents intervention as neutral technical
 *   capacity-assessment, but the metrics themselves are authored by powerful
 *   states, capacity evaluation becomes an instrument of hegemony, and the
 *   result is a hierarchical state system where weak states are perpetually
 *   objects of external management. The constraint story captures the
 *   standing arrangement under contest—the graded-sovereignty order as it
 *   operates—not the challenger frames (absolute non-intervention or
 *   conditional responsibility) that would replace it.
 *
 * KEY AGENTS:
 *   - Capacity-evaluating authorities (UN, IMF, World Bank, NATO, Western alliances): author metrics, assess states, justify intervention
 *   - Strong states with intervention mandate (US, EU, other Western powers): benefit from capacity-framework legitimacy, execute interventions
 *   - Weak states subject to paternalism (post-conflict nations, low-income states, fragile institutions): trapped in the evaluation system, subordinated to external oversight
 *   - Populations of intervened states: experience lost self-determination, foreign military presence, externally-directed policy as powerless, identity-locked actors
 *   - Alternative frameworks (non-aligned movements, postcolonial scholars): excluded from metric authoring, delegitimized
 *   - Regional powers (China, Russia): must navigate a Western-authored metric system they did not consent to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'ebdedeee-0c3e-4226-968b-d76ef8a3956d').
narrative_ontology:cs_kernel_codification('ebdedeee-0c3e-4226-968b-d76ef8a3956d', fixed_text).
narrative_ontology:cs_authority_grounding('ebdedeee-0c3e-4226-968b-d76ef8a3956d', extraction).
narrative_ontology:cs_interpretation_layer_present('ebdedeee-0c3e-4226-968b-d76ef8a3956d').
narrative_ontology:cs_reading_relation('ebdedeee-0c3e-4226-968b-d76ef8a3956d', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('ebdedeee-0c3e-4226-968b-d76ef8a3956d', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('ebdedeee-0c3e-4226-968b-d76ef8a3956d', foundational, sovereignty_is_scalar_not_categorical).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('ebdedeee-0c3e-4226-968b-d76ef8a3956d', sovereignty_is_scalar_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('ebdedeee-0c3e-4226-968b-d76ef8a3956d', foundational, state_capacity_metrics_legitimate_intervention).
narrative_ontology:cs_axiom_status(state_capacity_metrics_legitimate_intervention, holdable).
narrative_ontology:cs_axiom_grounding('ebdedeee-0c3e-4226-968b-d76ef8a3956d', state_capacity_metrics_legitimate_intervention, instrumental).
narrative_ontology:cs_reference_frame('ebdedeee-0c3e-4226-968b-d76ef8a3956d', equal_territorial_sovereignty).
narrative_ontology:cs_drift_state('ebdedeee-0c3e-4226-968b-d76ef8a3956d', contemporary_post_cold_war, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('ebdedeee-0c3e-4226-968b-d76ef8a3956d', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, strong_states_with_intervention_mandate).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states_subject_to_paternalism).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_of_intervened_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, scholarly_consensus_builders).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, implementation_bureaucrats).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, implementation_bureaucrats).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, regional_and_rival_powers).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, sovereignty_is_scalar_not_categorical).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, state_capacity_legitimates_authority).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, paternalism_is_justified_for_capacity_deficits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International bodies (UN Security Council, World Bank, IMF, NATO) that assess state capacity across metrics: monopoly of violence, fiscal extraction, administrative reach, rule-of-law institutions, democratic representation. Define which states meet 'full' sovereignty and which sit lower on the spectrum. Author the evaluation frameworks, decide when capacity deficits justify external administration or intervention. The evaluation power itself becomes the political prize.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities, agenda_setter,
    institutional, generational, analytical, universal).

% Western democracies, regional powers (US, EU, China, India) that justify intervention, trusteeship, sanctions, or conditional aid based on capacity assessment. Argument: intervening in 'failed' or 'fragile' states prevents humanitarian catastrophe, terrorism, refugee flows. Benefit accrues both as humanitarian legitimacy and as de facto control over weak-state policy, resource access, and strategic positioning. Can exit if political costs rise, but the institutional incentive structures reward expanded intervention mandates.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, strong_states_with_intervention_mandate, beneficiary,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, strong_states_with_intervention_mandate, agenda_setter).

% States assessed as having capacity deficits (post-conflict nations, low-income states, those with weak institutions) become objects of external oversight. Their formal sovereignty is recognized but conditional—subject to IMF conditionality, UN trusteeship, military intervention justified by capacity deficit, international administrations of territory or sectors. Exit from the arrangement means military conquest, state collapse, or acceptance of the evaluation metrics and slow capacity building under external tutelage (decades-long timescale with no guarantee of reaching 'full' status).
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states_subject_to_paternalism, payer,
    moderate, biographical, trapped, local).

% Civilians in weak states experience intervention as foreign military presence, externally-directed policy, loss of control over domestic institutions, and (in worst cases) direct violence in the name of capacity-building or humanitarian protection. The graded sovereignty frame presents them as beneficiaries of protection and modernization; they experience loss of self-determination. Identity fusion with the state means exile or statelessness are the primary exit routes.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_of_intervened_states, payer,
    powerless, immediate, identity_locked, local).

% Non-aligned movements, regional sovereignty advocates, postcolonial intellectuals, and scholars arguing for absolute non-intervention or conditional-responsibility frames. Would contest the capacity metrics themselves as instruments of neocolonial power, argue that sovereignty is categorical not scalar, or insist intervention require atrocity evidence not merely capacity deficits. Excluded from the agenda-setting layer where capacity metrics are defined and legitimacy is calibrated.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, alternative_sovereignty_frameworks, excluded,
    moderate, generational, constrained, universal).

% International relations scholars, lawyers, policy analysts who professionally articulate and defend the graded-sovereignty frame. Publishing careers, think-tank positions, advising roles depend on the frame's adoption as 'serious' policy analysis. Benefit from institutional legitimacy and funding flows aligned with capacity-assessment agendas. Can exit by changing research direction, but career path dependence and disciplinary prestige create identity fusion with the frame.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, scholarly_consensus_builders, beneficiary,
    organized, generational, mobile, global).

% UN administrators, IMF technical staff, military planners executing intervention mandates. Experience the constraint as a job—they implement capacity-assessment rubrics and justifications they may or may not endorse. Some genuinely believe in the mandate; others are trapped by professional identity (UN career, military rank) and cannot exit without abandoning status. Modest benefits (employment, professional standing) coupled with bearing moral responsibility for intervention outcomes.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, implementation_bureaucrats, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, implementation_bureaucrats, beneficiary).

% China, Russia, non-Western regional powers that object to capacity-evaluation frameworks they perceive as tools for Western strategic dominance. Must navigate a system where sovereignty recognition is graded by metrics they did not author. Can resist specific interventions and build alternative spheres of influence, but cannot fully exit the global norm structure. Pay by subordinating their own weaker-state allies to evaluation and by accepting constraints on their intervention legitimacy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, regional_and_rival_powers, payer,
    powerful, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for assessing when external intervention in domestic affairs is legitimate based on measurable capacity deficits (monopoly of violence, fiscal extraction, administrative reach, rule of law, democratic representation). Solves the collective action problem: strong states benefit from a coherent metric for when intervention is justified; weak states benefit from a rule-based rather than purely hegemonic system for determining sovereignty. Enables coordination on what constitutes a 'failed state' and what triggers multilateral or unilateral action.
% TRANSFER_FUNCTION: Transfers authority over domestic policy from weak states to international bodies (IMF conditionality, UN administration, foreign military presence). Transfers legitimacy from the absolute-non-intervention reading to the strong states conducting intervention. Transfers prestige and resources to scholars and institutions defining capacity metrics. Transfers de facto control over key state functions (security, fiscal policy, sometimes legislative authority) from locally-accountable to externally-appointed actors.
% ABSENT_VOICES: Populations of intervened states are rarely consulted on whether they prefer external administration to self-determination (even failed self-determination). Non-aligned movements and postcolonial theorists who reject the scalar frame are excluded from the capacity-metric-authoring layer. Regional powers not party to Western institutional structures (China, Russia) must work around the framework rather than inside it. Weak-state leaders who resist the evaluation framework are delegitimized as authoritarian rather than treated as negotiating partners.
% DISAPPEARANCE_RATIONALE: If the graded-sovereignty frame vanished—if intervention legitimacy reverted to either absolute non-intervention or threshold-based responsibility to prevent atrocities—the global order would reorganize dramatically. Strong states would lose the legal and institutional infrastructure for capacity-based intervention; weak states would regain formal equal sovereignty but lose access to conditional aid and IMF technical support; the UN system would fragment into competing sovereignty frameworks; and decades of external administration, military occupation, and institutional trusteeships would require renegotiation or abandonment.
% FOUNDING_PROBLEM: Cold War collapse created vacuum: post-colonial states with weak institutions, civil conflicts within states, refugee crises, terrorism harbored in ungoverned spaces, and humanitarian disasters in states unable or unwilling to protect populations. The strong-state order faced a choice: return to absolute non-intervention (risking humanitarian catastrophe and ungoverned chaos), or develop a framework for conditional intervention. Graded sovereignty emerged as the compromise: respect sovereignty in form, but calibrate intervention to measurable capacity deficits, replacing hegemonic power with 'impartial' technical assessment.
% FOUNDING_PROBLEM_CORROBORATION: Strong states and international bureaucrats attest the founding problem remains live: failed states, civil conflicts, terrorism, humanitarian crises persist and require external capacity-building. Weak-state leaders and postcolonial scholars attest the founding problem was overstated and exaggerated to justify intervention; they argue the problem was Western geopolitical dominance seeking new legitimacy after colonialism, not genuine incapacity. Independent analyses (from non-aligned think tanks, regional organizations outside the Western institutional nexus) are divided: some support capacity-building logic, others argue it is neocolonial cover.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval (post-Cold War expansion of intervention mandates, increasing IMF restructuring, expansion of UN administration mandates, military interventions justified by capacity assessment). Theater ratio tracks this rise: early in the interval the capacity-building justification is relatively believed; over time more enforcement effort goes to defending the evaluation framework itself rather than genuine capacity-building—scholarship defending metrics, media narratives about 'failed states,' institutional expansion. Theater plateaus at 0.41 (remains low because genuine coordination function persists: there IS a coordination problem around intervention legitimacy). Suppression requirement rises sharply early (establishing the metric framework, delegitimizing alternative readings, normalizing external administration) and plateaus at 0.72 (active suppression needed to keep weak states accepting the arrangement and to silence non-aligned alternatives). The time grid is shared across all three metrics so every authored point represents a coherent moment in the constraint's development.
 *
 * PERSPECTIVAL GAP:
 *   The capacity-evaluating authorities experience this as rope: genuine coordination problem (when SHOULD states intervene?), cooperative solution (rules-based rather than hegemonic). Weak states experience this as snare: the coordination story is cover; persistence depends on evaluating authorities' power to define metrics and enforce the tiering system; alternatives (absolute non-intervention, threshold-based atrocity response) are suppressed. The engine computes per-seat classification from the power/exit/directionality data: powerful institutional actors with arbitrage options will see lower extraction (d near beneficiary end); powerless trapped actors will see high extraction (d near target end). This is exactly the structural asymmetry the graded-sovereignty reading creates: a hierarchical system where assessment authority itself becomes the political prize.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluating authorities are the structural beneficiaries (they define what counts as 'capacity,' which states get upgraded or downgraded, which interventions are legitimate, which are forbidden). Strong states benefit from the legitimacy infrastructure (justify intervention without appearing hegemonic) and from policy control over weak-state allies (IMF conditionality, military bases, strategic positioning). Weak states are the victims: their formal sovereignty is recognized but conditional; they are perpetually assessed, perpetually potentially subject to external intervention; exit means state collapse or slow capacity-building under external tutelage with no guarantee of reaching 'full' status. Populations of intervened states are trapped (identity-locked to a state they cannot exit) and powerless, with immediate time horizons and no real alternatives. Alternative frameworks and non-aligned powers are excluded from the agenda-setting layer. The directionality divergence is structural: beneficiary seats drive the constraint forward; victim seats experience it as imposed. No override is needed—the declared roles and exit options produce the correct divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries a genuine coordination function (legitimacy for intervention is a real problem) AND asymmetric extraction (capacity metrics are authored by beneficiaries, weak states are perpetually objects of assessment rather than partners). This is the defining signature of tangled_rope: coordination + coercion, both required by the structure. Neither is cover story; both are real. The alternative readings (absolute non-intervention and conditional responsibility) would dissolve both the coordination AND the extraction, replacing them with different tradeoffs. Absolute non-intervention recovers state equality but creates vacuum for ungoverned chaos. Conditional responsibility replaces scalar assessment with threshold-based (atrocity evidence required), which is narrower and creates different extraction dynamics. Graded sovereignty is the middle reading—it genuinely solves coordination while genuinely enabling extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_objectivity,
    'Are the capacity metrics (monopoly of violence, fiscal extraction, rule of law, democratic representation) objective measures of genuine state function, or instruments through which powerful states encode their preferred governance models?',
    'Comparative institutional analysis: do metrics predict resilience, poverty reduction, and protection of populations equally across cultural contexts, or do Western-model states systematically score high while non-Western states with different governance models (but equivalent functionality) score low? Do metrics correlate with improved outcomes for populations, or primarily with increased foreign access and policy control?',
    'If metrics are objective: the constraint is justified coordination; weak states benefit from capacity-building to genuine functionality. If instruments: the constraint is neocolonial; metrics are the mechanism through which Western dominance is maintained and laundered through ''technical assessment.'' This determines whether classification is rope or snare at the weak-state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_objectivity, empirical, 'Whether capacity metrics measure genuine state function or encode Western governance preferences.').

omega_variable(
    coordination_necessity,
    'Does the graded-sovereignty framework solve a genuine collective action problem around intervention legitimacy that CANNOT be solved equally well by absolute non-intervention or by threshold-based responsibility to prevent mass atrocity?',
    'Natural experiment: examine regions that operated under different sovereignty frames (non-aligned movement sovereignty principle, conditional-responsibility atrocity standard, graded-sovereignty Western framework). Measure outcomes: state capacity, humanitarian protection, civilian mortality, institutional development, autonomy satisfaction. Does the graded framework outperform alternatives on any dimension, or is the coordination function primarily used to justify extraction?',
    'If graded-sovereignty solves coordination uniquely: the tangled-rope classification is appropriate—genuine coordination + coercion. If alternatives solve it equally well: the coordination function is cover story for extraction mechanism, reclassifying toward snare at weak-state seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity, empirical, 'Whether graded sovereignty solves coordination problems that alternative sovereignty readings cannot solve.').

omega_variable(
    kernel_reading_contest,
    'Which of the three readings of the Westphalia kernel is structurally correct—or are all three live readings held by different parties with no logical way to choose?',
    'This is a conceptual/preference omega, not empirical: no data resolves whether sovereignty is categorical (absolute non-intervention), threshold-based (conditional responsibility), or scalar (graded). Resolution depends on foundational commitments about state legitimacy and self-determination. The empirical path is to measure which reading produces better outcomes for populations, state stability, and autonomy satisfaction—but even that depends on value choices about what counts as ''better.''',
    'If absolute non-intervention is structurally correct: graded sovereignty is illegitimate intervention, reclassifying heavily toward snare. If conditional responsibility is correct: graded sovereignty extends intervention beyond atrocity thresholds, still snare-side. If graded sovereignty is correct: the constraint is legitimately justified, remaining tangled_rope. This is the meta-contest the constraint story exists within.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the Westphalian kernel is structurally sound—categorical, threshold-based, or scalar sovereignty.').

omega_variable(
    suppression_mechanism,
    'Is the measured suppression (0.72) primarily structural (weak states genuinely lack resources to resist external assessment and intervention) or internalized (weak-state leaders have accepted the graded-sovereignty frame as legitimate, so suppression persists even absent explicit coercion)?',
    'Post-exit suppression trajectory: in cases where weak states have successfully resisted capacity-assessment frameworks (e.g., non-aligned spheres of influence, regional alternatives to IMF), does suppression of alternative readings persist? If internalized, the shared leaders would continue to suppress non-aligned arguments even after structural power shifts; if structural, suppression would relax once the asymmetric power is removed.',
    'If internalized: the constraint''s effective suppression is higher than structural measures suggest; victim seats carry the suppression with them through identity fusion (professionals trained in capacity-assessment language, elites educated in Western universities). If structural: removing the external enforcement machinery would rapidly de-suppress alternative readings. This determines how durable the constraint is to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Whether suppression of alternative readings is structural or internalized among weak-state elites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(west_tr_t5, observed).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(west_tr_t15, observed).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(west_tr_t25, observed).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(west_tr_t30, observed).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__graded_sovereignty, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(west_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(west_be_t5, observed).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(west_be_t15, observed).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(west_be_t25, observed).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(west_be_t30, observed).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(west_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(west_su_t5, observed).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(west_su_t15, observed).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(west_su_t25, observed).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(west_su_t30, observed).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(west_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_aid_conditionality).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, imf_structural_adjustment).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, un_trusteeship_and_administration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Westphalian sovereignty kernel alongside absolute_non_intervention and conditional_responsibility. All three readings share the same referent (territorial authority and intervention legitimacy) but instantiate different extraction structures. Graded-sovereignty creates hierarchy; conditional-responsibility creates atrocity threshold; absolute non-intervention preserves equality. The three constraints form a constraint family linked by shared kernel. Each reading's ε is measured relative to the standing arrangement it describes, not relative to its siblings' endorsed alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
