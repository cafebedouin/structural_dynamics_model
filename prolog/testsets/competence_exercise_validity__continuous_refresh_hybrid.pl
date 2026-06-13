% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Competence Retention via Continuous Drill Cycles
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical organizations maintain that practitioner competence
 *   requires continuous exercise through drill cycles, not one-time
 *   certification. This reading instantiates a hybrid model: simulation and
 *   drills ARE necessary to maintain competence, but the continuous-refresh
 *   mandate functions as both a genuine safety mechanism and an extraction
 *   mechanism that benefits training administration and certification bodies.
 *   The constraint coordinates on a real problem (competence decay) while
 *   simultaneously extracting governance authority and revenue from field
 *   practitioners. The founding problem is partially true — some competencies
 *   degrade; others do not. The policy's universality and continuous nature
 *   extract value beyond what the partial truth justifies.
 *
 * KEY AGENTS:
 *   - training_administration: institutional agenda-setter, controls what counts as valid competence exercise
 *   - certification_bodies: beneficiaries, issue renewals contingent on continuous participation
 *   - safety_oversight: beneficiary, enforces compliance as expression of safety vigilance
 *   - field_practitioners: payers bearing time, fee, and identity costs; identity-locked to profession
 *   - budget_constrained_organizations: payers bearing per-capita training overhead
 *   - real_world_test_environment: observer, provides ground truth on whether drill frequency predicts real-world performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.62).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.71).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Competence Retention via Continuous Drill Cycles").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'f10b4d1f-e328-46c6-8f51-56dbf9378c46').
narrative_ontology:cs_kernel_codification('f10b4d1f-e328-46c6-8f51-56dbf9378c46', distributed).
narrative_ontology:cs_authority_grounding('f10b4d1f-e328-46c6-8f51-56dbf9378c46', extraction).
narrative_ontology:cs_interpretation_layer_present('f10b4d1f-e328-46c6-8f51-56dbf9378c46').
narrative_ontology:cs_reading_relation('f10b4d1f-e328-46c6-8f51-56dbf9378c46', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('f10b4d1f-e328-46c6-8f51-56dbf9378c46', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('f10b4d1f-e328-46c6-8f51-56dbf9378c46', foundational, simulation_exercises_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(simulation_exercises_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('f10b4d1f-e328-46c6-8f51-56dbf9378c46', simulation_exercises_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('f10b4d1f-e328-46c6-8f51-56dbf9378c46', foundational, continuous_mandate_extracts_beyond_safety_requirement).
narrative_ontology:cs_axiom_status(continuous_mandate_extracts_beyond_safety_requirement, holdable).
narrative_ontology:cs_axiom_grounding('f10b4d1f-e328-46c6-8f51-56dbf9378c46', continuous_mandate_extracts_beyond_safety_requirement, deontological).
narrative_ontology:cs_reference_frame('f10b4d1f-e328-46c6-8f51-56dbf9378c46', competence_requires_periodic_reexercise).
narrative_ontology:cs_drift_state('f10b4d1f-e328-46c6-8f51-56dbf9378c46', contemporary_audit_focused_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f10b4d1f-e328-46c6-8f51-56dbf9378c46', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, training_administration).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, certification_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_oversight).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, field_practitioners).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, budget_constrained_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the mandate for continuous drill cycles, determines frequency and modality of competence-renewal exercises, and adjudicates what counts as valid training. Controls the certification pipeline and the interpretation of 'competence' itself. Benefits from sustained demand for training services and regulatory compliance infrastructure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Issue and renew certifications contingent on continuous drill participation. Generate revenue from re-certification fees and maintain authority over competence adjudication. Have institutional interest in the continuous-refresh model because it sustains their gatekeeping function.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, certification_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Enforce the continuous-drill mandate as the operational expression of safety policy. Cite drill compliance rates and training uptake as proof of safety vigilance, even when the causal link between drill frequency and real-world competence retention is contested. Benefit from the constraint's visibility and auditability.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_oversight, beneficiary,
    institutional, generational, analytical, national).

% Bear the direct cost of continuous drill cycles: time away from productive work, participation fees, and mandatory attendance even when personal competence assessment differs from institutional judgment. Identity-locked: abandoning the profession means losing certification and professional standing. Exit is theoretically available but career-costly.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, field_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Must fund continuous training cycles for their workforce or face non-compliance penalties and loss of operating licenses. Small and resource-limited organizations bear higher per-capita training costs relative to large operators. Cannot exit without losing the right to operate in regulated sectors.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, budget_constrained_organizations, payer,
    moderate, biographical, constrained, national).

% Empirical data on whether continuous drill cycles actually correlate with superior performance in real-world high-stress scenarios. Would contest the assumption that drill frequency is the driver of competence retention rather than proficiency-based assessment or competence-decay modeling. Structurally excluded from the mandate-setting conversation.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, field_evidence, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_exercise_validity__continuous_refresh_hybrid, field_evidence).

% Proficiency-based mastery models, spaced-repetition learning, or decay-curve monitoring could in principle replace frequency-based mandates. These approaches are not admitted to the policy design space because their adoption would redistribute authority from training administration to individual practitioners or algorithmic systems.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, alternate_competence_models, excluded,
    moderate, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_exercise_validity__continuous_refresh_hybrid, alternate_competence_models).

% Actual high-stress scenarios where competence must execute under pressure. Provides the ground truth against which the constraint's design is measured. Evidence comes from incident reports, close calls, post-incident reviews, and comparative outcomes across organizations with different drill schedules.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, real_world_test_environment, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(competence_exercise_validity__continuous_refresh_hybrid, real_world_test_environment).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, training_administration).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared metric for competence readiness across decentralized practitioners and organizations: continuous drill participation and re-certification create a standardized, auditable proof that practitioners maintain baseline safety readiness. Solves the problem of how to distribute confidence in human readiness when centralized oversight cannot directly monitor each actor's current capability.
% TRANSFER_FUNCTION: Moves time, funding, and governance authority from field practitioners and budget-constrained organizations to training administration and certification bodies. Practitioners surrender discretion over their own competence assessment and must participate in externally-set drill cycles. Organizations redirect operational budget to training overhead. The transfer is structured as a condition of authorization to operate.
% ABSENT_VOICES: Real-world performance data and practitioners' self-assessments of competence decay are structurally excluded from the mandate-setting process. A practitioner who believes they have retained competence through spaced experience and targeted review cannot override the frequency mandate. Organizations experimenting with alternative competence models cannot report back into policy design without formal regulatory approval. The empirical ground truth (what actually predicts performance in real catastrophe) is not admitted as an equal voice.
% DISAPPEARANCE_RATIONALE: If the continuous-drill mandate and re-certification gate vanished overnight, organizations would rapidly adopt alternative competence-validation models (proficiency-based assessment, spaced-repetition schedules, personal skill portfolios). Training administration would lose the revenue from mandatory cycles and re-certifications. Practitioners would have discretion to design their own competence maintenance. The safety outcomes would depend on whether the continuous-drill model is actually the causal driver of safety or merely a proxy that correlates with other factors (organizational investment, demographic stability, risk culture). Without the mandate, that causality would be testable.
% FOUNDING_PROBLEM: High-stress professionals (pilots, surgeons, emergency responders, nuclear operators) degrade in real-world decision-making when years pass without exposure to peak-stress scenarios. One-time certification does not capture competence retention; practitioners who passed certification three years ago may have atrophied in ways not visible without re-exercise. Continuous practice maintains muscle memory, decision-making patterns, and psychological readiness.
% FOUNDING_PROBLEM_CORROBORATION: Training administration and safety oversight cite incident reports and near-misses as evidence the problem is live. Practitioners and organizations report that proficiency-based mastery and targeted review (not frequency-based drills) maintain their readiness. Independent research in cognitive science and expertise shows both patterns: some high-stress competencies degrade rapidly (procedural memory, pattern recognition under pressure), while others degrade slowly or not at all (domain knowledge, strategic reasoning). The founding problem is partially true for some competencies and some individuals, not universally true. No corroborating voice outside the benefiting parties treats continuous drill as the ONLY solution.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the continuous mandate extracts ongoing governance authority, re-certification revenue, and time allocation from practitioners and organizations beyond what the founding problem structurally requires. Suppression is substantial (0.71) because the constraint is actively enforced through licensing and authorization gates — practitioners cannot simply exit the drill cycle without forfeiting professional status. Theater is moderate-high (0.48 and rising) because the mandate emphasizes compliance rates and participation audits more than actual competence outcomes; the metric that gets tracked is 'did you show up?' not 'did you retain the competence we claim to be testing?' The measurement trajectory shows extractiveness and theater both rising as the system matures — the initial phase focused on safety, the mature phase on compliance theater and revenue stability.
 *
 * PERSPECTIVAL GAP:
 *   From the training-administration seat, the constraint is genuine coordination with beneficial side-effects: practitioners need exercise, organizations need assurance, and continuous cycles provide both. From the field-practitioner seat identity-locked to certification, the constraint is enforced extraction: participation is mandatory regardless of self-assessed competence, fees are non-negotiable, and the burden falls on the individual. The engine should compute these seats divergently: the beneficiary seat at low/negative χ (benefits, low directionality), the payer seat at high χ (bears costs, high directionality toward 1.0). The divided verdict is the constraint's true structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Training administration and certification bodies: d near 0.0 (full beneficiary — they control the mandate, collect revenue, maintain gatekeeping authority). Safety oversight: d near 0.1-0.15 (benefits from the visibility and auditability of compliance, though their primary commitment is genuine safety). Field practitioners: d near 0.85-0.95 (identity-locked target: abandoning the profession costs everything, so exit is theoretically available but practically foreclosed; they bear mandatory participation costs with limited ability to exit or negotiate). Budget-constrained organizations: d near 0.75-0.80 (constrained payers: they cannot operate without compliance, so they absorb costs or reduce other safety investments). Real-world test environment: d = 0.5 analytical (observes but does not participate in the extraction). No directionality_overrides needed; the structural data should derive these automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophic in the classical sense — the founding problem is real, and the mechanism (continuous exercise) is partially justified. However, the constraint's persistence does NOT depend on the founding problem remaining 'live'. Even if empirical research proved that spaced-repetition learning or proficiency-based mastery retained competence equally well (or better) at lower cost, the continuous-drill mandate would persist because training administration and certification bodies have institutional interest in its continuation. The true mandate — 'competence retention requires exercise' — has bifurcated: the narrower, justified mandate ('some competencies decay and require periodic re-exercise') is true; the broader, extractive mandate ('continuous drill cycles of uniform frequency and duration are the only valid form of exercise') is maintained by institutional capture, not by the evidence. Mandatrophy detection should flag this as a constraint whose founding problem is partially superseded but whose extraction mechanism persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_heterogeneity,
    'Do all competencies in high-stress domains decay at the same rate, or do some competencies retain for years while others decay within weeks?',
    'Empirical study: track competence in multiple sub-domains (procedural memory, strategic reasoning, pattern recognition) over time in practitioners with varying drill frequency. Measure retention curves per competency type.',
    'If decay is heterogeneous, a one-size-fits-all continuous mandate extracts governance authority that should be distributed per competency type. If decay is uniform, the mandate''s universality is justified. Different impact: mandates should calibrate to competency class, not to uniform frequency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_heterogeneity, empirical, 'Whether competence-decay rates are uniform or heterogeneous across sub-domains.').

omega_variable(
    simulation_vs_spaced_learning_equivalence,
    'Is continuous simulation-based drill the ONLY effective way to retain competence, or do proficiency-based mastery, spaced-repetition review, and targeted scenario practice retain competence equally at lower cost?',
    'Randomized trial: split practitioners into continuous-drill cohort and proficiency-based / spaced-repetition cohort. Measure real-world performance (incident rates, near-miss handling, decision quality under pressure) over 2-3 years.',
    'If proficiency-based mastery is equivalent or superior, the continuous mandate is extractive overhead disguised as safety necessity. If continuous drill is demonstrably superior, the mandate is justified. If the difference is small and context-dependent, the mandate should be optional or organization-calibrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_vs_spaced_learning_equivalence, empirical, 'Whether continuous drill cycles are the only effective competence-retention mechanism or whether alternative models are equivalent.').

omega_variable(
    identity_lock_measurement_in_field_practitioners,
    'Is the measured suppression (0.71) primarily structural (licensing gates, legal barriers) or internalized (practitioners have fused their identity with continuous certification, perceive exit as unthinkable even if legally available)?',
    'Post-exit trajectory analysis: interview practitioners who left the regulated field and measure whether suppression persists in their self-reported experience after the structural gate is removed. If suppression persists or converts to shame/regret, it is internalized; if it vanishes, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the measured 0.71 — the target carries the suppression with them after exit. Reclassify the exit_options from ''identity_locked'' to a deeper category if one exists. If structural, suppression is correctly measured and exit is genuinely available to those who can absorb the identity cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_measurement_in_field_practitioners, empirical, 'Whether suppression in field practitioners is structural or internalized.').

omega_variable(
    theater_ratio_rising_mechanism,
    'Is the rising theater_ratio (0.38 to 0.48) driven by increasing compliance-audit focus, or by genuinely improved confidence in simulation''s ability to proxy real competence?',
    'Policy document analysis and interview: compare early-phase policy rationale (focused on actual competence retention) with current-phase rationale (focused on auditable compliance, training uptake rates, re-certification revenue). Trace which metrics are reported upward and how they frame safety.',
    'If theater is rising because the focus has shifted to auditability and revenue, the constraint is converting from coordination to extraction. If theater is rising because the system is discovering simulation is MORE predictive of real competence than initially thought, theater reflects refinement, not drift. The measurement trajectory supports the extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_rising_mechanism, empirical, 'Whether rising theater ratio reflects conversion from safety focus to compliance theater, or genuine refinement.').

omega_variable(
    reading_contest_framing,
    'The three readings of this kernel disagree on what counts as valid competence exercise. How would empirical evidence force a reclassification among the three readings?',
    'Design a high-fidelity real-world prediction test: practitioners from all three reading communities (continuous-drill, simulation-as-proxy, real-catastrophe-only) complete a standardized high-stress scenario after varying intervals without practice. Measure who performs best under pressure. This is the ground truth the readings contest.',
    'Results would reclassify the readings themselves: if continuous-drill cohort dominates, this reading''s claim is vindicated and simulation-as-proxy is overridden; if real-catastrophe-only cohort dominates, this reading is overridden; if all perform equally, a fourth reading (competence is robust and does not decay) emerges and may foreclose all three current readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_framing, empirical, 'Which reading''s core premise is vindicated by real-world performance prediction tests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(comp_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(comp_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comp_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: 'competence_exercise_validity'. The three readings are: (1) continuous_refresh_hybrid (this file): both simulation and reality matter; the continuous mandate extracts authority beyond what the founding problem structurally requires. (2) simulation_as_proxy: simulation IS valid exercise; continuous participation in simulations solves the competence problem. (3) real_catastrophe_only: only real catastrophe exercises competence; simulation is theater. Each reading has its own ε, beneficiary/victim structure, and stakeholder positions. They are linked here not because they agree but because they contest the same kernel. The ε-invariance principle requires decomposition: one kernel, three readings, three files, three structural stories. The readings compete in the policy space; this file documents the continuous_refresh_hybrid reading as a pure constraint model.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
