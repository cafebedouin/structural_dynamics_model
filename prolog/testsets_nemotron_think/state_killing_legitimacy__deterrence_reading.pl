% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment as Deterrence Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The deterrence reading of capital punishment justifies state killing as a
 *   rational signal that prevents future murders by altering the cost-benefit
 *   calculus of potential offenders. This reading emerged after Furman v.
 *   Georgia (1972) as a constitutional reconstruction: the Court required
 *   that death penalty statutes serve a measurable penological purpose, and
 *   deterrence became the primary articulated justification alongside
 *   retribution. The constraint is a tangled rope — it claims a genuine
 *   coordination function (murder prevention through credible threat) but
 *   extracts asymmetrically from executed offenders who are instrumentalized
 *   as means to that end. The empirical foundation is contested:
 *   meta-analyses consistently fail to find a deterrent effect beyond
 *   incapacitation, yet the justification persists. The engine computes
 *   per-seat classifications from the structural data; the claimed_type
 *   (tangled_rope) and metrics are authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.45).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.55).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment as Deterrence Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'fd49367c-320a-4072-bee7-6658df69d2d4').
narrative_ontology:cs_kernel_codification('fd49367c-320a-4072-bee7-6658df69d2d4', formalized).
narrative_ontology:cs_authority_grounding('fd49367c-320a-4072-bee7-6658df69d2d4', extraction).
narrative_ontology:cs_interpretation_layer_present('fd49367c-320a-4072-bee7-6658df69d2d4').
narrative_ontology:cs_reading_relation('fd49367c-320a-4072-bee7-6658df69d2d4', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd49367c-320a-4072-bee7-6658df69d2d4', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_axiom('fd49367c-320a-4072-bee7-6658df69d2d4', foundational, deterrence_justifies_state_killing).
narrative_ontology:cs_axiom_status(deterrence_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('fd49367c-320a-4072-bee7-6658df69d2d4', deterrence_justifies_state_killing, empirically_contingent).
narrative_ontology:cs_axiom('fd49367c-320a-4072-bee7-6658df69d2d4', foundational, offender_instrumentalization_permissible_for_social_end).
narrative_ontology:cs_axiom_status(offender_instrumentalization_permissible_for_social_end, holdable).
narrative_ontology:cs_axiom_grounding('fd49367c-320a-4072-bee7-6658df69d2d4', offender_instrumentalization_permissible_for_social_end, instrumental).
narrative_ontology:cs_reference_frame('fd49367c-320a-4072-bee7-6658df69d2d4', consequentialist_penal_authority).
narrative_ontology:cs_drift_state('fd49367c-320a-4072-bee7-6658df69d2d4', contemporary_empirical_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fd49367c-320a-4072-bee7-6658df69d2d4', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, general_public_safety).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, executed_offenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, general_public_safety).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, consequentialist_penal_justification).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_monopoly_on_lethal_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the death penalty system: legislates capital statutes, conducts trials, manages appeals, carries out executions. Justifies the system as a rational deterrent signal that prevents future murders. Controls the machinery of enforcement and the narrative of its necessity. Can modify or abolish the practice through legislative or executive action but faces political incentives to maintain it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals sentenced to death and executed. Instrumentalized as means to the claimed social end of deterrence. Bear the ultimate cost (life) with no possibility of exit once sentenced. Their deterrent value is asserted by the system but cannot be verified at the individual level. No meaningful agency within the constraint once caught in its machinery.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Members of the public who would allegedly be murdered absent the deterrent signal of executions. The claimed beneficiaries of the constraint. Their benefit is statistical and counterfactual — they do not know they were protected, and the protection (if real) is diffuse and unobservable at the individual level. Cannot opt out of the protection or the system that claims to provide it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, biographical, constrained, national).

% Society at large as the aggregate recipient of claimed murder reduction. Bears the fiscal costs of the capital system (trials, appeals, incarceration, execution) and the moral costs of state killing. Benefits if deterrence is real; pays regardless. Exit requires political mobilization to abolish or reform the system — possible but structurally difficult.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, general_public_safety, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, general_public_safety, payer).

% Organizations and individuals who categorically oppose state killing on dignity grounds. Excluded from the constraint's internal justification because their frame (deontological prohibition) is treated as external to the consequentialist calculus. Would object to the instrumentalization of offenders and the empirical claims of deterrence. Their exclusion is structural: the deterrence reading only operates within a framework that accepts consequentialist trade-offs.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Researchers who study whether executions actually deter murder. Produce the evidence base that the deterrence claim rests on. Meta-analyses consistently find no credible deterrent effect beyond incapacitation. Their findings challenge the constraint's foundational empirical premise but do not directly control the policy. Occupy an analytical seat: they see the full structure but lack enforcement power.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, empirical_criminologists, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to solve the coordination problem of preventing murder by providing a credible, extreme signal that the state will impose the ultimate cost on murderers, thereby altering the cost-benefit calculus of potential offenders.
% TRANSFER_FUNCTION: Transfers the life of the executed offender (and the fiscal/moral costs of the capital system) to the state's account, in exchange for a claimed statistical reduction in future murders that benefits potential victims and the public.
% ABSENT_VOICES: The executed offenders themselves are silenced by the constraint. Abolitionist voices are structurally excluded from the consequentialist framework that authorizes the practice. Families of murder victims who oppose the death penalty are marginalized in the deterrence calculus. International human rights bodies that categorize capital punishment as a violation are treated as external to domestic penal sovereignty.
% DISAPPEARANCE_RATIONALE: If the death penalty vanished overnight, the penal system would reorganize around life imprisonment as the maximum sanction. The deterrence signal would disappear; murder rates would be tested against the counterfactual. States would redirect capital-case resources to investigation, prosecution, and incarceration. The political coalition sustaining the death penalty would lose its central organizing object. The world rearranges because the constraint actively structures penal resources, political discourse, and the state's lethal authority.
% FOUNDING_PROBLEM: Post-Furman (1972) crisis of legitimacy for capital punishment: the Supreme Court struck down existing statutes as arbitrary. The deterrence justification was rebuilt as a rational, empirically grounded response to restore constitutional legitimacy — moving from unguided discretion to 'guided discretion' statutes that purported to serve a measurable social end.
% FOUNDING_PROBLEM_CORROBORATION: The state (via legislative records and court opinions) attests the deterrence purpose remains live. Empirical criminologists (National Research Council 2012, subsequent meta-analyses) attest the founding empirical premise is dead — no credible evidence supports deterrence. The corroboration is split: the benefiting institution maintains the problem is live; the epistemic community outside the institution has declared it dead.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).
:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint takes a life from the offender while the claimed benefit (deterrence) is diffuse, statistical, and empirically unsubstantiated. Suppression is moderate (0.55) because the constraint requires active enforcement machinery (trials, appeals, execution protocols) and excludes alternatives (life without parole) from the deterrence calculus. Theater ratio is moderate (0.38) — the elaborate procedural apparatus (guided discretion, proportionality review, method-of-execution litigation) performs due process while the core empirical claim erodes. Accessibility collapse is moderate (0.42) — alternatives exist (LWOP, restorative justice) but are structurally disadvantaged by the deterrence frame. Resistance is moderate-high (0.58) — sustained abolitionist movement, international pressure, and empirical refutation create persistent opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the constraint is coordination (rope-like): it solves murder prevention through credible signaling. From the executed offender's seat, it is pure extraction (snare): life taken for an unproven social benefit. From potential victims' seat, it is a claimed shield they cannot verify. The engine computes this divergence from power/exit/beneficiary structure — the metrics do not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus is the agenda_setter with arbitrage-grade exit (can abolish legislatively) — d near beneficiary end. Executed offenders are powerless, trapped, immediate horizon — d = 1.0 (full target). Potential future victims are powerless beneficiaries with constrained exit — d near 0.0 but benefit is counterfactual. General public is organized, constrained exit, bears fiscal/moral costs — d near symmetric (0.5). Abolitionist advocates are excluded from the framework — their directionality is not computed within the constraint's logic. Empirical criminologists are analytical observers — d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Furman legitimacy crisis) is contested: the state says deterrence is still the live purpose; the empirical community says the evidence is dead. This mismatch (status=contested, verdict=world_rearranges) flags mandatrophy risk — the constraint may persist as a zombie arrangement after its empirical justification has collapsed. The theater_ratio trajectory (rising) and extractiveness trajectory (slowly rising) are consistent with mandatrophy: coordination function atrophying, performative maintenance increasing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_status,
    'Does the empirical evidence definitively falsify the deterrence claim, or does methodological ambiguity leave it contested?',
    'Consensus among criminologists/economists via repeated meta-analysis; or a natural experiment (e.g., moratorium/abolition in a major jurisdiction with high-quality before/after data) that produces a clear signal.',
    'If definitively falsified, the constraint''s coordination function collapses — it becomes a snare (pure extraction) or piton (inertial performance). If ambiguity persists, the tangled_rope classification holds: genuine but unproven coordination function + asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Whether the deterrence effect is empirically dead or merely unproven.').

omega_variable(
    kernel_reading_boundary,
    'Is the deterrence reading a distinct constraint from the retributive reading, or do they operate as a single blended justification in practice?',
    'Analyze whether jurisdictions that retain the death penalty rely on deterrence, retribution, or both in their statutes, court opinions, and political discourse. If they are inseparably blended in practice, they may be one constraint with dual ε.',
    'If blended, the ε-invariance principle requires decomposition into a single constraint story with hybrid justification, or the two readings must be modeled as a constraint family with shared enforcement machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether deterrence and retribution are structurally separable constraints or a fused justification.').

omega_variable(
    committer_frame_ambiguity,
    'Does this reading''s ε referent (the standing arrangement of state killing assessed by deterrence lights) adequately capture the extraction from offenders, or does the kernel''s multi-reading structure require a shared referent that this reading cannot isolate?',
    'Compare ε across the three readings for the same physical arrangement (state killing). If ε differs radically by reading, the referent is reading-relative, not arrangement-relative — violating ε-invariance unless the readings are distinct constraints.',
    'If ε is reading-relative, the kernel is not a single arrangement but a contested label — the framework''s ε-invariance test forces decomposition (already done here). If ε converges across readings, the kernel is one constraint with multiple framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the committer frame (kernel + readings) preserves ε-invariance or requires the kernel itself to be decomposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the state_killing_legitimacy kernel. The deterrence_reading claims a consequentialist coordination function with moderate ε (contested evidence). The retributive_reading claims a deontological desert function with different beneficiary/victim structure. The abolition_reading claims the arrangement is a snare from all seats. All three share the same physical enforcement machinery but have different ε, different claimed_types, and different structural relationships to the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
