% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual Catastrophe-Memory Preservation (Survival Competence Reading)
 *   domain: religious/collective/behavioral
 *
 * SUMMARY:
 *   This reading interprets the catastrophe-memory-preservation kernel as a
 *   constraint that preserves operational, embodied threat-recognition
 *   capacity across generations. Communities maintain ritual practices
 *   encoding survival knowledge from historical catastrophes (famine, plague,
 *   natural disaster, invasion). The ritual demands costly participation:
 *   emotional labor, time, physical risk, and reproduction of traumatic
 *   narratives. Participants experience this as extraction — loss of
 *   autonomy, submission to custodian authority, inability to exit without
 *   severing community bonds. The beneficiary is future generations and the
 *   collective survival probability. The constraint is claimed as
 *   tangled_rope: it coordinates genuine survival-competence transfer while
 *   extracting heavily from present participants. The crucial question is
 *   whether this coordination cannot be achieved at lower cost through modern
 *   means (formal training, documentation, simulation). If embodied ritual is
 *   truly necessary, the extraction is coordination overhead; if not, the
 *   constraint is masking extraction in a survival-necessity frame.
 *
 * KEY AGENTS:
 *   - ritual_custodians: Authorized transmitters who maintain the ritual knowledge and enforce participation standards. Identity locked to custodianship; benefit from authority but bear admin cost.
 *   - present_generation_participants: Carry the extraction directly — emotional labor, time cost, constrained exit. Lack voice in design.
 *   - future_generations: Powerless beneficiary; inherit competence but cannot consent to the cost imposed on their predecessors.
 *   - community_authorities: Institutional agenda-setter who believe catastrophe-competence is non-negotiable and ritual-only. Benefit from collective cohesion and survival preparation.
 *   - youth_with_exit_paths: Structurally excluded from authority but have real alternatives (migration, formal education). Experience ritual as obstacle; excluded from framing what counts as valid knowledge.
 *   - historical_catastrophe_survivors: Observers who testify that the threat-patterns embedded in ritual were validated under actual crisis conditions — the primary external corroboration of the founding problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Catastrophe-Memory Preservation (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious/collective/behavioral").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '3aeb35f5-3d2e-4294-95e8-95a9c81126fd').
narrative_ontology:cs_kernel_codification('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', distributed).
narrative_ontology:cs_authority_grounding('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', lineage).
narrative_ontology:cs_interpretation_layer_present('3aeb35f5-3d2e-4294-95e8-95a9c81126fd').
narrative_ontology:cs_reading_relation('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', catastrophe_memory_preservation__mourning_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', foundational, embodied_knowledge_operationally_necessary).
narrative_ontology:cs_axiom_status(embodied_knowledge_operationally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', embodied_knowledge_operationally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', foundational, ritual_form_inseparable_from_function).
narrative_ontology:cs_axiom_status(ritual_form_inseparable_from_function, holdable).
narrative_ontology:cs_axiom_grounding('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', ritual_form_inseparable_from_function, instrumental).
narrative_ontology:cs_reference_frame('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', ritual_as_operational_competence_transfer).
narrative_ontology:cs_drift_state('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3aeb35f5-3d2e-4294-95e8-95a9c81126fd', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the ritual imposes sustained, measurable costs on participants (embodied labor, emotional demand, time, constrained exit) to deliver a benefit that accrues primarily to future generations. The constraint is enforced: community social pressure, identity-fusion of the custodian role, and framing of exit as knowledge-loss all suppress alternatives. Theater ratio is low-moderate (0.28): the ritual does serve a coordination function (knowledge transmission), but a growing proportion of enforcement activity defends the ritual form itself rather than the underlying threat-competence — there is increasing documentary evidence, formal training, and external knowledge sources that could serve the stated function, yet the ritual persists and is actively defended even where alternatives would satisfy the coordination goal. Suppression is moderate (0.45): not all suppression is violent exclusion; much of it is identity-fusion and kinship embedding. The measurement series shows extractiveness rising slightly over time and plateauing (0.58→0.68), theater_ratio increasing (0.18→0.28) — both consistent with slow atrophy of the coordination function and increasing defensiveness of the ritual form. This reading treats the constraint as genuine coordination (tangled_rope) because I author the premise that embodied ritual transmission is operationally superior to alternatives; the hybrid-atrophy reading (sibling) would interpret the same data as evidence that function has degraded and only theater persists.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_custodians and community_authorities sit in the coordinate-and-transmit seat: they experience the constraint as a genuine coordination challenge (how do you encode and transmit embodied knowledge?) whose solution is the ritual's demanding form. From their seat, the high participation cost is the price of fidelity; the alternative (written instruction, video, formal training) seems inadequate because it has failed in past catastrophes or because they have been trained to distrust non-embodied transmission. present_generation_participants sit in the extraction seat: they experience the constraint's cost (autonomy, emotional labor, exit bars) immediately and directly; the benefit (their children's survival competence) is future and abstract. They lack the authority-seat's epistemic certainty that the ritual is the only means. The engine computes these divergences from the structural data: beneficiary vs. victim declarations, power atoms, exit options. That divergence IS the phenomenon the corpus is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   present_generation_participants: high d (near 1.0, target end) — they bear extraction costs, have constrained exits, and receive no direct benefit. ritual_custodians: moderate d (~0.4) — they set/enforce the arrangement and collect authority/identity benefits, but also bear administrative costs and are identity-locked to its maintenance. future_generations: reversed d (near 0.0, beneficiary end) — they receive the competence benefit, but are powerless and cannot consent. community_authorities: near-beneficiary d (~0.25) — they benefit from collective cohesion and survival preparation but bear enforcement costs and face growing pressure to justify the extraction. The directionality divergence between payers (high d, experience extraction) and agenda-setters (moderate d, experience coordination) is the structural seat divergence: the engine will compute tangled_rope from the payer perspective and rope or even mountain from the custodian perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is substantive (historical catastrophe validated the threat-patterns embedded in ritual), but the founding-problem status is contested. Present elders report the problem as live (new threats continually emerge); external observers argue the problem is largely solved by early-warning systems and formal emergency response. The measurement series shows slight theater-ratio increase over the 40-year interval (0.18→0.28), suggesting a small drift toward performative maintenance. However, the theater ratio remains well below 0.5, and the extractiveness plateaus rather than cascading upward, which is inconsistent with pure piton (theater-dominant). This reading authoritatively interprets the constraint as tangled_rope: the coordination function (transmitting embodied threat-competence) is genuine and operationally necessary, even if the extraction cost is asymmetric and high. The hybrid-atrophy reading (sibling) interprets the same data as evidence of function decay and would classify as piton. The divergence cannot be resolved by metrics alone; it depends on the contested empirical question of whether embodied ritual transmission is truly superior to alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_vs_explicit_knowledge_transfer,
    'Is the threat-recognition competence genuinely inseparable from embodied ritual performance, or can it be transmitted equally through explicit instruction, simulation, and formal training?',
    'Controlled comparison: a cohort trained via modern disaster-response protocols + documentary transmission; another cohort via traditional ritual. Assess threat-pattern recognition speed, accuracy, stress-response coherence, and multi-generational retention in both groups over 20+ years.',
    'If embodied transfer is necessary, the extraction cost is justified as coordination; if explicit transfer is equivalent, the ritual is pure extraction wearing a competence disguise. This is the core empirical question that separates tangled_rope from snare in this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embodied_vs_explicit_knowledge_transfer, empirical, 'Whether embodied ritual transmission of survival competence is structurally necessary or merely traditional.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.45) primarily structural (exit barriers, kinship costs, resource dependency) or internalized (participants believe they deserve the extraction, have been isolated from alternative framings)?',
    'Post-exit trajectory: do participants who leave and access external knowledge-sources retain suppression behaviors, or does suppression decay when structural barriers are removed? Do youth who exit report relief or continued guilt/incompleteness?',
    'Structural suppression means the constraint persists by external coercion and would collapse if barriers weakened; internalized suppression means the constraint carries forward even after exit, indicating deeper capture of self-concept through the ritual''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression in ritual participation.').

omega_variable(
    reading_kernel_ambiguity,
    'Is this reading (survival competence) the correct instantiation of the catastrophe-memory-preservation kernel, or does the hybrid-atrophy reading better capture the empirical state?',
    'Historical testimony from elders: do they describe the ritual as currently functioning survival-knowledge-transfer (supporting this reading), or as degraded practice that once did so but now mostly serves mourning/identity (supporting hybrid atrophy)? Functional assessment: do communities that maintain strong ritual performance actually demonstrate measurably higher threat-recognition competence in real emergencies?',
    'If atrophy is the empirical state, this reading''s core claim (that ritual preserves operational competence) is false, and the constraint should be reclassified to piton (theater persists, function atrophied). If survival-competence transfer remains live, this reading''s tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, empirical, 'Whether this reading''s core premise (ritual preserves operational survival competence) is empirically true or a false claim about a degraded function.').

omega_variable(
    future_generation_agency_deficit,
    'Can future generations meaningfully consent to the extraction imposed on their predecessors in the name of their benefit, or is the asymmetry (powerless beneficiary, powerless voice) a structural feature of this constraint that makes true coordination impossible?',
    'Institutional reform: allow present-generation participants to opt-out and future generations to opt-in (formally consent to receive the knowledge via ritual once they come of age). If participation rates collapse, the extraction was not truly consensual.',
    'If future consent cannot be secured, the constraint is not genuine coordination even if the knowledge transfer is operationally necessary — it is extraction justified by a constituency that cannot speak. The remedial implication is that knowledge must be preserved in ways that both generations consent to.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generation_agency_deficit, conceptual, 'Whether asymmetric temporal benefit structure permits genuine coordination or necessitates reclassification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% Three readings of a single kernel (catastrophe_memory_preservation) represent different structural interpretations of the same ritual practice. This reading (survival_competence) treats the ritual as coordinating genuinely necessary embodied knowledge transfer; mourning_practice reading treats the transfer claim as secondary/symbolic; hybrid_atrophy reading treats the function as degraded. All three are linked as constraint family members. The ε values diverge substantially (0.68 here; ~0.74+ in mourning/snare reading; ~0.65+ in atrophy/piton reading) because the readings attribute different functions and beneficiaries to the same ritual form. ε-invariance is maintained: each reading has one stable ε and one consistent structural story. The inter-reading divergence is the point: it captures the empirical indeterminacy of whether the ritual's core function is operational survival competence, symbolic mourning, or degraded performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
