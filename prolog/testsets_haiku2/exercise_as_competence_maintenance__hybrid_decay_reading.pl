% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance with Judgment Decay
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   Safety and crisis-response organizations worldwide mandate simulation
 *   exercises as the primary vehicle for distributed competence maintenance.
 *   The constraint couples procedural competence (which simulation robustly
 *   improves) with judgment-under-uncertainty (which decays without
 *   real-stakes exposure). This reading instantiates the hybrid_decay_reading
 *   of the contested kernel exercise_as_competence_maintenance: the claim is
 *   that simulation exercises procedural competence but NOT
 *   judgment-under-stakes, and that the kernel's two components have
 *   different exercise requirements. The kernel is contested because sibling
 *   readings claim either that simulation is sufficient
 *   (simulation_sufficiency_reading) or that only lived catastrophe truly
 *   exercises the kernel (lived_catastrophe_necessity_reading). This reading
 *   coexists with both siblings; it forecloses neither. It asserts a
 *   structural bifurcation within the competence kernel itself.
 *
 * KEY AGENTS:
 *   - safety_administration: Sets the mandate, audits compliance, benefits from standardization.
 *   - operational_personnel: Invest time in drills, retain procedures, lose judgment capacity.
 *   - actual_crisis_subjects: Face degraded judgment capacity when crises deviate from drills.
 *   - judgment_capacity_researchers: Excluded from design; would argue for hybrid training.
 *   - post_crisis_investigators: Document judgment failures but lack authority to alter mandate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.72).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance with Judgment Decay").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'cae130a4-1cfd-4569-b0b2-d3affcc70e32').
narrative_ontology:cs_kernel_codification('cae130a4-1cfd-4569-b0b2-d3affcc70e32', distributed).
narrative_ontology:cs_authority_grounding('cae130a4-1cfd-4569-b0b2-d3affcc70e32', extraction).
narrative_ontology:cs_reading_relation('cae130a4-1cfd-4569-b0b2-d3affcc70e32', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('cae130a4-1cfd-4569-b0b2-d3affcc70e32', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('cae130a4-1cfd-4569-b0b2-d3affcc70e32', foundational, competence_kernel_bifurcated).
narrative_ontology:cs_axiom_status(competence_kernel_bifurcated, holdable).
narrative_ontology:cs_axiom_grounding('cae130a4-1cfd-4569-b0b2-d3affcc70e32', competence_kernel_bifurcated, empirically_contingent).
narrative_ontology:cs_axiom('cae130a4-1cfd-4569-b0b2-d3affcc70e32', foundational, judgment_requires_real_stakes).
narrative_ontology:cs_axiom_status(judgment_requires_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('cae130a4-1cfd-4569-b0b2-d3affcc70e32', judgment_requires_real_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('cae130a4-1cfd-4569-b0b2-d3affcc70e32', competence_as_unitary_procedure_based).
narrative_ontology:cs_drift_state('cae130a4-1cfd-4569-b0b2-d3affcc70e32', post_crisis_investigation_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cae130a4-1cfd-4569-b0b2-d3affcc70e32', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, safety_administration).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_standardization).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, operational_personnel).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, actual_crisis_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_curriculum_designers).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, competence_bifurcation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, mandates, and audits simulation exercises as the primary vehicle for maintaining crisis-response competence across dispersed operational units. Justifies simulation on cost, safety (no harm to real subjects), and coverage grounds (every operator can be exercised predictably). Administers the measurement of 'competence' through scenario completion metrics and procedure-adherence scoring. Sets the frequency and content of drills.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Invest time and cognitive effort in repeated simulation exercises (often during normal operations or in addition to baseline work). They retain procedural memory and muscle memory from drills and experience genuine coordination benefit: they know the checklist, they can execute steps without thinking, they have practiced hand positions and communication patterns. They also experience the hidden cost: when a real crisis deviates from the simulation script, their judgment-under-uncertainty degrades because the simulated environment removes exactly the conditions that sharpen real-time improvisation (stakes, time pressure, information degradation, failure consequences). The constraint extracts from them the assumption that simulation readiness equals crisis readiness.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, operational_personnel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, operational_personnel, beneficiary).

% When a crisis diverges from the simulation template, they face operational personnel whose procedural competence is high but whose judgment capacity has atrophied due to lack of real-stakes exercise. The mismatch surfaces as rigid adherence to inapplicable procedures, delayed improvisation, or frozen-decision states when conditions do not match the drill. They bear the cost in slower response, incomplete adaptation, and direct harm from judgment failures. They have no voice in the exercise design and no exit from the crisis.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, actual_crisis_subjects, payer,
    powerless, immediate, trapped, local).

% The constraint vindicates the doctrine that competence is standardizable, auditable, and distributable through formalizable procedures. Simulation exercises are the operational proof that this doctrine works: if all personnel execute the same checklist in a controlled environment, the system is certified as 'ready.' This vindication is non-agent (the proposition collects no direct rents) but shields the institutional architecture from the challenge that real competence requires judgment, which is context-singular and difficult to standardize.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_standardization, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_standardization).

% Cognitive science, naturalistic decision-making, and high-reliability research communities would argue that judgment-under-uncertainty improves only through exposure to real consequences and outcome feedback. They demonstrate that simulation-trained personnel show poor transfer to novel conditions and degraded calibration in time-pressured, information-poor environments. They are excluded from competence-assessment decisions by the institutional preference for procedure-based metrics over judgment metrics.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, judgment_capacity_researchers, excluded,
    powerful, generational, analytical, global).

% Hold a professional stake in simulation's sufficiency: their careers, expertise, and institutional role depend on the framing that well-designed drills constitute genuine competence maintenance. They benefit from the constraint insofar as it protects their domain from the claim that simulation alone is insufficient and that real-stakes or high-fidelity scenario training is necessary. They face pressure from operational personnel who report judgment deficits in actual crises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_curriculum_designers, beneficiary,
    moderate, biographical, constrained, national).

% Conduct after-action reviews and incident investigations. They observe and document cases where operational personnel followed the drilled procedure correctly but the crisis state was not a drill state, and where judgment failures contributed to harm. They produce evidence that the constraint's operation is failing but lack the authority to alter the simulation mandate directly.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, post_crisis_investigators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, safety_administration).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, predictable, low-risk training environment in which all personnel can practice and validate procedural knowledge and muscle memory for crisis response. Solves the coordination problem: how to distribute competence across dispersed operators without exposing real subjects to training risk, while maintaining consistent standards and auditability.
% TRANSFER_FUNCTION: Moves cognitive effort and time from operational_personnel and actual_crisis_subjects (who bear the cost of judgment atrophy) to safety_administration and procedural_standardization (which benefit from standardization, auditability, and institutional stability). The transfer is hidden: the extraction is not visible in the simulation mandate itself but emerges when the real crisis deviates from the script.
% ABSENT_VOICES: Judgment-capacity researchers, actual crisis subjects (they have no input into exercise design), and the subset of operational personnel who have experienced judgment failure in real-stakes crises are structurally absent from the design-and-assessment loop. Their testimony would argue for hybrid training regimes (simulation + managed real-stakes exposure) or for judgment-literacy curricula alongside procedure standardization. They are kept out because their claims challenge the sufficiency narrative.
% DISAPPEARANCE_RATIONALE: If the simulation mandate vanished and exercise authority decentralized, organizations would move toward hybrid training: some simulation (retained for cost and safety), some managed real-stakes exposure or high-fidelity scenario training emphasizing judgment under uncertainty. Crisis response times would increase initially (less uniformity), but judgment capacity would improve. The procedural_standardization doctrine would lose institutional vindication and would face direct challenge from judgment-centric curricula.
% FOUNDING_PROBLEM: Early distributed crisis-response systems (public health, emergency management, hazmat response) had no uniform way to maintain competence across dispersed personnel. Some operators were highly trained; others were undertrained or deskilled. Real-crisis exposure was infrequent and dangerous for training subjects. Simulation exercises solved the distribution and safety problem: every operator could be exercised, costs were controlled, and no real subjects were harmed.
% FOUNDING_PROBLEM_CORROBORATION: Safety administration attests the founding problem remains live: operators still need distributed, repeatable, safe training. Researchers in high-reliability organizations (Weick, Roberts, NASA studies), post-crisis investigators (NTSB, aviation safety boards), and operational personnel working in crises that deviated from drills attest that the founding problem (uniform, safe competence distribution) has been solved, but a SECOND problem has been created: judgment-capacity atrophy. The constraint persists as procedure maintenance despite the shifted functional picture.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading measures extractiveness at 0.68 (rising from 0.45 over the interval), capturing the hidden cost: procedural benefit is real, but judgment decay accumulates over time as real-crisis exposure remains infrequent and simulation standardizes around known scenarios. Suppression is high (0.72) because the constraint's persistence depends on institutionally suppressing the claim that judgment requires different training than procedures — the suppression is not physical (personnel are not coerced to attend drills) but structural (the institutional framework blocks challenge to the simulation_sufficiency reading). Theater ratio is high and rising (0.35 → 0.58) because an increasing share of the simulation activity is defensive: ensuring the mandate is complied with and documented, rather than improving actual crisis response. The measurement series tracks the constraint's drift as the contradiction between the founding problem (distribute training safely) and the lived problem (maintain judgment capacity) becomes more visible, and as the theater ratio rises to defend the mandate against that visibility. All metrics share one time grid (t0, 5, 10, 15, 25, 40) so the engine can read the correlation.
 *
 * PERSPECTIVAL GAP:
 *   From the safety_administration seat: the constraint is genuine coordination and protective. Simulation is cost-effective, distributable, and safe. Judgment failures are operator failures, not design failures. From the operational_personnel seat (and especially those who have experienced judgment gaps in real crises): the constraint is bifurcated extraction disguised as competence maintenance. The procedural benefit is real and valuable; the judgment cost is hidden until a crisis deviates from the script. From the actual_crisis_subjects seat: the constraint is pure extraction. They bear the cost of judgment failures they had no voice in creating. The engine computes these divergences from the structural data: safety_administration is the beneficiary with high power and arbitrary exit (arbitrage); operational_personnel are payers with constrained exit; actual_crisis_subjects are trapped powerless payers. The directionality values (d near 0.0 for agenda-setter, d near 1.0 for victims) yield different type classifications per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety_administration benefits from the constraint (standardized competence is institutional asset) and has exit at the arbitrage level — they can change the mandate at will. Their effective extraction should be near zero or negative (they collect benefit). operational_personnel benefit from procedures (genuine coordination) but pay in judgment atrophy (extraction) and cannot exit — their constrained exit and payer role yield moderate d (near 0.5–0.6 range after adjustment). actual_crisis_subjects are trapped, powerless, and pure payers (they experience only judgment-failure harm, no procedural benefit). Their d should be near 1.0 (full target). No directionality overrides are needed; the derivation chain (beneficiary/victim + exit options) produces accurate per-seat directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distribute training safely) is no longer live in the same form: training distribution is solved. What persists is not the original problem but a procedural regime that vindicates a doctrine (procedures are sufficient for competence) despite emerging evidence to the contrary. The constraint shows classic mandatrophy: the original founding function (solve the distribution and safety problem) has substantially achieved its aim; the constraint persists as a tangled rope that extracts from judgment capacity to maintain institutional belief in procedure-sufficiency. The measurement series shows extraction rising while the founding problem's salience falls — a signal of mandatrophy drift. The high theater_ratio (institutional activity defending the mandate rather than improving crisis response) corroborates the mandatrophy reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_kernel_bifurcation_contestation,
    'Kernel claim: Is the competence kernel inherently bifurcated (procedure vs. judgment as distinct components with different exercise paths), or is it a unified property being artificially split by this reading''s framing?',
    'Cognitive science and high-reliability literature review: how do expert researchers characterize competence in crisis response? Is judgment separable from procedural knowledge or emergent from it? Operationalize competence in a way that does not presume the bifurcation, and measure both procedural and judgment components across operators trained under different regimes.',
    'If the bifurcation is real, the hybrid_decay_reading is structurally sound and the extraction is inescapable without changing training regimes. If the competence kernel is unitary and the bifurcation is a reading artifact, the constraint may be a rope or even a scaffold (redesignable) rather than a permanent tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_kernel_bifurcation_contestation, conceptual, 'Whether competence bifurcation is structural or a reading artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(exer_tr_t0, observed).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(exer_tr_t5, observed).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(exer_tr_t10, observed).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(exer_tr_t15, observed).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement_basis(exer_tr_t25, observed).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(exer_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(exer_be_t0, observed).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(exer_be_t5, observed).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(exer_be_t10, observed).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(exer_be_t15, observed).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(exer_be_t25, observed).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(exer_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(exer_su_t0, observed).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(exer_su_t5, observed).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(exer_su_t10, observed).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(exer_su_t15, observed).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(exer_su_t25, observed).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(exer_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% The kernel exercise_as_competence_maintenance has three structurally distinct readings, each a separate constraint story. hybrid_decay_reading (this file) claims competence is bifurcated (procedure vs. judgment) and simulation exercises only procedure. simulation_sufficiency_reading claims fidelity determines competence retention fully. lived_catastrophe_necessity_reading claims only real stakes exercise the true kernel. All three readings coexist as live institutional positions; none forecloses the others. The readings differ in what they take the competence kernel TO BE and in what they claim about the sufficiency of simulation. Link them via network.affects_constraints to enable the corpus to track the kernel contestation and the institutional dynamics of suppression/visibility across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
