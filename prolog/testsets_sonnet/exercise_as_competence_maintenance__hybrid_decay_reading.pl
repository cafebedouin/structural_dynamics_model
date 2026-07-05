% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   A crisis-response organization runs a mature, well-funded simulation
 *   exercise program: tabletop drills, scenario walkthroughs, and periodic
 *   full-scale simulations that responders complete on a recurring schedule
 *   and that regulators accept as satisfying statutory readiness
 *   requirements. Completion rates are high, certification is routine, and
 *   the organization's exercise metrics look excellent year over year. The
 *   claimed type here is tangled_rope: there is a genuine coordination
 *   function (procedural competence really is maintained and really does
 *   benefit everyone who needs procedures executed correctly), but there is
 *   also asymmetric extraction — the certification apparatus converts
 *   procedural fluency into a claim of general readiness that the exercise
 *   design was never built to support, and the gap is paid for by whoever
 *   encounters the non-scripted portion of a real crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.52).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '93262ce7-2bf0-48e5-94f8-5fa73154c994').
narrative_ontology:cs_kernel_codification('93262ce7-2bf0-48e5-94f8-5fa73154c994', distributed).
narrative_ontology:cs_authority_grounding('93262ce7-2bf0-48e5-94f8-5fa73154c994', practice).
narrative_ontology:cs_interpretation_layer_present('93262ce7-2bf0-48e5-94f8-5fa73154c994').
narrative_ontology:cs_reading_relation('93262ce7-2bf0-48e5-94f8-5fa73154c994', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('93262ce7-2bf0-48e5-94f8-5fa73154c994', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('93262ce7-2bf0-48e5-94f8-5fa73154c994', foundational, competence_kernel_is_structurally_partitioned).
narrative_ontology:cs_axiom_status(competence_kernel_is_structurally_partitioned, holdable).
narrative_ontology:cs_axiom_grounding('93262ce7-2bf0-48e5-94f8-5fa73154c994', competence_kernel_is_structurally_partitioned, empirically_contingent).
narrative_ontology:cs_axiom('93262ce7-2bf0-48e5-94f8-5fa73154c994', secondary, judgment_under_stakes_requires_irreversible_consequence_exposure).
narrative_ontology:cs_axiom_status(judgment_under_stakes_requires_irreversible_consequence_exposure, holdable).
narrative_ontology:cs_axiom_grounding('93262ce7-2bf0-48e5-94f8-5fa73154c994', judgment_under_stakes_requires_irreversible_consequence_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('93262ce7-2bf0-48e5-94f8-5fa73154c994', procedural_drill_as_readiness_proxy).
narrative_ontology:cs_drift_state('93262ce7-2bf0-48e5-94f8-5fa73154c994', post_maturation_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93262ce7-2bf0-48e5-94f8-5fa73154c994', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, institutional_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_scenarios).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, populations_affected_by_real_crises).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, junior_operators_denied_real_stakes_exposure).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the tabletop and simulation exercise calendar, set pass/fail criteria, and report completion rates upward as evidence of organizational readiness. Their professional standing depends on exercises running smoothly and producing certifiable outcomes, not on how the organization performs during an actual crisis they may never personally face.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Accept documented simulation completion as satisfying statutory readiness requirements. Their audit burden is discharged by checking exercise logs rather than by any measure of judgment-under-stakes performance, since no such measure is currently mandated or auditable.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers, beneficiary,
    institutional, generational, arbitrage, national).

% Cites exercise completion in public statements and budget justifications as proof of preparedness. Benefits from the reputational and liability cover the exercise regime provides, and typically rotates to new positions before any latent judgment-capacity gap is tested by a real event.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, institutional_leadership, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, institutional_leadership, agenda_setter).

% Have drilled the scripted procedural branches repeatedly and perform them fluently, but when a real incident deviates from the rehearsed scenario tree, must improvise under genuine stakes with no equivalent repetition behind that specific capacity. They cannot opt out of exercises without professional penalty, nor can they manufacture real-stakes experience on their own.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_scenarios, payer,
    moderate, immediate, constrained, local).

% Depend on responders' judgment during the unscripted portion of an actual emergency. They bear the consequences when procedural fluency does not translate into improvisational competence, and have no visibility into the exercise design that produced this gap until after a failure occurs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, populations_affected_by_real_crises, payer,
    powerless, immediate, trapped, regional).

% Are certified as competent based entirely on simulation performance, since organizational and legal structures deliberately shield them from real-stakes exposure during training. They enter their careers with procedural skill but an untested and likely thin judgment reserve, discovered only under pressure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, junior_operators_denied_real_stakes_exposure, payer,
    powerless, biographical, constrained, local).

% Understand from professional experience that scripted simulations cannot replicate the branching uncertainty and consequence-weight of real stakes, and have proposed higher-fidelity or stress-inoculation designs. Their recommendations are frequently deprioritized because higher-fidelity exercises cost more, run less predictably, and are harder to certify as 'passed.'
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_design_specialists, excluded,
    moderate, biographical, constrained, national).

% Conduct after-action analysis following real incidents and can compare pre-incident exercise records against actual performance under stakes. They are positioned to detect the procedural/judgment split but their findings are advisory and not binding on the exercise program's design.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, post_incident_review_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation exercises genuinely coordinate and maintain procedural competence: checklist execution, equipment handling, communication protocols, and role assignments are demonstrably retained and improved through repeated drilling, and this benefits everyone who might need those procedures executed correctly.
% TRANSFER_FUNCTION: Moves liability comfort and certifiable readiness credit from the organization's actual crisis-response capacity to its documented exercise-completion record; moves the cost of the resulting judgment-capacity gap onto whoever is present when a real, non-scripted crisis occurs.
% ABSENT_VOICES: Exercise design specialists who advocate higher-fidelity, higher-uncertainty training are structurally deprioritized because their proposals are costlier and less certifiable; affected populations in future incidents have no seat in the exercise design process at all, since they are not yet identifiable individuals.
% DISAPPEARANCE_RATIONALE: Administrators and compliance officers would say the world rearranges catastrophically if exercises stopped — procedural competence would erode and audits would fail. Post-incident review boards and exercise design specialists would say the world changes less than claimed, because the judgment-under-stakes component the exercises were never actually building would remain exactly as thin as it already is; only the paperwork trail would vanish.
% FOUNDING_PROBLEM: Organizations needed a repeatable, auditable, low-cost mechanism to keep procedural knowledge fresh between rare real incidents, since letting procedures atrophy between actual crises produces avoidable failures on the parts that ARE scriptable.
% FOUNDING_PROBLEM_CORROBORATION: Exercise administrators and compliance officers attest the founding problem remains fully live and fully addressed by the current regime. Post-incident review boards — an audience outside the benefiting administrative chain — attest in after-action reports that the procedural component of the founding problem is addressed while the judgment-under-stakes component was never targeted by the exercise design and remains as unaddressed as before the program existed.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the exercise regime extracts certification value, budget justification, and liability cover disproportionate to what it actually maintains — it is real coordination for the procedural half and a false credential for the judgment half. Suppression (0.52) reflects the structural and institutional pressure against higher-fidelity, higher-uncertainty exercise designs that would surface the gap; it is moderate rather than severe because the suppression operates through cost and career incentives rather than direct coercion. Theater ratio rises across the measured interval (0.30 to 0.61) as the exercise program matures: early on, exercises were closer to genuine skill-building; over time, exercises increasingly optimize for certifiable, repeatable, low-variance scenarios that produce clean pass rates rather than for the harder, messier scenarios that would actually stress judgment capacity — a textbook Goodhart drift from the founding problem toward the measurable proxy. Accessibility collapse (0.42) is moderate: alternatives to the current exercise design (stress inoculation, red-teaming, deliberately unscripted drills) exist and are known to specialists but have been progressively deprioritized, not eliminated. Resistance (0.55) reflects active pushback from exercise design specialists and post-incident review boards, which the administrative chain absorbs without structural change.
 *
 * DIRECTIONALITY LOGIC:
 *   Exercise administrators, compliance officers, and institutional leadership sit near the beneficiary end: they collect certification value, audit closure, and reputational cover, and their exit options (arbitrage, mobility) let them rotate away from any eventual reckoning. Frontline responders, junior operators, and affected populations sit near the target end: responders and junior operators are trapped inside a certification regime they cannot individually correct, and affected populations bear the tail risk of the judgment gap with zero visibility into its existence beforehand. The asymmetry is structural, not moral — administrators are not acting in bad faith, but the incentive gradient rewards certifiable procedural performance and does not reward (and actively penalizes, via cost and unpredictability) investment in the judgment component.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — procedural atrophy between rare real incidents — is genuinely still live and the exercise regime genuinely still addresses it; this prevents mislabeling the whole arrangement as pure extraction. But the regime's mandate has quietly expanded from 'maintain procedural competence' to 'certify general readiness,' and that expanded mandate has outlived what the underlying mechanism can support. The hybrid_decay reading exists precisely to prevent both over-corrections: crediting simulation with more than it delivers (the simulation_sufficiency_reading's error) and discrediting simulation for the real procedural work it does accomplish (a risk of overcorrecting toward the lived_catastrophe_necessity_reading). The tangled_rope classification holds both truths in the same structure rather than forcing a choice between rope and snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_partition_validity,
    'Is the procedural/judgment-under-stakes partition a real structural feature of the competence kernel, or is it an artifact of how post-incident review boards frame failures after the fact?',
    'Controlled comparison of responder performance on scripted versus deliberately unscripted stress-inoculation scenarios, isolating whether performance divergence tracks the claimed partition or some other variable (fatigue, scenario novelty generally, team composition).',
    'If the partition holds up under controlled testing, this reading''s core distinguishing claim is vindicated over both siblings. If performance divergence does not track the procedural/judgment boundary specifically, the hybrid reading collapses toward one of the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_partition_validity, empirical, 'Whether the two-component kernel partition is empirically real or a post-hoc framing.').

omega_variable(
    simulation_ceiling_uncertainty,
    'Is there a fidelity threshold above which simulation DOES begin to exercise judgment-under-stakes, or is the gap categorical regardless of production values, realism, or consequence-framing within the simulation?',
    'Longitudinal tracking of responders trained under progressively higher-fidelity simulation (VR, full-scale live-actor exercises, career-consequential stakes within training) against real-incident judgment outcomes, to locate any fidelity threshold or confirm its absence.',
    'If a fidelity threshold exists, this reading''s decay claim is only true at current, sub-threshold fidelity, and the arrangement would be better read as a temporary scaffold toward higher-fidelity simulation rather than a stable tangled_rope. If no threshold exists at any achievable fidelity, this reading is confirmed as durable rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_ceiling_uncertainty, empirical, 'Whether the procedural/judgment gap is closeable by simulation fidelity or is categorical.').

omega_variable(
    which_reading_the_regulator_actually_certifies,
    'When compliance officers accept exercise completion as satisfying statutory readiness requirements, are they implicitly certifying under the simulation_sufficiency_reading, unaware that the hybrid_decay_reading is the operative structural truth?',
    'Review of the statutory and regulatory language defining ''readiness'' to determine whether it specifies procedural competence, general readiness including judgment, or is silent/ambiguous on the distinction.',
    'If regulatory language assumes simulation_sufficiency while the operative reality is hybrid_decay, the certification regime is built on a category error with direct liability and mandatrophy consequences — the vindicated_propositions status of procedural_readiness_doctrine would need re-examination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_the_regulator_actually_certifies, conceptual, 'Whether regulatory certification language matches the hybrid_decay structural reality or assumes the simulation_sufficiency framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.51).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.56).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_decay reading within the exercise_as_competence_maintenance kernel family. simulation_sufficiency_reading holds that fidelity alone determines full retention (no categorical gap); lived_catastrophe_necessity_reading holds that simulation never exercises the kernel at all and only real catastrophe does. This reading partitions the kernel into two components with different exercise requirements, producing a distinct beneficiary/victim structure (a genuine but partial coordination function, plus a specific victim class harmed by the unexercised judgment component) that neither sibling reading's single-component model produces. ε is not shared across the three readings — each has its own stable, non-averaged extraction value reflecting its own structural claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
