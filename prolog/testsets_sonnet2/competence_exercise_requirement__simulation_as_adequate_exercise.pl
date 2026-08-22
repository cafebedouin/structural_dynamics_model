% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: High-Fidelity Simulation as Adequate Competence Exercise
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'simulation_as_adequate_exercise' reading of
 *   the competence exercise requirement kernel found across high-reliability
 *   domains (aviation, nuclear operations, surgical training, emergency
 *   response). On this reading, high-fidelity simulation with structured
 *   debriefing is a complete and sufficient exercise of the
 *   competence-maintenance function — regulatory sign-off on simulator hours
 *   is treated as equivalent to real-world exposure, and decades without
 *   catastrophic failure in simulation-trained fleets are read as validation
 *   of the standard. This is deliberately ONE reading among three siblings
 *   sharing the same kernel: catastrophe_as_necessary_anchor (which holds
 *   that only real catastrophic exposure provides irreducible exercise) and
 *   hybrid_dependency (which holds simulation necessary but insufficient,
 *   requiring periodic real-world anchoring). Those sibling readings are
 *   separate constraint stories, not alternative framings folded into this
 *   one; this story's epsilon is authored solely from this reading's own
 *   lights, applied to the standing arrangement (simulation-only
 *   certification as currently practiced and enforced) it is about.
 *
 * KEY AGENTS:
 *   - airline_and_operator_management: sets training calendar and certifies competence via simulator sign-off
 *   - simulator_training_providers: sell fidelity and debrief methodology as the certified sufficient path
 *   - regulatory_agencies: write standards recognizing simulator hours as adequate exercise
 *   - line_pilots_and_operators: complete recurrent sessions, benefit from safe practice, bear fidelity-gap risk
 *   - junior_operators_denied_real_exposure: accumulate simulator hours without real anchoring, powerless to change this
 *   - passengers_and_public: bear residual tail-risk with no visibility or voice
 *   - safety_investigators_and_researchers: retrospectively examine whether simulation fidelity gaps caused competence failures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "High-Fidelity Simulation as Adequate Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8').
narrative_ontology:cs_kernel_codification('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', formalized).
narrative_ontology:cs_authority_grounding('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', expertise).
narrative_ontology:cs_interpretation_layer_present('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8').
narrative_ontology:cs_reading_relation('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', foundational, high_fidelity_debrief_cycle_definitionally_complete).
narrative_ontology:cs_axiom_status(high_fidelity_debrief_cycle_definitionally_complete, holdable).
narrative_ontology:cs_axiom_grounding('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', high_fidelity_debrief_cycle_definitionally_complete, instrumental).
narrative_ontology:cs_axiom('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', secondary, catastrophe_free_interval_constitutes_validation).
narrative_ontology:cs_axiom_status(catastrophe_free_interval_constitutes_validation, holdable).
narrative_ontology:cs_axiom_grounding('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', catastrophe_free_interval_constitutes_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', regulatory_simulator_certification_standard).
narrative_ontology:cs_drift_state('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', contemporary_multi_decade_operation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3ec1559c-ebbd-4bc1-8725-551d2ac0ccf8', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_providers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_and_operator_management).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots_and_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, passengers_and_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, junior_operators_denied_real_exposure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots_and_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training budgets and schedules, decides simulation cycle frequency, and certifies crews as competent based on simulator performance and debrief records. Benefits from lower costs versus maintaining real-world exposure programs (line time, non-jeopardy audits) and from the regulatory sufficiency of simulation-based sign-off.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_and_operator_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell simulator time, fidelity upgrades, and debrief methodology as the certified path to competence maintenance. Their business model depends on simulation being treated as sufficient rather than supplementary; expanding the definition of 'adequate exercise' to include real anchoring would shrink their addressable service.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_providers, beneficiary,
    organized, biographical, arbitrage, global).

% Write the certification standards that recognize simulator hours and debrief records as satisfying recurrent competence requirements. Benefit administratively from a checkable, auditable proxy (simulator logs) rather than a harder-to-verify real-world exposure requirement; catastrophe-free intervals are cited as validating the standard.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies, beneficiary).

% Complete recurrent simulator sessions to remain certified; benefit from lower-stakes practice of emergency procedures without real risk, but bear the cost if simulation fidelity gaps mean certain reflexes or judgment calls (rare edge-case sensory or decision environments) go untested until a real event surfaces the gap.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots_and_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots_and_operators, payer).

% Enter service having accumulated simulator hours but limited real-world non-jeopardy exposure, because the institution has decided simulation is sufficient and real exposure programs are not funded or scheduled. Cannot unilaterally obtain more real-world time; their competence trajectory is set entirely by the reading their employer adopts.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, junior_operators_denied_real_exposure, payer,
    powerless, biographical, trapped, national).

% Bear the residual risk if simulation-adequate competence proves insufficient in a genuine edge case the simulator model did not capture with fidelity. Have no visibility into, or voice in, whether the operator's training regime is simulation-only or hybrid; their safety depends entirely on a reading they did not choose and cannot audit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, passengers_and_public, payer,
    powerless, immediate, trapped, global).

% Examine incident and near-miss records after the fact to determine whether competence gaps trace to simulation fidelity limits. Their retrospective findings are the primary evidence that could shift the kernel toward hybrid_dependency or catastrophe_as_necessary_anchor, but they operate only after failures occur, not prospectively.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_investigators_and_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_providers).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates recurrent competence verification across large operator workforces using a standardized, repeatable, low-risk training modality — allowing regulators, employers, and crews to agree on a common, auditable definition of 'currently competent' without requiring anyone to be exposed to real catastrophic conditions.
% TRANSFER_FUNCTION: Moves training cost from real-world exposure programs (line time, non-jeopardy audits, live anchoring) toward simulator vendor contracts and internal training departments; moves residual tail-risk from the institution's balance sheet onto passengers and the public, who absorb the consequence if simulation fidelity proves inadequate in a genuine edge case.
% ABSENT_VOICES: Passengers and the public have no seat in setting training standards and no visibility into whether their carrier follows simulation-only or hybrid competence maintenance. Junior operators denied real exposure would likely argue for hybrid_dependency but are institutionally powerless to change the training calendar.
% DISAPPEARANCE_RATIONALE: If the simulation-as-adequate reading were withdrawn as a certifying standard overnight, operators would be forced to fund and schedule real-world anchoring (line audits, actual aircraft time), training costs would rise sharply, simulator vendor revenue models would need restructuring, and near-term certification pipelines would bottleneck on scarce real-world exposure slots.
% FOUNDING_PROBLEM: Real catastrophic or near-catastrophic events are too rare, too costly, and too dangerous to serve as the primary vehicle for maintaining operator competence at scale — a workforce cannot be kept sharp by waiting for disasters, so a repeatable, safe substitute for high-stakes real exposure was needed.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and simulator training providers attest the founding problem is solved — decades of catastrophe-free operation in high-fidelity-simulation-trained fleets are cited as validation. Independent safety investigators and researchers, examining post-incident competence gaps in edge cases simulators failed to model faithfully, attest the founding problem is only partially solved and that simulation-only regimes have produced detectable blind spots — this corroboration comes from outside the training-provider and regulatory beneficiary set.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).
:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38 at interval end) rather than low: the reading has a genuine coordination function (standardized, auditable, safe competence verification at scale) but the standing arrangement also transfers real cost-avoidance value to training providers and management while shifting tail-risk onto passengers who cannot audit it — a structural asymmetry, not neutral coordination. Suppression is moderate (0.42): the standard is enforced through certification requirements rather than raw coercion, but crews and junior operators cannot unilaterally obtain more real-world exposure even if they judge simulation insufficient for a specific edge case. Theater ratio rises modestly (0.12 to 0.31) reflecting a documented drift where debrief and fidelity metrics increasingly serve as compliance artifacts (auditable checkboxes) rather than being reliably diagnostic of latent competence gaps — a Goodhart-style substitution risk this reading's proponents would dispute but which the temporal record shows accumulating.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and training-provider seat, catastrophe-free decades are proof the reading is correct and sufficient. From the safety-investigator seat, catastrophe-free decades are consistent with either 'the reading is correct' or 'the tail events simply have not occurred yet within the observed window' — a survivorship-bias concern the metrics cannot resolve on their own, which is why an omega variable is required rather than an adjustment to epsilon.
 *
 * DIRECTIONALITY LOGIC:
 *   Airline management and regulatory agencies sit near the beneficiary end: they set the standard, capture cost savings or administrative tractability, and can adjust policy at will (institutional power, arbitrage/analytical exit). Simulator training providers are pure beneficiaries with global exit and no exposure to the tail-risk their product is meant to mitigate. Line pilots occupy a mixed position — genuine beneficiaries of safe practice, but constrained payers if fidelity gaps surface in their own emergency. Junior operators and passengers are structural targets: powerless, trapped, bearing the downside of a reading they did not choose and have no mechanism to contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope (rather than tangled_rope or snare) reflects that the coordination function here is real and substantial — recurrent, safe, scalable competence verification solves a genuine problem that cannot be solved by waiting for catastrophes. The engine's seat-level computation is expected to diverge: the agenda-setter and beneficiary seats likely compute close to rope, while the powerless payer seats (junior operators, passengers) may compute nearer tangled_rope given their trapped exit and inability to audit or influence the standard. This divergence is the intended signal — it distinguishes 'coordination that works for most parties with a residual, unaudited tail-risk transferred downward' from either pure extraction or pure natural coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survivorship_bias_in_catastrophe_free_validation,
    'Does the absence of catastrophic failure over decades of simulation-only training validate the sufficiency of simulation, or does it reflect a small-sample survivorship artifact where the untested tail scenarios simply have not yet occurred?',
    'Compare near-miss and incident reports across fleets/operators with differing training regimes (simulation-only vs. hybrid) over a matched multi-decade window; examine whether documented competence failures cluster around scenario classes simulators are known to model with lower fidelity (rare sensory/environmental conditions, genuine high-consequence decision pressure).',
    'If failures cluster in low-fidelity-modeled scenario classes even under simulation-only regimes, this reading''s core empirical claim weakens substantially and the hybrid_dependency reading gains support; if no such clustering appears after controlling for base rates, this reading''s sufficiency claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_catastrophe_free_validation, empirical, 'Whether catastrophe-free intervals validate simulation sufficiency or merely reflect survivorship bias in rare-event data.').

omega_variable(
    simulator_fidelity_ceiling,
    'Is there a class of competence-relevant conditions (genuine physiological stress, irreversible real-world consequence perception, novel unmodeled failure combinations) that high-fidelity simulation is structurally incapable of reproducing, regardless of technological improvement?',
    'Physiological and decision-quality studies comparing crew performance under simulated versus real jeopardy conditions matched for scenario type; longitudinal tracking of whether simulator fidelity improvements measurably close performance gaps over successive technology generations.',
    'If a fidelity ceiling exists and is not closing with technology, this reading''s premise that ''sufficiently high fidelity'' resolves the exercise requirement is undermined in principle, not just in current practice — supporting hybrid_dependency as structurally necessary rather than merely currently prudent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulator_fidelity_ceiling, conceptual, 'Whether a structural fidelity ceiling exists that no amount of simulation technology improvement can close.').

omega_variable(
    committer_structure_reading_disagreement_locus,
    'Where exactly do the three kernel readings (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor, hybrid_dependency) locate their disagreement — is it about the definition of ''adequate exercise,'' about what evidence counts as validation, or about acceptable residual risk allocation?',
    'Structural analysis of each reading''s axioms: this reading holds fidelity+debrief as definitionally complete; catastrophe_as_necessary_anchor holds real irreducible exposure as definitionally necessary; hybrid_dependency holds both as jointly necessary. The disagreement is located at the definitional/axiomatic level (what counts as exercise), not at the empirical level (all three could agree on the same incident data and still disagree on what it implies about sufficiency).',
    'If the disagreement is genuinely definitional/axiomatic rather than empirical, no amount of additional incident data alone resolves the kernel contest — regulatory or professional consensus would need to adjudicate the definition itself, which is a preference-type resolution, not an empirical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_reading_disagreement_locus, conceptual, 'Locating whether the three sibling readings disagree definitionally, evidentially, or on risk allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.16).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.2).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.24).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.28).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_dependency).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the competence_exercise_requirement kernel per the epsilon-invariance principle. Each reading authors its own epsilon over the same referent (the standing simulation-centric certification arrangement, as that reading's own lights assess it) but reaches different conclusions about sufficiency. simulation_as_adequate_exercise (this story) authors moderate extraction (0.38) reflecting a genuine but partial coordination function with a transferred tail-risk; catastrophe_as_necessary_anchor and hybrid_dependency are expected to author different epsilon values reflecting their different assessments of whether the standing arrangement under-invests in real exposure. All three are linked bidirectionally via affects_constraints; none averages or references the others' epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
