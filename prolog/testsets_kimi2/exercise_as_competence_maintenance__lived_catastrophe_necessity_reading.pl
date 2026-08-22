% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Real-Catastrophe Competence Validation Doctrine
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In safety-critical industries, a doctrinal commitment holds that only
 *   operators who have survived actual catastrophes possess validated
 *   competence, while simulation remains mere rehearsal. This reading of the
 *   competence-maintenance kernel treats covert atrophy as the default fate
 *   of untested operators and privileges a scarce 'tested' class with status
 *   and authority. The constraint is claimed as coordination (ensuring only
 *   truly capable operators lead in crisis) but structurally extracts from
 *   the untested and imposes risk on the dependent public.
 *
 * KEY AGENTS:
 *   - crisis_management_elite: Primary agenda-setter (institutional/constrained) â administers the doctrine through hiring and standards
 *   - veteran_operators: Primary beneficiary (organized/identity_locked) â collect status and authority from the scarcity of validated competence
 *   - untested_operators: Primary payer (moderate/constrained) â bear devaluation and exclusion despite simulation training
 *   - public_dependents: Secondary payer (powerless/trapped) â bear risk exposure from operators assumed to have atrophied
 *   - simulation_training_vendors: Excluded voice (organized/constrained) â would advocate for simulation legitimacy but are marginalized
 *   - safety_science_researchers: Analytical observer (institutional/analytical) â study competence retention but are overridden by practice-based authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.63).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.73).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Real-Catastrophe Competence Validation Doctrine").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '04f94200-5ae5-4002-9af2-55e820cd7c1e').
narrative_ontology:cs_kernel_codification('04f94200-5ae5-4002-9af2-55e820cd7c1e', implicit).
narrative_ontology:cs_authority_grounding('04f94200-5ae5-4002-9af2-55e820cd7c1e', practice).
narrative_ontology:cs_interpretation_layer_present('04f94200-5ae5-4002-9af2-55e820cd7c1e').
narrative_ontology:cs_reading_relation('04f94200-5ae5-4002-9af2-55e820cd7c1e', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('04f94200-5ae5-4002-9af2-55e820cd7c1e', exercise_as_competence_maintenance__hybrid_decay_reading, forecloses).
narrative_ontology:cs_axiom('04f94200-5ae5-4002-9af2-55e820cd7c1e', foundational, only_real_stakes_exercise_competence).
narrative_ontology:cs_axiom_status(only_real_stakes_exercise_competence, holdable).
narrative_ontology:cs_axiom_grounding('04f94200-5ae5-4002-9af2-55e820cd7c1e', only_real_stakes_exercise_competence, empirically_contingent).
narrative_ontology:cs_axiom('04f94200-5ae5-4002-9af2-55e820cd7c1e', foundational, covert_atrophy_without_activation).
narrative_ontology:cs_axiom_status(covert_atrophy_without_activation, holdable).
narrative_ontology:cs_axiom_grounding('04f94200-5ae5-4002-9af2-55e820cd7c1e', covert_atrophy_without_activation, empirically_contingent).
narrative_ontology:cs_reference_frame('04f94200-5ae5-4002-9af2-55e820cd7c1e', trial_by_fire_competence_standard).
narrative_ontology:cs_drift_state('04f94200-5ae5-4002-9af2-55e820cd7c1e', contemporary_safety_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04f94200-5ae5-4002-9af2-55e820cd7c1e', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_management_elite).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, veteran_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior safety officials and organizational leaders who have managed real crises and set institutional standards for competence validation. They enforce the doctrine through hiring requirements, promotion criteria, and training budget allocation. Their own legitimacy derives from real-crisis experience, creating a structural incentive to maintain the scarcity of validated competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_management_elite, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_management_elite, beneficiary).

% Operators who have survived actual catastrophes and whose competence is treated as uniquely validated. They receive preferential hiring, elevated status in training programs, and authority in safety discourse. Their professional identity is fused with real-stakes experience, making them strong advocates for the doctrine.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, veteran_operators, beneficiary,
    organized, biographical, identity_locked, national).

% Competent operators who have trained extensively in simulation but lack real-catastrophe experience. Their competence is systematically devalued in hiring and promotion; they are paid less or passed over for leadership roles. They cannot easily acquire the validating experience because catastrophes are rare and dangerous.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_operators, payer,
    moderate, biographical, constrained, national).

% General population dependent on emergency operators who may never have been tested under real stakes. They bear the risk of operator failure if covert atrophy exists, and they cannot verify or select for real-stakes experience when they call for help.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_dependents, payer,
    powerless, immediate, trapped, local).

% Organizations that develop high-fidelity simulation training. They would argue that well-designed simulation exercises genuine competence components, but their voice is marginalized in standards-setting bodies dominated by veteran operators and crisis elites.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_vendors, excluded,
    organized, biographical, constrained, national).

% Academic researchers studying skill retention, stress performance, and simulation fidelity. They produce evidence that challenges or supports the necessity of real-stakes activation, but their findings are often overridden in practice by the institutional authority of lived experience.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_science_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a hierarchy of competence credibility in high-stakes safety domains by ensuring that crisis leadership is drawn from operators who have survived real catastrophes, under the theory that only such experience validates genuine high-stakes performance capability.
% TRANSFER_FUNCTION: Moves status, authority, and economic opportunity from operators without real-catastrophe experience to those with it, and transfers risk exposure to the public dependent on operators whose competence is assumed to have covertly atrophied.
% ABSENT_VOICES: Simulation training vendors and safety scientists demonstrating simulation sufficiency are structurally excluded from standards-setting bodies; untested operators who dispute their own covert incompetence are dismissed as unproven and self-interested.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, hiring and promotion criteria would shift toward simulation-validated competence, veteran status would decouple from institutional authority, training budgets would rebalance toward high-fidelity simulation, and the public would face a contested but different risk profile regarding operator readiness.
% FOUNDING_PROBLEM: Safety-critical domains needed a way to distinguish genuinely competent operators from the merely trained in environments where failure costs are catastrophic and real events are too rare to provide routine feedback.
% FOUNDING_PROBLEM_CORROBORATION: Safety scientists and organizational researchers attest that the founding problemâdistinguishing real competence under stressâremains live, but dispute whether the trial-by-fire solution is the only or best approach. Veteran operators attest the problem is still live and only their approach solves it. No neutral corroboration accepts the necessity claim without qualification; the corroboration that does exist comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) reflects the substantial transfer of status and opportunity from untested to tested operators, plus the public risk externality. Suppression (0.73) is high because the doctrine actively devalues simulation credentials and excludes simulation advocates from standards bodies. Theater_ratio (0.55) captures the growing performative dimension: as simulation technology improves, the doctrine is maintained increasingly through narrative credentialism ('war stories') rather than demonstrated safety superiority. Accessibility_collapse (0.72) reflects that once the doctrine is accepted, simulation-based alternatives nearly vanish as legitimate competence pathways. Resistance (0.50) captures active pushback from safety scientists and untested operators, though it remains overridden by practice-based authority.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (crisis elites, veterans) experience this constraint as necessary quality control protecting the public from atrophied operators. The payer seats (untested operators, public dependents) experience it as an arbitrary status hierarchy that endangers them by devaluing proven simulation skills and restricting the talent pool. The engine will compute these seats as divergent types because the structural data declares opposed beneficiary/victim roles with asymmetric exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Crisis elites and veteran operators are declared beneficiaries: they collect authority and status from the doctrine, giving them low directionality (near the beneficiary end). Untested operators and public dependents are declared victims/payers: they bear the costs of devaluation and risk exposure, giving them high directionality (near the target end). Safety scientists are observers with analytical exit; simulation vendors are excluded but structurally constrained. The high theater ratio and rising suppression over the interval indicate that the constraint's persistence depends increasingly on active enforcement rather than self-evident merit.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination functionâensuring competent crisis leadershipâis genuine, which prevents classifying this as a pure snare. However, the asymmetric extraction is equally real: the doctrine assumes covert atrophy without evidence, creates a scarce credential, and transfers risk to the public. Classifying it as tangled_rope captures both faces. If the coordination function were absent (if real stakes conferred no genuine advantage), it would be a snare; if the extraction were absent (if the doctrine merely certified without devaluing alternatives), it would be a rope. The temporal measurements show extraction and theater rising over the interval, suggesting the coordination component is stable while the extractive component grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the lived-catastrophe-necessity reading the only defensible framing of the competence-maintenance kernel, or do the simulation-sufficiency and hybrid-decay readings describe the same empirical territory with equal structural coherence?',
    'Comparative analysis of predictive validity: which reading''s implied competence metrics better predict operator performance in subsequent real events?',
    'If sibling readings are structurally coherent, this constraint''s extraction is reading-dependent rather than kernel-intrinsic, and the kernel should be decomposed into multiple constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether this reading is the only structurally defensible framing of the kernel').

omega_variable(
    covert_atrophy_reality,
    'Does operator competence actually atrophy covertly in the absence of real-stakes activation, or is this an untested assumption that sustains the veteran status hierarchy?',
    'Longitudinal performance studies comparing operators with and without real-catastrophe exposure, controlling for simulation hours and career stage.',
    'If atrophy is minimal or non-existent, the constraint''s extraction is substantially higher than its coordination value; if atrophy is severe, the coordination function is genuine and the extraction is the cost of necessary scarcity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_atrophy_reality, empirical, 'Whether covert competence atrophy is real or constructed').

omega_variable(
    real_stakes_irreducibility,
    'Is there an irreducible component of crisis competence that can only be exercised under real existential stakes, or can high-fidelity simulation replicate all relevant stress-response pathways?',
    'Psychophysiological and performance studies under matched stress conditions, including hormonal response, decision latency, and team coordination metrics.',
    'If real stakes are irreducible, the coordination function of this constraint is genuine and the classification may shift toward rope; if simulation can replicate, the constraint is substantially extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_stakes_irreducibility, empirical, 'Whether real catastrophe provides irreducible competence exercise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise_as_competence_maintenance kernel. The three readings (lived_catastrophe_necessity, simulation_sufficiency, hybrid_decay) are structurally distinct: they differ on whether simulation exercises the kernel at all, and therefore differ in beneficiary/victim structure and empirical premises. They should be modeled as separate constraints linked in a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
