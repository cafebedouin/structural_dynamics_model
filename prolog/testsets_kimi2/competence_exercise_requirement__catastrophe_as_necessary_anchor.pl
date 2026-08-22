% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as Necessary Anchor for Competence Maintenance
 *   domain: safety engineering/organizational learning
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'competence_exercise_requirement' in safety engineering and
 *   high-reliability organizations. The reading asserts that only real
 *   catastrophic events or near-misses provide the irreducible exercise
 *   required to maintain operational competence, and that simulation
 *   necessarily fails to replicate the somatic and social stress that
 *   constitutes genuine capability. In practice, this doctrine functions as
 *   an organizational belief system that privileges experiential authority,
 *   justifies underinvestment in simulation infrastructure, and treats
 *   populations near critical facilities as the necessary substrate for
 *   organizational learning. The constraint is claimed as a Mountain (a
 *   natural law of human competence maintenance) but is authored with
 *   beneficiaries and moderate extractiveness to test for false-summit
 *   detection, per the FSM authoring protocol.
 *
 * KEY AGENTS:
 *   - experiential_elite: Primary agenda-setter (powerful/mobile) â enforces the doctrine that real catastrophic experience is the sole legitimate foundation of competence
 *   - operating_institutions: Primary beneficiary (institutional/mobile) â captures cost savings from reduced simulation investment while preserving credibility
 *   - novice_operators: Primary target (moderate/constrained) â bears devalued training, delayed professional standing, and elevated first-event risk
 *   - exposed_public: Secondary target (powerless/trapped) â bears the risk of events treated as necessary training rather than preventable failures
 *   - simulation_researchers: Excluded voice (organized/constrained) â empirically challenges the doctrine but lacks standing in competence-definition bodies
 *   - safety_science_observers: Analytical observer (analytical/analytical) â maps the empirical debate without institutional authority to adjudicate it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.52).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, mountain).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe as Necessary Anchor for Competence Maintenance").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety engineering/organizational learning").

domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e0661f69-c4f0-4038-935c-4335a2940c0c').
narrative_ontology:cs_kernel_codification('e0661f69-c4f0-4038-935c-4335a2940c0c', distributed).
narrative_ontology:cs_authority_grounding('e0661f69-c4f0-4038-935c-4335a2940c0c', practice).
narrative_ontology:cs_interpretation_layer_present('e0661f69-c4f0-4038-935c-4335a2940c0c').
narrative_ontology:cs_reading_relation('e0661f69-c4f0-4038-935c-4335a2940c0c', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('e0661f69-c4f0-4038-935c-4335a2940c0c', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('e0661f69-c4f0-4038-935c-4335a2940c0c', foundational, muscle_memory_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(muscle_memory_irreducible_to_simulation, holdable).
narrative_ontology:cs_axiom_grounding('e0661f69-c4f0-4038-935c-4335a2940c0c', muscle_memory_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_axiom('e0661f69-c4f0-4038-935c-4335a2940c0c', foundational, catastrophic_stress_as_unique_revelator).
narrative_ontology:cs_axiom_status(catastrophic_stress_as_unique_revelator, holdable).
narrative_ontology:cs_axiom_grounding('e0661f69-c4f0-4038-935c-4335a2940c0c', catastrophic_stress_as_unique_revelator, empirically_contingent).
narrative_ontology:cs_reference_frame('e0661f69-c4f0-4038-935c-4335a2940c0c', experiential_competence_primacy).
narrative_ontology:cs_drift_state('e0661f69-c4f0-4038-935c-4335a2940c0c', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0661f69-c4f0-4038-935c-4335a2940c0c', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, experiential_elite).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, operating_institutions).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, exposed_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators and incident veterans whose authority and professional standing derive from having survived or managed real catastrophic events. They set training standards, evaluate junior competence, and enforce the norm that simulation-acquired skills are not equivalent to 'muscle memory' forged in real jeopardy. Their status and market value increase as the doctrine narrows the path to legitimate expertise.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, experiential_elite, agenda_setter,
    powerful, biographical, mobile, national).

% Organizations running safety-critical systems that treat the necessity doctrine as a budgetary and operational rationale. By privileging on-the-job catastrophic exposure over expensive simulator maintenance and deliberate rehearsal programs, they reduce training capital expenditures while preserving a publicly credible claim that competence is maintained through 'real-world' seasoning.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, operating_institutions, beneficiary,
    institutional, generational, mobile, national).

% Entry-level and mid-career personnel who receive simulation-based training but are told it does not confer the irreducible competence that only catastrophes provide. They remain professionally subordinate until they experience a qualifying event, operate with devalued credentials during the interim, and carry elevated personal risk when the first real event reveals gaps that the doctrine predicted but did not prevent.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_operators, payer,
    moderate, biographical, constrained, national).

% Populations living or working near safety-critical facilities where organizational belief in catastrophe-as-necessary-exercise refracts into toleration of near-misses and incidents as 'training opportunities' rather than preventable system failures. They bear the risk and harm of events that the doctrine treats as structurally necessary for organizational competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, exposed_public, payer,
    powerless, biographical, trapped, local).

% Scientists and engineers who develop and validate high-fidelity simulation and deliberate-practice protocols, with empirical evidence of skill transfer to real operations. They are structurally excluded from professional competence-definition bodies dominated by experiential practitioners, and their findings are routinely discounted as theoretically sound but operationally naive.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_researchers, excluded,
    organized, biographical, constrained, global).

% Interdisciplinary researchers who study competence decay, simulator fidelity, and learning transfer in high-reliability domains. They document the empirical relationship between exercise modality and performance but hold no authority to set professional standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_science_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes trust and authority in high-stakes environments where routine operations provide no feedback about failure modes, by grounding legitimate competence in direct experience of catastrophic stress.
% TRANSFER_FUNCTION: Moves professional credibility, advancement eligibility, and decision-making authority from operators without catastrophic experience to those who have survived real events; moves risk exposure and training costs toward novice personnel and proximate populations.
% ABSENT_VOICES: Simulation researchers and proactive safety engineers who argue that competence can be maintained through high-fidelity rehearsal are structurally excluded from competence-definition authority; their empirical findings are filtered out by practitioner bodies that treat operational experience as the sole legitimate epistemic credential.
% DISAPPEARANCE_RATIONALE: If the necessity doctrine vanished, training budgets would shift toward simulator development and scenario-based rehearsal, professional advancement would decouple from catastrophe exposure, incident investigations would stop treating harm as a necessary tuition payment, and safety investment would reallocate from reactive experience-gathering to proactive prevention engineering.
% FOUNDING_PROBLEM: How to maintain reliable human performance in safety-critical systems where normal operations are too benign to reveal failure modes, and where trust in operators must be grounded in demonstrated capability under extreme stress.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigators and high-reliability-organization researchers attest that competence atrophies without practice and that real events reveal gaps simulation misses. However, cognitive scientists and simulation researchers outside the experiential authority structure attest that high-fidelity simulation with deliberate practice can maintain equivalent procedural competence, challenging whether catastrophes are structurally necessary rather than merely historically convenient.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, ExtMetricName, E),
    domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the doctrine genuinely coordinates professional trust in high-stakes environments, but it asymmetrically extracts by devaluing simulation-based skill acquisition and justifying catastrophe exposure as necessary. Suppression (0.52) is moderate-high because the doctrine actively suppresses simulation alternatives through professional derogation ('it is just a sim'). Theater ratio (0.35) reflects growing performative credentialism around 'war stories' and experiential gatekeeping as simulation fidelity improves. Accessibility collapse (0.8) is high because the 'you had to be there' epistemology is extremely resistant to counter-argument once adopted. Resistance (0.3) reflects sustained but institutionally weak opposition from simulation science. The temporal grid is shared across all three metrics: extraction and theater rise steadily while suppression requirement ratchets upward as improving simulation technology forces the doctrine to work harder to maintain its legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the experiential elite's seat, the constraint is a discovered natural law of competence maintenance; their own survival and operational success are the evidence. From the novice operator's seat, it is a credentialing bottleneck that devalues earned simulation skill and forces reliance on rare, dangerous events for professional legitimacy. From the exposed public's seat, it is an externalization of risk that treats their safety as the raw material for organizational learning. The engine computes this divergence from the structural data without reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Experiential elite and operating institutions derive low directionality (beneficiaries): the constraint subsidizes their status and cost structures. Novice operators and exposed public derive high directionality (targets): the constraint extracts professional standing from the former and safety from the latter. Simulation researchers are excluded rather than targeted; their exclusion is the enforcement object that maintains the beneficiary seats' advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining competence in the absence of routine failure feedback â is live and serious. The reading's specific solution (catastrophe necessity) is contested. The mandatrophy has not been resolved: the doctrine persists not solely because it solves the founding problem, but because it simultaneously concentrates authority in the experiential elite and reduces institutional training costs. This hybridity prevents pure classification as either coordination or extraction; the engine must evaluate whether the coordination function can be decoupled from the asymmetric extraction, or whether they are structurally fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the necessity of real catastrophic events for competence maintenance a genuine irreducible limit of human cognition and team coordination, or a constructed professional doctrine that preserves experiential authority and institutional cost structures?',
    'Longitudinal comparison of safety outcomes between organizations that treat simulation as primary rehearsal and those that treat catastrophe exposure as necessary, controlling for domain complexity and resource levels.',
    'If genuine cognitive limit, the Mountain classification holds and extraction is an unfortunate byproduct; if constructed doctrine, the constraint is a false summit and should reclassify toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the constraint is a genuine natural law of competence or a constructed doctrine serving identifiable beneficiaries.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation alternatives structural (budget denial, institutional exclusion from standards bodies) or internalized (practitioners genuinely cannot trust sim-acquired skills due to embodied epistemic priors)?',
    'Post-intervention trajectory analysis: when organizations are mandated to adopt high-fidelity simulation, does practitioner resistance persist after structural barriers are removed (indicating internalized suppression) or collapse (indicating structural suppression)?',
    'If internalized, effective suppression is higher than structural measure suggests and the constraint is stickier; if purely structural, removal of funding and authority barriers may rapidly shift practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of simulation alternatives.').

omega_variable(
    extraction_concentration,
    'Does the doctrine primarily extract from novice operators through career gating, from the general public through risk exposure, or from organizational learning budgets through prevention underinvestment?',
    'Comparative case analysis tracing resource flows and risk distributions across organizations with varying commitment to the necessity doctrine.',
    'Concentration on novice operators suggests identity-coordination extraction; concentration on public suggests risk-externalization extraction; budget extraction suggests institutional cost-shifting. Each implies different remediation paths.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_concentration, conceptual, 'Which seat bears the primary extraction from the catastrophe-necessity doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 5, 0.22).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.26).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 15, 0.3).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.33).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 25, 0.35).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% The natural-language concept 'competence exercise requirement' decomposes into three structurally distinct constraints corresponding to three kernel readings. Each reading instantiates a different epsilon, stakeholder structure, and classification. This reading (catastrophe_as_necessary_anchor) claims the highest extractiveness and the strongest natural-law framing; the simulation reading claims near-zero extraction and Mountain naturality without beneficiaries; the hybrid reading sits between. They form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
