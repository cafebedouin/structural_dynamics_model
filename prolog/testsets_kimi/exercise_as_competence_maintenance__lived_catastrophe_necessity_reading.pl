% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Lived Catastrophe Necessity Doctrine
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the lived_catastrophe_necessity_reading of
 *   the exercise_as_competence_maintenance kernel. The kernel asks what kind
 *   of event is required to exercise crisis-response competence. This reading
 *   holds that only actual catastrophe exercises the kernel; simulation is
 *   rehearsal but not the thing itself, and competence atrophies without
 *   real-stakes activation. In safety engineering and emergency management,
 *   this doctrine has been formalized into credentialing standards that
 *   discount simulation-validated skill in favor of experiential lineage.
 *   Sibling readings are simulation_sufficiency_reading and
 *   hybrid_decay_reading.
 *
 * KEY AGENTS:
 *   - organizational_safety_leadership: agenda_setter (institutional/constrained) â interprets and enforces the real-stakes requirement
 *   - catastrophe_veteran_operators: beneficiary (powerful/mobile) â collect gatekeeping rents and status premium
 *   - untested_operators: payer (moderate/identity_locked) â bear career devaluation and internalized inadequacy
 *   - exposed_public: payer (powerless/trapped) â bear risk of unvalidated response systems
 *   - simulation_training_advocates: excluded (moderate/constrained) â structurally absent from standards bodies
 *   - safety_systems_researchers: observer (institutional/analytical) â study transfer and decay with limited policy uptake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.65).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.6).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity Doctrine").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '7e5d3371-1cde-4219-941f-f92d1250e1e7').
narrative_ontology:cs_kernel_codification('7e5d3371-1cde-4219-941f-f92d1250e1e7', formalized).
narrative_ontology:cs_authority_grounding('7e5d3371-1cde-4219-941f-f92d1250e1e7', lineage).
narrative_ontology:cs_interpretation_layer_present('7e5d3371-1cde-4219-941f-f92d1250e1e7').
narrative_ontology:cs_reading_relation('7e5d3371-1cde-4219-941f-f92d1250e1e7', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('7e5d3371-1cde-4219-941f-f92d1250e1e7', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('7e5d3371-1cde-4219-941f-f92d1250e1e7', foundational, only_actual_catastrophe_exercises_kernel).
narrative_ontology:cs_axiom_status(only_actual_catastrophe_exercises_kernel, holdable).
narrative_ontology:cs_axiom_grounding('7e5d3371-1cde-4219-941f-f92d1250e1e7', only_actual_catastrophe_exercises_kernel, empirically_contingent).
narrative_ontology:cs_axiom('7e5d3371-1cde-4219-941f-f92d1250e1e7', foundational, competence_atrophies_without_real_stakes).
narrative_ontology:cs_axiom_status(competence_atrophies_without_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('7e5d3371-1cde-4219-941f-f92d1250e1e7', competence_atrophies_without_real_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('7e5d3371-1cde-4219-941f-f92d1250e1e7', real_stakes_mastery_framework).
narrative_ontology:cs_drift_state('7e5d3371-1cde-4219-941f-f92d1250e1e7', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e5d3371-1cde-4219-941f-f92d1250e1e7', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_veteran_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets credentialing standards, promotion criteria, and crisis-team composition rules for emergency response organizations. Interprets what counts as 'actual catastrophe' experience in hiring guidelines and maintains the professional doctrine that real-stakes activation is mandatory for senior command roles.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizational_safety_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Possess experiential capital from having operated during actual catastrophes. Their authority is treated as the non-replicable gold standard for crisis competence. They benefit from exclusive gatekeeping over legitimate competence, commanding premium consulting roles, leadership appointments, and deference in standards-setting bodies.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_veteran_operators, beneficiary,
    powerful, biographical, mobile, national).

% Extensively trained in simulation and rehearsal but denied full professional standing because they have not faced an actual catastrophe. Career advancement is capped, their judgment is systematically discounted in favor of veterans, and they cannot manufacture a qualifying event. Many internalize the doctrine and believe they are not truly competent until 'tested by fire'.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_operators, payer,
    moderate, biographical, identity_locked, national).

% Relies on regional emergency response systems staffed partly by operators whose competence has never been validated under real stakes. Bears the risk of system failure when a catastrophe occurs and reveals gaps that simulation did not surface. No individual exit from public safety services.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_public, payer,
    powerless, immediate, trapped, regional).

% Develop and promote high-fidelity simulation and continuous assessment programs. They argue that modern simulation can exercise procedural and judgment competencies sufficiently for credentialing. They are structurally excluded from standards-setting bodies dominated by veteran operators and leadership who discount their evidence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_advocates, excluded,
    moderate, biographical, constrained, national).

% Study learning transfer from simulation to real performance and the epidemiology of competence decay. They publish evidence on whether simulation fidelity can substitute for real-stakes activation, but their findings are selectively absorbed by the credentialing regime.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_systems_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_veteran_operators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the identification and allocation of crisis-competent personnel by creating a legible, difficult-to-fake signal (real-stakes experience) that correlates with validated performance under extreme uncertainty and scarcity.
% TRANSFER_FUNCTION: Moves authority, gatekeeping power, and premium career access from untested operators to catastrophe veterans; moves risk-bearing to populations served by response systems that rely partly on unvalidated operators.
% ABSENT_VOICES: Simulation-training researchers who argue for credentialing reform; untested operators who have internalized their own devaluation and do not contest the standard; affected communities who cannot know whether their local responders have been tested under real stakes.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, credentialing systems would reorganize around simulation-validated and continuously assessed competence metrics; veteran status would deflate to one input among many; untested operators would gain professional standing; and liability frameworks would shift from 'who has been there' to 'who can demonstrate skill under validated test conditions.'
% FOUNDING_PROBLEM: Crisis response organizations could not distinguish genuinely competent operators from those who merely appeared competent in classroom training, leading to dangerous assignments of untested personnel to high-stakes events where failure carried severe consequences.
% FOUNDING_PROBLEM_CORROBORATION: Organizational safety leadership and veteran operators attest the problem remains live, citing the irreplaceability of stress inoculation. Safety systems researchers and simulation advocates attest the problem has been substantially addressed by validated simulation and continuous assessment technology, and the arrangement now functions to protect veteran privilege; independent human-factors research from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the doctrine decouples credentialing from validated simulation and continuous assessment, creating a monopoly on legitimate competence for a small cohort. Suppression (0.60) reflects the active exclusion of simulation-sufficiency claims from credentialing discourse and the institutional discounting of non-veteran judgment. Theater_ratio (0.40) captures the growing performative dimension: as simulation fidelity improves, the insistence that only 'the real thing' counts increasingly functions as retrospective storytelling and lineage protection rather than evidence-based practice. Accessibility_collapse (0.60) indicates that alternative pathways to competence recognition are partially but not fully closed â they exist but are not credentialed. Resistance (0.45) reflects the ongoing but subordinate advocacy from simulation researchers and excluded trainers.
 *
 * PERSPECTIVAL GAP:
 *   The veteran operator seat computes as beneficiary (low d) because the constraint subsidizes their experiential capital and gatekeeping authority. The untested operator seat computes as target (high d) because the constraint caps their career, locks their identity in a devalued status, and charges them with waiting for a random catastrophic event. The exposed public computes as target (high d) because they bear the stochastic risk of system failure. The agenda-setting leadership seat sits near symmetric: they derive institutional legitimacy from administering a rigorous standard but do not personally capture the status premium, and they are constrained by the professional culture they enforce.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to catastrophe_veteran_operators, who demonstrably collect the constraint's extraction in the form of irreplaceable authority, premium consulting access, and gatekeeping power. Victim declarations map to exposed_public (who bear the risk of response failure) and untested_operators (who bear the career and psychological costs of perpetual 'untested' status). The engine's structural derivation therefore assigns low d to veterans and high d to untested operators and the public. Simulation_training_advocates are excluded rather than declared victims; their structural absence is captured in absent_voices and the suppression metric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling by preserving the genuine coordination function: real-stakes experience does solve an information problem about crisis performance that is costly to fake, and the constraint is not a pure snare because that signal has real value. At the same time, it prevents mislabeling as a rope because the extraction is asymmetric: the doctrine systematically devalues all non-veteran competence, identity-locks untested operators, and protects a status monopoly. Were the mandate resolved â if empirical evidence showed simulation fully substitutable â the constraint would likely degrade toward a piton maintained by veteran cohort self-interest; the temporal measurements show extraction and theater rising over the interval, indicating drift rather than resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_mechanism_ambiguity,
    'Is competence decay without real-stakes activation an empirically measured phenomenon, or a doctrinal assumption that protects veteran lineage?',
    'Longitudinal performance studies comparing veteran-only and simulation-validated operators in subsequent real events, controlling for recency and frequency of practice.',
    'If decay is not independently supported, the coordination function is weaker than claimed and extraction dominates; if supported, the constraint retains genuine coordination value that justifies part of its asymmetric structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_mechanism_ambiguity, empirical, 'Empirical basis of the competence decay claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the discounting of simulation-based competence structural (credentialing barriers, hiring standards) or internalized (untested operators believe in their own inadequacy)?',
    'Post-credential-change surveys and career-trajectory analysis tracking whether untested operators'' self-efficacy and advancement patterns shift when simulation pathways open.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â the target carries the suppression after any barrier removal, and identity_lock is deeper than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_decomposability,
    'Does the competence kernel decompose into separable procedural and judgment sub-skills, or is it irreducibly unitary?',
    'Cognitive task analysis and learning-transfer studies isolating procedural fluency from decision-making under uncertainty.',
    'If decomposable, the hybrid_decay reading gains support and this reading''s foreclosure of partial simulation exercise weakens; if unitary, this reading''s strong claim is structurally reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_decomposability, conceptual, 'Whether the competence kernel is unitary or decomposable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_lcn_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(exercise_lcn_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(exercise_lcn_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(exercise_lcn_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(exercise_lcn_tr_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(exercise_lcn_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(exercise_lcn_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(exercise_lcn_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(exercise_lcn_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(exercise_lcn_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(exercise_lcn_be_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(exercise_lcn_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(exercise_lcn_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(exercise_lcn_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(exercise_lcn_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(exercise_lcn_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(exercise_lcn_su_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(exercise_lcn_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise_as_competence_maintenance kernel. The kernel decomposes into three structurally distinct constraints based on whether simulation can exercise the competence kernel (simulation_sufficiency), whether only real catastrophe can (this reading), or whether the kernel has separable components with different exercise requirements (hybrid_decay). Each reading carries a distinct epsilon, stakeholder structure, and victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
