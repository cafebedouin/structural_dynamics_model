% ============================================================================
% CONSTRAINT STORY: prerequisite_debt_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prerequisite_debt_reading, []).

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
 *   constraint_id: prerequisite_debt_reading
 *   human_readable: Learning Difficulty as Prerequisite Debt
 *   domain: educational_psychology/learning_theory/epistemology
 *
 * SUMMARY:
 *   The prerequisite debt reading frames learning difficulty as accumulated
 *   knowledge gaps: a learner who fails to understand new material is missing
 *   prior concepts that the new material depends on, not lacking the capacity
 *   to understand. This reading emerged from mastery learning research and
 *   cognitive load theory. It competes with ability-ceiling readings (some
 *   learners cannot grasp certain abstractions) and access-barrier readings
 *   (the difficulty is in how material is presented, not what the learner
 *   knows). The constraint is claimed as rope—a genuine coordination
 *   mechanism for organizing remediation—while the metrics track moderate
 *   extraction as the model benefits diagnostic and remediation industries
 *   and shifts institutional resources toward gap-filling.
 *
 * KEY AGENTS:
 *   - remediation_industry: Primary beneficiary (organized/mobile) — revenue scales with gap diagnosis
 *   - diagnostic_assessment_providers: Beneficiary (organized/mobile) — debt model creates demand for granular diagnostics
 *   - educational_institutions: Agenda setter + beneficiary (institutional/constrained) — adopt the framework, benefit from attributing failure to remediable gaps
 *   - struggling_learners: Beneficiary (powerless/constrained) — gain a non-stigmatizing path forward, contingent on access to remediation
 *   - advanced_learners: Payer (moderate/mobile) — bear opportunity cost of slowed pacing
 *   - cognitive_scientists: Observer (analytical/analytical) — assess empirical validity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prerequisite_debt_reading, 0.48).
domain_priors:suppression_score(prerequisite_debt_reading, 0.42).
domain_priors:theater_ratio(prerequisite_debt_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prerequisite_debt_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(prerequisite_debt_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(prerequisite_debt_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prerequisite_debt_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(prerequisite_debt_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prerequisite_debt_reading, rope).
narrative_ontology:human_readable(prerequisite_debt_reading, "Learning Difficulty as Prerequisite Debt").
narrative_ontology:topic_domain(prerequisite_debt_reading, "educational_psychology/learning_theory/epistemology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(prerequisite_debt_reading, '640a6895-1240-4719-a2e4-cd5e0facd072').
narrative_ontology:cs_kernel_codification('640a6895-1240-4719-a2e4-cd5e0facd072', distributed).
narrative_ontology:cs_authority_grounding('640a6895-1240-4719-a2e4-cd5e0facd072', expertise).
narrative_ontology:cs_interpretation_layer_present('640a6895-1240-4719-a2e4-cd5e0facd072').
narrative_ontology:cs_reading_relation('640a6895-1240-4719-a2e4-cd5e0facd072', learning_difficulty_substrate__ability_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('640a6895-1240-4719-a2e4-cd5e0facd072', learning_difficulty_substrate__access_barrier_reading, coexists_with).
narrative_ontology:cs_axiom('640a6895-1240-4719-a2e4-cd5e0facd072', foundational, comprehension_failure_is_structural_debt).
narrative_ontology:cs_axiom_status(comprehension_failure_is_structural_debt, holdable).
narrative_ontology:cs_axiom_grounding('640a6895-1240-4719-a2e4-cd5e0facd072', comprehension_failure_is_structural_debt, empirically_contingent).
narrative_ontology:cs_axiom('640a6895-1240-4719-a2e4-cd5e0facd072', foundational, all_learners_capable_given_prerequisites).
narrative_ontology:cs_axiom_status(all_learners_capable_given_prerequisites, holdable).
narrative_ontology:cs_axiom_grounding('640a6895-1240-4719-a2e4-cd5e0facd072', all_learners_capable_given_prerequisites, empirically_contingent).
narrative_ontology:cs_reference_frame('640a6895-1240-4719-a2e4-cd5e0facd072', mastery_learning_foundation).
narrative_ontology:cs_drift_state('640a6895-1240-4719-a2e4-cd5e0facd072', contemporary_educational_psychology, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('640a6895-1240-4719-a2e4-cd5e0facd072', '').
narrative_ontology:cs_kernel_id(prerequisite_debt_reading, learning_difficulty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prerequisite_debt_reading, remediation_industry).
narrative_ontology:constraint_beneficiary(prerequisite_debt_reading, diagnostic_assessment_providers).
narrative_ontology:constraint_beneficiary(prerequisite_debt_reading, educational_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(prerequisite_debt_reading, struggling_learners).
narrative_ontology:constraint_victim(prerequisite_debt_reading, advanced_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provides tutoring, supplemental instruction, and gap-filling curricula. Benefits from the framing that learning difficulty is addressable through systematic prerequisite remediation rather than being an immutable trait. Revenue scales with the number of learners diagnosed with knowledge gaps.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, remediation_industry, beneficiary,
    organized, biographical, mobile, national).

% Develop and administer assessments that identify specific prerequisite gaps. The debt model creates demand for granular diagnostic tools that map knowledge structures and pinpoint missing foundations.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, diagnostic_assessment_providers, beneficiary,
    organized, biographical, mobile, national).

% Adopt the prerequisite debt framework to structure curriculum sequencing and intervention programs. Benefit from a model that attributes learning failure to remediable gaps rather than institutional failure, while also genuinely coordinating around a shared theory of knowledge building.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(prerequisite_debt_reading, educational_institutions, beneficiary).

% Experience learning difficulty as a solvable problem rather than a fixed limitation. The debt model offers a path forward through targeted remediation, though accessing that remediation depends on institutional resources and family capacity to pay for supplemental services.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, struggling_learners, beneficiary,
    powerless, biographical, constrained, local).

% Bear the opportunity cost of instructional time allocated to prerequisite review and remediation. Curriculum pacing slows to accommodate gap-filling; enrichment and acceleration are deferred to ensure foundational coverage.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, advanced_learners, payer,
    moderate, biographical, mobile, local).

% Study the empirical validity of the prerequisite debt model through learning experiments and longitudinal tracking. Assess whether targeted remediation produces the predicted gains and whether the model accounts for observed variance in learning outcomes.
narrative_ontology:constraint_stakeholder(prerequisite_debt_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for diagnosing learning difficulty and organizing intervention: if comprehension failure is structural debt, then the remedy is systematic prerequisite instruction, which can be planned, sequenced, and delivered.
% TRANSFER_FUNCTION: Moves instructional time and resources toward remediation and diagnostic assessment; moves learner self-concept from fixed-ability to addressable-gap framing; moves institutional accountability from 'some students can't learn' to 'we must fill the gaps.'
% ABSENT_VOICES: Proponents of ability-ceiling models (who would argue some learners lack the capacity for certain abstractions) and access-barrier theorists (who would argue the difficulty is in pedagogical method or cultural mismatch, not missing prerequisites) are structurally excluded from the design of remediation programs built on the debt model.
% DISAPPEARANCE_RATIONALE: If the prerequisite debt model vanished, educational institutions would revert to mixed explanations for learning difficulty—some attributing it to ability, some to pedagogy, some to motivation—and remediation programs would lose their organizing principle. Diagnostic assessment markets would contract. Learners experiencing difficulty would face a less systematic path to intervention.
% FOUNDING_PROBLEM: Early educational psychology lacked a mechanistic account of why some learners failed to grasp new material while others succeeded, leading to inconsistent and often stigmatizing explanations that attributed difficulty to innate deficiency.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and educational researchers outside the remediation industry attest that the problem of explaining learning variance remains live and that the prerequisite debt model is one empirically supported mechanism among several. Meta-analyses of mastery learning and spaced repetition interventions provide independent corroboration.
narrative_ontology:disappearance_verdict(prerequisite_debt_reading, world_rearranges).
narrative_ontology:founding_problem_status(prerequisite_debt_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(prerequisite_debt_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-27',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(prerequisite_debt_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prerequisite_debt_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(prerequisite_debt_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(prerequisite_debt_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) because the model genuinely coordinates around a theory of knowledge building, but also benefits industries that profit from gap diagnosis and remediation. Suppression is lower (0.42) because alternative explanations (ability, pedagogy) remain live in research and practice, though the debt model dominates institutional remediation design. Theater ratio rises modestly (0.31 at interval end) as some 'prerequisite review' becomes ritualized coverage rather than targeted gap-filling. Accessibility collapse is low (0.38) because competing models remain accessible; resistance is moderate (0.52) from educators who see the model as over-mechanistic or from ability-ceiling proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the remediation industry's seat, the constraint is a coordination mechanism that enables systematic intervention and creates a sustainable market. From the struggling learner's seat, it is a lifeline if remediation is accessible, but a gate if it is not. From the advanced learner's seat, it is a tax on their progression. From the cognitive scientist's seat, it is one empirically supported model among several, with known boundary conditions.
 *
 * DIRECTIONALITY LOGIC:
 *   Remediation and diagnostic industries are structural beneficiaries—they collect revenue from the model's adoption. Educational institutions are mixed: they set the agenda and benefit from a non-stigmatizing explanation for learning failure, but also bear the cost of implementing remediation programs. Struggling learners are beneficiaries if they gain access to effective remediation, but the benefit is contingent on institutional resources. Advanced learners are payers—they lose instructional time to prerequisite review. Cognitive scientists observe and assess.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy: the founding problem (explaining learning variance) remains live, and the prerequisite debt model is one mechanism among several that addresses it. The model's persistence is not purely inertial—it continues to organize effective interventions. However, the rising theater ratio suggests some drift toward ritualized prerequisite coverage that no longer targets actual gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_vs_ability_boundary,
    'What proportion of observed learning difficulty is attributable to remediable prerequisite gaps versus inherent cognitive capacity limits?',
    'Longitudinal studies tracking learners who receive intensive, well-designed prerequisite remediation: if difficulty persists after gaps are systematically filled, the ability-ceiling model gains support; if difficulty resolves, the debt model is vindicated.',
    'A high proportion attributable to gaps supports the debt reading and justifies remediation investment; a low proportion supports the ability-ceiling reading and would shift resources toward differentiated instruction or alternative pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_vs_ability_boundary, empirical, 'Empirical boundary between remediable gaps and capacity limits.').

omega_variable(
    debt_vs_access_boundary,
    'Is the observed difficulty due to missing prerequisites (debt) or to pedagogical methods that fail to connect with the learner''s existing knowledge structures (access)?',
    'Controlled experiments comparing targeted prerequisite remediation against alternative pedagogical approaches (e.g., culturally responsive teaching, multiple representations, discovery learning) for the same learners: which intervention produces greater gains?',
    'If remediation outperforms pedagogical redesign, the debt model is supported; if pedagogical redesign outperforms, the access-barrier model is supported. The boundary determines whether resources flow to gap-filling or to instructional innovation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debt_vs_access_boundary, empirical, 'Empirical boundary between prerequisite gaps and pedagogical mismatch.').

omega_variable(
    committer_frame_under_determination,
    'Does the prerequisite debt reading foreclose the ability-ceiling reading, or do they coexist as competing hypotheses?',
    'Conceptual analysis of the readings'' core premises: if the debt reading asserts ''all learners can grasp any concept given sufficient prerequisite instruction'' and the ability reading asserts ''some learners cannot grasp certain abstractions regardless of instruction,'' the premises are contradictory and one forecloses the other. If the debt reading asserts ''most difficulty is remediable'' and the ability reading asserts ''some difficulty is not,'' they coexist.',
    'If foreclosing, the readings cannot both be held within a single educational framework—institutions must choose. If coexisting, both can inform practice simultaneously (e.g., remediation for most, alternative pathways for some).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_under_determination, conceptual, 'Whether the debt and ability readings are logically exclusive or compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prerequisite_debt_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prer_tr_t0, prerequisite_debt_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prer_tr_t10, prerequisite_debt_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(prer_tr_t20, prerequisite_debt_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(prer_tr_t30, prerequisite_debt_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(prer_tr_t40, prerequisite_debt_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(prer_be_t0, prerequisite_debt_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(prer_be_t10, prerequisite_debt_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(prer_be_t20, prerequisite_debt_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(prer_be_t30, prerequisite_debt_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(prer_be_t40, prerequisite_debt_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(prer_su_t0, prerequisite_debt_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(prer_su_t10, prerequisite_debt_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(prer_su_t20, prerequisite_debt_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(prer_su_t30, prerequisite_debt_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(prer_su_t40, prerequisite_debt_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prerequisite_debt_reading, information_standard).
narrative_ontology:affects_constraint(prerequisite_debt_reading, ability_ceiling_reading).
narrative_ontology:affects_constraint(prerequisite_debt_reading, access_barrier_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the learning_difficulty_substrate kernel. The kernel decomposes into three structurally distinct claims with different beneficiary sets and ε values: prerequisite_debt_reading (moderate ε, remediation industry benefits), ability_ceiling_reading (higher ε, tracking/sorting systems benefit), access_barrier_reading (lower ε, pedagogical innovation benefits). Each reading is a separate constraint story linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
