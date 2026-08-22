% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics as Foundational Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics reading of the
 *   reading-acquisition-mechanism kernel: the claim that reading acquisition
 *   requires explicit, systematic instruction in grapheme-phoneme
 *   correspondence as a foundational skill, because a substantial subset of
 *   learners cannot reliably induce the alphabetic code from meaning-focused
 *   exposure alone. Since roughly 2013, state legislatures acting on 'science
 *   of reading' evidence have converted this pedagogical claim into a
 *   mandated instructional regime with curriculum-adoption lists,
 *   teacher-licensure requirements, and compliance audits — moving the
 *   constraint from professional consensus toward enforced policy. The
 *   sibling readings (whole_language_reading, balanced_literacy_reading) are
 *   separate constraint stories with their own ε and stakeholder structures;
 *   this file does not average across them or describe the contest
 *   internally, per the ε-invariance rule.
 *
 * KEY AGENTS:
 *   - struggling_readers: Primary beneficiary (powerless/trapped) — depend on systematic instruction they cannot obtain by other means
 *   - students_with_dyslexia: Primary beneficiary (powerless/trapped) — highest-yield population for explicit phonics
 *   - teachers_favoring_literature_immersion: Primary payer (moderate/constrained) — bears retraining and discretion-loss costs
 *   - phonics_curriculum_publishers: Secondary beneficiary (organized/arbitrage) — captures mandated curriculum revenue
 *   - state_education_agencies: Agenda-setter (institutional/analytical) — writes and enforces the mandate
 *   - reading_scientists_cognitive_researchers: Analytical observer (analytical/global) — supplies the evidence base independent of curriculum sales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.28).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.42).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics as Foundational Reading Instruction Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '5a1e5ab3-0e21-40aa-a516-fec69317171f').
narrative_ontology:cs_kernel_codification('5a1e5ab3-0e21-40aa-a516-fec69317171f', distributed).
narrative_ontology:cs_authority_grounding('5a1e5ab3-0e21-40aa-a516-fec69317171f', expertise).
narrative_ontology:cs_interpretation_layer_present('5a1e5ab3-0e21-40aa-a516-fec69317171f').
narrative_ontology:cs_reading_relation('5a1e5ab3-0e21-40aa-a516-fec69317171f', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('5a1e5ab3-0e21-40aa-a516-fec69317171f', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('5a1e5ab3-0e21-40aa-a516-fec69317171f', foundational, decoding_requires_explicit_instruction_for_most_learners).
narrative_ontology:cs_axiom_status(decoding_requires_explicit_instruction_for_most_learners, holdable).
narrative_ontology:cs_axiom_grounding('5a1e5ab3-0e21-40aa-a516-fec69317171f', decoding_requires_explicit_instruction_for_most_learners, empirically_contingent).
narrative_ontology:cs_axiom('5a1e5ab3-0e21-40aa-a516-fec69317171f', secondary, implicit_code_induction_is_unreliable_absent_explicit_teaching).
narrative_ontology:cs_axiom_status(implicit_code_induction_is_unreliable_absent_explicit_teaching, holdable).
narrative_ontology:cs_axiom_grounding('5a1e5ab3-0e21-40aa-a516-fec69317171f', implicit_code_induction_is_unreliable_absent_explicit_teaching, empirically_contingent).
narrative_ontology:cs_reference_frame('5a1e5ab3-0e21-40aa-a516-fec69317171f', pre_1990s_mixed_methods_baseline).
narrative_ontology:cs_drift_state('5a1e5ab3-0e21-40aa-a516-fec69317171f', post_national_reading_panel_legislative_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5a1e5ab3-0e21-40aa-a516-fec69317171f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, novice_classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_favoring_literature_immersion).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, schools_with_established_whole_language_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, novice_classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who do not spontaneously induce the alphabetic code from text exposure alone. Explicit grapheme-phoneme instruction gives them a systematic route into decoding that they cannot reliably construct on their own; without it they fall into a remediation trajectory that compounds across grades. They have no say in which instructional approach their classroom uses.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).

% Students whose phonological processing differences make implicit pattern-induction from whole texts especially unreliable. Systematic, explicit, cumulative phonics instruction is documented as the highest-yield intervention for this population; withholding it in favor of an immersion approach disproportionately harms them since they cannot compensate through incidental exposure the way many typically-developing readers can.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, local).

% Teachers early in their careers benefit from a scope-and-sequence curriculum that specifies what to teach and when, reducing the improvisational burden whole-language approaches place on pedagogical judgment. They also pay an upfront cost in required training and reduced classroom-level discretion to deviate from the sequence.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, novice_classroom_teachers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, novice_classroom_teachers, payer).

% Companies producing structured-literacy curricula, decodable texts, and phonics assessment tools capture revenue when districts mandate systematic phonics programs. They lobby state legislatures for 'science of reading' adoption mandates that route purchasing decisions toward their specific product lines.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Experienced teachers who built their practice around authentic-text immersion and readers'/writers' workshop models bear the cost of retraining, discarding accumulated classroom materials, and having their professional judgment about individual student needs overridden by a mandated sequence. Their exit options are limited to leaving the profession or the district, or complying under supervision.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_favoring_literature_immersion, payer,
    moderate, biographical, constrained, regional).

% Schools and districts that invested substantially in whole-language or balanced-literacy infrastructure (leveled libraries, workshop training, staff expertise) face sunk-cost losses and reputational exposure when state 'science of reading' laws mandate curriculum replacement, sometimes with short compliance windows and audit requirements.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, schools_with_established_whole_language_programs, payer,
    institutional, biographical, constrained, regional).

% State legislatures and departments of education that pass 'science of reading' mandates, approve curriculum lists, and require teacher-preparation programs to teach systematic phonics. They set the enforcement mechanism (curriculum audits, licensure requirements) and can adjust its scope and timeline.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Cognitive and educational psychology researchers who study how children acquire the alphabetic code. They produce the meta-analytic evidence base (e.g., National Reading Panel findings, subsequent replications) that this reading treats as decisive; they do not administer classrooms or collect curriculum revenue.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_scientists_cognitive_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom instruction, curriculum development, and teacher preparation around a single evidence-convergent sequence for teaching the alphabetic code, so that students who would not otherwise induce grapheme-phoneme correspondence from exposure alone are not left to chance.
% TRANSFER_FUNCTION: Moves instructional time, teacher preparation resources, and curriculum-purchasing budgets away from literature-immersion materials and independent teacher pedagogical discretion, toward structured-literacy programs, phonics assessments, and standardized scope-and-sequence products.
% ABSENT_VOICES: Practicing teachers with strong track records using whole-language or balanced approaches for typically-developing readers are rarely centered in policy hearings dominated by cognitive-science testimony; their classroom-level counter-evidence (successful readers under their prior approach) is treated as anecdotal rather than as data.
% DISAPPEARANCE_RATIONALE: If systematic phonics instruction requirements disappeared, curriculum purchasing would revert to district and teacher discretion, phonics-curriculum publishers would lose mandated markets, teacher-preparation programs would restore elective treatment of decoding instruction, and the population of students who do not self-induce the code would again depend on which classroom they were assigned to.
% FOUNDING_PROBLEM: A substantial fraction of children, when taught reading primarily through meaning-based immersion in authentic texts, do not reliably induce the alphabetic code and become persistent poor decoders, with the deficit compounding into comprehension and vocabulary gaps across school years.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive-science meta-analyses (National Reading Panel 2000 and subsequent replications), international dyslexia and reading-research organizations outside the curriculum-publishing industry, and longitudinal studies of remediation costs in districts that shifted approaches all corroborate that unsystematic exposure leaves an identifiable subset of children without functional decoding skills. Curriculum publishers who benefit financially are not the corroborating source here.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because the dominant transfer is instructional-resource reallocation and reduced teacher discretion, not rent extraction from a captive population — though the rising trend documents the growing commercial capture by curriculum publishers as state mandates created a guaranteed purchasing market. Suppression is moderate (0.42) and rising over the interval, reflecting the shift from voluntary professional adoption to state-enforced curriculum audits and licensure gating that narrows what a teacher may lawfully teach, regardless of local classroom evidence. Theater ratio is kept low (0.15) because the coordination function (closing the decoding gap for at-risk readers) is substantively real and evidence-corroborated, not merely performed compliance. Accessibility collapse is moderate (0.4): the reading is contested by a genuine parallel professional tradition (balanced literacy, whole language), so alternatives have not vanished the way they would under a true mountain — they have been legislatively disfavored, not eliminated as live positions.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (struggling readers, dyslexic students), the constraint functions as long-overdue coordination correcting an instructional failure that previously left them behind by chance of classroom assignment. From the payer seats (immersion-trained teachers, established whole-language schools), the same enforcement machinery reads as an externally imposed narrowing of professional judgment backed by legislative compulsion. The engine should compute these as structurally different experiences of one constraint, not resolve them into a single averaged verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and students with dyslexia sit near the beneficiary end: the constraint subsidizes them directly by guaranteeing exposure to the one instructional method the cognitive-science evidence identifies as reliably effective for their profile, and they have no exit (they cannot choose their classroom's pedagogy). Teachers with sunk investment in literature-immersion approaches and schools with established whole-language infrastructure sit toward the target end: they bear retraining costs, discarded materials, and discretion loss through the same enforcement mechanism that delivers the coordination benefit — this is why the constraint reads differently at the payer seat than at the beneficiary seat. Curriculum publishers are beneficiaries via a captured, state-mandated purchasing channel rather than through the pedagogical coordination function itself, which is a distinct rent layered onto the underlying claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a subset of children fail to induce the alphabetic code from immersion alone) remains empirically live per independent replication literature, which argues against mandatrophy at the level of the phonics claim itself. However, the specific enforcement apparatus — state curriculum-adoption lists tied to particular commercial publishers — is a candidate for a distinct, layered mandatrophy: even if the pedagogical need is permanent, the current commercial-capture mechanism for meeting it is not, and could in principle be met through open-source or district-authored scope-and-sequence materials instead of mandated commercial curricula.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_reading_kernel_position,
    'This constraint is one reading (phonics_reading) of the contested reading_acquisition_mechanism kernel. The sibling readings are whole_language_reading (decoding emerges implicitly from authentic text exposure) and balanced_literacy_reading (both explicit phonics and authentic literature integrated). Which reading a jurisdiction adopts determines an entirely different beneficiary/victim structure and a different ε.',
    'Not resolvable within this story by design — each reading is authored as a separate constraint file per the ε-invariance principle. Cross-reading resolution would require comparing longitudinal reading-outcome data across jurisdictions that adopted each reading as policy.',
    'Adopting the phonics_reading as here authored means the constraint''s ε (0.28), beneficiary set (struggling readers, dyslexic students, novice teachers, curriculum publishers) and victim set (immersion-trained teachers, established whole-language schools) are all reading-specific and would differ substantially under the whole_language_reading or balanced_literacy_reading siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonics_reading_kernel_position, conceptual, 'This story instantiates one reading of a multiply-read kernel; sibling readings are separate constraints, not alternative measurements of this one.').

omega_variable(
    mandate_vs_evidence_scope,
    'Does the cognitive-science evidence base actually support the claim that ALL children require explicit systematic phonics instruction, or does it support the narrower claim that a subset of children require it while others acquire decoding adequately through less structured exposure?',
    'Subgroup analysis within existing meta-analytic datasets (e.g., disaggregating National Reading Panel and successor studies by baseline phonological awareness) to determine whether universal mandates are evidentially warranted or whether targeted intervention for at-risk readers would achieve equivalent outcomes with lower suppression.',
    'If the evidence supports only a targeted-subgroup claim, universal state mandates overshoot their own evidentiary warrant and the suppression imposed on teachers/schools serving primarily low-risk populations would be disproportionate to the coordination benefit — pushing this reading''s classification toward tangled_rope. If the evidence supports a universal claim, the current rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_evidence_scope, empirical, 'Whether the phonics mandate''s universal scope matches the universal or subgroup-specific evidence base.').

omega_variable(
    publisher_capture_separability,
    'Is the commercial curriculum-publisher rent (mandated purchasing of specific branded programs) separable from the underlying pedagogical claim (systematic phonics instruction should occur), or are they structurally fused by how state adoption lists are written?',
    'Compare states that mandate systematic phonics content standards without specifying approved commercial vendors against states with closed adoption lists; measure whether curriculum cost and vendor concentration differ.',
    'If separable, the extraction captured by phonics_curriculum_publishers is an add-on rent that could be eliminated via open-content mandates without abandoning the phonics claim itself, which would lower measured ε without touching the underlying coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_separability, empirical, 'Whether commercial curriculum capture is intrinsic to phonics mandates or a separable policy-design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__phonics_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 25, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint, whole_language_reading, and balanced_literacy_reading are three readings of the single reading_acquisition_mechanism kernel. Each reading names a structurally distinct mechanism claim about how children acquire decoding ability, with its own beneficiary/victim structure and ε. The phonics_reading (this file) authors ε=0.28 with beneficiaries concentrated among struggling and dyslexic readers and victims concentrated among teachers/schools invested in alternative approaches; the sibling files author their own independent ε values reflecting their own structural claims. They are linked here for network/contamination-propagation purposes, not averaged or reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
