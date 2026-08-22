% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation as Universal Instructional Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'structured_literacy_remediation'
 *   reading of the contested kernel 'reading_acquisition_legitimacy.' It
 *   asserts that legitimate reading instruction must be designed for the most
 *   vulnerable learners first — meaning explicit, cumulative, diagnostic,
 *   multisensory structured literacy principles become universal Tier 1
 *   prevention rather than Tier 2/3 remediation. The reading draws its
 *   authority from cognitive science convergence (phonology, orthography,
 *   morphology, syntax, semantics as teachable structures) and implementation
 *   science (Tier 1 quality determines system load). It positions itself
 *   against balanced literacy and whole language framings that treat explicit
 *   structure as optional or remedial. The claim/metric independence is
 *   observed: the reading claims rope (coordination of evidence-aligned
 *   prevention), while metrics show low but non-zero extraction (professional
 *   learning burden on teachers, opportunity cost for typically developing
 *   readers) and rising resistance from excluded framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.18).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.22).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.18).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation as Universal Instructional Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '8581229e-5d6b-4df5-aa3d-b1eb1beba2d1').
narrative_ontology:cs_kernel_codification('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', distributed).
narrative_ontology:cs_authority_grounding('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', practice).
narrative_ontology:cs_interpretation_layer_present('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1').
narrative_ontology:cs_reading_relation('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', foundational, vulnerable_learner_as_design_criterion).
narrative_ontology:cs_axiom_status(vulnerable_learner_as_design_criterion, holdable).
narrative_ontology:cs_axiom_grounding('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', vulnerable_learner_as_design_criterion, empirically_contingent).
narrative_ontology:cs_axiom('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', foundational, diagnostic_assessment_drives_instruction).
narrative_ontology:cs_axiom_status(diagnostic_assessment_drives_instruction, holdable).
narrative_ontology:cs_axiom_grounding('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', diagnostic_assessment_drives_instruction, empirically_contingent).
narrative_ontology:cs_axiom('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', foundational, explicit_cumulative_multisensory_for_all).
narrative_ontology:cs_axiom_status(explicit_cumulative_multisensory_for_all, holdable).
narrative_ontology:cs_axiom_grounding('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', explicit_cumulative_multisensory_for_all, empirically_contingent).
narrative_ontology:cs_reference_frame('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', prevention_oriented_instructional_design).
narrative_ontology:cs_drift_state('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', post_science_of_reading_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8581229e-5d6b-4df5-aa3d-b1eb1beba2d1', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, reading_specialists_interventionists).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parents_families).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_prevention_model).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, explicit_instruction_cognitive_load_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, diagnostic_assessment_continuous).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Students with dyslexia, language impairments, low SES backgrounds, or other risk factors who historically receive intervention only after failure. Under this reading, they receive Tier 1 instruction designed for their needs from day one, preventing the wait-to-fail cycle.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners, beneficiary,
    powerless, biographical, constrained, national).

% Students who do not meet benchmarks but may not have formal diagnoses. They receive explicit, cumulative, multisensory instruction as core curriculum rather than pull-out remediation, eliminating stigma and instructional fragmentation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, struggling_readers, beneficiary,
    powerless, biographical, constrained, national).

% Students with neurobiological reading disabilities whose needs define the instructional floor. The constraint asserts that what works for them works for everyone; their required instructional intensity becomes the universal standard.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_students, beneficiary,
    powerless, biographical, identity_locked, national).

% Typically developing readers who receive more structured, explicit instruction than they might minimally need. They bear the opportunity cost of slower pacing and higher structure, but gain a stronger foundation and avoid later gaps.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students, payer).

% Must master complex structured literacy knowledge (phonology, orthography, morphology, syntax, semantics) and deliver diagnostic instruction. They bear high professional development costs and instructional labor but gain a coherent framework that reduces decision fatigue and improves outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers, beneficiary).

% Their expertise becomes central rather than peripheral. They shift from remediating casualties to coaching Tier 1 fidelity, gaining professional status and preventative impact, but face expanded caseload expectations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, reading_specialists_interventionists, beneficiary,
    organized, biographical, mobile, local).

% Develop and market structured literacy programs aligned to this reading. They shape implementation through materials, training, and consulting. They profit from adoption but must meet rigorous evidence and design standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).

% Authorize adoption, allocate PD budgets, set pacing guides, and evaluate fidelity. They bear political risk if outcomes don't improve quickly, but gain a defensible, evidence-aligned instructional framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, district_administrators, agenda_setter,
    institutional, generational, constrained, regional).

% Pass dyslexia screening laws, mandate structured literacy coursework for licensure, tie funding to approved curricula. They set the regulatory infrastructure that makes this reading the compliance standard.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Provide the empirical warrant: convergence evidence from cognitive psychology, neuroscience, linguistics, and instructional experiments supporting explicit, systematic, cumulative, diagnostic instruction for all learners.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_scientists_reading_researchers, observer,
    analytical, civilizational, analytical, universal).

% Proponents of meaning-first, immersion-based, or balanced approaches who argue this reading over-structures instruction, reduces engagement, narrows curriculum, and misdiagnoses instructional casualties as student deficits. They are structurally excluded from the legitimacy framework this reading establishes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_balanced_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Experience the constraint through their children's instruction and progress. Those with struggling readers gain transparency and preventative action; those with typically developing children may perceive excessive drill. Their exit is limited to school choice or supplementation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_families, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of instructional fragmentation: without a shared evidence-aligned framework, each teacher/building/district reinvents reading instruction, vulnerable learners fall through inconsistent Tier 1, and intervention systems collapse under preventable caseloads. This constraint coordinates curriculum, preparation, assessment, and policy around a single prevention-oriented instructional logic.
% TRANSFER_FUNCTION: Transfers instructional intensity and structural explicitness from the intervention tier (historically reserved for identified struggling readers) to the universal tier. Moves professional learning burden onto teachers and districts (time, cost, cognitive load). Moves curricular authority toward publishers and researchers aligned with structured literacy. Moves regulatory authority to state policy mandating screening, preparation, and materials alignment.
% ABSENT_VOICES: Students themselves — especially older students who experienced the previous system — are absent from the design table. Families in communities where structured literacy conflicts with cultural literacy practices (oral storytelling traditions, multilingual home literacies) are often not consulted. Teachers who have developed effective hybrid practices outside the structured literacy canon are excluded by fidelity requirements.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, instruction would revert to the fragmented, curriculum-dependent, wait-to-fail status quo: vulnerable learners would again receive intervention only after years of failure; teacher preparation would remain misaligned with evidence; screening and early identification would lose policy mandate; publishers would revert to balanced literacy materials. The prevention infrastructure would dissolve.
% FOUNDING_PROBLEM: The wait-to-fail model: children with dyslexia and other reading difficulties were identified only after years of academic struggle, by which point remediation was exponentially harder and social-emotional damage was entrenched. The founding problem is the moral and practical failure of a system that treats preventable reading failure as inevitable individual deficit rather than instructional failure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: National Reading Panel (2000) convergence evidence; decades of NICHD-funded longitudinal studies (Lyon, Fletcher, Shaywitz, Vellutino); cognitive load theory (Sweller, Kalyuga) on explicit instruction efficiency; implementation science showing Tier 1 quality determines Tier 2/3 caseloads; state dyslexia legislation passed in 40+ states with bipartisan support from parent advocacy groups (Decoding Dyslexia) and researchers independent of publisher interests.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).
:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.18) reflects real but bounded costs: teacher PD burden, pacing trade-offs for advanced readers, publisher/district compliance costs. These are coordination overheads, not rents. Suppression (0.22) is low: the constraint operates through professional consensus, policy incentives, and evidence alignment — not coercion. Balanced literacy advocates can still teach their way in many contexts; the constraint shapes legitimacy and funding, not classroom doors directly. Theater (0.12) is low: the instructional practices have measurable student outcome effects. Accessibility collapse (0.35) is moderate: alternative instructional models persist but lose legitimacy and policy access. Resistance (0.68) is substantial: balanced literacy/whole language institutions, university teacher prep programs, and major publishers (Heinemann, Lucy Calkins Units of Study) actively contest the evidence base and policy mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable learner seat (powerless, identity_locked for dyslexic students), this constraint is a mountain — the difference between literacy and illiteracy, no exit, no alternative. From the teacher seat (organized, constrained exit), it is a rope with real coordination value but high implementation cost. From the publisher/administrator seat (institutional, arbitrage/constrained exit), it is a coordination infrastructure they can shape and profit from. From the excluded advocate seat (organized, constrained exit), it is a snare — an epistemic closure that delegitimizes their life's work. The engine computes this divergence from power/exit/role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable learners, struggling readers, dyslexic students are primary beneficiaries (d near 0.0) — the constraint subsidizes them by making their needs the design floor. General education students are secondary beneficiaries with minor payer costs (d ~ 0.3). Teachers are net payers of professional learning labor but beneficiaries of coherent framework (d ~ 0.45, near symmetric). Specialists gain status and preventative role (d ~ 0.2). Publishers and administrators are agenda_setters with arbitrage/constrained exit — they shape the constraint but are bound by its evidence requirements (d ~ 0.3-0.4). Excluded advocates are structurally locked out of the legitimacy framework (d ~ 0.8 for their framing's viability). Cognitive scientists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as extraction by making the beneficiary structure explicit: the 'extraction' from teachers and typically developing readers is the price of a prevention system that eliminates the far larger extraction of wait-to-fail remediation (grade retention, special education referral, lifetime earnings loss). The founding problem (wait-to-fail) remains live — NAEP 2024 shows 37% of 4th graders below basic. The constraint's mandate has not outlived its function; it is still building the infrastructure it was designed to create.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'How does this reading''s structural classification change if evaluated from a sibling reading''s framework (e.g., balanced_literacy_integration''s legitimacy criteria)?',
    'Cross-reading classification audit: compute each sibling''s claimed_type and metrics from their own beneficiary/victim declarations and evidence base, then compare divergence patterns.',
    'If sibling readings compute as different types (e.g., balanced_literacy computes as tangled_rope due to publisher rent extraction), the kernel itself is a constraint family with internal type variation — not a single constraint with measurement noise. This validates the decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or framing variants of one constraint.').

omega_variable(
    teacher_burden_as_extraction_vs_coordination_cost,
    'Is the high professional learning burden on teachers (the main extraction signal) a necessary coordination cost of evidence-aligned instruction, or does it extract teacher labor for system-level legitimacy without proportional student gain?',
    'Longitudinal implementation studies measuring: teacher knowledge growth, instructional fidelity, student outcome trajectories, and teacher retention — disaggregated by PD quality, coaching intensity, and curricular support.',
    'If burden yields proportionate student gains (especially for vulnerable learners), it is coordination cost (rope). If burden is high but gains plateau or concentrate in compliant subgroups, it edges toward extraction (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_burden_as_extraction_vs_coordination_cost, empirical, 'Whether teacher professional learning burden is functional coordination overhead or extractive overhead.').

omega_variable(
    cultural_linguistic_fit_for_multilingual_learners,
    'Does the structured literacy framework as universally mandated adequately serve emergent bilingual learners, or does its English-orthography-centric structure extract compliance from multilingual communities without addressing cross-linguistic transfer?',
    'Comparative effectiveness research: structured literacy implementations with and without cross-linguistic transfer scaffolds, home language literacy integration, and culturally sustaining pedagogies — measuring outcomes for emergent bilingual students.',
    'If the constraint as currently mandated fails multilingual learners, the beneficiary declaration for ''vulnerable_learners'' is incomplete — the constraint extracts from a subgroup it claims to center. This would shift classification toward tangled_rope or snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_linguistic_fit_for_multilingual_learners, empirical, 'Whether the universal mandate''s structure fits linguistically diverse learners or imposes monolingual norms.').

omega_variable(
    publisher_capture_of_structured_literacy_label,
    'Are curriculum publishers capturing the ''structured literacy'' label to market materials that meet surface criteria (scope/sequence, decodables) but lack the diagnostic, responsive, cumulative depth the reading requires?',
    'Independent curriculum audits (e.g., Reading League, EdReports, state adoption reviews) evaluating: diagnostic assessment integration, cumulative review design, responsiveness to student data, and alignment to the full structured literacy definition (not just phonics scope/sequence).',
    'If publishers successfully dilute the label, the constraint''s coordination function degrades — the label coordinates adoption but the materials don''t coordinate instruction. Theater_ratio rises. Classification drifts toward piton or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_of_structured_literacy_label, empirical, 'Whether the structured literacy label is being hollowed out by commercial capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 1997, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1997, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 1997, 0.05).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(read_tr_t2026, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(read_be_t1997, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 1997, 0.08).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2005, 0.12).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2020, 0.17).
narrative_ontology:measurement(read_be_t2026, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1997, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 1997, 0.15).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2000, 0.17).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2020, 0.21).
narrative_ontology:measurement(read_su_t2026, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.02).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the reading_acquisition_legitimacy kernel. Each reading instantiates a different constraint with distinct beneficiary structures, extraction profiles, and legitimacy claims. They are linked as a constraint family via affects_constraints. The structured_literacy_remediation reading is distinguished by: (1) universal preventative design for the most vulnerable learner, (2) diagnostic assessment as instructional engine, (3) cumulative multisensory explicit instruction across all language domains (phonology through semantics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, powerless, 0.05).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, organized, 0.25).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
