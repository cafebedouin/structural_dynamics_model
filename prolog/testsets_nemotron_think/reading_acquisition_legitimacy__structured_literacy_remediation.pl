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
 *   human_readable: Structured Literacy Mandate: Instruction Designed for Most Vulnerable Learners First
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story captures the 'structured literacy remediation'
 *   reading of the contested kernel 'reading_acquisition_legitimacy.' The
 *   reading asserts that legitimate reading instruction must be designed for
 *   the most vulnerable learners first — meaning explicit, cumulative,
 *   diagnostic, multisensory structured literacy for all students
 *   preventatively. It is instantiated as state mandates (dyslexia screening
 *   laws, curriculum adoption lists, teacher licensure exams) that require
 *   this approach. The constraint operates as a tangled rope: it solves a
 *   genuine coordination problem (ensuring evidence-aligned instruction
 *   reaches every child) while extracting asymmetric costs from teachers,
 *   districts, and the balanced literacy establishment who bear retraining,
 *   curriculum replacement, and professional identity losses. The engine will
 *   compute per-seat classifications from the structural data below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Mandate: Instruction Designed for Most Vulnerable Learners First").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '2be99416-5cc4-4524-b7b2-d48e876b916f').
narrative_ontology:cs_kernel_codification('2be99416-5cc4-4524-b7b2-d48e876b916f', distributed).
narrative_ontology:cs_authority_grounding('2be99416-5cc4-4524-b7b2-d48e876b916f', distributed).
narrative_ontology:cs_reading_relation('2be99416-5cc4-4524-b7b2-d48e876b916f', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2be99416-5cc4-4524-b7b2-d48e876b916f', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('2be99416-5cc4-4524-b7b2-d48e876b916f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('2be99416-5cc4-4524-b7b2-d48e876b916f', foundational, instructional_design_for_most_vulnerable_first).
narrative_ontology:cs_axiom_status(instructional_design_for_most_vulnerable_first, holdable).
narrative_ontology:cs_axiom_grounding('2be99416-5cc4-4524-b7b2-d48e876b916f', instructional_design_for_most_vulnerable_first, deontological).
narrative_ontology:cs_axiom('2be99416-5cc4-4524-b7b2-d48e876b916f', foundational, explicit_cumulative_diagnostic_instruction_necessary).
narrative_ontology:cs_axiom_status(explicit_cumulative_diagnostic_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('2be99416-5cc4-4524-b7b2-d48e876b916f', explicit_cumulative_diagnostic_instruction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('2be99416-5cc4-4524-b7b2-d48e876b916f', secondary, preventative_intervention_grade_for_all).
narrative_ontology:cs_axiom_status(preventative_intervention_grade_for_all, holdable).
narrative_ontology:cs_axiom_grounding('2be99416-5cc4-4524-b7b2-d48e876b916f', preventative_intervention_grade_for_all, empirically_contingent).
narrative_ontology:cs_reference_frame('2be99416-5cc4-4524-b7b2-d48e876b916f', structured_literacy_preventative_universal).
narrative_ontology:cs_drift_state('2be99416-5cc4-4524-b7b2-d48e876b916f', post_science_of_reading_legislation_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2be99416-5cc4-4524-b7b2-d48e876b916f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, all_students_preventative).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_researchers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, districts_balanced_literacy_investment).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_programs).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_advocates).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, explicit_instruction_necessary_for_decoding).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, cumulative_scope_and_sequence_required).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, diagnostic_assessment_drives_instruction).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, multisensory_methods_benefit_all_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Students with dyslexia, language-based learning disabilities, or who otherwise fail to acquire reading through implicit methods. They bear the cost of instructional failure (academic cascade, behavioral referrals, life-outcome deficits). Under structured literacy mandates, they receive the instruction they need from day one rather than waiting for failure-triggered intervention. No exit from the school system; their literacy trajectory is determined by the method adopted.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners, beneficiary,
    powerless, biographical, trapped, national).

% General-education students who receive intervention-grade explicit instruction preventatively rather than waiting to struggle. They benefit from the highest structure regardless of need. No choice in instructional method; their reading development is shaped by the system's choice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, all_students_preventative, beneficiary,
    powerless, biographical, trapped, national).

% Parents who advocated for dyslexia laws, structured literacy mandates, and curriculum transparency. They organized politically to change state policy. They benefit when mandates pass but remain constrained by implementation fidelity gaps. Exit options: private tutoring (costly), relocation (disruptive), or advocacy continuation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_vulnerable_learners, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_vulnerable_learners, agenda_setter).

% Cognitive scientists and reading researchers whose work on explicit systematic instruction gains policy validation and funding. Their frameworks become the mandated standard. Mobile exit: can move between institutions, but professional reputation tied to the paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of mandated retraining, scripted curriculum adoption, loss of instructional autonomy, and accountability for fidelity to programs they did not choose. Many entered teaching under balanced literacy; the shift requires unlearning and relearning. Exit: leave profession (high personal cost), move to non-mandate states (limited), or comply with varying fidelity.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers, payer,
    moderate, biographical, constrained, national).

% Districts with sunk costs in balanced literacy materials (Units of Study, Fountas & Pinnell), professional development, and teacher leadership structures. Mandates force costly curriculum replacement and PD overhaul. Constrained exit: cannot easily abandon investments; must phase transition while maintaining instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, districts_balanced_literacy_investment, payer,
    institutional, generational, constrained, regional).

% Commercial publishers (Heinemann, etc.) whose core products are displaced by state adoption lists favoring structured literacy. They lose market share but can pivot (some now offer 'phonics supplements' or structured literacy lines). Mobile exit: corporate restructuring, product line diversification.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_publishers, payer,
    powerful, generational, mobile, national).

% University education departments must overhaul reading methods courses to meet new licensure requirements (e.g., Foundations of Reading tests). Faculty with balanced literacy expertise face devaluation. Constrained: accreditation and state approval tied to new standards; cannot easily exit teacher prep market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_programs, payer,
    institutional, generational, constrained, national).

% Literacy leaders, authors, and organizations (TCRWP, NCTE affiliates) who argue for balanced integration of phonics and meaning-making. Excluded from mandate-writing rooms; their testimony dismissed as 'ideological.' Constrained: professional identity and networks built on the paradigm; exit means professional marginalization.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_advocates, excluded,
    organized, biographical, constrained, national).

% Enact dyslexia screening laws, curriculum mandates, and teacher licensure requirements aligned to structured literacy. Set the agenda under pressure from parent advocacy groups. Arbitrage exit: can claim credit for 'science of reading' alignment regardless of implementation outcomes; electoral risk low.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_legislators_education_committees, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Researchers and think-tank analysts evaluating implementation fidelity, outcome data, and unintended consequences (e.g., narrowed curriculum, reduced reading volume, teacher burnout). Neither collect nor pay; they document the constraint's operation across seats.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, literacy_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of ensuring every child receives evidence-aligned reading instruction by making the most vulnerable learner the design target: explicit, systematic, cumulative, diagnostic, multisensory instruction prevents reading failure rather than remediating it.
% TRANSFER_FUNCTION: Moves instructional authority and resource allocation from teacher-chosen balanced literacy materials to state-mandated structured literacy programs; moves professional development funding from balanced literacy PD to structured literacy training; moves curriculum purchasing from trade publishers to approved structured literacy vendors; moves teacher autonomy to scripted fidelity.
% ABSENT_VOICES: Students who thrived under balanced literacy (avid readers developed through workshop models) and their families — they are not represented in the 'most vulnerable first' framing. Also absent: emergent bilingual students whose language development needs may not align with monolingual structured literacy scope-and-sequences; their advocates are rarely in mandate-writing rooms.
% DISAPPEARANCE_RATIONALE: If structured literacy mandates vanished overnight, districts would revert to balanced literacy or local control within months; teacher prep programs would rewrite syllabi; state dyslexia laws would become unenforced; the commercial market would shift back to Units of Study. The instructional landscape would reorganize around the prior equilibrium.
% FOUNDING_PROBLEM: The persistent reading crisis: 30-40% of students nationally reading below basic (NAEP), with disproportionate impact on low-income, Black, Hispanic, and disabled students. The founding problem is that implicit, meaning-first instruction fails the most vulnerable, and the wait-to-fail model delays intervention until gaps are entrenched.
% FOUNDING_PROBLEM_CORROBORATION: NAEP longitudinal data (independent of structured literacy advocates) confirms the reading crisis persists. Cognitive science consensus (National Reading Panel 2000, subsequent replication) corroborates explicit systematic phonics necessity. However, whether 'most vulnerable first' design prevents failure for ALL learners at scale remains contested — implementation studies show mixed fidelity and outcomes.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects substantial imposed costs: teacher retraining, curriculum replacement, loss of professional judgment, commercial displacement. Suppression (0.55) reflects active enforcement: state mandates, adoption lists, licensure tests that exclude alternative approaches. Theater ratio (0.30) is moderate: genuine instructional change occurs, but performative compliance (box-checking PD, fidelity without understanding) grows as mandates scale. Accessibility collapse (0.60): balanced literacy becomes professionally risky and administratively difficult to maintain. Resistance (0.70): organized pushback from balanced literacy advocates, teacher unions concerned about autonomy, and scholars questioning generalizability. All metrics measured at interval end (2024).
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable learner seat, the constraint is a rope (pure coordination benefit). From the classroom teacher seat, it is a snare (extraction of autonomy, imposed script). From the state legislator seat, it is a scaffold (transitional fix for a crisis). The engine computes this divergence; the authored claim (tangled_rope) reflects the system-level structure where both coordination and extraction are real and simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable learners and all students are structural beneficiaries (d near 0.0) — the constraint subsidizes their literacy trajectory. Parents of vulnerable learners are beneficiary-agenda_setters (d low). Structured literacy researchers are beneficiaries (d low). Classroom teachers are primary payers (d near 1.0) — bear costs, constrained exit. Districts with sunk investments are institutional payers (d high). Balanced literacy publishers are powerful payers with mobile exit (d moderate-high). Teacher prep programs are institutional payers (d high). Balanced literacy advocates are excluded (d not computed — they are not governed by the constraint but by its absence). State legislators are agenda_setters with arbitrage exit (d near 0.0). Policy analysts are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's founding problem (reading crisis for vulnerable learners) is live — NAEP scores remain low. But the mandate itself may be outrunning its evidence base: 'most vulnerable first' design does not guarantee 'all students benefit equally' at scale. Mandatrophy risk: the constraint persists and expands (more states, stricter mandates) even as implementation fidelity gaps and outcome plateaus emerge. The theater ratio rise (0.10 to 0.30) suggests performative compliance is growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_structured_literacy_remediation,
    'This constraint is one reading of the contested kernel ''reading_acquisition_legitimacy'' (reading_id: structured_literacy_remediation). How does the kernel''s contestation structure the classification of this reading?',
    'Map each sibling reading to its own constraint story with independent ε, beneficiaries, victims, and claimed_type. The engine will reveal whether the contest is about measurement (same constraint, different ε) or identity (different constraints, different ε).',
    'If sibling readings produce divergent classifications (e.g., balanced_literacy_integration computes as rope while this computes as tangled_rope), the kernel is a false unity — the label ''reading instruction'' covers structurally distinct constraints. If all compute similarly, the contest is about parameter values within one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structured_literacy_remediation, conceptual, 'Commitment-system framing under-determination: whether the kernel represents one constraint or a family.').

omega_variable(
    preventative_universal_vs_targeted_extraction,
    'Does delivering intervention-grade instruction to ALL students (universal preventative) extract more from teachers and the system than targeted intervention for identified vulnerable learners would?',
    'Compare cost-structure and outcome data from universal structured literacy implementations (e.g., Mississippi, Colorado) vs. targeted intervention models (RTI/MTSS with structured literacy only at Tier 2/3).',
    'If universal preventative extracts substantially more (teacher burnout, narrowed curriculum, resource diversion) without proportional outcome gains for non-vulnerable students, the constraint''s extraction is inflated by its universal scope — a candidate for scaffold (targeted) vs. tangled_rope (universal) decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preventative_universal_vs_targeted_extraction, empirical, 'Whether the universal preventative design is structurally necessary or extractively excessive.').

omega_variable(
    balanced_literacy_displacement_as_extraction_or_coordination,
    'Is the displacement of balanced literacy materials and pedagogy a necessary coordination cost (replacing ineffective practice) or an extractive transfer (rent capture by new vendors, professional devaluation of existing workforce)?',
    'Track curriculum adoption markets, PD contracts, and vendor revenue shifts pre/post mandates. Assess whether new structured literacy programs demonstrate superior outcomes at comparable cost, or whether market consolidation creates new rents.',
    'If displacement creates new vendor lock-in and recurring costs without outcome improvement, extraction is higher than authored. If displacement is one-time transition to sustainably better practice, extraction is transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balanced_literacy_displacement_as_extraction_or_coordination, empirical, 'Whether curriculum market restructuring is coordination cost or extractive transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_structured_literacy_tr_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(reading_structured_literacy_tr_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(reading_structured_literacy_tr_t2016, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(reading_structured_literacy_tr_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(reading_structured_literacy_tr_t2022, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2022, 0.28).
narrative_ontology:measurement(reading_structured_literacy_tr_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(reading_structured_literacy_be_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(reading_structured_literacy_be_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(reading_structured_literacy_be_t2016, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(reading_structured_literacy_be_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(reading_structured_literacy_be_t2022, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(reading_structured_literacy_be_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(reading_structured_literacy_su_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(reading_structured_literacy_su_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(reading_structured_literacy_su_t2016, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(reading_structured_literacy_su_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(reading_structured_literacy_su_t2022, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2022, 0.53).
narrative_ontology:measurement(reading_structured_literacy_su_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'reading_acquisition_legitimacy' into four readings with distinct ε values and beneficiary/victim structures. structured_literacy_remediation has the highest extractiveness (universal preventative mandate) and claims tangled_rope. phonics_decoding_primacy likely computes as rope (narrower coordination, less displacement). whole_language_meaning_primacy likely computes as snare (suppresses explicit instruction, extracts from vulnerable learners). balanced_literacy_integration likely computes as tangled_rope or scaffold (transitional compromise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, institutional, 0.15).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
