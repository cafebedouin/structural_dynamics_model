% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Explicit Phonics-First Reading Instruction Requirement
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   Reading acquisition via explicit phonics instruction is one reading of a
 *   contested kernel about how children learn to decode written language. The
 *   phonics reading claims that systematic, direct instruction in
 *   grapheme-phoneme correspondence is foundational and necessary — that
 *   students acquire decoding by learning letter-sound relationships in a
 *   controlled sequence, building toward fluency. This reading is in direct
 *   structural opposition to the whole-language reading (decoding emerges
 *   implicitly from meaningful text engagement) and in measured tension with
 *   balanced-literacy (phonics and literature integrate as co-equal
 *   components). The phonics reading grounds its legitimacy in reading
 *   science evidence and special education efficacy; rival readings ground
 *   theirs in developmental psychology and authentic literacy engagement
 *   theory. KEY AGENTS (by structural relationship): struggling_readers and
 *   low_ses_students (primary beneficiaries — phonics closes the gap implicit
 *   learning leaves open); students_with_dyslexia (secondary beneficiaries —
 *   phonics is necessary, not optional, for this population);
 *   teachers_constrained_by_scope_sequence (primary payers — reduced autonomy
 *   and professional discretion); early_fluent_readers (secondary payers —
 *   instructional opportunity cost); curriculum_administrators
 *   (agenda-setters — enforce scope-and-sequence); whole_language_advocates
 *   and balanced_literacy_proponents (excluded — their pedagogical voice is
 *   marginalized by phonics-first mandates).
 *
 * KEY AGENTS:
 *   - struggling_readers: powerless, identity_locked exit — literacy outcomes and self-concept depend entirely on school instruction type; implicit approaches leave them behind; explicit phonics is the only intervention that moves them forward.
 *   - low_ses_students: powerless, constrained exit — lack home literacy resources; explicit instruction provides the only reliable pathway to decoding skill and reading fluency.
 *   - students_with_dyslexia: powerless, identity_locked exit — phonological processing deficits make implicit learning structurally ineffective; explicit phonics is necessary, not optional; reading outcomes without it approach zero.
 *   - teachers_constrained_by_scope_sequence: moderate power, constrained exit — required to follow mandated progressions; cannot pace according to student readiness or interest; must conduct frequent assessments tied to curriculum scope; experience loss of professional autonomy.
 *   - early_fluent_readers: moderate power, constrained exit — must participate in the same phonics sequence despite implicit competence; delayed access to complex texts; opportunity cost in engagement and fluency development.
 *   - curriculum_administrators: institutional power, constrained exit — legally mandated to ensure evidence-based instruction; enforce phonics-first frameworks through policy, materials approval, and compliance monitoring.
 *   - whole_language_advocates: moderate power, constrained exit — excluded from curriculum design; their pedagogical approach is suppressed by policy; constrained to either work within phonics-first mandates or exit the system entirely.
 *   - balanced_literacy_proponents: moderate power, constrained exit — constrained by phonics-first rigidity; cannot integrate authentic reading and decoding as co-equal components; professional judgment is overruled by policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.38).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.29).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Explicit Phonics-First Reading Instruction Requirement").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'bc98463a-b11b-4ecc-b9af-a9aa53bffdf3').
narrative_ontology:cs_kernel_codification('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', distributed).
narrative_ontology:cs_authority_grounding('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', expertise).
narrative_ontology:cs_interpretation_layer_present('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3').
narrative_ontology:cs_reading_relation('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', foundational, explicit_grapheme_phoneme_instruction_foundational).
narrative_ontology:cs_axiom_status(explicit_grapheme_phoneme_instruction_foundational, holdable).
narrative_ontology:cs_axiom_grounding('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', explicit_grapheme_phoneme_instruction_foundational, empirically_contingent).
narrative_ontology:cs_axiom('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', foundational, decoding_prerequisite_fluency).
narrative_ontology:cs_axiom_status(decoding_prerequisite_fluency, holdable).
narrative_ontology:cs_axiom_grounding('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', decoding_prerequisite_fluency, empirically_contingent).
narrative_ontology:cs_reference_frame('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', evidence_based_reading_instruction_framework).
narrative_ontology:cs_drift_state('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', contemporary_education_policy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc98463a-b11b-4ecc-b9af-a9aa53bffdf3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, low_ses_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_constrained_by_scope_sequence).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, early_fluent_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, early_fluent_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, alphabetic_principle_foundational).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, explicit_instruction_efficacy).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, decoding_prerequisite_fluency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Students who do not acquire reading through implicit exposure benefit substantially from explicit phonics instruction: they receive systematic breakdown of letter-sound relationships, cumulative skill building, and diagnostic feedback tied to documented gaps. Without it, they fall further behind each passing year; with it, remediation accelerates. Their literacy identity forms around either learned competence or internalized inadequacy depending on instructional approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, identity_locked, national).

% Students from low-SES households often have less pre-literacy exposure (fewer home books, less read-aloud time) and benefit from school-provided systematic instruction in phoneme awareness and grapheme-phoneme links. Whole-language and balanced approaches assume literacy background resources that are unequally distributed; explicit phonics levels the starting line. Their access to literacy depends on whether the school's instructional approach makes decoding mechanics explicit or leaves them implicit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, low_ses_students, beneficiary,
    powerless, biographical, constrained, national).

% Students with dyslexia have phonological processing deficits that make implicit skill acquisition ineffective. They require explicit, multisensory phonics instruction targeting phoneme manipulation and grapheme linkage. Without it, they face reading failure even with exposure to print; with evidence-based phonics, many achieve functional reading and remove a source of identity-based shame.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, identity_locked, national).

% Teachers are required to deliver scope-and-sequence phonics instruction, which reduces autonomy in pacing and literature selection. They must follow predetermined progressions of phoneme-grapheme pairs, limit student choice of texts during the decoding phase, and conduct frequent phonological assessments. They bear the cost of reduced professional discretion and the emotional labor of tighter monitoring; they experience the constraint as reducing responsiveness to individual student interests and readiness variability.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_constrained_by_scope_sequence, payer,
    moderate, biographical, constrained, national).

% Students who would acquire reading fluently through any exposure or through implicit learning in whole-language environments still encounter the same systematic phonics sequence, potentially delaying their access to complex, engaging texts and implicit fluency development. They are asked to slow-walk through decoding mechanics they have already internalized, creating a cost in instructional opportunity — though they benefit from the floor the same instruction creates for struggling peers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, early_fluent_readers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, early_fluent_readers, beneficiary).

% School districts, state departments of education, and textbook publishers have adopted phonics-first reading policies as the mandated instructional framework. They set scope-and-sequence standards, approve materials, monitor compliance, and allocate resources accordingly. They justify this via reading science evidence; their authority to enforce it rests on the public legitimacy of evidence-based instruction and the legal mandate for public education.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_administrators, agenda_setter,
    institutional, generational, constrained, national).

% The empirical body of research showing phonics instruction efficacy (National Reading Panel meta-analysis, structured literacy frameworks, dyslexia intervention literature). This is not an actor but a documented knowledge base that grounds the legitimacy of the constraint — administrators invoke it to justify enforcement, and rival reading theories contest its interpretation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_consensus, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(reading_acquisition_mechanism__phonics_reading, reading_science_consensus).

% Teachers, literacy scholars, and curriculum designers who believe reading acquisition is primarily meaning-driven and implicit, and who would prioritize authentic literature, student choice, and emergent literacy approaches, are excluded from the instructional design process or required to work within phonics-first constraints. Their voice in the reading-instruction conversation is marginal, and jurisdictions that mandate phonics-first actively suppress alternative pedagogical approaches through policy and curriculum oversight.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, excluded,
    moderate, biographical, constrained, national).

% Educators and researchers advocating integrated phonics-and-literature approaches are constrained by phonics-first mandates that separate decoding instruction from meaning-making and authentic reading. They experience the constraint as overly rigid, unable to follow the measured discovery pace that student engagement and comprehension interest might support, and forced to defend their professional judgment against policy enforcement.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, balanced_literacy_proponents, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, curriculum_administrators).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of reading acquisition variability: without explicit phonics instruction, students with weak implicit learning capacity or low prior exposure fall behind, creating stratification by home literacy resources. Systematic grapheme-phoneme instruction ensures consistent foundational skill regardless of student background, enabling all students to reach reading fluency benchmarks.
% TRANSFER_FUNCTION: Moves instructional time from student-choice literature exploration and teacher professional discretion to mandated phoneme-grapheme sequencing and decoding skill practice. It reallocates resources (teacher attention, curriculum minutes, assessment capacity) from advanced readers and from pedagogical flexibility to the floor-raising function for struggling readers.
% ABSENT_VOICES: Whole-language and balanced-literacy advocates are substantially excluded from curriculum design and policy; they hold professional disagreement about the primary mechanisms of reading acquisition but are not seated at the policy table in most US jurisdictions. Teachers who wish to prioritize literature, student agency, and emergent-literacy approaches are not excluded persons but are constrained in their practice by mandated scope-and-sequence. Students' own reading preferences and self-directed discovery of texts are decentered during the phonics-foundational phase.
% DISAPPEARANCE_RATIONALE: If explicit phonics-first mandates disappeared overnight, schools would return to mixed pedagogical approaches (some whole-language, some balanced, some phonics-light); teacher autonomy in pacing and literature selection would expand; struggling readers would face reinstated risk of falling further behind without explicit instruction; reading outcomes would stratify again by home literacy resources and implicit learning capacity. The instructional ecosystem would reorganize toward teacher choice and potentially less systematic support for struggling readers.
% FOUNDING_PROBLEM: Reading failure and illiteracy in school populations, particularly among students with dyslexia, low pre-literacy exposure, or weak implicit learning capacity. Early reading instruction lacked scientific grounding; pedagogical approaches varied widely; many students who could read never did despite years of schooling.
% FOUNDING_PROBLEM_CORROBORATION: Reading science researchers (National Reading Panel, International Dyslexia Association, structured literacy frameworks) and special education advocates attest the problem is live: students with dyslexia and low-SES students continue to show reading failure under non-phonics approaches, and remediation is dramatically more costly than prevention. Teachers and literacy researchers outside the phonics-first framework acknowledge the problem but dispute the solution's necessity and breadth.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) because the constraint does extract teacher autonomy and instructional flexibility, but the extraction is justified by a real coordination function (ensuring struggling readers reach fluency) and benefits a powerless population (struggling readers, low-SES, dyslexic students). Suppression is low-moderate (0.29) because the constraint is actively enforced (scope-and-sequence requirements, curriculum oversight, assessment mandates) but the enforcement is not coercive in the police sense — it is bureaucratic and pedagogical, operating through policy and professional standards rather than threat. Theater is very low (0.12) because the phonics instruction function is genuinely operational — decoding mechanics are taught, phonological awareness is assessed, fluency benchmarks are tracked — and performative elements (scope-and-sequence compliance theater) are minimal. The measurement series show extractiveness and suppression rising over the interval as policy becomes tighter and rival approaches are more actively suppressed, but from a low baseline. The constraint claims to be a rope (real coordination) and the metrics support that claim: beneficiaries exist (struggling readers reach fluency), the function is genuine (phoneme-grapheme instruction works), but extraction and suppression are non-zero and rising. This is a clean rope story with moderate extraction riding on a real coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute to different perceived types: from the struggling reader's seat, this is a mountain (reading acquisition requires phonics — a natural fact of how the brain decodes print) or at worst a benign rope (the phonics instruction is necessary coordination). From the constrained teacher's seat, it is a tangled rope or snare (mandatory scope-and-sequence is extraction masked as coordination). From the curriculum administrator's seat, it is a genuine rope (coordinating consistent reading outcomes). The divergence exists because the stakeholders hold different models of reading acquisition — the phonics reading vs. alternatives — and map the same constraint onto different underlying mechanisms. The engine computes each seat's perception from power, exit, and the beneficiary/victim data; the divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers, low-SES students, and students with dyslexia are beneficiaries (d near 0.0) — the constraint subsidizes their reading outcomes by providing explicit instruction they need. Teachers and early fluent readers are secondary payers (d near 0.5-0.7) — they bear costs (autonomy, opportunity) in exchange for some benefit (consistent floor, peer reading outcomes). Whole-language and balanced-literacy advocates are not stakeholders but excluded parties — they would experience the constraint as suppression of their pedagogical approach. The directionality is not uniform across seats: a powerless struggling reader has d near 0.0 (full beneficiary); a moderate-power teacher has d near 0.6 (target of autonomy extraction); a powerful curriculum administrator has d near 0.0 (collects the legitimacy of evidence-based instruction). The automated derivation from beneficiary/victim + power + exit should produce correct d values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading failure among struggling students) is live and corroborated by reading science research and special education evidence — students with dyslexia and low implicit learning capacity do fail to read under whole-language approaches, and explicit phonics intervention demonstrably moves outcomes. The mandatrophy question is whether the solution's scope is appropriate: does the benefit to struggling readers justify the cost to teacher autonomy and to early fluent readers? The phonics reading answers yes (the floor is worth the ceiling constraint); the balanced-literacy reading answers no (integration is more efficient). This is not a mandatrophy-resolved constraint (the founding problem and solution scope remain contested), but it is a constraint where the mandatrophy boundary is visible — the constraint is not a zombie (the founding problem is live), but the cost-benefit question is live and the scope-of-reach is the debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_acquisition_mechanism_implicit_vs_explicit,
    'Is reading decoding skill acquired primarily through implicit learning (exposure to print, meaning-driven engagement) or primarily through explicit instruction (direct teaching of letter-sound relationships)?',
    'Twin experimental cohorts: one receiving whole-language, one phonics-first, with randomization by reading-skill-level and cognitive profile; measure decoding fluency, comprehension, and long-term outcomes over 5+ years; control for home literacy resources and prior phonological awareness.',
    'If implicit learning routes dominate, phonics-first becomes extraction (constraining teacher discretion and literature engagement without commensurate benefit to early fluent readers). If explicit instruction is necessary for most learners, phonics-first is justified rope. The magnitude of the effect for different learner populations (struggling vs. fluent, low-SES vs. high-SES, dyslexic vs. typical) determines whether the constraint''s broad application is appropriate or overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_acquisition_mechanism_implicit_vs_explicit, empirical, 'The primary mechanism of reading acquisition: implicit exposure vs. explicit instruction, and variation by learner cognitive profile.').

omega_variable(
    phonics_scope_appropriate_for_all,
    'Should explicit phonics instruction be universal and scope-sequenced for all students, or should it be targeted to struggling readers and students with dyslexia, with flexible/literature-first approaches available for early fluent readers?',
    'Longitudinal comparison of outcomes between universal-phonics and targeted-phonics with differentiated pathways; measure reading fluency, comprehension, engagement, and literacy trajectory for each learner profile across both systems.',
    'If universal phonics produces superior outcomes across all learner types, broad mandates are justified. If outcomes for early fluent readers are equivalent or superior under differentiated approaches (phonics optional/late for fluent readers), the universal scope is extraction. The constraint''s appropriateness hinges on whether the floor-raising benefit to struggling readers is worth the ceiling constraint on others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_scope_appropriate_for_all, empirical, 'Whether universal scope-and-sequence phonics instruction is necessary, or whether targeted phonics with differentiated pathways would produce equivalent or better outcomes.').

omega_variable(
    teacher_discretion_necessity,
    'Does the reading acquisition benefit from phonics-first instruction depend on strict adherence to a fixed scope-and-sequence, or is the benefit preserved with teacher flexibility in pacing, literature selection, and sequencing?',
    'Comparison between rigid scope-and-sequence implementation and flexible phonics-informed instruction (phonics concepts taught but with teacher discretion in ordering, pacing, and text choice); measure decoding fluency, comprehension, engagement, and teacher-reported instructional responsiveness.',
    'If rigid scope-and-sequence is necessary for phonics efficacy, teacher-discretion extraction is justified. If flexible phonics-informed instruction preserves reading outcomes while reducing extraction, the tight mandates are unnecessary and the constraint''s true function is control rather than coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_discretion_necessity, empirical, 'Whether rigid scope-and-sequence adherence is necessary for phonics efficacy, or whether flexible phonics-informed instruction preserves benefits while respecting teacher professional discretion.').

omega_variable(
    sibling_reading_theoretical_foreclosure,
    'Does the phonics reading''s core premise (explicit instruction is foundational) logically foreclose the whole-language reading''s core premise (implicit learning is primary), or can both be held by a single epistemic framework?',
    'Philosophical and cognitive-science analysis: do the two claims make incompatible predictions about reading-acquisition pathways and learner outcomes, or are they compatible descriptions of different learning populations or phases?',
    'If the readings are logically incompatible within a single learning-science framework, foreclosure is appropriate (one must be false; the evidence will determine which). If they are compatible (e.g., both pathways exist, activated by different learner profiles or instructional contexts), coexistence is appropriate and policy should allow both rather than mandating one universally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_theoretical_foreclosure, conceptual, 'Whether the phonics and whole-language readings foreclose each other (logically incompatible) or coexist (both valid for different populations or phases).').

omega_variable(
    authority_grounding_reading_science_vs_practice,
    'Does the legitimacy of phonics-first instruction derive from reading science evidence (expertise-grounded authority) or from distributed teacher-community agreement and classroom practice (practice-grounded authority)?',
    'Historical and institutional analysis: trace the authority sources cited by phonics-first mandates; examine whether policy change follows reading science publication or classroom-practice shifts; assess whether teacher communities accept or resist the mandate.',
    'If legitimacy is primarily expertise-grounded (reading science evidence), the constraint''s justification is empirical; challenge or reversal depends on evidence. If legitimacy is primarily practice-grounded or requires buy-in from teacher communities, suppression of alternative pedagogies may be unjustified and unsustainable; classroom resistance would indicate the mandate overreaches its justified scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_reading_science_vs_practice, empirical, 'The actual authority grounding for phonics-first mandates: reading science expertise vs. teacher-community practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__phonics_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(read_tr_t25, observed).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__phonics_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(read_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(read_be_t25, observed).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(read_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(read_su_t25, observed).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement_basis(read_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.18).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The reading_acquisition_mechanism kernel instantiates three constraint stories corresponding to three pedagogical theories of how children learn to read. phonics_reading (this story) claims explicit grapheme-phoneme instruction is foundational; whole_language_reading claims implicit learning from authentic texts is primary; balanced_literacy_reading claims integration of both components. The three stories share the same beneficiary (reading fluency for all students) and the same contested empirical referent (the mechanisms of reading acquisition) but make different structural claims about necessity, scope, and the role of explicit vs. implicit learning. Each story has its own ε, beneficiary/victim structure, and type; the engine reads each seat's perception independently. The stories are linked via network.affects_constraints to enable contamination analysis — each reading influences the others' legitimacy conditions and resource allocation (if phonics-first policy dominates, balanced and whole-language approaches are suppressed; if whole-language evidence undermines phonics claims, phonics mandates weaken). Sibling stories instantiate different readings of the same kernel; they are not alternative framings of one constraint but structurally distinct constraints derived from the same contested underlying question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
