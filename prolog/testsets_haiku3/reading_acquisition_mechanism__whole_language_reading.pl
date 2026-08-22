% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Model
 *   domain: educational/cognitive science
 *
 * SUMMARY:
 *   Whole language reading is a pedagogical model and ideological commitment
 *   claiming that reading acquisition emerges naturally from meaningful
 *   engagement with authentic texts, without systematic explicit instruction
 *   in grapheme-phoneme correspondence. The constraint instantiates this
 *   reading by shaping classroom practice, teacher training, textbook
 *   selection, and assessment methods in schools adopting the approach. The
 *   constraint is authored as a rope (genuine coordination of literary
 *   meaning-making and student autonomy) while the metrics (extractiveness
 *   0.68, suppression 0.72, theater 0.58) describe a substantially
 *   extractive, actively enforced arrangement that harms struggling readers.
 *   This claim/metric gap is intentional: the engine will compute whether the
 *   rope claim or the extractive metrics capture the actual structure.
 *
 * KEY AGENTS:
 *   - Teacher practitioners: moderate power, set instructional approach within school adoption, gain autonomy but constrained by curriculum and ideology
 *   - Proficient readers: powerful, benefit from rich literary environment and implicit learning success, high exit via external support
 *   - Struggling readers: powerless, trapped in classrooms where implicit learning fails, bear cost of later remediation
 *   - Dyslexic students: powerless, neurologically contraindicated for implicit-only learning, trapped until diagnosis and special services
 *   - Low-SES readers: powerless, lack home literacy background and external tutoring, trapped and dependent on school instruction alone
 *   - Progressive education ideology: non-agent beneficiary, vindicated by the constraint's operation and dominance
 *   - Phonics advocates: excluded from policy and teacher training during whole language dominance, cannot implement their reading model
 *   - Reading scientists: observers, measure outcomes and conduct empirical analysis revealing divergence between claims and results
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.72).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Model").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational/cognitive science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'bccb7ef1-ba09-4c08-9a59-b06b5a9c832f').
narrative_ontology:cs_kernel_codification('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', formalized).
narrative_ontology:cs_authority_grounding('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', extraction).
narrative_ontology:cs_interpretation_layer_present('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f').
narrative_ontology:cs_reading_relation('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', foundational, implicit_learning_sufficiency).
narrative_ontology:cs_axiom_status(implicit_learning_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', implicit_learning_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', foundational, meaning_centeredness_primacy).
narrative_ontology:cs_axiom_status(meaning_centeredness_primacy, holdable).
narrative_ontology:cs_axiom_grounding('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', meaning_centeredness_primacy, deontological).
narrative_ontology:cs_axiom('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', secondary, teacher_autonomy_liberation).
narrative_ontology:cs_axiom_status(teacher_autonomy_liberation, holdable).
narrative_ontology:cs_axiom_grounding('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', teacher_autonomy_liberation, conventional).
narrative_ontology:cs_reference_frame('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', implicit_learning_adequacy_framework).
narrative_ontology:cs_drift_state('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bccb7ef1-ba09-4c08-9a59-b06b5a9c832f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, proficient_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, progressive_education_ideology).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, low_socioeconomic_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement whole language instruction in classrooms; gain autonomy in choosing texts and pacing while avoiding systematic phonics scope-and-sequence requirements. Do not receive direct compensation tied to the approach but gain professional discretion and alignment with progressive pedagogy ideology. Exit requires adopting a competing instructional model (phonics, balanced literacy) and defending that choice to colleagues and administrators.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_practitioners, agenda_setter,
    moderate, biographical, constrained, national).

% Acquire reading fluency through meaningful text exposure; develop literary taste and comprehension strategies in the presence of rich language. Their implicit learning capacities are sufficient; they require minimal explicit decoding instruction and benefit from contextualized, literature-rich environments. Can access reading support outside school if needed (home library, tutoring, digital resources).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, proficient_readers, beneficiary,
    powerful, generational, arbitrage, national).

% Spend years immersed in texts without receiving explicit instruction in grapheme-phoneme relationships. Their implicit learning does not transfer to decoding; they fall further behind as grade-level texts become more complex. Later diagnosed as dyslexic or severely behind, they require intensive, costly remediation. Have no choice of instructional approach; are assigned to whole language classrooms by school placement.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Neurologically require explicit, systematic phonics instruction to acquire alphabetic mapping; whole language exposure alone does not trigger the neural pathways needed for decoding. Spend years in classrooms where the instructional model is contraindicated for their learning profile. Identified late (often grade 3–4 or later), then require expensive special education remediation or private tutoring.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, national).

% Depend on school instruction as their primary literacy exposure; lack home libraries, parental reading models, or tutoring resources. Whole language's reliance on implicit learning and home literacy background leaves them unsupported. Early decoding delays compound across grades, reducing college readiness and lifetime earnings. Have no choice of instructional approach; attend public schools implementing whole language policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, low_socioeconomic_readers, payer,
    powerless, biographical, trapped, national).

% The constraint vindicates the ideological claim that learning emerges from authentic engagement and child-centered discovery, not systematic instruction. The ideology shapes teacher training, curriculum design, and educational policy; whole language is its flagship literacy implementation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, progressive_education_ideology, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(reading_acquisition_mechanism__whole_language_reading, progressive_education_ideology).

% Argue for explicit, systematic phonics instruction as the evidence-based foundation for reading acquisition. Excluded from policy-setting and teacher training programs dominated by whole language ideology during the period of whole language dominance (1980s–2000s). Cannot implement their reading model in schools committed to whole language; their research and advocacy are marginalized in educational publishing and teacher preparation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_advocates, excluded,
    moderate, biographical, constrained, national).

% Conduct empirical research on reading acquisition mechanisms. Measure reading outcomes, neural imaging, dyslexia etiology, and long-term effects of instructional approaches. Take testimony and evidence from other seats; their findings reveal divergence between whole language claims and measured outcomes for struggling readers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_scientists, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes literacy instruction around meaningful engagement with authentic texts and student-selected reading materials rather than systematic phonics scope-and-sequence. Allows teachers to adapt pacing and material selection based on student interest and classroom dynamics. Simplifies initial instructional design and gives autonomy to teachers over decoding scope.
% TRANSFER_FUNCTION: Transfers the cost of poor decoding outcomes (remediation, special education services, lowered lifetime literacy attainment) from schools and teachers to struggling readers, dyslexic students, and low-socioeconomic children who lack external literacy support. Teachers gain professional autonomy; students with sufficient implicit learning capacity gain rich literary engagement and meaning-centered instruction; students without sufficient implicit learning capacity bear accumulated, compounding decoding gaps.
% ABSENT_VOICES: Dyslexic students are not present in initial policy adoption conversations (diagnosis typically arrives years after whole language implementation begins). Parents of struggling readers who lack resources for external tutoring are shut out of policy debates dominated by progressive educators and educational ideology advocates. Phonics researchers and advocates are marginalized during periods of whole language institutional dominance (1980s–2000s); their empirical findings are excluded from teacher training and curriculum adoption cycles.
% DISAPPEARANCE_RATIONALE: If whole language instruction were replaced with mixed or phonics-based approaches, reading outcomes for struggling readers and dyslexic students would improve measurably within 2–3 years; remediation costs would decline as early decoding support increased; teacher training would shift to include explicit phonics pedagogy; and the institutional and ideological commitment to implicit-learning-only reading would lose its policy anchors. The literary engagement and student autonomy benefits would persist under mixed approaches; only the exclusive reliance on implicit learning would shift.
% FOUNDING_PROBLEM: 1970s–1980s educational pedagogy was criticized for over-reliance on phonics drill, disconnection between skill instruction and literary meaning-making, and student disengagement from reading as a lived practice. Whole language emerged as a response: organize reading instruction around authentic texts, student choice, and meaning-centered comprehension from the beginning, trusting that decoding skills would emerge implicitly from exposure to written language in meaningful contexts.
% FOUNDING_PROBLEM_CORROBORATION: Educational historians and critical pedagogists attest the founding problem was real: 1970s phonics instruction was often divorced from meaning and student interest. However, neuroscience data (functional MRI, dyslexia research, developmental trajectories) and large-scale reading research (National Reading Panel 2000, National Literacy Panel 2008, subsequent meta-analyses) show that explicit phonics instruction and authentic literary engagement are NOT mutually exclusive. Effective reading instruction integrates both. Reading scientists and cognitive neuroscientists outside the whole language ideological community attest that the founding problem (over-mechanized, meaning-disconnected reading) is solved in contemporary balanced-literacy and evidence-based phonics approaches. Whole language persists as ideological commitment, not problem-solving response.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68, rising from 0.22 in 1975 to 0.68 by 2020) because the constraint imposes costs on struggling readers and dyslexic students (remediation, lowered attainment, opportunity loss) while concentrating benefits on proficient readers, teachers, and progressive ideology. Suppression is higher (0.72) because the constraint persists despite decades of contradictory reading science: schools and teacher training actively exclude phonics instruction and marginalize phonics research, maintaining ideological commitment over empirical responsiveness. Theater ratio is moderate-high (0.58) because the constraint increasingly performs ideological meaning-making while functional literacy outcomes for struggling readers deteriorate — teachers describe student 'authentic engagement' while reading gaps widen. The measurement series shows extraction accumulating over the interval (1975–2020): as research evidence mounted against whole language and costs to struggling readers compounded, the constraint required more active enforcement (rising suppression_requirement) and more theatrical justification (rising theater_ratio) to persist. All metrics are authored on a single shared time grid (1975, 1985, 1995, 2005, 2015, 2020) with every metric present at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the teacher-practitioner and proficient-reader seats, whole language is genuine coordination: freedom to choose engaging texts, student autonomy, literary development, classroom community. From the struggling-reader and dyslexic-student seats, the same structure operates as enforced extraction: years without decoding instruction, compounding gaps, late diagnosis, expensive remediation, lowered lifetime outcomes. The engine computes per-seat types from the structural data (power, exit options, beneficiary/victim membership): proficient readers and teachers should compute as beneficiaries near or in rope-type outcomes; struggling readers and dyslexic students should compute as victims in snare or tangled-rope territory. The divergence is the measurement the corpus takes — the claim (rope) vs. the computed type is the diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher practitioners hold moderate power and constrained exit (leaving whole language means adopting and defending a competing model to colleagues and administrators). They benefit from instructional autonomy and alignment with progressive ideology. Directionality: d ≈ 0.25–0.35 (near beneficiary, constrained but not trapped). Proficient readers hold powerful exit options (parental support, tutoring, home literacy) and gain from rich literary engagement. Directionality: d ≈ 0.1 (strong beneficiary, fully mobile). Struggling readers and dyslexic students are powerless, trapped in school placement, bear the cost of implicit-learning-only instruction that doesn't work for their neurological profile, and have no choice of approach. Directionality: d ≈ 0.85–0.95 (strong targets, nearly fully trapped). Low-SES readers are powerless, trapped (school is their primary literacy source), and dependent on explicit instruction whole language does not provide. Directionality: d ≈ 0.88 (strong targets, trapped). Progressive education ideology is a non-agent beneficiary (vindicated by the constraint's operation and institutional dominance, collects ideological legitimacy, not rents). Phonics advocates are excluded: they have moderate power, constrained exit (cannot implement their model in whole-language schools), and are structurally barred from policy and training. Directionality: d ≈ 0.65 (near target, constrained). The directionality distribution shows asymmetry: teachers and proficient readers near the beneficiary end, struggling readers and dyslexic students near the target end, systematic exclusion of phonics alternatives at the institutional level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s over-mechanized, meaning-disconnected reading instruction) was real and whole language offered a genuine alternative. By 2000, the National Reading Panel and subsequent cognitive neuroscience research established that effective reading instruction requires BOTH explicit phonics and authentic literary engagement — the dichotomy was false. The constraint's founding problem is dead: contemporary reading pedagogy no longer employs rote, meaning-disconnected phonics; the 'choice' between meaning and skill is no longer the pedagogical question. Yet whole language persists and has become institutionally entrenched in teacher training, textbooks, and school policy. The constraint now persists despite its founding problem being solved, which is the signature of mandatrophy. The rising theater_ratio (0.15 in 1975 to 0.58 in 2020) and rising suppression_requirement (0.35 to 0.72) show increasing theatrical maintenance: the constraint's functional legitimacy (solving the founding problem) has eroded, but institutional and ideological commitment keeps it in place, requiring active suppression of competing models and performative meaning-making narratives to defend it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is whole language a reading of a contested kernel (competing theories of how reading acquisition works), or is it a empirically falsified theory that persists for ideological reasons?',
    'The committer frame instantiates this as a kernel reading. If the founding problem is dead and the constraint persists despite contradictory evidence, the question becomes whether it is a failed ideology (false premise, false kernel claim) or a legacy institutional form. Distinguish: does the constraint still claim legitimacy from the kernel (reading IS implicit learning), or has it switched to defending itself through institutional inertia (theater)? If the former, this is a kernel reading. If the latter, reclassify as piton.',
    'If kernel reading: the engine validates the cs_structure block and routes this story to the commitment-system analysis. If piton: the story shifts focus to institutional persistence without legitimacy claims, theater_ratio becomes the primary diagnostic, and mandatrophy resolves in a different direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether whole language persists as a legitimacy claim (kernel reading) or as institutional inertia (piton).').

omega_variable(
    sibling_reading_foreclosure,
    'Do the axioms of whole language reading logically foreclose phonics reading, or do they coexist as alternative pedagogical choices?',
    'Analyze the axiom statements: if whole language''s foundational claim (reading emerges implicitly from meaningful engagement) logically requires that explicit phonics is ineffective or harmful, the readings foreclose. If whole language allows that explicit phonics might work but chooses meaning-centered engagement anyway, the readings coexist. Check historical debates: did whole language advocates claim phonics is counterproductive (foreclosure) or claim their method is better (coexistence)?',
    'If foreclose: the cs_structure.reading_relations entry should be ''forecloses''. If coexist: it should be ''coexists_with''. Foreclosure would suggest the sibling reading''s axioms are logically ruled out; coexistence would suggest competing institutional commitments rather than incompatible premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical scope of whole language''s core premise relative to phonics-based reading.').

omega_variable(
    implicit_learning_capacity_variation,
    'Does implicit learning capacity for reading vary substantially across learners due to neurological or developmental differences, or is it a universal human capacity?',
    'Neuroscience data on dyslexia, reading development trajectories across SES and genetic factors, and response to intervention studies. If implicit learning capacity is universal, whole language should work for all. If it varies widely, struggling readers and dyslexic students are systemically harmed.',
    'If capacity varies: the measured victimization of struggling readers and dyslexic students is structural, not accidental. The constraint becomes snare-like (harms identifiable victims who cannot exit). If capacity is universal: failure is due to implementation or environmental factors, not the reading model itself. The classification might shift toward tangled_rope (genuine coordination function with uneven distribution of costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_learning_capacity_variation, empirical, 'Individual variation in implicit learning capacity for reading acquisition.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (teachers lack training in explicit phonics, schools lack phonics materials) or internalized (teachers believe whole language is pedagogically correct despite evidence)?',
    'Post-evidence suppression trajectory: after meta-analyses and reading science consensus shift (post-2000), did whole language adoption decline due to material/training barriers (structural), or did ideological commitment persist despite access to alternatives (internalized)? If teachers continued whole language implementation while expressing awareness that phonics evidence exists, suppression is internalized. If teachers lacked phonics training and materials, it is structural.',
    'If structural: removing barriers (training, materials, policy flexibility) would lower suppression and shift outcomes. If internalized: the constraint carries deeper institutional roots; resistance to phonics persists even when barriers fall. The distinction informs whether remediation is instructional (quick) or cultural (slow).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of phonics instruction is structural barriers or internalized ideological commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1975, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1975, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(read_tr_t1985, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2005, 0.51).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2015, 0.56).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2020, 0.58).

% Extraction over time
narrative_ontology:measurement(read_be_t1975, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(read_be_t1985, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1975, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(read_su_t1985, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.18).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The reading_acquisition_mechanism kernel decomposes into three constraint stories: whole_language_reading (this story), phonics_reading, and balanced_literacy_reading. Each instantiates a distinct reading of how reading acquisition works, with different axioms, different stakeholder arrangements, and different extraction profiles. All three share the same kernel (reading acquisition mechanisms) but diverge on the mechanism claimed. This story captures whole language's claim and its measured operation; the sibling stories capture competing institutional commitments. Network edges link all three, indicating they are mutually constraining readings of a single contested educational kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
