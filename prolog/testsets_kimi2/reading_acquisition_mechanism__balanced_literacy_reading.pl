% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition Mandate
 *   domain: educational/psychological/institutional
 *
 * SUMMARY:
 *   Balanced literacy is an institutional compromise reading of the
 *   reading-acquisition kernel, asserting that children need both explicit
 *   phonics instruction and engagement with authentic literature in an
 *   integrated practice. Emerging in the 1990s as a truce in the reading
 *   wars, it has become the dominant curriculum mandate in many
 *   English-speaking school districts. The constraint coordinates between
 *   warring pedagogical factions but extracts from studentsâparticularly
 *   those with dyslexia and other decoding difficultiesâby allowing the
 *   phonics component to be diluted or poorly sequenced in practice. Textbook
 *   publishers, teacher-training programs, and district administrators
 *   benefit from the model's institutionalized ambiguity, while classroom
 *   teachers bear implementation confusion and students bear the risk of
 *   reading failure. The authored metrics describe a constraint whose
 *   extraction and theatricality have risen as it has matured, while the
 *   claimed type identifies it as a tangled rope: genuine coordination
 *   function plus asymmetric extraction.
 *
 * KEY AGENTS:
 *   - textbook_publishers: Primary beneficiary (powerful/mobile) â captures curriculum revenue
 *   - district_administrators: Agenda setter (institutional/constrained) â mandates adoption and mediates political conflict
 *   - teacher_training_programs: Secondary beneficiary (institutional/constrained) â certifies teachers in the method
 *   - classroom_teachers: Payer (moderate/constrained) â bears implementation ambiguity and accountability pressure
 *   - students_with_dyslexia: Primary target (powerless/trapped) â depends on systematic phonics that is often under-delivered
 *   - struggling_readers: Secondary target (powerless/trapped) â receives contextual guessing strategies instead of decoding instruction
 *   - phonics_advocates: Excluded voice (organized/constrained) â pushed out of curriculum committees
 *   - reading_researchers: Analytical observer (analytical/analytical) â produces evidence often ignored by adoption bodies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.55).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational/psychological/institutional").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'ca4229b6-a001-4956-a3a3-6c8809c020e9').
narrative_ontology:cs_kernel_codification('ca4229b6-a001-4956-a3a3-6c8809c020e9', distributed).
narrative_ontology:cs_authority_grounding('ca4229b6-a001-4956-a3a3-6c8809c020e9', distributed).
narrative_ontology:cs_reading_relation('ca4229b6-a001-4956-a3a3-6c8809c020e9', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca4229b6-a001-4956-a3a3-6c8809c020e9', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('ca4229b6-a001-4956-a3a3-6c8809c020e9', foundational, integrated_practice_required).
narrative_ontology:cs_axiom_status(integrated_practice_required, holdable).
narrative_ontology:cs_axiom_grounding('ca4229b6-a001-4956-a3a3-6c8809c020e9', integrated_practice_required, empirically_contingent).
narrative_ontology:cs_axiom('ca4229b6-a001-4956-a3a3-6c8809c020e9', foundational, explicit_phonics_contextualized).
narrative_ontology:cs_axiom_status(explicit_phonics_contextualized, holdable).
narrative_ontology:cs_axiom_grounding('ca4229b6-a001-4956-a3a3-6c8809c020e9', explicit_phonics_contextualized, empirically_contingent).
narrative_ontology:cs_reference_frame('ca4229b6-a001-4956-a3a3-6c8809c020e9', integrated_literacy_equilibrium).
narrative_ontology:cs_drift_state('ca4229b6-a001-4956-a3a3-6c8809c020e9', science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca4229b6-a001-4956-a3a3-6c8809c020e9', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, textbook_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell balanced literacy curricula, leveled readers, and assessment packages to school districts. Revenue depends on districts adopting programs that blend phonics and literature components. Can pivot product lines if district demand shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, textbook_publishers, beneficiary,
    powerful, generational, mobile, national).

% Select and mandate literacy curricula for their districts. Adopt balanced literacy frameworks to mediate between phonics and whole-language constituencies locally. Face political pressure from parents and school boards if test scores stagnate.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).

% Certify teachers in balanced literacy methods, offering coursework on integrating phonics mini-lessons with workshop models and leveled texts. Enrollment and grant funding flow from the institutionalized status of balanced literacy in state standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs, beneficiary,
    institutional, generational, constrained, national).

% Are required to implement district-mandated balanced literacy programs, often without clear scope and sequence for phonics. Experience professional ambiguity about how much explicit decoding instruction is enough and face accountability pressure for reading outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Depend on systematic, explicit phonics instruction to acquire decoding skills. In balanced literacy classrooms, they often receive incidental phonics exposure through leveled texts rather than structured lessons, delaying identification and remediation of reading disabilities.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Require consistent decoding and spelling instruction to build reading fluency. Under balanced literacy, they may be cued to guess words from context or pictures rather than taught grapheme-phoneme correspondences systematically, leaving them behind peers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Promote research supporting systematic explicit phonics for all students. Are often excluded from curriculum adoption committees and state textbook reviews in districts where balanced literacy is entrenched, or their input is tokenized within balance frameworks that preserve whole-language pedagogy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Conduct empirical studies on reading acquisition, comparing outcomes across instructional methods. Publish meta-analyses showing larger effect sizes for systematic phonics; their findings circulate in academic and policy venues but do not always penetrate district adoption processes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the persistent disciplinary conflict between phonics and whole-language approaches by proposing that both explicit decoding instruction and authentic text exposure are necessary and must be integrated in classroom practice.
% TRANSFER_FUNCTION: Moves financial resources from school districts to textbook publishers and teacher-training programs; moves instructional ambiguity and accountability risk to classroom teachers; moves the risk of reading failure to students who need systematic phonics but receive variable implementation.
% ABSENT_VOICES: Explicit systematic phonics researchers and advocates, as well as parents of children with dyslexia, are often structurally excluded from curriculum adoption committees and textbook review panels in balanced-literacy districts, or their recommendations are diluted by balance requirements.
% DISAPPEARANCE_RATIONALE: Districts would revert to whichever instructional model dominates local politics or evidence-based policy (systematic phonics or whole language); the market for balanced literacy curricula would contract; teacher training would reorient; and student reading outcomes would shift toward the surviving model's profile.
% FOUNDING_PROBLEM: The reading wars â a decades-long ideological and methodological conflict between phonics and whole-language camps that produced curricular paralysis, teacher confusion, and inconsistent reading outcomes across districts.
% FOUNDING_PROBLEM_CORROBORATION: Textbook publishers and colleges of education attest the conflict was real and required an integrative compromise. Reading scientists and phonics advocates outside the balanced-literacy industry attest the conflict was resolvable by following the evidence for systematic phonics, and that the compromise was manufactured to preserve institutional markets; large-scale empirical studies from outside the benefiting parties support the systematic-phonics account.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.65) reflects the gap between the model's claimed integration and its typical implementation, which research shows often collapses to whole-language practices. Suppression (0.55) is moderate: phonics advocates face structural exclusion from committees and textbook adoption processes, but the Science of Reading movement has recently increased resistance. Theater ratio (0.70) is high because districts and publishers perform phonics inclusionâadding phonics components to labels and materialsâwhile the actual instructional minutes and systematicity remain low. Accessibility collapse (0.50) captures that once a district adopts balanced literacy, alternative systematic phonics programs become politically difficult to adopt. Resistance (0.45) reflects growing organized pushback from parents and researchers. The temporal series track the constraint's evolution from a seemingly genuine compromise (low theater, modest extraction) to a hardened institutional format where the compromise itself serves extraction.
 *
 * PERSPECTIVAL GAP:
 *   The textbook publisher and district administrator seats experience the constraint as a viable coordination mechanism that preserves markets and political peace. The teacher, student, and phonics-advocate seats experience it as an enforced arrangement where the phonics half of the balance is systematically underweight. The engine computes this divergence from the structural data: agenda setters with constrained but secure institutional positions versus trapped or constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Textbook publishers, teacher-training programs, and district administrators are declared beneficiaries, deriving revenue, enrollment, and conflict-avoidance from the constraint's persistence. Students with dyslexia, struggling readers, and classroom teachers are declared payers, bearing the costs of implementation ambiguity and reading failure. Phonics advocates are excluded, receiving high directionality by virtue of their structural exclusion. Reading researchers are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction facts. A pure rope reading would ignore the documented implementation collapse and the identifiable financial beneficiaries. A pure snare reading would ignore the genuine coordination problem the reading wars presented and the real (if uneven) phonics inclusion some balanced programs achieve. The tangled rope classification captures that the compromise itself has become the extraction mechanism: the balance mandate coordinates institutional peace while the under-specification of phonics systematicity extracts literacy outcomes from vulnerable students.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_ambiguity,
    'Does balanced literacy fail because teachers lack training fidelity, or because the model structurally under-specifies systematic phonics?',
    'Randomized controlled trials comparing high-fidelity balanced literacy implementation against systematic phonics, coupled with classroom observation protocols measuring actual phonics time.',
    'If high-fidelity implementation still underperforms systematic phonics, the constraint is structurally extractive; if fidelity is the sole issue, extraction is implementation noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_ambiguity, empirical, 'Whether extraction is structural or implementation-noise driven').

omega_variable(
    naturalness_of_integration,
    'Is integrated instruction in phonics and literature a genuine cognitive requirement for typical reading acquisition, or a political construct serving institutional compromise?',
    'Cognitive science research on whether decoding and meaning-making require simultaneous integration or can be sequentially developed.',
    'If integration is not cognitively necessary, the coordination claim is cover for institutional market preservation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_integration, conceptual, 'Whether the integration mandate is cognitively necessary or politically constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of systematic phonics advocates in balanced-literacy districts structural (committee exclusion, funding denial) or internalized (educators believe balance is self-evidently correct)?',
    'Documentation of committee composition and funding flows; teacher belief surveys in balanced-literacy districts.',
    'Structural suppression would classify the constraint as more actively extractive; internalized suppression indicates ideological capture of the practitioner base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of phonics advocacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bal_lit_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bal_lit_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(bal_lit_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(bal_lit_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(bal_lit_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(bal_lit_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.65).
narrative_ontology:measurement(bal_lit_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.7).

% Extraction over time
narrative_ontology:measurement(bal_lit_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bal_lit_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(bal_lit_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(bal_lit_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(bal_lit_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(bal_lit_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(bal_lit_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bal_lit_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bal_lit_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(bal_lit_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(bal_lit_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(bal_lit_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(bal_lit_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(bal_lit_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
