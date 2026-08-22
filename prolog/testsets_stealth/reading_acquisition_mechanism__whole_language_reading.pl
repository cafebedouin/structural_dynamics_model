% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole-Language Reading Instruction Doctrine
 *   domain: educational psychology/literacy pedagogy
 *
 * SUMMARY:
 *   Between roughly 1985 and 2025, the dominant doctrine of early reading
 *   instruction in the English-speaking world held that children learn to
 *   read the way they learn to speak: through immersion in meaningful,
 *   authentic text, with decoding skill emerging as a by-product of
 *   engagement rather than through explicit teaching of letter-sound
 *   correspondences. The doctrine was carried by schools of education (which
 *   certify every teacher), by a publishing ecosystem of leveled texts,
 *   running records, and cueing posters, and by professional orthodoxy that
 *   dismissed systematic phonics as 'drill and kill.' It delivered a genuine
 *   good — classrooms organized around real books, motivated readers, teacher
 *   professional judgment — while imposing its costs on the minority it never
 *   named: children who do not infer the code from exposure, who were taught
 *   to guess from pictures and context instead, and who arrived in
 *   adolescence functionally illiterate. The enforcement web (certification,
 *   adoption cycles, coaching, professional gatekeeping) held for four
 *   decades against accumulating contrary evidence and was breached only by
 *   an unusual coalition of investigative journalism, parent advocacy, and
 *   state legislation. This file instantiates the whole_language_reading of
 *   the reading_acquisition_mechanism kernel; the phonics_reading and
 *   balanced_literacy_reading siblings are separate constraints with their
 *   own epsilon, victim sets, and classifications. The claim/metric gap is
 *   deliberate: claimed_type records the structurally true type from the
 *   authoring seat; the metrics record the arrangement's actual operation.
 *   KEY AGENTS (by structural relationship): - teacher_education_faculty:
 *   agenda-setting beneficiary (institutional/identity_locked) — certifies
 *   the workforce and owns the doctrine - literacy_curriculum_publishers:
 *   agenda-setting beneficiary (institutional/arbitrage) — administers daily
 *   practice through materials and collects program revenue -
 *   autonomy_valuing_classroom_teachers: beneficiary
 *   (organized/identity_locked) — collects professional autonomy and identity
 *   - struggling_readers: primary target (powerless/trapped) — bears the cost
 *   in literacy itself - dyslexic_students: primary target
 *   (powerless/trapped) — bears the highest per-capita cost -
 *   parents_of_struggling_readers: target with secondary excluded position
 *   (moderate/constrained) — bears remediation costs -
 *   reading_science_researchers: analytical observer (organized/analytical) —
 *   produced the evidence that broke the enforcement -
 *   state_education_authorities: external corrector and observer
 *   (institutional/mobile) — now legislates the arrangement's replacement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.72).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.74).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole-Language Reading Instruction Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational psychology/literacy pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'eb8e2372-286d-4cc4-b72d-7779851b82c5').
narrative_ontology:cs_kernel_codification('eb8e2372-286d-4cc4-b72d-7779851b82c5', distributed).
narrative_ontology:cs_authority_grounding('eb8e2372-286d-4cc4-b72d-7779851b82c5', lineage).
narrative_ontology:cs_interpretation_layer_present('eb8e2372-286d-4cc4-b72d-7779851b82c5').
narrative_ontology:cs_reading_relation('eb8e2372-286d-4cc4-b72d-7779851b82c5', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('eb8e2372-286d-4cc4-b72d-7779851b82c5', reading_acquisition_mechanism__balanced_literacy_reading, forecloses).
narrative_ontology:cs_axiom('eb8e2372-286d-4cc4-b72d-7779851b82c5', foundational, decoding_emerges_implicitly_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('eb8e2372-286d-4cc4-b72d-7779851b82c5', decoding_emerges_implicitly_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('eb8e2372-286d-4cc4-b72d-7779851b82c5', secondary, meaning_construction_precedes_word_recognition).
narrative_ontology:cs_axiom_status(meaning_construction_precedes_word_recognition, holdable).
narrative_ontology:cs_axiom_grounding('eb8e2372-286d-4cc4-b72d-7779851b82c5', meaning_construction_precedes_word_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('eb8e2372-286d-4cc4-b72d-7779851b82c5', psycholinguistic_guessing_framework).
narrative_ontology:cs_drift_state('eb8e2372-286d-4cc4-b72d-7779851b82c5', post_national_reading_panel_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eb8e2372-286d-4cc4-b72d-7779851b82c5', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, autonomy_valuing_classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, constructivist_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, three_cueing_hypothesis).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, psycholinguistic_guessing_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and certify the teachers who deliver early reading instruction; their coursework, textbooks, and research programs carry the doctrine that children learn to read by engaging with meaningful text rather than by being taught letter-sound patterns. Certification requirements route every new teacher through their programs. Their enrollments, standing, and published work depend on the approach remaining the field's center of gravity; reorienting toward explicit instruction would require retraining themselves and conceding decades of scholarship.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty, agenda_setter,
    institutional, generational, identity_locked, national).

% Sell the leveled texts, running-record kits, cueing posters, and guided-reading programs that operationalize the approach in classrooms; their catalogs and named-author franchises define what districts buy. Revenue follows adoption cycles; when districts shift toward decodable texts and scripted lessons, product lines must be rebuilt. Their teacher guides script the daily reading block, which makes them co-administrators of classroom practice, and the program revenue accrues to them directly.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers, beneficiary).

% Deliver the daily instruction. The approach hands them professional judgment: choose the books, follow the child, no scripted sequence. Many built their self-concept as teachers around being facilitators of discovery rather than deliverers of drills. Changing approaches means retraining, adopting materials they were taught to disdain, and confronting the possibility that past instruction failed some students. Unions and professional organizations amplify their voice in curriculum fights.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, autonomy_valuing_classroom_teachers, beneficiary,
    organized, biographical, identity_locked, national).

% Children who do not infer the letter-sound code from exposure on the timetable the approach assumes. They spend the primary grades with books leveled above their actual decoding skill, guessing from pictures, first letters, and context, and fall progressively behind peers. They cannot choose their instruction, opt out, or switch schools in most cases; each year of missed foundational instruction compounds, and many never fully catch up even with later remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, immediate, trapped, national).

% Children with a specific difficulty mapping letters to sounds, for whom implicit acquisition is least available and explicit systematic instruction is most necessary. Under the approach they are often labeled slow or unmotivated rather than identified, because the doctrine holds that struggling signals insufficient meaningful engagement rather than an instructional mismatch. Identification and appropriate teaching typically arrive only after years of failure, frequently via private diagnosis their families pay for.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, immediate, trapped, national).

% Watch their children fail to learn to read, then navigate the system: request evaluations, get told to read more at home, eventually pay out of pocket for tutors trained in explicit methods or for private assessments. Historically they had no seat in curriculum adoption committees; their influence arrived late, through advocacy organizations and legislative testimony. Exit is costly: private school, homeschooling, or years of paid tutoring.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, excluded).

% Produce the evidence base: national panel findings, meta-analyses of phonics instruction, eye-tracking studies of cueing, and the cognitive science of how word recognition becomes automatic. For decades their findings were dismissed within schools of education as reductionist; their influence rose sharply once investigative journalism and parent advocacy carried the findings to legislatures. They hold no administrative power over classrooms but now shape the legislative environment.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_science_researchers, observer,
    organized, generational, analytical, global).

% Set certification rules, adopt standards, and fund materials; they delegated reading pedagogy to the education schools and publishers for decades, then began legislating it directly once the evidence became politically undeniable — mandating evidence-based instruction, retraining funds, and dyslexia screening. They can rewrite the rules but must carry the political cost of confronting the education establishment, unions, and incumbent vendors.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, state_education_authorities, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early literacy classrooms around authentic books, shared reading, and meaning-making; gives teachers a coherent professional identity and daily autonomy; solves the district-level problem of running reading instruction without scripted sequences, systematic materials, or intensive teacher training in linguistics.
% TRANSFER_FUNCTION: Moves instructional labor from explicit teaching to child-led discovery; moves the cost of failed acquisition from school budgets to families (private tutoring, private assessment) and to the students themselves (years of literacy); moves curriculum dollars from basals and phonics programs to leveled texts, running records, and cueing materials; moves professional authority to education faculties and named literacy authors.
% ABSENT_VOICES: The children — the seats bearing the largest cost — had no voice in any curriculum decision. Parents of struggling readers were historically absent from adoption committees and were told to read more at home; they organized (Decoding Dyslexia and similar bodies) only after failures accumulated. Reading scientists were present in the research literature but excluded from the institutional conversation, dismissed as reductionist by the education schools that certified teachers.
% DISAPPEARANCE_RATIONALE: If the doctrine and its enforcement web vanished overnight, classrooms would reorganize around explicit code instruction within a school year, the leveled-text and cueing-material market would collapse into decodables and scripted programs, education-school methods curricula would be rewritten, the private remediation industry would shrink, and the population of adolescent struggling readers would fall within a decade — the entire literacy-instruction economy is arranged around this constraint and would rearrange around its absence.
% FOUNDING_PROBLEM: Mid-century reading instruction — round-robin basal reading, decontextualized drills, comprehension quizzes — was widely experienced as joyless and was failing to produce children who loved reading; the arrangement was built to make reading acquisition natural and meaningful, on the model of oral language acquisition, and to put real books in every classroom.
% FOUNDING_PROBLEM_CORROBORATION: The motivational half of the founding problem is corroborated from outside the benefiting parties: reading scientists and historians of instruction agree mid-century skills teaching was often joyless and that engagement matters. The mechanism half — that decoding therefore emerges implicitly — is attested by no source outside the beneficiary set: the National Reading Panel, subsequent meta-analyses, and eye-tracking studies of cueing all contradict it, and parents of dyslexic children attest from their own seats that the premise failed their children. The benefiting parties (education faculties, publishers, the tradition's authors) attest the founding problem is live; no independent source attests that it justifies the standing arrangement.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.72) is high but not total because the arrangement's cost falls on a minority of learners — the children who needed what it declined to teach — while the majority acquired decoding under nearly any method, which both subsidized the doctrine's credibility and concealed its victims. Suppression (0.74) is authored as a raw structural property, unscaled: it lives in certification requirements, adoption cycles, coaching orthodoxy, and the professional cost of teaching against the doctrine; only extractiveness is scaled by directionality and scope downstream. Theater (0.55) rises across the interval as the 'balanced literacy' rebrand let institutions keep the practice while renaming it — by the 2010s a growing share of activity was performing engagement rather than producing it. Accessibility_collapse (0.25) is low: explicit phonics instruction remained fully workable and available throughout — the arrangement never made alternatives impossible, only institutionally costly — which is why it is not a natural law and why it could ultimately be legislated away. Resistance (0.75) is correspondingly high: the reading wars, parent advocacy organizations, and the science-of-reading legislative wave are the resistance record. The measurement series share one grid (t = 0, 8, 16, 24, 32, 40 on a 1985–2025 interval): base_extractiveness climbs as contrary evidence accumulates and the arrangement persists anyway; suppression_requirement ratchets upward as keeping alternatives out grows more expensive, then dips at the interval end as legislative breach arrives; theater_ratio climbs monotonically with the rebrand. Coordination type is declared identity_coordination: the arrangement's binding force is professional identity (the teacher-as-facilitator self-concept) and membership boundary maintenance. The FNL gaming alert applies and is acknowledged: this is precisely the case where identity framing could launder extraction — the structure concentrates cost on powerless agents (children) at national scope, which the identity offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the teacher-education seat the arrangement is professional liberation: it removed scripted programs, restored judgment, and made teaching a meaning-centered profession. From the struggling-reader seat the same arrangement is years of stolen literacy — instruction calibrated to children for whom it was never necessary. From the publisher seat it is a product line with a forty-year adoption cycle. From the researcher seat it is a falsified hypothesis that retained institutional power for two decades after falsification. The engine computes these per-seat classifications from power, exit, and directionality; the divergence between the identity-locked beneficiary seats and the trapped payer seats is the story's core asymmetry. Note also the coalition structure: the seats bearing the largest per-capita cost (children) cannot coalition, which is why effective resistance came from parents and researchers — seats outside the classroom — rather than from the primary targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: teacher_education_faculty and literacy_curriculum_publishers sit near the full-beneficiary end — the arrangement subsidizes their enrollments, research programs, and revenue, and their positions (identity-locked tenure in the doctrine; arbitrage-grade product pivots) insulate them from its costs. autonomy_valuing_classroom_teachers benefit (autonomy, professional identity) but bear secondary costs (defending the doctrine against evidence, eventual retraining), placing them near but not at the beneficiary pole; their identity_locked exit means the benefit is fused with self-concept, so they defend the arrangement as themselves. Victims: struggling_readers and dyslexic_students sit near the full-target end — powerless, trapped (children cannot exit their instruction), bearing the cost in the very currency the arrangement is supposed to produce. parents_of_struggling_readers are targets with constrained exit who pay remediation directly and were historically excluded from the decisions that set the instruction. The derivation from these beneficiary/victim declarations plus exit options already separates the seats cleanly; no directionality overrides were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: mid-century skills instruction was often joyless, and engagement with authentic literature is a genuine pedagogical good that a phonics-only caricature neglects. That real core prevents labeling the arrangement a pure snare — the coordination function is not cover; it operated. But the mechanism premise on which the arrangement's distinctive claims rested — that decoding emerges implicitly, making explicit instruction unnecessary and harmful — is dead as empirical matter, and the arrangement persisted for two decades past that death by renaming itself. The classification therefore resists two mislabels: pure rope (which would ignore the identifiable victims and the enforcement web) and pure snare (which would erase the genuine coordination and the majority of children who learned adequately regardless). The mandatrophy question — has the mandate outlived its function — resolves as split: the motivational half of the mandate is live, the mechanism half is dead, and persistence after the death of the mechanism premise is maintained by identity fusion and revenue, which is what the rising theater_ratio records. founding_problem_status is authored 'contested' for exactly this reason; the mismatch consumer should read that status against disappearance_verdict 'world_rearranges' with the theater trajectory in view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates one reading (whole_language_reading) of the kernel reading_acquisition_mechanism; what would the sibling readings (phonics_reading, balanced_literacy_reading) change structurally, and where is the disagreement located?',
    'Compare the compiled sibling stories. The disagreement is located in the mechanism premise: whether decoding skill is acquired implicitly from meaningful exposure or requires explicit systematic grapheme-phoneme instruction. The phonics_reading sibling inverts the victim and beneficiary sets (struggling readers become the served population; decodable-text publishers replace leveled-text publishers); the balanced_literacy_reading sibling produces an intermediate victim set.',
    'This story''s substantially extractive profile depends on the implicit-acquisition premise being false as a general account; under a sibling reading the same classroom arrangement classifies differently because the victim set and the coordination function are different constraints, not different views of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    implicit_acquisition_population_fraction,
    'What fraction of children actually acquire decoding implicitly from meaningful exposure, and does the arrangement''s credibility rest on generalizing that minority''s success to all learners?',
    'Longitudinal studies crossing instruction type with learner profile (home literacy exposure, phonological processing ability), tracking decoding outcomes rather than engagement proxies.',
    'If implicit acquisition works only for a minority, the arrangement''s cost concentrates on the majority who needed explicit instruction and its coordination-function defense largely collapses; if it works for most, measured extraction is substantially lower and the victims reduce to a small tail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_acquisition_population_fraction, empirical, 'Whether implicit acquisition is a majority mechanism whose failure is exceptional, or a minority phenomenon generalized into doctrine.').

omega_variable(
    teacher_suppression_internalization,
    'Is the persistence of whole-language practice among trained teachers structural (materials lock-in, certification requirements, coaching orthodoxy) or internalized (professional identity fused with the facilitator role)?',
    'Post-mandate practice trajectories in states that retrained teachers and replaced materials: if cueing behaviors persist after the structural enforcement is removed, the suppression is partly internalized.',
    'If internalized, effective suppression outlasts the enforcement web and remediation of the arrangement requires identity-level change in the profession, not merely new materials and mandates; measured suppression understates the true persistence force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_suppression_internalization, empirical, 'Structural versus internalized suppression mechanism in the teacher population.').

omega_variable(
    motivation_benefit_magnitude,
    'How large is the genuine motivational and comprehension benefit of organizing classrooms around authentic texts, independent of the decoding instruction the arrangement displaced?',
    'Controlled comparisons of classrooms matched on explicit code instruction but differing in authentic-text emphasis, measuring motivation, vocabulary, and comprehension.',
    'This bounds the coordination-function share of the arrangement: the portion of its cost that is the price of a real good rather than extraction. A large independent benefit supports the tangled-rope reading; a negligible one pushes the arrangement toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(motivation_benefit_magnitude, empirical, 'Magnitude of the real coordination good the arrangement delivers alongside its costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(read_tr_t8, observed).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(read_tr_t16, observed).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(read_tr_t24, observed).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement_basis(read_tr_t32, observed).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(read_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(read_be_t8, observed).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(read_be_t16, observed).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(read_be_t24, observed).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement_basis(read_be_t32, observed).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(read_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(read_su_t8, observed).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(read_su_t16, observed).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement_basis(read_su_t24, observed).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement_basis(read_su_t32, observed).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(read_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'how children learn to read' decomposes into three structurally distinct constraints — phonics_reading (explicit systematic instruction foundational; low epsilon, broad beneficiary set), balanced_literacy_reading (integrated practice; intermediate epsilon and victim set), and whole_language_reading (this file; implicit acquisition; substantially extractive with concentrated victims). The upstream reading in empirical confidence is phonics_reading, whose evidence base was cited against this reading; the downstream, contested reading is this one. Each family member links the others via affects_constraints; each carries its own epsilon, stakeholders, and claimed type per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
