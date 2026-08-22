% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-First Reading Instruction Legitimacy
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'reading_acquisition_legitimacy': the phonics-decoding-primacy reading.
 *   It asserts that reading is fundamentally the decoding of written symbols
 *   to sound, and that legitimate reading instruction makes the alphabetic
 *   principle explicit through systematic, structured phonics teaching. This
 *   reading has become dominant in U.S. education policy since ~2000, backed
 *   by cognitive science research on phonological processing and by
 *   institutional advocates (structured literacy trainers, Science of Reading
 *   organizations, curriculum vendors). The sibling readings —
 *   whole_language_meaning_primacy, balanced_literacy_integration,
 *   structured_literacy_remediation — hold different premises about what
 *   reading IS and what legitimate instruction looks like. This constraint
 *   models the phonics-decoding-primacy reading alone: its ε
 *   (extractiveness), its beneficiaries and victims, its legitimacy
 *   mechanisms, and its enforcement. The other readings are separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - structured_literacy_trainers: institutional agenda-setter; set curriculum standards around phoneme-grapheme mapping and decodable texts
 *   - phonics_curriculum_vendors: powerful beneficiary; profit from adoption mandates and curriculum replacement cycles
 *   - educational_psychologists_decoding_focused: powerful beneficiary; build research careers and federal funding on decoding-problem framing
 *   - whole_language_practitioners: organized payer; face professional reclassification and curriculum replacement
 *   - classroom_teachers_meaning_centered: moderate-power identity-locked payer; professional identity constituted through practices now deemed illegitimate
 *   - english_language_learner_teachers: moderate-power constrained payer; face mandates misaligned with multilingual acquisition research
 *   - struggling_early_readers: powerless trapped beneficiary-payer; get early intervention but lose meaning-centered support
 *   - policy_makers_state_federal: institutional agenda-setter; mandate phonics-first via legislation and accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.71).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-First Reading Instruction Legitimacy").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '64580888-1614-4781-a1f3-87e97dd17ede').
narrative_ontology:cs_kernel_codification('64580888-1614-4781-a1f3-87e97dd17ede', fixed_text).
narrative_ontology:cs_authority_grounding('64580888-1614-4781-a1f3-87e97dd17ede', extraction).
narrative_ontology:cs_interpretation_layer_present('64580888-1614-4781-a1f3-87e97dd17ede').
narrative_ontology:cs_reading_relation('64580888-1614-4781-a1f3-87e97dd17ede', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('64580888-1614-4781-a1f3-87e97dd17ede', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('64580888-1614-4781-a1f3-87e97dd17ede', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('64580888-1614-4781-a1f3-87e97dd17ede', foundational, decoding_is_reading_foundation).
narrative_ontology:cs_axiom_status(decoding_is_reading_foundation, holdable).
narrative_ontology:cs_axiom_grounding('64580888-1614-4781-a1f3-87e97dd17ede', decoding_is_reading_foundation, empirically_contingent).
narrative_ontology:cs_axiom('64580888-1614-4781-a1f3-87e97dd17ede', foundational, alphabetic_principle_must_be_explicit).
narrative_ontology:cs_axiom_status(alphabetic_principle_must_be_explicit, holdable).
narrative_ontology:cs_axiom_grounding('64580888-1614-4781-a1f3-87e97dd17ede', alphabetic_principle_must_be_explicit, empirically_contingent).
narrative_ontology:cs_reference_frame('64580888-1614-4781-a1f3-87e97dd17ede', alphabetic_principle_as_reading_foundation).
narrative_ontology:cs_drift_state('64580888-1614-4781-a1f3-87e97dd17ede', contemporary_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64580888-1614-4781-a1f3-87e97dd17ede', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_trainers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, educational_psychologists_decoding_focused).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers_meaning_centered).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, english_language_learner_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_scientists_meaning_language_focused).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train teachers in explicit phonics instruction and Orton-Gillingham or Science of Reading frameworks. Set curriculum standards emphasizing phoneme-grapheme correspondences, sound sequencing, and decodable texts. Certify teachers in their methods. Benefit from growing policy mandates requiring phonics-first instruction in schools.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_trainers, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell structured phonics programs, decodable readers, assessment tools, and teacher training materials. Profit directly from adoption mandates and curriculum replacement cycles driven by phonics-first policy shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Build research careers, grant funding, and professional standing on cognitive science of decoding and alphabetic principle. Secure federal research funding and influence policy by framing reading acquisition as primarily a decoding problem. Cited in policy documents as scientific authority.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, educational_psychologists_decoding_focused, beneficiary,
    powerful, generational, mobile, national).

% Teachers trained in whole language and balanced literacy approaches over decades. Face professional reclassification (their training is now labeled 'unscientific'), curriculum replacement mandates, and re-training requirements. Cannot easily exit — they are embedded in school systems and their professional identity is constituted through the practices now deemed illegitimate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_practitioners, payer,
    organized, biographical, constrained, national).

% Teach reading through guided reading, literature-based instruction, and student-centered meaning-making. Are increasingly required to adopt phonics-first curricula, attend re-training on decoding instruction, and replace classroom libraries with decodable readers. Their professional judgment and accumulated classroom knowledge are treated as obstacles rather than assets.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers_meaning_centered, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers_meaning_centered, excluded).

% Work with students learning English as a second or additional language, where meaning-based immersion in rich language contexts has proven effective. Face mandates to prioritize phonics-first instruction designed for monolingual speakers, which may not align with multilingual acquisition research or their students' linguistic realities.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, english_language_learner_teachers, payer,
    moderate, biographical, constrained, local).

% Children who show early difficulty with reading. Under phonics-first instruction, they receive explicit decoding intervention early, which helps many. However, some face intensive phonics drills that isolate decoding from meaningful communication, and may not receive the comprehension support or authentic reading experience that other approaches prioritize.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers, payer).

% Children who acquire decoding easily. Benefit minimally from explicit phonics instruction (they figure out the pattern quickly); often under-served by phonics-first curricula that focus on struggling readers, decodable-text libraries, and repetitive decoding practice rather than rich literature and meaning-making.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_readers, beneficiary,
    powerful, biographical, mobile, local).

% Researchers whose work emphasizes comprehension, vocabulary, language experience, and meaning-making in reading development. See their research cited less frequently in policy; struggle to secure funding for studies outside the decoding-focused framework; face professional pressure to shift research programs toward phonics-aligned questions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_scientists_meaning_language_focused, payer,
    moderate, generational, constrained, national).

% Manage school improvement and reading outcomes under state mandates and federal accountability requirements. Navigate pressure to adopt phonics-first curricula while balancing teacher resistance, parent concerns, and limited budgets for curriculum replacement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, school_district_administrators, observer,
    institutional, generational, constrained, regional).

% Shape reading instruction policy through legislation, guidelines, and accountability measures. Increasingly mandate phonics-first instruction based on neuroscience framing and organized advocacy from structured literacy advocates; cite 'science of reading' as settled authority.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_makers_state_federal, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_trainers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate literacy instruction around an explicit, empirically-validated core theory (alphabetic principle + phoneme-grapheme mapping) so that all teachers across jurisdictions use a theoretically coherent sequence, early identification of struggling readers is consistent, and instructional time is focused on the foundational mechanism of reading acquisition.
% TRANSFER_FUNCTION: Moves authority and legitimacy from individual teacher judgment and literature-centered pedagogy to curriculum designers, structured literacy trainers, and decoding-assessment systems. Moves public education spending toward phonics programs, decodable texts, and teacher re-training. Moves professional standing from experienced whole-language teachers to decoding-research experts and Science of Reading trainers.
% ABSENT_VOICES: Whole-language and balanced-literacy practitioners are not absent but are increasingly classified as non-expert voices whose training is 'unscientific'; they are present but delegitimized. Multilingual education researchers and English-language-learner specialists are structurally under-represented in policy deliberation on reading instruction. Classroom teachers' lived experience with diverse readers is treated as anecdotal rather than evidence. Advanced readers and their teachers (whose needs are different from struggling readers) are largely absent from the deficit-focused framing.
% DISAPPEARANCE_RATIONALE: If the phonics-first legitimacy mandate vanished, schools would revert to mixed instructional approaches, curriculum publishers would re-stock literature and meaning-centered resources, teachers trained in whole language would regain professional standing, and districts would stop spending on phonics-program adoption and retraining. The entire machinery of structured literacy certification, assessment, and teacher audit would dissolve.
% FOUNDING_PROBLEM: Early reading instruction in the late 20th century was often unsystematic about decoding instruction; many struggling readers were not identified early or given explicit phonics support, and some teachers lacked training in phoneme awareness and sound sequencing. Research on the alphabetic principle and phonological processing showed decoding is a foundational mechanism that must be explicitly taught to many children.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and reading researchers studying phonological processing (Linnea Ehri, David Share, Stanislas Dehaene) document the mechanism of decoding and phoneme awareness; their work is cited as authority by phonics advocates. Whole-language and balanced-literacy researchers attest the problem is overstated — systematic literature shows many children acquire decoding without explicit phonics, and meaning-centered contexts support both decoding and comprehension. No consensus on whether the founding problem (insufficient decoding instruction in 1990s schools) justifies the current phonics-first mandate's scope and persistence.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because this reading's enforcement systematically privileges one pedagogical tradition over others, redirects curriculum spending, reclassifies experienced teachers as non-expert, and creates credential scarcity (only trained phonics teachers are 'science-aligned'). Suppression (0.71) is higher than extractiveness because enforcement is not passive: whole-language approaches must be actively deligitimized (by calling them 'unscientific'), teachers must be re-trained and audited, literature-based curricula must be replaced, and alternative research is systematically under-funded. Theater (0.42) is moderate-rising: early enforcement (1980–2005) was genuinely about correcting unsystematic instruction, but by 2020 a growing share of enforcement activity sustains the reading's institutional position and vendor profits rather than improves outcomes. Accessibility_collapse (0.78) is high: once the reading is established as 'science-based' in policy, teachers perceive no alternative as legitimate; their exit from phonics-first instruction appears professionally self-destructive. Resistance (0.58) is moderate: whole-language practitioners and ELL teachers actively resist, but they are professionally constrained and institutionally under-resourced compared to the phonics-advocacy coalition.
 *
 * PERSPECTIVAL GAP:
 *   From the structured-literacy-trainer seat, the constraint is genuine coordination of evidence-based instruction across schools — a public good. From the whole-language-practitioner seat, it is an illegitimate enforcement of one cognitive model that excludes other valid pedagogical traditions. From the struggling-reader seat, early decoding support is beneficial, but isolation from authentic literature and meaning-making is extractive. The engine computes per-seat directionality from power + exit + beneficiary/victim status; the seated divergence should emerge from that structural data. The authored metrics describe the constraint as extractive because enforcement is active, beneficiaries are concentrated, and victims are diffuse but constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured_literacy_trainers and phonics_vendors benefit directly (low d: subsidized by the constraint). Educational_psychologists focused on decoding benefit from research funding and policy influence (low-to-moderate d: the constraint vindicates their research program). Whole-language practitioners are targets (high d: their professional standing is extracted). Classroom teachers face identity-lock: exit (leaving teaching, abandoning their professional formation) is prohibitively costly, so their exit_options are identity_locked rather than mobile or constrained — they remain trapped not by external barriers but by self-conception. ELL teachers are constrained (they could leave the profession or switch to districts with different policies, but at significant career cost). Struggling readers benefit from early decoding intervention but lose meaning-centered support — they sit near symmetric (d ~0.5) except that their exit is trapped (children cannot choose schools), making effective extraction higher.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unsystematic decoding instruction in 1990s schools, insufficient identification of struggling readers) was real and the early enforcement (1980–2005) addressed a genuine gap. By 2020, the problem has substantially changed: most schools teach phonics, struggling readers are identified early, and decoding intervention is normative. Theater_ratio rising from 0.05 to 0.42 indicates growing performative maintenance: schools adopt phonics programs to satisfy accountability, not because their decoding instruction was previously absent. The constraint does not show classic mandatrophy (complete function loss with pure inertial persistence) — reading outcomes have not improved proportionally to the enforcement intensification, which suggests the founding problem's solve-value has diminished. The rising suppression_requirement (0.10 to 0.71) is the diagnostic signal: early enforcement required little suppression because teachers mostly agreed decoding needed attention; current enforcement requires intense suppression because it must actively deligitimize alternative pedagogical traditions. This is the hallmark of a constraint whose original coordination function has atrophied and persists through institutional capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_sufficiency_for_reading,
    'Is decoding sufficient for reading, or is reading fundamentally a meaning-making process that requires decoding as one mechanism among many?',
    'Longitudinal studies of reading development controlling for decoding skill, comprehension vocabulary, and background knowledge; analysis of struggling readers'' outcomes under phonics-only vs. integrated instruction; cross-linguistic studies of scripts with different phonetic transparency.',
    'If decoding is merely necessary but not sufficient, the reading''s framing is incomplete and meaning-making approaches gain structural legitimacy. If decoding is sufficient (given adequate oral language), the reading''s primacy is vindicated. Current evidence suggests a middle position: decoding is necessary and foundational, but comprehension and meaning-making are not automatically generated by decoding skill alone — they require separate instruction and language exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_sufficiency_for_reading, empirical, 'Whether decoding is the central mechanism of reading or one component in a larger constellation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (policy mandates, accountability, curriculum exclusion) or internalized (teachers have accepted the decoding-primacy framing as legitimate science)?',
    'Post-mandate trajectory: if schools that abandon phonics-first mandates quickly revert to meaning-centered practice, suppression is primarily structural. If teachers resist reversion because they have internalized the decoding-primacy framing, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — whole-language teachers carry the delegitimizing frame with them after policy changes. If structural, policy reversal would quickly decompress. This affects both the piton classification (inertial persistence) and the theater dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural enforcement vs internalized belief about legitimacy of decoding-primacy.').

omega_variable(
    science_of_reading_empirical_status,
    'Is the Science of Reading framework (phonological processing, the alphabetic principle, phoneme awareness) an empirically mature theory or a research program with unsettled questions about reading''s full architecture?',
    'Meta-analysis of reading intervention outcomes; assessment of gaps in the Science of Reading model (narrative comprehension, inference, motivation); examination of whether the model explains reading across different languages, scripts, and contexts.',
    'If mature, the reading''s framing as science-based is justified and policy enforcement is appropriate. If unsettled, the reading''s claim to scientific authority is overstated, and alternative research traditions have equal standing. Current status: the decoding component is robustly supported, but comprehension and fluency research remain contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(science_of_reading_empirical_status, empirical, 'Whether Science of Reading is a closed theory or an active research frontier.').

omega_variable(
    alternative_pedagogies_effectiveness,
    'Are meaning-centered, literature-based, and whole-language pedagogies genuinely less effective for struggling readers, or do they produce different reading outcomes (e.g., better comprehension, lower reading anxiety, higher engagement) that phonics-first metrics do not capture?',
    'Comparative studies of reading outcomes (decoding accuracy, comprehension, fluency, motivation, engagement) across instructional approaches; analysis of outcomes by reader profile (struggling, average, advanced; monolingual, multilingual; with/without dyslexia).',
    'If alternative pedagogies are genuinely less effective overall, phonics-primacy is justified. If they produce different outcome profiles (weaker decoding, stronger comprehension engagement), the choice of which outcomes to prioritize is a normative judgment, not an empirical resolution. Current evidence: phonics-first excels at decoding accuracy; integrated approaches show comparable or better comprehension and engagement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pedagogies_effectiveness, empirical, 'Whether effectiveness is properly measured by decoding metrics or requires multidimensional outcome assessment.').

omega_variable(
    kernel_contest_reduction_risk,
    'Does this reading''s institutional dominance and delegitimization of alternatives risk reducing the reading_acquisition_legitimacy kernel to a single reading, foreclosing genuine contestation about what reading fundamentally is?',
    'Observation of policy trajectory: if alternative pedagogical traditions are fully eliminated from teacher training and school practice, the kernel has collapsed into monovocal authority. If alternative traditions persist (even if subordinated), the kernel remains contested.',
    'If the kernel collapses, reading instruction becomes a matter of technical implementation, not legitimate pedagogical choice. If the kernel remains contested, future policy reversals and theoretical shifts remain possible. Current state: the kernel is heavily weighted toward phonics-primacy but alternative traditions persist in some schools, districts, and university teacher-preparation programs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_reduction_risk, conceptual, 'Risk of single-reading dominance collapsing a genuinely contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.05).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'reading_acquisition_legitimacy'. The kernel admits four readings, each with different ε, beneficiary/victim structures, and legitimacy mechanisms. All four readings are linked via network.affects_constraints: phonics_decoding_primacy influences the institutional legitimacy and resource availability for the other readings. The other three readings are separate constraint stories (not this file). The kernel contest is located in fundamental disagreement about what reading IS (decoding, meaning-making, or an integrated process) and what institutional forms this implies. This is not an observer-relative measurement ambiguity; it is a genuinely contested kernel where different parties hold incompatible definitions. Each reading instantiates a different constraint from that shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
