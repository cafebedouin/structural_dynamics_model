% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy: Integrated Phonics and Literature Exposure Requirement
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   The balanced literacy reading of the reading acquisition mechanism kernel
 *   emerged in the 1990s as an institutional compromise between phonics-first
 *   and whole-language positions. It claims that reading acquisition requires
 *   BOTH explicit phonics instruction AND authentic literature exposure in
 *   integrated practice. However, implementation studies consistently show
 *   that the 'integrated' requirement collapses in practice: phonics becomes
 *   incidental, unsystematic, or displaced by literature immersion. The
 *   constraint persists through curriculum mandates, teacher certification
 *   standards, assessment frameworks, and professional development ecosystems
 *   that treat balanced literacy as the professional consensus. Beneficiaries
 *   include curriculum publishers, teacher education programs, and literacy
 *   coaches whose professional authority and revenue streams depend on the
 *   balanced literacy framework. Victims include students with dyslexia (who
 *   need systematic phonics), students from low-income backgrounds (who lack
 *   compensatory home literacy), English learners (who need explicit code
 *   instruction), and classroom teachers (who bear implementation burden
 *   without adequate support). The theater_ratio is high (0.68) because the
 *   'integration' claim is performed in professional discourse and curriculum
 *   documents while the functional reality is whole-language-dominant
 *   practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.48).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy: Integrated Phonics and Literature Exposure Requirement").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'a760abd1-19e4-4918-ad95-a376b0c6cdb8').
narrative_ontology:cs_kernel_codification('a760abd1-19e4-4918-ad95-a376b0c6cdb8', distributed).
narrative_ontology:cs_authority_grounding('a760abd1-19e4-4918-ad95-a376b0c6cdb8', practice).
narrative_ontology:cs_interpretation_layer_present('a760abd1-19e4-4918-ad95-a376b0c6cdb8').
narrative_ontology:cs_reading_relation('a760abd1-19e4-4918-ad95-a376b0c6cdb8', reading_acquisition_mechanism__phonics_reading, influences).
narrative_ontology:cs_reading_relation('a760abd1-19e4-4918-ad95-a376b0c6cdb8', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('a760abd1-19e4-4918-ad95-a376b0c6cdb8', foundational, integration_of_skills_and_meaning_necessary).
narrative_ontology:cs_axiom_status(integration_of_skills_and_meaning_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a760abd1-19e4-4918-ad95-a376b0c6cdb8', integration_of_skills_and_meaning_necessary, instrumental).
narrative_ontology:cs_axiom('a760abd1-19e4-4918-ad95-a376b0c6cdb8', foundational, teacher_professional_judgment_supersedes_scripted_sequence).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_supersedes_scripted_sequence, holdable).
narrative_ontology:cs_axiom_grounding('a760abd1-19e4-4918-ad95-a376b0c6cdb8', teacher_professional_judgment_supersedes_scripted_sequence, deontological).
narrative_ontology:cs_axiom('a760abd1-19e4-4918-ad95-a376b0c6cdb8', secondary, authentic_literature_engagement_primary_context).
narrative_ontology:cs_axiom_status(authentic_literature_engagement_primary_context, holdable).
narrative_ontology:cs_axiom_grounding('a760abd1-19e4-4918-ad95-a376b0c6cdb8', authentic_literature_engagement_primary_context, instrumental).
narrative_ontology:cs_reference_frame('a760abd1-19e4-4918-ad95-a376b0c6cdb8', balanced_literacy_consensus_framework).
narrative_ontology:cs_drift_state('a760abd1-19e4-4918-ad95-a376b0c6cdb8', post_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a760abd1-19e4-4918-ad95-a376b0c6cdb8', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_education_programs_balanced).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, literacy_coaches_balanced).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates_institutional).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_from_low_income_backgrounds).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_learning_english).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers_implementing).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, school_districts_adopting_balanced).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and sell balanced literacy curriculum materials (e.g., Units of Study, Fountas & Pinnell). Revenue depends on district adoptions tied to balanced literacy frameworks. They shape professional development, conference programming, and literacy coaching markets. Exit is easy — they can pivot to phonics-aligned materials (some have begun doing so under pressure).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Prepare teachers using balanced literacy frameworks. Faculty hires, program accreditation, and institutional reputation are built on this approach. Changing the curriculum requires faculty buy-in, accreditation navigation, and new expertise — institutionally constrained exit. They set the agenda for what new teachers learn.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_education_programs_balanced, agenda_setter,
    institutional, generational, constrained, national).

% Provide in-service coaching on balanced literacy practices (reading workshop, guided reading, running records). Their professional identity, certification, and employment depend on the framework. Exit requires retraining and identity shift — constrained but possible.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_coaches_balanced, beneficiary,
    organized, biographical, constrained, regional).

% Longtime advocates of whole language who found institutional shelter in the balanced literacy compromise. Their professional identity is fused with the 'authentic literature' and 'teacher as decision-maker' rhetoric. Exit would mean acknowledging the compromise was a strategic retreat — identity-locked.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates_institutional, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates_institutional, agenda_setter).

% Require systematic, explicit phonics instruction to learn to read. Balanced literacy's incidental phonics fails them. They cannot exit the classroom arrangement; parents must secure private tutoring, advocacy, or legal action to access effective instruction — trapped at the individual level.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Often lack compensatory home literacy environments (books, read-alouds, language enrichment). School is their primary literacy access. Unsystematic phonics in balanced literacy disproportionately harms them. No individual exit; dependent on systemic change — trapped.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_from_low_income_backgrounds, payer,
    powerless, biographical, trapped, local).

% Need explicit code instruction to map English graphemes to phonemes. Balanced literacy's 'immersion' assumption fails them. No exit from assigned classroom; dependent on teacher knowledge and district policy — trapped.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_learning_english, payer,
    powerless, biographical, trapped, local).

% Bear the implementation burden: expected to 'integrate' phonics and literature with inadequate training, materials, and time. Evaluated on balanced literacy rubrics. Some believe in the framework; others see the harm but cannot deviate. Exit requires moving districts, grades, or careers — constrained.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers_implementing, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers_implementing, payer).

% Invest millions in balanced literacy curricula, professional development, assessment systems, and coaching. Switching costs are high (sunk costs, retraining, political resistance). They pay financially and in student outcomes. Exit is possible but politically and financially constrained.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, school_districts_adopting_balanced, payer,
    organized, biographical, constrained, regional).

% Advocate for systematic phonics based on cognitive science evidence. Structurally excluded from balanced literacy decision-making (curriculum adoption committees, teacher prep accreditation, state standards boards). They organize externally (legislation, media, parent advocacy) — mobile exit from the balanced literacy institution.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, science_of_reading_advocates, excluded,
    organized, generational, mobile, national).

% Study reading acquisition mechanisms (neural circuitry, developmental trajectories, instructional effects). Their consensus: systematic phonics is necessary; balanced literacy's 'integration' claim lacks empirical support for its implementation. They observe the constraint from outside the institutional ecosystem.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_scientists_reading, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reading instruction around a shared framework that claims to integrate decoding skills with meaningful text engagement, providing a common language for teacher preparation, curriculum design, and professional development across the literacy field.
% TRANSFER_FUNCTION: Moves instructional time, teacher attention, curriculum budgets, and student learning outcomes from systematic phonics instruction toward literature-immersion activities, while maintaining the label 'balanced'. The transfer benefits publishers, teacher educators, and coaches who control the framework; the cost falls on students who need explicit code instruction.
% ABSENT_VOICES: Students with dyslexia, low-income students, and English learners are the primary absent voices — they experience the constraint's harm but have no seat in curriculum adoption, teacher preparation, or policy decisions. Parents of struggling readers are often excluded until they organize externally. Cognitive scientists were historically excluded from the 'balanced literacy consensus' formation.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, districts would adopt systematic phonics curricula (many already exist), teacher preparation would shift to evidence-based reading science, students with dyslexia would receive appropriate instruction earlier, and the curriculum/coaching market would reorganize around phonics-aligned materials. The world rearranges because arrangements of power, money, and professional identity depend on this constraint.
% FOUNDING_PROBLEM: End the 1980s-1990s 'reading wars' between phonics-first and whole-language proponents by creating an integration framework that honored both code instruction and meaning-making, preserving teacher professional judgment against scripted programs.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (ending the reading wars through integration) is attested as DEAD by: (1) National Reading Panel (2000) — systematic review finding explicit phonics necessary; (2) cognitive neuroscience consensus (Dehaene, Seidenberg, Wolf, etc.) — reading circuitry requires systematic grapheme-phoneme mapping; (3) state legislative phonics mandates (38+ states, 2019-2024) — policy recognition that balanced literacy failed; (4) curriculum publisher pivots (Calkins, Fountas & Pinnell adding phonics supplements) — market acknowledgment. The ONLY voices attesting the problem as 'live' are balanced literacy institutional beneficiaries (publisher representatives, some teacher education faculty) — no independent corroboration exists.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48) reflects that the constraint transfers instructional time, teacher effort, and student outcomes from systematic phonics to a less effective integrated model — the extraction is the opportunity cost of inadequate decoding instruction for vulnerable students. Suppression (0.52) reflects active enforcement through curriculum adoption cycles, state standards alignment, teacher evaluation rubrics, and professional gatekeeping that marginalizes systematic phonics approaches. Theater_ratio (0.68) is high because the 'balanced' framing is maintained in official discourse while practice drifts toward whole-language. Accessibility_collapse (0.45) is moderate: alternatives (systematic phonics programs) exist and are accessible in some jurisdictions, but the balanced literacy framework dominates teacher preparation and curriculum markets. Resistance (0.40) is moderate: the 'science of reading' movement has grown but faces institutional inertia. The claimed_type is tangled_rope because there IS a genuine coordination function (integrating decoding and comprehension instruction) but it is hybridized with asymmetric extraction (the integration claim covers whole-language dominance that benefits institutional actors).
 *
 * PERSPECTIVAL GAP:
 *   From the balanced literacy advocate seat (curriculum publishers, teacher educators), the constraint is a rope: a professional consensus that solves the coordination problem of integrating skills and meaning. From the student-with-dyslexia seat, it is a snare: the integration claim masks the absence of systematic phonics they need. From the classroom teacher seat, it is a scaffold that became a piton: initially presented as transitional support for professional judgment, it hardened into a mandate they cannot easily escape. The engine computes these per-seat types from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Balanced literacy publishers and teacher education programs are structural beneficiaries: they collect curriculum revenue, professional development fees, and institutional authority from the framework's dominance (d ~ 0.15). Whole-language advocates are indirect beneficiaries: the balanced literacy compromise legitimized their rhetoric and preserved their institutional positions (d ~ 0.25). Students with dyslexia, low-income students, and English learners are primary victims: they bear the opportunity cost of unsystematic phonics with constrained exit (cannot access alternative instruction without parental advocacy/resources — d ~ 0.85). Classroom teachers are secondary victims: they implement a framework they may not believe in, with inadequate training, and face evaluation systems tied to balanced literacy rubrics (d ~ 0.65). School districts are payers: they invest in curricula, PD, and assessments locked to the framework (d ~ 0.55).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1990s): end the 'reading wars' by integrating phonics and whole-language insights. That problem is DEAD: cognitive science has established that systematic phonics is necessary and the 'integration' claim was an institutional compromise, not a scientific synthesis. Yet the arrangement persists (mandatrophy_unresolved). The coordination function (integrating decoding and comprehension) is real but has been captured by the extraction function (maintaining whole-language dominance under a phonics-inclusive label). The constraint prevents mislabeling by declaring BOTH beneficiaries (coordination) and victims (extraction) and requiring active enforcement — the engine will classify seats accordingly rather than forcing a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_pedagogy,
    'Is the claim that reading acquisition requires both explicit phonics AND authentic literature exposure a cognitive-scientific fact about human learning, or an institutional compromise doctrine that benefits identifiable actors?',
    'Convergent evidence from cognitive neuroscience (reading circuitry development), longitudinal implementation studies controlling for phonics systematicity, and historical analysis of the balanced literacy compromise formation (1990s-2000s).',
    'If cognitive-scientific fact, the constraint approaches mountain/rope; if institutional compromise, it is tangled_rope/snare with beneficiaries capturing extraction through implementation flexibility. Directly affects FSM evaluation for any mountain claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_pedagogy, conceptual, 'Whether the dual-requirement claim is a natural law of reading acquisition or a constructed institutional position').

omega_variable(
    implementation_fidelity_collapse_mechanism,
    'Does the observed collapse to whole-language in practice reflect teacher knowledge gaps, curriculum design ambiguity, ideological drift, or structural incentives in the educational ecosystem?',
    'Mixed-methods study: classroom observation of phonics systematicity in balanced literacy classrooms, teacher knowledge assessments, curriculum material analysis, and incentive structure mapping (assessment, evaluation, professional development).',
    'If teacher knowledge gaps dominate, extraction is unintended (rope/tangled_rope). If ideological drift or structural incentives dominate, the constraint is a snare with whole_language_reading as the functional extractive core. Determines whether theater_ratio reflects incompetence or design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_collapse_mechanism, empirical, 'Mechanism driving the phonics-to-whole-language implementation collapse').

omega_variable(
    committer_kernel_reading_disagreement,
    'What specific structural element do the three kernel readings (balanced_literacy, phonics, whole_language) disagree on, and does this reading''s commitment to ''integration'' logically foreclose either sibling?',
    'Structural mapping of each reading''s victim/beneficiary sets, coordination claims, and enforcement requirements against the shared referent (standing reading instruction arrangements).',
    'If balanced_literacy_reading forecloses phonics_reading (by making systematic phonics optional), the relation is ''forecloses''. If it merely competes for institutional adoption, ''coexists_with''. If it creates legitimacy pressure on whole_language_reading by appropriating its rhetoric while retaining phonics language, ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_disagreement, conceptual, 'Committer-frame structural disagreement location among kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.52) structural (curriculum mandates, assessment alignment, certification requirements) or internalized (teacher belief in balanced literacy as professional identity, ideological commitment to ''authentic'' practice)?',
    'Post-policy-change trajectory: if suppression persists after mandate removal (e.g., state phonics laws), reclassify as partially internalized. Compare suppression in mandate vs. non-mandate jurisdictions.',
    'If internalized, effective suppression is higher than structural measure suggests — teachers carry the constraint with them. Affects classification for teacher seats (identity_locked exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in balanced literacy implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1995, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2005, 0.62).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2015, 0.7).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2020, 0.68).

% Extraction over time
narrative_ontology:measurement(read_be_t1995, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2020, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1995, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2020, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_certification_literacy_standards).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, state_ela_curriculum_adoption_cycles).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, dyslexia_screening_and_intervention_mandates).

% DUAL FORMULATION NOTE:
% Part of the reading_acquisition_mechanism kernel family (3 readings). This reading (balanced_literacy) claims integration; phonics_reading claims systematic phonics as necessary and sufficient foundation; whole_language_reading claims authentic text engagement as sufficient. The three readings have different ε values (this: 0.48; phonics: ~0.15; whole_language: ~0.65), different victim sets, and different enforcement requirements. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, organized, 0.25).
constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, moderate, 0.65).
constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
