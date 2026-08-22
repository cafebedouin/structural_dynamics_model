% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Instruction (Orton-Gillingham Kernel Reading)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint instantiates the structured literacy reading of the
 *   literacy acquisition kernel. The reading asserts that reading acquisition
 *   requires explicit, systematic, cumulative instruction in five empirically
 *   validated components: phonological awareness, phonics, fluency,
 *   vocabulary, and comprehension. Designed initially for students with
 *   dyslexia (Orton-Gillingham tradition, 1930s onward), the reading extends
 *   the claim universally—that all students benefit from explicit component
 *   instruction, not just struggling readers. This constraint competes with
 *   three sibling readings of the same kernel: phonics_reading (emphasizes
 *   decoding as foundational), balanced_literacy_reading (coordinates
 *   explicit phonics with meaningful text engagement as complementary), and
 *   whole_language_reading (meaning-first, phonics incidental). The kernel
 *   contest is live: different school systems, research communities, and
 *   teacher-training programs instantiate different readings, and the contest
 *   shapes policy, curriculum adoption, and teacher credentialing. This story
 *   models the structured literacy reading specifically—its extractiveness
 *   profile (high on teacher certification burden, beneficiary for dyslexic
 *   students), its enforcement structure (standardized curriculum, assessment
 *   alignment, specialist mandates), and its contested relationship to the
 *   alternative readings.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: beneficiary of structured intervention; powerless; identity-locked to educational participation
 *   - struggling_readers: beneficiary of explicit, cumulative instruction; powerless; constrained exit
 *   - reading_specialists: beneficiary of professional legitimacy and employment; moderate power; mobile exit
 *   - general_education_teachers: payer of training burden and curriculum change; moderate power; constrained exit
 *   - school_districts: payer of implementation costs; organized power; constrained by mandate
 *   - whole_language_educators: excluded; their approaches are discredited by this reading
 *   - cognitive_neuroscience_community: observer; generates empirical legitimacy for the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Instruction (Orton-Gillingham Kernel Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '5bdaeb94-7982-4188-ad7c-6f5c8f3c809c').
narrative_ontology:cs_kernel_codification('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', formalized).
narrative_ontology:cs_authority_grounding('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', expertise).
narrative_ontology:cs_interpretation_layer_present('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c').
narrative_ontology:cs_reading_relation('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', foundational, five_component_explicit_instruction_universal).
narrative_ontology:cs_axiom_status(five_component_explicit_instruction_universal, holdable).
narrative_ontology:cs_axiom_grounding('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', five_component_explicit_instruction_universal, empirically_contingent).
narrative_ontology:cs_axiom('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', foundational, phonological_processing_neurobiological_specificity).
narrative_ontology:cs_axiom_status(phonological_processing_neurobiological_specificity, holdable).
narrative_ontology:cs_axiom_grounding('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', phonological_processing_neurobiological_specificity, empirically_contingent).
narrative_ontology:cs_reference_frame('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', orton_gillingham_cumulative_scaffolding).
narrative_ontology:cs_drift_state('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', contemporary_universal_adoption_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5bdaeb94-7982-4188-ad7c-6f5c8f3c809c', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, reading_specialists).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children identified with dyslexia or significant reading difficulties receive systematic, multi-component instruction in phonological awareness, phonics, fluency, vocabulary, and comprehension. The structured approach explicitly targets their neurological reading profile and provides cumulative scaffolding. Without this instruction, they face academic failure, shame, and exclusion from grade-level content. Their identity as 'struggling readers' is fused with their educational participation; exit is not a meaningful option.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, identity_locked, national).

% Students without diagnosed dyslexia but with significant reading lag benefit from the same explicit, systematic instruction. The constraint's universality claim extends the intervention beyond the clinical population. They receive cumulative, scaffolded instruction that addresses the five components explicitly rather than through incidental exposure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers, beneficiary,
    powerless, biographical, constrained, national).

% Special education teachers, literacy coaches, and reading intervention specialists gain professional legitimacy, employment, and research authority from the structured literacy framework. The Orton-Gillingham tradition codifies their expertise and creates a defined professional niche. They advocate for the approach and benefit from professional development revenue and prestige.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_specialists, beneficiary,
    moderate, generational, mobile, national).

% Grade-level classroom teachers who are NOT specialists face increased training burdens: many lack formal training in structured literacy instruction and must retrain or implement specialist-designed interventions. They bear the cost of professional development, curriculum change, and the administrative burden of differentiating instruction. Their existing pedagogical practices (balanced literacy, guided reading) are implicitly devalued by the structured literacy framing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Districts adopting structured literacy frameworks must fund specialist training, curriculum adoption, assessment systems aligned to the five components, and often hiring of reading specialists. The implementation cost is substantial and the mandate (especially via special education law and increasingly via state literacy mandates) leaves little discretion to defer. Pressure to show outcomes creates compliance demands even where capacity is limited.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    organized, generational, constrained, national).

% Teachers and curriculum developers who built careers on meaning-first, text-engagement-first approaches find their frameworks discredited. They are excluded from the policy conversation and their approaches are increasingly treated as ineffective or harmful. They would contest the empirical claims and advocate for integration, but their voices are structurally excluded from the literacy policy conversation once the structured literacy kernel is adopted as foundational.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_educators, excluded,
    moderate, biographical, constrained, national).

% Researchers in reading science, cognitive psychology, and neurolinguistics provide evidence for the five-component model and the specificity of phonological processing in dyslexia. They occupy an analytical seat: they generate the empirical claims that legitimize the constraint, but do not directly implement or administer the instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, cognitive_neuroscience_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, reading_specialists).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reading acquisition around the five empirically validated components (phonological awareness, phonics, fluency, vocabulary, comprehension) rather than leaving the pathway implicit. Solves the coordination problem of what to teach, in what order, with what intensity, and how to diagnose reading failure — answers that were previously fragmented or missing. Addresses the specific neurological profile of dyslexia with cumulative, scaffolded instruction rather than expecting natural acquisition.
% TRANSFER_FUNCTION: Moves professional authority and prestige from general education teachers to reading specialists; shifts instructional time away from student-selected text engagement toward explicit skill instruction; concentrates certification and credentialing requirements on teachers; transfers financial resources from general education to special education and intervention programs.
% ABSENT_VOICES: Whole-language and balanced literacy educators who built practice around meaning-first approaches are structurally excluded from the policy debate once the structured literacy kernel is adopted. Classroom teachers' concerns about implementation burden and curriculum time are not centered in the policy discussion. Student choice in reading material and engagement preferences are backgrounded by the focus on measured decoding skills.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, reading instruction would revert to approaches less standardized and less dependent on specialist certification. Districts would retain flexibility in curriculum choice, general education teachers would have reduced training mandates, and reading intervention would be less systematized. The students with dyslexia would experience the largest harm: without the explicit, cumulative, five-component framework, they would lack the structured support their neurological reading profile requires and would continue experiencing high failure rates.
% FOUNDING_PROBLEM: Children with dyslexia and significant reading difficulty fail consistently under approaches that assume reading develops naturally through text exposure. The phonological processing deficit underlying dyslexia requires explicit, systematic instruction in phoneme-grapheme correspondence and cumulative scaffolding; this instruction was not being provided in general classrooms, and affected students were being failed or labeled as unmotivated.
% FOUNDING_PROBLEM_CORROBORATION: The Orton-Gillingham tradition (founded 1930s) attests the problem is live and persisting. Contemporary cognitive neuroscience, especially fMRI studies of dyslexic reading circuits and phonological processing deficits, corroborates from outside the reading-education field. The National Reading Panel (2000) and subsequent meta-analyses in cognitive psychology confirm the five-component model's empirical support. Whole-language advocates dispute this framing (see absent_voices), arguing that the problem was misdiagnosed and that structured decoding instruction is overspecified; that contest is unresolved.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval because the structured literacy reading, once adopted at policy and curriculum level, concentrates professional authority on reading specialists and creates training/certification burdens on general education teachers. The five-component framework is presented as empirically grounded (and is supported by substantial cognitive neuroscience evidence), but implementation requires specialist oversight, standardized curriculum, and aligned assessment—all of which extract authority and resources from generalist classrooms. Suppression rises to 0.72 because the constraint's persistence depends on actively suppressing alternative framings (whole-language, balanced-literacy approaches) in policy, teacher-training, and curriculum adoption decisions. Theater ratio remains moderate (0.28) because the underlying coordination function (systematizing reading instruction for students with specific neurological profiles) is real and functional, but is increasingly supplemented by performative elements: districts adopt structured literacy frameworks partly for measured outcomes, partly because the reading has become the 'evidence-based' hegemonic position. Accessibility_collapse is high (0.71) because once the structured literacy reading becomes institutionalized in state standards and special education law, alternatives collapse for classroom teachers: they must implement it or face compliance pressure. Resistance is substantial (0.64) because educators trained in whole-language and balanced-literacy approaches actively resist, argue for integration, and defend their own frameworks—the reading does not face passive acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (students with dyslexia, reading specialists) experience the constraint as enabling coordination—explicit, cumulative instruction solves a real learning problem. The payer seats (general education teachers, school districts) experience it as enforcement of specialist practice and training demands. The excluded seat (whole-language educators) experiences it as delegitimization of their professional framework. The engine computes these divergences from the structural data (beneficiary vs. payer; power; exit options); the authored claim does not reconcile them. What looks like coordination to a reading specialist looks like extraction of classroom autonomy to a generalist teacher.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia are full beneficiaries (d near 0.0): the intervention directly addresses their neurological reading profile; they have no exit (identity-locked to educational participation); the constraint subsidizes them relative to general students. Reading specialists are partial beneficiaries (d near 0.2): they benefit from professional legitimacy, employment, and research authority; they have moderate exit (can move to other educational roles); they advocate for the reading and benefit from its adoption. General education teachers are partial targets (d near 0.65): they bear training costs, curriculum change, and implicitly devalued pedagogy; they have constrained exit (can leave teaching, but not without career cost). School districts are targets (d near 0.75): they bear implementation costs and face mandate compliance; their exit is constrained by special education law. Whole-language educators are excluded (not modeled in d directly); their exclusion IS the enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint displays tangled-rope structure: genuine coordination (systematizing reading instruction for students with neurological reading difficulties) AND asymmetric extraction (specialist credentialing, teacher retraining, authority concentration). The founding problem (dyslexia's failure under implicit-phonics approaches) is live and empirically grounded, so the constraint is not a zombie—it solves a real problem. However, the universality claim (that all students need explicit, systematic component instruction, not just those with dyslexia) is contested. If the universality claim is false or overstated, the constraint contains extraction layered onto coordination: the specialist framework extracts authority and resources beyond what addressing dyslexia requires. The mandatrophy question is whether the current implementation scope (universal adoption, general teacher training requirements) is calibrated to the founding problem (students with specific neurological reading profiles) or has expanded to extract rents from general education.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_universality_vs_specific_intervention,
    'Is the five-component structured literacy model a universal pathway for all reading acquisition, or a specialized intervention for students with dyslexia and significant phonological processing deficits?',
    'Comparative efficacy trials on typical readers (no reading difficulty) receiving structured literacy instruction vs. balanced or whole-language instruction; measurement of reading outcomes, reading motivation, and long-term literacy engagement across approaches and student profiles.',
    'If universal: the constraint''s extractiveness is justified as coordination for all readers; teacher retraining and specialist authority are legitimate infrastructure costs. If specialized: the constraint contains substantial extraction layered onto coordination; applying specialist-designed intervention to general students extracts resources and autonomy without equal benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_universality_vs_specific_intervention, empirical, 'Whether structured literacy is universal best practice or dyslexia-specific intervention.').

omega_variable(
    reading_specialist_certification_gatekeeping,
    'Do reading specialist certifications and Orton-Gillingham training requirements reflect the minimum necessary expertise to teach struggling readers, or do they function partly as professional gatekeeping that concentrates authority and excludes capable general teachers?',
    'Comparative data on outcomes from general-teacher-delivered structured literacy (with professional development) vs. specialist-delivered; analysis of labor market effects (wages, hiring, professional prestige) for reading specialists; qualitative interviews with teachers on training burden perception.',
    'If gatekeeping is minimal: specialist certification is justified as expertise protection. If gatekeeping is substantial: the extractiveness from teachers is partly professional rent-extraction rather than necessary infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specialist_certification_gatekeeping, empirical, 'Whether reading specialist credentialing requirements reflect necessary expertise or professional gatekeeping.').

omega_variable(
    suppression_of_whole_language_educators,
    'Is the active exclusion of whole-language and balanced-literacy educators from policy conversations a necessary enforcement mechanism to implement the reading (because frameworks are incompatible), or a suppression mechanism to eliminate competing professional authority?',
    'Institutional history: how did curriculum adoption decisions treat alternative approaches? Were they debated and integrated or actively suppressed? Did policy spaces close to alternative perspectives once structured literacy became dominant?',
    'If necessary enforcement: the suppression is the structural cost of the coordination. If suppression mechanism: the constraint contains intentional foreclosure of alternatives, raising the extraction classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_whole_language_educators, conceptual, 'Whether excluding alternative readings is a necessary enforcement or intentional suppression.').

omega_variable(
    reading_kernel_sibling_relationship_contest,
    'Is the structured_literacy_reading a distinct reading of the literacy_acquisition_kernel, or is it a variant of the phonics_reading with additional components layered on?',
    'Genealogical and intellectual history: trace the origins of structured literacy (Orton-Gillingham tradition) and the phonics reading (explicit phoneme-grapheme instruction). Do they represent different kernel interpretations or different levels of the same intervention?',
    'If distinct readings: the constraint family includes four siblings with different implications for teacher training and curriculum design. If variant of phonics: the constraint may collapse into phonics_reading under alternative framings, reducing the contest to two main positions (phonics/structured vs. whole-language/balanced).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_sibling_relationship_contest, conceptual, 'Whether structured literacy is a distinct kernel reading or a phonics variant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(lite_tr_t25, observed).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(lite_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(lite_be_t25, observed).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lite_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(lite_su_t25, observed).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(lite_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, resource_allocation).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_training_specialization).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, special_education_resource_allocation).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel has four structurally distinct readings: phonics_reading (decoding-first emphasis), balanced_literacy_reading (phonics + meaningful text as complementary), structured_literacy_reading (this story—five-component explicit, cumulative model), and whole_language_reading (meaning-first, incidental phonics). Each reading instantiates a different constraint with a different extractiveness profile, different beneficiary/victim structure, and different enforcement mechanisms. They are related by the shared kernel (the persisting commitment to a reading acquisition pathway) but are not variants of a single constraint. The network edges link them as a constraint family and document how policy adoption of one reading (e.g., structured literacy becoming hegemonic) affects the others (whole-language educators are excluded; phonics educators' claims are absorbed into the five-component model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
