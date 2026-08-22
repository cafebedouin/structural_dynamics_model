% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Phonics-First Reading Instruction Constraint
 *   domain: educational/cognitive/literacy
 *
 * SUMMARY:
 *   The phonics-reading constraint instantiates one reading of the contested
 *   literacy-acquisition kernel. It asserts that reading development requires
 *   explicit, systematic instruction in phoneme-grapheme correspondence as a
 *   prerequisite to connected text exposure, grounded in cognitive science
 *   research on phonological awareness and decoding prerequisites. This
 *   reading directly contests the whole-language reading (which asserts
 *   phonics develops naturally from meaningful text), coexists with
 *   balanced-literacy reading (which combines both approaches), and
 *   influences structured-literacy reading (which shares the phonics-first
 *   premise but adds cumulative structure and multisensory methods). The
 *   reading's empirical support comes from experimental evidence and
 *   intervention trials; its contestation comes from educators reporting
 *   sustained success with alternative methods and from scholars questioning
 *   whether the phonological-deficit model overgeneralizes a causal finding
 *   from one subpopulation to all learners.
 *
 * KEY AGENTS:
 *   - students_weak_phonological_awareness: beneficiaries, powerless, trapped — depend on institutional provision of phonics instruction
 *   - struggling_readers: beneficiaries, powerless, identity-locked — access contingent on deficit labeling within the reading intervention system
 *   - dyslexic_students: beneficiaries, powerless, constrained — receive evidence-based instruction designed for their neurological profile
 *   - teachers_professional_judgment: payers, moderate power, constrained — implement mandated curricula with reduced autonomy and pacing flexibility
 *   - students_contextual_learners: payers, powerless, identity-locked — experience deferred meaningful text engagement, contrary to their learning profile; labeled as low-achieving
 *   - district_administrators: agenda-setters, institutional power, mobile exit — implement mandates in response to research consensus and policy pressure
 *   - cognitive_science_researchers: beneficiaries, institutional power, analytical — their empirical findings vindicating the constraint's core premises
 *   - reading_recovery_educators and whole_language_educators: excluded, moderate power, constrained exit — advocates for alternative approaches structurally prevented from setting curriculum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.71).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Phonics-First Reading Instruction Constraint").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational/cognitive/literacy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36').
narrative_ontology:cs_kernel_codification('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', fixed_text).
narrative_ontology:cs_authority_grounding('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', expertise).
narrative_ontology:cs_interpretation_layer_present('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36').
narrative_ontology:cs_reading_relation('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', foundational, phoneme_grapheme_prerequisite_decoding).
narrative_ontology:cs_axiom_status(phoneme_grapheme_prerequisite_decoding, holdable).
narrative_ontology:cs_axiom_grounding('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', phoneme_grapheme_prerequisite_decoding, empirically_contingent).
narrative_ontology:cs_axiom('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', foundational, phonological_awareness_causal_reading_development).
narrative_ontology:cs_axiom_status(phonological_awareness_causal_reading_development, holdable).
narrative_ontology:cs_axiom_grounding('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', phonological_awareness_causal_reading_development, empirically_contingent).
narrative_ontology:cs_reference_frame('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', phonological_awareness_decoding_causal_model).
narrative_ontology:cs_drift_state('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', contemporary_alternative_pathway_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5abe47ee-1de8-48e8-98fa-d9d1fd7c2f36', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, dyslexic_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_contextual_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, cognitive_science_researchers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, phonological_awareness_causal_reading).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, decoding_prerequisite_comprehension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement mandated phonics-first curricula with fidelity measures that constrain lesson design flexibility. Teachers report reduced autonomy in pacing, sequencing, and responsiveness to individual student needs. Exit options are limited: working outside the system (private tutoring, homeschooling) or accepting the loss of professional judgment within institutional settings.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, constrained, national).

% Receive systematic, explicit instruction in phoneme-grapheme correspondence designed specifically to remediate phonological deficits. This targeted instruction has been shown to accelerate decoding skill development and reduce reading failure rates for students with weak underlying phonological awareness. They are dependent on institutional provision and cannot opt out.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Access explicit, sequential decoding instruction that builds from sound-symbol correspondence to connected text. The constraint's benefit is contingent: it works well for students whose reading difficulty stems from decoding deficits, but the one-size-fit-all implementation may miss students whose struggles are comprehension-based or motivation-based. Identity lock occurs through the labeling system (diagnosed reader, intervention student) that ties instructional access to deficit categories.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, identity_locked, national).

% Structured phonics instruction (especially Orton-Gillingham-based approaches) provides evidence-based, multisensory teaching specifically designed for their neurological profile. The constraint ensures they receive instruction aligned with their learning needs rather than being left to struggle with generic approaches. Exit is constrained: access to specialized instruction is contingent on institutional provision.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, dyslexic_students, beneficiary,
    powerless, biographical, constrained, national).

% Experience reading instruction that defers meaningful text engagement until phonics mastery, contrary to their learning profile. These students acquire literacy more readily through connected text, semantic context, and meaning-making. The constraint's requirement to delay connected text reduces their motivation and may produce false negatives (students appearing to have reading difficulties when they actually learn differently). Identity lock: labeled as low-achieving or remedial within the system even though their reading profile diverges from the phonics-first model rather than failing it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_contextual_learners, payer,
    powerless, biographical, identity_locked, national).

% Implement phonics-first mandates in response to legislative pressure, research consensus in cognitive science, and accountability demands. They set curriculum scope and sequence, enforce fidelity monitoring, and allocate resources (professional development, scripted materials). Their exit is relatively open: they can choose alternative frameworks, but do so at political risk.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, district_administrators, agenda_setter,
    institutional, generational, mobile, national).

% The constraint vindicates their empirical findings on phonological awareness, decoding prerequisite, and explicit instruction. Research funding, publication venues, and professional authority flow through the phonics-first framework. Their position is sustained by institutional alignment with the constraint.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, cognitive_science_researchers, beneficiary,
    institutional, generational, analytical, global).

% Advocate for balanced approaches that combine phonics with meaning-making and are systematically excluded from curriculum committees and policy formation. Their expertise in context-responsive reading intervention is treated as secondary to the phonics framework. They would argue that the constraint's rigidity produces false positives (students labeled reading-disabled who succeed with alternative approaches) but lack institutional voice to challenge the mandate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_recovery_educators, excluded,
    moderate, biographical, constrained, national).

% Were influential in prior literacy instruction policy but are now excluded from primary authority positions under phonics-first mandates. They hold that meaningful text engagement produces natural phonological awareness and decoding skills without explicit instruction, and that explicit phonics undermines motivation and comprehension focus. Their alternative is structurally prevented from operating in mandated settings.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_educators, excluded,
    moderate, biographical, constrained, national).

% Observe outcomes (reading proficiency rates, standardized test scores) and evaluate the constraint's effectiveness. Parents with resources can exit through private schooling or tutoring; parents without resources are dependent on institutional provision. Their observation shapes policy pressure but they do not set the constraint.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_outcome_focused, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, district_administrators).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes reading instruction around a teachable, systematically sequenced skill progression (phoneme-grapheme correspondence → blending → fluency → comprehension) so reading intervention is efficient, replicable across teachers and schools, and targeted to remediable decoding deficits. Solves the coordination problem of heterogeneous teaching approaches producing inconsistent outcomes.
% TRANSFER_FUNCTION: Transfers autonomy and professional judgment from teachers to curriculum, research, and administration; transfers learning pathway from student-initiated meaning-making to system-directed skill-building; transfers resource allocation toward phonics-structured materials and phonological assessment tools.
% ABSENT_VOICES: Students who learn to read through context-based approaches without explicit phonics instruction; teachers with sustained success using balanced or whole-language methods; neurodiversity advocates arguing that the phonics prerequisite model privileges one neurotype (auditory-sequential processing) over others. These voices are structurally absent from policy formation because the constraint's empirical validation framework only hears evidence of phonics efficacy, not evidence of alternative pathways.
% DISAPPEARANCE_RATIONALE: If explicit phonics-first instruction disappeared, reading pedagogy would revert to varied, context-dependent approaches; some students (particularly those with weak phonological awareness) would face higher reading failure rates; teachers would recover instructional autonomy and responsiveness; some struggling readers would succeed under alternative methods and be recategorized as non-disabled. The literacy outcomes distribution would shift: fewer uniform success, more heterogeneity, different profile of who struggles.
% FOUNDING_PROBLEM: Reading instruction in the 1980s–2000s was dominated by whole-language approaches that did not explicitly teach phoneme-grapheme correspondence; cognitive science research (particularly longitudinal studies of dyslexia and intervention trials) showed phonological awareness and explicit decoding instruction were causally necessary for reading development, especially for students with phonological deficits; whole-language practice produced persistent reading failure for these students because the needed skill-building was left implicit.
% FOUNDING_PROBLEM_CORROBORATION: The constraint is corroborated by cognitive science research consensus (meta-analyses by the National Institute of Child Health and Human Development, longitudinal twin studies, intervention trials showing phonics efficacy for at-risk students) from researchers outside the education profession. It is contested by reading educators who report success with balanced and whole-language approaches and by neurodiversity scholars who argue the phonological-deficit model pathologizes normal variation. Legislative mandates in multiple states cite the research consensus as justification; parent outcomes data show both improved and unchanged reading proficiency depending on the comparison group.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.45→0.68) as phonics-first mandates move from pilot implementation to system-wide adoption, accumulating cost to teacher autonomy and students whose learning profiles diverge from the phonological-processing model. The plateau at t=20-25 reflects policy stabilization: extractiveness asymptotes as alternative approaches are fully marginalized and institutional alignment crystallizes. Suppression tracks similarly (0.55→0.71): enforcement infrastructure intensifies (fidelity monitoring, standardized assessments, professional development mandates) to maintain the constraint against teacher resistance and student-outcome heterogeneity. Theater rises but stays moderate (0.25→0.42): the functional core (phoneme-grapheme instruction for decoding-deficient students) is real, but an increasing share of enforcement activity suppresses alternative pathways and defends the phonics-first sequencing prerequisite rather than addressing decoding failure itself. Accessibility collapse is moderate (0.65): students and teachers with resources can partially exit through private schools, homeschooling, or tutoring; students and teachers without resources are trapped. Resistance is moderate (0.58): cognitive science consensus sustains institutional support, but educator testimony and neurodiversity advocacy mount persistent, though marginalized, challenge. The measurement series is aligned to one shared time grid across all three metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of students with weak phonological awareness and dyslexic students, the constraint operates as genuine coordination: it provides remediable, evidence-based instruction specifically designed for their neurological profile, reducing reading failure and enabling literacy access they would not achieve under generic approaches. From the seat of teachers, the constraint operates as extraction: their professional judgment is transferred to curriculum, pacing is scripted, responsiveness to individual need is constrained, and fidelity monitoring enforces compliance. From the seat of contextual learners, the constraint appears as mislabeling extraction: they are sorted into intervention categories (low-achieving, at-risk) because their learning profile diverges from the phonological-processing model, not because they cannot read. From the seat of cognitive science researchers, the constraint vindicates their empirical findings and sustains their institutional authority. From the seat of reading-recovery and whole-language educators, the constraint is suppression: their alternative expertise is marginalized, their success stories are unheard, their methodologies are legally foreclosed in many jurisdictions. The engine computes per-seat classifications from these structural differences; no single type fits all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint benefits students with weak phonological awareness (d→0.1, beneficiary end) who receive instruction specifically designed for their deficit: decoding acceleration, reduced reading failure, specific remediable skill-building. It benefits struggling readers contingent on their deficit being phonological (d→0.2); students whose struggles are comprehension-based, motivation-based, or neurodivergent experience it as extractive (d→0.7). It extracts from teachers' professional judgment (d→0.8, target end): they implement scripted curricula with fidelity monitoring, reduced pacing flexibility, reduced responsiveness to individual student need. It extracts from students whose learning profile favors context-based acquisition (d→0.75, target end): they experience delayed meaningful text engagement, false-negative labeling, reduced motivation. District administrators are near symmetric (d→0.5): they coordinate literacy outcomes and research alignment but absorb political cost from alternative-framework advocates. The engine derives directionality from beneficiary/victim declarations and exit options; the perspectival gap is real: from the beneficiary seat (phonologically-deficit student) the constraint is coordination; from the payer seat (constrained teacher) it is enforced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (whole-language approaches failing phonologically-deficient students) is live in cognitive science but contested in education practice and policy. Some jurisdictions report improved reading proficiency under phonics-first mandates; others report unchanged or worse outcomes depending on implementation quality and student population composition. The constraint's mandate has not yet outlived its function in the research frame (phonological deficits remain remediable via explicit instruction), but the one-size-fit-all enforcement produces false positives (contextual learners labeled deficient) and false negatives (students succeeding under alternative methods classified as failures). Mandatrophy detection hinges on the mismatch: founding_problem_status=contested + disappearance_verdict=world_rearranges + theater_ratio rising toward 0.5 suggests the constraint is acquiring performative maintenance (defending phonics-first sequencing as orthodoxy) alongside its functional core (remediating decoding deficits). If theater continues rising and alternative approaches demonstrate equal or better outcomes for non-phonologically-deficient students, reclassification to piton becomes likely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonological_deficit_universality,
    'Is phonological processing deficit the causal basis of reading failure for all struggling readers, or only for a subset?',
    'Longitudinal studies of reading failure etiology disaggregating by cognitive profile; intervention trials comparing phonics-first outcomes across students with and without phonological deficits; neuroimaging studies of reading-disabled populations showing heterogeneity of underlying mechanisms.',
    'If phonological deficits cause only a subset of reading failure, the one-size-phonics-first constraint overgeneralizes and produces false positives (students labeled deficient who would succeed under alternative methods); the constraint should reclassify from tangled_rope to snare (extracting on false labeling). If phonological deficits are causal for all, the constraint''s universality is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonological_deficit_universality, empirical, 'Whether the phonological-deficit model explains reading failure heterogeneously or universally.').

omega_variable(
    decoding_comprehension_sequencing,
    'Does explicit phoneme-grapheme instruction need to precede connected text exposure, or can the two develop in parallel or even with meaning-making preceding systematic phonics?',
    'Comparative intervention studies (phonics-first vs. balanced vs. whole-language) on reading proficiency and comprehension outcomes; longitudinal data on children who read successfully without explicit phonics instruction; studies of bilingual and multilingual acquisition pathways.',
    'If sequencing is flexible, the constraint''s mandatory prerequisite is extraction (forcing a specific pathway); if sequencing must be decoding-first, the constraint''s coordination function is validated. If pathways are student-specific (phonologically-deficient students need sequencing, contextual learners benefit from parallelism), the constraint requires personalization rather than universal application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_comprehension_sequencing, empirical, 'Whether decoding-first sequencing is necessary or student-profile-dependent.').

omega_variable(
    teacher_professional_judgment_cost,
    'What is the actual cost of scripted phonics curricula to teacher responsiveness, student engagement, and instructional quality compared to the benefit of increased standardization?',
    'Classroom observation studies comparing fidelity-enforced phonics implementation to teacher-directed instruction on engagement, pacing responsiveness, and outcome quality; teacher surveys on autonomy experience; student motivation and attendance data.',
    'If scripting costs outweigh standardization benefits, the constraint reclassifies toward pure extraction (snare); if benefits exceed costs, the tangled_rope classification holds. If costs differ by teacher expertise and student population, the constraint requires graduated application rather than universal mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_professional_judgment_cost, empirical, 'Whether standardization cost to teacher responsiveness exceeds the benefit of uniform implementation.').

omega_variable(
    alternative_pathway_validation,
    'Are there educationally valid reading acquisition pathways that do not center phoneme-grapheme prerequisite, and if so, do they produce comparable outcomes for all student populations?',
    'Naturalistic studies of children learning to read via whole-language, bilingual, multilingual, or apprenticeship-based methods; intervention trials with students previously classified as reading-disabled under phonics-first frameworks, measured under alternative frameworks.',
    'If valid alternative pathways exist for some or all students, the constraint''s universality is overreach; reclassify toward snare (false-positive extraction via inappropriate labeling). If all pathways require phonological awareness and decoding prerequisite (even if reached by different instructional routes), the constraint''s core premise holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pathway_validation, empirical, 'Whether alternative reading acquisition pathways exist and what populations they serve effectively.').

omega_variable(
    reading_recovery_educator_outcome_gap,
    'Do students working with Reading Recovery educators (who combine phonics with context and meaning-making) achieve reading proficiency comparable to or better than students in phonics-first-only frameworks?',
    'Randomized controlled trials comparing Reading Recovery outcomes to phonics-first outcomes on equivalent student populations; longitudinal follow-up data; meta-analysis of Reading Recovery efficacy literature.',
    'If Reading Recovery produces comparable or superior outcomes, the constraint''s suppression of that approach is pure extraction (snare); if phonics-first produces superior outcomes, the tangled_rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_recovery_educator_outcome_gap, empirical, 'Whether suppressed alternative approaches produce comparable or superior reading outcomes.').

omega_variable(
    neurodiversity_phonological_assumption,
    'Does the phonological-processing route to reading represent one valid pathway among several, or is it the single necessary route for all neurotypes?',
    'Neurodiversity-informed research on reading acquisition across neurotypes (autism, ADHD, dyslexia, visual-spatial processing strengths); studies of bilingual and multilingual reading development; classroom data disaggregated by neurotype showing heterogeneous response to phonics-first instruction.',
    'If phonological processing is neurodiversity-specific rather than universal, the constraint privileges one neurotype and extracts from others via inappropriate labeling; reclassify toward snare. If phonological processing is universal (though experienced differently), the constraint''s universality is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neurodiversity_phonological_assumption, conceptual, 'Whether the phonological-deficit model applies to neurodiversity or overgeneralizes from specific neurotypes.').

omega_variable(
    whole_language_core_premise_foreclosure,
    'Does the phonics-reading reading logically foreclose the whole-language-reading reading within any single framework, or do they coexist as different educational philosophies?',
    'Philosophical analysis of the core premises: does asserting ''explicit phonics is necessary'' logically entail ''whole-language approaches are wrong,'' or is the disagreement empirical (about what works) rather than logical (about what must be true)?',
    'If foreclosure is logical, the reading_relation should be ''forecloses'' (stronger claim); if disagreement is empirical/philosophical, the relation should be ''coexists_with'' (weaker claim, more common). This determines whether the kernel contest can be resolved empirically or is fundamentally incommensurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(whole_language_core_premise_foreclosure, conceptual, 'Whether the phonics-reading and whole-language-reading core premises are logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(lite_tr_t20, observed).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__phonics_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(lite_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(lite_be_t20, observed).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(lite_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(lite_su_t20, observed).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(lite_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.18).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy-acquisition kernel decomposes into four structurally distinct constraint stories instantiating different readings. This story (phonics-reading) asserts decoding-first sequencing and phonological-deficit remediation as core; whole-language-reading asserts natural development through meaningful text; balanced-literacy-reading asserts complementary phonics and meaning-making; structured-literacy-reading asserts systematic cumulative instruction with multisensory methods. Each reading has distinct ε values (phonics-reading: 0.68, reflecting asymmetric teacher extraction; whole-language-reading: lower extraction, benefits meaning-making learners; balanced-literacy-reading: moderate extraction, compromises autonomy for both approaches; structured-literacy-reading: higher extraction, more intensive intervention demands). The readings network via affects_constraints to enable contamination-propagation analysis: if empirical evidence undermines the phonological-deficit universality assumption (omega variable phonological_deficit_universality), the phonics-reading ε increases toward 1.0 (pure extraction via false labeling) and downstream readings recalibrate. The referent for all readings is the standing arrangement of reading instruction under contest (what is taught, how, to whom, with what justification), assessed by each reading's own lights, not by a common external standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
