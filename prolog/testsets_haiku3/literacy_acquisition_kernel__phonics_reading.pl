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
 *   human_readable: Phonics-First Reading Acquisition Model
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   The phonics-reading constraint instantiates ONE READING of the contested
 *   literacy-acquisition kernel. This reading asserts that reading
 *   acquisition requires explicit, systematic instruction in phoneme-grapheme
 *   correspondence, delivered BEFORE meaningful connected-text exposure, and
 *   that decoding competence precedes and enables comprehension. The
 *   constraint is CLAIMED as tangled_rope because it genuinely solves a
 *   coordination problem (systematic decoding instruction for struggling
 *   readers, especially those with dyslexia) while simultaneously extracting
 *   from classroom teachers' instructional autonomy and from students whose
 *   intrinsic motivation is dampened by mechanics-first sequencing. The
 *   beneficiary/victim structure is structurally asymmetric: beneficiaries
 *   (struggling readers, dyslexic students, reading specialists who control
 *   the model) are deeply dependent on the constraint; victims (classroom
 *   teachers, meaning-motivated students) bear costs that are diffuse and
 *   professionally normalized. The kernel context section (Rules 1–4)
 *   documents this reading's relationships to sibling readings and the
 *   specific structural axioms that distinguish it. The authored metrics
 *   intentionally diverge from the claimed type to capture the real
 *   operational reality: high extraction on teacher autonomy, high
 *   suppression via curriculum mandates and fidelity monitoring, and rising
 *   theater_ratio as the constraint's protective mechanisms harden against
 *   alternative approaches.
 *
 * KEY AGENTS:
 *   - reading_specialists_cognitive_scientists: agenda-setter, institutional power — controls the pedagogical model, research funding, and curriculum standards that enforce phonics-first sequencing
 *   - struggling_readers_weak_phonological_awareness: beneficiary, powerless, trapped — children who fail to decode incidentally and depend on explicit systematic instruction to access text
 *   - students_with_dyslexia_spectrum: beneficiary, powerless, trapped — children whose neurology is structured to benefit from Orton-Gillingham-based phonics instruction
 *   - classroom_teachers_general_education: payer, moderate power, constrained exit — bear the cost of scripted curricula, fidelity monitoring, and loss of pedagogical judgment
 *   - students_motivated_by_meaning_over_mechanics: payer, powerless, trapped — children whose intrinsic reading motivation is dampened by mechanics-first sequencing; cannot opt out during critical reading window
 *   - whole_language_advocates: excluded, organized, constrained — teachers and researchers whose pedagogical model is marginalized from curriculum-setting and funding
 *   - balanced_literacy_advocates: excluded, organized, constrained — teachers and researchers whose integrationist approach is pressured into false-choice position between models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition Model").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, 'f50cdc4e-0cc3-42e7-8803-5295ccf21005').
narrative_ontology:cs_kernel_codification('f50cdc4e-0cc3-42e7-8803-5295ccf21005', distributed).
narrative_ontology:cs_authority_grounding('f50cdc4e-0cc3-42e7-8803-5295ccf21005', expertise).
narrative_ontology:cs_interpretation_layer_present('f50cdc4e-0cc3-42e7-8803-5295ccf21005').
narrative_ontology:cs_reading_relation('f50cdc4e-0cc3-42e7-8803-5295ccf21005', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('f50cdc4e-0cc3-42e7-8803-5295ccf21005', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f50cdc4e-0cc3-42e7-8803-5295ccf21005', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('f50cdc4e-0cc3-42e7-8803-5295ccf21005', foundational, decoding_foundational_sequentially_prior).
narrative_ontology:cs_axiom_status(decoding_foundational_sequentially_prior, holdable).
narrative_ontology:cs_axiom_grounding('f50cdc4e-0cc3-42e7-8803-5295ccf21005', decoding_foundational_sequentially_prior, empirically_contingent).
narrative_ontology:cs_axiom('f50cdc4e-0cc3-42e7-8803-5295ccf21005', foundational, phoneme_grapheme_correspondence_explicitly_teachable).
narrative_ontology:cs_axiom_status(phoneme_grapheme_correspondence_explicitly_teachable, holdable).
narrative_ontology:cs_axiom_grounding('f50cdc4e-0cc3-42e7-8803-5295ccf21005', phoneme_grapheme_correspondence_explicitly_teachable, empirically_contingent).
narrative_ontology:cs_reference_frame('f50cdc4e-0cc3-42e7-8803-5295ccf21005', skilled_reading_builds_on_systematic_phonological_decoding).
narrative_ontology:cs_drift_state('f50cdc4e-0cc3-42e7-8803-5295ccf21005', contemporary_literacy_debates, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f50cdc4e-0cc3-42e7-8803-5295ccf21005', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, struggling_readers_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_dyslexia_spectrum).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, reading_specialists_speech_pathologists).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers_general_education).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_motivated_by_meaning_over_mechanics).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, decoding_as_foundational_skill).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, phonological_awareness_predicts_reading_success).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, explicit_instruction_outperforms_immersion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional research community and publishing infrastructure that develops, validates, and advocates for the phonics-first model. Controls research funding, journal publication, conference representation, and professional certification. Justifies the model as grounded in cognitive science of reading, phonological awareness, and dyslexia intervention. Sets standards that teachers must implement to be certified as competent. Accrues funding, prestige, and career advancement through the model's dominance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_specialists_cognitive_scientists, agenda_setter,
    institutional, generational, arbitrage, global).

% Children who have not developed strong implicit phonological awareness and cannot reliably decode novel words through context or guessing. Without explicit, systematic phonics instruction, they experience repeated reading failure and develop negative self-concept around literacy. The phonics-first model gives them explicit, rule-based pathways to decode unfamiliar words. Outcome research shows their decoding and reading comprehension improve measurably under this model. They cannot opt out of reading instruction and have no voice in curriculum design.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, struggling_readers_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Children whose neurology structurally impairs implicit phonological processing and visual-word-form matching. The Orton-Gillingham lineage and explicit phonics instruction, including multisensory elements and cumulative review, was designed for dyslexic learners and remains the most empirically supported intervention for them. Under whole-language or meaning-first approaches, dyslexic students are left to discover implicit patterns they are neurologically ill-equipped to discover, leading to persistent reading failure. They are entirely dependent on pedagogical choices made by teachers and specialists.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_dyslexia_spectrum, beneficiary,
    powerless, biographical, trapped, national).

% Elementary and special-education teachers responsible for implementing reading instruction in classrooms. Required by curriculum mandates, district policies, or state standards to implement a phonics-first or structured literacy model, often with scripted lesson plans and fidelity-monitoring that constrain pedagogical judgment. Must implement with fidelity or face performance evaluations keyed to compliance metrics. Lose the ability to respond fluidly to student interest, emergent reading cues, or contextual teachable moments. Can leave teaching but cannot refuse the curriculum while employed.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers_general_education, payer,
    moderate, biographical, constrained, national).

% Early readers who are intrinsically motivated by narrative, story, character, and the social experience of shared reading. They want to read books that excite them and understand the plot. Phonics-first sequencing that delays meaningful text engagement until decoding skills are built positions reading as a mechanics exercise and can dampen early-reading motivation. These children are often capable of decoding words using context and prediction and can engage with actual stories from the beginning, but the curriculum delays meaningful texts. They have no power to opt out and cannot advocate for an alternative instructional sequence during their critical reading development window.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_motivated_by_meaning_over_mechanics, payer,
    powerless, biographical, trapped, national).

% Teachers and researchers who believe reading acquisition is a natural developmental process that emerges from meaningful engagement with texts and that explicit phonics instruction is pedagogically misguided. They believe phonics-first delays the intrinsic reward of understanding stories and that immersion in meaningful text is the optimal context for all reading skills to develop incidentally. They are largely excluded from curriculum-setting conversations in phonics-first jurisdictions, receive less research funding, and are positioned as using anecdotal or ideologically driven reasoning rather than evidence-based practice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    organized, generational, constrained, global).

% Teachers and researchers who advocate for integration of systematic phonics instruction WITH simultaneous meaningful text engagement and student choice. They propose that phonics and comprehension can develop in parallel and that both skill-building and motivation are necessary. They receive pressure to choose between phonics-first and meaning-first models for implementation fidelity, and are often dismissed by both extremes as unprincipled compromisers. Their research is less well-funded than phonics-first research in current policy environments.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, balanced_literacy_advocates, excluded,
    organized, generational, constrained, national).

% State education departments, school boards, and federal education policy authorities. They set or endorse reading curricula standards, make funding decisions, and interpret research findings for policy. They mediate between competing pedagogical models and reading research communities. They can mandate phonics-first implementation, require outcomes-based flexibility, or leave choice at the district level. They are influenced by research findings, advocacy from reading specialists, and political pressure from parent and teacher organizations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_policy_makers, observer,
    institutional, generational, analytical, national).

% Academic researchers producing empirical studies on reading acquisition, phonological awareness, phonics effectiveness, dyslexia intervention, and reading motivation. Controls journal publication, conference representation, and peer-review standards that determine what counts as legitimate evidence. Supplies the research base that legitimate pedagogical models. Internal disagreements about methodological standards, effect-size interpretation, and generalization of findings across heterogeneous populations create ongoing uncertainty about which instructional model is actually most effective for the full range of learners.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_research_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, reading_specialists_cognitive_scientists).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to systematically teach decoding when some children's phonological awareness does not develop incidentally — provides a rule-based, replicable instructional sequence that can be standardized across classrooms and tailored to struggling readers' needs.
% TRANSFER_FUNCTION: Moves pedagogical autonomy and instructional time from classroom teachers and students' self-directed reading exploration to a standardized phoneme-grapheme sequence administered by specialists and curriculum designers. Teachers implement curricula rather than design them. Reading instruction time is allocated to phonemic awareness and decoding before meaningful text engagement.
% ABSENT_VOICES: Whole-language and balanced-literacy advocates are substantially excluded from curriculum-setting and professional development in phonics-first jurisdictions. Teachers' lived experience of student motivation and engagement is deprioritized in favor of standardized fidelity measures. Students who are intrinsically motivated by narrative are not consulted on their preference for meaning-first versus mechanics-first sequencing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, classroom teachers would regain instructional autonomy and would likely rebalance phonics and meaningful text in response to student interest and classroom context. Some struggling readers and students with dyslexia would improve under continued systematic phonics (not dependent on the constraint). Others would fall further behind without it, requiring reactive intervention. The efficiency of early identification and prevention would decline. Reading outcomes would likely become more stratified by initial phonological awareness and home literacy environment.
% FOUNDING_PROBLEM: Children's reading acquisition does not always follow a natural developmental curve when explicit phonological awareness and decoding instruction are not provided; some children, particularly those with weak phonological awareness or dyslexia, require systematic, structured instruction in phoneme-grapheme correspondence to access text. Meaning-only approaches leave these children behind.
% FOUNDING_PROBLEM_CORROBORATION: Reading scientists studying phonological awareness development and dyslexia intervention, longitudinal studies tracking reading outcomes under different instructional models, and meta-analyses of phonics intervention effectiveness all attest to the founding problem's live status. Classroom teachers observe children who fail to decode without explicit instruction. Special educators note that dyslexic students reliably benefit from Orton-Gillingham-based approaches. Outside-beneficiary corroboration: cognitive neuroscience of reading shows phonological processing as a foundational neural system; independent research reviews by organizations not proposing the solution (e.g., National Institute of Child Health and Human Development in the US) affirm that some form of systematic phonics instruction produces stronger outcomes than meaning-only approaches on standardized decoding measures.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   The constraint shows rising extractiveness over the 30-year interval (0.52 → 0.68), which reflects institutional consolidation of phonics-first model in policy and research funding. Suppression remains high and relatively stable (0.58 → 0.72) because the constraint's persistence depends on actively marginalizing whole-language and balanced-literacy approaches, not on participant preference — curriculum mandates, professional development requirements, and fidelity monitoring all serve to enforce the model against teachers' desire to adapt. Theater_ratio rises moderately (0.25 → 0.42) because enforcement activity increasingly focuses on legitimacy maintenance (research citation, policy alignment, rhetoric about 'science of reading') rather than on the core coordination function (helping struggling readers). The measurement grid is uniform across all three metrics at all seven time points, satisfying the alignment rule. Extractiveness plateaus by t=25 (0.68) because the institutional constraint has reached a stable dominance level in policy; further rise would trigger explicit backlash. The temporal pattern reflects not random drift but institutional consolidation of a pedagogical model through research funding, professional credentialing, and policy mandates.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional authority (reading scientists, policy makers) and frontline practitioners (classroom teachers, students). The authority structure reads the constraint as evidence-based best practice grounded in cognitive science. The practitioner structure reads it as coercive imposition of a model that ignores individual variation in learning pathway and readiness for meaning-engaged text. This gap is not resolvable by more evidence — it is a structural gap between two different seats' experiences of the same constraint. The engine models this by computing different effective extraction values for each seat from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and dyslexic students emerge as beneficiaries because (1) they are explicitly named as the target group the constraint was designed to serve; (2) their exit options are trapped (they cannot opt out of reading instruction; they cannot self-teach phonics without external scaffolding); (3) their power is powerless (they have no voice in curriculum design); and (4) outcome data show their reading performance improves measurably under this model. Their directionality is near the full-beneficiary end of the scale (d ≈ 0.2). Classroom teachers emerge as payers because (1) they bear the cost of scripted curricula and fidelity monitoring; (2) their exit options are constrained (they can leave teaching, but they cannot refuse the curriculum while remaining employed); (3) their power is moderate (they have professional status but limited institutional leverage); and (4) their professional autonomy is systematically constrained by enforcement mechanisms. Their directionality is near the target end (d ≈ 0.75). Reading specialists emerge as beneficiary-cum-agenda-setter because (1) they directly profit from the model's dominance (funding, credentials, career prestige); (2) their exit options are arbitrage-grade (they can advocate for alternative models but have institutional leverage to resist); (3) their power is institutional (they control research funding, publication, and professional standards); (4) they set the constraint rather than submit to it. Their directionality is near the full-beneficiary end (d ≈ 0.15). No directionality overrides were necessary; the structural derivation produces accurate values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early-stage mandatrophy risk. The founding problem (some children need explicit phonics instruction to access text) is live and well-corroborated. But the constraint has hardened into a universal model applied to all children regardless of phonological-awareness baseline. The theater_ratio is rising (0.25 → 0.42), which indicates defensive activity: research citation, curriculum compliance rubrics, and professional development mandatory trainings are increasingly focused on legitimacy maintenance rather than on the core function (helping struggling readers). If this trajectory continues, the constraint will eventually enter a state where the founding problem is partially obsolete (most children receive sufficient phonics exposure in the home and in informal reading contexts to develop adequate decoding skills) but the constraint persists because reading specialists and curriculum publishers have institutional interest in its continuation. The mandatrophy watch: if theater_ratio rises above 0.5 while extractiveness on teacher autonomy remains high, reclassify to piton (the constraint becomes mostly performance and institutional inertia). Current state: the mandatrophy risk is CONDITIONAL — it depends on whether the founding problem remains live for the populations the constraint ostensibly serves. If struggling readers' outcomes plateau despite increasing scripting and surveillance, mandatrophy is likely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_applicability_vs_targeted_intervention,
    'Is systematic phonics-first instruction optimal for all children, or is it a targeted intervention for children with weak phonological awareness, with meaning-first approaches working equally well for children with strong implicit decoding skills?',
    'Longitudinal studies comparing reading outcomes for children with heterogeneous phonological-awareness baselines, randomized to phonics-first vs. meaning-first vs. balanced instruction, controlling for socioeconomic and home-literacy variables. Meta-analysis of effect-size heterogeneity across subpopulations in existing phonics intervention studies.',
    'If phonics-first is optimal universally, the constraint''s beneficiary class is all children (justifying universal implementation). If it is targeted, the constraint should be applied selectively, and classroom teachers should retain discretion to match pedagogical approach to student profile — the constraint would then be substantially less extractive on teacher autonomy. The current enforcement posture assumes universality; evidence for targeting would require reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_vs_targeted_intervention, empirical, 'Whether phonics-first is universally superior or best targeted to specific learner profiles.').

omega_variable(
    reading_motivation_vs_decoding_skill_tradeoff,
    'Does meaning-first engagement in early reading increase intrinsic reading motivation (leading to longer engagement and more cumulative reading exposure) in ways that compensate for slower initial decoding-skill development under whole-language approaches?',
    'Longitudinal studies tracking both reading achievement (decoding, comprehension, fluency) AND reading motivation/engagement (time spent reading, self-reported enjoyment, reading persistence through later grades) for children under different instructional models, with measurement extended to age 13–16 to capture cumulative effects.',
    'If meaning-first increases motivation enough to produce equivalent or superior long-term reading outcomes despite slower initial decoding development, the whole-language reading becomes less clearly inferior and the extraction on meaning-motivated students becomes harder to justify. If phonics-first children maintain reading engagement into later grades, the motivation cost evaporates. Current evidence is sparse on this tradeoff; the outcome would reshape the benchmarking between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_motivation_vs_decoding_skill_tradeoff, empirical, 'Whether the decoding speed advantage of phonics-first is offset by motivation losses.').

omega_variable(
    kernel_framing_reading_vs_skill,
    'Is reading acquisition fundamentally a SKILL-acquisition process (where decoding-first and explicit instruction are optimal) or a MEANING-MAKING AND IDENTITY process (where motivation, social engagement, and self-directed text selection are optimal)? Or is the framing itself the contestation?',
    'Philosophical analysis of how reading is constituted in pedagogy; examination of what outcomes are measured (standardized decoding tests favor skill framing; engagement, book-reading frequency, self-concept-as-reader favor meaning framing). Policy analysis of how the kernel is coded in institutional settings — do mandates require fidelity to phonics-first or permit flexibility provided reading outcomes meet benchmarks?',
    'If reading is fundamentally a skill, phonics-first is justified and the constraint''s extraction is a necessary part of the solution. If reading is fundamentally a meaning-making identity practice, phonics-first is an extractive imposition of a skill-centric framing that undermines the real work of literacy. This omega is CONCEPTUAL not empirical — it will not be resolved by more data but by shifts in how the kernel is framed within professional communities and policy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_reading_vs_skill, conceptual, 'Whether the kernel contest is resolvable empirically or reflects incompatible framings of what reading IS.').

omega_variable(
    teacher_fidelity_monitoring_as_enforcement,
    'Is the high suppression value (0.72) attributable to the pedagogical model itself (phonics-first is inherently extractive of teacher autonomy) or to the enforcement machinery (fidelity monitoring, scripted curricula, evaluation rubrics that measure compliance rather than adaptation)?',
    'Policy analysis comparing teacher autonomy and satisfaction under phonics-first models WITH high-fidelity enforcement vs. phonics-first models with lower-fidelity flexibility. Survey of classroom teachers'' experience of autonomy under different levels of implementation monitoring. Analysis of whether reduced enforcement changes the constraint''s operational character (still tangled_rope if the phonics coordination function remains) or reclassifies it.',
    'If suppression is enforcement-machinery-driven (not inherent to phonics-first pedagogy), then relaxing fidelity monitoring while maintaining systematic phonics instruction could reduce extraction on teacher autonomy. This would reshape the constraint from high-extraction tangled_rope to lower-extraction rope. If suppression is inherent to the model (scripted lessons ARE the core), then the model and the enforcement are inseparable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_fidelity_monitoring_as_enforcement, empirical, 'Whether suppression of teacher autonomy is inherent to phonics-first or driven by enforcement infrastructure.').

omega_variable(
    kernel_reading_contested_by_construction,
    'Is the literacy-acquisition kernel genuinely contested (different parties hold incompatible positions with comparable evidence and institutional legitimacy), or has the phonics-reading already foreclosed the alternatives through institutional dominance and research funding consolidation?',
    'Analysis of research-funding distribution across reading pedagogies in the last 20 years; citation patterns in influential reviews (are alternatives cited as credible positions or as superseded views?); representation of competing pedagogies in teacher-training curricula; policy-mandate patterns across jurisdictions (are all mandates phonics-first or do some districts mandate alternatives?). Interview evidence from reading specialists and teachers about whether alternatives are perceived as live options or as defeated positions.',
    'If alternatives have been foreclosed through institutional consolidation (not evidence), the constraint is not a reading of a genuinely contested kernel but a coercive imposition of one reading. This would reclassify the constraint from tangled_rope (which requires genuine coordination alongside asymmetric extraction) to snare (pure extraction disguised as coordination). If alternatives remain live (held by organized professional coalitions, represented in some institutional settings), the kernel remains contested and this reading remains one option among others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested_by_construction, empirical, 'Whether the kernel contest is genuinely open or has been institutionally closed by phonics-first dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(literacy_phonics_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(literacy_phonics_tr_t0, observed).
narrative_ontology:measurement(literacy_phonics_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(literacy_phonics_tr_t5, observed).
narrative_ontology:measurement(literacy_phonics_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(literacy_phonics_tr_t10, observed).
narrative_ontology:measurement(literacy_phonics_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(literacy_phonics_tr_t15, observed).
narrative_ontology:measurement(literacy_phonics_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(literacy_phonics_tr_t20, observed).
narrative_ontology:measurement(literacy_phonics_tr_t25, literacy_acquisition_kernel__phonics_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(literacy_phonics_tr_t25, observed).
narrative_ontology:measurement(literacy_phonics_tr_t30, literacy_acquisition_kernel__phonics_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(literacy_phonics_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(literacy_phonics_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(literacy_phonics_be_t0, observed).
narrative_ontology:measurement(literacy_phonics_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(literacy_phonics_be_t5, observed).
narrative_ontology:measurement(literacy_phonics_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(literacy_phonics_be_t10, observed).
narrative_ontology:measurement(literacy_phonics_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(literacy_phonics_be_t15, observed).
narrative_ontology:measurement(literacy_phonics_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(literacy_phonics_be_t20, observed).
narrative_ontology:measurement(literacy_phonics_be_t25, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(literacy_phonics_be_t25, observed).
narrative_ontology:measurement(literacy_phonics_be_t30, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(literacy_phonics_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(literacy_phonics_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(literacy_phonics_su_t0, observed).
narrative_ontology:measurement(literacy_phonics_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(literacy_phonics_su_t5, observed).
narrative_ontology:measurement(literacy_phonics_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(literacy_phonics_su_t10, observed).
narrative_ontology:measurement(literacy_phonics_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(literacy_phonics_su_t15, observed).
narrative_ontology:measurement(literacy_phonics_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(literacy_phonics_su_t20, observed).
narrative_ontology:measurement(literacy_phonics_su_t25, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(literacy_phonics_su_t25, observed).
narrative_ontology:measurement(literacy_phonics_su_t30, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(literacy_phonics_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy-acquisition kernel decomposes into four reading-specific constraints, each instantiating a different pedagogical commitment about how reading is acquired and how instruction should be sequenced. This reading (phonics_reading) asserts decoding-first sequencing; whole_language_reading asserts meaning-first; balanced_literacy_reading asserts parallel sequencing; structured_literacy_reading asserts extended systematic phonics with multisensory elements. The ε values differ substantially across readings because they operate under different beneficiary/victim structures: phonics_reading benefits struggling readers and extracts from meaning-motivated students and classroom teachers; whole_language_reading would benefit meaning-motivated students and extract from struggling readers who fail without explicit instruction; balanced_literacy_reading would diffuse the extraction by serving both but satisfying neither fully. These are NOT the same constraint viewed from different angles — they are different constraints grounded in the same contested kernel. The network edges record that each reading is downstream of (influenced by) the others through institutional competition for funding, curricular authority, and teacher training.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
