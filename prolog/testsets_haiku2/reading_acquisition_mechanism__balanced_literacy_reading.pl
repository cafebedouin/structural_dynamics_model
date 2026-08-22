% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Balanced Literacy Reading Acquisition Model
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy is the dominant reading acquisition framework in U.S.
 *   K-2 classrooms, mandated by most state curricula and teacher training
 *   programs. It claims to synthesize explicit phonics instruction with
 *   authentic literature exposure in an integrated practice model. This
 *   reading asserts that BOTH components are necessary and that integration
 *   produces better outcomes than either alone. The constraint's operation
 *   creates an institutional compromise that neutralizes both phoneticists
 *   and whole-language advocates while failing to deliver either framework's
 *   core promise. The measurement series show rising theater ratio
 *   (performative maintenance increasing) and rising suppression requirement
 *   (more effort needed to hold the balance as evidence for phonics-first
 *   strengthens), tracking a constraint increasingly sustained by
 *   institutional inertia rather than pedagogical coherence.
 *
 * KEY AGENTS:
 *   - Institutional curricula administrators (agenda-setter): control adoption, benefit from conflict avoidance
 *   - Struggling early readers (payer): trapped in the system, bear the cost of delayed phonics instruction
 *   - Economically disadvantaged students (payer): identity-locked into school paths, lack compensatory home literacy
 *   - Dyslexic learners (payer): neurobiologically require what the framework systematically underweights
 *   - Teacher training programs (agenda-setter/beneficiary): enforce the framework, benefit from district alignment
 *   - Progressive education advocates (beneficiary): their pedagogical commitments are vindicated by the framework's literature-first language
 *   - Phonics research advocates (excluded): their evidence is subordinated by the balance frame
 *   - Reading scientists (observer): increasingly document that phonics-first models outperform balance on early decoding outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Model").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'b3669b22-191b-4db2-8341-144105040cbb').
narrative_ontology:cs_kernel_codification('b3669b22-191b-4db2-8341-144105040cbb', distributed).
narrative_ontology:cs_authority_grounding('b3669b22-191b-4db2-8341-144105040cbb', extraction).
narrative_ontology:cs_interpretation_layer_present('b3669b22-191b-4db2-8341-144105040cbb').
narrative_ontology:cs_reading_relation('b3669b22-191b-4db2-8341-144105040cbb', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3669b22-191b-4db2-8341-144105040cbb', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('b3669b22-191b-4db2-8341-144105040cbb', foundational, integration_of_phonics_and_literature_is_necessary).
narrative_ontology:cs_axiom_status(integration_of_phonics_and_literature_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b3669b22-191b-4db2-8341-144105040cbb', integration_of_phonics_and_literature_is_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b3669b22-191b-4db2-8341-144105040cbb', secondary, no_pedagogical_component_has_absolute_priority).
narrative_ontology:cs_axiom_status(no_pedagogical_component_has_absolute_priority, holdable).
narrative_ontology:cs_axiom_grounding('b3669b22-191b-4db2-8341-144105040cbb', no_pedagogical_component_has_absolute_priority, conventional).
narrative_ontology:cs_reference_frame('b3669b22-191b-4db2-8341-144105040cbb', pedagogy_integrates_both_phonics_and_authenticity).
narrative_ontology:cs_drift_state('b3669b22-191b-4db2-8341-144105040cbb', contemporary_cognitive_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b3669b22-191b-4db2-8341-144105040cbb', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, institutional_curricula_administrators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, progressive_education_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, economically_disadvantaged_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, mandate, and defend the balanced literacy framework across districts and schools. They justify it as research-aligned and developmentally appropriate. They benefit by avoiding the political conflict of choosing exclusively between phonics-first (conservative constituencies prefer phonics) and whole-language (progressive constituencies prefer authenticity). The framework's institutional legitimacy rests on the claim that it synthesizes both traditions, satisfying competing stakeholder groups simultaneously. They control curriculum adoption, textbook selection, teacher training alignment, and professional development spending. When evidence mounts for phonics-first methods, they defend the framework by citing 'implementation issues' rather than framework problems and invest in professional development reinforcing the balance commitment.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, institutional_curricula_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Are assigned to public schools where balanced literacy is the dominant framework. They experience a curriculum architecture in which whole-language-style meaning-first literature engagement occupies the front-loaded instructional time (shared reading, guided reading from authentic texts, independent reading of leveled trade books), while explicit phonics instruction is delayed and typically delivered in lower-intensity small-group 'stations' after the main lesson. By the time systematic phonics arrives (often months in), they have accumulated reading failure, shame, and the internalized belief that 'reading is hard for me.' Decodable texts introduced in late phonics intervention feel incoherent after immersion in authentic literature. Their exit options are nil: they cannot choose private school (resource constraint), cannot be classified as special education without additional disability marker (reading failure alone does not qualify), and cannot opt out of school attendance. They pay in lost reading acquisition time, compounded achievement gaps, and identity damage (internalized 'not a reader' self-concept).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_early_readers, payer,
    powerless, biographical, trapped, local).

% Depend almost entirely on school-based literacy instruction; home literacy environment is sparse (few books, limited print exposure, parents with lower literacy levels themselves). Balanced literacy's implicit phonological awareness instruction assumes prior exposure to rhyming, phonemic patterns, and sound play that these students lack. The framework expects students to 'discover' phonics patterns through word study and independent reading, but students without rich print exposure at home cannot make these discoveries. They are trapped by institutional assignment (public school attendance mandate) and identity-locked by the equation 'I am a student' = 'I learn through the school curriculum' (their primary literacy access is school, so the school path IS their learning identity). The cost is early reading failure, remedial placement, and the accumulation of achievement gaps that widen every year, reshaping their self-concept and educational trajectory into a 'struggling reader' identity.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, economically_disadvantaged_students, payer,
    powerless, biographical, identity_locked, local).

% Have neurobiological differences that make implicit phonemic awareness discovery impossible; they require explicit, sequential, structured phonics instruction delivered with high intensity (40-60 min/day minimum), high frequency (daily), and high systematicity (one skill at a time, cumulative review). Balanced literacy's typical architecture (45-60 min whole-group authentic literature engagement + 15-20 min optional phonics stations, 4 days/week, without specified sequencing) does not meet their neurobiological needs. They fail to acquire decoding skills in the typical timeline and are diagnosed with dyslexia, leading to special education classification. Exit from balanced literacy occurs through this diagnosis pathway, but the exit is framed as moving to 'special education services' rather than 'better instruction,' carrying stigma and often reduced access to grade-level curriculum. They pay in years of reading failure, special education placement, stigma, and reduced academic trajectory even after receiving appropriate intervention.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_learners, payer,
    powerless, biographical, trapped, local).

% License teachers in balanced literacy frameworks as the authoritative reading instruction method. They embed balanced literacy pedagogy in elementary teacher certification programs, requiring coursework in literacy workshops, guided reading, running records, and literature circles, with minimal or no systematic phonics course (often 2-3 hours embedded in a broader literacy survey course, vs. 30+ hours of balanced literacy methodology). They benefit by maintaining curricular coherence with district mandates (which ensures their graduates are employable and their reputation remains aligned with field practice) and by avoiding costly retraining if they shifted their curriculum to phonics-first emphasis. They enforce the framework through certification standards, course content, field placement supervision, and clinical observation rubrics that value 'authentic engagement' and 'student-centered discovery.' Their secondary benefit is institutional alignment: if they trained teachers for balanced literacy and districts continue to adopt balanced literacy, the training programs are validated. If they shifted to phonics-first training and districts did not, their graduates would be misaligned with hiring schools. They are constrained (could adopt phonics-first training, but it would require curriculum overhaul and the graduates might not be hireable).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs, beneficiary).

% Advocate for child-centered, constructivist, meaning-first reading instruction. They emphasize student agency, authentic literature engagement, and learning through discovery rather than direct instruction. They benefit substantially because balanced literacy's explicit inclusion of 'authentic literature' and 'authentic engagement' vindicates their theoretical commitments to child-centered learning, even as the framework's phonics component theoretically contradicts their whole-language philosophy. The framework represents institutional capture of their conceptual territory—the language of 'balanced literacy' now carries the legitimacy of official adoption and teacher certification, giving their pedagogy institutional endorsement it would not have under whole-language labeling alone. They occupy the 'advocated-by' and 'vindicated-by' seat without bearing the costs when implementation underweights phonics instruction in favor of the authentic engagement they prefer. Their exit options are mobile: they can promote alternatives, shift advocacy to different movements, or argue that balanced literacy should be renamed to emphasize the literature component more.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, progressive_education_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue that systematic, explicit, intensive phonics instruction must be foundational and should take precedence over or sequence before authentic literature exposure in early reading instruction. They cite cognitive science evidence showing phonics-first models produce faster decoding acquisition, particularly for struggling and dyslexic learners. They are excluded from curriculum authority in most balanced literacy jurisdictions; they can publish research and advocate professionally, but their findings are not integrated into teacher training or curriculum adoption. Their exclusion is maintained by the balanced literacy frame, which neutralizes their specific priority claim ('phonics first') by incorporating phonics as 'one component' of a framework that privileges balance and equal weight over prioritization. They would object that the framework defeats the evidence by refusing to accept the research finding that explicit phonics is more foundational than meaning-first engagement. Their mobility (they can publish, present, advocate) means they are not trapped, but they are structurally excluded from the curriculum authority that would implement their preferred reading. Recent years show their influence growing in state policy and parent advocacy groups, creating pressure on the balanced literacy framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_research_advocates, excluded,
    organized, generational, mobile, national).

% Conduct empirical studies of reading acquisition mechanisms using cognitive neuroscience, behavioral experiments, and longitudinal measurement. They observe the constraint's operation and measure whether the balanced literacy model produces the reading skill outcomes it claims and whether the integration thesis is supported by evidence. Their role is not to enforce or benefit but to measure. Recent years show reading scientists increasingly finding that phonics-first models produce faster early decoding acquisition, particularly for economically disadvantaged and dyslexic learners, contradicting the balanced literacy integration thesis that both components are equally necessary and must be integrated. The evidence divergence (science shows phonics-first advantage; framework assumes integration necessity) is the key measurement the observer seat registers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, institutional_curricula_administrators).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two pedagogical traditions (explicit phonics instruction and whole-language authentic literature exposure) into a single coherent curriculum framework, resolving institutional conflict over reading methodology by embedding both approaches in a unified model. This serves school administrative coherence—a single framework adopted districtwide avoids the need to choose between warring camps and satisfies constituencies across the political spectrum (conservatives prefer phonics, progressives prefer authenticity).
% TRANSFER_FUNCTION: Moves instructional time and pedagogical authority away from explicit, systematic phonics instruction (which is demanding of teacher expertise and requires structured sequencing) toward integrated meaning-making activities and authentic literature engagement (which are less demanding of specialized teacher training and fit the constructivist, student-led pedagogical model preferred by teacher educators). The transfer is from phonemic decoding precision and systematicity to authentic engagement and student discovery learning. This benefits teacher training programs (lower retraining costs) and progressive educators (their pedagogy is institutionalized) at the cost of struggling, disadvantaged, and dyslexic learners (who need the phonics they do not receive).
% ABSENT_VOICES: Dyslexic learners have no seat at curriculum policy tables; their neurobiological constraint (requirement for intensive explicit sequential phonics) is not represented in balance-seeking frameworks that treat reading processes as uniform across learners. Struggling readers and economically disadvantaged students are present only through aggregate achievement data, not as named participants in curriculum authority. Phonics research advocates are present in academic literature but structurally subordinated from curriculum authority—their evidence is treated as 'one component' rather than heard as a foundational-priority finding. Parents of children who have failed under balanced literacy are increasingly present in advocacy groups but remain outside institutional curriculum decision-making until recent years.
% DISAPPEARANCE_RATIONALE: If the balanced literacy constraint vanished and districts reverted to phonics-first or whole-language-only frameworks, reading instruction would reorganize around a single dominant logic and evidence base. Phonics-first adoption would shift resources to structured decodable texts, intensive daily phonics stations with explicit sequencing, and teacher training focused on systematic phonics delivery; early reading outcomes would shift significantly (faster early decoding for all groups, particularly struggling/disadvantaged/dyslexic learners). Whole-language reversion would remove phonics priority entirely and restore meaning-first authentic engagement as the sole framework; reading outcomes would follow the evidence pattern of the 1980s-90s (strong engagement and comprehension outcomes for advantaged learners, continued decoding failure for struggling/disadvantaged learners). Most significantly, the institutional equilibrium that balanced literacy maintains would collapse—teacher training programs would need to retrain, textbooks would need to be re-adopted, curricula would need to be rewritten. The political conflict between phonics and authenticity camps that balanced literacy suppressed would re-emerge in overt institutional struggle. Reading outcomes for the payer groups would diverge visibly from outcomes under balanced literacy.
% FOUNDING_PROBLEM: Reading pedagogy in U.S. schools was fractured by a decades-long conflict between two camps: (1) phonics traditionalists who emphasized explicit instruction in grapheme-phoneme correspondence and systematic decoding, viewing reading as a foundational skill requiring structured teaching; (2) whole-language progressives who emphasized meaning-making and authentic literature engagement, viewing reading as naturally acquired through immersion in meaningful texts. Both camps had research support; schools and teachers were torn between competing mandates and parent demands. Balanced literacy was designed by academics and teacher educators in the 1990s-2000s to end the pedagogical wars by synthesizing both traditions in one framework: explicit phonics instruction for building decoding skill, AND authentic literature engagement for motivation and meaning-making, both embedded in a single integrated daily practice.
% FOUNDING_PROBLEM_CORROBORATION: The framework's proponents (teacher training institutions, progressive education organizations, some school administrators) attest the pedagogical war problem remains live and balanced literacy successfully contains the conflict by honoring both traditions. Reading scientists and phonics-first advocates increasingly attest that the founding problem has been substantially resolved by cognitive science evidence strongly supporting phonics-first methods; they view balanced literacy's persistence as institutional inertia and political compromise rather than reasoned integration based on evidence. Longitudinal studies of early reading outcomes (Scarborough et al., 2020; Seidenberg, 2017; meta-analyses in Reading Research Quarterly) corroborate the phonics advocates' assessment: districts adopting explicit phonics-first models with high daily intensity show faster early decoding gains, particularly for economically disadvantaged and dyslexic students, contradicting the balanced literacy thesis that integration is necessary. Outside the benefiting parties: cognitive neuroscience research on reading (Dehaene's 'Reading in the Brain,' Seidenberg's 'Language at the Speed of Sight') strongly supports phonics primacy; early-adopter districts reporting phonics-first outcomes (see Florida Reading Initiative, Structured Literacy adoption studies) show achievement gains that validate phonics priority; special education literature shows that dyslexic students receiving intensive structured phonics (the phonics-reading model) outperform peers under balanced literacy on decoding measures.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.58) is moderate-to-high because the framework systematically underweights explicit phonics instruction (the component most effective for struggling readers) in favor of meaning-first literature engagement. This benefits curriculum administrators (avoids political choice) and teacher training programs (fewer retraining costs) while extracting reading acquisition time and skill-building opportunity from struggling and dyslexic learners. Suppression is high (0.62) and rising because the framework is maintained partly by institutional mandate and partly by excluding phonics-priority evidence from curriculum authority. The theater ratio (0.51, near the Piton threshold) indicates the framework's performative maintenance is approaching its core function—increasing share of activity is devoted to defending the integration claim rather than delivering reading skill. The measurement trajectory shows theater rising gradually (framework increasingly theatrically maintained) while extractiveness plateaus (extraction is stable once the compromise is locked in), a classic Piton sign. Accessibility collapse is low (0.42) because alternatives (phonics-first, whole-language-only) remain intellectually coherent and evidence-supported in academic discourse; they are excluded from institutional adoption, not from possibility space. Resistance is high (0.68) because reading scientists, phonics advocates, and parents of struggling readers mount increasing research and advocacy pressure against the framework.
 *
 * PERSPECTIVAL GAP:
 *   The administrator/teacher-training seat experiences balanced literacy as a solved coordination problem—it unified warring camps and created a coherent curriculum. The struggling-reader and dyslexic-learner seats experience it as extraction masked by integration language. The phonics-research seat experiences it as institutional suppression of evidence. The whole-language-advocate seat experiences it as vindication (their literature-focus is institutionalized) without bearing the cost (their de-emphasis of phonics is blamed on implementation, not framework). The reading scientist seat sees the data diverging from the framework's integration thesis: phonics-first models show faster early decoding, particularly for disadvantaged/dyslexic learners. The engine computes these per-seat divergences from the structural data (who benefits, who pays, who is excluded, what evidence they can access). The authored claim (tangled rope = hybrid coordination+extraction) sits between the administrator's-seat reading (rope/coordination) and the victim-seat reading (snare/pure extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional curricula administrators and teacher training programs are beneficiaries: they collect the coordination benefit (unified framework, avoided political choice, institutional coherence). Their exit options are arbitrage and constrained respectively—they can maintain the framework or adopt alternatives, but alternatives are costly. Their directionality is low (~0.2-0.3), producing low effective extraction. Struggling readers, economically disadvantaged students, and dyslexic learners are victims: they bear the cost of underweighted phonics through delayed reading acquisition, compounded achievement gaps, and identity damage. Their exit options are trapped or identity-locked—they cannot opt out of school assignment or reframe their student identity. Their directionality is high (~0.75-0.85), producing high effective extraction. Progressive education advocates are beneficiaries with mobile exit (they can promote alternatives); directionality ~0.1. Phonics advocates are excluded (no directionality value; they are not seated in the constraint). The teacher training institutional seat has secondary beneficiary role because they benefit from institutional alignment even as they are agenda-setters of the framework. This dual role reflects their position: they enforce and benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pedagogical war between phonics and whole-language) has been substantially resolved by cognitive science evidence favoring phonics-first methods. Yet balanced literacy persists and is increasingly theatrically maintained as the evidence diverges from its integration thesis. This is a mandatrophy candidate: the institutional commitment to the framework outlives its functional justification. The framework now serves primarily to avoid reopening the political choice (administrator benefit) rather than to deliver the best reading outcomes (original function). The rising theater ratio (0.42 → 0.51) tracks the constraint's shift from functional coordination to performative maintenance. The classification as tangled rope rather than piton reflects that extraction is still substantial and active enforcement is required, but the theater trajectory and suppression rise suggest piton-ward drift. The mismatch between founding_problem_status (dead—phonics evidence is now dominant) and world_rearranges verdict (yes—districts adopting phonics-first show different outcomes) is the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_necessity_assumption,
    'Is the simultaneous presence of explicit phonics AND authentic literature exposure necessary for optimal reading acquisition, or would explicit phonics followed by literature exposure (phonics-first sequence) produce equivalent or better outcomes?',
    'Randomized controlled trials comparing: (A) balanced literacy (integrated both components throughout), (B) phonics-first followed by literature exposure, (C) pure whole-language, measured on early decoding speed, comprehension, and long-term reading trajectory, stratified by baseline risk (struggling, disadvantaged, dyslexic).',
    'If (B) produces faster early decoding (particularly for struggling/dyslexic learners) without sacrificing comprehension or engagement, then integration is not necessary—the framework''s core axiom is falsified, and the constraint should reclassify toward snare (pure extraction of phonics priority from struggling learners). If (A) and (B) show equivalent outcomes, the framework is defensible as a valid pedagogical choice. If (A) outperforms, the integration claim is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_necessity_assumption, empirical, 'Whether integrated instruction or sequenced instruction (phonics-first) produces better early reading outcomes.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of phonics-first advocacy in curriculum decisions primarily structural (institutional barriers, administrative mandate) or primarily internalized (teachers'' and administrators'' genuine belief in balance, not forced compliance)?',
    'Post-mandate trajectory analysis: if jurisdiction removes balanced literacy mandate and teachers/administrators remain in the framework, suppression is substantially internalized. If they shift when mandate lifts, suppression is primarily structural. Interview studies of curriculum decision-makers: do they report mandate-driven choices (structural) or pedagogical conviction (internalized)?',
    'If structural: removing the mandate could enable rapid institutional shift to phonics-first where evidence supports it. If internalized: even mandate removal would not shift practice because teachers and administrators have fused the framework with their professional identity; professional retraining would be required. If both: suppression is durable and requires both policy change and identity work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of phonics alternatives is structural mandate or internalized pedagogical conviction.').

omega_variable(
    evidence_lag_structural_inevitability,
    'Does the gap between cognitive science evidence (phonics-first superiority for early decoding) and curriculum adoption (persisting balanced literacy) represent structural institutional lag (normal slow diffusion of evidence), or does it represent strategic institutional resistance (deliberate suppression of evidence to protect the framework)?',
    'Historical diffusion timeline analysis: compare lag time for balanced literacy adoption (1990s-2000s, rapid) vs. lag time for phonics-first re-adoption given recent evidence (2015-present, slower). If phonics adoption accelerates when forced by policy mandates or parent pressure, lag is structural (normal institutional friction). If adoption remains slow even under policy/parent pressure, resistance is strategic. Institutional actor interviews: do curriculum decision-makers report that they have NOT adopted phonics-first despite knowing the evidence (strategic), or that they have not yet learned/integrated the evidence (lag)? Textbook adoption cycles: do new textbooks incorporate phonics-first recommendations, or do they continue to feature balanced literacy despite publisher access to recent evidence?',
    'If structural lag: the framework will gradually shift as evidence propagates normally. If strategic resistance: the framework will persist until explicit policy intervention or institutional crisis forces change. The distinction affects remediation strategy: structural lag requires faster knowledge dissemination; strategic resistance requires institutional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_lag_structural_inevitability, conceptual, 'Whether the persistence of balanced literacy despite phonics evidence is normal institutional lag or deliberate strategic suppression.').

omega_variable(
    kernel_reading_specificity,
    'Is balanced literacy a defensible reading of the reading-acquisition-mechanism kernel, or does it represent a compromised institutional position that the kernel itself does NOT support?',
    'The kernel is the empirical question: what mechanisms are necessary and sufficient for reading acquisition to occur? If evidence shows (A) explicit phonics is necessary for early decoding in struggling learners, (B) authentic literature exposure is necessary for engagement and comprehension, but (C) they need not be integrated (phonics-first sequence works), then balanced literacy is a false reading of the kernel—the kernel says ''do phonics first, then literature,'' not ''integrate both throughout.'' A kernel-coherent reading must describe a true mechanism; a reading that misrepresents the mechanism is a false reading, not a legitimate rival.',
    'If balanced literacy misreads the kernel, then cs_structure.axiom_overriding would route the axiom ''integration_is_necessary'' to ''foreclosed'' rather than ''holdable,'' and the engine would compute forced rejection of the reading rather than coexistence. If it correctly reads the kernel, the reading remains holdable. This affects whether balanced literacy is one legitimate perspective on a contested kernel (coexists_with siblings) or a false summit mistaking institutional compromise for empirical truth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_specificity, conceptual, 'Whether balanced literacy is a coherent reading of the reading-acquisition kernel or an institutional compromise misrepresenting the kernel''s empirical content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ram_blr_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ram_blr_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(ram_blr_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(ram_blr_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ram_blr_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ram_blr_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement(ram_blr_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.51).

% Extraction over time
narrative_ontology:measurement(ram_blr_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ram_blr_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ram_blr_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ram_blr_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(ram_blr_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ram_blr_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(ram_blr_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ram_blr_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ram_blr_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(ram_blr_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(ram_blr_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(ram_blr_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(ram_blr_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(ram_blr_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, dyslexia_remediation_framework__intensive_phonics).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, early_literacy_screening_systems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'reading_acquisition_mechanism.' Sibling readings: phonics_reading (explicit phonics is foundational; literature comes after) and whole_language_reading (authentic literature exposure is sufficient; decoding emerges implicitly). Balanced literacy coexists with both siblings in contemporary educational discourse; it neither forecloses nor is foreclosed by them, though increasing evidence pressure from cognitive science research influences the viability of the integration axiom. See omegas for the evidence-lag and reading-fidelity uncertainties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
