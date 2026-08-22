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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Acquisition Kernel (Orton-Gillingham Tradition)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The structured literacy reading instantiates one authoritative
 *   interpretation of how reading is acquired: explicit, systematic,
 *   cumulative instruction across phonological awareness, phonics, fluency,
 *   vocabulary, and comprehension, grounded in the Orton-Gillingham tradition
 *   and optimized for students with dyslexia but claimed as universally
 *   applicable. This reading sits in contest with whole-language
 *   (meaning-first, natural acquisition), phonics-only (decoding-first), and
 *   balanced-literacy (both phonics and meaning equally from the start)
 *   readings of the same kernel—the fundamental question of how reading is
 *   learned. The constraint's operation extracts from general education
 *   teachers (training, inflexibility, reduced pedagogical autonomy) while
 *   reducing reading failure for students with dyslexia. The claim/metric gap
 *   is intentional: this reading is CLAIMED as rope (coordination solving a
 *   real problem) while the authored metrics capture the extractive dimension
 *   (teacher labor cost, suppression of alternative pedagogies). The engine
 *   measures whether these diverge and how.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: powerless beneficiaries whose literacy outcomes improve under structured instruction; identity-locked (exit means returning to failure-laden generic instruction)
 *   - general_education_teachers: moderate-power payers bearing training cost, labor intensification, and reduced pedagogical autonomy; constrained exit (legal duty to implement FAPE)
 *   - dyslexia_research_community: institutional agenda-setters maintaining the constraint through publication, training, advocacy; agenda-setting power and professional incentives aligned with constraint persistence
 *   - special_education_administrators: institutional agenda-setters enforcing the constraint through RTI/IEP frameworks; incentivized by litigation risk reduction and empirically-grounded service delivery
 *   - whole-language educators: excluded from authoritative conversation; would dispute the phonics-first and skills-sequencing premises
 *   - literacy_science_researchers: observers generating evidence for/against the constraint's core claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.41).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Acquisition Kernel (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '95bfc945-d8f4-420c-b44c-7f783f10dd37').
narrative_ontology:cs_kernel_codification('95bfc945-d8f4-420c-b44c-7f783f10dd37', fixed_text).
narrative_ontology:cs_authority_grounding('95bfc945-d8f4-420c-b44c-7f783f10dd37', expertise).
narrative_ontology:cs_interpretation_layer_present('95bfc945-d8f4-420c-b44c-7f783f10dd37').
narrative_ontology:cs_reading_relation('95bfc945-d8f4-420c-b44c-7f783f10dd37', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('95bfc945-d8f4-420c-b44c-7f783f10dd37', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('95bfc945-d8f4-420c-b44c-7f783f10dd37', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('95bfc945-d8f4-420c-b44c-7f783f10dd37', foundational, component_skills_require_explicit_instruction).
narrative_ontology:cs_axiom_status(component_skills_require_explicit_instruction, holdable).
narrative_ontology:cs_axiom_grounding('95bfc945-d8f4-420c-b44c-7f783f10dd37', component_skills_require_explicit_instruction, empirically_contingent).
narrative_ontology:cs_axiom('95bfc945-d8f4-420c-b44c-7f783f10dd37', foundational, cumulative_sequencing_necessary_for_disabled_readers).
narrative_ontology:cs_axiom_status(cumulative_sequencing_necessary_for_disabled_readers, holdable).
narrative_ontology:cs_axiom_grounding('95bfc945-d8f4-420c-b44c-7f783f10dd37', cumulative_sequencing_necessary_for_disabled_readers, empirically_contingent).
narrative_ontology:cs_reference_frame('95bfc945-d8f4-420c-b44c-7f783f10dd37', component_skill_reading_science).
narrative_ontology:cs_drift_state('95bfc945-d8f4-420c-b44c-7f783f10dd37', contemporary_neurodiverse_pedagogy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('95bfc945-d8f4-420c-b44c-7f783f10dd37', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_reading_disabilities).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_administrators).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, alphabetic_principle).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, cumulative_skill_building).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, explicit_systematic_instruction_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive reading instruction explicitly designed for phonological and orthographic processing differences. The structured, cumulative sequence (phonological awareness → phonics → fluency → vocabulary → comprehension) provides scaffolding that generic classroom instruction does not. Reading outcomes improve measurably compared to undifferentiated instruction. Exit would mean returning to classroom reading without specialized design, with sharply elevated failure risk.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, identity_locked, national).

% Benefit from the same component-skills scaffolding as dyslexic students. The constraint reduces their reading failure rate and improves fluency outcomes. They are enrolled in the same structured literacy programs through RTI (Response to Intervention) and special education service delivery.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_reading_disabilities, beneficiary,
    powerless, biographical, identity_locked, national).

% Must undergo specialized professional development (100–300 hours depending on certification level) to implement structured literacy. They adopt new curricula, sequencing, and assessment protocols aligned with the five-component model. Their pedagogical autonomy is reduced—they cannot flexibly pivot to literature-based or meaning-first approaches during instruction. Legal duties (FAPE, state mandates) and district adoption decisions constrain their ability to exit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Advocate that reading emerges naturally from meaningful, engaging text and that explicit phonics instruction dampens reading motivation and authentic engagement. They are structurally excluded from the authoritative discourse on reading instruction in most state standards, RTI frameworks, and dyslexia policy. Their research (reading engagement, comprehension motivation) is marginalized in policy priority compared to component-skills research.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_educators, excluded,
    organized, generational, mobile, national).

% Sets standards for evidence-based reading instruction for dyslexic learners through research publication, professional conferences (International Dyslexia Association, etc.), training certification programs (Orton-Gillingham Academy, etc.), and advocacy for state-level recognition and policy. They maintain and refine the structured literacy framework, generate the evidence base, and certify trainers. They collect publication impact, professional prestige, conference attendance, and training revenue from the constraint's operation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_research_community, agenda_setter,
    institutional, generational, analytical, global).

% Implement and enforce the constraint through RTI tiered-intervention models, IEP development and service delivery specifications, and special education curriculum adoption. They benefit by deploying an empirically-validated instructional model that reduces reading failure rates (lowering grade retention, behavior referrals, and litigation risk). They also manage the institutional burden—coordinating teacher training, monitoring fidelity, maintaining certification.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_administrators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, special_education_administrators, beneficiary).

% Conduct empirical studies on reading acquisition mechanisms, phonological processing, orthographic learning, fluency development, and reading intervention efficacy. They generate evidence supporting or challenging the structured literacy model's claims via randomized controlled trials, meta-analyses, neuroimaging studies, and longitudinal outcomes research.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, literacy_science_researchers, observer,
    institutional, civilizational, analytical, global).

% Mandate or strongly encourage structured literacy through state reading standards, dyslexia identification and service delivery rules, and teacher certification requirements. They enforce the constraint by tying funding, accountability, and compliance to structured literacy adoption.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_education_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_research_community).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to teach reading to learners whose neurocognitive profile does not permit incidental skill acquisition from exposure alone: provides a unified, teachable, sequenced instructional framework (phonological awareness → phonics → fluency → vocabulary → comprehension) that can be applied consistently across classrooms and reduces variance in outcomes attributed to teacher training and pedagogical preference.
% TRANSFER_FUNCTION: Moves instructional labor, professional development time, and material selection work from flexible, teacher-autonomous classroom reading instruction to highly specified, scaffolded, component-skills instruction. Teachers and districts invest training and curriculum resources; students with dyslexia and reading disabilities receive targeted intervention; general-education students are exposed to the same instructional sequence (claimed as universally beneficial, contested as engagement-reducing).
% ABSENT_VOICES: Whole-language and reading-engagement researchers are structurally excluded. They would argue that reading emerges naturally from meaningful engagement with authentic texts, that explicit phonics instruction is unnecessary and potentially harmful to reading motivation, and that the structured literacy model's focus on component skills reduces reading to decoding mechanics and overshadows comprehension, critical thinking, and love of reading. They produce counter-evidence emphasizing reader engagement and authentic-text research, but this evidence is marginalized in policy and state-standard development that prioritize component-skills research.
% DISAPPEARANCE_RATIONALE: If structured literacy instruction disappeared overnight, students with dyslexia would return to classroom reading instruction without systematic phonological and phonetic scaffolding. Reading failure rates in that population would spike, grade retention and special education referrals would increase, and the population's literacy outcomes would degrade measurably compared to the structured baseline. General education teachers would recover instructional flexibility and autonomy. Districts would no longer bear the cost of specialized training and curriculum adoption. The reading instruction ecosystem would reorganize around pedagogical preferences (whole-language, balanced, phonics-only, literature-based) without a unified mandate.
% FOUNDING_PROBLEM: Children with dyslexia and related language-based learning differences fail to acquire reading fluency under standard classroom reading instruction because their phonological awareness and phonetic processing do not permit incidental skill acquisition from text exposure alone. The Orton-Gillingham tradition (1930s onward) and subsequent dyslexia neuroscience established that explicit, systematic, cumulative instruction in phonological awareness, phonics, fluency, vocabulary, and comprehension is the evidence-based response to this neurological reading difference.
% FOUNDING_PROBLEM_CORROBORATION: The dyslexia research community, special education field, and literacy science researchers (neuroimaging, reading-science studies) attest the founding problem is live: reading failure rates for students with dyslexia remain substantially elevated in non-structured classrooms; neuroimaging confirms differential phonological processing; structured literacy produces larger effect sizes for disabled readers than for typical readers. Whole-language educators dispute the framing—they argue that the 'problem' is over-medicalization of normal reading variation and that authentic, meaning-driven instruction works for all learners given flexible pacing—but their counter-evidence comes from engagement and motivation data, not from reading-outcome data for the dyslexic population specifically. No corroboration from outside the benefiting parties for the claim that the founding problem is SOLVED or RESOLVED; the constraint's persistence is actively maintained by the research and special education communities.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.62) because the constraint imposes real teacher training costs and reduces pedagogical flexibility, but these costs are justified by measurable reading gains in the target population and reduced litigation/failure risk for districts. The constraint rises from 0.48 (early adoption, contained to special ed) to 0.62 (plateaus around universal-applicability claims, state mandates), then stabilizes—the trajectory suggests institutional entrenchment and saturation of adoption, not accumulating rent. Suppression is moderate (0.41) because whole-language and balanced-literacy pedagogies are not legally prohibited, but they are marginalized in policy, standards, and professional training. Teachers can theoretically resist (mobile exit, professional communities), but certification requirements, RTI mandates, and district adoption of structured literacy curriculum raise the cost. Theater is low (0.28)—the instructional sequence and fidelity monitoring are substantive, not performative. Accessibility collapse is high (0.72) because once the neurological reading-difference frame is accepted, alternatives appear either neglectful (failing to address dyslexia) or ideologically attached (whole-language educators' insistence on meaning-first despite evidence). Resistance is substantial (0.58) from whole-language advocates, reading-engagement researchers, and teachers experiencing the labor burden—they produce counter-evidence and maintain rival pedagogical communities.
 *
 * PERSPECTIVAL GAP:
 *   The claim/metric gap deserves explicit attention. This reading CLAIMS the constraint is rope—genuine coordination solving a problem neither party could solve alone. The authored metrics (extractiveness 0.62, suppression 0.41, theater 0.28) describe the constraint as substantially extractive, moderately suppressive, and maintaining modest performative elements. The engine will compute whether the claim matches the metrics or diverges; divergence does NOT invalidate the story—it is exactly the measurement the corpus exists to take. A claimed rope that computes as tangled_rope is how institutional capture is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for students with dyslexia is near zero (full beneficiaries): the constraint directly reduces reading failure, shame, and downstream special education referrals for them. No overrides needed—the derivation chain (beneficiary declaration + identity-locked exit + disabled population institutional framing) naturally produces low d. Directionality for general education teachers is near one (full targets): they pay the cost (training, labor, reduced autonomy), have constrained exit (legal duty, certification requirements, district mandates), and do not benefit directly (though they benefit diffusely if their classrooms improve overall). No override needed. Directionality for the dyslexia research community should reflect that they are agenda-setters (d near one) but also institutional beneficiaries (publication, career advancement, funding, professional authority), making them symmetric or slightly beneficiary-sided. This suggests no override, or a modest downward override (d = 0.4–0.5) to reflect the dual position. The specification provided does not request this detail, so the default derivation suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading failure in dyslexia) is LIVE and genuine; the constraint addresses it. The disappearance verdict is world_rearranges (if structured literacy disappeared, reading outcomes for dyslexic students would degrade and special education referrals would spike). The constraint does NOT show mandatrophy—the founding problem persists, the constraint's function persists, and the two remain tightly coupled. However, the universality claim (structured literacy works for ALL students) may obscure a narrower truth (it is optimized for disabled readers and applies to typical readers with either neutral or engagement-reducing effects). This is not mandatrophy (founding problem obsolescence) but rather false-summit risk—the universality framing masks the asymmetric benefit structure. An omega addresses this; it does not trigger mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the structured literacy reading a foundational distinct reading of the literacy acquisition kernel, or is it a methodological variant of the phonics reading?',
    'Examine whether the structured literacy reading''s distinctive claim (cumulative, systematic, scaffolded instruction across five components: phonological awareness, phonics, fluency, vocabulary, comprehension) is logically separable from the phonics reading''s core claim (explicit, systematic phoneme-grapheme instruction precedes connected text). If yes, they are distinct readings; if the structured literacy reading merely adds sequencing and components without changing the foundational claim, it is a variant.',
    'If distinct: the structured literacy reading has its own constraint story with its own ε and beneficiary structure. If variant: the phonics_reading constraint should absorb this reading''s data, and the pedagogical distinction (cumulative sequencing) becomes a secondary implementation detail rather than a kernel-level reading difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether structured literacy is a distinct kernel reading or a methodological variant of phonics reading.').

omega_variable(
    teacher_labor_extraction_ambiguity,
    'Is the extraction measured on general education teachers (0.62) a legitimate cost of implementing evidence-based intervention for a high-need population, or is it institutional capture wherein the dyslexia research and special education administration communities extract teacher labor to sustain their authority and funding streams?',
    'Comparative institutional analysis: examine whether the teacher training and implementation burden (a) tracks the actual complexity of the instructional model (fidelity requirements genuinely demand 100+ hours of training) or (b) is inflated to create dependency on professional certification and program licensing. If teacher outcomes improve and reading gains for dyslexic students persist under lower-intensity training, burden was inflated.',
    'If legitimate cost: the constraint is rope-like coordination with asymmetric distribution of burden (justified by benefit concentration on the high-need population). If inflated: the constraint is tangled_rope sliding toward snare, with the agenda-setting institutions extracting from general education teachers to sustain market dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_labor_extraction_ambiguity, empirical, 'Whether teacher training requirements track instructional complexity or institutional rent-extraction.').

omega_variable(
    universal_applicability_claim_validity,
    'Does the Orton-Gillingham tradition''s claim to universal applicability (structured literacy is beneficial for ALL students, not only those with dyslexia) accurately describe the constraint, or does the constraint''s rationale and benefit concentration make it a disability-specific intervention falsely universalized?',
    'Meta-analysis of reading outcomes under structured literacy instruction for typical-reading students vs. dyslexic students: if effect sizes are equal and classroom engagement/motivation metrics do not diverge, the universality claim is supported. If effect sizes for typical readers equal those under other methods and engagement declines, the universality claim is cover—the constraint is optimized for disabled readers, and its application to typical readers is either neutral or net-negative despite the inclusive framing.',
    'If truly universal: the constraint coordinates reading instruction across the full range of learners, and the cost to general education teachers is justified as the price of including disabled students in one classroom. If not universal: the constraint redistributes instruction toward disabled students (legitimate) by normalizing special-needs pedagogy as default (possibly overstandardizing), and the universality claim becomes a false-summit mask for asymmetric intervention design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_claim_validity, empirical, 'Whether structured literacy provides equal benefit across all reading profiles or is specifically optimized for disabled readers.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.41) structural—imposed by certification requirements, state mandates, and RTI frameworks that legally obligate teacher compliance—or is it partly internalized wherein teachers adopt the structured literacy frame as pedagogically correct and suppress their own pedagogical doubts without external enforcement?',
    'Post-mandate suppression trajectory: if suppression persists after legal/certification requirements are removed, or if suppression rises when requirements tighten, the internalization mechanism is operating. If suppression drops when requirements are relaxed, it is primarily structural.',
    'If internalized: the constraint''s effective suppression is higher than the structural measure suggests; teachers carry the suppression into new contexts. If structural: the suppression can be reversed by removing mandates, and the constraint''s persistence depends on continued institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of alternative reading pedagogies is structural or internalized in teachers'' professional identity.').

omega_variable(
    reading_kernel_axiom_overriding_drift,
    'Has the empirical axiom ''component_skills_require_explicit_instruction'' been substantially overridden by evidence that contradicts or qualifies it (e.g., neuroscience showing implicit phonological learning under certain conditions, or longitudinal studies showing reader motivation and engagement predict reading outcomes as strongly as component-skills instruction)?',
    'Systematic review of recent neuroscience and reading-science literature (2015–2026): does the evidence base support the axiom unambiguously, or has counter-evidence accumulated such that the axiom''s status is now contested (overridden in some subdisciplines, holdable in others)?',
    'If the axiom is overridden: the structured literacy reading''s foundational claim is empirically challenged, and the drift_state magnitude (currently ''substantial'') should be revised. The reading''s authority to adjudicate literacy policy weakens. If the axiom remains holdable: the structured literacy reading retains empirical warrant despite competing evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_axiom_overriding_drift, empirical, 'Whether the empirical axioms grounding the structured literacy reading have been substantially challenged by counter-evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(lite_tr_t20, observed).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(lite_tr_t25, observed).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(lite_tr_t30, observed).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(lite_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(lite_be_t20, observed).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(lite_be_t25, observed).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(lite_be_t30, observed).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(lite_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(lite_su_t20, observed).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(lite_su_t25, observed).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(lite_su_t30, observed).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(lite_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of literacy_acquisition_kernel, decomposed from a contested kernel into four separate stories per ε-invariance: structured_literacy_reading (this file) claims ε=0.62 and Rope with asymmetric teacher burden; phonics_reading claims ε≈0.45 and Rope with simpler implementation; balanced_literacy_reading claims ε≈0.35 and Rope with lower institutional entrenchment; whole_language_reading claims ε≈0.20 and Rope with different exclusion structure (excludes structured literacy researchers rather than whole-language educators). Sibling readings are linked via network.affects_constraints. The structured reading influences (but does not foreclose) balanced literacy by creating institutional pressure toward phonics primacy; it coexists with phonics reading (both are live policy positions in different jurisdictions); it forecloses whole_language reading at the level of evidence hierarchies (one cannot simultaneously hold 'component skills must be explicitly taught' and 'reading emerges naturally without explicit instruction' in a single framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
