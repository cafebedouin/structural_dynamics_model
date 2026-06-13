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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Framework
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Whole language reading instruction asserts that reading acquisition is a
 *   natural process parallel to spoken language acquisition: children exposed
 *   to authentic, meaningful texts in print-rich environments will develop
 *   decoding skills implicitly through exposure, without explicit
 *   phoneme-grapheme instruction. The approach maximizes teacher autonomy,
 *   eliminates scripted curriculum, and prioritizes comprehension and
 *   engagement. However, empirical evidence shows this framework imposes
 *   substantial costs on struggling readers, dyslexic students, and
 *   second-language learners—populations whose implicit learning pathways do
 *   not align with the implicit-exposure model. This constraint is ONE
 *   READING of a contested kernel about reading acquisition mechanisms; the
 *   sibling readings (phonics-first, balanced literacy) offer structurally
 *   different accounts. The claim/metric gap is intentional: whole language
 *   is CLAIMED as a rope (genuine coordination around authentic reading)
 *   while the authored metrics describe substantially extractive
 *   operation—the constraint's implicit assumptions extract costs from
 *   neurologically atypical and linguistically novice readers while
 *   distributing benefits to teachers and to researchers who study reading
 *   failure. The engine computes this divergence; the JSON does not reconcile
 *   it.
 *
 * KEY AGENTS:
 *   - classroom_teachers: agenda setters who implement and defend whole language; benefit from autonomy and reduced curriculum burden
 *   - struggling_early_readers: payers trapped in classrooms; accumulate reading deficit and internalize failure as personal inability
 *   - dyslexic_students: payers identity-locked to neurologically atypical reading profiles; whole language leaves them furthest behind
 *   - second_language_learners: payers constrained to classrooms; lack the implicit phonological foundation whole language assumes
 *   - special_education_specialists: pay remedial costs; partially benefit from teacher autonomy but inherit escalated caseloads
 *   - reading_researchers (explicit instruction camp): benefit from population of intervention-responsive struggling readers
 *   - balanced_literacy_advocates: excluded from curriculum decisions in whole-language-dominated spaces
 *   - education_policymakers: analytical observers; see aggregate reading proficiency data and remedial costs but face institutional resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.67).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.72).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Framework").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '97aec4a2-4552-4f32-8409-dbbddd68f4f3').
narrative_ontology:cs_kernel_codification('97aec4a2-4552-4f32-8409-dbbddd68f4f3', distributed).
narrative_ontology:cs_authority_grounding('97aec4a2-4552-4f32-8409-dbbddd68f4f3', practice).
narrative_ontology:cs_interpretation_layer_present('97aec4a2-4552-4f32-8409-dbbddd68f4f3').
narrative_ontology:cs_reading_relation('97aec4a2-4552-4f32-8409-dbbddd68f4f3', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('97aec4a2-4552-4f32-8409-dbbddd68f4f3', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('97aec4a2-4552-4f32-8409-dbbddd68f4f3', foundational, implicit_learning_hypothesis_sufficient).
narrative_ontology:cs_axiom_status(implicit_learning_hypothesis_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('97aec4a2-4552-4f32-8409-dbbddd68f4f3', implicit_learning_hypothesis_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('97aec4a2-4552-4f32-8409-dbbddd68f4f3', foundational, authentic_text_engagement_primary).
narrative_ontology:cs_axiom_status(authentic_text_engagement_primary, holdable).
narrative_ontology:cs_axiom_grounding('97aec4a2-4552-4f32-8409-dbbddd68f4f3', authentic_text_engagement_primary, deontological).
narrative_ontology:cs_reference_frame('97aec4a2-4552-4f32-8409-dbbddd68f4f3', implicit_learning_through_authentic_engagement).
narrative_ontology:cs_drift_state('97aec4a2-4552-4f32-8409-dbbddd68f4f3', contemporary_reading_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97aec4a2-4552-4f32-8409-dbbddd68f4f3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teachers_with_autonomy_preference).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_time_efficiency_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, second_language_learners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).

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
 *   Extractiveness is measured at 0.67 (interval end) because the constraint transfers the labor and cost of reading skill development from explicit teacher instruction to implicit student discovery, and from early-grade intervention to later-grade remediation. The distribution is asymmetric: students with strong implicit phonological awareness (typically native English speakers from print-rich homes) acquire reading without explicit instruction; students with atypical or missing phonological awareness (dyslexic, ESL, low-literacy-exposure background) fall into a remediation pipeline they could have avoided. Suppression is high (0.72) because the constraint's persistence requires active defense: teachers must resist phonics curricula, marginalize remedial reading research, and reframe reading failure as individual difference rather than method mismatch. Theater ratio is substantial (0.58 at interval end, rising from 0.35) because a growing share of activity in whole language classrooms—guided reading groups, reading recovery sessions, special education referrals—is remediation dressed as authentic reading engagement; the constraint's original function (meaning-first engagement with authentic texts) persists but is increasingly overwhelmed by performance of that function while the actual work shifts to managing failure. The measurement series shows extractiveness and theater rising steeply in years 0–15 (interval early period) as accumulated reading deficits become visible and remediation load increases, then plateauing (years 15–40) as the system stabilizes with sustained special education caseloads. Suppression rises steeply in years 0–25 (enforcement machinery hardens: professional development, curriculum adoption, union protection of teacher autonomy) then stabilizes as resistance from explicit instruction advocates becomes organized and durable.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (teachers) and the payer seats (struggling readers, dyslexic students, ESL learners) should compute markedly different constraint types from the same structural data. Teachers experience the arrangement as genuine coordination (authentic engagement with literature, professional autonomy, no burden of scripted drilling) and mild suppression (resistance to phonics mandates). Struggling readers experience suppression as total (no exit from ineffective method, identity-fused internalization of failure, institutional barriers to alternative instruction) and extraction as asymmetric (their effort yields no reading progress while native-speaker peers progress). The engine computes per-seat directionality from the structural data (exit options, power, beneficiary/victim declarations) and should produce rope-type classification for teachers and snare-type for struggling readers—the same constraint perceived as coordination from the agenda-setter seat and extraction from the target seat. The divergence is the finding; the authored metrics do not presuppose it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classroom teachers are structural beneficiaries with high autonomy (low d, near 0.2–0.3): they set the agenda, choose texts, pace instruction, and defend the method against phonics pressure. No exit option threatens them; worst case is adopting structured phonics, which many teachers experience as loss rather than threat. Struggling early readers are structural targets with trapped exit (high d, near 0.8–0.9): they cannot leave the classroom, cannot demand alternative instruction without parental/institutional advocacy, and accumulate reading deficit that becomes harder to escape as years pass. Dyslexic students are targets with identity_locked exit (d near 0.9): their neurology makes them maximally dependent on explicit instruction that whole language does not provide, and whole language environment pathologizes their reading profile as disability rather than neurodivergence. Second-language learners are targets with constrained exit (d near 0.8–0.85): they need explicit phonological mapping and lack the implicit phonological foundation; their exit requires changing schools or demanding ESL-specific curriculum, both difficult. Special education specialists have dual positioning (payer/beneficiary, d near 0.5–0.6): they benefit from teacher autonomy (inherit classrooms where teachers have latitude to experiment) but pay remedial costs (inherit struggling readers). Reading researchers (explicit instruction camp) have analytical exit with arbitrage optionality (d near 0.1–0.2): they benefit from whole language failures that fund their research and do not depend on whole language for professional advancement. No override needed: the structural derivation produces defensible directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling by grounding the tangled_rope claim in two independent structural facts: (1) genuine coordination function (teachers + students coordinate around authentic text engagement; scripted phonics curricula would eliminate this coordination space), and (2) asymmetric extraction (the coordination function is real but concentrated on students with native implicit learning pathways; students without such pathways subsidize the teachers' autonomy with accumulated reading deficit and later remediation costs). The founding problem (over-mechanistic drill-heavy reading instruction) is live for teachers (autonomy erosion in phonics-mandated districts) and dead for struggling readers (the problem was scripted drilling's lack of engagement; implicit whole language exposure is still not the same as explicit phoneme instruction they need). The mandatrophy (founding problem dead + constraint persists) is most visible in the special education seat: reading disability diagnoses accelerated after whole language adoption; the constraint persists as remediation industry rather than as solved original problem. The tangled_rope classification holds because the constraint genuinely coordinates something (teacher autonomy, authentic engagement) AND extracts from identifiable victims (struggling readers); the extraction is not incidental—it is the mechanism that funds the coordination (if teachers had to diagnose and remediate dyslexic readers explicitly, their autonomy would compress).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_learning_sufficiency,
    'Is implicit exposure to print sufficient to develop phonological awareness and grapheme-phoneme mapping in all readers, or only in readers with pre-existing phonological sensitivity?',
    'Longitudinal studies tracking reading development in whole language classrooms, stratified by baseline phonological awareness and family literacy exposure; neuroimaging studies of implicit-learning pathways in dyslexic vs. typical readers.',
    'If implicit learning is insufficient for atypical readers, whole language extraction is structural (not incidental); if implicit learning suffices for all readers given adequate time, extraction is remedial (late remediation is a feature, not a bug). This distinction determines whether the constraint is fundamentally asymmetric (snare) or a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_learning_sufficiency, empirical, 'Whether implicit exposure is sufficient for all reading pathways.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external barriers: lack of phonics curriculum, teacher resistance to alternative methods, institutional inertia) or internalized (struggling readers believe they are not good readers, teachers believe whole language is pedagogically superior)?',
    'Post-intervention trajectory analysis: if suppression persists after whole language classrooms adopt explicit phonics, suppression is partially internalized (students carry reading shame, teachers resist skill-based instruction even when enabled); if suppression drops when phonics curriculum is available, suppression is primarily structural.',
    'If suppression is internalized, the constraint''s effective suppressive force is higher than the structural measure suggests—struggling readers carry reading failure identity into new instructional contexts. If structural, remediation can succeed by changing materials and methods without re-socializing reader identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural barrier or internalized belief.').

omega_variable(
    implicit_vs_explicit_orchestration,
    'Can authentic text engagement AND explicit phoneme instruction coexist in a single classroom, or does whole language''s commitment to authentic engagement require rejection of explicit sequencing?',
    'Balanced literacy implementations and their outcomes; teacher interviews about whether explicit phonics feels compatible with meaning-first reading; neuropsychological evidence on whether implicit and explicit learning pathways interfere or support each other.',
    'If they can coexist, the constraint''s specificity is pedagogical choice (whole language is not logically necessary), and the extraction is a product of that choice, not an inevitable consequence of reading-by-meaning. If they interfere, whole language has a coherent rationale and balanced literacy is a conceptual confusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_vs_explicit_orchestration, conceptual, 'Whether whole language explicitly forecloses phonics or merely deemphasizes it.').

omega_variable(
    theater_ratio_driver_remediation_vs_authentic_engagement,
    'Is the rising theater ratio (0.35 → 0.58) driven by increasing share of remediation activity (guided reading recovery, intervention groups) mislabeled as authentic engagement, or by authentic engagement work becoming performative (reading for assessment rather than comprehension)?',
    'Classroom time allocation studies: what fraction of whole language instruction time is spent on remediation vs. authentic text engagement? Teacher survey on whether authentic engagement goal persists or has become rhetorical cover for management of reading failure.',
    'If remediation is mislabeled as authentic engagement, the constraint''s original coordination function (genuine engagement) persists but is increasingly hollowed out. If authentic engagement has become performative, the constraint''s function has shifted from coordination to theater (performing the function while managing failure). Theater ratio classification differs accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_driver_remediation_vs_authentic_engagement, empirical, 'Whether theater ratio rise reflects remediation load or functional erosion.').

omega_variable(
    teacher_autonomy_vs_student_outcomes_tradeoff,
    'Is the constraint''s persistence driven by genuine pedagogical benefit to engaged, neurotypical readers, or by institutional preference for teacher autonomy and avoidance of scripted curriculum?',
    'Comparative reading outcomes for whole language vs. phonics students, stratified by neurotype and family literacy background; teacher motivation surveys asking whether autonomy or student outcomes drive method preference; policy adoption data showing whether whole language persists in high-literacy-access districts where scripted phonics are available.',
    'If persistence is outcome-driven, the constraint coordinates real value. If persistence is autonomy-driven and outcomes suffer for struggling readers, the constraint is capturing autonomy benefit for teachers and externalizing cost to readers—snare classification. If both, Tangled Rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes_tradeoff, empirical, 'Whether constraint persists because of reading outcomes or teacher autonomy preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 35, 0.59).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(read_be_t35, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(read_su_t35, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, special_education_caseload_inflation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_disability_identification_criteria).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel reading_acquisition_mechanism (constraint family with phonics_reading and balanced_literacy_reading). All three stories share the same domain and foundational problem but differ in their core claim about how reading acquisition occurs: whole_language_reading asserts implicit exposure suffices; phonics_reading asserts explicit instruction is necessary; balanced_literacy_reading asserts both are needed. The readings have different ε values (whole language shows high extraction for atypical readers; phonics shows low extraction through explicit scaffolding; balanced literacy shows moderate extraction through dual labor). Each story must be authored as a clean, ε-invariant constraint; the family links enable the corpus to model how the same empirical domain supports competing claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
