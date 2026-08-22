% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition Constraint
 *   domain: education/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy is an institutional instructional framework adopted
 *   across most U.S. public school districts since the 2000s. It claims to
 *   synthesize phonics and meaning-engagement as equally necessary,
 *   complementary components of reading instruction. The constraint is the
 *   enforcement of this bifurcated instructional structure in classrooms.
 *   Research in cognitive science and dyslexia studies increasingly
 *   challenges the balanced framing, suggesting systematic phonics is the
 *   causal driver of reading acquisition and that meaning engagement emerges
 *   from fluency, not alongside it. The balanced reading represents one
 *   specific reading of a contested kernel: the question of how reading
 *   develops and what instruction produces it. This reading claims synthesis;
 *   sibling readings claim primacy (phonics-first, structured literacy) or
 *   emergence (whole language). The constraint story models balanced literacy
 *   as instantiated in practice: maintained by institutional adoption, school
 *   curricula, teacher training, and textbook publishing; generating
 *   extraction through method churn, insufficient intensity for struggling
 *   readers, and instructional load imposed on teachers and students.
 *
 * KEY AGENTS:
 *   - balanced_literacy_advocates: institutional agenda-setters who have embedded the framework into standards and training
 *   - curriculum_publishers: institutional beneficiaries capturing revenue from material production and adoption cycles
 *   - education_schools: organized coordinating bodies that allocate time and resources according to balanced literacy structure
 *   - classroom_teachers: moderate-power payers bearing the instructional labor of managing two simultaneous streams
 *   - struggling_readers: powerless, identity-locked payers experiencing insufficient phonics intensity and premature meaning-engagement pressure
 *   - low_income_students: trapped powerless payers lacking home literacy resources to compensate for insufficient explicit instruction
 *   - structured_literacy_advocates: excluded powerful actors whose research evidence sits outside curriculum adoption pipelines
 *   - research_community: observers generating empirical evidence that increasingly contradicts the balanced framework's foundational claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Constraint").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "education/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '81743cf5-5852-44c4-8739-816cd1018bf4').
narrative_ontology:cs_kernel_codification('81743cf5-5852-44c4-8739-816cd1018bf4', formalized).
narrative_ontology:cs_authority_grounding('81743cf5-5852-44c4-8739-816cd1018bf4', expertise).
narrative_ontology:cs_interpretation_layer_present('81743cf5-5852-44c4-8739-816cd1018bf4').
narrative_ontology:cs_reading_relation('81743cf5-5852-44c4-8739-816cd1018bf4', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('81743cf5-5852-44c4-8739-816cd1018bf4', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('81743cf5-5852-44c4-8739-816cd1018bf4', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('81743cf5-5852-44c4-8739-816cd1018bf4', foundational, phonics_and_meaning_coordinate_principles).
narrative_ontology:cs_axiom_status(phonics_and_meaning_coordinate_principles, holdable).
narrative_ontology:cs_axiom_grounding('81743cf5-5852-44c4-8739-816cd1018bf4', phonics_and_meaning_coordinate_principles, empirically_contingent).
narrative_ontology:cs_axiom('81743cf5-5852-44c4-8739-816cd1018bf4', foundational, reading_emerges_via_integration_not_sequence).
narrative_ontology:cs_axiom_status(reading_emerges_via_integration_not_sequence, holdable).
narrative_ontology:cs_axiom_grounding('81743cf5-5852-44c4-8739-816cd1018bf4', reading_emerges_via_integration_not_sequence, empirically_contingent).
narrative_ontology:cs_reference_frame('81743cf5-5852-44c4-8739-816cd1018bf4', phonics_meaning_coordinate_synthesis).
narrative_ontology:cs_drift_state('81743cf5-5852-44c4-8739-816cd1018bf4', contemporary_research_environment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('81743cf5-5852-44c4-8739-816cd1018bf4', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, low_income_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Education schools, teacher-training programs, and literacy researchers who have adopted and promote balanced literacy as the standard instructional framework. They set curriculum standards, publish widely-adopted textbooks, design teacher professional development, and defend the approach in policy forums. Institutional stake in method persistence and adoption breadth.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_advocates, agenda_setter,
    institutional, generational, mobile, national).

% Commercial textbook and curriculum-materials publishers who produce and sell balanced-literacy aligned lesson plans, basal readers, workbooks, and digital materials to schools. Collect revenue from repeated adoption cycles and material updates. Method stability is a revenue stream; method contestation can be a market opportunity (material churn).
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    institutional, biographical, arbitrage, national).

% Public and charter schools adopting balanced literacy as their reading curriculum framework. They set instructional priorities, allocate teacher time across phonics and reading practice, purchase aligned materials, and report reading outcomes. They benefit from a coherent, recognized instructional standard but also face pressure to show reading gains and manage teacher retraining costs when methods shift.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, beneficiary).

% Elementary teachers tasked with implementing balanced literacy in their classrooms. They learn the framework in professional development, manage two simultaneous instructional streams (phonics and connected text), assess student progress on both dimensions, and navigate contradictory demands when students struggle. They bear the instructional labor cost of balancing; they also benefit from a structured, recognized method that is recognized by peers and administrations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Students who have difficulty acquiring reading skills, including those with dyslexia, processing deficits, or limited early literacy exposure. Under balanced literacy they receive some phonics instruction but often insufficient systematicity or intensity; they also experience pressure to engage meaningfully with texts at grade level before fluency is established. Identity forms around reading ability; school setting (classroom, school) is the only context in which this constraint operates. No exit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, identity_locked, local).

% Students from low-income backgrounds with limited home literacy exposure, fewer books at home, less parental reading modelling, and often less access to supplementary tutoring or intensive intervention. Balanced literacy's emphasis on meaning engagement and minimal explicit phonics instruction assumes literacy-rich home environments that low-income students often lack. They bear the cost of insufficient explicit instruction but cannot exit to better-resourced schools or private tutoring.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, low_income_students, payer,
    powerless, biographical, trapped, local).

% Researchers, dyslexia specialists, and educational psychologists advocating for Orton-Gillingham-derived structured literacy as superior to balanced literacy. They publish research, lobby policy makers, and train teachers, but operate outside the mainstream curriculum-adoption pipeline. Exclusion from curriculum standards and textbook markets limits their direct influence on classroom practice at scale.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    powerful, generational, constrained, national).

% Teachers and educators who still practice more purely whole-language approaches (emphasis on meaning, minimal systematic phonics). They are not formally excluded but increasingly marginalized as balanced literacy has become the institutional standard. Some operate in progressive schools; many are aging out of the profession. Their voice in professional forums is diminished.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_practitioners, excluded,
    moderate, generational, constrained, regional).

% Cognitive science researchers, literacy scientists, and educational researchers who study reading acquisition empirically. They produce evidence on phonics effectiveness, meaning engagement, cognitive load, and population-specific effects. Their role is to generate data; policy adoption lags evidence by years or decades.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, research_community, observer,
    institutional, generational, analytical, national).

% State and district education officials, legislatures, and education department administrators who set standards, approve curricula, and allocate funding. They broker between research, teacher practice, commercial interests, and public pressure. They observe the constraint but do not directly operate it; their decisions about standards and funding shape which methods persist.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, recognizable instructional framework that integrates explicit phonics instruction with meaningful connected-text engagement. Solves the coordination problem of how to structure reading instruction across heterogeneous classrooms and schools: both phonics and meaning-engagement happen; neither is supposed to dominate. Enables teacher training, material alignment, and outcome measurement within a unified standard.
% TRANSFER_FUNCTION: Transfers instructional time and attention from intensive, targeted phonics to a bifurcated approach; transfers revenue from single-method textbook series to publishers who produce both phonics materials and guided-reading collections; transfers students' cognitive effort to managing two simultaneous learning demands rather than a more unified pathway.
% ABSENT_VOICES: Structured literacy specialists and dyslexia researchers are institutionally excluded from mainstream curriculum standard-setting; their evidence about systematic phonics effectiveness and cumulative sequencing for struggling readers sits outside the balanced framework's decision loops. Whole-language practitioners who still advocate for meaning-first approaches are marginalized. Struggling readers themselves cannot articulate their experience as instructional feedback that shapes method design.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared, schools would reorganize around one of the sibling readings (structured literacy, phonics-first, or renewed whole-language emphasis), or would attempt to avoid an explicit method in favor of ad-hoc practice. Teachers would reallocate instructional time; publishers would reorient material production; outcome data would shift. The instructional landscape would change significantly.
% FOUNDING_PROBLEM: In the 1990s–2000s, reading instruction was polarized between phonics-only advocates and whole-language advocates, each claiming efficacy and criticizing the other's approach. Schools varied wildly; some students were taught phonics in isolation (demotivating, disconnected from meaning), others learned meaning without explicit decoding skills (struggling with fluency and multisyllabic words). Balanced literacy emerged as a proposed synthesis: both phonics AND meaning matter; both should be present.
% FOUNDING_PROBLEM_CORROBORATION: The balanced literacy advocates attest the founding problem (polarization, instructional variance) was real and the synthesis is necessary. Structured literacy researchers and the National Institute of Child Health and Human Development (NICHD) reading research consensus attest that the founding problem was misdiagnosed—phonics effectiveness is well-established empirically; what was needed was systematic, intensive phonics for all, not a compromise between phonics and whole language. The contested status reflects the living research debate.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because balanced literacy does provide a recognizable, coherent instructional standard (coordination function) but also generates extraction: publishers gain revenue from repeated material production cycles, schools gain alignment but lose flexibility, teachers gain a recognized framework but lose instructional autonomy and time. Struggling readers and low-income students bear the highest cost—insufficient phonics intensity and premature meaning-engagement pressure—without exit. Theater ratio is high (0.48) because the constraint's persistence partly depends on narrative maintenance: the 'balanced' framing requires continuous assertion that both streams are equally necessary (even as research evidence suggests phonics is causal and meaning-engagement is derivative). Suppression is moderate (0.42) because the constraint is not maintained primarily through coercive mechanisms; rather, it persists through institutional embedding, professional consensus, and textbook availability. But suppression does operate: structured literacy research is marginalized, whole-language practitioners are pushed out, and teachers who deviate from the framework face pressure. Measurement series show extractiveness and theater rising early (0–10 interval) as adoption accelerates, then plateauing as the method becomes normalized—a classic pattern of institutional drift toward institutional theater. Suppression remains stable, suggesting enforcement infrastructure neither hardens nor decays.
 *
 * PERSPECTIVAL GAP:
 *   From the balanced literacy advocates' position, the constraint is a genuine synthesis solving a real coordination problem (polarization, instructional variance). From the structured literacy advocates' position, the constraint is a false compromise that perpetuates the phonics-denial error of whole language, just with token phonics added—they see the two streams as fundamentally misaligned. From the struggling readers' and low-income students' positions, the constraint is oppressive: phonics instruction is neither systematic enough nor intense enough for them to catch up, but meaning-engagement pressure forces them to attempt grade-level texts before decoding fluency is established, generating frustration and identity-formed reading anxiety. From the teachers' position, the constraint is manageable but cognitively demanding: they have to design lessons that satisfy both phonics and meaning objectives, manage mixed-readiness classrooms under the constraint structure, and report progress on two axes. The engine computes these divergences from the power/exit/beneficiary/victim structural data; they emerge as different effective extraction scores per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Balanced literacy advocates (institutional, mobile) have directionality near 0.0 (low d, beneficiaries): they designed the framework and maintain its institutional position; they can exit to other roles or methods. Curriculum publishers (institutional, arbitrage-capable) sit near 0.1–0.2 (beneficiaries): they capture revenue and can shift material production if markets change. Education schools (organized, constrained) sit near 0.3–0.4 (symmetric): they gain coordination structure but are locked into material purchases, teacher training, and outcome commitments that shift if methods change. Teachers (moderate power, constrained) sit near 0.5–0.6 (approaching targets): they gain a recognized framework but bear instructional labor and time costs without direct benefit. Struggling readers (powerless, identity-locked, trapped) and low-income students (powerless, trapped) sit near 0.85–0.95 (targets): they cannot exit, bear cognitive load and social costs from the constraint structure, and lack resources to access better-resourced alternative methods. Structured literacy advocates (excluded powerful) sit near 0.6–0.7: they are not formally victims but are structurally prevented from accessing the decision-making apparatus, so their directionality is constrained-to-powerful with exclusion as the suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced literacy constraint does not appear to satisfy the mandatrophy criterion (where the founding problem persists but the arrangement becomes pure inertia). The founding problem (polarization, instructional variance) has been solved in one dimension: schools are no longer chaotic; balanced literacy is the recognizable standard. But research evidence increasingly indicates that the constraint itself is misdiagnosed—the 'balance' is not the solution but part of the problem. Structured literacy research suggests the founding problem was not polarization but insufficient phonics, and that the solution is intensified phonics, not compromise. So the founding problem status is contested, not dead. If structured literacy evidence becomes overwhelming and schools begin to shift, the mandatrophy question would arise then: will schools rapidly adopt intensive phonics (solution-found), or will they persist with balanced literacy despite contradicting evidence (true mandatrophy)? At the present measurement interval, mandatrophy is not yet operative; contestation is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_vs_rebranding,
    'Is balanced literacy a genuine synthesis of phonics and meaning-engagement as coordinate principles, or is it whole-language relabeled to accommodate post-2000 phonics advocacy?',
    'Empirical analysis of classroom implementation: do teachers actually allocate time equally to both streams, or does one stream dominate? Do students receiving intensive phonics + meaning-engagement outperform students receiving only intensive phonics, or do the two groups show equivalent gains? Natural experiments from schools that shifted to structured literacy after balanced literacy adoption.',
    'If true synthesis, balanced literacy''s moderate extractiveness may be justified by genuine coordination benefit. If rebranding of whole-language, the extractiveness is closer to pure extraction—the meaning-engagement stream is sufficient, phonics is theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synthesis_vs_rebranding, empirical, 'Whether balanced literacy is a genuine third position or a relabeling of whole language to accommodate phonics evidence.').

omega_variable(
    causality_vs_correlation_phonics,
    'Is systematic phonics instruction the causal driver of reading acquisition, or is it a correlate of instructional time and intensity?',
    'Controlled studies comparing equivalent instructional intensity in phonics vs. meaning-engagement only; meta-analysis of phonics intervention trials; neuroimaging studies of phonetic vs. semantic processing during reading acquisition.',
    'If phonics is causal and sufficient, the balanced requirement for coordinate meaning-engagement is extractive—it imposes unnecessary cognitive load and dilutes phonics intensity. If phonics and meaning are truly interdependent, balanced literacy''s structure is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_vs_correlation_phonics, empirical, 'Whether phonics is the causal agent of reading acquisition or a necessary but not sufficient component.').

omega_variable(
    home_literacy_resource_dependency,
    'Does balanced literacy''s reliance on meaning-engagement assume home literacy resources (books, reading models, family print exposure) that low-income students systematically lack?',
    'Comparative study of balanced literacy outcomes in high-literate vs. low-literate home environments; intervention studies providing intensive explicit phonics to low-literacy-background students and measuring reading growth independent of home resources.',
    'If balanced literacy''s efficacy is dependent on literacy-rich home environments, it systematically extracts from low-income students by imposing unequal instructional demands. Structured literacy''s systematic, cumulative approach may better serve students without home literacy resources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(home_literacy_resource_dependency, empirical, 'Whether balanced literacy''s meaning-engagement component requires home literacy resources that low-income students lack.').

omega_variable(
    dyslexia_and_struggling_reader_impact,
    'For students with dyslexia or reading disabilities, does the balanced literacy approach provide sufficient phonological and phonemic scaffolding, or does structured literacy''s cumulative, explicit approach produce better outcomes?',
    'Longitudinal studies comparing dyslexic students'' reading trajectories under balanced vs. structured literacy; neuroimaging of phonological processing in both frameworks; intervention trials with identified dyslexic populations.',
    'If structured literacy produces significantly better outcomes for struggling readers, balanced literacy extracts from the highest-need population by failing to provide sufficient intensity. This would reframe the constraint as a tangled rope serving institutional convenience at the cost of dyslex performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dyslexia_and_struggling_reader_impact, empirical, 'Whether balanced literacy adequately serves students with dyslexia or significant reading difficulties.').

omega_variable(
    reading_versus_meaning_engagement_sequencing,
    'Does meaning-engagement with grade-level connected text before fluency is established harm or help reading acquisition? Is the cognitive load of decoding-plus-meaning-making optimal, or does it exceed working memory limits and reduce learning?',
    'Cognitive load experiments comparing performance on meaning-comprehension tasks with vs. without fluency; eye-tracking studies of reading processing in fluent vs. non-fluent readers engaging with grade-level text; instructional experiments comparing early-fluency-building vs. early-meaning-engagement.',
    'If premature meaning-engagement overloads working memory and reduces decoding skill acquisition, balanced literacy''s requirement for coordinate engagement is extractive for all learners but especially for low-skilled students. A sequential approach (fluency-first, then meaning-engagement) might be more efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_meaning_engagement_sequencing, empirical, 'Whether simultaneous decoding-and-meaning-engagement optimizes learning or overloads cognitive resources.').

omega_variable(
    reading_research_consensus_adoption_lag,
    'Why does balanced literacy persist as the institutional standard despite decades of cognitive science and dyslexia research pointing toward structured literacy?',
    'Historical analysis of NICHD consensus on reading research, state standard adoptions, teacher-training curriculum evolution, and textbook publication timelines; interviews with policy makers on adoption lag and inertia factors.',
    'If adoption lag is primarily institutional inertia and professional consensus lag, the constraint may degrade toward piton status as research evidence strengthens. If adoption lag reflects genuine alternative-pathway advocacy and distributive justice concerns (that structured literacy is viewed as potentially elitist or rigid), the persistence is more contestable. If adoption lag reflects commercial incentives favoring method churn, the extractiveness shifts toward pure rent collection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_research_consensus_adoption_lag, empirical, 'Why institutional reading standards lag behind research consensus, and whether this is inertia, ideology, or commercial incentive.').

omega_variable(
    theater_ratio_interpretation,
    'As theater_ratio rises (measurement data suggests 0.35→0.48 from t0 to t25), does this reflect displacement of actual phonics/meaning instruction by performative assessment and reporting, or simply increasing public discourse about reading while actual instruction remains stable?',
    'Classroom observation studies quantifying actual instructional minutes in phonics vs. meaning engagement vs. assessment/reporting; teacher surveys on time pressure and reporting burden; curriculum material analysis tracking balance of actual-practice content vs. reporting scaffolds.',
    'If theater is rising because real instruction is being displaced by assessment burden, the constraint may be degrading toward piton (inert, performative). If theater rise is just increased public rhetoric without instructional change, it suggests theater is not the primary extraction driver—extraction is sustained by genuine bifurcated instruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether rising theater_ratio reflects growing performativity in reading instruction or just increased public discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.18).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four constraint stories, each instantiating a different reading of the kernel claim about how reading develops and what instruction produces it. Each reading carries a different set of beneficiaries, victims, and institutional positions. Balanced literacy claims synthesis; phonics and structured literacy claim primacy of phonics; whole language claims natural emergence. The four constraints are linked because each reading's policy adoption affects the others: widespread balanced literacy adoption suppresses structured literacy advocacy; increasing research evidence favors structured literacy, pressuring balanced literacy; resurgent whole-language practice challenges balanced literacy's legitimacy on motivation grounds. The network captures these mutual pressures: balanced→others via institutional displacement; phonics/structured→balanced via research pressure; whole_language→balanced via ideological/pedagogical challenge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
