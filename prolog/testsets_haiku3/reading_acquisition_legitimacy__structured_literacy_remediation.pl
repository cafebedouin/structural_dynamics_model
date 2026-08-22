% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy as Legitimacy Standard for Reading Instruction
 *   domain: education/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Structured literacy has emerged as the legitimacy standard for reading
 *   instruction, particularly in early grades and in special education. The
 *   constraint is framed as scientifically grounded: cognitive science
 *   research demonstrates that explicit, systematic phonics and cumulative,
 *   diagnostic instruction are effective, especially for readers with
 *   dyslexia and phonological processing deficits. The reading instantiates a
 *   particular normative claim: that 'reading instruction must be designed
 *   for the most vulnerable learners first' — implying that the explicit,
 *   cumulative, diagnostic principles of structured literacy should be
 *   universal and mandatory, not just available for students identified as
 *   struggling. This reading competes with sibling readings that prioritize
 *   decoding (phonics_decoding_primacy), meaning-making
 *   (whole_language_meaning_primacy), and integration
 *   (balanced_literacy_integration). The constraint's operation extracts
 *   professional authority from alternative approaches and concentrates it in
 *   a structured literacy cadre, while imposing retraining burden on teachers
 *   and compliance burden on resource-constrained schools. The beneficiary
 *   structure is asymmetric: early readers get genuine preventative support;
 *   structured literacy practitioners and researchers get expanded authority
 *   and funding; but the gains to the field accumulate most visibly among
 *   specialists, not among classroom teachers or struggling readers
 *   themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.71).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy as Legitimacy Standard for Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '9f6521eb-c601-4a15-96bd-c72636c2a95f').
narrative_ontology:cs_kernel_codification('9f6521eb-c601-4a15-96bd-c72636c2a95f', distributed).
narrative_ontology:cs_authority_grounding('9f6521eb-c601-4a15-96bd-c72636c2a95f', extraction).
narrative_ontology:cs_interpretation_layer_present('9f6521eb-c601-4a15-96bd-c72636c2a95f').
narrative_ontology:cs_reading_relation('9f6521eb-c601-4a15-96bd-c72636c2a95f', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('9f6521eb-c601-4a15-96bd-c72636c2a95f', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('9f6521eb-c601-4a15-96bd-c72636c2a95f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('9f6521eb-c601-4a15-96bd-c72636c2a95f', foundational, vulnerable_learners_first_universalism).
narrative_ontology:cs_axiom_status(vulnerable_learners_first_universalism, holdable).
narrative_ontology:cs_axiom_grounding('9f6521eb-c601-4a15-96bd-c72636c2a95f', vulnerable_learners_first_universalism, instrumental).
narrative_ontology:cs_axiom('9f6521eb-c601-4a15-96bd-c72636c2a95f', foundational, phonological_deficit_primacy).
narrative_ontology:cs_axiom_status(phonological_deficit_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9f6521eb-c601-4a15-96bd-c72636c2a95f', phonological_deficit_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('9f6521eb-c601-4a15-96bd-c72636c2a95f', integrated_balanced_literacy_baseline).
narrative_ontology:cs_drift_state('9f6521eb-c601-4a15-96bd-c72636c2a95f', contemporary_mandate_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f6521eb-c601-4a15-96bd-c72636c2a95f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_specialists).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, early_literacy_teachers_trained_in_alternatives).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_in_resource_constrained_districts).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, struggling_readers_in_non_compliant_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parent_advocates_for_dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_early_support_needs).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional community including speech-language pathologists, special educators, dyslexia specialists, and structured literacy trainers. Their credentials and funding expand when structured literacy becomes the legitimacy standard. They operate training centers, professional certifications, and intervention programs. They set the standard through policy testimony, research dissemination, and professional guidance. They benefit directly from mandates that require teacher retraining and assessment tool adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners, agenda_setter).

% Teachers in early-grade and general education classrooms must implement explicit, cumulative, diagnostic structured literacy instruction, even if trained differently. They face mandatory retraining, increased assessment burden (continuous diagnostic monitoring), and accountability pressure to meet fidelity benchmarks. Their professional autonomy narrows. Non-compliance can result in credential scrutiny or reassignment. Many lack time and funding for required professional development. Their expertise in integrated literacy, meaning-making, and child-centered instruction is reframed as deficit.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_classroom_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, general_classroom_teachers, excluded).

% Educators trained in balanced literacy or whole language approaches who find their professional competence redefined as inadequate. They must retrain or face institutional pressure. Their professional identity is built on alternative frameworks; exit means career reinvention. The cost is to identity and career continuity. Some leave teaching; others retrain but carry ambivalence about abandoning frameworks they still believe in.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, early_literacy_teachers_trained_in_alternatives, payer,
    moderate, biographical, identity_locked, national).

% Teachers in high-poverty and rural schools lack resources for structured literacy implementation: no funding for specialized curricula, no time for training, no specialists for diagnostic assessment. The mandate adds compliance burden without resources. Non-compliance triggers remediation pressure and potential state intervention. They cannot exit: school choice and funding structures enabling exit are absent. They and their students bear the cost of a legitimacy standard they cannot implement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_in_resource_constrained_districts, payer,
    powerless, biographical, trapped, local).

% Students in schools or districts where structured literacy has not been implemented (due to resource constraints, delayed adoption, or philosophical resistance) continue to receive reading instruction that the structured literacy frame treats as inadequate. They bear the cost of a legitimacy standard whose implementation is spotty: in compliant settings, intervention-grade instruction is universal; in non-compliant settings, they struggle under a now-delegitimized instructional model. They cannot choose their school or method.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, struggling_readers_in_non_compliant_programs, payer,
    powerless, biographical, trapped, national).

% Publishing houses that produce structured literacy curricula, assessment tools, and diagnostic materials experience market expansion when structured literacy becomes the legitimacy standard. Schools and districts adopt and purchase mandated materials. Publishers can shift between curriculum lines as adoption spreads and can arbitrage between jurisdictions with different adoption timelines.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Researchers in cognitive science, dyslexia, and reading acquisition whose work documents effectiveness of explicit systematic phonics experience vindication and expanded research funding. Their studies are cited as the scientific foundation for the constraint. They benefit from advisory roles, policy consulting, and academic advancement based on policy influence. They can arbitrage between jurisdictions and institutions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_researchers, beneficiary,
    organized, generational, arbitrage, global).

% Scholars in whole language, balanced literacy, critical literacy, and culturally responsive pedagogy are structurally excluded from legitimacy-setting. Their research is either not cited or cited as cautionary contrast. Their ability to influence policy and practice narrows. Journal space and conference platforms for their work contract. They would argue for integration of methods, cultural responsiveness, and learner agency but cannot do so within a frame that has already ruled their approaches inadequate. Their expertise is delegitimized by design.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, alternative_reading_theorists, excluded,
    organized, generational, constrained, global).

% State education agencies and legislative bodies that adopt structured literacy mandates or guidelines. They set the enforcement apparatus: fidelity monitoring, assessment requirements, professional development standards. They adjudicate disputes and allocate remediation resources. They face pressure from multiple constituencies (advocates for dyslexic students, teacher unions, alternative-literacy researchers). They can shift or revise mandates but do so slowly. They see themselves as implementing best practices.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, policy_makers_and_state_departments, agenda_setter,
    institutional, generational, analytical, national).

% Parent organizations (International Dyslexia Association, state advocacy groups) mobilize to mandate structured literacy, arguing it is the only evidence-based approach for dyslexic children. They benefit from the legitimacy standard because it creates legal and moral grounds for intervention and resource allocation. Their children benefit from early identification and intensive support. Their exit options are constrained: they need their children to remain in schools and to access services within the system.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parent_advocates_for_dyslexic_students, beneficiary,
    organized, biographical, constrained, national).

% Children identified as struggling readers in compliant structured literacy programs receive explicit, cumulative, diagnostic instruction; intervention is preventative rather than remedial. They benefit from early identification and intensive support. They experience the genuine coordination function: their reading needs are systematically diagnosed and met. They cannot exit the program; their benefit is trapped in their position as the primary target group the constraint was designed to serve.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_early_support_needs, beneficiary,
    powerless, biographical, trapped, national).

% Researchers in cross-linguistic reading acquisition, orthographic typology, and typological variation observe that structured literacy works differently across writing systems. They track whether the constraint's claims generalize across languages or are specific to opaque alphabetic orthographies. They maintain analytical distance and document the constraint's context-dependence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, observer_seat_comparative_linguistics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how to identify and support early readers who struggle: explicit, cumulative instruction and continuous diagnostic assessment create a common language for identifying risk and a standardized intervention pathway across schools and districts. Without the coordination, struggling readers are identified late or inconsistently, intervention timing and quality vary, and the causes of reading difficulty are not systematically diagnosed.
% TRANSFER_FUNCTION: Moves professional authority from general classroom teachers and whole-language-trained educators toward structured literacy specialists, speech-language pathologists, and dyslexia experts. Moves curricular resources from literature-rich, child-centered materials toward structured, phonologically explicit, scope-and-sequence-bound programs. Moves assessment capacity from teacher observation toward standardized diagnostic tools. Moves funding from flexible classroom resources toward specialized instruction and assessment infrastructure.
% ABSENT_VOICES: Teachers trained in alternative approaches (whole language, balanced literacy, critical literacy, culturally responsive pedagogy) are not at the table where legitimacy standards are set; their expertise in meaning-making, authentic literature, and learner agency is treated as deficit. Researchers in alternative literacy frameworks are excluded or cited only as cautionary contrast. Parents and community members whose cultural, linguistic, or educational values emphasize meaning-first or culturally grounded reading acquisition are not consulted. English learners and speakers of nonstandard dialects, whose reading acquisition may follow different trajectories, are treated as deviants from the monolingual, standard-English baseline the constraint assumes.
% DISAPPEARANCE_RATIONALE: If the structured literacy legitimacy standard disappeared overnight, schools and districts would revert to heterogeneous reading instruction approaches (balanced literacy, whole language, alternative frameworks); the unified early-identification and intervention pathway would fragment; dyslexia specialists and structured literacy practitioners would lose mandated training and assessment referrals; curriculum publishers would need to diversify back into multiple reading frameworks; teacher retraining would stop and credentials would become less consequential. The professional authority structure that consolidated around the constraint would decentralize.
% FOUNDING_PROBLEM: Early reading instruction was fragmented and failed to identify and support struggling readers until well after the critical window for intervention closed. Dyslexic and otherwise at-risk readers did not receive explicit, systematic instruction in phonological awareness and phonics; instead, they were given 'wait and see' approaches or meaning-first instruction that bypassed their underlying phonological processing needs. Identification depended on teacher judgment and occurred late (grade 3+), when intervention was harder. Some children never caught up.
% FOUNDING_PROBLEM_CORROBORATION: Structured literacy practitioners and dyslexia researchers attest the founding problem is live and severe, citing longitudinal studies showing the critical importance of early explicit phonics. Cognitive science research corroborates that phonological processing deficits underlie many reading difficulties and that explicit instruction is effective. However, balanced literacy and whole language practitioners attest that the founding problem has been mischaracterized: they argue that authentic literature exposure and comprehension-first approaches also identify and support struggling readers, and that meaning-making is not incompatible with phonological attention. State-level implementation data show mixed results: early identification improved in compliant districts, but closure of the equity gap remains slow, and some districts report that rigid fidelity to structured literacy has reduced engagement and cultural responsiveness. Independent educational researchers note that the founding problem is real for some readers but that the solution (mandatory structured literacy for all) is broader than the diagnosis strictly requires.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) and rising over the first 20 years as the constraint embedded: the measurable extraction is the reallocation of professional authority and instructional resources away from alternative frameworks toward structured literacy. Suppression is high (0.71) because the constraint's persistence depends on actively delegitimizing alternative approaches, restricting what teachers are permitted to do, and narrowing the space for competing methodologies in policy and curriculum adoption. Theater is moderate (0.42) and rising: the diagnostic and assessment machinery is functionally real (it does identify struggling readers), but a growing share of the enforcement activity defends the legitimacy of the structured literacy frame itself rather than directly serving students. The measurement series show extractiveness and theater both accelerating through years 0-20 and then plateauing around year 20 as adoption saturation increases compliance burden relative to novelty. Suppression rises in parallel as the enforcement machinery consolidates (teacher evaluation, curriculum mandates, credential requirements). The same-grid shared time points enable the compiler to emit all three series without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state departments, policy makers) sees this constraint as solving a real coordination problem and implementing best practices; from the beneficiary seat (structured literacy practitioners), it is a necessary and correct mandate that finally puts science into practice. From the target seats (classroom teachers, especially those trained in alternatives), it is enforcement of a single ideology that strips professional autonomy and ignores other evidence. From the excluded seat (alternative literacy researchers), it is a deliberate narrowing of legitimate knowledge that serves the interests of a particular professional cadre. The engine should compute these divergences from the structural data: beneficiaries compute rope/coordination; targets compute snare/extraction; the excluded seat should not compute at all in the constraint's own frame (they are excluded by design). The same structure, experienced radically differently by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The structured literacy practitioners and researchers are the structural beneficiaries (d near 0.1-0.2): their professional authority, funding, and market expand; their exit options are arbitrage (they can move between jurisdictions, advisory roles, consulting). General classroom teachers are targets (d near 0.75-0.85): they must retrain, lose autonomy, face accountability; their exit options are constrained. Teachers trained in alternatives face the highest directionality (d near 0.9): their professional identity and expertise are delegitimized. Struggling readers in compliant programs are beneficiaries (d near 0.2): they get intensive support; their exit is trapped (they cannot choose instruction). Struggling readers in non-compliant programs are targets (d near 0.8): they bear the cost of the constraint's legitimacy without the benefit of its implementation. Resource-constrained schools are powerless targets (d near 0.95): compliance burden without resources; no exit. The policy-maker seat is the agenda-setter (d near 0.5 to 0.4): they make the rules, face pressure from multiple constituencies, but do not directly extract or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (late identification and inadequate support for struggling readers) is live and serious; the founding legitimacy (that explicit, cumulative, diagnostic instruction works better than alternatives for at-risk readers) is corroborated by cognitive science research. However, mandatrophy emerges in the gap between the founding problem and the solution design: the constraint mandates structured literacy for ALL students, not just for struggling readers or those at risk. The founding problem could be solved by better early identification and intensive intervention for the vulnerable; instead, the solution has become universal structuring of reading instruction according to structured literacy principles. Additionally, the constraint's persistence no longer depends primarily on serving struggling readers; it depends on the professional interests of a structurally powerful community (structured literacy practitioners, dyslexia specialists) and on the policy inertia of state-level mandates. A teacher or district that achieves strong reading outcomes using balanced literacy or alternative approaches is nonetheless non-compliant. This is mandatrophy: the constraint has outlived the narrower function (identifying and supporting truly at-risk readers) and persists as a means of maintaining professional authority and enforcing ideological conformity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_definition_kernel_ambiguity,
    'What is reading, at its core: a decoding skill (phonological processing → orthographic matching → semantic access), a comprehension and meaning-making practice (bringing prior knowledge and interpretation to text), or an integrated practice (decoding + comprehension + cultural/critical engagement)?',
    'Cognitive science evidence on reading acquisition mechanisms; longitudinal studies comparing outcomes across instructional approaches; analysis of reading behavior in skilled adult readers (silent reading, skim-reading, rereading, annotation patterns) to determine what ''real'' reading involves.',
    'If reading is primarily decoding, structured literacy is the legitimate baseline and alternatives are inferior. If reading is primarily meaning-making, structured literacy''s emphasis on explicit phonics is a necessary but insufficient component, and alternatives emphasizing comprehension and engagement are equally legitimate. If reading is integrated, then mandating one approach over another is over-specification, and legitimacy should allow multiple paths.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_definition_kernel_ambiguity, conceptual, 'Definitional disagreement on what reading IS, underlying all four sibling readings.').

omega_variable(
    vulnerable_learner_universality,
    'Should instruction designed for the most vulnerable learners (those with dyslexia, phonological deficits, or severe struggles) be made universal, or should it be targeted and differentiated based on learner needs and response to instruction?',
    'Comparative study of outcomes in districts with universal structured literacy vs. districts with tiered response-to-intervention models; analysis of engagement and reading attitudes across approaches; tracking of whether universal structured literacy narrows reading instruction for advanced readers or reduces their exposure to complex texts and critical engagement.',
    'If vulnerable-learners-first universalism works across the ability range and produces better outcomes for all, the mandate is justified. If it narrows instruction for advanced readers or produces disengagement in non-vulnerable learners, then targeted intervention is more efficient and equitable. This is the reading''s distinctive axiom; if overridden, the constraint becomes snare (pure extraction of professional authority) rather than tangled_rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_learner_universality, empirical, 'Whether the principle ''design for the most vulnerable first'' should be universal or differentiated.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural (external barriers: mandates, curricula constraints, assessment requirements, threat of non-compliance), internalized (teachers have come to believe alternative approaches are illegitimate or ineffective), or both?',
    'Post-mandate surveys of teacher beliefs about reading instruction in districts that have revoked or significantly loosened structured literacy mandates; interviews with teachers after curriculum restrictions are removed; analysis of whether teachers voluntarily continue structured literacy or return to alternative approaches when external enforcement is removed.',
    'If suppression is mostly structural, removing or loosening mandates would quickly restore diversity of approaches. If internalized, the suppression persists in teacher belief and practice even after external enforcement is removed, indicating that legitimacy delegitimization has succeeded. If both, then the constraint is more durable than structural enforcement alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative approaches is structural mandate or internalized belief.').

omega_variable(
    equity_gap_closure_attribution,
    'Do improvements in early reading outcomes and earlier identification of struggling readers in structured literacy districts result from the explicit, cumulative, diagnostic principles of structured literacy specifically, or from the focused attention, increased intervention intensity, and resource allocation that accompany any major reading initiative?',
    'Natural experiment: comparison of outcome trajectories in districts that adopted structured literacy with outcome trajectories in districts that received similar resource investment and intervention intensity but used alternative methods (balanced literacy with intensive assessment and intervention). Control for implementation fidelity and resource per-student.',
    'If improvements are specific to structured literacy, the constraint''s scientific legitimacy is high. If improvements result from focused attention and resources regardless of method, then structured literacy is not uniquely legitimate; the mandate extracts professional authority without justified superiority, making it closer to snare than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_gap_closure_attribution, empirical, 'Whether reading improvements in structured literacy districts are method-specific or resource/attention-specific.').

omega_variable(
    orthographic_and_linguistic_variation,
    'Does the structured literacy constraint apply equally across all writing systems and all languages, or is it specific to opaque alphabetic orthographies (like English) where explicit phonics instruction is cognitively efficient?',
    'Cross-linguistic research on reading acquisition in transparent orthographies (Spanish, Finnish, German), logographic systems (Chinese), and abugida systems (Tamil, Arabic); analysis of whether explicit phonics is equally necessary or whether orthographic transparency makes alternative approaches effective.',
    'If structured literacy is universally legitimate, it should work equally well across orthographies. If effectiveness is orthography-dependent, then the constraint''s legitimacy claim is over-generalized; it should be limited to opaque alphabetic contexts and allow alternative approaches in other orthographies. This would reframe the constraint as partially false (its claim to universal legitimacy is unfounded) and contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthographic_and_linguistic_variation, empirical, 'Whether structured literacy is universally legitimate or orthography-specific.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.24).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.31).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.41).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the kernel 'reading_acquisition_legitimacy'. The kernel is the fundamental question: what counts as legitimate reading instruction? Each reading instantiates a different answer (different ε, different beneficiary/victim structure, different computed type). All four should be generated as separate constraint stories, linked via this affects_constraints network. The ε-invariance principle applies: each reading assesses the standing arrangement (what teachers do NOW, under THAT reading's lights) against a different referent. The structured_literacy_remediation reading assesses current practice as inadequate where it deviates from explicit, cumulative, diagnostic instruction; its ε is therefore high (much to extract/reform). The whole_language_meaning_primacy reading would assess the same practice as inadequate where it over-emphasizes decoding at the expense of comprehension and engagement; its ε might be lower (less to reform, or different things to reform). The four stories together form a constraint family showing how the same domain (reading instruction legitimacy) decomposes into structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, powerless, 0.95).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
