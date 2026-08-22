% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Structured Literacy Reading Acquisition (Orton-Gillingham Tradition)
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   The structured literacy reading of the literacy acquisition kernel holds
 *   that reading requires explicit, systematic, cumulative instruction across
 *   phonological awareness, phonics, fluency, vocabulary, and
 *   comprehensionâthe Orton-Gillingham tradition extended to universal
 *   application. In recent decades, this reading has migrated from
 *   special-education dyslexia intervention to general-education mandate in
 *   many U.S. states, driven by the 'science of reading' movement. The
 *   structural result is a constraint that genuinely coordinates reading
 *   acquisition for students with dyslexia (substantial benefit) while
 *   extracting significant costs from general education teachers through
 *   specialized certification and training requirements. The extraction
 *   accrues to a growing industry of OG trainers and curriculum providers.
 *   This story treats the constraint as the standing arrangement: the
 *   mandated structured literacy instructional model with its associated
 *   credentialing infrastructure.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: Primary beneficiary (powerless/trapped) â receives intervention that reduces reading failure under the mandate
 *   - general_education_teachers: Primary payer (moderate/constrained) â bears specialized certification burden and loss of instructional autonomy
 *   - specialized_literacy_trainers: Secondary beneficiary (organized/mobile) â captures training revenue and institutional status from mandate expansion
 *   - state_education_agencies: Agenda setter (institutional/arbitrage) â mandates requirements via legislation, could reverse them through political process
 *   - balanced_literacy_advocates: Excluded voice (organized/constrained) â displaced from policy panels and curriculum adoption in mandated jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.55).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Acquisition (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '8a810d42-8dfb-455a-8ccd-471ec9a66454').
narrative_ontology:cs_kernel_codification('8a810d42-8dfb-455a-8ccd-471ec9a66454', formalized).
narrative_ontology:cs_authority_grounding('8a810d42-8dfb-455a-8ccd-471ec9a66454', expertise).
narrative_ontology:cs_interpretation_layer_present('8a810d42-8dfb-455a-8ccd-471ec9a66454').
narrative_ontology:cs_reading_relation('8a810d42-8dfb-455a-8ccd-471ec9a66454', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('8a810d42-8dfb-455a-8ccd-471ec9a66454', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('8a810d42-8dfb-455a-8ccd-471ec9a66454', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('8a810d42-8dfb-455a-8ccd-471ec9a66454', foundational, explicit_instruction_universal_necessity).
narrative_ontology:cs_axiom_status(explicit_instruction_universal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8a810d42-8dfb-455a-8ccd-471ec9a66454', explicit_instruction_universal_necessity, empirically_contingent).
narrative_ontology:cs_axiom('8a810d42-8dfb-455a-8ccd-471ec9a66454', foundational, multisensory_cumulative_structure_required).
narrative_ontology:cs_axiom_status(multisensory_cumulative_structure_required, holdable).
narrative_ontology:cs_axiom_grounding('8a810d42-8dfb-455a-8ccd-471ec9a66454', multisensory_cumulative_structure_required, empirically_contingent).
narrative_ontology:cs_reference_frame('8a810d42-8dfb-455a-8ccd-471ec9a66454', explicit_systematic_instruction_optimal).
narrative_ontology:cs_drift_state('8a810d42-8dfb-455a-8ccd-471ec9a66454', contemporary_science_of_reading_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a810d42-8dfb-455a-8ccd-471ec9a66454', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive explicit, multisensory reading instruction through school-based intervention or general education classrooms. For many, this approach significantly improves decoding and comprehension compared to implicit methods. Exit is limited by IEP processes, district resource availability, and parental advocacy capacity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Must complete specialized, often costly certification in structured literacy or Orton-Gillingham methods to comply with state mandates or district policy. The training burden falls on personal time and frequently personal finances. Alternative employment outside public education exists but carries significant transition costs; within education, teaching assignments are increasingly contingent on certification compliance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Provide OG certification courses, structured literacy professional development, and curriculum materials to districts and individual teachers. Revenue and institutional standing grow as state mandates expand the requirement for their services. They can move between markets or adapt offerings as policy shifts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers, beneficiary,
    organized, generational, mobile, national).

% Mandate structured literacy approaches and associated teacher training requirements through legislation and administrative rule. Justify mandates by citing reading failure rates and cognitive science research. Can reverse or modify mandates through legislative process, though doing so requires overcoming advocacy coalitions and public concern about literacy outcomes.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_education_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for workshop models, guided reading, and leveled texts as primary literacy instruction. Increasingly excluded from curriculum adoption committees and state policy panels in jurisdictions that have mandated structured literacy. Their pedagogical approach is formally delegitimized in states with science-of-reading laws.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides explicit, systematic, cumulative reading instruction that enables students with dyslexia and other reading disabilities to acquire decoding, fluency, and comprehension skills through a multisensory, structured approach.
% TRANSFER_FUNCTION: Moves time, financial resources, and instructional autonomy from general education teachers to specialized training programs and structured literacy curricula, in exchange for promised universal reading proficiency and reduced special education referrals.
% ABSENT_VOICES: Whole language and balanced literacy advocates are often excluded from policy-making bodies in districts and states that have mandated structured literacy; general education teachers without specialized certification are rarely at the table when training requirements are set.
% DISAPPEARANCE_RATIONALE: If the requirement for explicit structured literacy instruction and specialized certification vanished, districts would revert to varied pedagogical approaches (balanced literacy, workshop models), teacher professional development would shift away from OG certification, the market for specialized literacy trainers would contract, and dyslexic students might lose consistent access to evidence-based intervention.
% FOUNDING_PROBLEM: A significant subset of studentsâparticularly those with dyslexiaâfail to learn to read under implicit, whole-language, or minimally structured phonics approaches, leading to lifelong literacy deficits and associated social and economic harms.
% FOUNDING_PROBLEM_CORROBORATION: Independent reading researchers, cognitive scientists, and special education advocates attest to the persistence of reading failure under less-structured approaches; longitudinal outcome studies and meta-analyses from outside the OG training industry support the efficacy of explicit, systematic instruction for at-risk readers.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the high cost imposed on general education teachers for specialized certification, offset by genuine learning gains for dyslexic students. Suppression (0.55) is moderate: whole language and balanced literacy approaches are delegitimized in mandated jurisdictions but persist elsewhere. Theater ratio (0.25) captures performative complianceâsome districts implement OG 'in name only' while maintaining prior instructional practices. Accessibility collapse (0.45) is incomplete because alternative pedagogies remain viable in non-mandated contexts. Resistance (0.58) is substantial, driven by teacher unions, balanced literacy advocates, and districts facing unfunded training mandates. Metrics were measured at the end of the interval (T=25) and are not tuned to match the claimed type.
 *
 * PERSPECTIVAL GAP:
 *   From the student-with-dyslexia seat, the constraint presents as genuine coordination that corrects a policy failure (inadequate reading instruction). From the general-education-teacher seat, the same constraint operates as extractionâmandated professional development that consumes time and money without commensurate salary increase, often delivered by trainers with proprietary financial interests. The specialized trainer seat experiences the constraint as beneficial market expansion. These divergences are structurally inherent to the constraint's design and are computed by the engine from the authored beneficiary/victim data.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia are structural beneficiaries (d near 0.0): the constraint subsidizes their access to evidence-based intervention. General education teachers are structural targets (d near 1.0): they pay the certification costs and surrender instructional autonomy. Specialized literacy trainers sit near the beneficiary end (d ~0.2) because the constraint's expansion directly increases their revenue and status. State agencies sit nearer symmetric (d ~0.5) because they bear political implementation costs while gaining policy success metrics. Directionality is derived from these structural positions without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreading failure under unstructured approachesâremains live, preventing piton classification. However, the constraint's expansion from targeted dyslexia intervention to universal mandate risks metric substitution: teacher certification hours become a proxy for instructional quality, and OG fidelity becomes a compliance checkbox. The genuine coordination function (dyslexia remediation) prevents snare classification, while the asymmetric training burden on general educators prevents rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_kernel_position,
    'Is structured literacy a distinct reading of the literacy acquisition kernel, or merely a specialized variant of the phonics reading?',
    'Historical and conceptual analysis of whether the Orton-Gillingham tradition''s multisensory, cumulative scope constitutes a separate structural commitment or an extension of phonics-first principles.',
    'If a variant, its extraction profile should be merged with phonics_reading; if distinct, it warrants separate epsilon tracking and distinct network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_kernel_position, conceptual, 'Contested position of structured literacy within the kernel family').

omega_variable(
    training_extraction_beneficiary,
    'Who captures the surplus generated by mandatory structured literacy certification requirements imposed on general education teachers?',
    'Financial audit of state-mandated training contracts, district professional-development expenditures, and certification-body revenue streams.',
    'If concentrated in specific training providers, the constraint leans snare-like for the teacher seat; if diffuse across the system, it remains tangled_rope with distributed overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_extraction_beneficiary, empirical, 'Beneficiary ambiguity in teacher training extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative pedagogies structural (state mandates and licensing barriers) or internalized (teacher belief that explicit structured literacy is the only morally acceptable approach)?',
    'Cross-jurisdiction comparison: measure alternative pedagogy persistence in districts without structured literacy mandates versus those with mandates.',
    'If primarily structural, suppression would collapse with mandate removal; if internalized, the constraint''s effective suppression exceeds the structural measure and removal would leave residual compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of literacy alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(structured_lit_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(structured_lit_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(structured_lit_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(structured_lit_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(structured_lit_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(structured_lit_tr_t25, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(structured_lit_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(structured_lit_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(structured_lit_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(structured_lit_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(structured_lit_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(structured_lit_be_t25, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(structured_lit_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(structured_lit_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(structured_lit_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(structured_lit_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(structured_lit_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(structured_lit_su_t25, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the literacy_acquisition_kernel, which decomposes into structurally distinct claims about how reading acquisition operates. The structured_literacy_reading is distinguished by its Orton-Gillingham heritage, multisensory cumulative scope, and universal applicability claim. It is contested whether it is a distinct fourth reading or a variant of phonics_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
