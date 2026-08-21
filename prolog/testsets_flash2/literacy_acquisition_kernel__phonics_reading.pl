% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition (Literacy Kernel Reading)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the literacy
 *   acquisition kernel, asserting that explicit, systematic instruction in
 *   phoneme-grapheme correspondence is a prerequisite for connected text
 *   exposure and that decoding enables comprehension. It is a dominant
 *   pedagogical approach, particularly in response to concerns about reading
 *   failure. The constraint is framed as a Tangled Rope due to its genuine
 *   coordination function (providing a structured path to literacy) coupled
 *   with significant extraction from teacher autonomy and potential
 *   disengagement for some students, maintained by active enforcement through
 *   curriculum mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition (Literacy Kernel Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '36135d1a-627a-46ab-a31d-f0d08adf5741').
narrative_ontology:cs_kernel_codification('36135d1a-627a-46ab-a31d-f0d08adf5741', formalized).
narrative_ontology:cs_authority_grounding('36135d1a-627a-46ab-a31d-f0d08adf5741', expertise).
narrative_ontology:cs_interpretation_layer_present('36135d1a-627a-46ab-a31d-f0d08adf5741').
narrative_ontology:cs_reading_relation('36135d1a-627a-46ab-a31d-f0d08adf5741', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('36135d1a-627a-46ab-a31d-f0d08adf5741', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('36135d1a-627a-46ab-a31d-f0d08adf5741', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('36135d1a-627a-46ab-a31d-f0d08adf5741', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('36135d1a-627a-46ab-a31d-f0d08adf5741', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('36135d1a-627a-46ab-a31d-f0d08adf5741', foundational, explicit_systematic_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('36135d1a-627a-46ab-a31d-f0d08adf5741', explicit_systematic_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('36135d1a-627a-46ab-a31d-f0d08adf5741', scientific_consensus_on_decoding).
narrative_ontology:cs_drift_state('36135d1a-627a-46ab-a31d-f0d08adf5741', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('36135d1a-627a-46ab-a31d-f0d08adf5741', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_phonics_programs).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students benefit directly from systematic, explicit phonics instruction, as it provides the foundational decoding skills they need to access text. Without it, they are likely to struggle significantly with reading acquisition. Their 'exit' from this instructional method is often failure to read.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, local).

% These publishers profit from the widespread adoption of phonics-first curricula, which often come with scripted lessons and extensive materials. They actively promote the phonics-first approach through lobbying and marketing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_phonics_programs, beneficiary,
    organized, generational, mobile, national).

% Teachers are often required to follow scripted phonics programs, limiting their autonomy to adapt instruction based on student needs or their own pedagogical expertise. This can lead to professional dissatisfaction and a feeling of de-skilling.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, constrained, local).

% These students may find highly systematic phonics instruction redundant or boring, potentially dampening their motivation for reading. They might acquire decoding skills quickly and then be held back from engaging with more complex, meaningful texts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness, payer,
    powerless, biographical, trapped, local).

% Administrators often mandate specific curricula, including phonics-first approaches, driven by policy directives, test scores, or perceived scientific consensus. They enforce adherence to these programs within their districts or schools.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_administrators, agenda_setter,
    institutional, generational, constrained, regional).

% Advocates for whole language or balanced literacy approaches are often marginalized in policy discussions when phonics-first mandates are strong. Their pedagogical philosophy is often dismissed as 'unscientific' or 'disproven', limiting their influence on curriculum decisions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_phonics_programs).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, sequential pathway for teaching reading, ensuring all students receive explicit instruction in foundational decoding skills, which is particularly beneficial for those who do not acquire these skills implicitly.
% TRANSFER_FUNCTION: Transfers instructional authority and curriculum design from individual teachers to standardized, often commercially produced, phonics programs. It also transfers cognitive effort from guessing words in context to systematic decoding.
% ABSENT_VOICES: Advocates for whole language or purely balanced literacy approaches, who emphasize meaning-making and intrinsic motivation, are often excluded from policy-making bodies that mandate phonics-first curricula. Their concerns about student engagement and the richness of early reading experiences are frequently sidelined.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate disappeared, many schools would likely revert to more eclectic or 'balanced' approaches, and some might even re-embrace whole language. Curriculum publishers would face a more fragmented market, and teachers would regain significant autonomy in instructional design. Student outcomes, particularly for struggling readers, would become more varied depending on individual teacher expertise.
% FOUNDING_PROBLEM: A significant number of students were failing to learn to read, particularly those from disadvantaged backgrounds or with specific learning difficulties, due to insufficient or unsystematic instruction in basic decoding skills.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science research, particularly studies on phonological awareness and the 'simple view of reading', consistently corroborates the importance of decoding skills. Educational psychologists and special education experts also attest to the ongoing problem of reading failure and the efficacy of systematic phonics for many students. This corroboration comes from outside the direct beneficiaries (curriculum publishers) and is widely accepted in scientific communities, though its pedagogical implications remain contested.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is driven by the cost to teachers' professional judgment and the potential for disengagement among students who don't need intensive phonics. Suppression (0.70) reflects the strong policy mandates and curriculum enforcement that limit alternative pedagogical approaches. Theater ratio (0.10) is low because the core function of teaching decoding is genuinely performed, though the emphasis on 'fidelity' to scripted programs can sometimes become performative. The historical measurements show a rise in both extractiveness and suppression as the 'reading wars' intensified and phonics-first mandates became more widespread, stabilizing in recent years.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students struggling with decoding, this constraint is a lifeline, a pure Rope. From the perspective of teachers whose professional judgment is overridden by scripted curricula, it is a Snare. The engine's classification as Tangled Rope reflects this hybrid nature, coordinating for some while extracting from others through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness are clear beneficiaries, as this approach directly addresses their learning needs. Curriculum publishers also benefit significantly from the market for phonics programs. Teachers' professional judgment is a victim, as their autonomy is curtailed. Students with strong phonological awareness are also victims, as the pace and content may not be optimal for them. Educational administrators act as agenda-setters, enforcing the mandates. Whole language advocates are excluded, their perspectives often dismissed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_efficacy_for_all_students,
    'Does a phonics-first approach optimize reading acquisition for ALL students, or primarily for those with specific phonological deficits?',
    'Longitudinal studies comparing diverse student populations under phonics-first vs. balanced literacy approaches, measuring not just decoding but also reading comprehension, fluency, and motivation.',
    'If optimal for all, the ''victim'' status of strong phonological awareness students might be re-evaluated. If primarily for deficits, the extractiveness on other students and teacher autonomy would be seen as a cost not offset by universal benefit, pushing classification towards Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_efficacy_for_all_students, empirical, 'Whether the pedagogical benefits are universal or targeted.').

omega_variable(
    teacher_autonomy_vs_fidelity,
    'To what extent does strict adherence to scripted phonics curricula genuinely improve student outcomes versus de-skilling teachers and reducing their responsiveness to individual student needs?',
    'Studies comparing student outcomes and teacher retention/satisfaction in contexts with high vs. low curriculum fidelity requirements, controlling for teacher training and student demographics.',
    'If high fidelity offers marginal gains at high cost to teachers, the extractiveness on teachers'' professional judgment is amplified. If fidelity is critical, the suppression is justified as a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_fidelity, empirical, 'Trade-off between curriculum fidelity and teacher autonomy.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine reading of the literacy acquisition kernel, or a policy choice masquerading as a scientific imperative?',
    'Analysis of the ''reading wars'' history, policy documents, and scientific consensus statements to distinguish empirically supported claims from political or commercial advocacy.',
    'If a policy choice, the ''emerges_naturally'' aspect of the kernel is weakened, and the constraint''s classification might shift towards a more purely constructed type (e.g., Snare) if the coordination function is deemed secondary to extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing scientific consensus from policy advocacy in literacy pedagogy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel'. Its strong emphasis on phonics-first instruction creates structural pressure on other pedagogical approaches, particularly whole language and balanced literacy, by influencing curriculum mandates and resource allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
