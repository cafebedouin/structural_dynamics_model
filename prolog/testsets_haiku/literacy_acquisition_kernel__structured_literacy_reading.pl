% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Acquisition (Orton-Gillingham Kernel Reading)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Structured literacy reading instruction—explicit, systematic, cumulative
 *   training in phonological awareness, phonics, fluency, vocabulary, and
 *   comprehension—originated in the Orton-Gillingham tradition and is now the
 *   dominant evidence-based framework for reading intervention in the United
 *   States. The constraint combines genuine coordination (solving the
 *   reading-acquisition problem for students with dyslexia) with asymmetric
 *   extraction (imposing specialized teacher-training requirements,
 *   curriculum adoption costs, and instructional authority shifts on general
 *   education). This reading (structured_literacy_reading) is one of four
 *   competing readings of the contested kernel literacy_acquisition_kernel,
 *   alongside balanced_literacy_reading, phonics_reading, and
 *   whole_language_reading. Each reading is a structurally distinct
 *   constraint: each has its own ε value, beneficiary/victim structure, and
 *   persistence mechanism. This reading is authored as its own constraint,
 *   with the kernel-reading structure recorded in cs_structure and omega
 *   variables.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: powerless; trapped exit; beneficiary (intervention reduces failure); primary target of the constraint's coordination function.
 *   - general_education_teachers: moderate power; constrained exit; payer (required to implement or defer to specialists); carry the enforcement burden through professional development and instructional change.
 *   - literacy_specialists_and_interventionists: organized; mobile exit; agenda-setter + beneficiary (control instructional standards, design scope-and-sequence, secure employment); structural winners.
 *   - reading_science_research_community: powerful; arbitrage exit; beneficiary (prestige, funding, policy influence); control the legitimacy apparatus.
 *   - whole_language_educators: moderate; constrained exit; excluded (marginalized from policy, treated as outdated); would contest the constraint but lack institutional voice.
 *   - policy_makers: institutional; analytical exit; agenda_setter (enforce through standards, funding, special education law); operationalize the constraint.
 *   - dyslexia_advocacy_organizations: organized; mobile exit; agenda_setter (drove policy adoption, shape definitions of dyslexia and reading science); structural authors of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.55).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Acquisition (Orton-Gillingham Kernel Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '58420f3f-5d5f-4124-a312-6c1635635e5b').
narrative_ontology:cs_kernel_codification('58420f3f-5d5f-4124-a312-6c1635635e5b', formalized).
narrative_ontology:cs_authority_grounding('58420f3f-5d5f-4124-a312-6c1635635e5b', expertise).
narrative_ontology:cs_interpretation_layer_present('58420f3f-5d5f-4124-a312-6c1635635e5b').
narrative_ontology:cs_reading_relation('58420f3f-5d5f-4124-a312-6c1635635e5b', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('58420f3f-5d5f-4124-a312-6c1635635e5b', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('58420f3f-5d5f-4124-a312-6c1635635e5b', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('58420f3f-5d5f-4124-a312-6c1635635e5b', foundational, phonological_awareness_necessary_foundational).
narrative_ontology:cs_axiom_status(phonological_awareness_necessary_foundational, holdable).
narrative_ontology:cs_axiom_grounding('58420f3f-5d5f-4124-a312-6c1635635e5b', phonological_awareness_necessary_foundational, empirically_contingent).
narrative_ontology:cs_axiom('58420f3f-5d5f-4124-a312-6c1635635e5b', foundational, five_component_cumulative_model).
narrative_ontology:cs_axiom_status(five_component_cumulative_model, holdable).
narrative_ontology:cs_axiom_grounding('58420f3f-5d5f-4124-a312-6c1635635e5b', five_component_cumulative_model, empirically_contingent).
narrative_ontology:cs_axiom('58420f3f-5d5f-4124-a312-6c1635635e5b', secondary, dyslexia_requires_specialized_intervention).
narrative_ontology:cs_axiom_status(dyslexia_requires_specialized_intervention, holdable).
narrative_ontology:cs_axiom_grounding('58420f3f-5d5f-4124-a312-6c1635635e5b', dyslexia_requires_specialized_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('58420f3f-5d5f-4124-a312-6c1635635e5b', reading_science_phonological_basis).
narrative_ontology:cs_drift_state('58420f3f-5d5f-4124-a312-6c1635635e5b', contemporary_policy_universalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58420f3f-5d5f-4124-a312-6c1635635e5b', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_specific_learning_disabilities).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, underfunded_school_districts).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).

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
 *   Extractiveness rises from 0.25 (1970, niche specialty) to 0.68 (2025, dominant policy framework) because the constraint's scope expanded from specialized dyslexia intervention to universal reading instruction. The theater_ratio remains moderate (0.28) because the constraint has real pedagogical content—phonological awareness instruction does improve reading outcomes—but a growing share of enforcement activity defends the exclusion of alternative reading approaches and the authority of specialized literacy professionals, not the core phonological-awareness science itself. Suppression rises sharply (0.15→0.55) because enforcement against whole-language and balanced-literacy approaches intensified: policy mandates, curriculum adoption requirements, teacher-certification specifications, and research-funding priorities actively suppress alternative reading frameworks. Accessibility_collapse is high (0.72) because once systematic phonics is framed as 'science-based' and alternatives as 'unsupported,' teachers and districts perceive few legitimate alternatives; the closure is structural (policy-enforced certification requirements, funding leverage, adoption mandates) and epistemic (reading science frames alternatives as discredited). Resistance is moderate-high (0.61) because balanced-literacy advocates, whole-language educators, and some reading researchers actively resist structured-literacy dominance, though they lack institutional power to reverse it. The measurement series spans the interval [1970, 2025] on a single time grid to enable lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the dyslexia-advocacy, specialist, and research-science seats, the constraint is genuine coordination—a life-changing intervention for students who would otherwise face reading failure. From the general-education-teacher and underfunded-district seats, the same structure imposes a costly, non-negotiable obligation that displaces teacher autonomy and competes with other instructional priorities. From the whole-language-educator seat, the constraint is extractive suppression of a legitimate pedagogical alternative and a de-professionalization of teaching (replacing craft judgment with scripted, specialist-controlled curricula). The engine computes these divergent classifications from the structural data (beneficiary/victim declarations + power + exit options); the claim of 'tangled_rope' reflects genuine coordination for students with dyslexia, but the authored metrics and structural asymmetries (specialist control, teacher deskilling, alternative suppression) support the extraction half of the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia and struggling readers (beneficiaries, powerless, trapped exit) sit at the low-d end (near 0.0, subsidy/benefit end) because the constraint's operation subsidizes their reading outcomes relative to no intervention. Literacy specialists and researchers (beneficiaries, organized/powerful, mobile exit) sit near the low-d end nominally (they benefit) but experience the constraint as enabling their professional authority, so their effective directionality is mixed; they are better modeled as agenda-setters whose power and mobility amplify the constraint's persistence. General-education teachers (payers, moderate power, constrained exit) sit at the high-d end (near 1.0, extraction/target end) because they bear costs (training burden, curriculum constraints, authority loss) and cannot easily exit. Underfunded districts (payers, moderate power, constrained exit) sit at the high-d end because they must allocate scarce budgets to specialized instruction or accept reading-failure penalties. Whole-language educators (excluded, moderate power, constrained exit) sit at the high-d end because they are actively suppressed and carry professional costs. Directionality overrides are not required—the derivation from beneficiary/victim + power + exit produces accurate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Structured literacy reading is NOT a mandatrophy candidate at the current interval end (2025), because the founding problem (reading failure in students with dyslexia) remains live and the constraint actively solves it (evidence-based effectiveness for the target population is real). However, a mandatrophy trajectory is forming: the constraint's scope has expanded from targeted intervention (students with diagnosed dyslexia) to universal reading instruction (all students, justified via 'universal design for learning'), and the constraint's enforcement increasingly suppresses alternative reading approaches regardless of their effectiveness for typically-developing readers. If the constraint continues to expand in scope while its coordinating function becomes decoupled from the original problem (serving only as a teacher-control mechanism or publisher revenue stream), a mandatrophy reclassification would be warranted. The omega variable addressing this trajectory is detailed below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the structured_literacy_reading constraint a distinct reading of the literacy_acquisition_kernel, or is it a variant of the phonics_reading with added vocabulary-comprehension components?',
    'Historical and textual analysis of the Orton-Gillingham tradition, phonics-revival movement (post-2000s ''science of reading''), and the claims made by structured-literacy advocates about what distinguishes their approach from pure phonics instruction. Determine whether the five-component model (phonological awareness, phonics, fluency, vocabulary, comprehension) is a fundamentally different reading or a refinement of phonics_reading.',
    'If a distinct reading: this constraint''s classification stands. If a variant of phonics_reading: the structured_literacy_reading story should be merged with phonics_reading or reclassified as a proxy reading with lower epistemic weight. The distinction is material for understanding the kernel contest—is it a four-way dispute or a three-way dispute with a refined variant?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether structured literacy is a fourth independent reading or a variant of phonics.').

omega_variable(
    universal_applicability_claim,
    'Is structured literacy reading instruction equally effective and necessary for typically-developing readers without dyslexia, or is it optimized for and necessary only for students with reading disabilities?',
    'Meta-analysis of reading intervention studies comparing structured literacy vs. balanced or meaning-centered instruction for typically-developing readers. Examination of outcome data disaggregated by disability status. Cross-national comparative studies of reading systems and instruction methods that produce strong outcomes without structured-literacy frameworks.',
    'If structured literacy is equally effective for all students: the constraint''s claim of universal applicability is validated and the constraint''s beneficiary set expands to all students. If structured literacy is optimal only for students with specific reading disabilities: the constraint''s scope should be narrowed to targeted intervention, reducing the extraction on general-education teachers and underfunded districts. The universal claim is currently contested; clarifying it would shift the balance between coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_claim, empirical, 'Whether structured literacy is universally necessary or optimized for dyslexia intervention.').

omega_variable(
    suppression_of_alternative_reading_frameworks,
    'Do policy implementations of structured literacy actively suppress scientifically-defensible alternative reading approaches (balanced literacy, meaning-centered instruction, language-experience approaches), or do they neutrally elevate structured literacy without denigrating alternatives?',
    'Policy document analysis of state reading standards, curriculum adoption requirements, teacher-certification specifications, and funding mechanisms. Interview data from teachers and administrators about perceived latitude to use alternative methods. Comparison of research-funding streams and publication bias toward structured-literacy studies vs. alternatives.',
    'If suppression is active and systematic: the constraint''s suppression metric is justified and the constraint qualifies more strongly as snare-like (pure extraction riding on coordination cover). If suppression is minimal and alternatives remain available: the constraint is more defensible as tangled_rope (genuine coordination with asymmetric costs, but not coercive suppression). The current measurement reflects active suppression; if this is empirically wrong, the metric and type classification must be revised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_alternative_reading_frameworks, empirical, 'Whether structured literacy suppresses alternative reading instruction methods.').

omega_variable(
    mandatrophy_scope_expansion_trajectory,
    'Will the structured-literacy-reading constraint continue to expand in scope from targeted dyslexia intervention to universal reading instruction, and if so, will the constraint''s coordinating function decay relative to its enforcement function?',
    'Longitudinal tracking of state policy changes, teacher-certification requirements, curriculum adoption mandates, and funding allocations over the next 10–15 years. Tracking of outcome data for typically-developing readers under structured-literacy instruction vs. alternatives. Monitoring of specialist-profession expansion and the balance between phonological-awareness research and other reading-science topics.',
    'If scope expands while coordination function decays: the constraint becomes a candidate for mandatrophy reclassification (constraint persists but founding problem is solved or scope-shifted; persistence is driven by specialist authority and policy inertia, not genuine coordination). If scope stabilizes at targeted intervention and coordination function remains strong: the constraint remains tangled_rope with live mandates. This trajectory is the most material uncertainty for the constraint''s long-term classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_scope_expansion_trajectory, empirical, 'Whether structured literacy will expand in scope while its coordinating function decays.').

omega_variable(
    dyslexia_identity_lock_mechanism,
    'To what extent does the structured-literacy-reading constraint lock students with dyslexia into an identity as ''reading-disabled'' requiring specialized (often segregated or resource-room based) instruction, rather than integrating them into general literacy development?',
    'Longitudinal studies of student outcomes, self-concept, and educational trajectories comparing structured-literacy intervention in inclusive vs. pull-out settings. Qualitative interviews with students about how the constraint shapes their identity and self-perception. Measurement of attrition from specialized instruction and rates of reclassification to general education.',
    'If identity lock is substantial: the constraint carries an internalization/suppression cost (post-exit trajectory) that is not captured in the suppression metric—the cost persists even after specialized instruction ends. If identity lock is minimal: the constraint operates primarily as an external structural mechanism. High identity lock would suggest the suppression metric understates the constraint''s actual cost to beneficiaries and would support a higher suppression value and a snare-leaning classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dyslexia_identity_lock_mechanism, empirical, 'Whether structured literacy locks students with dyslexia into a disability identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1970, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(lite_tr_t2018, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(lite_tr_t2025, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t1970, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(lite_be_t2018, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement(lite_be_t2025, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1970, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(lite_su_t2018, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(lite_su_t2025, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, special_education_referral_pipeline).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_training_and_certification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested literacy_acquisition_kernel. The kernel-level claim is: 'Reading acquisition requires [method X].' This reading specifies structured, systematic, cumulative instruction in five components (phonological awareness, phonics, fluency, vocabulary, comprehension), designed for dyslexia but claimed as universally applicable. Sibling readings include whole_language_reading (meaning-emergence model), phonics_reading (decoding-first model), and balanced_literacy_reading (complementary-components model). Each reading is a structurally distinct constraint with its own ε, beneficiary/victim structure, and enforcement mechanism. The reading_relations in cs_structure.reading_relations document the structural relationships between this reading and each sibling. The axioms in cs_structure.axioms document the foundational normative claims distinguishing this reading from siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
