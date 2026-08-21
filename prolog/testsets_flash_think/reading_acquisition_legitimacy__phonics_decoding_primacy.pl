% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'phonics decoding primacy' reading of the
 *   broader 'reading acquisition legitimacy' kernel. It asserts that reading
 *   is fundamentally decoding, and legitimate instruction must explicitly
 *   teach the alphabetic principle through systematic phonics. This approach
 *   is often mandated through education policy, curriculum standards, and
 *   teacher training, positioning it as the 'science of reading.' While
 *   proponents claim it's a 'rope' that liberates readers, the metrics
 *   reflect the analytical observer's view of its operation, which involves
 *   significant extraction of pedagogical freedom and suppression of
 *   alternative methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '1d85f2bd-1822-49df-b266-bb3fa72fec6f').
narrative_ontology:cs_kernel_codification('1d85f2bd-1822-49df-b266-bb3fa72fec6f', formalized).
narrative_ontology:cs_authority_grounding('1d85f2bd-1822-49df-b266-bb3fa72fec6f', expertise).
narrative_ontology:cs_interpretation_layer_present('1d85f2bd-1822-49df-b266-bb3fa72fec6f').
narrative_ontology:cs_reading_relation('1d85f2bd-1822-49df-b266-bb3fa72fec6f', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('1d85f2bd-1822-49df-b266-bb3fa72fec6f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('1d85f2bd-1822-49df-b266-bb3fa72fec6f', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('1d85f2bd-1822-49df-b266-bb3fa72fec6f', foundational, alphabetic_principle_is_foundational).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('1d85f2bd-1822-49df-b266-bb3fa72fec6f', alphabetic_principle_is_foundational, empirically_contingent).
narrative_ontology:cs_axiom('1d85f2bd-1822-49df-b266-bb3fa72fec6f', foundational, explicit_systematic_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('1d85f2bd-1822-49df-b266-bb3fa72fec6f', explicit_systematic_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('1d85f2bd-1822-49df-b266-bb3fa72fec6f', science_of_reading_consensus).
narrative_ontology:cs_drift_state('1d85f2bd-1822-49df-b266-bb3fa72fec6f', contemporary_education_policy_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1d85f2bd-1822-49df-b266-bb3fa72fec6f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, education_policymakers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_phonics).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_acquiring_literacy).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_who_prefer_holistic_learning).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and market systematic phonics curricula, benefiting directly from policy mandates and school district adoptions. They actively lobby for phonics-first policies.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).

% Their research on the alphabetic principle and decoding is cited as the scientific basis for phonics-first approaches, granting them influence and funding. They benefit from the validation of their findings in policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers, beneficiary,
    analytical, biographical, analytical, global).

% Mandate phonics-first curricula and teacher training, often in response to perceived reading crises or lobbying efforts. They seek to improve literacy rates and demonstrate accountability.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, education_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Are aligned with the mandated approach, finding their training and methods validated. They benefit from clear instructional guidelines and resources, though their pedagogical autonomy may be reduced.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_phonics, beneficiary,
    moderate, biographical, constrained, local).

% Are the primary intended beneficiaries, receiving explicit instruction in decoding. For many, this provides a clear pathway to reading. However, those whose learning styles are not suited to this method may struggle.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_acquiring_literacy, beneficiary,
    powerless, immediate, trapped, local).

% Often advocate for phonics-first approaches, seeing them as a concrete solution for their children's reading difficulties. They benefit from clear, systematic instruction being available.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Believe reading is primarily about meaning-making and that decoding emerges naturally from immersion in authentic texts. Their pedagogical philosophy is actively suppressed by phonics-first mandates, limiting their influence and curriculum options.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Are forced to abandon or significantly alter their preferred, often more holistic, teaching methods to comply with phonics mandates. They bear the cost of retraining and loss of pedagogical autonomy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% May find the rigid, decontextualized nature of systematic phonics instruction disengaging or ineffective, potentially leading to a diminished love of reading or slower progress than with alternative methods. They pay with reduced engagement and potentially less effective learning.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_who_prefer_holistic_learning, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes reading instruction across schools and districts, ensuring a consistent, explicit approach to teaching the alphabetic principle, aiming to reduce variability in student outcomes.
% TRANSFER_FUNCTION: Transfers pedagogical authority from individual teachers and diverse educational philosophies to a prescribed, phonics-first methodology. It also transfers resources (funding, curriculum adoption) to publishers and trainers aligned with this approach.
% ABSENT_VOICES: Whole language advocates, some experienced teachers who value holistic approaches, and students whose learning styles are not served by rigid phonics are marginalized. They would argue for broader pedagogical freedom and a more balanced approach to literacy.
% DISAPPEARANCE_RATIONALE: If the mandate for phonics-decoding primacy vanished, schools would likely revert to a wider array of pedagogical approaches, curriculum markets would diversify, and teacher training programs would broaden their scope. The current structure of literacy education would fundamentally reorganize.
% FOUNDING_PROBLEM: Persistent low literacy rates, particularly among disadvantaged students, were attributed to inconsistent and insufficient explicit instruction in foundational decoding skills, leading to a call for evidence-based, systematic phonics.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (cognitive scientists, phonics publishers) assert the problem is still live, citing ongoing literacy challenges. Critics (some educational theorists, whole language advocates) argue the problem is oversimplified, that other factors are more significant, or that the solution itself creates new problems, supported by qualitative studies and historical pedagogical debates.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because while it aims to provide a skill, it does so by imposing a rigid method, extracting pedagogical autonomy from teachers, and potentially limiting the breadth of literary engagement for students. It also directs significant resources to specific curriculum providers. Suppression is high (0.75) as this approach actively marginalizes and often prohibits alternative pedagogical methods (e.g., whole language) through policy and funding mechanisms. Theater ratio is low (0.15) because the instruction is genuinely functional, even if contested. Accessibility collapse is high (0.80) as it claims to be the only 'evidence-based' path, making alternatives seem illegitimate. Resistance is moderate-high (0.60) due to ongoing 'reading wars' debates and pushback from educators and theorists favoring other approaches. The temporal measurements show a steady increase in extractiveness and suppression as phonics-first policies have gained ascendancy since the 1980s.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its proponents (e.g., cognitive science researchers, phonics publishers), this constraint is a 'rope' or even a 'mountain' – a scientifically validated, universally beneficial method for teaching reading. They see its 'extraction' as necessary coordination costs and its 'suppression' as the legitimate rejection of ineffective alternatives. From the perspective of those who advocate for whole language or balanced literacy, the same constraint operates as a 'snare' or 'tangled rope,' extracting pedagogical freedom and imposing a narrow, potentially harmful, approach.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics curriculum publishers, cognitive science researchers, and education policymakers are clear beneficiaries and agenda-setters, gaining influence, funding, and validation. Teachers trained in phonics and parents of struggling readers also benefit from clear guidelines and perceived effectiveness. Students acquiring literacy are the intended beneficiaries, though some may experience it as a cost. Conversely, whole language advocates and teachers trained in other methods are victims, experiencing suppression of their philosophies and loss of autonomy. Students who prefer holistic learning may also be victims if the method doesn't suit them.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''reading_acquisition_legitimacy'' kernel, or is it merely a policy implementation of a broader scientific consensus?',
    'Analysis of policy documents and pedagogical debates: if the ''phonics decoding primacy'' position actively forecloses or suppresses other legitimate interpretations of reading science, it is a distinct reading. If it is merely one application of a universally accepted scientific principle, it is a less contested constraint.',
    'If a distinct reading, its classification is more likely to reflect contestation and extraction. If a policy implementation of a consensus, it might lean closer to a Rope or even Mountain (from the scientific perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing a specific reading from a general scientific consensus.').

omega_variable(
    efficacy_vs_pedagogical_freedom,
    'Does the universal application of systematic phonics genuinely serve all learners optimally, or does it extract pedagogical freedom from teachers and diverse learning experiences from students without universal benefit?',
    'Longitudinal studies comparing diverse pedagogical approaches across varied student populations, including qualitative data on student engagement and teacher satisfaction. Analysis of ''implementation fidelity'' vs. ''adaptive teaching'' outcomes.',
    'If universal benefit is not demonstrated, the measured extractiveness and suppression are more clearly unjustified, pushing the classification towards Snare. If universal benefit is robust, the constraint leans more towards a Rope, with extraction seen as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_vs_pedagogical_freedom, empirical, 'Assessing the true universal efficacy of phonics-first vs. its costs in pedagogical diversity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative pedagogies primarily structural (policy mandates, funding restrictions) or internalized (teachers feeling pressure to conform, self-censoring methods)?',
    'Surveys and interviews with teachers in jurisdictions with varying policy strictness. Analysis of curriculum choices in contexts where mandates are relaxed or absent. If alternative methods persist after structural barriers are removed, internalized suppression is significant.',
    'If internalized suppression is high, the constraint''s effective suppression is more pervasive and harder to dislodge, potentially amplifying its extractive power even if formal mandates weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of pedagogical alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, curriculum_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_training_accreditation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
