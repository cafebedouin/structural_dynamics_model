% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'whole language' approach to reading
 *   acquisition, a pedagogical philosophy that gained prominence from the
 *   1970s. It posits that children learn to read naturally through immersion
 *   in authentic texts, with decoding skills emerging implicitly from
 *   meaningful engagement. This story is one reading of the broader
 *   'reading_acquisition_mechanism' kernel, focusing on the structural
 *   implications of the whole language approach, particularly its costs for
 *   struggling learners and its resistance to explicit phonics instruction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.75).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'c1cc47a1-a0dd-45da-bbd5-920933e35a32').
narrative_ontology:cs_kernel_codification('c1cc47a1-a0dd-45da-bbd5-920933e35a32', implicit).
narrative_ontology:cs_authority_grounding('c1cc47a1-a0dd-45da-bbd5-920933e35a32', practice).
narrative_ontology:cs_interpretation_layer_present('c1cc47a1-a0dd-45da-bbd5-920933e35a32').
narrative_ontology:cs_reading_relation('c1cc47a1-a0dd-45da-bbd5-920933e35a32', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('c1cc47a1-a0dd-45da-bbd5-920933e35a32', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('c1cc47a1-a0dd-45da-bbd5-920933e35a32', foundational, reading_is_natural_language_process).
narrative_ontology:cs_axiom_status(reading_is_natural_language_process, holdable).
narrative_ontology:cs_axiom_grounding('c1cc47a1-a0dd-45da-bbd5-920933e35a32', reading_is_natural_language_process, conventional).
narrative_ontology:cs_axiom('c1cc47a1-a0dd-45da-bbd5-920933e35a32', foundational, decoding_emerges_implicitly).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly, holdable).
narrative_ontology:cs_axiom_grounding('c1cc47a1-a0dd-45da-bbd5-920933e35a32', decoding_emerges_implicitly, empirically_contingent).
narrative_ontology:cs_reference_frame('c1cc47a1-a0dd-45da-bbd5-920933e35a32', natural_language_acquisition_paradigm).
narrative_ontology:cs_drift_state('c1cc47a1-a0dd-45da-bbd5-920933e35a32', science_of_reading_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c1cc47a1-a0dd-45da-bbd5-920933e35a32', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_autonomy_proponents).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of the whole language philosophy, often educational theorists and teacher trainers, who believe reading is a natural process akin to spoken language acquisition. They set curriculum standards and influence teacher education, actively promoting the approach and resisting alternatives.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, global).

% Teachers and administrators who value pedagogical freedom and a less prescriptive curriculum. Whole language allows for more flexible, child-centered instruction, aligning with their professional identity and reducing pressure for standardized, explicit methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_autonomy_proponents, beneficiary,
    organized, biographical, constrained, national).

% Children who do not implicitly acquire decoding skills and are disproportionately harmed by the lack of explicit phonics instruction. They face significant academic and emotional costs, often requiring expensive and intensive remediation outside the school system.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Advocate for their children, often facing frustration and high costs for private tutoring when schools adhere strictly to whole language. They are constrained by school district policies and the prevailing pedagogical philosophy, with limited options for systemic change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% Researchers in cognitive psychology and neuroscience who study reading acquisition. Their empirical findings often contradict the implicit learning tenets of whole language, but their influence on pedagogical practice can be slow and indirect.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% Educators, researchers, and parent groups who champion explicit, systematic phonics instruction. They are often marginalized or actively resisted by whole language proponents in curriculum debates, despite a growing body of evidence supporting their position.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_advocates, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a holistic, meaning-centered approach to literacy instruction that fosters a love of reading and integrates reading with other language arts, aiming for natural language acquisition.
% TRANSFER_FUNCTION: Transfers pedagogical authority and flexibility to teachers, allowing them to focus on authentic texts and student engagement. It transfers the burden of decoding skill acquisition to the individual child's implicit learning capacity, and remediation costs to families of struggling learners.
% ABSENT_VOICES: Cognitive scientists and phonics advocates were historically excluded from curriculum design processes, and parents of struggling readers often feel unheard. They would argue for evidence-based, explicit instruction and accountability for reading outcomes.
% DISAPPEARANCE_RATIONALE: If the whole language approach vanished overnight, pedagogical practices in many schools would undergo a significant shift towards more explicit, systematic phonics instruction. Teacher training, curriculum materials, and assessment methods would all reorganize, fundamentally altering the landscape of early literacy education.
% FOUNDING_PROBLEM: Traditional phonics instruction was perceived as overly mechanistic, decontextualized, and detrimental to children's motivation and comprehension, leading to a 'drill and kill' approach that stifled a love of reading.
% FOUNDING_PROBLEM_CORROBORATION: Whole language proponents maintain that the problem of disengaged readers persists and their approach remains the best solution. However, cognitive scientists and phonics advocates, supported by extensive meta-analyses and longitudinal studies, argue that the founding problem was misdiagnosed and that whole language created new, more severe problems for many learners; independent research from outside the benefiting parties corroborates this shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'rope' because whole language proponents genuinely believe it offers a superior, more natural way to coordinate reading instruction. However, the metrics reflect its actual operation: extractiveness is high (0.68) due to the significant remediation costs borne by struggling readers and their families. Suppression (0.75) is also high, as the approach actively de-emphasizes or excludes explicit phonics instruction, limiting alternatives. Theater ratio (0.40) reflects a growing performative aspect, where the 'love of reading' narrative sometimes masks inadequate skill development. Accessibility collapse (0.70) is severe for children who do not implicitly acquire decoding, as the primary instructional path fails them. Resistance (0.80) is high due to ongoing 'reading wars' and advocacy from parents and scientists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates and teachers, the approach is a beneficial coordination mechanism that fosters engaged readers and teacher autonomy. From the perspective of struggling readers and their parents, it operates as a highly extractive and suppressive system that fails to provide foundational skills, leading to significant personal and financial costs. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and teacher autonomy proponents are beneficiaries, gaining influence and pedagogical freedom. Struggling readers and their parents are victims, bearing the costs of inadequate instruction and remediation. Cognitive scientists and phonics advocates act as observers or excluded voices, providing critical analysis and advocating for alternative approaches.
 *
 * MANDATROPHY ANALYSIS:
 *   The initial mandate was to foster a love of reading and make literacy instruction more engaging. While this goal remains partially live, the constraint has drifted, with the implicit learning tenet leading to significant, unacknowledged extraction from a subset of learners. The persistence of the approach, despite empirical challenges, suggests a degree of mandatrophy where the original coordination function is overshadowed by the costs it imposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_learning_efficacy,
    'To what extent do decoding skills genuinely emerge implicitly from exposure to authentic texts for all learners, particularly those with specific learning differences?',
    'Longitudinal studies comparing reading outcomes in whole language vs. explicit phonics environments, particularly for diverse learner populations, and neuroscientific research on the mechanisms of reading acquisition.',
    'If implicit learning is insufficient for a significant portion of learners, the extractiveness of whole language is higher than currently measured, and its coordination function is fundamentally flawed for those individuals, pushing its classification towards Snare for that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_learning_efficacy, empirical, 'Empirical validity of implicit decoding acquisition for all learners.').

omega_variable(
    pedagogical_autonomy_cost,
    'Is the benefit of increased teacher autonomy under whole language outweighed by the systemic costs of reading failure for struggling students?',
    'Policy analysis comparing teacher satisfaction and retention rates with student literacy outcomes across different pedagogical mandates, alongside economic analysis of societal costs associated with low literacy.',
    'If the societal costs of reading failure are demonstrably higher than the benefits of teacher autonomy, the constraint''s overall social utility is negative, reinforcing its extractive nature and challenging its ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_autonomy_cost, preference, 'Trade-off between teacher autonomy and student literacy outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of explicit phonics instruction structural (e.g., curriculum mandates, textbook selection) or internalized (e.g., teachers'' beliefs about best practice, professional identity)?',
    'Post-policy-change analysis: if explicit phonics instruction remains de-emphasized after mandates are removed, it suggests internalized suppression. If it rapidly re-emerges, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher and more resistant to policy changes, indicating a deeper identity-lock for proponents. If structural, policy interventions could more readily reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative pedagogical methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel, focusing on the whole language approach. It is structurally distinct from the phonics and balanced literacy readings, which represent alternative pedagogical philosophies with different beneficiary/victim structures and empirical bases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
