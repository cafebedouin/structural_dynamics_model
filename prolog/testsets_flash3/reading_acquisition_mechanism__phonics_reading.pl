% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the reading
 *   acquisition mechanism, emphasizing explicit, systematic instruction in
 *   grapheme-phoneme correspondence as a foundational skill. It is one
 *   reading of the broader 'reading_acquisition_mechanism' kernel, alongside
 *   'whole_language_reading' and 'balanced_literacy_reading'. This reading
 *   asserts that effective reading instruction must prioritize decoding
 *   skills before broader comprehension strategies. The constraint is
 *   classified as a Tangled Rope because it genuinely coordinates a solution
 *   to a collective action problem (how to teach reading effectively) but
 *   does so with significant extraction from teachers and students accustomed
 *   to other methods, requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Instruction Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'c58c3bac-950e-47ee-b9bc-9429de93bfff').
narrative_ontology:cs_kernel_codification('c58c3bac-950e-47ee-b9bc-9429de93bfff', formalized).
narrative_ontology:cs_authority_grounding('c58c3bac-950e-47ee-b9bc-9429de93bfff', expertise).
narrative_ontology:cs_interpretation_layer_present('c58c3bac-950e-47ee-b9bc-9429de93bfff').
narrative_ontology:cs_reading_relation('c58c3bac-950e-47ee-b9bc-9429de93bfff', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('c58c3bac-950e-47ee-b9bc-9429de93bfff', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('c58c3bac-950e-47ee-b9bc-9429de93bfff', foundational, decoding_is_primary_skill).
narrative_ontology:cs_axiom_status(decoding_is_primary_skill, holdable).
narrative_ontology:cs_axiom_grounding('c58c3bac-950e-47ee-b9bc-9429de93bfff', decoding_is_primary_skill, empirically_contingent).
narrative_ontology:cs_axiom('c58c3bac-950e-47ee-b9bc-9429de93bfff', foundational, systematic_instruction_is_necessary).
narrative_ontology:cs_axiom_status(systematic_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c58c3bac-950e-47ee-b9bc-9429de93bfff', systematic_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('c58c3bac-950e-47ee-b9bc-9429de93bfff', science_of_reading_consensus).
narrative_ontology:cs_drift_state('c58c3bac-950e-47ee-b9bc-9429de93bfff', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c58c3bac-950e-47ee-b9bc-9429de93bfff', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, educational_publishers_phonics_materials).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, students_with_prior_exposure_to_other_methods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate the adoption of phonics-first curricula, often in response to literacy crises or research consensus. They bear the political cost of implementation but benefit from improved literacy rates and public perception.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, educational_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit disproportionately from explicit, systematic phonics instruction, as it provides a clear pathway to decoding that other methods often fail to deliver. Their 'exit' from illiteracy is facilitated by this constraint.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).

% Must retrain, adapt their pedagogy, and often discard existing materials and lesson plans. They bear the cost of professional development and the psychological burden of changing established practice, often feeling their professional autonomy is curtailed.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Profit from the increased demand for phonics-based textbooks, workbooks, and digital resources. They actively lobby policy makers and fund research that supports phonics-first approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, educational_publishers_phonics_materials, beneficiary,
    organized, biographical, mobile, national).

% May experience confusion or frustration when transitioning from a different instructional approach (e.g., whole language) to a phonics-first method, requiring them to unlearn prior strategies.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, students_with_prior_exposure_to_other_methods, payer,
    powerless, immediate, trapped, local).

% May advocate for alternative literacy approaches (e.g., balanced literacy) based on their own educational philosophies or their children's learning styles, but often find their voices marginalized in policy debates dominated by the 'Science of Reading' movement.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents_advocating_for_other_methods, excluded,
    moderate, biographical, constrained, local).

% Provide the scientific evidence base for the efficacy of phonics instruction, influencing policy makers and curriculum developers. They benefit from the validation of their research and increased funding for related studies.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, cognitive_scientists_supporting_phonics, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a consistent, evidence-based approach to early reading instruction across diverse classrooms and schools, aiming to standardize foundational literacy skills.
% TRANSFER_FUNCTION: Transfers instructional effort and curriculum design towards explicit phonics, shifting resources from other literacy approaches (e.g., incidental decoding, whole-text immersion) to systematic skill-building. It also transfers the burden of initial decoding from implicit discovery to explicit teaching.
% ABSENT_VOICES: Advocates for whole language and balanced literacy approaches, including many experienced educators and some researchers, are often excluded from policy-making bodies that mandate phonics-first curricula. They would argue for broader pedagogical flexibility and a more holistic view of reading.
% DISAPPEARANCE_RATIONALE: If the mandate for phonics-first instruction vanished, schools would likely revert to a wider array of pedagogical approaches, including balanced literacy or whole language. Curriculum publishers would diversify their offerings, and teacher training programs would broaden their scope. Literacy outcomes, particularly for struggling readers, might diverge more widely.
% FOUNDING_PROBLEM: Persistent and widespread illiteracy, particularly among disadvantaged populations, despite decades of varied instructional approaches, leading to a 'reading crisis' in many educational systems.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers, literacy advocates, and government reports consistently corroborate the ongoing challenge of ensuring universal reading proficiency. While the severity is debated, the problem of literacy gaps remains widely acknowledged by parties outside the direct beneficiaries of phonics materials.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial due to the high initial instructional cost for teachers (retraining, curriculum changes) and the potential for disengagement among students who might thrive with more holistic approaches. Suppression (0.70) is high because policy mandates often limit pedagogical freedom and curriculum choice, actively suppressing alternative methods. Theater ratio (0.10) is low, as the instruction is generally direct and functional, with little performative overhead. Accessibility collapse (0.40) is moderate; while it narrows the path, it doesn't completely eliminate alternatives, as some teachers may still integrate other methods informally. Resistance (0.55) is moderate, reflecting ongoing debates and pushback from educators and parents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of struggling readers and policy makers, this constraint is a necessary coordination mechanism to ensure foundational literacy. From the perspective of many teachers and some students, it is an extractive mandate that limits pedagogical flexibility and may not suit all learners. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational policy makers (agenda setters) benefit from perceived improvements in literacy rates. Struggling readers are primary beneficiaries, gaining access to a proven decoding method. Educational publishers of phonics materials are clear beneficiaries. Teachers trained in other methods and students with prior exposure to those methods are payers, bearing the costs of adaptation and potential disruption. Parents advocating for other methods are excluded, their preferences often overridden by policy mandates. Cognitive scientists supporting phonics act as observers, providing the evidence base.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to solve the 'reading crisis' by providing a reliable method for literacy acquisition. While the problem of literacy gaps remains live, the 'phonics-first' approach is contested as the sole solution. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from teachers) or a Snare (ignoring the genuine coordination benefit for struggling readers). The ongoing debate about the 'Science of Reading' highlights the tension between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_autonomy_vs_standardization,
    'To what extent does mandating a phonics-first approach genuinely improve overall literacy outcomes versus merely shifting the burden of instruction and curtailing teacher professional autonomy?',
    'Longitudinal studies comparing literacy outcomes and teacher retention/satisfaction in systems with strict phonics mandates versus those with more flexible pedagogical frameworks, controlling for socioeconomic factors.',
    'If outcomes are not significantly better, or if teacher attrition increases, the constraint''s extractiveness from teachers would be re-evaluated as less justified by coordination benefits, potentially shifting classification towards a Snare for the teaching profession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_autonomy_vs_standardization, empirical, 'Trade-off between standardized instruction and teacher autonomy/student diversity.').

omega_variable(
    curriculum_capture_by_publishers,
    'Is the increased adoption of phonics-first curricula driven primarily by scientific consensus on efficacy, or by the lobbying efforts and market power of educational publishers specializing in phonics materials?',
    'Analysis of campaign finance data, lobbying expenditures, and the composition of curriculum adoption committees, alongside independent meta-analyses of reading research that are not funded by publishers.',
    'If publisher influence is found to be a dominant driver, the ''educational_publishers_phonics_materials'' seat''s directionality would shift further towards full beneficiary, and the constraint''s overall extractiveness would be seen as more concentrated and less justified by pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(curriculum_capture_by_publishers, empirical, 'Influence of commercial interests on pedagogical policy.').

omega_variable(
    reading_acquisition_kernel_framing,
    'Is the ''reading_acquisition_mechanism'' kernel best understood as a purely cognitive process (phonics-first), a socio-cultural practice (whole language), or an integrated system (balanced literacy)?',
    'A shift in the dominant paradigm within cognitive science and educational psychology, or a broad consensus across disciplines on the most effective and equitable approach to literacy.',
    'If a different framing gains dominance, this ''phonics_reading'' constraint would be re-evaluated against that new kernel, potentially altering its perceived coordination function and extractiveness, and possibly leading to its reclassification or obsolescence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_acquisition_kernel_framing, conceptual, 'The fundamental conceptual framing of how reading is acquired.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
