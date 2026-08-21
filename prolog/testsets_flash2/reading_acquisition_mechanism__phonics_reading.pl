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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the reading
 *   acquisition mechanism, emphasizing explicit, systematic instruction in
 *   grapheme-phoneme correspondence as a foundational skill. It is one
 *   reading of the broader 'reading_acquisition_mechanism' kernel, which also
 *   includes 'whole_language_reading' and 'balanced_literacy_reading'. This
 *   reading asserts that foundational decoding skills are prerequisite for
 *   fluent reading and comprehension, and must be taught directly, rather
 *   than emerging implicitly. The constraint is claimed as a Rope due to its
 *   genuine coordination function in providing a clear, effective pathway to
 *   literacy, particularly for struggling readers, despite the costs it
 *   imposes on those accustomed to other methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.35).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.45).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'c61b0e63-0168-477b-8ba6-71667db82736').
narrative_ontology:cs_kernel_codification('c61b0e63-0168-477b-8ba6-71667db82736', formalized).
narrative_ontology:cs_authority_grounding('c61b0e63-0168-477b-8ba6-71667db82736', expertise).
narrative_ontology:cs_interpretation_layer_present('c61b0e63-0168-477b-8ba6-71667db82736').
narrative_ontology:cs_reading_relation('c61b0e63-0168-477b-8ba6-71667db82736', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('c61b0e63-0168-477b-8ba6-71667db82736', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('c61b0e63-0168-477b-8ba6-71667db82736', foundational, reading_is_a_learned_skill).
narrative_ontology:cs_axiom_status(reading_is_a_learned_skill, holdable).
narrative_ontology:cs_axiom_grounding('c61b0e63-0168-477b-8ba6-71667db82736', reading_is_a_learned_skill, empirically_contingent).
narrative_ontology:cs_axiom('c61b0e63-0168-477b-8ba6-71667db82736', foundational, explicit_phonics_is_necessary_for_decoding).
narrative_ontology:cs_axiom_status(explicit_phonics_is_necessary_for_decoding, holdable).
narrative_ontology:cs_axiom_grounding('c61b0e63-0168-477b-8ba6-71667db82736', explicit_phonics_is_necessary_for_decoding, empirically_contingent).
narrative_ontology:cs_reference_frame('c61b0e63-0168-477b-8ba6-71667db82736', scientific_consensus_on_decoding).
narrative_ontology:cs_drift_state('c61b0e63-0168-477b-8ba6-71667db82736', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c61b0e63-0168-477b-8ba6-71667db82736', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, early_literacy_researchers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, school_districts_with_legacy_curricula).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, simple_view_of_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, science_of_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students disproportionately benefit from explicit, systematic phonics instruction, as it provides a clear pathway to decoding that implicit methods often fail to deliver. Their 'exit' from illiteracy is directly enabled by this approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, identity_locked, local).

% Researchers whose work supports the 'Science of Reading' paradigm find their theories and empirical findings validated and adopted into policy and practice, advancing their careers and research agendas.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, early_literacy_researchers, beneficiary,
    organized, generational, analytical, global).

% Teachers previously trained in whole language or balanced literacy approaches face pressure to retrain, adopt new curricula, and change established pedagogical practices, often with limited support or resources. This can be a significant professional and emotional cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Districts that have invested heavily in non-phonics-based curricula face substantial costs for new materials, professional development, and potential political backlash from parents and educators resistant to change. Their options are to comply or face declining literacy outcomes and public criticism.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, school_districts_with_legacy_curricula, payer,
    institutional, biographical, constrained, regional).

% Parents advocating for effective reading instruction for their children often find validation and success with phonics-based approaches, seeing tangible improvements in their children's literacy skills. They benefit from clearer instructional methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, immediate, constrained, local).

% Advocates for whole language or balanced literacy, who believe reading is a natural process best learned through immersion in meaningful texts, find their pedagogical philosophy marginalized and their influence diminished in policy debates. Their professional identity is tied to these methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, evidence-based instructional approach for reading acquisition, ensuring that all students, especially those at risk, receive explicit instruction in foundational decoding skills, leading to more consistent literacy outcomes across schools and districts.
% TRANSFER_FUNCTION: Transfers instructional effort from implicit, text-based strategies to explicit, systematic phonics instruction, shifting resources (curriculum, training) towards methods supported by cognitive science research. It also transfers the burden of decoding from the student's implicit inference to the teacher's explicit instruction.
% ABSENT_VOICES: Advocates for whole language and balanced literacy are often excluded from policy-making bodies and curriculum development committees, despite their historical influence. They would argue for a broader, more integrated approach to literacy that prioritizes meaning-making from the outset.
% DISAPPEARANCE_RATIONALE: If the emphasis on explicit phonics vanished, instructional practices would likely revert to more eclectic or implicit methods, leading to a resurgence of inconsistent reading outcomes, particularly for struggling learners, and a renewed 'reading wars' debate among educators and policymakers.
% FOUNDING_PROBLEM: Persistent and widespread illiteracy, particularly among disadvantaged populations, despite decades of varied pedagogical approaches, indicating a failure to consistently teach foundational reading skills.
% FOUNDING_PROBLEM_CORROBORATION: Longitudinal studies on reading outcomes, educational achievement gaps, and cognitive science research on reading acquisition consistently corroborate the existence and persistence of the problem, independent of pedagogical preferences. Data from national and international literacy assessments also support this.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate, reflecting the costs imposed on teachers and districts to adopt new methods and curricula, but also the long-term benefits of reduced remediation. Suppression (0.45) is moderate, as it requires active policy and curriculum enforcement to shift away from entrenched pedagogical traditions. Resistance (0.7) is high, indicating ongoing debate and pushback from proponents of other methods. Accessibility collapse (0.6) is moderate, as it narrows the range of acceptable instructional approaches. Theater ratio (0.1) is low, as the instruction is genuinely functional, not performative. The temporal measurements show a gradual increase in extractiveness and suppression as the phonics-first approach gains policy traction and displaces other methods over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of struggling readers and their advocates, this constraint is a clear Rope, providing a vital pathway to literacy. From the perspective of teachers and districts deeply invested in other methods, it can feel more extractive, forcing costly changes. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and early literacy researchers are primary beneficiaries, gaining effective instruction and vindication of their research, respectively. Teachers trained in other methods and school districts with legacy curricula are payers, bearing the costs of retraining and curriculum overhaul. Parents of struggling readers are also beneficiaries, seeing direct positive outcomes. Whole language advocates are excluded, as their pedagogical philosophy is marginalized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instructional_cost_vs_remediation_cost,
    'Does the high initial instructional cost of systematic phonics truly lead to lower long-term remediation costs, or are the costs merely shifted?',
    'Longitudinal studies tracking cohorts of students through elementary and secondary education, comparing initial instructional investment with later remediation needs and overall literacy outcomes.',
    'If long-term remediation costs are not significantly reduced, the net extractiveness of this approach (considering the system as a whole) might be higher than currently estimated, suggesting a more ''tangled'' coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instructional_cost_vs_remediation_cost, empirical, 'Assessing the true cost-benefit balance of upfront phonics instruction versus later remediation.').

omega_variable(
    teacher_discretion_impact,
    'Does the narrowing of teacher discretion under systematic phonics curricula lead to a deskilling of the profession or to more equitable outcomes by standardizing effective practice?',
    'Qualitative studies on teacher professional satisfaction and autonomy, combined with quantitative analysis of student outcomes in classrooms with varying levels of prescribed curriculum fidelity.',
    'If deskilling is significant without commensurate gains in equity, the ''payer'' experience for teachers is more severe, potentially pushing the constraint towards a Snare from their seat. If equity gains are substantial, it reinforces the Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_discretion_impact, conceptual, 'Impact of narrowed teacher discretion on professional agency and student equity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative methods structural (policy mandates, funding streams) or internalized (teachers'' belief in phonics as the ''only'' way, professional identity tied to the ''Science of Reading'')?',
    'Post-policy-change trajectory: if alternative methods persist or resurface after mandates are removed, reclassify as partially internalized suppression. If they remain suppressed, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the pedagogical identity carries the suppression with them after policy shifts, making exit harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(read_tr_t1985, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(read_be_t1985, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(read_su_t1985, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel. Its rise influences the legitimacy and resource allocation for sibling readings like 'whole_language_reading' and 'balanced_literacy_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
