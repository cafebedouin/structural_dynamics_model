% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'whole language' pedagogical philosophy,
 *   which posits that reading is a natural process of meaning-making, and
 *   that decoding skills emerge organically through immersion in authentic
 *   literature. It contrasts with approaches emphasizing explicit phonics.
 *   While proponents claim it as a beneficial coordination mechanism (a
 *   'rope'), its operational metrics, particularly for struggling learners,
 *   reveal significant extraction and suppression of alternative methods. The
 *   temporal measurements reflect its rise in influence, the subsequent
 *   increase in observed extraction (due to widespread reading failure), and
 *   a slight decline as resistance mounted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'd336d9b7-4a22-4fc7-b1a2-b5b41d1047de').
narrative_ontology:cs_kernel_codification('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', implicit).
narrative_ontology:cs_authority_grounding('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', expertise).
narrative_ontology:cs_interpretation_layer_present('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de').
narrative_ontology:cs_reading_relation('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_axiom('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', foundational, reading_is_meaning_construction).
narrative_ontology:cs_axiom_status(reading_is_meaning_construction, holdable).
narrative_ontology:cs_axiom_grounding('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', reading_is_meaning_construction, deontological).
narrative_ontology:cs_axiom('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', foundational, decoding_emerges_naturally).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally, holdable).
narrative_ontology:cs_axiom_grounding('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', decoding_emerges_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', child_centered_meaning_making).
narrative_ontology:cs_drift_state('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', contemporary_science_of_reading_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d336d9b7-4a22-4fc7-b1a2-b5b41d1047de', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, publishers_of_authentic_literature).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_preferring_less_structured_pedagogy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_implementing_whole_language).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_implementing_whole_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of the whole language philosophy, often academics and teacher educators, who champion the idea that reading is a natural process of meaning-making and that explicit phonics instruction is unnecessary or even detrimental. They set pedagogical standards and influence curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Teachers who adopt whole language methods, often finding them more engaging and less rote than traditional phonics. They benefit from the pedagogical freedom but may bear costs if their students struggle or if policy shifts away from whole language.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_implementing_whole_language, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_implementing_whole_language, payer).

% Children who do not naturally 'discover' decoding through immersion in authentic literature. They bear the primary cost of this approach through delayed literacy acquisition, academic struggles, and potential long-term educational disadvantage.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Advocate for their children, often seeking alternative instruction or remediation when whole language methods fail. They bear the emotional and financial costs of their children's struggles and the effort to find effective alternatives.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% Researchers, educators, and parent groups who champion explicit, systematic phonics instruction based on cognitive science. They are often marginalized or dismissed within whole language-dominant educational systems, despite growing empirical support for their position.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates, excluded,
    organized, generational, mobile, national).

% Benefit from the emphasis on using 'real books' in classrooms, as it drives demand for their products over phonics-specific readers or workbooks. They have a vested interest in the persistence of whole language pedagogy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, publishers_of_authentic_literature, beneficiary,
    powerful, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified pedagogical framework for early literacy instruction, emphasizing engagement with meaningful texts and fostering a love of reading, rather than fragmented skill drills.
% TRANSFER_FUNCTION: Transfers pedagogical authority from prescriptive, skill-based curricula to teacher-as-facilitator roles, and transfers educational resources towards authentic literature and away from explicit phonics materials.
% ABSENT_VOICES: Cognitive scientists specializing in reading acquisition, parents of children with dyslexia, and advocates for evidence-based reading instruction are often excluded from policy-making and curriculum design in whole language-dominant contexts.
% DISAPPEARANCE_RATIONALE: If the whole language philosophy and its associated practices vanished overnight, literacy instruction would immediately shift towards more explicit and systematic approaches, curriculum materials would change, and teacher training would be fundamentally reoriented. The entire landscape of early reading education would reorganize.
% FOUNDING_PROBLEM: Traditional phonics instruction was often perceived as rote, boring, and disconnected from the joy and purpose of reading, leading to disengaged learners who could 'bark at print' but not comprehend.
% FOUNDING_PROBLEM_CORROBORATION: Whole language proponents assert the problem of disengaged readers is still live, citing the importance of intrinsic motivation. Critics (cognitive scientists, phonics advocates) argue that while engagement is important, the founding problem of disconnected instruction has been largely addressed by modern, engaging phonics programs, and that whole language created a new, more severe problem of reading failure for many children. Independent research on reading acquisition from outside the benefiting parties largely corroborates the critics' view regarding efficacy for all learners.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'rope' because whole language aims to coordinate a holistic, engaging approach to literacy. However, the metrics reflect its real-world impact: high extractiveness for struggling readers who fail to acquire decoding skills, high suppression of explicit phonics instruction, and a moderate theater ratio as the 'natural emergence' narrative is maintained despite empirical challenges. The resistance metric reflects the 'reading wars' and the pushback from cognitive science and parent advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, the constraint is a beneficial 'rope' that fosters a love of reading and natural development. From the perspective of struggling readers and phonics advocates, it operates as a 'snare' or 'tangled_rope', extracting educational opportunity and suppressing effective alternatives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and publishers of authentic literature are clear beneficiaries, as the approach aligns with their ideological and economic interests. Teachers adopting the method may benefit from pedagogical freedom but can also bear costs if their students struggle. Struggling readers and their parents are the primary victims, facing significant educational and emotional costs. Phonics advocates are structurally excluded from the dominant discourse in whole language-influenced systems.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_emergence_empirical_validity,
    'Does decoding truly ''emerge naturally'' for the majority of children through immersion in authentic literature, or is explicit instruction necessary for most?',
    'Longitudinal studies comparing literacy outcomes in whole language vs. explicit phonics classrooms, particularly for diverse learner populations, and meta-analyses of reading acquisition research.',
    'If decoding does not reliably emerge naturally, the constraint''s extractiveness for struggling learners is higher, and its coordination function is undermined, pushing it towards a ''snare'' or ''tangled_rope'' classification. If it does, the ''rope'' claim is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoding_emergence_empirical_validity, empirical, 'Empirical validity of the ''natural emergence'' claim for decoding skills.').

omega_variable(
    pedagogical_freedom_vs_efficacy,
    'Is the pedagogical freedom afforded by whole language methods a genuine benefit for teachers and students, or does it mask an underlying lack of efficacy for a significant portion of the student population?',
    'Qualitative studies on teacher satisfaction and student engagement alongside quantitative studies on literacy outcomes across different pedagogical approaches. Policy analysis of teacher training and curriculum mandates.',
    'If pedagogical freedom is prioritized over evidence-based efficacy, the ''beneficiary'' role for teachers becomes more complex, and the constraint''s ''theater_ratio'' may increase as the performance of ''child-centered'' learning overshadows actual learning outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_freedom_vs_efficacy, conceptual, 'Balancing teacher autonomy and student learning outcomes in literacy instruction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of explicit phonics structural (e.g., curriculum mandates, lack of materials) or internalized (e.g., teachers'' beliefs, professional identity)?',
    'Post-policy-shift analysis: if phonics instruction remains suppressed after mandates change, reclassify as partially internalized. Surveys of teacher beliefs and professional development content.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as teachers carry the suppression with them even after external barriers are removed. This makes policy reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for phonics instruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, focusing on the whole language perspective. It is structurally distinct from other readings (phonics-first, balanced literacy, structured literacy) due to differing core premises and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
