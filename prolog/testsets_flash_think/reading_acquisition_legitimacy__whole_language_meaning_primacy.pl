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
 *   human_readable: Whole Language: Meaning Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'whole language' reading of reading
 *   acquisition, which posits that reading is primarily a meaning-making
 *   process and that decoding skills emerge naturally through immersion in
 *   authentic literature. It emphasizes a child-centered, facilitator-led
 *   approach to literacy instruction. The constraint's claimed type (Rope)
 *   reflects its proponents' view of it as a beneficial coordination
 *   mechanism, while the metrics reflect the increasing evidence of its
 *   extractive and suppressive effects on a significant portion of learners,
 *   particularly those for whom decoding does not emerge naturally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.35).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.7).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language: Meaning Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '3ef95574-82e3-4985-bc66-be91c245ddc2').
narrative_ontology:cs_kernel_codification('3ef95574-82e3-4985-bc66-be91c245ddc2', implicit).
narrative_ontology:cs_authority_grounding('3ef95574-82e3-4985-bc66-be91c245ddc2', practice).
narrative_ontology:cs_interpretation_layer_present('3ef95574-82e3-4985-bc66-be91c245ddc2').
narrative_ontology:cs_reading_relation('3ef95574-82e3-4985-bc66-be91c245ddc2', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('3ef95574-82e3-4985-bc66-be91c245ddc2', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('3ef95574-82e3-4985-bc66-be91c245ddc2', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_axiom('3ef95574-82e3-4985-bc66-be91c245ddc2', foundational, reading_is_natural_language_process).
narrative_ontology:cs_axiom_status(reading_is_natural_language_process, holdable).
narrative_ontology:cs_axiom_grounding('3ef95574-82e3-4985-bc66-be91c245ddc2', reading_is_natural_language_process, empirically_contingent).
narrative_ontology:cs_axiom('3ef95574-82e3-4985-bc66-be91c245ddc2', secondary, explicit_decoding_instruction_harms_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('3ef95574-82e3-4985-bc66-be91c245ddc2', explicit_decoding_instruction_harms_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('3ef95574-82e3-4985-bc66-be91c245ddc2', child_centered_meaning_construction).
narrative_ontology:cs_drift_state('3ef95574-82e3-4985-bc66-be91c245ddc2', contemporary_science_of_reading_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('3ef95574-82e3-4985-bc66-be91c245ddc2', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_proponents).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_who_acquire_decoding_naturally).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, publishers_of_authentic_literature).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_who_struggle_with_decoding).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement whole language pedagogy, believing reading is a natural process of meaning-making. They benefit from the philosophical alignment and professional identity associated with this approach, and actively shape curriculum and teacher training.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_proponents, agenda_setter,
    institutional, generational, identity_locked, national).

% These children thrive in a whole language environment, developing a love for reading and acquiring decoding skills without explicit, systematic instruction. They benefit from engaging with authentic literature from an early age.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_who_acquire_decoding_naturally, beneficiary,
    powerless, immediate, constrained, local).

% These children do not naturally acquire decoding skills and are significantly disadvantaged by the lack of explicit phonics instruction. They bear the cost of delayed literacy, academic struggles, and potential long-term educational and economic impacts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_who_struggle_with_decoding, payer,
    powerless, biographical, trapped, local).

% Bear the emotional and financial costs of their children's literacy struggles, often seeking expensive private tutoring or advocating for policy changes. Their options are limited by the prevailing pedagogical approach in schools.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    organized, generational, constrained, global).

% Researchers, educators, and parent groups who champion explicit, systematic phonics instruction. They are often marginalized or actively resisted by whole language proponents, despite growing scientific evidence supporting their position.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Develop and market educational materials aligned with whole language principles, including authentic literature collections and guided reading resources. They benefit from the market demand created by this pedagogical approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_developers, beneficiary,
    powerful, biographical, mobile, national).

% Responsible for setting state and national literacy standards and funding educational programs. They observe the ongoing 'reading wars' and are subject to lobbying from various pedagogical camps, often struggling to reconcile conflicting evidence and advocacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, education_policymakers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, child-centered pedagogical framework for early literacy instruction, aiming to foster a love of reading and natural language acquisition through immersion in authentic texts.
% TRANSFER_FUNCTION: Transfers pedagogical authority from explicit, systematic skill instruction to the teacher as a facilitator of meaning-making. It also transfers the burden of decoding acquisition to the child's natural emergent abilities, potentially transferring educational disadvantage to those for whom decoding does not emerge naturally.
% ABSENT_VOICES: Cognitive scientists specializing in reading acquisition, parents of children with dyslexia, and advocates for structured literacy approaches are often excluded from the core pedagogical discourse, despite offering evidence-based alternatives.
% DISAPPEARANCE_RATIONALE: If the whole language meaning-primacy constraint vanished overnight, literacy instruction would immediately shift towards more explicit and systematic decoding methods, curriculum materials would be overhauled, and teacher training would be fundamentally reoriented. The entire landscape of early literacy education would reorganize.
% FOUNDING_PROBLEM: Traditional phonics instruction was perceived as dry, decontextualized, and stifling to children's natural curiosity and love of reading, failing to connect reading to meaningful comprehension.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of whole language and some educators attest that the problem of decontextualized instruction and lack of reading motivation remains live. However, cognitive scientists and advocates for phonics and structured literacy argue that the founding problem was misdiagnosed, and that the current crisis in reading proficiency demonstrates the failure of the whole language solution. Independent research on reading acquisition often contradicts the core premises of the founding problem as framed by whole language proponents.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at a moderate 0.35, reflecting the reading's ideal of natural, low-friction learning. However, the temporal measurements show a rising trend, indicating that in practice, the approach became increasingly extractive for struggling learners over time. Suppression is high (0.7) because the pedagogical framework actively discourages and often excludes explicit phonics instruction, which is a critical alternative for many children. Theater ratio is moderate (0.4) and rising, as the performance of 'authentic literature' immersion can sometimes mask the lack of foundational skill development. Resistance is very high (0.85) due to the ongoing 'reading wars' and the strong advocacy for phonics-based approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language proponents, this constraint is a beneficial Rope, coordinating natural learning and fostering a love of reading. From the perspective of struggling learners and phonics advocates, it operates as a Snare or Tangled Rope, actively suppressing effective alternatives and extracting educational opportunity. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language proponents and children who naturally acquire decoding are beneficiaries, experiencing the constraint as a supportive framework. Children who struggle with decoding and their parents are victims, bearing the costs of an instructional method that does not meet their needs. Phonics advocates are excluded, as their pedagogical philosophy is directly opposed and suppressed by this constraint. Curriculum developers aligned with whole language benefit from the market for their materials.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_for_all_learners,
    'Does decoding truly emerge naturally for all children, or does the whole language approach systematically disadvantage a significant portion of learners?',
    'Longitudinal studies comparing literacy outcomes of children taught exclusively with whole language versus those taught with explicit, systematic phonics, particularly for children with diverse learning profiles.',
    'If decoding does not emerge naturally for a significant portion of learners, the constraint''s effective extraction and suppression are much higher than claimed, reclassifying it towards a Snare for those learners. If it proves effective for all, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_for_all_learners, empirical, 'Whether the core premise of natural decoding acquisition holds true for all children.').

omega_variable(
    suppression_of_alternatives_justification,
    'Is the suppression of explicit phonics instruction a necessary component of fostering a love of reading and meaning-making, or an extractive mechanism that harms learners who require explicit instruction?',
    'Pedagogical research exploring methods that integrate explicit phonics with authentic literature in ways that maintain motivation and comprehension, or policy changes that mandate such integration.',
    'If integration is possible without harming motivation, the suppression is revealed as unnecessary and extractive, increasing the constraint''s Snare-like qualities. If suppression is truly necessary for the claimed benefits, the Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_justification, conceptual, 'Justification for suppressing alternative instructional methods.').

omega_variable(
    identity_lock_of_proponents,
    'To what extent is the persistence of whole language pedagogy driven by the professional identity and ideological commitment of its proponents, rather than by its demonstrated efficacy for all learners?',
    'Sociological studies of educational reform movements, analysis of resistance to evidence-based practices, and examination of teacher training program curricula and professional development offerings.',
    'If identity-lock is a primary driver, the constraint''s theater_ratio is higher, and its persistence is more akin to a Piton or Snare, maintained by institutional inertia and ideological commitment despite mounting evidence of inefficacy for some.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_proponents, empirical, 'Role of professional identity in maintaining pedagogical commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.5).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_design_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_training_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
