% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Instructional Framework
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   Balanced literacy presents itself as the synthesis that ends the reading
 *   wars: systematic phonics instruction AND meaningful text engagement,
 *   complementary rather than contradictory. In practice, it has become the
 *   dominant instructional framework in U.S. elementary education, organizing
 *   teacher preparation, curriculum publishing, professional development, and
 *   assessment. The constraint extracts moderate revenue (method churn for
 *   education schools and publishers) while its coordination benefit (a
 *   unified framework) is contested — structured literacy advocates argue the
 *   phonics component is unsystematic; whole language purists reject any
 *   explicit decoding. Students, particularly those with dyslexia, may be
 *   victims if the synthesis fails them. The engine must compute whether the
 *   coordination function is genuine enough to sustain a tangled_rope
 *   classification or whether extraction dominates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.35).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Instructional Framework").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '1c6d42d4-c3a8-4d10-8262-5d9e53119569').
narrative_ontology:cs_kernel_codification('1c6d42d4-c3a8-4d10-8262-5d9e53119569', distributed).
narrative_ontology:cs_authority_grounding('1c6d42d4-c3a8-4d10-8262-5d9e53119569', practice).
narrative_ontology:cs_interpretation_layer_present('1c6d42d4-c3a8-4d10-8262-5d9e53119569').
narrative_ontology:cs_reading_relation('1c6d42d4-c3a8-4d10-8262-5d9e53119569', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c6d42d4-c3a8-4d10-8262-5d9e53119569', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('1c6d42d4-c3a8-4d10-8262-5d9e53119569', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('1c6d42d4-c3a8-4d10-8262-5d9e53119569', foundational, phonics_and_meaning_are_complementary_not_contradictory).
narrative_ontology:cs_axiom_status(phonics_and_meaning_are_complementary_not_contradictory, holdable).
narrative_ontology:cs_axiom_grounding('1c6d42d4-c3a8-4d10-8262-5d9e53119569', phonics_and_meaning_are_complementary_not_contradictory, instrumental).
narrative_ontology:cs_axiom('1c6d42d4-c3a8-4d10-8262-5d9e53119569', foundational, instructional_balance_optimizes_reading_acquisition_for_all_learners).
narrative_ontology:cs_axiom_status(instructional_balance_optimizes_reading_acquisition_for_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('1c6d42d4-c3a8-4d10-8262-5d9e53119569', instructional_balance_optimizes_reading_acquisition_for_all_learners, empirically_contingent).
narrative_ontology:cs_reference_frame('1c6d42d4-c3a8-4d10-8262-5d9e53119569', post_reading_wars_synthesis).
narrative_ontology:cs_drift_state('1c6d42d4-c3a8-4d10-8262-5d9e53119569', contemporary_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c6d42d4-c3a8-4d10-8262-5d9e53119569', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, students_in_balanced_literacy_classrooms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, instructional_balance_optimizes_reading_acquisition).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, phonics_and_meaning_are_complementary_not_contradictory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control teacher preparation curricula and certification requirements; profit from recurring professional development contracts and method-specific training programs; maintain influence through accreditation standards and state policy networks
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, beneficiary).

% Produce and sell balanced literacy curricula, leveled readers, assessment systems, and professional development packages; revenue depends on district adoption cycles and method churn; lobby for adoption-friendly policies
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Bear the cost of learning and implementing balanced literacy protocols through mandated professional development, purchasing materials, and allocating instructional time; exit requires changing districts or leaving the profession; evaluated on fidelity to the framework
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Receive reading instruction through the balanced literacy framework; if the synthesis fails them (especially students with dyslexia or limited home literacy), they bear the lifelong cost of reading difficulty; no meaningful exit until adulthood
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, students_in_balanced_literacy_classrooms, payer,
    powerless, biographical, trapped, local).

% Study reading acquisition mechanisms; some validate the balanced literacy synthesis, others argue it dilutes systematic phonics; their research informs but does not control adoption decisions
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_scientists, observer,
    analytical, generational, analytical, global).

% Argue that balanced literacy's phonics component is unsystematic and insufficient; push for structured literacy mandates through legislation (e.g., dyslexia screening laws); structurally excluded from balanced literacy adoption committees
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Reject any explicit phonics instruction as harmful to reading motivation; view balanced literacy as a co-optation of whole language principles; marginalized in both policy and publisher markets
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_purists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified instructional framework that integrates systematic phonics with meaningful text engagement, resolving the phonics vs. whole language debate by synthesis rather than elimination; coordinates teacher practice, curriculum materials, and assessment around a common 'balance' principle
% TRANSFER_FUNCTION: Moves district professional development budgets and curriculum adoption funds to education schools (training programs, certification courses) and publishers (balanced literacy materials, leveled libraries, assessment kits), while allocating teacher instructional time to the balanced literacy protocol (workshop model, guided reading, word study)
% ABSENT_VOICES: Students who struggle under balanced literacy (particularly those with dyslexia or limited home literacy), phonics-first advocates who argue the synthesis dilutes systematic instruction, whole language purists who reject any explicit phonics — these voices are marginalized in adoption decisions and professional development design
% DISAPPEARANCE_RATIONALE: The constraint organizes a massive instructional infrastructure: teacher preparation programs, professional development markets, curriculum publishing, assessment systems, and state policy frameworks. Its removal would cause immediate reorganization of how reading is taught in adopting districts — teacher prep would pivot, publishers would restructure catalogs, professional development would be retooled
% FOUNDING_PROBLEM: The reading wars of the 1980s-1990s polarized instruction into phonics-first vs. whole language camps, leaving teachers without a coherent framework and students with inconsistent outcomes; balanced literacy emerged as a synthesis promising to end the wars
% FOUNDING_PROBLEM_CORROBORATION: National Reading Panel (2000) and subsequent synthesis reports document the polarization; education school faculty and balanced literacy proponents (e.g., Calkins, Fountas & Pinnell) attest the synthesis resolved it; structured literacy advocates and dyslexia organizations (IDA, Decoding Dyslexia) attest the problem persists for struggling readers and the synthesis is insufficient
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).
:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the method-churn revenue stream: education schools and publishers profit from recurring adoption cycles, but the framework does deliver some instructional coherence. Suppression (0.35) is moderate — alternatives (structured literacy, phonics-first) exist but are marginalized in adoption committees and teacher prep. Theater ratio (0.4) captures the growing gap between the 'balance' rhetoric and classroom reality where phonics is often incidental. Accessibility collapse (0.5) reflects that alternatives are available but structurally disadvantaged. Resistance (0.5) captures the active contestation from both phonics and whole language camps. All metrics are authored independently of the claimed_type; the engine will compute per-seat classifications.
 *
 * PERSPECTIVAL GAP:
 *   From the education school seat, balanced literacy is a genuine coordination achievement — it ended the wars and gave teachers a workable framework. From the student seat (especially struggling readers), the same structure may operate as extraction — their difficulty is the cost of maintaining the synthesis. From the phonics advocate seat, the constraint is a snare — the phonics component is cover for whole language practices. The engine computes this divergence; the authored claim (tangled_rope) states the author's structural judgment that both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and publishers are structural beneficiaries (d near 0.0): they collect revenue and set the agenda. Classroom teachers are payers with constrained exit (d ~0.6): they bear implementation costs and fidelity pressure but have some professional autonomy. Students are trapped payers (d ~0.9): no exit, bear lifelong consequences if the method fails. Reading scientists are analytical observers (d=0.5). Phonics advocates and whole language purists are excluded — their structural position is opposition, not participation. The derivation chain from beneficiary/victim + power + exit produces these d values; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading wars polarization) is contested as live/dead. If dead, the constraint persists as a zombie — method churn without coordination justification. If live, it remains a tangled_rope. The corridation between founding_problem_status=contested and disappearance_verdict=world_rearranges signals capture risk: the infrastructure would reorganize if the constraint vanished, but the reorganization might not benefit students. The mandatrophy_resolved flag is not set — the analysis is open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_status_ambiguity,
    'Are students in balanced literacy classrooms beneficiaries (achieving synthesis) or victims (failed by insufficient systematic phonics)?',
    'Longitudinal comparative studies of reading outcomes for matched populations under balanced literacy vs. structured literacy, disaggregated by dyslexia status and home literacy environment',
    'If students are net beneficiaries, the constraint is a genuine tangled_rope with real coordination. If students (especially vulnerable subgroups) are net victims, the coordination function is illusory and the constraint reclassifies toward snare. The victim declaration in base_properties reflects the ''failed by insufficient phonics'' reading; this omega documents the contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_status_ambiguity, empirical, 'Whether the constraint''s primary subjects benefit or bear net cost').

omega_variable(
    whole_language_rebrand_question,
    'Is balanced literacy a genuine theoretical synthesis or a strategic rebrand of whole language that retains its extraction structure while adopting phonics terminology?',
    'Discourse analysis of balanced literacy foundational texts (Calkins, Fountas & Pinnell) pre- and post-National Reading Panel (2000); comparison of classroom enactment fidelity to systematic phonics criteria',
    'If rebrand, the coordination function is performative and the constraint is a snare with whole_language_reading as its true kernel. If genuine synthesis, the tangled_rope classification holds with a distinct coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whole_language_rebrand_question, conceptual, 'Whether the constraint''s theoretical identity is synthesis or rebrand').

omega_variable(
    coordination_extraction_boundary,
    'Is the ''balance'' principle a genuine coordination mechanism that solves a real instructional problem, or is it a rhetorical cover that enables method churn?',
    'Analysis of adoption cycle frequency and revenue correlation: do districts adopt new balanced literacy materials because the prior materials failed instructionally, or because the professional development market requires renewal?',
    'If method churn drives adoption, extraction dominates and the constraint trends toward snare/pi ton. If adoption cycles track genuine instructional improvement, coordination is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s persistence is driven by coordination value or revenue cycles').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of phonics-first and whole language purist alternatives structural (policy, accreditation, publisher gatekeeping) or internalized (teacher identity, professional socialization)?',
    'Post-exit suppression trajectory: when teachers move to structured literacy environments, do they report feeling liberated or do they carry balanced literacy assumptions that hinder implementation?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent. This affects the identity_locked assessment for teachers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lit_acq_balanced_tr_t1990, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(lit_acq_balanced_tr_t2000, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(lit_acq_balanced_tr_t2010, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(lit_acq_balanced_tr_t2015, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(lit_acq_balanced_tr_t2020, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(lit_acq_balanced_tr_t2024, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(lit_acq_balanced_be_t1990, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(lit_acq_balanced_be_t2000, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(lit_acq_balanced_be_t2010, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(lit_acq_balanced_be_t2015, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(lit_acq_balanced_be_t2020, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(lit_acq_balanced_be_t2024, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lit_acq_balanced_su_t1990, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(lit_acq_balanced_su_t2000, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(lit_acq_balanced_su_t2010, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(lit_acq_balanced_su_t2015, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(lit_acq_balanced_su_t2020, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(lit_acq_balanced_su_t2024, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four constraint stories with distinct ε values and beneficiary/victim structures. Balanced literacy claims synthesis (ε=0.45); phonics_reading claims decoding-first (lower ε, different beneficiaries); whole_language_reading claims natural emergence (different victims); structured_literacy_reading claims universal explicit instruction (different coordination function). All four are linked via affects_constraints. The kernel's contested commitment is 'how reading acquisition works' — each reading instantiates a different constraint from this kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
