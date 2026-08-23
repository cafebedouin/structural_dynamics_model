% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Balanced Literacy Instructional Model
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   Balanced literacy presents itself as a synthesis resolving the reading
 *   wars by combining systematic phonics with meaningful text engagement. In
 *   practice, the 'balance' is frequently implemented with phonics as an
 *   incidental add-on to a whole-language core, while the framework's
 *   flexibility sustains markets for curricula, training, and teacher
 *   preparation. The constraint coordinates instructional coherence across
 *   systems but extracts via method churn and deferred accountability for
 *   reading outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.35).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Instructional Model").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'cbeee05a-6bf6-4c40-bd51-b3494158e024').
narrative_ontology:cs_kernel_codification('cbeee05a-6bf6-4c40-bd51-b3494158e024', distributed).
narrative_ontology:cs_authority_grounding('cbeee05a-6bf6-4c40-bd51-b3494158e024', practice).
narrative_ontology:cs_interpretation_layer_present('cbeee05a-6bf6-4c40-bd51-b3494158e024').
narrative_ontology:cs_reading_relation('cbeee05a-6bf6-4c40-bd51-b3494158e024', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbeee05a-6bf6-4c40-bd51-b3494158e024', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbeee05a-6bf6-4c40-bd51-b3494158e024', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_axiom('cbeee05a-6bf6-4c40-bd51-b3494158e024', foundational, instructional_balance_is_optimal).
narrative_ontology:cs_axiom_status(instructional_balance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('cbeee05a-6bf6-4c40-bd51-b3494158e024', instructional_balance_is_optimal, instrumental).
narrative_ontology:cs_axiom('cbeee05a-6bf6-4c40-bd51-b3494158e024', foundational, phonics_and_meaning_are_complementary_not_sequential).
narrative_ontology:cs_axiom_status(phonics_and_meaning_are_complementary_not_sequential, holdable).
narrative_ontology:cs_axiom_grounding('cbeee05a-6bf6-4c40-bd51-b3494158e024', phonics_and_meaning_are_complementary_not_sequential, empirically_contingent).
narrative_ontology:cs_reference_frame('cbeee05a-6bf6-4c40-bd51-b3494158e024', reading_wars_compromise_settlement).
narrative_ontology:cs_drift_state('cbeee05a-6bf6-4c40-bd51-b3494158e024', post_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbeee05a-6bf6-4c40-bd51-b3494158e024', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, professional_development_providers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, early_elementary_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, instructional_balance_principle).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, complementary_mechanisms_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control teacher preparation curricula and certification requirements; balanced literacy frameworks generate sustained demand for coursework, credential programs, and faculty expertise. Benefit from method churn as 'balance' definitions shift across policy cycles.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, beneficiary).

% Produce and market blended curricula combining phonics materials with leveled readers and workshop frameworks; balanced literacy's flexibility creates recurring adoption cycles and multi-product revenue streams.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers, beneficiary,
    organized, biographical, mobile, national).

% Sell training, coaching, and implementation support for balanced literacy frameworks; the model's interpretive flexibility creates sustained demand for expert guidance on 'what balance looks like in practice.'
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, professional_development_providers, beneficiary,
    organized, biographical, mobile, regional).

% Experience the instructional model as delivered; when balance skews toward insufficient explicit decoding, students at risk for reading difficulties bear the cumulative cost of delayed skill acquisition with no alternative pathway.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, early_elementary_students, payer,
    powerless, biographical, trapped, local).

% Must implement frameworks they often did not choose; bear accountability for outcomes while lacking authority to select methods. Some benefit from professional autonomy the model permits, but most experience the 'balance' mandate as an unfunded instructional demand.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Argue that balanced literacy's phonics component is typically unsystematic and insufficient for at-risk learners; excluded from core curriculum adoption processes in many districts where balanced literacy is entrenched.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    organized, biographical, constrained, national).

% Study reading acquisition mechanisms; provide evidence on decoding-comprehension relationships that challenges the 'complementary' framing when phonics instruction is incidental rather than systematic.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying instructional framework that allows districts, schools, and teachers to claim adherence to both phonics and meaning-based approaches without resolving the substantive disagreements between them.
% TRANSFER_FUNCTION: Moves instructional authority and resource allocation toward flexible frameworks that require ongoing professional development and multi-product curricula; moves risk of reading failure onto students who receive insufficient explicit decoding instruction.
% ABSENT_VOICES: Students who experience reading failure under balanced literacy and their families are structurally absent from curriculum adoption decisions; structured literacy advocates are excluded from the 'consensus' that balanced literacy claims to represent.
% DISAPPEARANCE_RATIONALE: If balanced literacy frameworks vanished overnight, districts would face immediate curriculum vacuums; adoption processes would reopen with phonics-first and structured literacy alternatives gaining ground; professional development markets would shift; teacher preparation would require redesign.
% FOUNDING_PROBLEM: The reading wars of the 1980s-1990s polarized instruction into phonics-first vs. whole-language camps; balanced literacy emerged as a compromise framework that would end the conflict by incorporating both approaches.
% FOUNDING_PROBLEM_CORROBORATION: The compromise framing is attested by balanced literacy proponents; cognitive scientists and structured literacy advocates attest the founding problem was misdiagnosed — the conflict was not between two equally valid approaches but between evidence-aligned and evidence-misaligned instruction; the 'compromise' institutionalized the misalignment.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.45) reflects the gap between the framework's promised synthesis and its typical implementation; suppression (0.35) captures the institutional inertia and accreditation structures that resist adoption of more explicit alternatives; theater ratio (0.5) marks the growing proportion of 'balance' discourse that performs synthesis while delivering neither systematic phonics nor robust comprehension. The constraint is claimed as tangled_rope because it genuinely coordinates (districts need a framework) while extracting (the coordination function is degraded by commercial and institutional interests).
 *
 * PERSPECTIVAL GAP:
 *   From the education school seat, balanced literacy is a coherent framework that respects teacher professionalism; from the student seat, it is a lottery where outcomes depend on whether a given teacher's 'balance' includes sufficient decoding. The engine computes this divergence from structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and publishers sit at the beneficiary end (d ~ 0.15): they set agendas, collect revenue, and control the definition of 'balance.' Early elementary students are full targets (d ~ 0.9): trapped, no exit, bear the cost of instructional insufficiency. Teachers are constrained payers (d ~ 0.65): they implement under accountability pressure with limited authority. Structured literacy advocates are excluded — their exclusion is what the enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading wars polarization) was real but the solution (compromise without evidence adjudication) institutionalized the conflict rather than resolving it. The mandate persists because the compromise framework serves the institutional beneficiaries who define it, not because it solves the instructional problem for at-risk learners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_vs_rebrand,
    'Is balanced literacy a genuine third reading that structurally integrates systematic phonics with meaningful engagement, or is it a whole-language rebrand that adopts phonics terminology without its systematic implementation?',
    'Analyze implementation fidelity data: measure the proportion of balanced literacy classrooms delivering explicit, systematic, cumulative phonics instruction vs. incidental phonics. If the mode is incidental, the reading is a rebrand.',
    'If rebrand, the constraint''s claimed coordination function is false — it coordinates around a misrepresentation. The true constraint family would be whole_language_reading with a marketing layer. If genuine synthesis, the tangled_rope classification stands but the extraction profile shifts toward coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_vs_rebrand, conceptual, 'Whether the balanced literacy reading''s core premise (complementary integration) is implemented or performed.').

omega_variable(
    method_churn_revenue_model,
    'Do education schools and publishers structurally depend on the instability of ''balance'' definitions for recurring revenue, or does the model stabilize around evidence-aligned practice?',
    'Track curriculum adoption cycles and professional development revenue against policy shifts (e.g., state reading laws, NAEP results). Correlation between policy volatility and adoption revenue would indicate churn dependence.',
    'If churn-dependent, the extraction is structural — the constraint persists because its beneficiaries profit from its irresolution. If stabilizing, the model may be transitioning toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_churn_revenue_model, empirical, 'Whether the constraint''s beneficiaries extract via managed instability rather than stable coordination.').

omega_variable(
    victim_identification_ambiguity,
    'Are the victims of this constraint specifically students who receive insufficient explicit decoding (a subset), or all students subjected to a suboptimal framework (the class)?',
    'Disaggregate reading outcomes by student risk profile under balanced literacy implementation. If only at-risk students are harmed, the victim set is narrower; if typically developing students also underperform relative to explicit instruction, the victim set is the class.',
    'Narrower victim set (at-risk only) makes the constraint a tangled_rope with asymmetric extraction; universal victim set makes it a snare. Affects directionality computations for the student stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, empirical, 'Whether the constraint''s extraction is targeted or universal across the student population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1995, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(lite_tr_t2005, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(lite_tr_t2015, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(lite_tr_t2025, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2025, 0.5).

% Extraction over time
narrative_ontology:measurement(lite_be_t1995, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(lite_be_t2005, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(lite_be_t2015, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(lite_be_t2025, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1995, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(lite_su_t2005, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(lite_su_t2015, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(lite_su_t2025, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the 'literacy acquisition kernel' into structurally distinct constraints with different ε values, beneficiary/victim structures, and enforcement requirements. The kernel label 'balanced literacy' conflates a coordination claim (instructional framework) with an extraction mechanism (method churn). Each reading authors its own ε from the shared referent (the standing arrangement of reading instruction) assessed by its own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, organized, 0.2).
constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, powerless, 0.9).
constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, moderate, 0.65).
constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
