% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story captures the 'phonics_decoding_primacy' reading of
 *   the contested kernel 'reading_acquisition_legitimacy' — the claim that
 *   legitimate reading instruction must make the alphabetic principle
 *   explicit through systematic phonics. The reading emerged from cognitive
 *   science research on reading acquisition (NICHD, 1990s-2000s) and was
 *   translated into policy via the 'science of reading' movement
 *   (2010s-present), producing state laws mandating phonics-first curricula,
 *   teacher retraining, and universal decoding screening. The constraint
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (ensuring all children, especially vulnerable learners, receive explicit
 *   decoding instruction) while extracting through commercial
 *   curriculum/assessment markets, reducing teacher professional autonomy,
 *   and structurally excluding alternative pedagogical frameworks. The
 *   claimed type (tangled_rope) reflects this dual structure; the metrics
 *   capture the extraction (0.58), suppression of alternatives (0.62), and
 *   rising theater as fidelity mandates outpace evidence nuance (0.28).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '930d4436-7417-4bf5-ad9b-55f07ed584b3').
narrative_ontology:cs_kernel_codification('930d4436-7417-4bf5-ad9b-55f07ed584b3', distributed).
narrative_ontology:cs_authority_grounding('930d4436-7417-4bf5-ad9b-55f07ed584b3', expertise).
narrative_ontology:cs_interpretation_layer_present('930d4436-7417-4bf5-ad9b-55f07ed584b3').
narrative_ontology:cs_reading_relation('930d4436-7417-4bf5-ad9b-55f07ed584b3', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('930d4436-7417-4bf5-ad9b-55f07ed584b3', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('930d4436-7417-4bf5-ad9b-55f07ed584b3', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('930d4436-7417-4bf5-ad9b-55f07ed584b3', foundational, alphabetic_principle_is_primary_causal_mechanism).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_primary_causal_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('930d4436-7417-4bf5-ad9b-55f07ed584b3', alphabetic_principle_is_primary_causal_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('930d4436-7417-4bf5-ad9b-55f07ed584b3', foundational, systematic_explicit_phonics_necessary_for_all_learners).
narrative_ontology:cs_axiom_status(systematic_explicit_phonics_necessary_for_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('930d4436-7417-4bf5-ad9b-55f07ed584b3', systematic_explicit_phonics_necessary_for_all_learners, empirically_contingent).
narrative_ontology:cs_reference_frame('930d4436-7417-4bf5-ad9b-55f07ed584b3', explicit_code_instruction_norm).
narrative_ontology:cs_drift_state('930d4436-7417-4bf5-ad9b-55f07ed584b3', post_science_of_reading_policy_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('930d4436-7417-4bf5-ad9b-55f07ed584b3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, decoding_assessment_companies).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_mandate_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_mismatched_to_phonics_first).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_practitioners).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_necessity).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_instruction_efficacy_for_decoding).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, systematic_phonics_prevents_reading_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and sell comprehensive phonics programs (core curricula, interventions, decodable text libraries) adopted via state adoption lists and district mandates. Revenue scales with mandate scope. Can pivot to new markets if policy shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Provide universal screening, progress monitoring, and diagnostic assessments aligned to phonics scope-and-sequence. Contracts tied to state early literacy laws and district MTSS frameworks. Diversified across assessment types.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, decoding_assessment_companies, beneficiary,
    powerful, generational, arbitrage, global).

% Advocacy organizations, legislative champions, and state education officials who draft and implement 'science of reading' laws mandating phonics-first instruction, approved curriculum lists, and teacher retraining. Gain political capital and institutional authority from mandate enforcement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_mandate_advocates, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_mandate_advocates, beneficiary).

% Required to implement scripted phonics programs with fidelity, often replacing integrated literacy practices they were trained in. Bear costs of retraining, loss of professional discretion, and accountability for student outcomes on decoding metrics. Exit requires leaving the profession or moving to non-mandate jurisdictions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Children for whom intensive phonics-first instruction is not the optimal pathway — including some dyslexic profiles needing different scaffolds, hyperlexic children who decode early but need comprehension support, and multilingual learners whose orthographic knowledge doesn't map to English phonics sequences. Cannot opt out of mandated Tier 1 instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_mismatched_to_phonics_first, payer,
    powerless, biographical, trapped, local).

% Educators and researchers committed to meaning-centered, literature-immersion approaches. Professionally identified with whole language philosophy; their expertise and pedagogical repertoire are delegitimized by the mandate. Exit would require abandoning professional identity and community.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_practitioners, excluded,
    organized, biographical, identity_locked, national).

% Proponents of integrated approaches (e.g., reading/writing workshop, guided reading with embedded phonics). Their programs (Units of Study, Fountas & Pinnell) are removed from approved lists. Professional identity fused to balanced literacy framework; exclusion is experienced as epistemic erasure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_advocates, excluded,
    organized, biographical, identity_locked, national).

% Researchers studying reading acquisition, neural mechanisms, and instructional efficacy. Provide the evidence base cited by mandate advocates but often note nuances (dosage, differentiation, comprehension integration) lost in policy translation. No direct stake in mandate adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_scientists_reading_research, observer,
    analytical, civilizational, analytical, universal).

% Specialists (often Orton-Gillingham trained) delivering explicit, cumulative, diagnostic instruction. Aligned with phonics_decoding_primacy but emphasize vulnerable-learner-first design. Gain professional recognition and demand under mandates but face pressure to conform to commercial program fidelity over diagnostic responsiveness.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_practitioners, beneficiary,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures all children, especially the most vulnerable, acquire the alphabetic code through explicit, systematic instruction rather than leaving decoding to chance or implicit learning; creates a shared instructional language and assessment framework across classrooms and grades.
% TRANSFER_FUNCTION: Moves instructional authority and public resources toward commercially published phonics programs and standardized decoding assessments; moves professional discretion away from teachers toward scripted curricula, pacing guides, and fidelity rubrics; moves students into uniform Tier 1 phonics sequences regardless of individual profile.
% ABSENT_VOICES: Children for whom phonics-first is not the optimal pathway (some dyslexic profiles needing different scaffolds, hyperlexic children needing comprehension focus, multilingual learners with different orthographic backgrounds); teachers whose craft knowledge includes responsive, meaning-integrated approaches; families in communities with rich oral/literacy traditions not centered on decoding drills; researchers emphasizing comprehension-integrated models.
% DISAPPEARANCE_RATIONALE: If the phonics-decoding-primacy mandate vanished overnight, state early literacy laws would lapse, approved curriculum lists would dissolve, teacher retraining infrastructure would lose its mandate, assessment contracts would be rebid, and the entire early literacy ecosystem — materials, preparation, accountability — would reorganize around whatever approach fills the legitimacy vacuum.
% FOUNDING_PROBLEM: The persistent failure of many children, especially from historically marginalized backgrounds, to acquire fluent decoding skills under whole language and balanced literacy approaches that treated phonics as incidental or discovery-based rather than explicitly taught.
% FOUNDING_PROBLEM_CORROBORATION: NAEP reading scores showing persistent gaps for Black, Hispanic, and low-income students; NICHD longitudinal reading research; state policy reports documenting early literacy crises; but contested by balanced literacy researchers citing PIRLS comprehension data and studies showing integrated approaches' efficacy for comprehension outcomes.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.58) reflects the transfer of public funds to commercial phonics programs and assessments, and the extraction of teacher professional judgment via scripted fidelity requirements. Suppression (0.62) captures the policy-level removal of balanced literacy and whole language materials from approved lists, the delegitimization of teacher preparation programs not aligned to the mandate, and the narrowing of what counts as 'evidence-based.' Theater ratio (0.28) is moderate-low: explicit phonics instruction is genuinely enacted, but a growing share of enforcement activity monitors curriculum fidelity rather than student learning, and comprehension integration is often performatively added to phonics blocks. Accessibility collapse (0.72) is high because once a state adopts a phonics-first mandate, the entire ecosystem (materials, PD, assessment, teacher eval) reorients, making alternatives practically inaccessible within the system. Resistance (0.55) reflects ongoing pushback from balanced literacy advocates, teacher unions, and researchers emphasizing comprehension integration.
 *
 * PERSPECTIVAL GAP:
 *   From the policy_mandate_advocate seat, the constraint is a rope: genuine coordination solving the decoding gap for vulnerable learners. From the classroom_teacher seat, it is a snare: extraction of professional autonomy via scripted fidelity. From the students_mismatched_to_phonics_first seat, it is a snare: extraction of instructional fit with no exit. From the phonics_curriculum_publisher seat, it is a rope with beneficiary capture: coordination function real, but they capture the gains. The engine computes these seat-level types from the structural data; the claimed_type (tangled_rope) represents the author's structural reading of the constraint as a whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics publishers and assessment companies are structural beneficiaries (collect revenue, shape policy — d near 0.0). Policy mandate advocates are agenda_setters who also benefit institutionally (d ~ 0.15). Classroom teachers are payers bearing implementation costs with constrained exit (d ~ 0.75). Mismatched students are trapped payers with no exit (d ~ 0.95). Whole language and balanced literacy practitioners are excluded and identity-locked — their professional identity is constituted through the contested framework, making exit existentially costly (d ~ 0.85). Cognitive scientists are analytical observers (d ~ 0.5). Structured literacy practitioners are beneficiaries of mandate alignment but payers of fidelity pressure (dual position, d ~ 0.45).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decoding failure under implicit phonics) remains live per NAEP gaps and policy narratives, so mandatrophy is not resolved. However, the mandate has expanded beyond its founding justification: universal Tier 1 mandates go beyond the evidence (which supports explicit phonics for at-risk learners, not necessarily all learners at same dosage), and commercial programs exceed the evidence base. This is mandate creep — the coordination function (targeted explicit instruction) has been universalized into an extraction vehicle (universal commercial program adoption).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_targeted_mandate_boundary,
    'Does the evidence base for explicit systematic phonics support universal Tier 1 mandates for all children, or only targeted intervention for at-risk learners?',
    'Meta-analysis of RCTs comparing universal phonics-first core instruction vs. differentiated models (strong core + targeted Tier 2/3) on both decoding and comprehension outcomes across diverse learner profiles.',
    'If evidence supports only targeted intervention, the universal mandate extracts from students and teachers who don''t need it, reclassifying toward snare. If universal mandate is evidence-supported, extraction is more plausibly coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_targeted_mandate_boundary, empirical, 'Whether the mandate''s scope exceeds its evidence base.').

omega_variable(
    commercial_capture_of_mandate,
    'Do commercial phonics programs capture the mandate beyond what the evidence specifies (e.g., specific scopes, sequences, pacing, decodable text ratios not empirically differentiated)?',
    'Content analysis of state-approved program lists vs. experimental programs in efficacy studies; tracing of advocacy funding to publisher interests.',
    'If commercial programs embed non-evidenced specifications that become de facto requirements via fidelity rubrics, extraction is amplified and theater rises — the constraint becomes more snare-like.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_capture_of_mandate, empirical, 'Whether commercial interests shape mandate implementation beyond evidence.').

omega_variable(
    comprehension_integration_exclusion_cost,
    'Does the phonics-first mandate''s suppression of integrated meaning-making instruction create a delayed extraction (students who decode but cannot comprehend), and is this cost borne disproportionately by marginalized students?',
    'Longitudinal studies tracking decoding-comprehension trajectories under phonics-first mandates vs. integrated models, disaggregated by demographic subgroups.',
    'If a ''decoding ceiling'' effect emerges where comprehension lags decoding for marginalized groups under the mandate, the constraint''s extraction extends beyond immediate implementation costs to long-term opportunity costs, strengthening snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(comprehension_integration_exclusion_cost, empirical, 'Whether suppression of meaning-making integration creates downstream extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''reading_acquisition_legitimacy'' admit a single legitimate reading, or is the contest itself the stable structure (i.e., legitimate instruction is inherently pluralistic)?',
    'Historical analysis of whether periods of consensus (e.g., 1990s whole language dominance, 2020s phonics dominance) reflect evidentiary resolution or political/institutional cycles.',
    'If the kernel is inherently pluralistic, all single-reading mandates are structurally extractive (snare/tangled_rope) because they foreclose legitimate alternatives. If one reading is uniquely correct, mandates aligned to it are rope/scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s structure is monist or pluralist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ral_pdp_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ral_pdp_tr_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 4, 0.21).
narrative_ontology:measurement(ral_pdp_tr_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 8, 0.24).
narrative_ontology:measurement(ral_pdp_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.26).
narrative_ontology:measurement(ral_pdp_tr_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 16, 0.27).
narrative_ontology:measurement(ral_pdp_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(ral_pdp_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ral_pdp_be_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ral_pdp_be_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ral_pdp_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ral_pdp_be_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(ral_pdp_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ral_pdp_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ral_pdp_su_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(ral_pdp_su_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(ral_pdp_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(ral_pdp_su_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(ral_pdp_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, early_literacy_assessment_mandates).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_preparation_accreditation_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, state_curriculum_adoption_lists).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'reading_acquisition_legitimacy'. The kernel decomposes into at least four constraint stories (this one plus three siblings), each with distinct ε, stakeholder structures, and claimed types. They are linked via affects_constraints. The ε values differ substantially: this reading (tangled_rope, ε=0.58) vs. whole_language_meaning_primacy (likely snare from phonics-advocate seat, rope from whole-language seat) vs. balanced_literacy_integration (likely rope or tangled_rope depending on implementation) vs. structured_literacy_remediation (scaffold/tangled_rope hybrid). The decomposition follows the ε-invariance principle: the label 'reading instruction' covers structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, organized, 0.45).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
