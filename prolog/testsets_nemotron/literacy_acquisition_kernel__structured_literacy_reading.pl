% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The structured literacy reading asserts that reading acquisition requires
 *   explicit, systematic, cumulative instruction across five pillars —
 *   phonological awareness, phonics, fluency, vocabulary, and comprehension —
 *   rooted in the Orton-Gillingham tradition originally designed for dyslexic
 *   students but now claimed as universally applicable. This reading has
 *   gained significant policy traction since 2010 (dyslexia screening laws,
 *   'science of reading' legislation, LETRS training mandates), moving from
 *   special education intervention to general education mandate. The
 *   constraint's extraction profile is asymmetric: highest on general
 *   education teachers (specialized certification, curriculum overhaul,
 *   coaching cycles) and lowest on dyslexic students (who experience reduced
 *   failure). The coordination function is real — structured literacy solves
 *   the 'instructional lottery' where dyslexic students' outcomes depend on
 *   whether they encounter a trained teacher — but the universal mandate
 *   extends the coordination structure to populations where the evidence for
 *   net benefit over balanced literacy is contested.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: Primary beneficiary (organized/trapped) — intervention reduces catastrophic failure
 *   - students_with_specific_learning_disabilities_in_reading: Primary beneficiary (organized/trapped) — same structural position
 *   - struggling_readers_tier2_tier3: Secondary beneficiary (organized/constrained) — gains from systematic instruction
 *   - general_education_classroom_teachers: Primary victim (moderate/constrained) — bears training burden and implementation fidelity demands
 *   - pre_service_teacher_candidates: Secondary victim (powerless/trapped) — certification requirements embedded in preparation programs
 *   - school_districts_implementation_budget: Institutional payer (institutional/constrained) — absorbs training, materials, coaching costs
 *   - structured_literacy_training_providers: Institutional beneficiary (institutional/arbitrage) — captures certification revenue
 *   - balanced_literacy_practitioners: Excluded (organized/identity_locked) — displaced by mandate, professional identity threatened
 *   - cognitive_science_researchers: Observer (analytical/analytical) — evidence base contested across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, 'e51202b2-0f20-4285-891d-f0a5a631b055').
narrative_ontology:cs_kernel_codification('e51202b2-0f20-4285-891d-f0a5a631b055', distributed).
narrative_ontology:cs_authority_grounding('e51202b2-0f20-4285-891d-f0a5a631b055', practice).
narrative_ontology:cs_interpretation_layer_present('e51202b2-0f20-4285-891d-f0a5a631b055').
narrative_ontology:cs_reading_relation('e51202b2-0f20-4285-891d-f0a5a631b055', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('e51202b2-0f20-4285-891d-f0a5a631b055', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('e51202b2-0f20-4285-891d-f0a5a631b055', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('e51202b2-0f20-4285-891d-f0a5a631b055', foundational, explicit_systematic_cumulative_instruction_necessary_for_all).
narrative_ontology:cs_axiom_status(explicit_systematic_cumulative_instruction_necessary_for_all, holdable).
narrative_ontology:cs_axiom_grounding('e51202b2-0f20-4285-891d-f0a5a631b055', explicit_systematic_cumulative_instruction_necessary_for_all, empirically_contingent).
narrative_ontology:cs_axiom('e51202b2-0f20-4285-891d-f0a5a631b055', foundational, dyslexia_intervention_as_universal_design).
narrative_ontology:cs_axiom_status(dyslexia_intervention_as_universal_design, holdable).
narrative_ontology:cs_axiom_grounding('e51202b2-0f20-4285-891d-f0a5a631b055', dyslexia_intervention_as_universal_design, instrumental).
narrative_ontology:cs_axiom('e51202b2-0f20-4285-891d-f0a5a631b055', secondary, multisensory_orthography_phonology_integration_required).
narrative_ontology:cs_axiom_status(multisensory_orthography_phonology_integration_required, holdable).
narrative_ontology:cs_axiom_grounding('e51202b2-0f20-4285-891d-f0a5a631b055', multisensory_orthography_phonology_integration_required, empirically_contingent).
narrative_ontology:cs_reference_frame('e51202b2-0f20-4285-891d-f0a5a631b055', orton_gillingham_clinical_practice_1930s).
narrative_ontology:cs_drift_state('e51202b2-0f20-4285-891d-f0a5a631b055', universal_mandate_era_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e51202b2-0f20-4285-891d-f0a5a631b055', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_specific_learning_disabilities_in_reading).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_tier2_tier3).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, pre_service_teacher_candidates).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts_implementation_budget).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_training_providers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, explicit_instruction_necessity_for_reading_acquisition).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, systematic_cumulative_phonology_first_approach).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, universal_design_via_dyslexia_intervention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive explicit systematic instruction that prevents reading failure. Without this constraint, their literacy outcomes depend on random teacher assignment (instructional lottery). They cannot exit the school system and have no alternative instructional pathway if the constraint is not implemented.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    organized, biographical, trapped, national).

% Same structural position as dyslexic students — the constraint's intervention structure was designed for this population. They bear the cost of failure when the constraint is absent and gain the benefit when it is present.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_specific_learning_disabilities_in_reading, beneficiary,
    organized, biographical, trapped, national).

% Students identified for reading intervention who receive structured literacy as Tier 2/3 support. They benefit from the systematic instruction but are not the primary design population; some would succeed with less intensive approaches.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_tier2_tier3, beneficiary,
    organized, biographical, constrained, national).

% Required to complete specialized training (LETRS, Orton-Gillingham, Wilson — 60-180 hours), adopt new curricula, submit to coaching cycles, and demonstrate fidelity. Exit options: leave teaching (high personal cost), move to non-mandate states (geographic constraint), or comply. Professional autonomy over instructional decisions is substantially reduced.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_classroom_teachers, payer,
    moderate, biographical, constrained, national).

% Certification requirements embedded in teacher preparation programs before they enter the profession. No choice in training content; the constraint shapes their entire professional foundation. Cannot exit without abandoning teaching career.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, pre_service_teacher_candidates, payer,
    powerless, biographical, trapped, national).

% Absorbs direct costs: training fees ($2,000-5,000 per teacher), curriculum replacement ($500-1,500 per classroom), coaching positions, substitute coverage for training days. Budget reallocation from other priorities. Some federal/state grant funding available but time-limited.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts_implementation_budget, payer,
    institutional, generational, constrained, national).

% Organizations (LETRS/Voyager Sopris, Wilson Language Training, Orton-Gillingham Academy, AIM Institute) that provide mandated certification training. Capture substantial revenue from state/district contracts. Can pivot offerings across states; low exit barriers. Their interest aligns with mandate expansion.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_training_providers, beneficiary,
    institutional, generational, arbitrage, national).

% Teachers, coaches, and teacher educators whose professional identity and practice are built on balanced literacy (reading/writing workshop, guided reading, three-cueing). The mandate displaces their expertise and community. Exit requires identity restructuring — not just learning new skills but relinquishing a professional self-concept. They are structurally excluded from policy conversations that frame balanced literacy as 'debunked.'
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_practitioners, excluded,
    organized, biographical, identity_locked, national).

% Produce and interpret the evidence base cited by all readings. Divided on universal applicability claims: some support structured literacy as best-evidence for all; others argue evidence supports targeted intervention for at-risk readers with balanced literacy sufficient for typical readers. Their role is analytical, not positioned within the mandate's extraction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, cognitive_science_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the instructional lottery: ensures every dyslexic/struggling reader receives explicit systematic instruction regardless of teacher assignment. Coordinates teacher knowledge, curriculum scope/sequence, and intervention alignment around a cumulative framework.
% TRANSFER_FUNCTION: Moves training time, instructional autonomy, and district budget from general education teachers and districts to training providers and certification bodies, in exchange for reduced reading failure among at-risk students and (claimed) improved outcomes for all students.
% ABSENT_VOICES: Balanced literacy practitioners (teachers, coaches, teacher educators) are structurally excluded — their professional framework is framed as 'debunked' rather than engaged. Parents of typical readers who may prefer meaning-rich approaches are not consulted. Students who thrive in balanced literacy but would receive more code-focused instruction under universal mandates.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, dyslexia screening laws would lose their instructional prescription, districts would revert to prior curricula (largely balanced literacy), training providers would lose mandated revenue, and the instructional lottery would return — dyslexic students' outcomes would again depend on chance teacher assignment.
% FOUNDING_PROBLEM: Dyslexic students were failing catastrophically under whole language and early balanced literacy approaches because they require explicit systematic instruction in phoneme-grapheme correspondence that those approaches do not provide. The instructional lottery meant a dyslexic child's literacy fate depended on whether they encountered a teacher who knew Orton-Gillingham methods.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by independent cognitive scientists (e.g., Seidenberg, Wolf, Shaywitz) and longitudinal studies (CTOPP, NICHD) showing dyslexic readers require explicit systematic phonics. The International Dyslexia Association (advocacy, not provider) attests the problem persists. However, the universal extension beyond the founding population is contested by reading researchers who argue typical readers acquire decoding efficiently through less explicit approaches (e.g., Castles, Rastle, Nation 2018; some balanced literacy researchers).
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects the asymmetric burden: teachers and districts pay substantial costs (time, money, professional autonomy) while the primary beneficiaries (dyslexic/struggling readers) are a minority of the mandated population. Suppression (0.42) is moderate — balanced literacy curricula and practices are actively displaced by legislation and state department mandates, but teacher resistance and implementation fidelity gaps persist. Theater ratio (0.31) captures the growing performative compliance: districts adopt 'science of reading' branding while preserving balanced literacy practices underneath, and training hours accumulate without proportional instructional change. Accessibility collapse (0.62) is elevated because once the explicit-systematic-cumulative framework is understood, alternative pedagogies (whole language, pure balanced literacy) appear incoherent to adherents — but not irreversibly so, as evidenced by persistent balanced literacy enclaves. Resistance (0.48) reflects organized pushback from teacher preparation programs, balanced literacy advocates, and local control proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the dyslexic student seat: the constraint is a rope — genuine coordination that prevents catastrophic failure. From the general education teacher seat: the constraint is a tangled rope — coordination function acknowledged but extraction via certification mandates is experienced as punitive. From the training provider seat: the constraint is a snare — certification revenue extraction masked as quality assurance. From the researcher seat: the constraint is contested — evidence base supports efficacy for at-risk readers but universal benefit claims exceed data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: students with dyslexia/SLD (d ≈ 0.15 — intervention subsidizes them); struggling readers Tier 2/3 (d ≈ 0.25 — partial benefit). Victims: general education teachers (d ≈ 0.85 — constrained exit, high training burden); pre-service candidates (d ≈ 0.9 — trapped in certification pipeline); districts (d ≈ 0.7 — institutional payer with some budgetary discretion). Excluded: balanced literacy practitioners (identity_locked exit — professional identity fused to displaced paradigm). Training providers sit at d ≈ 0.1 (beneficiary end) but are not declared as beneficiaries because their gain is secondary rent capture, not the constraint's stated purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dyslexic students failing under whole language) remains live and corroborated by independent researchers. However, the mandate has expanded beyond the founding population to universal application, where the coordination function is less clearly established. The extraction on teachers has accumulated (training mandates, curriculum replacement cycles) while the original intervention fidelity has diluted (theater ratio rising). This is mandatrophy in the classical sense: a genuine coordination solution for a specific population has been universalized into a broader mandate whose extraction now exceeds its coordination justification for the expanded population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the structured_literacy_reading a distinct fourth reading of the literacy_acquisition_kernel, or a specialized variant of the phonics_reading?',
    'Analyze whether the five-component cumulative framework (phonological awareness, phonics, fluency, vocabulary, comprehension) plus Orton-Gillingham multisensory methodology constitutes a structurally distinct pedagogical commitment from phoneme-grapheme correspondence primacy alone.',
    'If distinct fourth reading, the kernel has four irreducibly different structural instantiations. If phonics variant, the kernel has three readings with structured_literacy as the most specified phonics descendant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is structurally distinct from phonics_reading').

omega_variable(
    teacher_training_extraction_naturalness,
    'Does the specialized certification requirement (Orton-Gillingham, LETRS, Wilson, etc.) represent genuine coordination overhead for effective implementation, or extractive credentialing that benefits training providers?',
    'Compare student outcomes under certified vs. non-certified structured literacy implementation; audit training provider revenue and accreditation gatekeeping structures.',
    'If coordination overhead, the high extractiveness on teachers is the price of fidelity. If extractive credentialing, the constraint imposes unnecessary costs that could be reduced without losing efficacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_training_extraction_naturalness, empirical, 'Whether teacher certification requirements are coordination cost or rent extraction').

omega_variable(
    universal_applicability_claim,
    'Is the ''applicable universally'' claim structurally true — does structured literacy benefit typical readers as much as dyslexic readers — or is it a rhetorical frame that extends the intervention''s mandate beyond its evidence base?',
    'Meta-analysis of structured literacy vs. balanced literacy outcomes for non-struggling readers; cost-benefit analysis of universal vs. targeted implementation.',
    'If universally beneficial, the constraint coordinates at population scale with low per-capita extraction. If only dyslexia-specific, universal mandates impose extraction on non-beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_claim, empirical, 'Whether universal application is evidence-backed or mandate expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1990, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(lite_tr_t2015, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(lite_tr_t2023, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2023, 0.31).

% Extraction over time
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(lite_be_t2015, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(lite_be_t2023, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2023, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(lite_su_t2015, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(lite_su_t2023, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.18).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_preparation_accreditation_standards).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, state_dyslexia_screening_laws).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, curriculum_adoption_policies).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four readings with distinct ε profiles and beneficiary/victim structures. structured_literacy_reading has the highest ε on teacher training (specialized certification), lowest ε on dyslexic students, and claims universal applicability. phonics_reading has lower ε on training (narrower scope), similar ε on students. balanced_literacy_reading has lower ε on training (existing practice accommodated) but higher ε on at-risk students (instructional lottery). whole_language_reading has lowest ε on training but highest ε on dyslexic students (catastrophic failure). These are distinct constraints linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, moderate, 0.85).
constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, powerless, 0.9).
constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
