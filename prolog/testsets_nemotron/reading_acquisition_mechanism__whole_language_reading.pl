% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint story captures the whole language reading of the
 *   contested kernel 'reading_acquisition_mechanism.' The whole language
 *   reading asserts that reading acquisition occurs through meaningful
 *   engagement with authentic texts and that decoding skills emerge
 *   implicitly from exposure — no systematic phonics sequence is required.
 *   This reading dominated U.S. literacy education from roughly 1970-2000 and
 *   persists in teacher preparation, curriculum materials, and classroom
 *   practice despite converging cognitive science evidence that explicit
 *   systematic phonics is necessary for most children and critical for
 *   struggling readers. The constraint operates as a tangled rope: it
 *   coordinates teacher autonomy and constructivist pedagogy (genuine
 *   coordination function for educators who reject scripted instruction)
 *   while extracting literacy outcomes from struggling readers, dyslexic
 *   students, and low-income children who cannot access external remediation.
 *   The extraction is active — the constraint requires enforcement through
 *   teacher preparation gatekeeping, curriculum adoption committees, and
 *   professional identity maintenance to suppress the phonics alternative.
 *
 * KEY AGENTS:
 *   - whole_language_teacher_practitioners: Primary agenda_setter (organized/identity_locked) — professional identity fused with the approach, controls classroom enactment
 *   - teacher_autonomy_advocates: Primary beneficiary (organized/mobile) — gains rhetorical leverage against scripted programs
 *   - whole_language_publisher_networks: Primary beneficiary (institutional/arbitrage) — captive market for authentic literature materials
 *   - progressive_education_institutions: Primary beneficiary (institutional/mobile) — protects curricular authority and constructivist epistemology
 *   - struggling_readers: Primary payer (powerless/trapped) — cumulative failure without systematic decoding
 *   - dyslexic_students: Primary payer (powerless/trapped) — neurobiologically cannot acquire decoding implicitly
 *   - low_income_students_without_tutoring_access: Primary payer (powerless/trapped) — economically foreclosed from exit
 *   - phonics_advocates_and_reading_scientists: Excluded (moderate/constrained) — evidence base ignored by design
 *   - cognitive_science_observers: Observer (analytical/analytical) — documents extraction pattern from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '55e7890c-7f9a-4360-b8ef-111507de7fa9').
narrative_ontology:cs_kernel_codification('55e7890c-7f9a-4360-b8ef-111507de7fa9', distributed).
narrative_ontology:cs_authority_grounding('55e7890c-7f9a-4360-b8ef-111507de7fa9', practice).
narrative_ontology:cs_interpretation_layer_present('55e7890c-7f9a-4360-b8ef-111507de7fa9').
narrative_ontology:cs_reading_relation('55e7890c-7f9a-4360-b8ef-111507de7fa9', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('55e7890c-7f9a-4360-b8ef-111507de7fa9', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('55e7890c-7f9a-4360-b8ef-111507de7fa9', foundational, decoding_emerges_implicitly_from_meaningful_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('55e7890c-7f9a-4360-b8ef-111507de7fa9', decoding_emerges_implicitly_from_meaningful_engagement, empirically_contingent).
narrative_ontology:cs_axiom('55e7890c-7f9a-4360-b8ef-111507de7fa9', foundational, teacher_professional_judgment_supersedes_external_evidence).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_supersedes_external_evidence, holdable).
narrative_ontology:cs_axiom_grounding('55e7890c-7f9a-4360-b8ef-111507de7fa9', teacher_professional_judgment_supersedes_external_evidence, deontological).
narrative_ontology:cs_axiom('55e7890c-7f9a-4360-b8ef-111507de7fa9', secondary, authentic_texts_are_sufficient_for_literacy_development).
narrative_ontology:cs_axiom_status(authentic_texts_are_sufficient_for_literacy_development, holdable).
narrative_ontology:cs_axiom_grounding('55e7890c-7f9a-4360-b8ef-111507de7fa9', authentic_texts_are_sufficient_for_literacy_development, empirically_contingent).
narrative_ontology:cs_reference_frame('55e7890c-7f9a-4360-b8ef-111507de7fa9', constructivist_literacy_origin).
narrative_ontology:cs_drift_state('55e7890c-7f9a-4360-b8ef-111507de7fa9', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55e7890c-7f9a-4360-b8ef-111507de7fa9', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_autonomy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_publisher_networks).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, progressive_education_institutions).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, low_income_students_without_tutoring_access).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, literacy_as_meaning_construction).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, authentic_text_engagement_primary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enact literacy curricula centered on authentic literature, writer's workshop, and implicit phonics. Professional identity is fused with the approach — abandoning it means abandoning the pedagogical self-concept built over decades. They control classroom-level implementation and resist mandated phonics sequences as deprofessionalization.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_teacher_practitioners, agenda_setter,
    organized, biographical, identity_locked, national).

% Gain rhetorical and policy leverage from the whole language frame: it positions teacher judgment as the legitimate authority against scripted programs and standardized testing. They do not necessarily teach whole language themselves but benefit from the constraint's framing of autonomy as the primary professional good.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_autonomy_advocates, beneficiary,
    organized, generational, mobile, national).

% Publish and sell trade books, leveled readers, and professional development materials aligned to whole language. The constraint's rejection of decodable texts and systematic scope-and-sequence creates a captive market for their authentic literature catalogs. They can pivot to other pedagogies if adoption shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_publisher_networks, beneficiary,
    institutional, generational, arbitrage, global).

% University education schools and professional organizations that certify teachers and produce literacy research. The whole language frame aligns with their constructivist epistemology and protects their curricular authority from external mandates. They collect prestige and enrollment from maintaining the approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, progressive_education_institutions, beneficiary,
    institutional, generational, mobile, national).

% Children who fail to acquire decoding skills through implicit exposure alone. They experience the constraint as cumulative academic failure — falling further behind each year without systematic intervention. No exit exists within the classroom; remediation requires external tutoring their families often cannot afford. The constraint extracts their literacy trajectory and future opportunity.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Children with neurobiological differences in phonological processing who structurally cannot acquire decoding through implicit exposure. The constraint actively harms them by delaying or denying the explicit instruction they require. Their exit is blocked by the same teacher autonomy that prevents systematic phonics — they are trapped in a mechanism that does not work for their cognitive architecture.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Students from families who cannot purchase private tutoring or diagnostic assessment. In whole language classrooms they receive no systematic decoding instruction; at home they lack the literacy-rich environment the approach assumes. The constraint extracts disproportionately from them because their exit options are economically foreclosed — the mechanism assumes a home literacy scaffold that does not exist.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, low_income_students_without_tutoring_access, payer,
    powerless, biographical, trapped, local).

% Researchers, clinicians, and policy advocates who argue for explicit systematic phonics based on converging evidence from cognitive psychology and neuroscience. They are structurally excluded from whole language curriculum decisions and teacher preparation programs. Their objection is that the constraint ignores the evidence base; they would impose a different mechanism if they had authority.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_advocates_and_reading_scientists, excluded,
    moderate, biographical, constrained, national).

% Analyze reading acquisition as a cognitive mechanism independent of pedagogical commitments. They see the constraint as an empirically falsified theory of reading development that persists through institutional inertia and identity protection. They do not participate in the curriculum wars but document the extraction pattern.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared pedagogical language and curricular coherence for teachers who reject behaviorist reading instruction: a constructivist frame where literacy emerges from meaning-making with authentic texts, protecting teacher professional judgment from scripted programs.
% TRANSFER_FUNCTION: Moves instructional time and cognitive load from explicit decoding instruction to meaning-focused engagement; moves remediation cost from the system (preventive instruction) to the individual child (reactive intervention); moves professional authority from external evidence to teacher autonomy.
% ABSENT_VOICES: Struggling readers, dyslexic students, and low-income families are structurally absent from curriculum adoption decisions. They would object to a mechanism that denies them systematic decoding instruction, but they have no seat at the table where pedagogical frameworks are chosen. Phonics advocates are excluded from teacher preparation and curriculum committees.
% DISAPPEARANCE_RATIONALE: If the whole language constraint vanished overnight, teacher preparation programs would restructure around evidence-based decoding instruction; publishers would shift from trade-book catalogs to decodable text series; struggling readers would receive preventive systematic phonics instead of waiting for failure-triggered remediation; the literacy achievement gap would narrow as the mechanism's regressive extraction ended.
% FOUNDING_PROBLEM: Mid-20th century reading instruction was dominated by basal readers with controlled vocabulary and explicit skill drills that many educators experienced as deadening, anti-intellectual, and disconnected from real literacy. Whole language was built to solve the problem of meaningless, decontextualized reading instruction that failed to create engaged readers.
% FOUNDING_PROBLEM_CORROBORATION: Whole language proponents attest the founding problem remains live — they cite ongoing pressure for scripted phonics programs and standardized testing as evidence that the deadening instruction they opposed has returned in new forms. Reading scientists and cognitive psychologists attest the founding problem is substantially solved: modern evidence-based phonics instruction is explicit, systematic, AND embedded in rich literature — the false dichotomy between 'phonics' and 'meaning' was never structurally necessary. Legislative testimony and independent meta-analyses from outside the benefiting parties support the reading science position.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint systematically denies preventive decoding instruction to children who need it, transferring remediation cost to individuals and families. The theater ratio (0.45) is substantial: the meaning-construction rhetoric is genuine but increasingly performs a cover function for a mechanism that fails its most vulnerable participants. Suppression (0.52) is moderate — the constraint does not legally ban phonics but structurally suppresses it through teacher preparation, curriculum adoption, and professional identity enforcement. Accessibility collapse (0.38) is moderate: alternatives (explicit phonics) exist and are known, but the constraint's framing makes them professionally illegible. Resistance (0.55) is moderate and growing — the 'science of reading' movement represents organized resistance from excluded voices and cognitive science. The metrics describe the constraint's actual operation; the claimed_type (tangled_rope) is independently authored as the structural truth.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (teacher practitioners) experiences the constraint as genuine coordination protecting professional judgment; the payer seats (struggling readers, dyslexic students, low-income students) experience it as enforced extraction with no exit. The beneficiary seats (autonomy advocates, publishers, institutions) experience it as a coordination good that incidentally extracts from non-participants. The engine computes this divergence from the structural data — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher practitioners are agenda_setters with identity_locked exit — their professional self-concept is constituted through the approach, making exit structurally improbable. Teacher autonomy advocates and publisher networks are beneficiaries with mobile/arbitrage exit — they gain from the frame but can pivot. Progressive institutions are beneficiaries with mobile exit — they protect authority but could adapt. Struggling readers, dyslexic students, and low-income students are payers with trapped exit — no classroom-level exit exists, and external remediation is economically or structurally blocked. Phonics advocates are excluded with constrained exit — they can advocate but cannot access the decision apparatus. Cognitive scientists are observers with analytical exit — they analyze from outside the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deadening basal reader instruction) was real but has been substantially solved by modern evidence-based phonics embedded in rich literature. The constraint persists because the identity_locked agenda_setters cannot distinguish the solved problem from the current mandate, and because the beneficiary institutions capture the coordination rents without bearing the extraction costs. The classification prevents mislabeling this as pure coordination (rope) by documenting the asymmetric extraction on trapped payers, and prevents mislabeling as pure extraction (snare) by documenting the genuine coordination function for teacher practitioners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_learning_ambiguity,
    'Is the claim that ''reading acquisition occurs naturally through meaningful engagement'' a genuine description of a cognitive universal, or a constructed pedagogical claim that benefits identifiable agents?',
    'Cross-cultural and historical analysis of literacy acquisition in societies with and without explicit instruction; neuroimaging of reading development in instructed vs. uninstructed learners; longitudinal outcomes for children in pure whole language vs. explicit phonics environments.',
    'If reading acquisition is naturally implicit (like spoken language), the constraint is a mountain — it describes a biological fact. If reading requires explicit instruction for most learners (the current scientific consensus), the constraint is a constructed claim that extracts from those who need instruction — a false summit mountain or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_learning_ambiguity, empirical, 'Whether the whole language mechanism describes a natural cognitive universal or a constructed pedagogical claim with asymmetric extraction').

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Does the teacher autonomy coordination function genuinely require the whole language mechanism, or could teacher professional judgment be protected under an evidence-based framework that includes systematic phonics?',
    'Analysis of teacher preparation programs and curriculum adoption processes in jurisdictions that have mandated evidence-based reading instruction while preserving teacher decision-making in other domains.',
    'If autonomy and evidence are compatible, the coordination function is separable from the extraction mechanism — the constraint is a tangled rope where extraction rides on a genuine coordination good. If they are inseparable, the constraint may be a snare where the autonomy rhetoric is the cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, conceptual, 'Whether the coordination function (teacher autonomy) structurally requires the extraction mechanism (implicit-only decoding)').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of explicit phonics in whole language classrooms structural (curriculum mandates, teacher preparation gatekeeping) or internalized (teachers genuinely believe phonics harms children, have fused their identity with the approach)?',
    'Post-exit suppression trajectory: if teachers who leave whole language environments continue to resist phonics instruction, the suppression is partially internalized. Survey data on teacher beliefs vs. institutional mandates.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target (teachers who might want to teach phonics) carries the suppression with them. This affects the identity_locked classification and the engine''s directionality computation for the agenda_setter seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the agenda_setter seat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t1970, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t1985, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t2000, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t2010, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t2020, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_tr_t2025, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t1970, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t1985, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t2000, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t2010, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t2020, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_be_t2025, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t1970, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t1985, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t2000, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t2010, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t2020, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(reading_acquisition_mechanism__whole_language_reading_su_t2025, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, teacher_preparation_gatekeeping).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_adoption_processes).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, dyslexia_identification_and_intervention_policy).

% DUAL FORMULATION NOTE:
% This story is one member of the reading_acquisition_mechanism constraint family. The kernel decomposes into three structurally distinct readings with different ε values: whole_language_reading (ε≈0.68, tangled_rope), phonics_reading (ε≈0.15, rope), balanced_literacy_reading (ε≈0.35, tangled_rope→rope transition). The whole language reading forecloses the phonics reading within a single framework but coexists with both in public discourse. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, organized, 0.15).
constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
