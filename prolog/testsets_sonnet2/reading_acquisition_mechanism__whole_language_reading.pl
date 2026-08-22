% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the whole-language reading of the contested
 *   kernel 'how does reading acquisition occur.' It holds that decoding
 *   ability emerges implicitly from meaningful engagement with authentic
 *   texts, the way oral language is acquired through immersion, and that
 *   explicit systematic phonics instruction is unnecessary or even
 *   counterproductive to genuine literacy and love of reading. This is a
 *   distinct constraint from the phonics reading (explicit systematic
 *   grapheme-phoneme instruction as foundational) and the balanced-literacy
 *   reading (both together); each has its own ε and its own
 *   beneficiary/victim structure and is authored as a separate file per the
 *   ε-invariance principle. As the standing arrangement in many
 *   teacher-preparation programs and district curricula for decades, this
 *   reading's extraction is assessed by its own lights: real coordination
 *   value (meaning-centered, motivating instruction; low upfront
 *   curriculum-design cost; high teacher autonomy) sits alongside asymmetric
 *   extraction (diffuse population-level remediation costs concentrated on
 *   children without the extra-instructional supports the model implicitly
 *   assumes).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.48).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '593f6201-6957-47b9-a723-7e6471b17dc8').
narrative_ontology:cs_kernel_codification('593f6201-6957-47b9-a723-7e6471b17dc8', distributed).
narrative_ontology:cs_authority_grounding('593f6201-6957-47b9-a723-7e6471b17dc8', practice).
narrative_ontology:cs_interpretation_layer_present('593f6201-6957-47b9-a723-7e6471b17dc8').
narrative_ontology:cs_reading_relation('593f6201-6957-47b9-a723-7e6471b17dc8', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('593f6201-6957-47b9-a723-7e6471b17dc8', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('593f6201-6957-47b9-a723-7e6471b17dc8', foundational, decoding_emerges_implicitly_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('593f6201-6957-47b9-a723-7e6471b17dc8', decoding_emerges_implicitly_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('593f6201-6957-47b9-a723-7e6471b17dc8', secondary, authentic_text_engagement_is_necessary_and_sufficient_for_literacy).
narrative_ontology:cs_axiom_status(authentic_text_engagement_is_necessary_and_sufficient_for_literacy, holdable).
narrative_ontology:cs_axiom_grounding('593f6201-6957-47b9-a723-7e6471b17dc8', authentic_text_engagement_is_necessary_and_sufficient_for_literacy, empirically_contingent).
narrative_ontology:cs_reference_frame('593f6201-6957-47b9-a723-7e6471b17dc8', meaning_centered_literacy_tradition).
narrative_ontology:cs_drift_state('593f6201-6957-47b9-a723-7e6471b17dc8', post_science_of_reading_legislation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('593f6201-6957-47b9-a723-7e6471b17dc8', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, credentialing_schools_of_education).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_favoring_autonomy).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, fluent_readers_from_print_rich_homes).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, children_from_low_print_exposure_homes).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, english_language_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_favoring_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell leveled-reader libraries, guided-reading kits, and 'authentic text' programs that do not require a systematic phonics scope-and-sequence. Revenue depends on schools continuing to adopt exposure-based materials rather than structured decodable-text programs.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Train teacher candidates in meaning-first, exposure-based literacy theory as settled science, staff literacy coursework with faculty whose research and professional identity are built on this framework, and certify new teachers into the approach. Reversing course would require repudiating decades of published scholarship and coursework by their own faculty.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, credentialing_schools_of_education, agenda_setter,
    institutional, generational, identity_locked, national).

% Value the flexibility to choose authentic literature and design their own reading experiences rather than following a scripted phonics sequence. Many were trained exclusively in this model and have no alternative pedagogical toolkit; when struggling readers appear, they lack diagnostic and remediation skills the model never taught them, which becomes their own professional liability.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_favoring_autonomy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_favoring_autonomy, payer).

% Arrive at school already exposed to extensive print and often infer decoding patterns implicitly regardless of instructional method, so the whole-language approach looks like it 'works' for them. Their outcomes mask the method's failure for children without equivalent prior exposure.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, fluent_readers_from_print_rich_homes, beneficiary,
    moderate, biographical, mobile, local).

% Never receive explicit instruction in grapheme-phoneme correspondence and are expected to induce the code from immersion in authentic texts. Fall progressively behind, are frequently misdiagnosed as having attention or motivation problems rather than an instructional gap, and require expensive remediation years later if it is available at all.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Lack the pre-existing print exposure that lets some peers succeed despite the absence of systematic instruction. The method's implicit assumption of rich home literacy environments converts an instructional choice into a socioeconomic sorting mechanism.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, children_from_low_print_exposure_homes, payer,
    powerless, biographical, trapped, local).

% Have neurological differences in phonological processing that make implicit code-induction from exposure especially unlikely to succeed; require explicit, systematic, cumulative phonics instruction as an accommodation the whole-language model structurally does not provide, often leading to late or missed diagnosis.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Are simultaneously acquiring oral English and the writing system, with less implicit exposure time and often less print-rich home support in English; the meaning-first model assumes a base of oral vocabulary and cultural familiarity with texts that many have not yet built.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, english_language_learners, payer,
    powerless, biographical, trapped, local).

% Cognitive scientists and reading researchers whose converging evidence on phonological processing and systematic decoding instruction is largely absent from teacher preparation curricula grounded in this reading of acquisition. They publish in journals outside the education-school pipeline and have limited influence on classroom practice or credentialing standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_scientists_advocating_structured_literacy, excluded,
    organized, generational, constrained, national).

% Set literacy standards and approve instructional materials for public schools; increasingly commission audits and legislative 'science of reading' mandates in response to persistent low reading proficiency scores, potentially reshaping which reading acquisition model schools of education and publishers may promote.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, state_curriculum_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice around a shared theory that reading is acquired the way oral language is — through rich, meaningful engagement with real texts — allowing teachers to design literature-centered instruction without needing a common scripted phonics sequence.
% TRANSFER_FUNCTION: Moves instructional-design cost, curriculum-development cost, and teacher-preparation cost downward (low upfront cost, high teacher autonomy) while moving diagnostic and remediation cost forward onto struggling readers, their families, and later-stage special education and tutoring systems.
% ABSENT_VOICES: Reading scientists and cognitive researchers whose converging phonological-processing evidence is largely excluded from teacher-preparation curricula; parents of struggling readers who lack the specialized vocabulary to contest the pedagogical framework their child is being taught under.
% DISAPPEARANCE_RATIONALE: If this reading of acquisition vanished overnight, teacher-preparation curricula, published curriculum materials, and classroom practice built on exposure-based meaning-first instruction would all require replacement with systematic phonics sequences; publishers, schools of education, and current teacher training pipelines are structurally organized around this model persisting.
% FOUNDING_PROBLEM: Mid-20th-century basal readers relied on rote, decontextualized drill (look-say word memorization, meaningless controlled-vocabulary sentences) that produced children who could recite words but disliked reading and often failed to comprehend connected text; whole language arose to restore meaning, motivation, and authentic literature to reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language advocates within schools of education attest the founding problem (meaningless rote drill, disengagement) remains a live risk if instruction reverts to phonics-only drilling. Independent cognitive-science reading researchers, national reading panels, and dyslexia advocacy organizations outside the whole-language teaching establishment attest that the original problem was a false dichotomy — engagement and systematic decoding instruction are not in tension — and that the persistence of exposure-only instruction is now the primary obstacle to reading proficiency, not its solution.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that a meaningful minority of children — those without rich home print exposure, dyslexic students, English learners — do not induce the alphabetic code from exposure alone and pay in reading failure, remediation costs, and downstream academic harm, while the model's design and marketing costs remain low for adopters. Suppression (0.48) is moderate: no one is coerced into this pedagogy by force, but teacher-preparation credentialing, faculty tenure incentives, and publisher lock-in create real barriers to alternative instruction reaching classrooms, and struggling readers/families often lack the expertise to identify the instructional cause of the child's difficulty. Theater ratio (0.42) is elevated because 'balanced literacy' relabeling and cueing-strategy add-ons (e.g., three-cueing systems marketed as compatible with phonics) increasingly function as rebranding of the same underlying exposure-based theory rather than genuine incorporation of systematic phonics. Accessibility collapse (0.4) is moderate — the alternative (systematic phonics) is not epistemically hidden, but institutional credentialing pipelines make it practically hard for a given classroom teacher to access training in it. Resistance (0.58) is substantial and growing: reading-science researchers, dyslexia advocacy groups, and state legislatures increasingly contest the model directly ('science of reading' movement).
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and schools of education sit near the beneficiary end: they set the framework, train the workforce, and sell the materials, with mobile/arbitrage exit relative to any given cohort's outcomes. Teachers occupy a dual position — beneficiaries of autonomy and low prep burden, but also payers when they inherit struggling readers they were never trained to diagnose or remediate. Struggling decoders, low-print-exposure children, dyslexic students, and English learners are structural targets: trapped in the classroom instructional model assigned to them, with no individual exit option, bearing concentrated long-term cost for a design choice made at the institutional level.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) preserves the fact that this reading solves a real problem: rote, meaningless decoding drill genuinely undermined engagement and comprehension in its predecessor regime, and immersion in authentic, meaningful text is a real and valuable component of literacy development. Classifying this purely as extraction would erase that authentic coordination function. But the requires_active_enforcement flag and the victims array register that the model's persistence — via credentialing pipelines, publisher lock-in, and professional identity investment in schools of education — imposes disproportionate, foreseeable, and largely unremediated cost on children who most need explicit instruction, which a pure Rope classification would launder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_induction_sufficiency,
    'Does implicit exposure to authentic text reliably produce grapheme-phoneme decoding ability across the full range of learners, or only for children with substantial prior print exposure and typical phonological processing?',
    'Large-scale longitudinal comparison of reading outcomes across whole-language, phonics, and balanced-literacy classrooms, disaggregated by home print-exposure level, ELL status, and phonological processing profile (dyslexia screening).',
    'If implicit induction is broadly sufficient, the extraction assessment overstates harm and this reading is closer to a genuine Rope for most learners. If insufficient for a substantial subpopulation, current extraction and victim declarations understate the structural harm and the classification should move toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_induction_sufficiency, empirical, 'Whether implicit code-acquisition from exposure generalizes across learner populations.').

omega_variable(
    coordination_versus_professional_identity_lockin,
    'How much of this reading''s persistence is genuine coordination value (meaning-centered pedagogy, teacher autonomy, engagement) versus institutional/professional identity lock-in within schools of education whose faculty built careers on this theoretical framework?',
    'Track whether teacher-preparation programs update curricula and credentialing standards when new converging cognitive-science evidence emerges, versus continuing unchanged; measure faculty turnover and publication patterns relative to evidence shifts.',
    'High identity lock-in with low responsiveness to evidence would support reclassifying credentialing_schools_of_education''s role from good-faith agenda_setter toward a captured institutional actor, strengthening the case for active suppression rather than passive institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_professional_identity_lockin, conceptual, 'Whether persistence reflects genuine coordination benefit or professional identity capture.').

omega_variable(
    kernel_framing_disaggregation,
    'Given that ''the science of reading debate'' is popularly discussed as one dispute, is decomposing it into three separate kernel readings (whole language / phonics / balanced literacy) the correct level of granularity, or does balanced_literacy in practice reduce to a relabeled whole_language approach with cosmetic phonics add-ons (the ''three-cueing'' critique)?',
    'Compare instructional time allocation, sequencing, and assessment practices in self-labeled ''balanced literacy'' classrooms against both the whole_language and phonics readings'' structural criteria to determine whether balanced_literacy is a genuinely distinct third constraint or effectively a relabeled instance of this one.',
    'If balanced_literacy in practice collapses into this reading, the two constraints should be merged or the balanced_literacy story''s beneficiary/victim structure should be revised to reflect near-identical extraction; if genuinely distinct, the three-way decomposition holds as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_disaggregation, conceptual, 'Whether the three declared kernel readings are structurally distinct or whether balanced_literacy collapses into whole_language in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the reading_acquisition_mechanism kernel. phonics_reading claims explicit systematic grapheme-phoneme instruction is foundational and necessary; balanced_literacy_reading claims both explicit phonics and authentic literature are jointly required; this reading (whole_language_reading) claims decoding emerges implicitly from meaningful exposure without a systematic instructional sequence. Each carries its own ε (this reading's ε=0.62 reflects concentrated harm to struggling decoders under low-upfront-cost, high-autonomy conditions) and its own beneficiary/victim structure. The three are linked here so that contamination/coupling analysis can trace how legitimacy shifts in one reading (e.g., 'science of reading' state legislation undermining whole_language's institutional standing) propagate pressure onto the sibling readings' resource availability and legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
