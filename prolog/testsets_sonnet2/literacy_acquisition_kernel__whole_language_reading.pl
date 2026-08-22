% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading: Meaning-Emergence Literacy Instruction
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the whole_language_reading claim within the
 *   literacy_acquisition_kernel: reading acquisition emerges from meaningful
 *   engagement with connected text, phonics develops naturally through
 *   exposure, and explicit decoding instruction is unnecessary and
 *   potentially motivation-harming. This is one reading among four contested
 *   readings of the same kernel (phonics_reading, balanced_literacy_reading,
 *   structured_literacy_reading are separate constraint files); this story
 *   does not describe or average over those siblings — it authors a single,
 *   stable epsilon for the emergence-model arrangement as it actually
 *   operates in classrooms and teacher-preparation pipelines today, from the
 *   standing of this reading's own commitments.
 *
 * KEY AGENTS:
 *   - credentialed_literacy_faculty: institutional agenda-setter, arbitrage exit — trains teachers, sets certification, near-zero personal risk from the model's failure modes
 *   - whole_language_curriculum_publishers: organized beneficiary — revenue tied to continued non-adoption of systematic phonics materials
 *   - classroom_teachers_professional_autonomy: moderate power, constrained exit — genuine beneficiary of professional-judgment framing, but bears retraining cost when gaps surface
 *   - students_without_print_rich_homes: powerless, trapped — bears the primary extraction; the model assumes background literacy exposure they do not have
 *   - students_with_dyslexia: powerless, trapped — needs explicit systematic decoding instruction the model structurally withholds until failure triggers referral
 *   - english_language_learners: powerless, constrained — the model presupposes an oral English base absent for many
 *   - cognitive_science_reading_researchers: analytical observer — converging evidence base largely outside the adoption pipeline's practical reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading: Meaning-Emergence Literacy Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '8c7ae013-e753-4219-9573-8adc201012b3').
narrative_ontology:cs_kernel_codification('8c7ae013-e753-4219-9573-8adc201012b3', distributed).
narrative_ontology:cs_authority_grounding('8c7ae013-e753-4219-9573-8adc201012b3', practice).
narrative_ontology:cs_interpretation_layer_present('8c7ae013-e753-4219-9573-8adc201012b3').
narrative_ontology:cs_reading_relation('8c7ae013-e753-4219-9573-8adc201012b3', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('8c7ae013-e753-4219-9573-8adc201012b3', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7ae013-e753-4219-9573-8adc201012b3', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_axiom('8c7ae013-e753-4219-9573-8adc201012b3', foundational, decoding_skill_emerges_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_skill_emerges_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('8c7ae013-e753-4219-9573-8adc201012b3', decoding_skill_emerges_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('8c7ae013-e753-4219-9573-8adc201012b3', foundational, explicit_decoding_instruction_harms_reading_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harms_reading_motivation, holdable).
narrative_ontology:cs_axiom_grounding('8c7ae013-e753-4219-9573-8adc201012b3', explicit_decoding_instruction_harms_reading_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('8c7ae013-e753-4219-9573-8adc201012b3', meaning_centered_progressive_literacy_tradition).
narrative_ontology:cs_drift_state('8c7ae013-e753-4219-9573-8adc201012b3', post_reading_science_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c7ae013-e753-4219-9573-8adc201012b3', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, credentialed_literacy_faculty).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, english_language_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, school_district_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains teacher-candidates in schools of education using a meaning-emergence model of reading acquisition, sets certification coursework, authors the textbooks that define what counts as literacy pedagogy expertise, and administers accreditation review. Their professional standing and research programs are built on the emergence framework; adopting a decoding-first model would devalue decades of scholarship and require retooling entire teacher-preparation curricula.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, credentialed_literacy_faculty, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell leveled-reader libraries, guided-reading kits, and 'balanced' materials built around the premise that exposure to authentic text drives skill development. Revenue depends on schools continuing to adopt curricula that avoid systematic, sequenced phonics scope-and-sequence materials, which are supplied by a different vendor ecosystem.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Were trained in the emergence model, exercise professional judgment picking 'just right' books and running guided reading groups rather than delivering scripted phonics lessons, and experience this as respecting their expertise. When students fail to decode, they lack a decoding-focused pedagogy to fall back on, and some absorb blame or retrain at their own cost once gaps surface in later grades.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy, payer).

% Depend entirely on school instruction to learn to decode because they arrive without the incidental exposure to letter-sound patterns, rhyme, and print conventions that the emergence model assumes children absorb from a literate home environment. Without explicit, systematic phonics instruction, many stall at guessing-from-context and picture cues and fall progressively further behind peers who had that background exposure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes, payer,
    powerless, biographical, trapped, local).

% Have a neurological difference in phonological processing that specifically requires explicit, systematic, cumulative decoding instruction to bridge; an emergence-based classroom offers no structured intervention pathway until failure is severe enough to trigger special-education referral, by which point remediation is far more costly and confidence damage is often already done.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Are learning English phonology and print conventions simultaneously; the assumption that meaning will emerge from context presupposes an oral-language base in English that many of these students do not yet have, leaving them without an explicit map from sound to symbol while classmates' comprehension-first strategies race ahead on English oral fluency they lack.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, english_language_learners, payer,
    powerless, biographical, constrained, local).

% Select curricula, often on the recommendation of literacy coaches trained in the emergence model, and are institutionally invested in the adopted program; when third-grade reading scores lag, they bear reputational and political cost but face high switching costs to replace curriculum, retrain staff, and repurchase materials.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, school_district_administrators, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, school_district_administrators, payer).

% Study how the brain learns to map print to sound using controlled experiments and neuroimaging; largely converge on findings that decoding requires explicit instruction for most learners, but their research enters classrooms only through the credentialing and publishing pipeline, which has been slow and resistant to revising the emergence model despite the evidence base.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_science_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice around a shared theory that treats reading as an extension of oral language acquisition: immerse children in authentic, engaging texts, let them construct meaning from context and pictures, and trust that decoding skill will emerge as a byproduct, preserving teacher professional judgment over scripted lesson delivery.
% TRANSFER_FUNCTION: Moves instructional time and curricular resources away from systematic phoneme-grapheme instruction and toward exposure-based activities (independent reading, guided reading with leveled texts, context-cueing strategies), transferring the cost of any resulting decoding gap onto students who cannot compensate with outside literacy exposure, while preserving publishers' and faculty's existing investment in the emergence-based instructional model.
% ABSENT_VOICES: Parents of struggling readers and dyslexia advocacy organizations are frequently outside the room where curriculum adoption decisions are made; cognitive science researchers publish in journals that reach practitioners only indirectly through slow-moving teacher-preparation pipelines. Students themselves, especially those without print-rich homes, have no voice in the adoption process at all.
% DISAPPEARANCE_RATIONALE: If the emergence model disappeared overnight, teacher-preparation curricula, certification exams, and an entire tier of curriculum publishing built around leveled readers and guided-reading kits would need to be replaced with systematic phonics scope-and-sequence materials; literacy faculty whose expertise is credentialed in this model would face significant professional devaluation, and classroom practice would shift toward explicit decoding instruction with measurable near-term effects on early reading scores.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction had drifted toward rote, meaningless phonics drills disconnected from real text and comprehension, producing children who could sound out words without understanding them; the emergence model was built to restore engagement, motivation, and meaning-making to reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Literacy faculty and curriculum publishers attest the founding problem (meaningless, demotivating phonics drilling) remains live and justifies the model. Cognitive science researchers, dyslexia advocacy organizations, and multiple state legislative reading-reform commissions — sources outside the beneficiary set — attest that decades of converging reading-science evidence show the founding problem was addressed by pairing systematic phonics with meaningful text (not by abandoning explicit decoding instruction), and that the emergence model's persistence past that evidence reflects institutional and commercial inertia rather than a still-unsolved problem.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high and rising over the interval: the model imposes negligible cost on students with strong home literacy exposure but substantial, compounding cost on students without it, and that gap widens as unaddressed decoding deficits accumulate through the grades. Suppression (0.42) is moderate rather than severe — no coercive mechanism forces a teacher to withhold explicit phonics, but certification requirements, curriculum adoption cycles, and professional culture in schools of education create real friction against alternatives, which is why resistance (0.6) is substantial: parent advocacy, dyslexia organizations, and state legislative reform efforts increasingly push back. Accessibility collapse (0.35) is low-moderate: individual teachers can and do supplement with phonics informally, so alternatives are not fully foreclosed, just structurally disfavored. Theater ratio (0.4) reflects that a meaningful share of 'balanced' framing in current practice is presentational — programs marketed as balanced while retaining an emergence-model core — without being the dominant mode.
 *
 * DIRECTIONALITY LOGIC:
 *   Literacy faculty and curriculum publishers sit near the full-beneficiary end: institutional/organized power, arbitrage or mobile exit, and a direct stake in the model's continued adoption. Classroom teachers are genuinely dual-positioned — real professional-autonomy benefit, but constrained exit and downstream payer exposure when their students underperform. The three victim groups (print-poor-home students, dyslexic students, English learners) are powerless with trapped or constrained exit and carry the extraction directly: the model's core assumption — that phonics 'emerges' from exposure — is true only for children who already receive substantial incidental literacy exposure outside school, making the constraint's cost distribution a direct function of home background rather than classroom effort.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (meaningless, demotivating rote phonics drilling divorced from real text) was real and the model's engagement-first response was a legitimate corrective at the time. But the founding_problem_status is authored as contested: literacy faculty and publishers attest the problem persists and justifies continuation; a broad coalition outside the beneficiary set (cognitive scientists, dyslexia advocates, legislative reform commissions) attests the underlying problem was substantially resolved by pairing systematic phonics with authentic text engagement decades ago, and that the emergence model's survival past that resolution is now sustained by institutional and commercial inertia in teacher preparation and curriculum publishing rather than by an unmet instructional need. Classifying this as tangled_rope rather than snare preserves the real coordination value it delivers to teachers and its founding motivation, while still registering the asymmetric, non-consensual cost transferred to structurally disadvantaged student populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_versus_explicit_instruction_efficacy,
    'Does reading skill genuinely emerge from exposure to connected text for the majority of learners, or does the appearance of ''emergence'' in past studies reflect uncontrolled confounding with home literacy background and incidental phonics exposure?',
    'Randomized or quasi-experimental comparison of emergence-model classrooms against explicit-decoding classrooms, stratified by home literacy environment and controlling for teacher effects, tracked through longitudinal reading outcomes including for dyslexic and English-learner subgroups.',
    'If emergence is genuinely sufficient for most learners once controlled for background, the beneficiary framing (teacher autonomy) dominates and the constraint looks closer to a rope; if the apparent efficacy is an artifact of unmeasured home literacy exposure, the extraction on background-disadvantaged students is even more directly structural than currently authored, pushing the constraint toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_versus_explicit_instruction_efficacy, empirical, 'Whether emergence-model efficacy is genuine or an artifact of confounded home literacy background.').

omega_variable(
    professional_identity_versus_evidence_lag,
    'Is the persistence of the emergence model in teacher-preparation programs driven by continued belief in its evidentiary merit, or by institutional and professional-identity inertia among literacy faculty whose credentials and scholarship are built on it?',
    'Survey and interview literacy faculty on their engagement with the cognitive-science reading-research literature; track curriculum-adoption timelines against publication dates of key converging evidence to measure lag and identify whether revision correlates with faculty turnover versus new evidence.',
    'If driven primarily by identity and institutional inertia rather than live evidentiary dispute, this supports treating the beneficiary group (credentialed literacy faculty) as capturing a rent from delayed reform rather than defending a genuinely contested empirical position, strengthening the tangled_rope-toward-snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_identity_versus_evidence_lag, conceptual, 'Whether persistence reflects genuine evidentiary contest or professional identity lock-in.').

omega_variable(
    reading_kernel_framing_under_determination,
    'Is the correct framing of this kernel a single continuous instructional-philosophy spectrum (with balanced_literacy as a midpoint), or four genuinely discrete, mutually exclusive pedagogical commitments each with their own certification and material ecosystem?',
    'Examine whether teacher-preparation programs and curriculum adoption processes in practice treat these as blendable inputs or as discrete, competing package purchases; a spectrum framing would predict widespread blending in practice, a discrete framing would predict institutional lock-in to one package.',
    'A spectrum framing would suggest this story''s epsilon should be read as one point on a continuum shared with balanced_literacy_reading; a discrete framing (which this story adopts, consistent with the ε-invariance principle) supports treating each reading as its own constraint with its own stable epsilon, which is the choice made here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_framing_under_determination, conceptual, 'Whether the four kernel readings are points on a spectrum or discrete competing constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(lite_tr_t32, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(lite_be_t32, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(lite_su_t32, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% Part of the literacy_acquisition_kernel constraint family (4 sibling readings of one contested kernel). whole_language_reading (this file) authors epsilon=0.58, tangled_rope, with teachers'-professional-identity as beneficiary and background-disadvantaged students as victim. phonics_reading and structured_literacy_reading author the explicit-instruction claims as their own constraints with their own epsilon and stakeholder structures; balanced_literacy_reading authors the complementary-instruction claim as its own constraint. No epsilon is shared or averaged across files; each reading's file states independently what it believes true of its own arrangement's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
