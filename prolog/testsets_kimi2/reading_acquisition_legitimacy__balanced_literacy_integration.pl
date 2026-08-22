% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Reading
 *   domain: education/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy integration is a pedagogical framework claiming that
 *   legitimate reading instruction requires both explicit phonics and
 *   meaning-making through authentic literature. It positions itself as the
 *   resolution to the 'reading wars' between phonics-first and whole-language
 *   approaches, prescribing that teachers toggle between direct instruction
 *   and facilitation while using both decodable and leveled texts. The
 *   framework is institutionalized in curriculum packages, teacher
 *   preparation programs, and district mandates across many Anglophone
 *   education systems. This story treats the constraint as a tangled rope: it
 *   coordinates a genuine methodological dispute, but its
 *   institutionalization extracts disproportionately from struggling readers
 *   and their families by diluting systematic phonics intensity, while
 *   curriculum publishers and teacher training programs capture sustained
 *   revenue and professional authority.
 *
 * KEY AGENTS:
 *   - balanced_literacy_publishers (powerful/arbitrage) â primary beneficiary, captures curriculum revenue from district adoption
 *   - teacher_preparation_programs (institutional/constrained) â secondary beneficiary, institutionalizes the framework in pre-service training
 *   - district_curriculum_coordinators (institutional/constrained) â agenda setter, enforces fidelity and procurement decisions
 *   - classroom_teachers (moderate/identity_locked) â dual-positioned agent, benefits from professional autonomy but bears implementation costs and identity fusion
 *   - struggling_readers (powerless/trapped) â primary target, bears the extraction through insufficient explicit phonics
 *   - families_seeking_remediation (moderate/constrained) â secondary target, pays for outside tutoring to compensate for instructional gaps
 *   - science_of_reading_researchers (analytical/analytical) â analytical observer, supplies countervailing evidence and legislative testimony
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.68).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Reading").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'fda6cfc7-bd53-4efe-981f-58d8c2c760ca').
narrative_ontology:cs_kernel_codification('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', formalized).
narrative_ontology:cs_authority_grounding('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', expertise).
narrative_ontology:cs_interpretation_layer_present('fda6cfc7-bd53-4efe-981f-58d8c2c760ca').
narrative_ontology:cs_reading_relation('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', foundational, reading_requires_decoding_and_meaning_integration).
narrative_ontology:cs_axiom_status(reading_requires_decoding_and_meaning_integration, holdable).
narrative_ontology:cs_axiom_grounding('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', reading_requires_decoding_and_meaning_integration, empirically_contingent).
narrative_ontology:cs_axiom('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', foundational, teacher_mediation_between_methods_is_legitimate).
narrative_ontology:cs_axiom_status(teacher_mediation_between_methods_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', teacher_mediation_between_methods_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', balanced_instructional_synthesis).
narrative_ontology:cs_drift_state('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', contemporary_evidence_based_scrutiny, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fda6cfc7-bd53-4efe-981f-58d8c2c760ca', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, district_curriculum_coordinators).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, families_seeking_remediation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell leveled-text systems, guided reading curricula, assessment frameworks, and teacher coaching materials that embed the balanced literacy instructional model. Revenue depends on district adoption cycles and teacher fidelity to the framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Train pre-service teachers in balanced literacy methods, including guided reading, leveled texts, and cueing systems. Accreditation, faculty expertise, and enrollment are tied to this model; switching to structured literacy requires curriculum overhaul and faculty retraining.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs, beneficiary,
    institutional, generational, constrained, national).

% Select, mandate, and monitor balanced literacy curriculum packages across schools. Responsible for fidelity of implementation and professional development. Career trajectory and institutional credibility are tied to the chosen framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_curriculum_coordinators, agenda_setter,
    institutional, biographical, constrained, regional).

% Implement the mandated balanced literacy program, toggling between explicit phonics lessons and guided reading with authentic literature. Many entered the profession trained in this model and identify with its emphasis on child-centered meaning-making; they bear professional and emotional costs when students fail to acquire reading skills despite their effort.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer).

% Children, especially those with dyslexia or other reading difficulties, who receive insufficient explicit systematic phonics within the balanced framework. They are promoted through grades with persistent reading deficits, often internalizing failure as personal, while the instructional model attributes the gap to lack of exposure or readiness rather than curriculum inadequacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Parents and guardians who pay for private tutoring, outside assessments, and advocacy to secure explicit phonics instruction their children do not receive in school. They are geographically bound to district assignment and legally required to send children to school.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, families_seeking_remediation, payer,
    moderate, biographical, constrained, local).

% Cognitive scientists and reading researchers who publish evidence that explicit systematic phonics is necessary for most children and especially critical for at-risk readers. They testify before legislatures, publish meta-analyses, and challenge the empirical foundations of cueing-based instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, science_of_reading_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the long-running dispute between phonics-first and whole-language approaches by providing an instructional framework that incorporates both explicit decoding instruction and immersion in authentic literature, allowing teachers to address different learner needs without committing to a single methodological pole.
% TRANSFER_FUNCTION: Moves instructional time, curriculum dollars, and professional development resources toward a hybrid model; moves the costs of insufficient systematic phonics onto struggling readers and their families, who must absorb the deficit or pay for outside remediation.
% ABSENT_VOICES: Speech-language pathologists, dyslexia advocates, and cognitive scientists emphasizing the necessity of explicit systematic phonics for all children were historically underrepresented in curriculum committees and teacher preparation program design; they are now entering the conversation but remain outside the balanced literacy institutional core.
% DISAPPEARANCE_RATIONALE: If the balanced literacy constraint vanished overnight, district procurement would shift toward structured literacy and explicit phonics programs, teacher preparation would reorient around the science of reading, publishing revenue would reallocate, and struggling readers would receive systematically different instruction. The classroom experience would reorganize around a different instructional grammar.
% FOUNDING_PROBLEM: The reading wars of the 1980s and 1990s created a polarized and politically charged landscape where phonics advocates and whole-language advocates were in bitter conflict, leaving teachers without a coherent classroom framework and children exposed to abrupt pedagogical lurches depending on district ideology.
% FOUNDING_PROBLEM_CORROBORATION: Teacher educators and district administrators attest the problem was resolved by balanced literacy. Cognitive scientists and parent advocates attest the problem was not resolved but displaced, and that the polarized landscape itself was a false binary; legislative hearings and meta-analytic reviews from outside the benefiting institutions support the displaced-problem reading.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the 'balance' structurally favors meaning-making and cueing over systematic phonics for children who most need explicit instruction, transferring remediation costs to families. Suppression (0.68) reflects the active exclusion of structured literacy alternatives from teacher preparation and district procurement through mandated curricula and fidelity enforcement. Theater ratio (0.45) captures the performative aspect: teachers conduct guided reading groups and leveled-text rotations that appear responsive to individual needs while often avoiding the intensive explicit instruction required by the science of reading. Accessibility collapse (0.58) is moderate because alternative approaches exist but are institutionally hard to access within mandated districts. Resistance (0.55) is substantial and growing, driven by cognitive science research, parent advocacy, and legislative mandates for phonics. The measurement series tracks the institutional hardening of the framework from its emergence in the 1990s through the contemporary science-of-reading challenge.
 *
 * PERSPECTIVAL GAP:
 *   The publisher and teacher preparation seats experience the constraint as coordination they maintain and benefit from; the struggling reader and remediation-paying family seats experience it as an extractive arrangement that denies them effective instruction. The district coordinator seat sits between â enforcing a framework that validates their institutional role while constraining their ability to respond to countervailing evidence. The engine computes this divergence from structural position: low directionality for beneficiaries with arbitrage or constrained exit, high directionality for trapped or identity-locked payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and teacher preparation programs are structural beneficiaries with low directionality: they collect rents from curriculum sales and certification, and can arbitrage to new models if market conditions shift. District curriculum coordinators are agenda-setters with moderate directionality: they administer the constraint and benefit from institutional stability, though their exit is constrained by sunk costs and political commitments. Classroom teachers are dual-positioned (beneficiary/payer) with identity-locked exit: many fused their professional self-concept with balanced literacy practices, which pulls directionality toward the symmetric-to-target range despite incidental benefits. Struggling readers are full targets (powerless, trapped) with directionality near 1.0. Families paying for remediation are high-directionality payers with constrained geographic exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by separating the genuine coordination function â resolving an intractable methodological dispute for classroom practitioners â from the extraction mechanism: the institutional capture of curriculum procurement and teacher training that persists even as empirical evidence accumulates against cueing-based instruction. If the founding problem (reading wars polarization) is dead but the arrangement persists primarily to protect institutional investments, the constraint drifts toward piton; if the problem remains live because teachers genuinely need an integrative framework, it stays tangled rope. The founding_problem_status is contested, with corroboration split between the benefiting institutions and external researchers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explicit_phonics_dilution,
    'Does the balanced literacy framework structurally dilute explicit phonics instruction below the threshold required for at-risk readers, or does it genuinely preserve sufficient phonics intensity within a mixed model?',
    'Randomized controlled trials comparing reading outcomes of balanced literacy versus explicit systematic phonics for at-risk populations; classroom observation studies measuring minutes of explicit phonics instruction in nominally balanced classrooms.',
    'If dilution is structural, the constraint is more extractive than its coordination story suggests; if phonics is preserved at sufficient intensity, the coordination function dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_phonics_dilution, empirical, 'Whether balanced literacy structurally undermines the phonics component it claims to include').

omega_variable(
    institutional_inertia_vs_genuine_belief,
    'Does balanced literacy persist because practitioners genuinely believe it optimizes reading acquisition, or because institutional investments in curriculum, training, and assessment infrastructure create sunk-cost lock-in?',
    'Surveys of teacher and administrator beliefs paired with curriculum adoption and abandonment data; observation of whether districts change frameworks when presented with robust countervailing evidence.',
    'If inertia dominates, the constraint drifts toward piton; if genuine belief dominates, it remains a contested coordination mechanism with live adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_genuine_belief, conceptual, 'Whether persistence is driven by authentic pedagogical commitment or institutional sunk costs').

omega_variable(
    kernel_reading_contest_location,
    'Is the disagreement between balanced literacy and its siblings located in empirical claims about reading acquisition, or in normative claims about teacher autonomy and childhood experience?',
    'Analysis of whether empirical convergence on phonics importance resolves the policy dispute, or whether the dispute persists on normative grounds regardless of evidence.',
    'If empirical, the reading with superior evidence should dominate; if normative, the kernel is irreducibly polysemic and requires political rather than scientific resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the kernel contest is empirical or normative in nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 8, 0.3).
narrative_ontology:measurement(read_tr_t17, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 17, 0.38).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.42).
narrative_ontology:measurement(read_tr_t34, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 34, 0.45).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(read_be_t17, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 17, 0.52).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(read_be_t34, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 34, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(read_su_t17, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 17, 0.6).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(read_su_t34, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 34, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_legitimacy kernel, which decomposes into structurally distinct claims about how reading should be taught. Balanced literacy integration claims both decoding and meaning-making are necessary; phonics_decoding_primacy privileges the alphabetic principle; whole_language_meaning_primacy privileges authentic text immersion; structured_literacy_remediation prioritizes explicit systematic instruction for vulnerable learners. Each reading has a different epsilon, beneficiary structure, and effective extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
