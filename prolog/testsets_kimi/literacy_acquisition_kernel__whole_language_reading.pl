% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Whole Language Reading Pedagogy
 *   domain: educational/psychological
 *
 * SUMMARY:
 *   This constraint instantiates the whole-language reading of the
 *   literacy_acquisition_kernel: the pedagogical paradigm holding that
 *   reading emerges naturally from meaningful text engagement, that phonics
 *   develops implicitly through exposure, and that explicit decoding
 *   instruction is unnecessary and potentially harmful to motivation. It is
 *   authored as a tangled_rope because it provides genuine coordination for
 *   teacher autonomy and professional identity while asymmetrically
 *   extracting from students who lack print-rich home environments. The
 *   claim/metric independence principle is observed: the claimed type is
 *   tangled_rope and the metrics describe a moderately extractive, actively
 *   enforced constraint with rising theater as empirical counter-evidence
 *   accumulates.
 *
 * KEY AGENTS:
 *   - whole_language_policy_network: Agenda setter (institutional/mobile) â administers teacher training and curriculum standards
 *   - classroom_teachers: Primary beneficiary (organized/identity_locked) â professional autonomy and identity validated
 *   - students_without_home_literacy: Primary target (powerless/trapped) â bear the cost of withheld explicit instruction
 *   - cognitive_science_researchers: Analytical observer (institutional/analytical) â document efficacy gaps from outside
 *   - parents_of_struggling_readers: Excluded voice (moderate/constrained) â observe harm but lack override authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/psychological").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '16d17180-1286-4981-bdf1-398d249a7624').
narrative_ontology:cs_kernel_codification('16d17180-1286-4981-bdf1-398d249a7624', formalized).
narrative_ontology:cs_authority_grounding('16d17180-1286-4981-bdf1-398d249a7624', practice).
narrative_ontology:cs_interpretation_layer_present('16d17180-1286-4981-bdf1-398d249a7624').
narrative_ontology:cs_reading_relation('16d17180-1286-4981-bdf1-398d249a7624', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('16d17180-1286-4981-bdf1-398d249a7624', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('16d17180-1286-4981-bdf1-398d249a7624', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('16d17180-1286-4981-bdf1-398d249a7624', foundational, literacy_emerges_from_meaningful_text).
narrative_ontology:cs_axiom_status(literacy_emerges_from_meaningful_text, holdable).
narrative_ontology:cs_axiom_grounding('16d17180-1286-4981-bdf1-398d249a7624', literacy_emerges_from_meaningful_text, empirically_contingent).
narrative_ontology:cs_axiom('16d17180-1286-4981-bdf1-398d249a7624', foundational, explicit_phonics_instruction_harmful).
narrative_ontology:cs_axiom_status(explicit_phonics_instruction_harmful, holdable).
narrative_ontology:cs_axiom_grounding('16d17180-1286-4981-bdf1-398d249a7624', explicit_phonics_instruction_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('16d17180-1286-4981-bdf1-398d249a7624', natural_emergence_reference_frame).
narrative_ontology:cs_drift_state('16d17180-1286-4981-bdf1-398d249a7624', science_of_reading_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16d17180-1286-4981-bdf1-398d249a7624', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets teacher certification requirements, curriculum standards, and professional development norms that embed whole-language principles. Promotes the view that explicit phonics instruction is unnecessary and that reading emerges naturally from exposure to meaningful text. Benefits from grant funding, publishing revenue, and institutional prestige tied to the paradigm.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_policy_network, agenda_setter,
    institutional, generational, mobile, national).

% Trained in whole-language methods, they exercise professional autonomy to select literature and design meaning-centered activities. Their professional identity is validated by the paradigm's emphasis on teacher judgment over scripted curricula. They bear no direct cost from the constraint and are insulated from accountability for decoding outcomes by the framework's theoretical tenets.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, classroom_teachers, beneficiary,
    organized, biographical, identity_locked, regional).

% Enter school without the extensive prior exposure to print that the paradigm assumes all children possess. Because the curriculum withholds systematic phonics and explicit decoding instruction, they fail to acquire reading skills naturally and accumulate deficits that compound across grades. They cannot opt out of the assigned instructional method.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy, payer,
    powerless, biographical, trapped, local).

% Conduct empirical studies on reading acquisition, phonological awareness, and instructional efficacy. They report that explicit systematic phonics produces superior outcomes, especially for at-risk populations, but their findings are frequently excluded from teacher training and policy discourse dominated by the whole-language network.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_science_researchers, observer,
    institutional, generational, analytical, global).

% Observe their children failing to acquire reading in classrooms that withhold explicit decoding instruction. They would demand phonics-based intervention but are told by schools that meaning-based strategies are superior and that their children will catch up with more exposure. They lack the authority to override classroom methodology.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, excluded,
    moderate, biographical, constrained, local).

narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves teacher professional autonomy by delegating instructional decisions to classroom judgment rather than scripted curricula; coordinates classrooms around meaning-making and literature engagement as the primary path to literacy.
% TRANSFER_FUNCTION: Transfers the burden of foundational decoding acquisition from the institutional instructional system to the child's home environment; students with rich home literacy absorb the transfer invisibly, while students without absorb the cost as reading failure.
% ABSENT_VOICES: Parents of struggling readers, cognitive scientists studying phonological processing, and students themselves are largely absent from curriculum design; their objections are filtered through teacher-training gatekeepers who frame explicit phonics as mechanistic and harmful.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, teacher training programs would reorient around explicit systematic phonics, classroom time would shift from literature immersion to decoding instruction, and the population of students failing to acquire reading would drop substantially; the professional identity built on whole-language premises would lose its institutional foundation.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction was dominated by decontextualized phonics drills and basal readers that produced rote decoding without comprehension or motivation; children were bored and disengaged from literature.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers and reading outcomes data from jurisdictions that shifted to explicit phonics attest that the founding problem is solved by modern integrated phonics approaches; whole-language advocates attest it remains live. Corroboration from outside the benefiting parties comes from longitudinal reading achievement studies and international comparisons showing high literacy rates under explicit instruction regimes.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the asymmetric transfer of decoding burden to the home; it is not higher because the constraint genuinely reduces scripted-instruction burdens on teachers. Suppression (0.68) captures the active marginalization of phonics research in teacher training and policy. Theater_ratio (0.45) indexes the growing performative maintenance of natural acquisition narratives as cognitive science evidence accumulates. Accessibility_collapse (0.58) reflects that teachers trained in whole-language frameworks often cannot access or implement phonics instruction even when mandated. Resistance (0.52) measures the sustained empirical critique from reading scientists and policy reformers. The temporal grid uses one shared time base (0â40) to prevent misaligned substitution errors.
 *
 * PERSPECTIVAL GAP:
 *   The teacher seat and the disadvantaged-student seat diverge sharply: from the teacher's position the constraint preserves professional judgment and meaningful classroom activity (coordination); from the student's position the same structure withholds the explicit instruction required to crack the orthographic code (extraction). The policy network experiences low extraction and high subsidy (institutional prestige and funding). The engine computes this divergence from beneficiary/victim declarations and exit profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Teachers are declared beneficiaries with identity-locked exit, placing their directionality near the beneficiary pole (low d, damped chi). Students without home literacy are declared victims with trapped exit, placing their directionality near the target pole (high d, amplified chi). The policy network, as agenda setter with mobile exit, sits at the extreme beneficiary end. Cognitive scientists, as analytical observers with analytical exit, carry no directional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â disengagement from decontextualized phonics drills â was genuine but has been solved by modern integrated phonics and balanced approaches. The constraint persists beyond its problem's death because it subsidizes a valuable professional identity and institutional apparatus. Mandatrophy is not yet resolved (the constraint is ACTIVE). The tangled_rope classification prevents mislabeling the coordination component (teacher autonomy) as pure extraction, while the victim declaration prevents treating the whole structure as benign rope. The metrics and the claim are independent: a claimed tangled_rope with theater_ratio 0.55 and rising extraction would compute toward snare at the student seat, which is exactly the divergence the corpus exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_naturalness_ambiguity,
    'Is reading acquisition a naturally emergent language capacity akin to oral language development, or a culturally invented skill requiring explicit systematic instruction?',
    'Comparative neuroimaging and instructional efficacy studies across orthographies; evidence from non-print-rich societies on spontaneous literacy emergence.',
    'If natural, whole-language premises are partially vindicated as coordination; if invented, the constraint functions as extraction by withholding necessary instruction from dependent populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_naturalness_ambiguity, empirical, 'Whether literacy is a biologically primary or secondary skill.').

omega_variable(
    phonics_suppression_mechanism,
    'Is the exclusion of explicit phonics from classrooms driven by structural barriers (training, materials, policy) or by internalized ideology (teacher identity, belief in natural acquisition)?',
    'Post-policy-shift trajectories: if phonics adoption surges when mandates change, suppression was structural; if teachers resist adoption despite mandate, suppression is internalized.',
    'Internalized suppression would indicate higher effective extraction than structural measures suggest, as teachers carry the constraint with them across institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_suppression_mechanism, empirical, 'Structural vs internalized suppression of phonics alternatives.').

omega_variable(
    kernel_reading_decomposition,
    'Does this constraint''s classification hold if the kernel is reframed as a pedagogical preference rather than a theory of cognitive development?',
    'Cross-reading comparison: if the same practices are classified as identity_coordination under whole-language but as enforcement_mechanism under structured-literacy, the epsilon-invariance principle requires decomposition.',
    'Reframing would shift the constraint from tangled_rope toward snare or scaffold depending on whether the coordination function is seen as genuine or cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Framing under-determination of the literacy acquisition kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(lite_tr_t32, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(lite_be_t32, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(lite_su_t32, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the literacy_acquisition_kernel, which decomposes into structurally distinct claims about how reading is acquired. Whole-language and phonics-first readings have different epsilon values because they make contradictory empirical claims with different beneficiary/victim structures; they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
