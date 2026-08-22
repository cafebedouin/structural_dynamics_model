% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   This constraint instantiates the whole_language_reading of the
 *   literacy_acquisition_kernel. It asserts that reading develops naturally
 *   through meaningful engagement with connected text, rendering explicit
 *   phonics instruction unnecessary and potentially harmful to motivation.
 *   The constraint is actively enforced through teacher preparation programs,
 *   curriculum adoption criteria, and classroom-level instructional materials
 *   (leveled readers, cueing systems). While it preserves professional
 *   autonomy for whole-language practitioners, it asymmetrically extracts
 *   reading competence from students who lack print-rich home environments,
 *   transferring the burden of decoding development to familial background
 *   knowledge. The sibling readingsâphonics_reading,
 *   balanced_literacy_reading, and structured_literacy_readingâare
 *   structurally distinct constraints with different epsilon profiles, victim
 *   sets, and beneficiary structures, linked in a constraint family.
 *
 * KEY AGENTS:
 *   - whole_language_practitioners: Primary agenda-setter and beneficiary (organized/identity_locked) â enforces the pedagogy and captures professional autonomy.
 *   - students_without_print_rich_homes: Primary target (powerless/trapped) â bears the extraction through reading failure when explicit decoding is withheld.
 *   - parents_in_low_income_communities: Excluded voice (powerless/constrained) â lacks capacity to alter district curriculum.
 *   - reading_researchers: Analytical observer (analytical/analytical) â documents the empirical failure mode but is marginalized in teacher training.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.6).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/psychological").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3').
narrative_ontology:cs_kernel_codification('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', fixed_text).
narrative_ontology:cs_authority_grounding('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', lineage).
narrative_ontology:cs_interpretation_layer_present('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3').
narrative_ontology:cs_reading_relation('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', literacy_acquisition_kernel__balanced_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_axiom('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', foundational, literacy_emerges_from_authentic_text_engagement).
narrative_ontology:cs_axiom_status(literacy_emerges_from_authentic_text_engagement, holdable).
narrative_ontology:cs_axiom_grounding('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', literacy_emerges_from_authentic_text_engagement, empirically_contingent).
narrative_ontology:cs_axiom('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', foundational, explicit_phonics_is_unnecessary_and_harmful).
narrative_ontology:cs_axiom_status(explicit_phonics_is_unnecessary_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', explicit_phonics_is_unnecessary_and_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', authentic_literacy_practice).
narrative_ontology:cs_drift_state('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f4bf839-83c2-4c4a-8d5a-ea06e7e913b3', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_practitioners).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, natural_literacy_emergence_hypothesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, professional_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement meaning-centered reading instruction in classrooms and schools, prioritize leveled texts and cueing systems over explicit phonics, and train new teachers in the same tradition. Their professional identity is constituted through facilitation of natural literacy development rather than direct transmission of skills.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_practitioners, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, whole_language_practitioners, beneficiary).

% Attend schools where reading instruction expects them to infer word meanings from context and picture cues rather than decode systematically. Without explicit phonics at home or school, they fall behind in early grades and face long-term literacy deficits. They cannot opt out of the district's mandated pedagogical approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes, payer,
    powerless, biographical, trapped, local).

% Seek effective reading instruction for their children but lack the political influence to change district curriculum or the resources to pay for supplemental tutoring outside school. Their requests for phonics-based intervention are often dismissed by educators as lacking trust in professional judgment.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_in_low_income_communities, excluded,
    powerless, biographical, constrained, local).

% Conduct empirical studies on reading acquisition, many finding that explicit phonics is necessary for all learners and especially critical for at-risk populations. Their findings are frequently sidelined in teacher preparation programs committed to whole-language lineage.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, whole_language_practitioners).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates teacher professional practice around a child-centered, meaning-first pedagogy that preserves educator autonomy and resists centralized scripting of instruction.
% TRANSFER_FUNCTION: Transfers the burden of initial decoding development from explicit school instruction to the child's home literacy environment; students with rich print exposure acquire reading naturally while those without are left to struggle.
% ABSENT_VOICES: Cognitive scientists and explicit phonics researchers are structurally excluded from curriculum decision-making in whole-language-dominant districts; parents of struggling readers lack voice in pedagogical policy; students themselves are not consulted.
% DISAPPEARANCE_RATIONALE: If the whole-language constraint disappeared, classroom practice would reorganize around explicit decoding instruction, teacher training programs would shift content, textbook adoption lists would change, and students without home literacy support would receive systematic phonics instruction rather than being expected to infer it from context.
% FOUNDING_PROBLEM: Early reading instruction was seen as overly rigid, scripted, and disconnected from meaning, producing rote decoders who lacked comprehension and love of reading.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language teacher educators attest the problem is still live. Cognitive scientists and reading researchers outside the benefiting party attest the founding problem was either historically overstated or has been solved by modern explicit instruction methods that integrate meaning; the National Reading Panel (2000) and subsequent meta-analyses from outside the whole-language tradition support the alternative framing.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) is moderate-high because the constraint systematically withholds explicit decoding instruction from children who most need it, producing long-term literacy deficits. Suppression (0.60) reflects the active exclusion of phonics alternatives through textbook adoption, teacher-education accreditation, and classroom-level resistance to scripted curricula. Theater_ratio (0.55) captures the growing performative dimension: as empirical challenges mount, whole-language instruction is increasingly reframed as 'balanced literacy' or 'workshop model' while retaining the core avoidance of systematic phonics. Accessibility_collapse (0.60) indicates that phonics alternatives are difficult to access within whole-language-dominant districts, though not impossible (home tutoring, private schools). Resistance (0.55) reflects sustained pushback from cognitive scientists, reading researchers, and parent advocates, especially post-National Reading Panel. The measurement series tracks the constraint's evolution from an insurgent progressive pedagogy (T=0, low extraction, low theater) to an entrenched institutional practice (T=45, higher extraction, higher theater) that persists despite empirical delegitimation.
 *
 * PERSPECTIVAL GAP:
 *   The practitioner seat experiences the constraint as autonomy-preserving coordination: it safeguards professional judgment against de-skilling and scripted curricula. The student seat experiences the same structural arrangement as extraction: the school abdicates the decoding-teaching function and expects the child to supply it from home. These divergent experiences are not merely opinions; they follow from the same structural facts (who controls the instructional agenda, who bears the cost of background-knowledge deficits). The engine computes the per-seat classification from these asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   whole_language_practitioners are declared beneficiaries and agenda-setters with identity_locked exit, placing their derived directionality near the beneficiary pole (low d). students_without_print_rich_homes are declared victims (payers) with trapped exit, placing their directionality near the target pole (high d). reading_researchers occupy the analytical seat with analytical exit, yielding near-symmetric d. The directionality derivation does not require overrides; the structural declarations are sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârigid, decontextualized phonics instruction producing disengaged readersâis contested and likely dead in the sense that modern explicit-instruction methods integrate meaning and motivation. The constraint persists because it still coordinates a genuine professional-identity function for practitioners (avoiding de-skilling). This prevents misclassification as a pure snare: there is real coordination value, but it is hybridized with asymmetric extraction on a vulnerable population. If the professional-identity coordination atrophied and the constraint persisted purely through institutional inertia, it would drift toward piton. The temporal measurements show theater_ratio rising, signaling that the coordination component is degrading into performance, but not yet fully atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_divergence,
    'How would the classification of this constraint change if evaluated under the phonics_reading or structured_literacy_reading sibling reading rather than the whole_language_reading?',
    'Comparative analysis of the sibling constraint stories in this kernel family; the kernel decomposition is already handled by separate constraint files.',
    'Sibling readings would likely reclassify the beneficiary/victim structure: explicit instruction readings would identify teachers as agenda-setters with lower autonomy benefit and would redistribute extraction to teachers forced into scripted curricula, while removing students_without_print_rich_homes from the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Structural divergence between whole-language and explicit-instruction readings of the literacy acquisition kernel.').

omega_variable(
    home_literacy_hidden_variable,
    'Does the whole-language constraint''s extraction on students_without_print_rich_homes arise from the pedagogy itself, or from the interaction between the pedagogy and unequal home resource distribution?',
    'Longitudinal studies comparing whole-language and explicit-phonics outcomes controlling for home literacy environment; if the gap closes under phonics, the extraction is pedagogy-specific.',
    'If the gap is pedagogy-specific, the constraint is extractive by design (assuming background knowledge). If the gap persists under phonics, the extraction is misattributed to pedagogy and belongs to socioeconomic inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_literacy_hidden_variable, empirical, 'Whether whole-language extraction is pedagogy-specific or a proxy for home literacy inequality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative phonics instruction structural (institutional curriculum mandates and textbook markets) or internalized (teacher identity fusion with whole-language ideology)?',
    'Examine whether phonics alternatives remain excluded in jurisdictions where curriculum mandates are lifted; if exclusion persists, suppression is heavily internalized.',
    'If internalized, the constraint persists even against policy change (higher effective suppression, piton-like inertial dynamics). If purely structural, policy reform could rapidly reclassify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of phonics alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(lite_tr_t45, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(lite_be_t45, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 45, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(lite_su_t45, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 45, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the literacy_acquisition_kernel, which decomposes into structurally distinct claims about how reading is acquired. The whole_language_reading asserts natural emergence from text exposure; sibling readings assert explicit instruction necessity. They are not the same constraint viewed from different anglesâtheir epsilon values, victim sets, and beneficiary structures differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
