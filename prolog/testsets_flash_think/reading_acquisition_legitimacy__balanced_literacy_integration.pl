% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy integration' reading of
 *   the 'reading acquisition legitimacy' kernel. It posits that legitimate
 *   reading instruction requires both decoding and meaning-making, balancing
 *   explicit phonics with authentic literature exposure. While aiming for a
 *   comprehensive approach, its implementation has often been criticized for
 *   insufficient explicit phonics, leading to significant costs for
 *   struggling readers. The constraint is claimed as a 'rope' by its
 *   proponents, but the authored metrics reflect its operation as a 'tangled
 *   rope' due to its extractive effects on vulnerable learners and active
 *   suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '9fa43c28-b2b5-41ec-ba79-35f7a9ddf361').
narrative_ontology:cs_kernel_codification('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', formalized).
narrative_ontology:cs_authority_grounding('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', extraction).
narrative_ontology:cs_interpretation_layer_present('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361').
narrative_ontology:cs_reading_relation('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', foundational, reading_is_a_complex_sociocognitive_process).
narrative_ontology:cs_axiom_status(reading_is_a_complex_sociocognitive_process, holdable).
narrative_ontology:cs_axiom_grounding('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', reading_is_a_complex_sociocognitive_process, conventional).
narrative_ontology:cs_axiom('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', foundational, instruction_must_address_both_code_and_meaning).
narrative_ontology:cs_axiom_status(instruction_must_address_both_code_and_meaning, holdable).
narrative_ontology:cs_axiom_grounding('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', instruction_must_address_both_code_and_meaning, conventional).
narrative_ontology:cs_reference_frame('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', holistic_literacy_development).
narrative_ontology:cs_drift_state('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', contemporary_reading_wars_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9fa43c28-b2b5-41ec-ba79-35f7a9ddf361', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_institutions).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell curriculum materials aligned with balanced literacy principles, benefiting from widespread adoption in schools and districts. They can adapt their offerings if pedagogical trends shift, but profit from the current dominant paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, educational_publishers, beneficiary,
    powerful, biographical, arbitrage, national).

% Shape pedagogical practices by training new teachers in balanced literacy methods. They benefit from the stability of this paradigm, which underpins their curriculum and professional development offerings. They also set the agenda for what is considered legitimate instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_institutions, agenda_setter).

% Implement balanced literacy curricula and instructional strategies. They benefit from a coherent framework but bear the burden of making it work for all students, often feeling pressure to adhere to district mandates even when they perceive shortcomings. Their professional identity is often tied to this approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, beneficiary).

% Are the primary targets of instruction, but often bear the costs of insufficient explicit phonics, leading to difficulties in decoding and comprehension. Their inability to read effectively can lead to identity-lock, impacting self-esteem and future educational opportunities.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, immediate, identity_locked, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring or advocating for changes in school policy. Their options are constrained by the dominant pedagogical approach in schools.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Advocate for more systematic, explicit phonics instruction based on cognitive science research. They are often excluded from mainstream curriculum development and teacher training, despite presenting evidence of balanced literacy's shortcomings for many learners.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_researchers_phonics_advocates, excluded,
    organized, generational, analytical, global).

% Set state and district-level curriculum standards and teacher certification requirements, often influenced by prevailing pedagogical theories. They can mandate shifts in instructional approaches but face political and institutional inertia.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, education_policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a comprehensive and unified pedagogical framework for reading instruction that integrates both decoding skills and meaning-making strategies, aiming to resolve historical 'reading wars' by finding a middle ground.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design influence to proponents of balanced literacy, and financial resources to associated publishers and training programs. It transfers the burden of 'making sense' of reading to struggling learners when explicit decoding instruction is insufficient.
% ABSENT_VOICES: Advocates for systematic phonics-first approaches, including many cognitive scientists, dyslexia advocates, and parents of struggling readers, are often marginalized in policy discussions and curriculum adoption processes, despite presenting strong empirical evidence.
% DISAPPEARANCE_RATIONALE: If the balanced literacy paradigm vanished overnight, there would be a significant and rapid shift in curriculum, teacher training, and instructional practices across many schools and districts, likely towards more explicit, systematic phonics-based approaches, especially for early reading. Educational publishing markets would also reorganize.
% FOUNDING_PROBLEM: To move beyond the polarized 'reading wars' (phonics vs. whole language) by integrating perceived strengths of both, providing a more holistic and less dogmatic approach to literacy instruction that addresses both code and meaning.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (teacher training institutions, some policy makers) argue the problem of integrating diverse instructional needs is still live. Critics (literacy researchers, parents of struggling readers) argue the original problem has shifted, and the current implementation of balanced literacy creates new problems, with independent research and parent advocacy groups corroborating this shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is driven by the costs borne by struggling readers who do not receive adequate systematic phonics instruction under this paradigm, leading to literacy gaps. Suppression (0.75) is high due to the institutional dominance of balanced literacy in teacher training and curriculum mandates, which actively marginalizes or excludes alternative, more phonics-intensive approaches. The theater ratio (0.4) reflects that while 'phonics' is often mentioned as part of the 'balance,' its implementation can be superficial or inconsistent, making the 'balance' partly performative. The temporal measurements show a gradual increase in extractiveness and theater, reflecting the accumulating evidence of its shortcomings and the growing gap between stated intent and actual impact.
 *
 * PERSPECTIVAL GAP:
 *   Proponents (teacher training, publishers) perceive this as a beneficial coordination mechanism that offers a holistic approach to literacy. However, struggling readers and their advocates experience it as an extractive system that fails to provide essential decoding skills, trapping them in literacy difficulties. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational publishers and teacher training institutions are beneficiaries (low d) as they profit from and shape the dominant pedagogical framework. Struggling readers and their parents are targets (high d) as they bear the costs of the constraint's shortcomings. Classroom teachers are in a mixed position, benefiting from a coherent framework but also bearing the burden of implementation and student outcomes. Literacy researchers advocating for phonics are excluded, their alternatives suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to synthesize and move beyond the 'reading wars.' However, critics argue that its implementation has drifted, and while the 'balance' narrative persists, the actual coordination function has atrophied for many learners, becoming a cover for an extractive system. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict, combined with rising extractiveness, suggest a potential mandatrophy where the constraint's persistence serves institutional interests more than its stated educational goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_for_all_learners,
    'Is the ''balance'' in balanced literacy truly effective for all learners, especially those with specific learning difficulties (e.g., dyslexia) or from disadvantaged backgrounds?',
    'Longitudinal studies comparing literacy outcomes of diverse student populations under balanced literacy versus systematic phonics approaches, controlling for socioeconomic factors and teacher training fidelity.',
    'If balanced literacy is shown to be significantly less effective for vulnerable populations, its extractiveness would be re-evaluated as higher, and its classification might shift closer to a Snare for those groups. If equally effective, its coordination function would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_for_all_learners, empirical, 'Empirical question about the differential impact of balanced literacy on diverse learners.').

omega_variable(
    implementation_fidelity_drift,
    'To what extent is the balanced literacy approach implemented as intended, or does it drift towards less explicit phonics instruction in actual classroom practice?',
    'Classroom observation studies, teacher surveys on instructional time allocation, and curriculum analysis to assess the actual proportion and explicitness of phonics instruction versus other components.',
    'If implementation fidelity is low regarding explicit phonics, the ''theater_ratio'' would be higher, indicating a greater gap between stated policy and actual practice, reinforcing the ''tangled rope'' or ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_drift, empirical, 'Assesses the gap between prescribed balanced literacy pedagogy and actual classroom implementation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative pedagogical approaches (e.g., systematic phonics) structural (policy, curriculum mandates) or internalized (teacher belief, professional identity, lack of training)?',
    'Analysis of policy changes versus shifts in teacher attitudes and practices following professional development or new research dissemination. If suppression persists after structural barriers are removed, it indicates internalized components.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as teachers carry the constraint within their professional identity, making reform more challenging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative literacy pedagogies.').

omega_variable(
    framing_under_determination_pedagogical_legitimacy,
    'Is ''balanced literacy integration'' the most appropriate framing for this constraint, or would a ''pedagogical consensus maintenance'' framing better capture its function?',
    'Analyze whether the primary function is genuinely integrating instructional methods or maintaining a professional consensus that benefits certain institutional actors. If the latter, the ''extraction'' component is more central than ''coordination''.',
    'A ''pedagogical consensus maintenance'' framing might shift the ''authority_grounding'' in cs_structure towards ''extraction'' and increase the ''extractiveness'' metric, potentially reclassifying it as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_pedagogical_legitimacy, conceptual, 'Alternative framing for the constraint''s primary function and legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 6, 0.3).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 12, 0.35).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 18, 0.38).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 24, 0.39).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t6, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(read_be_t18, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(read_su_t6, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(read_su_t18, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, which decomposes into multiple structurally distinct claims about legitimate reading instruction. This reading focuses on the integration of phonics and meaning-making.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
