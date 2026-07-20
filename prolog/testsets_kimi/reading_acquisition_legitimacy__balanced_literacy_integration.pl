% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Balanced Literacy Integration Pedagogical Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the balanced_literacy_integration reading of
 *   the reading_acquisition_legitimacy kernel. It treats legitimate reading
 *   instruction as requiring an integration of explicit phonics and authentic
 *   meaning-making literature. The constraint coordinates warring pedagogical
 *   factions under a single curricular umbrella while extracting from
 *   struggling readers, who often receive insufficient systematic decoding,
 *   and from taxpayers, who fund a professional-development and textbook
 *   industry built around the balance concept. It requires active enforcement
 *   through teacher preparation accreditation, textbook adoption criteria,
 *   and district-level instructional coaching. The claim and metrics are
 *   authored independently: the constraint is claimed as tangled_rope while
 *   metrics describe moderate extractiveness rising over the interval as the
 *   industry consolidated, then slightly declining under external scientific
 *   pressure.
 *
 * KEY AGENTS:
 *   - balanced_literacy_industry: Primary agenda-setter and beneficiary (institutional/arbitrage) â defines and sells the integrated curriculum
 *   - classroom_teachers: Primary payer (moderate/constrained) â implements the dual instructional load
 *   - struggling_readers: Primary target (powerless/trapped) â bears the opportunity cost of insufficient systematic phonics
 *   - explicit_phonics_proponents: Excluded voice (organized/constrained) â structurally sidelined from adoption decisions
 *   - cognitive_science_researchers: Analytical observer (institutional/analytical) â evaluates claims against reading science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.46).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.46).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Pedagogical Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'dc6e0b80-29c7-4526-8972-509ff23d1356').
narrative_ontology:cs_kernel_codification('dc6e0b80-29c7-4526-8972-509ff23d1356', distributed).
narrative_ontology:cs_authority_grounding('dc6e0b80-29c7-4526-8972-509ff23d1356', expertise).
narrative_ontology:cs_interpretation_layer_present('dc6e0b80-29c7-4526-8972-509ff23d1356').
narrative_ontology:cs_reading_relation('dc6e0b80-29c7-4526-8972-509ff23d1356', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('dc6e0b80-29c7-4526-8972-509ff23d1356', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('dc6e0b80-29c7-4526-8972-509ff23d1356', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('dc6e0b80-29c7-4526-8972-509ff23d1356', foundational, decoding_and_meaning_are_coequal_prerequisites).
narrative_ontology:cs_axiom_status(decoding_and_meaning_are_coequal_prerequisites, holdable).
narrative_ontology:cs_axiom_grounding('dc6e0b80-29c7-4526-8972-509ff23d1356', decoding_and_meaning_are_coequal_prerequisites, empirically_contingent).
narrative_ontology:cs_axiom('dc6e0b80-29c7-4526-8972-509ff23d1356', foundational, teacher_professional_judgment_governs_balance).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_governs_balance, holdable).
narrative_ontology:cs_axiom_grounding('dc6e0b80-29c7-4526-8972-509ff23d1356', teacher_professional_judgment_governs_balance, conventional).
narrative_ontology:cs_reference_frame('dc6e0b80-29c7-4526-8972-509ff23d1356', integrative_pedagogical_synthesis).
narrative_ontology:cs_drift_state('dc6e0b80-29c7-4526-8972-509ff23d1356', post_naep_reading_crisis_acknowledgment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dc6e0b80-29c7-4526-8972-509ff23d1356', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_industry).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, explicit_phonics_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells balanced literacy curriculum packages, leveled-text libraries, and professional development seminars. Sets the conceptual framework for what counts as balanced practice by defining curricular scope and sequence. Revenues depend on districts adopting the integrated approach rather than pure phonics or whole-language programs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_industry, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_industry, beneficiary).

% Trains prospective teachers in balanced literacy methods, including guided reading, leveled texts, and contextualized phonics. Their enrollment and accreditation depend on alignment with state standards that favor the balanced framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs, beneficiary,
    institutional, generational, constrained, national).

% Gain continued legitimacy and curricular inclusion because the balanced framework incorporates authentic literature and meaning-making activities that whole language prioritizes. Their prior investments in leveled-text systems and workshop models retain institutional value under this reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, beneficiary,
    organized, generational, constrained, national).

% Are expected to toggle between explicit phonics lessons and facilitated guided-reading sessions using leveled texts. They bear the planning burden of maintaining two instructional systems and face ambiguity about which method to emphasize for which students.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Receive a mixture of phonics intervention and guided reading, but the balance often means less cumulative, systematic decoding practice than structured literacy would provide. They cannot opt out of the district's adopted curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Advocate for research-aligned systematic phonics instruction. They are frequently excluded from textbook adoption committees and district curriculum reviews when balanced literacy frameworks are already in place, and their recommended decodable text programs are rejected as insufficiently authentic.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, explicit_phonics_proponents, excluded,
    organized, generational, constrained, national).

% Study reading acquisition through the lens of cognitive psychology and neuroscience. They observe that the balanced framework often misrepresents the role of orthographic mapping and three-cueing, but their analytical seat does not set district policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_science_researchers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_industry).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the reading wars conflict by providing a pedagogical framework that nominally incorporates both systematic decoding instruction and authentic meaning-making literature exposure, allowing districts to adopt a single curriculum without fully siding with either phonics or whole-language camps.
% TRANSFER_FUNCTION: Moves legitimacy, textbook adoption dollars, and teacher training capacity toward curricular products and professional development frameworks that brand themselves as balanced; moves cognitive and opportunity costs toward students who receive insufficient systematic phonics and teachers forced to navigate contradictory training.
% ABSENT_VOICES: Systematic phonics researchers and cognitive scientists who view three-cueing as inconsistent with reading science are often excluded from curriculum adoption committees; parents of struggling readers lack representation in district instructional-materials selection.
% DISAPPEARANCE_RATIONALE: If the balanced literacy mandate vanished, districts would revert to purist camps or structured literacy alternatives, textbook markets would fragment away from integrated packages, teacher training would re-specialize, and the intermediate professional-development industry that sells balance would contract significantly.
% FOUNDING_PROBLEM: The polarized reading wars of the 1980s-1990s, where districts faced destructive conflict between phonics-first and whole-language factions, producing incoherent classroom practice and political gridlock over textbook adoption.
% FOUNDING_PROBLEM_CORROBORATION: Education historians and policy scholars outside the balanced-literacy industry acknowledge the reading wars as a genuine policy conflict. Cognitive scientists and reading researchers outside the beneficiary set argue the wars were exacerbated by the false dichotomy the balanced frame itself perpetuates, and that the founding problem was better understood as inadequate teacher knowledge of phonics rather than a need for integration. No purely neutral corroborator exists.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.46, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46) reflects the opportunity cost to struggling readers and the resource transfer to the balanced-literacy industry; suppression (0.55) captures the marginalization of systematic phonics advocates in curriculum adoption and teacher training; theater_ratio (0.40) acknowledges that many balanced classrooms perform phonics as a minor adjunct to leveled-text guided reading. Accessibility_collapse (0.50) is moderate because systematic phonics alternatives exist but are stigmatized; resistance (0.60) is elevated due to sustained pushback from cognitive scientists and phonics researchers. Temporal measurements show extraction and theater rising through 2005-2015 as balanced literacy became the default institutional framework, then slightly moderating after 2015 as empirical challenges accumulated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (industry, preparation programs) experiences the constraint as a legitimate synthesis that preserves professional autonomy and reduces political conflict; the payer seats (struggling readers, classroom teachers) experience it as an imposed ambiguity that withholds more effective systematic instruction. The excluded seat (explicit phonics proponents) experiences it as active suppression of research-aligned practice. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The balanced-literacy industry and whole-language advocates are structural beneficiaries (low d): their revenues and legitimacy are subsidized by the constraint. Classroom teachers and struggling readers are structural targets (high d): they bear the implementation and opportunity costs. Explicit phonics proponents are excluded from the beneficiary structure and experience the constraint's suppression directly. Cognitive science researchers occupy the analytical seat with arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiaries (the coordination function: resolving the reading wars, keeping whole-language legitimacy alive) and victims (the extraction function: struggling readers, sidelined phonics advocates). A pure rope would show no victims and negligible enforcement; a pure snare would show no genuine coordination function. The founding problem (reading wars polarization) is contested â it may have been manufactured or perpetuated by the same framework that claims to solve it â which triggers mandatrophy scrutiny without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balanced_integration_sincerity,
    'Is balanced literacy a genuine integration of decoding and meaning-making, or is it whole-language instruction with phonics performatively appended?',
    'Classroom observation studies measuring time allocation and instructional method between decodable text practice, explicit phonics, and guided reading with leveled texts.',
    'If phonics is theater within the balanced framework, effective extractiveness is higher than the coordination framing suggests and the constraint shifts toward snare-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_integration_sincerity, empirical, 'Whether the phonics component is substantive or performative within balanced literacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For teachers trained in balanced literacy, is the marginalization of systematic phonics externally enforced by district mandates or internalized through their professional training?',
    'Post-retraining suppression trajectory: if teachers continue to resist systematic phonics after explicit policy change and retraining, the suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because teachers carry the resistance to alternatives with them even when policy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of systematic phonics alternatives.').

omega_variable(
    kernel_contest_location,
    'Does reading acquisition legitimacy rest on a balanced integration of phonics and meaning-making, or on the primacy of one process over the other?',
    'Longitudinal randomized controlled trials comparing balanced literacy, systematic phonics, and whole-language-only instruction on decoding and comprehension outcomes, with fidelity-of-implementation measurement.',
    'If systematic phonics alone produces superior outcomes, the balanced integration reading loses legitimacy and collapses toward phonics_decoding_primacy; if meaning-making immersion alone suffices, it collapses toward whole_language_meaning_primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_location, empirical, 'This constraint is one reading of the reading_acquisition_legitimacy kernel; the contest is over whether legitimacy requires integration or primacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.22).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.28).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.34).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.4).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.46).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.44).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 35, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 35, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 35, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
