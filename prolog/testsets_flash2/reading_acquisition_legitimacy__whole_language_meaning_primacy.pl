% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the 'whole language' approach to reading
 *   instruction, which posits that reading is primarily a meaning-making
 *   process and that decoding skills emerge naturally through immersion in
 *   authentic literature. It emphasizes low-structure instruction, authentic
 *   texts from day one, and the teacher as a facilitator. Struggling readers
 *   receive individualized support via running records and guided reading.
 *   This is one reading of the broader 'reading_acquisition_legitimacy'
 *   kernel, which is highly contested in education policy and cognitive
 *   science.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.7).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '688a5368-620b-446f-9ff2-9a1e11539682').
narrative_ontology:cs_kernel_codification('688a5368-620b-446f-9ff2-9a1e11539682', implicit).
narrative_ontology:cs_authority_grounding('688a5368-620b-446f-9ff2-9a1e11539682', practice).
narrative_ontology:cs_interpretation_layer_present('688a5368-620b-446f-9ff2-9a1e11539682').
narrative_ontology:cs_reading_relation('688a5368-620b-446f-9ff2-9a1e11539682', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('688a5368-620b-446f-9ff2-9a1e11539682', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('688a5368-620b-446f-9ff2-9a1e11539682', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('688a5368-620b-446f-9ff2-9a1e11539682', foundational, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('688a5368-620b-446f-9ff2-9a1e11539682', reading_is_meaning_making, deontological).
narrative_ontology:cs_axiom('688a5368-620b-446f-9ff2-9a1e11539682', foundational, decoding_emerges_naturally).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally, holdable).
narrative_ontology:cs_axiom_grounding('688a5368-620b-446f-9ff2-9a1e11539682', decoding_emerges_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('688a5368-620b-446f-9ff2-9a1e11539682', child_centered_holistic_literacy).
narrative_ontology:cs_drift_state('688a5368-620b-446f-9ff2-9a1e11539682', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('688a5368-620b-446f-9ff2-9a1e11539682', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, publishers_of_authentic_literature).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_seeking_explicit_instruction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_as_facilitators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_as_facilitators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and implement whole language pedagogy, emphasizing immersion in authentic literature and natural emergence of decoding skills. They benefit from the institutionalization of their methods and associated professional development.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from curriculum mandates that prioritize authentic literature over decodable texts or phonics workbooks, increasing demand for their products.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, publishers_of_authentic_literature, beneficiary,
    powerful, biographical, mobile, national).

% Bear the primary cost of this approach if they do not naturally acquire decoding skills. They may fall behind academically, experience reduced literacy outcomes, and face long-term educational and economic disadvantages due to insufficient explicit instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Desire explicit phonics instruction for their children but are constrained by school district policies. They may resort to private tutoring or homeschooling, incurring additional costs and effort.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_seeking_explicit_instruction, payer,
    moderate, biographical, constrained, local).

% Are trained and expected to implement whole language methods, acting as facilitators rather than explicit instructors of decoding. They benefit from a pedagogical framework that aligns with their training but may struggle to support all learners effectively, leading to professional stress.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_as_facilitators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, teachers_as_facilitators, payer).

% Study reading acquisition and evaluate pedagogical approaches. Many provide evidence that challenges the efficacy of pure whole language for all learners, particularly for decoding skills.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_scientists_and_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates pedagogical practice around a unified philosophy of reading as meaning-making, fostering a consistent approach to literacy instruction across classrooms and schools.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources towards methods emphasizing authentic literature and natural decoding, away from explicit, systematic phonics instruction. It transfers the burden of decoding acquisition to the child's natural ability.
% ABSENT_VOICES: Advocates for systematic phonics and structured literacy are often marginalized in policy discussions dominated by whole language or balanced literacy proponents, despite strong empirical evidence supporting their methods for many learners.
% DISAPPEARANCE_RATIONALE: If the whole language meaning primacy constraint vanished, educational policy and classroom practice would rapidly shift towards more explicit and systematic phonics instruction, particularly for early readers and struggling learners. Curriculum materials would change, and professional development would reorient, leading to a significant reorganization of literacy education.
% FOUNDING_PROBLEM: Traditional phonics instruction was perceived as dry, decontextualized, and stifling to children's love of reading, failing to connect decoding with comprehension and meaning.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates maintain that the problem of decontextualized instruction is still live. Cognitive scientists and researchers, along with parents of struggling readers, argue that while the original problem was real, the whole language approach created new, more severe problems for decoding acquisition, and the founding problem is largely superseded by the need for evidence-based instruction.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the approach often fails to provide explicit decoding skills necessary for many children, particularly those with dyslexia or other learning differences, leading to significant educational costs for these learners. Suppression (0.70) is present through institutional resistance to alternative, more explicit methods, often framed as 'drill and kill.' The theater ratio (0.40) reflects that while some aspects of fostering a love for reading are genuine, a significant portion of the pedagogical effort is spent defending the 'natural emergence' premise despite evidence of its limitations for universal literacy. The metrics show a rise in extractiveness and suppression as the approach became more entrenched, followed by a slight decline as counter-evidence mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, this is a 'rope' or 'scaffold' that fosters a love of reading and holistic literacy. From the perspective of struggling readers and their parents, it operates as a 'snare' due to its high costs and suppressed alternatives. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and publishers of authentic literature are beneficiaries, as their professional identities and products are validated and promoted. Struggling readers and their parents are payers, bearing the costs of an instructional method that may not serve their needs. Teachers are both beneficiaries (aligned with training) and payers (stress from supporting struggling learners without explicit tools). Cognitive scientists are observers, providing analytical critique.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fostering a love of reading and holistic meaning-making) has not entirely atrophied, but its efficacy in ensuring universal decoding skills is highly contested. The classification as a tangled_rope acknowledges its genuine coordination function (a unified pedagogical philosophy) while highlighting the asymmetric extraction from vulnerable learners. This prevents mislabeling it as a pure rope (ignoring victims) or a pure snare (ignoring the coordination narrative).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_for_all_learners,
    'Does the whole language approach effectively teach decoding skills to all children, including those with learning differences?',
    'Longitudinal studies comparing literacy outcomes (decoding, fluency, comprehension) of diverse student populations taught with whole language versus explicit phonics or structured literacy.',
    'If found ineffective for a significant portion of learners, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying the constraint as a snare for those populations. If found universally effective, it would support a lower extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_for_all_learners, empirical, 'Uncertainty regarding the universal efficacy of whole language for decoding acquisition.').

omega_variable(
    pedagogical_identity_lock,
    'To what extent is adherence to whole language pedagogy driven by professional identity and ideological commitment rather than empirical evidence?',
    'Qualitative studies of teacher beliefs and professional development choices, and analysis of resistance to evidence-based instructional shifts in educational institutions.',
    'If identity-locked, the ''whole_language_advocates'' stakeholder''s exit_options would be more accurately ''identity_locked'', increasing their directionality and the constraint''s persistence due to internal rather than external factors. This would amplify the effective extraction from victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_lock, conceptual, 'Ambiguity regarding the role of identity in pedagogical adherence.').

omega_variable(
    kernel_reading_structural_delta,
    'What specific structural elements would change if a sibling reading of the ''reading_acquisition_legitimacy'' kernel were adopted?',
    'Comparative policy analysis of curriculum mandates, teacher training programs, and resource allocation in jurisdictions adopting different literacy pedagogies.',
    'Adopting ''phonics_decoding_primacy'' would shift resources to decodable texts and explicit phonics programs, reducing extractiveness for struggling readers but potentially increasing it for those who thrive on immersion. Adopting ''balanced_literacy_integration'' would attempt to mitigate the extremes, potentially lowering extractiveness and suppression but introducing new coordination challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Documents the structural differences between this reading and its siblings within the ''reading_acquisition_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
