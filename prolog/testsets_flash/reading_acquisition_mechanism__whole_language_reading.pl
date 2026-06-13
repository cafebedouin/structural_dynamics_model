% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   This constraint describes the 'whole language' approach to reading
 *   instruction, which posits that children acquire reading skills implicitly
 *   through immersion in authentic texts, with decoding skills emerging
 *   naturally. It is a reading of the broader 'reading_acquisition_mechanism'
 *   kernel, which is highly contested in educational psychology. This
 *   specific reading is characterized by low initial instructional cost but
 *   high long-term remediation costs for struggling readers, and it maximizes
 *   teacher autonomy while disproportionately harming those who do not
 *   implicitly acquire decoding skills.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'e51fe3db-5c7f-4c7c-a949-dda8d51bf453').
narrative_ontology:cs_kernel_codification('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', distributed).
narrative_ontology:cs_authority_grounding('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', practice).
narrative_ontology:cs_interpretation_layer_present('e51fe3db-5c7f-4c7c-a949-dda8d51bf453').
narrative_ontology:cs_reading_relation('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', foundational, decoding_emerges_implicitly).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly, holdable).
narrative_ontology:cs_axiom_grounding('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', decoding_emerges_implicitly, empirically_contingent).
narrative_ontology:cs_axiom('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', foundational, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', reading_is_meaning_making, deontological).
narrative_ontology:cs_reference_frame('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', natural_language_acquisition_model).
narrative_ontology:cs_drift_state('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', contemporary_cognitive_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e51fe3db-5c7f-4c7c-a949-dda8d51bf453', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, publishers_of_authentic_texts).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, often through academic departments, professional organizations, and teacher training programs. Their professional identity and careers are often tied to this pedagogical philosophy. They benefit from the lower initial instructional cost and greater teacher autonomy it affords.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Are disproportionately harmed by the lack of explicit decoding instruction, leading to significant reading difficulties, academic setbacks, and long-term educational and economic disadvantages. They have no agency in pedagogical choices.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking expensive private tutoring or advocating for changes in school curricula. Their options are limited by school district policies and available resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Are trained in whole language methods and may find initial implementation less prescriptive, offering more autonomy. However, they often struggle to support students who do not implicitly acquire decoding skills, leading to burnout and frustration. Their professional identity is shaped by their training.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, beneficiary).

% Benefit from the emphasis on using 'authentic' literature rather than phonics-based readers, driving demand for their existing catalogs and new publications that fit the whole language philosophy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, publishers_of_authentic_texts, beneficiary,
    organized, generational, mobile, national).

% Conduct research on reading acquisition, often providing empirical evidence that challenges the implicit decoding assumption of whole language. Their influence is primarily through academic publications and expert testimony.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes reading for meaning and engagement with rich literature, fostering a love of reading and integrating literacy across the curriculum.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design away from explicit, systematic phonics instruction towards a more holistic, meaning-centered approach, shifting the burden of decoding acquisition onto the individual learner.
% ABSENT_VOICES: The voices of adult struggling readers who were taught under whole language methods are often absent from policy debates, as are the voices of parents who lack the resources or knowledge to advocate for alternative instructional methods. They would argue for explicit, evidence-based phonics instruction.
% DISAPPEARANCE_RATIONALE: If the whole language approach and its institutional support vanished overnight, there would be a rapid shift towards more explicit, systematic phonics instruction in schools. Teacher training programs would overhaul their curricula, and publishers would adapt their materials. The landscape of early literacy education would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding problem was a perceived overemphasis on rote, decontextualized phonics instruction that made reading tedious and disconnected from meaning, leading to disengaged readers.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem of disengaged readers is still live. Cognitive scientists and advocates for struggling readers, citing decades of empirical evidence, attest that the founding problem was misdiagnosed and that the whole language approach created new, more severe problems for many learners; independent educational researchers corroborate the shift in understanding.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high due to the significant academic and life costs borne by struggling readers who do not receive explicit phonics instruction. Suppression (0.70) is also high, as the pedagogical paradigm is often institutionally entrenched, making it difficult for parents or teachers to advocate for alternative methods. The theater ratio (0.40) reflects the performative aspect of 'reading for meaning' that often masks the underlying failure to teach foundational decoding skills to all students. The resistance (0.75) is high due to ongoing 'reading wars' and advocacy from parents and researchers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, this is a 'rope' that fosters a love of reading and holistic literacy. From the perspective of struggling readers and their parents, it operates as a 'snare' that traps children in a cycle of reading failure. The engine's computation of 'tangled_rope' reflects this hybrid nature, acknowledging both the coordination function (fostering engagement) and the asymmetric extraction (harm to struggling readers).
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and publishers of authentic texts are beneficiaries, as the approach aligns with their professional identities and commercial interests. Struggling readers and their parents are clear victims, bearing the costs of an ineffective pedagogical method. Early career teachers are in a dual position, benefiting from initial autonomy but often becoming victims of the system's failures when their students struggle. Cognitive scientists act as analytical observers, providing evidence that challenges the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fostering engaged readers) has not entirely atrophied, but its effectiveness for a significant portion of the population is highly contested. The persistence of the whole language approach, despite strong empirical evidence for explicit phonics, suggests a mandatrophy where the original coordination function is overshadowed by institutional inertia and the benefits to certain stakeholders (e.g., teacher autonomy, alignment with certain academic theories). The classification as a tangled_rope prevents mislabeling it as a pure rope (ignoring the victims) or a pure snare (ignoring the genuine intent to foster reading engagement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_decoding_efficacy,
    'Does decoding skill truly emerge implicitly from exposure to authentic texts for all learners, or is explicit instruction necessary for a significant portion of the population?',
    'Longitudinal studies comparing reading outcomes of children taught exclusively with whole language vs. explicit phonics, particularly for children with varying cognitive profiles.',
    'If explicit instruction is necessary for many, the constraint''s extractiveness and suppression are higher than currently measured, as it denies essential skills. If implicit emergence is universal, the constraint is more genuinely a coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_decoding_efficacy, empirical, 'Empirical validity of the implicit decoding acquisition claim.').

omega_variable(
    pedagogical_identity_lock,
    'To what extent is adherence to whole language pedagogy driven by professional identity and prior training, rather than ongoing empirical assessment?',
    'Surveys and qualitative studies of teacher beliefs and decision-making, particularly after exposure to conflicting research evidence. Analysis of curriculum adoption processes in school districts.',
    'If identity-lock is a primary driver, the constraint''s persistence is more resistant to empirical challenge, and the ''agenda_setter'' role of whole language advocates is more entrenched, making reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_lock, conceptual, 'Role of professional identity in maintaining pedagogical adherence.').

omega_variable(
    kernel_reading_structural_delta,
    'Is this reading''s structural delta (low initial cost, high remediation, teacher autonomy, harm to struggling readers) accurately captured, or are there other significant structural consequences?',
    'Comparative economic analysis of educational systems adopting different reading pedagogies, including long-term societal costs of illiteracy.',
    'If the delta is underestimated, the extractiveness and suppression metrics for this reading are too low, understating the harm. If overestimated, the coordination function is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, empirical, 'Accuracy of the structural consequences of the whole language reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel. Its core premise (implicit decoding) is distinct from the 'phonics_reading' (explicit decoding) and 'balanced_literacy_reading' (integrated approach) siblings. Each reading represents a distinct pedagogical constraint with different beneficiaries, victims, and empirical consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
