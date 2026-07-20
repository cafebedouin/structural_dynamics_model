% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole-Language Meaning-Making Primacy in Reading Acquisition
 *   domain: education/cognitive_science
 *
 * SUMMARY:
 *   This constraint instantiates the whole_language_meaning_primacy reading
 *   of the contested reading_acquisition_legitimacy kernel. It asserts that
 *   reading is fundamentally meaning-making and that decoding emerges
 *   naturally when children are immersed in authentic literature. The
 *   doctrine is instantiated in teacher-preparation programs, district
 *   curriculum adoptions, and classroom-level facilitation models. While it
 *   solves a genuine coordination problemâproviding a unified professional
 *   framework for literacy instructionâit asymmetrically extracts
 *   educational opportunity from vulnerable learners who do not spontaneously
 *   infer phonics from text. The claim/metric independence is maintained: the
 *   constraint is claimed as tangled_rope, and the metrics describe
 *   substantial extraction, active suppression of phonics alternatives, and
 *   rising theater as the gap between meaning-making claims and decoding
 *   outcomes widens.
 *
 * KEY AGENTS:
 *   - literacy_education_establishment: Primary agenda-setter (institutional/identity_locked) â controls teacher preparation, credentialing, and professional development
 *   - struggling_readers: Primary target (powerless/trapped) â bear the extraction of withheld explicit decoding instruction
 *   - classroom_teachers: Coordinated beneficiary (moderate/identity_locked) â receive professional identity, autonomy, and facilitator role
 *   - parents_of_struggling_readers: Structurally excluded (moderate/constrained) â excluded from curriculum decisions and dismissed as non-expert
 *   - reading_science_researchers: Analytical observer (organized/mobile) â document failure but lack enforcement leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.68).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole-Language Meaning-Making Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'bc310d57-066f-4f81-94a2-711d821a5e91').
narrative_ontology:cs_kernel_codification('bc310d57-066f-4f81-94a2-711d821a5e91', formalized).
narrative_ontology:cs_authority_grounding('bc310d57-066f-4f81-94a2-711d821a5e91', lineage).
narrative_ontology:cs_interpretation_layer_present('bc310d57-066f-4f81-94a2-711d821a5e91').
narrative_ontology:cs_reading_relation('bc310d57-066f-4f81-94a2-711d821a5e91', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('bc310d57-066f-4f81-94a2-711d821a5e91', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('bc310d57-066f-4f81-94a2-711d821a5e91', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('bc310d57-066f-4f81-94a2-711d821a5e91', foundational, decoding_emerges_from_immersion).
narrative_ontology:cs_axiom_status(decoding_emerges_from_immersion, holdable).
narrative_ontology:cs_axiom_grounding('bc310d57-066f-4f81-94a2-711d821a5e91', decoding_emerges_from_immersion, empirically_contingent).
narrative_ontology:cs_axiom('bc310d57-066f-4f81-94a2-711d821a5e91', foundational, teacher_facilitates_not_instructs).
narrative_ontology:cs_axiom_status(teacher_facilitates_not_instructs, holdable).
narrative_ontology:cs_axiom_grounding('bc310d57-066f-4f81-94a2-711d821a5e91', teacher_facilitates_not_instructs, conventional).
narrative_ontology:cs_reference_frame('bc310d57-066f-4f81-94a2-711d821a5e91', authentic_literacy_immersion).
narrative_ontology:cs_drift_state('bc310d57-066f-4f81-94a2-711d821a5e91', contemporary_reading_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bc310d57-066f-4f81-94a2-711d821a5e91', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_education_establishment).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls teacher preparation programs, state literacy standards, and professional development pipelines. Derives institutional authority from foundational whole-language theorists and certifies teachers in the facilitator model. Collects research funding, conference circuits, and policy influence by maintaining the meaning-making primacy framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_education_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% Implement guided reading and leveled-text instruction in classrooms. Receive professional identity and autonomy through the facilitator role, which reduces the burden of direct, scripted instruction. Their training equips them with running records and miscue analysis rather than systematic phonics protocols; exiting the framework requires retraining against their professional self-concept.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary,
    moderate, biographical, identity_locked, local).

% Childrenâespecially those with dyslexia, limited home literacy exposure, or povertyâwho do not spontaneously infer decoding rules from immersive exposure. They receive individualized support framed as guided reading but are denied explicit, cumulative phonics instruction. Their schooling trajectory is constrained by the instructional method assigned to their classroom.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Observe their children failing to acquire decoding skills and request explicit phonics or structured literacy interventions. They are typically told the child needs more time, more exposure, or better home literacy environments; their requests for alternative curricula are treated as non-professional interference in pedagogical expertise.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, excluded,
    moderate, biographical, constrained, local).

% Produce convergent evidence from cognitive neuroscience, psychology, and large-scale trials showing that explicit, systematic phonics instruction is necessary for most children and that decoding does not emerge naturally from immersion. They publish meta-analyses and testify to legislative bodies, but their findings are systematically filtered out of teacher-preparation coursework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_science_researchers, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified pedagogical framework for mass literacy instruction that positions the teacher as facilitator rather than instructor, organizes classrooms around authentic literature, and gives the teaching profession a coherent theoretical identity and shared practice vocabulary.
% TRANSFER_FUNCTION: Moves explicit decoding instruction away from struggling readers and toward the maintenance of a teacher-facilitator model, institutional control over literacy methodology, and the leveled-reader curriculum market.
% ABSENT_VOICES: Parents of children with reading disabilities and explicit-phonics advocates are structurally absent from curriculum committees and teacher-preparation program design; their objections are dismissed as lacking professional legitimacy or as politically motivated attacks on teachers.
% DISAPPEARANCE_RATIONALE: If the meaning-making primacy constraint vanished, school districts would shift to explicit, systematic phonics and structured literacy curricula; teacher preparation would retrain around decoding science; the guided-reading and leveled-text material markets would contract; and struggling readers would gain access to cumulative, diagnostic instructionâthe organizational architecture of early literacy would reorganize around decoding explicitness.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction was dominated by decontextualized phonics drills and basal readers that produced disengaged, non-fluent students who associated reading with boredom rather than meaning; whole language sought to restore motivation and comprehension by grounding literacy in authentic, meaningful texts.
% FOUNDING_PROBLEM_CORROBORATION: Reading scientists and parent advocacy organizations outside the literacy education establishment attest that the disengagement problem is now solvable through rich literature combined with explicit phonics, and that the persistence of whole-language dominance reflects institutional inertia rather than a live pedagogical necessity; the establishment itself claims the problem remains live, citing student engagement and love of reading.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness rises from 0.30 to 0.62 over the interval because the institutionalization of whole language progressively concentrated costs on struggling readers while the establishment consolidated control over teacher training. Suppression tracks the establishment's defensive response to reading-science challenges: as empirical critiques mounted, enforcement of the facilitator model intensified through state standards, textbook adoption criteria, and professional-development mandates. Theater rises from 0.15 to 0.42 because an increasing share of classroom activity performs literacy engagement (leveled libraries, guided reading rituals, running records) while underlying decoding proficiency for at-risk groups stagnates or declines.
 *
 * PERSPECTIVAL GAP:
 *   From the literacy education establishment's seat, the constraint is professional coordination: a coherent theory, a dignified teacher role, and a research tradition. From the struggling reader's seat, the same structure is the absence of the explicit instruction they need to access text. The teacher seat sits near the beneficiary end but is identity-locked: they experience the constraint as professional belonging, not extraction, even when their students fail to decode.
 *
 * DIRECTIONALITY LOGIC:
 *   The literacy education establishment and classroom teachers are declared beneficiaries, yielding low directionality: the constraint subsidizes their professional identity and institutional position. Struggling readers are declared victims (payer role), yielding high directionality: the constraint extracts educational opportunity from them by substituting immersion for explicit decoding instruction. Parents are excluded rather than victimized in the structural derivation, reflecting their positional absence from the decision architecture rather than direct cost-bearing. Reading scientists are observers with mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdisengagement from decontextualized phonics drillsâhas been superseded. Contemporary evidence demonstrates that engagement and explicit decoding are not mutually exclusive. The constraint persists because the institutional architecture (teacher preparation, publishing markets, state standards) is organized around it, not because the original problem remains live. This is a classic mandatrophy pattern: the coordination function decayed, but the extraction and institutional identity it sustains keep it in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_emergence_empirical_status,
    'Does decoding competence emerge naturally from immersive exposure to authentic text for all learners, or only for those with rich home literacy environments and intact phonological processing?',
    'Longitudinal randomized controlled trials comparing pure immersion to explicit systematic phonics across diverse learner populations, including neuroimaging studies of reading circuitry formation.',
    'If decoding does not emerge naturally for a substantial subset, the constraint''s extraction from struggling readers is direct and the coordination story is cover; if it does emerge universally, the constraint is closer to genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_emergence_empirical_status, empirical, 'Empirical status of natural decoding emergence claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of explicit phonics in teacher training structural (institutional control of credentialing and state standards) or internalized (teachers believe explicit instruction harms children and destroys love of reading)?',
    'Post-exit suppression trajectory: observe whether teachers trained in whole language continue to resist explicit phonics after leaving establishment-controlled institutions (e.g., moving to charter or private settings with full autonomy).',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly through cognitive capture; if purely structural, removal of institutional gatekeeping would rapidly shift practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    coordination_extraction_boundary,
    'Is the facilitation/authentic-text framework structurally necessary for reading engagement, or is engagement achievable alongside explicit, systematic decoding instruction?',
    'Comparative outcome studies measuring both decoding proficiency and reading motivation in classrooms using structured literacy versus whole-language frameworks.',
    'If engagement is separable from the facilitation model, the constraint''s coordination function is separable from its extraction and the arrangement is tangled rope; if inseparable, the measured extraction includes necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 8, 0.22).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 16, 0.28).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 24, 0.35).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 32, 0.39).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(read_su_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_legitimacy kernel, which decomposes into structurally distinct claims: phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration, and structured_literacy_remediation. Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
