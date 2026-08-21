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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the pedagogical approach known as 'Whole
 *   Language' in reading instruction, which posits that reading is primarily
 *   a meaning-making process and that decoding skills emerge naturally
 *   through immersion in authentic literature. It stands in contrast to
 *   approaches emphasizing explicit phonics. This story instantiates one
 *   reading of the broader 'reading_acquisition_legitimacy' kernel, focusing
 *   on the structural implications of prioritizing meaning over explicit
 *   decoding instruction.
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
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '3149e583-b475-4ac8-8f4a-ac2ae4758f5a').
narrative_ontology:cs_kernel_codification('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', implicit).
narrative_ontology:cs_authority_grounding('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', practice).
narrative_ontology:cs_interpretation_layer_present('3149e583-b475-4ac8-8f4a-ac2ae4758f5a').
narrative_ontology:cs_reading_relation('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', reading_acquisition_legitimacy__balanced_literacy_integration, forecloses).
narrative_ontology:cs_reading_relation('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_axiom('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', foundational, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', reading_is_meaning_making, conventional).
narrative_ontology:cs_axiom('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', foundational, decoding_emerges_naturally).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally, holdable).
narrative_ontology:cs_axiom_grounding('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', decoding_emerges_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', child_centered_holistic_literacy).
narrative_ontology:cs_drift_state('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3149e583-b475-4ac8-8f4a-ac2ae4758f5a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers_whole_language).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_career_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, often holding positions in education departments and teacher training programs. Their professional identity is deeply tied to this pedagogical philosophy, and they benefit from its continued adoption through influence and curriculum sales.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Profit from the sale of authentic literature sets and guided reading materials aligned with whole language principles. They benefit from the market created by this pedagogical approach and resist shifts towards more structured phonics programs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers_whole_language, beneficiary,
    organized, biographical, constrained, national).

% Are placed in an instructional environment that may not provide the explicit decoding skills they need, leading to persistent reading difficulties. Their academic future is directly impacted by the pedagogical choices made for them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring or advocating for different instructional methods within the school system. Their options are limited by available school choices and resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Are trained in whole language methods and expected to implement them, even if they observe students struggling. Their professional identity and career progression are tied to adhering to the dominant pedagogical philosophy in their training and school district.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_career_teachers, payer,
    moderate, biographical, identity_locked, local).

% Conduct research on reading acquisition, often finding strong evidence for the importance of explicit phonics instruction. They observe the outcomes of different pedagogical approaches and advocate for evidence-based practices, but their influence on policy can be slow.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_scientists_literacy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes reading for meaning and fosters a love of literature, providing a consistent framework for teachers and curriculum developers.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum development resources to proponents of whole language, while transferring the burden of decoding acquisition to the child's natural emergence process, often at the cost of explicit instruction for struggling learners.
% ABSENT_VOICES: Neuroscientists and cognitive psychologists who study reading acquisition, often advocating for explicit phonics, are frequently marginalized in education policy discussions dominated by pedagogical theorists. Parents of children with dyslexia, who desperately need structured phonics, are often unheard in systems committed to whole language.
% DISAPPEARANCE_RATIONALE: If the whole language meaning primacy constraint vanished, educational institutions would be forced to re-evaluate their literacy curricula, likely leading to a rapid shift towards more explicit and systematic phonics instruction, especially for early readers. Teacher training would change, and curriculum publishers would adapt to new demands.
% FOUNDING_PROBLEM: Traditional phonics instruction was perceived as dry, decontextualized, and stifling to children's love of reading, leading to a desire for a more natural, engaging, and meaning-centered approach.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates maintain that the problem of disengaged readers persists and their approach is the solution. However, cognitive scientists and many parents of struggling readers argue that the original problem was misdiagnosed or overcorrected, and that the current approach creates new problems, citing decades of research on reading acquisition.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate a holistic approach to literacy (benefiting advocates and publishers) but does so through a structure that extracts significant costs from struggling readers and early-career teachers who are identity-locked into the method. Extractiveness is high due to the academic and social costs borne by children who do not 'naturally' acquire decoding skills. Suppression is high because alternative, evidence-based methods (like systematic phonics) are actively marginalized or dismissed within institutions dominated by whole language philosophy. The theater ratio reflects the performative aspects of 'authentic' reading experiences that may mask a lack of foundational skill development for some learners. The historical measurements show an increase in extractiveness and suppression as the whole language approach gained dominance and faced increasing resistance from scientific evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, this is a beneficial Rope, fostering a love of reading and natural development. From the perspective of struggling readers and their parents, it is a Snare, trapping children in a system that fails to provide essential skills. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and curriculum publishers are clear beneficiaries, shaping the pedagogical agenda and profiting from aligned materials. Struggling readers and their parents are direct victims, bearing the costs of inadequate instruction. Early-career teachers are also victims, often identity-locked into a system that may conflict with their observations of student needs. Cognitive scientists act as analytical observers, providing evidence that often challenges the constraint's premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fostering a love of reading and natural acquisition) has arguably outlived its functional utility for a significant portion of the student population, particularly those with dyslexia or other learning differences. The persistence of the constraint, despite mounting evidence for more explicit instruction, suggests a degree of mandatrophy, where the original coordination function is overshadowed by the maintenance of a particular pedagogical ideology and its associated institutional benefits. The 'contested' status of the founding problem highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_emergence_naturalness,
    'Does decoding truly emerge naturally for all children through immersion in authentic literature, or is explicit instruction necessary for a significant portion of learners?',
    'Longitudinal studies comparing reading outcomes in whole language vs. explicit phonics classrooms, particularly for diverse learner populations and those with predispositions to reading difficulties.',
    'If explicit instruction is found necessary for many, the ''natural emergence'' premise of whole language is undermined, shifting its classification towards a Snare for those learners. If natural emergence is universal, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoding_emergence_naturalness, empirical, 'Empirical validity of the ''natural emergence'' hypothesis for decoding skills.').

omega_variable(
    pedagogical_identity_lock,
    'To what extent is adherence to whole language principles driven by professional identity and institutional inertia, rather than ongoing pedagogical efficacy?',
    'Surveys and qualitative studies of teacher beliefs and practices, particularly in contexts where evidence-based alternatives are available but not adopted. Analysis of teacher training program curricula and their resistance to change.',
    'If identity-lock is a primary driver, the constraint''s suppression is more internalized and resistant to external evidence, amplifying its effective extractiveness for those trapped by the system. If efficacy is the driver, the constraint is more genuinely a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_lock, conceptual, 'Role of professional identity in maintaining whole language pedagogy despite conflicting evidence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative literacy pedagogies structural (e.g., curriculum mandates, administrative pressure) or internalized (e.g., teachers'' belief in whole language despite observed struggles)?',
    'Analysis of policy documents, school board minutes, and teacher interviews. If teachers express a desire for alternative methods but feel unable to implement them, structural suppression is dominant. If they genuinely believe whole language is superior despite evidence, internalized suppression is at play.',
    'If structural, interventions can target policy changes. If internalized, interventions must address professional development and belief systems, indicating a deeper, more persistent form of suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative literacy pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
