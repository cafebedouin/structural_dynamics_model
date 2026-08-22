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
 *   human_readable: Whole-Language Meaning-Primacy Reading Instruction
 *   domain: education/cognitive_science
 *
 * SUMMARY:
 *   This constraint is the whole_language_meaning_primacy reading of the
 *   reading_acquisition_legitimacy kernel. It holds that reading is
 *   fundamentally meaning-making and that legitimate instruction immerses
 *   children in authentic literature from the outset, allowing decoding to
 *   emerge naturally. Sibling readings include phonics_decoding_primacy
 *   (systematic explicit phonics), balanced_literacy_integration (synthetic
 *   balance), and structured_literacy_remediation (explicit cumulative design
 *   for vulnerable learners). The kernel conflates multiple structurally
 *   distinct claims about how reading is acquired; this reading is
 *   distinguished by its low-structure, facilitator-role, and authentic-text
 *   prerogative.
 *
 * KEY AGENTS:
 *   - whole_language_faculty: agenda_setter (institutional/arbitrage) â sets teacher-training curricula and pedagogical norms
 *   - classroom_teachers: beneficiary/coordinated agent (moderate/constrained) â implements the facilitative approach
 *   - typically_developing_readers: beneficiary (powerless/trapped) â often succeeds despite method, providing cover
 *   - struggling_readers: payer (powerless/trapped) â bears the cost of withheld explicit decoding instruction
 *   - students_with_dyslexia: payer (powerless/trapped) â denied access to structured literacy
 *   - parents_of_struggling_readers: excluded (moderate/constrained) â experiential knowledge discounted
 *   - cognitive_reading_researchers: observer (institutional/analytical) â sees the empirical mismatch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.76).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.74).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole-Language Meaning-Primacy Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'b5c88d1b-1514-4c9d-89b2-d080e578ac98').
narrative_ontology:cs_kernel_codification('b5c88d1b-1514-4c9d-89b2-d080e578ac98', fixed_text).
narrative_ontology:cs_authority_grounding('b5c88d1b-1514-4c9d-89b2-d080e578ac98', lineage).
narrative_ontology:cs_interpretation_layer_present('b5c88d1b-1514-4c9d-89b2-d080e578ac98').
narrative_ontology:cs_reading_relation('b5c88d1b-1514-4c9d-89b2-d080e578ac98', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('b5c88d1b-1514-4c9d-89b2-d080e578ac98', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('b5c88d1b-1514-4c9d-89b2-d080e578ac98', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('b5c88d1b-1514-4c9d-89b2-d080e578ac98', foundational, decoding_emerges_from_meaning_immersion).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaning_immersion, holdable).
narrative_ontology:cs_axiom_grounding('b5c88d1b-1514-4c9d-89b2-d080e578ac98', decoding_emerges_from_meaning_immersion, empirically_contingent).
narrative_ontology:cs_axiom('b5c88d1b-1514-4c9d-89b2-d080e578ac98', foundational, authentic_literature_prerogative).
narrative_ontology:cs_axiom_status(authentic_literature_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('b5c88d1b-1514-4c9d-89b2-d080e578ac98', authentic_literature_prerogative, conventional).
narrative_ontology:cs_reference_frame('b5c88d1b-1514-4c9d-89b2-d080e578ac98', meaning_making_origin).
narrative_ontology:cs_drift_state('b5c88d1b-1514-4c9d-89b2-d080e578ac98', science_of_reading_movement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5c88d1b-1514-4c9d-89b2-d080e578ac98', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, typically_developing_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They design teacher-preparation curricula, publish literacy research, and train new teachers to prioritize meaning-making through authentic literature, running records, and guided reading. Their professional authority and grant funding depend on the persistence of meaning-primacy frameworks in schools.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_faculty, agenda_setter,
    institutional, generational, arbitrage, national).

% They implement the facilitative approach in classrooms, conducting guided-reading groups and keeping running records. They receive professional identity, autonomy, and literature-based curriculum materials from the constraint, but are professionally disempowered when struggling students fail to acquire reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary,
    moderate, biographical, constrained, local).

% They often learn to read despite instructional method, benefiting from rich literature exposure and classroom meaning-making activities. Their visible success is used to validate the constraint while masking the failure of peers who do not respond to implicit instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, typically_developing_readers, beneficiary,
    powerless, biographical, trapped, local).

% They are immersed in authentic texts they cannot decode, expected to infer meaning from context and pictures. When they fail to read naturally, they receive more guided reading rather than explicit systematic phonics, prolonging failure and secondary harm.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% They require explicit, cumulative instruction in sound-symbol correspondence. The constraint withholds this in favor of meaning-based cues, denying them access to evidence-based interventions and routing them into long-term remediation or special education.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% They often request phonics-based tutoring or interventions for their children but are told the school uses research-based balanced or meaning-centered approaches. Their experiential knowledge is discounted against faculty expertise.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, excluded,
    moderate, biographical, constrained, local).

% They synthesize experimental and neuroscientific evidence showing that explicit phonics produces superior outcomes, especially for at-risk readers. Their findings are systematically marginalized in teacher-preparation programs committed to whole-language frameworks.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_reading_researchers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes reading instruction around meaning-making and authentic literature, solving the problem of disengaged learners by prioritizing comprehension motivation and textual richness over isolated skill drills.
% TRANSFER_FUNCTION: Transfers instructional time and cognitive attention from explicit decoding practice to meaning-oriented activities; transfers professional authority from empirical reading science to literacy faculty and classroom facilitators; transfers the cost of reading failure onto struggling students and their families.
% ABSENT_VOICES: Parents of struggling readers, cognitive scientists specializing in reading acquisition, and special education advocates are largely excluded from curriculum design and teacher training; their evidence-based objections are treated as disciplinary overreach or ideological opposition.
% DISAPPEARANCE_RATIONALE: If the meaning-primacy constraint vanished, classroom practice would shift toward explicit systematic phonics and decoding instruction; teacher training curricula would reorganize around the science of reading; struggling readers would gain access to interventions previously withheld; the professional authority of whole-language faculty would decline.
% FOUNDING_PROBLEM: Traditional phonics-drill instruction produced disengaged readers who saw reading as meaningless decoding exercises; the approach aimed to restore purpose and joy by grounding literacy in authentic communication.
% FOUNDING_PROBLEM_CORROBORATION: Whole language faculty attest the founding problem is live, citing anecdotal teacher reports. Cognitive reading researchers and special education practitioners outside the benefiting parties attest that the problem was historically specific to mid-20th-century drill methods and that modern explicit instruction integrates meaning; they further attest that the constraint now causes the larger problem of reading failure.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   The constraint is a tangled rope because it coordinates genuine meaning-making and literature-rich experiences for typically developing readers while extracting educational opportunity from struggling readers and students with dyslexia through the same structure. Base extractiveness (0.65) is substantial because the constraint withholds evidence-based interventions from learners who need them most. Suppression (0.76) is high because phonics alternatives are structurally excluded from teacher training and curriculum adoption. Theater ratio (0.74) has risen over the interval: running records, leveled libraries, and guided-reading rituals perform instructional diligence while the core empirical claim (natural emergence) is increasingly contested. The measurement series share a single time grid so every metric is sampled at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The whole-language faculty and typically developing readers experience the constraint as a rope: rich literature, joyful literacy, and coherent professional community. The struggling reader and dyslexic student experience it as a snare: they are subjected to the same structure but receive withheld decoding instruction, with exit blocked by compulsory schooling and the unavailability of alternative classroom placements. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole-language faculty are low-d beneficiaries and agenda-setters who collect professional authority and generational influence from the constraint. Typically developing readers and classroom teachers are low-to-moderate-d beneficiaries: they receive the coordination good (literature exposure, professional identity) even though they are powerless or constrained. Struggling readers and students with dyslexia are high-d targets: the constraint extracts from them by substituting meaning-based facilitation for the explicit instruction their cognitive profiles require. Cognitive reading researchers sit at analytical exit with near-zero d because they observe without being bound by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the mid-20th-century problem of disengaging phonics drills. That problem is either dead or contested: modern explicit instruction integrates meaning, and the empirical evidence for natural emergence is weak. The constraint persists because it has fused with teacher professional identity and teacher-preparation institutional structures, not because its founding coordination function remains live. This prevents mislabeling the current arrangement as a genuine rope: the coordination is partial (it works for some) and historically justified, while the extraction is ongoing and structurally embedded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_emergence_validity,
    'Does decoding actually emerge as a natural byproduct of meaning-focused immersion for most learners, or does it require explicit systematic instruction?',
    'Large-scale randomized controlled trials and longitudinal neuroimaging studies comparing meaning-only immersion against explicit phonics, with decoding outcomes measured by standardized instruments rather than running-record inference.',
    'If decoding does not emerge naturally, the constraint''s coordination function is limited to already-proficient learners and its extractive component becomes the dominant structural fact, supporting reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_emergence_validity, empirical, 'Whether decoding is a natural emergent property or requires explicit teaching.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of phonics alternatives structural (state textbook adoption, district mandates, licensure requirements) or internalized (teacher professional identity, shame about scripted instruction, belief that explicit phonics is developmentally inappropriate)?',
    'Post-mandate suppression trajectory: measure phonics adoption rates in jurisdictions that have removed whole-language mandates; if suppression persists, the mechanism is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because teachers carry the suppressive belief with them after policy reversal, prolonging harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    founding_problem_anachronism,
    'Is the disengagement problem that whole language was built to solve still live in contemporary classrooms, or has it been superseded by modern explicit instruction methods that integrate meaning?',
    'Comparative classroom ethnography and student-engagement measurement in explicit-instruction classrooms that use decodable texts versus meaning-immersion classrooms.',
    'If the founding problem is dead, the constraint persists without the coordination justification that established it, strengthening mandatrophy and piton readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_anachronism, conceptual, 'Whether the founding disengagement problem is still live or historically superseded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.22).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 5, 0.3).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 10, 0.42).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 15, 0.52).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 20, 0.6).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 25, 0.66).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 30, 0.7).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 35, 0.74).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 35, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 35, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one member of the reading_acquisition_legitimacy constraint family. The natural-language label 'reading instruction' conflates multiple structurally distinct claims about how reading is acquired and what makes instruction legitimate. Each reading has a different epsilon, different stakeholder structure, and different empirical status. This reading's high epsilon reflects its contested empirical foundation and identifiable victim population; sibling readings vary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
