% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the literacy
 *   acquisition kernel, asserting that explicit, systematic instruction in
 *   phoneme-grapheme correspondence is a prerequisite for connected text
 *   exposure and comprehension. It is a dominant pedagogical approach, often
 *   mandated by educational authorities based on 'science of reading'
 *   research. While it aims to coordinate effective reading instruction for
 *   students, it extracts significantly from teacher autonomy by prescribing
 *   curricula and methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '086508b9-d378-42ae-8ad9-a9efb4e80d16').
narrative_ontology:cs_kernel_codification('086508b9-d378-42ae-8ad9-a9efb4e80d16', formalized).
narrative_ontology:cs_authority_grounding('086508b9-d378-42ae-8ad9-a9efb4e80d16', expertise).
narrative_ontology:cs_interpretation_layer_present('086508b9-d378-42ae-8ad9-a9efb4e80d16').
narrative_ontology:cs_reading_relation('086508b9-d378-42ae-8ad9-a9efb4e80d16', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('086508b9-d378-42ae-8ad9-a9efb4e80d16', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('086508b9-d378-42ae-8ad9-a9efb4e80d16', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('086508b9-d378-42ae-8ad9-a9efb4e80d16', foundational, alphabetic_principle_is_foundational).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('086508b9-d378-42ae-8ad9-a9efb4e80d16', alphabetic_principle_is_foundational, empirically_contingent).
narrative_ontology:cs_axiom('086508b9-d378-42ae-8ad9-a9efb4e80d16', foundational, explicit_systematic_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('086508b9-d378-42ae-8ad9-a9efb4e80d16', explicit_systematic_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('086508b9-d378-42ae-8ad9-a9efb4e80d16', science_of_reading_consensus).
narrative_ontology:cs_drift_state('086508b9-d378-42ae-8ad9-a9efb4e80d16', contemporary_literacy_wars, gap(stable, minor, true)).
narrative_ontology:cs_created_at('086508b9-d378-42ae-8ad9-a9efb4e80d16', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, educational_publishers_of_phonics_curricula).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, cognitive_scientists_supporting_phonics).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_who_prefer_whole_text_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the adoption of phonics-first curricula, often in response to 'science of reading' advocacy and concerns about literacy rates. They enforce curriculum fidelity through training and assessment, limiting local pedagogical choice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Are required to implement prescribed phonics curricula, often with scripted lessons, reducing their autonomy to adapt instruction to individual student needs or integrate other pedagogical approaches they deem valuable. Resistance can lead to professional sanctions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, constrained, local).

% Benefit from the explicit, systematic instruction in decoding, which provides foundational skills necessary for reading, especially if they struggle with phonological processing or come from print-poor environments. This approach directly addresses their learning needs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, immediate, trapped, local).

% May find the systematic, decontextualized nature of phonics instruction less engaging or motivating than early exposure to meaningful, connected texts. Their preference for holistic engagement is de-prioritized.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_who_prefer_whole_text_engagement, payer,
    powerless, immediate, trapped, local).

% Profit significantly from the widespread adoption of phonics-first mandates, as schools and districts are compelled to purchase their materials and training programs. They actively lobby for these mandates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_publishers_of_phonics_curricula, beneficiary,
    organized, biographical, arbitrage, national).

% Are largely excluded from mainstream policy discussions and curriculum development, as their emphasis on natural language acquisition through immersion in connected text is seen as antithetical to the phonics-first approach. Their influence on policy is minimal.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, advocates_of_whole_language_pedagogy, excluded,
    organized, generational, constrained, national).

% See their research on the cognitive science of reading, particularly the importance of phonological awareness and decoding, directly translated into educational policy and practice. Their scientific findings are vindicated and applied.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, cognitive_scientists_supporting_phonics, beneficiary,
    analytical, generational, analytical, universal).

% Observe the implementation of phonics-first mandates, often raising concerns about oversimplification of reading science, potential negative impacts on reading comprehension or motivation, or the exclusion of other important literacy components.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, cognitive_scientists_critiquing_phonics, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a standardized, evidence-based method for teaching foundational decoding skills, ensuring all students, especially those at risk, acquire the ability to read words.
% TRANSFER_FUNCTION: Transfers instructional control and pedagogical decision-making from individual teachers to prescribed, systematic curricula. It transfers explicit decoding skills to students, aiming to reduce reading failure rates.
% ABSENT_VOICES: Teachers who advocate for greater professional autonomy and pedagogical flexibility, as well as proponents of whole language or balanced literacy who believe in a more holistic approach to reading acquisition, are often marginalized in policy debates.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished overnight, educational policy and pedagogical practices would immediately diversify. Schools would likely revert to more varied approaches, potentially reintroducing elements of whole language or balanced literacy, and teachers would regain significant autonomy in curriculum design.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among students from disadvantaged backgrounds or those with specific learning difficulties like dyslexia, which were attributed to insufficient or unsystematic decoding instruction.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing literacy statistics, advocacy groups for students with dyslexia, and a broad consensus within cognitive science research (from outside the direct beneficiaries like publishers) continue to attest to the importance of decoding skills and the problem of reading failure.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is primarily borne by teachers, who lose professional judgment and flexibility, and by some students who may find the approach unengaging. The suppression (0.75) is high due to the active enforcement of curriculum fidelity and the marginalization of alternative pedagogies. Theater ratio (0.10) is low because the instruction is genuinely systematic and intended to be effective, not merely performative. Accessibility collapse (0.50) is moderate, as alternative methods are de-emphasized but not entirely eliminated from discourse. Resistance (0.60) comes from teachers and advocates of other methods. The claimed type is Tangled Rope because it genuinely coordinates a solution to reading failure (benefiting many students) while simultaneously extracting from teachers and suppressing alternative approaches.
 *
 * PERSPECTIVAL GAP:
 *   Teachers experience this constraint as a top-down mandate that limits their professional judgment and forces adherence to specific methods, leading to high perceived extraction. Students with phonological challenges, however, may experience it as a beneficial coordination mechanism that provides them with essential decoding skills, perceiving low or even negative extraction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational authorities and phonics curriculum publishers are clear beneficiaries, gaining influence and revenue, respectively. Students with phonological weaknesses are also beneficiaries, as the constraint directly addresses their learning needs. Teachers' professional judgment is a primary target, as their autonomy is curtailed. Students who prefer holistic text engagement are also targets, as their learning preferences are not prioritized. Cognitive scientists supporting phonics are vindicated, while those critiquing it act as observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_efficacy_vs_autonomy,
    'Is the measured suppression of teacher autonomy justified by demonstrably superior student literacy outcomes across all student populations, or does it represent an overreach that stifles pedagogical innovation?',
    'Longitudinal studies comparing student outcomes (decoding, comprehension, motivation) in contexts with strict phonics mandates versus contexts allowing greater teacher autonomy and diverse pedagogical approaches.',
    'If superior outcomes are not universal, the extraction from teacher autonomy is less justified, potentially reclassifying the constraint as more Snare-like for teachers. If outcomes are universally superior, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_efficacy_vs_autonomy, empirical, 'Whether the benefits of phonics mandates outweigh the costs to teacher autonomy.').

omega_variable(
    reading_science_interpretation_ambiguity,
    'Is the ''science of reading'' consensus, which underpins this phonics-first mandate, a complete and settled account of reading acquisition, or is it an interpretation that over-emphasizes decoding at the expense of other critical components like vocabulary, background knowledge, and motivation?',
    'Ongoing meta-analyses and theoretical advancements in cognitive science and educational psychology that integrate findings across all components of reading, not just decoding.',
    'If the consensus is found to be an oversimplification, the constraint''s claim to scientific grounding weakens, potentially shifting its authority grounding and increasing perceived extraction for students whose needs are not fully met.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation and scope of the ''science of reading'' consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel'. Its structural properties and metrics are distinct from other readings (whole_language, balanced_literacy, structured_literacy), which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
