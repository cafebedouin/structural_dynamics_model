% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of reading
 *   acquisition, asserting that explicit, systematic instruction in
 *   grapheme-phoneme correspondence is a foundational and necessary skill. It
 *   is one reading of the broader 'reading_acquisition_mechanism' kernel,
 *   which is contested by 'whole_language_reading' and
 *   'balanced_literacy_reading'. This reading emphasizes a structured,
 *   sequential approach, often mandated through curriculum and teacher
 *   training, with significant implications for teacher autonomy and the
 *   market for educational materials.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.75).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '4d1560aa-00c3-4a6d-bba0-017e6d0701e3').
narrative_ontology:cs_kernel_codification('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', formalized).
narrative_ontology:cs_authority_grounding('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', expertise).
narrative_ontology:cs_interpretation_layer_present('4d1560aa-00c3-4a6d-bba0-017e6d0701e3').
narrative_ontology:cs_reading_relation('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', foundational, grapheme_phoneme_correspondence_is_primary).
narrative_ontology:cs_axiom_status(grapheme_phoneme_correspondence_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', grapheme_phoneme_correspondence_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', foundational, explicit_systematic_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', explicit_systematic_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', scientific_consensus_on_decoding).
narrative_ontology:cs_drift_state('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', contemporary_pedagogical_wars, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4d1560aa-00c3-4a6d-bba0-017e6d0701e3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_designers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, phonics_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers, policymakers, and parent groups who champion explicit, systematic phonics instruction as the most effective and scientifically-backed method for teaching reading. They push for curriculum mandates and teacher training aligned with this approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_advocates, agenda_setter,
    institutional, generational, mobile, national).

% Students who, without explicit phonics instruction, often fail to acquire foundational decoding skills. This method is argued to disproportionately benefit them by providing a clear, structured pathway to reading proficiency.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).

% Are required to implement specific phonics curricula, often with reduced autonomy over their pedagogical methods. They bear the cost of retraining and adapting to new instructional mandates, sometimes conflicting with their prior training or beliefs about holistic literacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers, payer,
    moderate, biographical, constrained, local).

% Develop and market phonics-based reading programs and materials. Their work is validated and adopted when this pedagogical approach gains traction, leading to increased demand for their products and services.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_designers, beneficiary,
    organized, generational, mobile, national).

% Profit from the sale of phonics textbooks, workbooks, and digital resources. Their market share expands significantly with the widespread adoption of phonics-first policies and curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Proponents of reading instruction that emphasizes meaning-making through authentic texts, with decoding skills emerging implicitly. Their methods are actively de-emphasized or suppressed by phonics-first mandates, leading to a loss of influence and resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Study the cognitive processes of reading and the efficacy of different instructional methods. Their research often provides the empirical grounding for phonics-first approaches, though interpretations can vary.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, phonics_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, explicit method for teaching foundational reading skills, ensuring that all students, particularly those at risk, receive direct instruction in decoding, which is argued to be essential for literacy.
% TRANSFER_FUNCTION: Transfers instructional time and resources from more holistic or implicit reading approaches to explicit, systematic phonics instruction. It also transfers pedagogical authority from individual teachers to curriculum mandates and 'science of reading' frameworks.
% ABSENT_VOICES: Advocates for whole language and less structured balanced literacy approaches are often marginalized or excluded from policy discussions, despite representing significant pedagogical traditions and teacher experience. They would argue for broader pedagogical freedom and a more integrated approach to literacy.
% DISAPPEARANCE_RATIONALE: If the mandate for explicit systematic phonics vanished overnight, educational systems would likely revert to more varied or 'balanced' approaches, teacher training would shift, and the market for phonics-specific curricula would contract. Reading outcomes, particularly for struggling readers, would likely become more variable, reorganizing the landscape of literacy education.
% FOUNDING_PROBLEM: A perceived crisis in reading proficiency, particularly among struggling learners, and a lack of consistent, evidence-based instructional practices across schools.
% FOUNDING_PROBLEM_CORROBORATION: Phonics advocates and many policymakers attest that the problem of reading proficiency remains live, citing ongoing literacy statistics and the need for consistent, effective instruction. While some educators and researchers from other camps contest the severity or the singular solution, the general concern for reading outcomes is widely acknowledged by independent educational bodies and public discourse.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.68) due to the imposition of a specific pedagogical method, which can limit teacher discretion and divert resources from other literacy activities. `Suppression` is also high (0.75) as this approach actively marginalizes or excludes alternative methods through policy and curriculum mandates. `Theater_ratio` is moderate (0.25) because while the instruction is genuinely functional, some aspects of rigid 'systematicity' can become performative if not deeply understood or adapted by teachers. `Resistance` is high (0.7) due to ongoing 'reading wars' and pushback from educators favoring more holistic methods. The `accessibility_collapse` is moderate (0.5) as it collapses alternatives for *how* to teach reading, even while aiming to make reading itself accessible.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of phonics advocates, this constraint is a 'rope' or 'scaffold' – a necessary, evidence-based coordination mechanism to ensure literacy for all. From the perspective of teachers or whole language advocates, it operates more like a 'snare' or 'tangled_rope', extracting autonomy and suppressing alternative, potentially valuable, pedagogical approaches. The engine's computation of 'tangled_rope' reflects this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics advocates, curriculum designers, and phonics publishers are clear beneficiaries, gaining influence, market share, and validation. Struggling readers are also beneficiaries, as the method is designed to address their specific needs. Teachers are payers, as they lose pedagogical autonomy and must adapt to mandated curricula. Whole language advocates are victims, as their methods are suppressed and de-emphasized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_efficacy_vs_teacher_autonomy,
    'Does the measured efficacy of explicit phonics instruction justify the reduction in teacher autonomy and the suppression of alternative pedagogical approaches?',
    'Longitudinal studies comparing student outcomes (reading proficiency, reading enjoyment) in contexts with high teacher autonomy vs. mandated phonics curricula, controlling for socioeconomic factors.',
    'If high autonomy yields comparable or better outcomes, the extraction from teachers is less justified, potentially reclassifying the constraint as more extractive. If mandated phonics consistently outperforms, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_efficacy_vs_teacher_autonomy, preference, 'Balancing instructional effectiveness with teacher professional judgment.').

omega_variable(
    cost_benefit_of_systematic_instruction,
    'Is the high initial instructional cost (systematic scope-and-sequence, teacher training) offset by lower long-term remediation costs for struggling readers?',
    'Economic analysis comparing the investment in early, systematic phonics programs against the costs of later remedial interventions for students who did not receive such instruction.',
    'If long-term remediation costs are significantly reduced, the overall societal benefit of the coordination function is higher, potentially dampening the effective extractiveness. If costs are not significantly reduced, the initial extraction is less justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_benefit_of_systematic_instruction, empirical, 'Evaluating the long-term economic efficiency of phonics-first instruction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative methods structural (curriculum mandates, policy) or internalized (teachers'' belief in phonics as the ''only'' way)?',
    'Post-mandate-removal pedagogical trajectory: if alternative methods persist after mandates are lifted, suppression was primarily structural. If teachers continue to adhere to phonics-first, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the pedagogical mindset persists after external barriers are removed, making exit harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in pedagogical adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
