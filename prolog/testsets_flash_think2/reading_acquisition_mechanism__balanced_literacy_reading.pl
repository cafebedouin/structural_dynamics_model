% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Approach to Reading Acquisition
 *   domain: Educational Psychology/Literacy Pedagogy/Cognitive Science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   acquisition, which attempts to integrate explicit phonics instruction
 *   with authentic literature exposure. It emerged as a compromise in the
 *   'reading wars' between phonics and whole language advocates. While
 *   intended as a coordination mechanism, its variable implementation
 *   fidelity often leads to insufficient systematic phonics, resulting in
 *   poor outcomes for many struggling readers. This story instantiates the
 *   'balanced_literacy_reading' of the 'reading_acquisition_mechanism'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.55).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Approach to Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "Educational Psychology/Literacy Pedagogy/Cognitive Science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '4203088b-aa5f-4a7c-ae01-859836d3fe29').
narrative_ontology:cs_kernel_codification('4203088b-aa5f-4a7c-ae01-859836d3fe29', formalized).
narrative_ontology:cs_authority_grounding('4203088b-aa5f-4a7c-ae01-859836d3fe29', practice).
narrative_ontology:cs_interpretation_layer_present('4203088b-aa5f-4a7c-ae01-859836d3fe29').
narrative_ontology:cs_reading_relation('4203088b-aa5f-4a7c-ae01-859836d3fe29', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('4203088b-aa5f-4a7c-ae01-859836d3fe29', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('4203088b-aa5f-4a7c-ae01-859836d3fe29', foundational, reading_is_a_complex_skill).
narrative_ontology:cs_axiom_status(reading_is_a_complex_skill, holdable).
narrative_ontology:cs_axiom_grounding('4203088b-aa5f-4a7c-ae01-859836d3fe29', reading_is_a_complex_skill, empirically_contingent).
narrative_ontology:cs_axiom('4203088b-aa5f-4a7c-ae01-859836d3fe29', foundational, integrated_instruction_is_optimal).
narrative_ontology:cs_axiom_status(integrated_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('4203088b-aa5f-4a7c-ae01-859836d3fe29', integrated_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('4203088b-aa5f-4a7c-ae01-859836d3fe29', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('4203088b-aa5f-4a7c-ae01-859836d3fe29', contemporary_science_of_reading_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4203088b-aa5f-4a7c-ae01-859836d3fe29', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, some_educators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for curriculum adoption and teacher training. Balanced literacy offers a politically palatable compromise in the 'reading wars', allowing them to claim a comprehensive approach while avoiding direct conflict.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell integrated curriculum materials that combine elements of phonics and literature, catering to the balanced literacy market. They benefit from the broad institutional adoption of this approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers, beneficiary,
    organized, biographical, mobile, global).

% Prefer the flexibility and less rigid structure of balanced literacy compared to highly prescriptive phonics programs. They find it aligns with their pedagogical philosophy of fostering a love of reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, some_educators, beneficiary,
    moderate, biographical, constrained, local).

% Often fail to acquire foundational decoding skills due to insufficient or unsystematic phonics instruction within balanced literacy frameworks, leading to long-term literacy deficits.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and financial costs of their children's poor literacy outcomes, often seeking private tutoring or advocating for changes in school curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% Advocate for explicit, systematic phonics instruction as foundational for all readers. They are often critical of balanced literacy's implementation and its outcomes, but their views are frequently marginalized in institutional curriculum decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% See balanced literacy as a way to preserve elements of whole language philosophy (e.g., authentic text exposure, meaning-making) within a broader framework, preventing a complete shift to pure phonics.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates, beneficiary,
    organized, generational, constrained, national).

% Conduct research on reading acquisition mechanisms and pedagogical effectiveness. They often provide empirical evidence that challenges the efficacy of balanced literacy's implementation, particularly regarding phonics instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to reconcile conflicting pedagogical approaches (explicit phonics vs. implicit whole language) into a single, integrated curriculum framework for schools, aiming to provide a comprehensive approach to reading instruction.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design from pure phonics or whole language camps to an integrated model. It often results in a transfer of effective literacy outcomes from some students (especially those needing explicit phonics) to others, or to no one, due to implementation variability.
% ABSENT_VOICES: Struggling readers (who cannot articulate their needs), parents of struggling readers (who often lack pedagogical expertise to challenge effectively), and pure phonics advocates (who are frequently marginalized in institutional curriculum debates) are often excluded from the decision-making processes that perpetuate balanced literacy's flaws.
% DISAPPEARANCE_RATIONALE: If balanced literacy and its institutional enforcement vanished overnight, schools would be forced to adopt either a pure phonics or pure whole language approach, or develop a new, genuinely integrated model. This would fundamentally alter curriculum, teacher training, and student outcomes, reorganizing the entire landscape of reading pedagogy.
% FOUNDING_PROBLEM: The 'reading wars' between phonics and whole language approaches created an intractable pedagogical conflict in education, leading to a search for a compromise that could satisfy both camps and improve overall literacy rates.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (some educators, publishers) argue the problem of reconciling diverse pedagogical needs is still live. Critics (phonics advocates, cognitive scientists) attest that the original problem of poor literacy persists due to balanced literacy's implementation flaws, or that the 'war' has shifted to implementation fidelity, making the founding problem effectively 'dead' in its original form but 'live' in its consequences. Legislative hearing testimony and independent educational research from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate disparate pedagogical approaches but often results in asymmetric extraction. Extractiveness is moderate-high (0.62) due to the opportunity cost for struggling readers who do not receive adequate foundational skills, and the continued institutional investment in a system with suboptimal outcomes. Suppression (0.55) reflects the institutional inertia and active defense of balanced literacy against calls for more systematic phonics, marginalizing alternative pedagogies. Theater ratio (0.45) is significant because many implementations pay lip service to 'phonics' without delivering systematic, explicit instruction, creating a performative rather than functional adherence to the phonics component. Resistance is high (0.70) from 'science of reading' advocates and parents of struggling readers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of school administrators and some educators, balanced literacy is a reasonable, comprehensive approach. From the perspective of struggling readers and phonics advocates, it is an extractive system that fails to deliver essential skills, sustained by institutional inertia and political compromise. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   School administrators and educational publishers benefit from the political compromise and market for integrated materials, respectively. Some educators benefit from the pedagogical flexibility. Struggling readers and their parents bear the primary costs of inadequate instruction. Phonics advocates are structurally excluded from influencing curriculum, while whole language advocates benefit from the preservation of their core tenets within the compromise. Cognitive scientists act as analytical observers, providing evidence that often challenges the constraint's efficacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling balanced literacy as a pure Rope (genuine coordination) or a pure Snare (pure extraction). It acknowledges the initial coordination intent (reconciling pedagogical camps) while highlighting the significant, often unacknowledged, extraction from students who fail to learn to read effectively due to its implementation flaws. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating that the original problem may have shifted or been inadequately addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_ambiguity,
    'Is the observed inefficacy of balanced literacy due to flaws in its theoretical framework, or primarily due to variable and often poor implementation fidelity in schools?',
    'Rigorous, controlled studies comparing high-fidelity balanced literacy implementation with systematic phonics programs, or large-scale audits of classroom practice against balanced literacy guidelines.',
    'If implementation is the primary issue, the theoretical framework might be salvageable with better training and oversight. If the theory itself is flawed, a fundamental pedagogical shift would be required, potentially reclassifying the constraint as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_ambiguity, empirical, 'Distinguishing theoretical flaws from implementation failures in balanced literacy.').

omega_variable(
    phonics_sufficiency_ambiguity,
    'Is the phonics component within balanced literacy frameworks truly systematic and explicit enough to meet the needs of all learners, particularly those at risk for reading difficulties?',
    'Curriculum analysis by independent experts against established criteria for systematic phonics, and longitudinal studies tracking reading outcomes for diverse student populations under balanced literacy.',
    'If phonics instruction is found to be insufficient, the ''coordination'' aspect of balanced literacy is undermined, strengthening its classification as a Tangled Rope or even Snare due to its failure to deliver foundational skills.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonics_sufficiency_ambiguity, empirical, 'Assessing the adequacy of phonics instruction within balanced literacy.').

omega_variable(
    reading_acquisition_kernel_framing,
    'Is reading acquisition primarily a cognitive skill (decoding grapheme-phoneme correspondence) or a meaning-making process (comprehending authentic texts), or an inseparable integration of both?',
    'Continued neuroscientific and cognitive psychological research into the mechanisms of reading, and philosophical analysis of the nature of literacy itself.',
    'A strong resolution towards one primary framing would challenge the foundational axioms of balanced literacy, potentially leading to its reclassification or obsolescence if its integrated approach is deemed fundamentally misaligned with how reading works.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_acquisition_kernel_framing, conceptual, 'The fundamental nature of reading acquisition as a cognitive or meaning-making process.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative reading pedagogies (e.g., pure systematic phonics) structural (institutional adoption, curriculum mandates) or internalized (teacher training, professional identity that resists change)?',
    'Surveys and interviews with educators regarding their pedagogical choices and perceived constraints, alongside analysis of institutional policies and curriculum adoption processes.',
    'If suppression is largely internalized, the effective suppression is higher than structural measures suggest, as educators carry the resistance to change with them. If primarily structural, policy changes could more readily open alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1980, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(read_tr_t1987, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement(read_tr_t1994, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(read_tr_t2001, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2001, 0.4).
narrative_ontology:measurement(read_tr_t2008, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2008, 0.43).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(read_tr_t2023, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(read_be_t1987, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement(read_be_t1994, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1994, 0.55).
narrative_ontology:measurement(read_be_t2001, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2001, 0.59).
narrative_ontology:measurement(read_be_t2008, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(read_be_t2023, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2023, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(read_su_t1987, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1987, 0.42).
narrative_ontology:measurement(read_su_t1994, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1994, 0.48).
narrative_ontology:measurement(read_su_t2001, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(read_su_t2008, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(read_su_t2023, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('balanced_literacy_reading') of the 'reading_acquisition_mechanism' kernel, which also includes 'phonics_reading' and 'whole_language_reading'. These three constraints represent different pedagogical approaches to the same underlying phenomenon, with distinct structural properties and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
