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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Approach to Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'Balanced Literacy' approach to reading
 *   acquisition, which posits that both explicit phonics instruction and
 *   authentic literature exposure are necessary and should be integrated. It
 *   emerged as a compromise in the 'reading wars' but often suffers from
 *   variable implementation fidelity, frequently collapsing to insufficient
 *   systematic phonics in practice. This constraint is one reading of the
 *   broader 'reading_acquisition_mechanism' kernel, alongside
 *   'phonics_reading' and 'whole_language_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Approach to Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '6a4eeb25-523b-4878-86c1-312e363b196a').
narrative_ontology:cs_kernel_codification('6a4eeb25-523b-4878-86c1-312e363b196a', formalized).
narrative_ontology:cs_authority_grounding('6a4eeb25-523b-4878-86c1-312e363b196a', lineage).
narrative_ontology:cs_interpretation_layer_present('6a4eeb25-523b-4878-86c1-312e363b196a').
narrative_ontology:cs_reading_relation('6a4eeb25-523b-4878-86c1-312e363b196a', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a4eeb25-523b-4878-86c1-312e363b196a', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('6a4eeb25-523b-4878-86c1-312e363b196a', foundational, reading_is_a_complex_skill_requiring_multiple_pathways).
narrative_ontology:cs_axiom_status(reading_is_a_complex_skill_requiring_multiple_pathways, holdable).
narrative_ontology:cs_axiom_grounding('6a4eeb25-523b-4878-86c1-312e363b196a', reading_is_a_complex_skill_requiring_multiple_pathways, conventional).
narrative_ontology:cs_axiom('6a4eeb25-523b-4878-86c1-312e363b196a', foundational, explicit_phonics_and_authentic_text_exposure_are_both_essential).
narrative_ontology:cs_axiom_status(explicit_phonics_and_authentic_text_exposure_are_both_essential, holdable).
narrative_ontology:cs_axiom_grounding('6a4eeb25-523b-4878-86c1-312e363b196a', explicit_phonics_and_authentic_text_exposure_are_both_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('6a4eeb25-523b-4878-86c1-312e363b196a', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('6a4eeb25-523b-4878-86c1-312e363b196a', contemporary_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a4eeb25-523b-4878-86c1-312e363b196a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, early_career_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the demand for diverse instructional materials that support both phonics and literature components, allowing them to market comprehensive, often expensive, curricula packages. They adapt materials to policy shifts but prefer broad, inclusive approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers, beneficiary,
    institutional, generational, mobile, national).

% Benefit from a pedagogical approach that requires extensive training in multiple methods, justifying longer programs and specialized certifications. They often advocate for this approach as a 'best practice' that integrates various theories.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions, beneficiary,
    institutional, generational, constrained, national).

% Implement and enforce balanced literacy policies in schools, often as a compromise between competing pedagogical factions. They face pressure from parents, teachers, and district mandates, and prefer approaches that appear comprehensive and politically palatable.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators, agenda_setter,
    organized, biographical, constrained, local).

% Bear the cost of inconsistent or insufficient systematic phonics instruction, often failing to develop foundational decoding skills. Their academic progress is directly impacted by the fidelity of implementation, which frequently falls short of the 'balanced' ideal.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Struggle to implement the complex, multi-faceted approach effectively, often receiving inadequate training in systematic phonics. They face pressure to cover all components, leading to superficial instruction and burnout.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, early_career_teachers, payer,
    moderate, immediate, constrained, local).

% Analyze the efficacy of balanced literacy, often identifying gaps between theory and practice, particularly regarding the systematicity of phonics instruction. They provide evidence that can challenge or support the approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_researchers, observer,
    analytical, generational, analytical, global).

% Advocate for a stronger, more systematic phonics component, arguing that balanced literacy often dilutes explicit instruction. They are often marginalized in policy discussions dominated by compromise positions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse pedagogical theories (phonics and whole language) into a single, comprehensive approach for teaching reading, providing a framework for curriculum development and teacher training.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design influence to institutions and publishers that can integrate multiple methods, while transferring the burden of complex, often inconsistent, implementation to teachers and the cost of inadequate foundational skills to struggling students.
% ABSENT_VOICES: Strong advocates for systematic phonics are often excluded from the core policy-making bodies that define 'balanced' approaches, as their insistence on explicit, sequential instruction is seen as undermining the 'whole language' component. Their absence allows for a less rigorous phonics component to persist.
% DISAPPEARANCE_RATIONALE: If the balanced literacy framework vanished, schools would immediately face a vacuum in reading instruction policy. Districts would likely revert to either pure phonics or whole language approaches, or scramble to develop new integrated models, leading to significant disruption in curriculum, teacher training, and instructional materials.
% FOUNDING_PROBLEM: The 'reading wars' of the late 20th century created a polarized debate between phonics and whole language, leading to a need for a compromise approach that could unify educators and policymakers.
% FOUNDING_PROBLEM_CORROBORATION: Educational policymakers and many teacher organizations attest that the 'reading wars' problem is still live, requiring a unifying framework. However, many literacy researchers and parent advocacy groups (outside the direct beneficiaries) argue that the 'balance' has often been insufficient in phonics, leading to a new set of problems for struggling readers, suggesting the original problem is only partially addressed or has mutated.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).
:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by struggling readers and early-career teachers due to inconsistent implementation, but not as high as pure extraction. Suppression (0.6) is significant because the institutional consensus around 'balanced literacy' actively discourages and marginalizes purely phonics-based or purely whole-language approaches, limiting alternatives for educators. The theater ratio (0.55) is high, indicating that the 'balance' is often more performative than functional, with explicit phonics often being superficial or unsystematic in practice, while the rhetoric of 'balance' is maintained.
 *
 * PERSPECTIVAL GAP:
 *   School administrators and teacher training institutions perceive this as a functional, comprehensive approach that resolves pedagogical conflicts. Struggling readers and early-career teachers experience it as a source of confusion and inadequate skill development, leading to academic and professional difficulties. The engine will compute these divergent experiences from the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational publishers and teacher training institutions are beneficiaries (d near 0.0) as the approach creates demand for their integrated curricula and training programs. School administrators are agenda-setters (d near 0.3) as they implement and defend the policy, balancing various pressures. Struggling readers and early-career teachers are payers (d near 1.0) as they bear the direct costs of the approach's implementation failures. Phonics advocates are excluded (d near 0.8) as their alternative is suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (resolving the 'reading wars' via compromise) is still 'live' in the sense that the debate persists, but its effectiveness is 'contested'. The high theater ratio and accumulating extractiveness suggest a drift towards a 'false summit' where the 'balance' is more about institutional peace and market for diverse materials than optimal reading outcomes. If the founding problem were truly 'dead' (i.e., the reading wars fully resolved and all students learning to read effectively), the constraint would be a clear Snare or Piton, as its persistence would be purely extractive or inertial. The 'contested' status prevents a full mandatrophy resolution, but the metrics indicate a strong drift in that direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_systematicity_fidelity,
    'What is the actual fidelity and systematicity of phonics instruction within ''balanced literacy'' classrooms, compared to the stated curriculum?',
    'Direct classroom observation, curriculum analysis, and teacher surveys focusing on the explicit, sequential nature of phonics delivery, rather than mere presence of phonics activities.',
    'If fidelity is low, the constraint''s effective extractiveness on struggling readers is higher, and its theater ratio is higher, pushing it closer to a Snare or Piton. If fidelity is high, it functions more as a genuine Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonics_systematicity_fidelity, empirical, 'Measures the gap between claimed and actual phonics instruction within balanced literacy.').

omega_variable(
    reading_acquisition_mechanism_kernel_reading,
    'Is this constraint a genuine ''balanced'' integration of phonics and whole language, or an institutional compromise that primarily serves to maintain a market for diverse materials and pedagogical training, while under-serving foundational skills?',
    'Longitudinal studies comparing reading outcomes (especially for at-risk populations) under high-fidelity balanced literacy vs. systematic phonics approaches, alongside analysis of institutional incentives for maintaining the ''balanced'' framing.',
    'If primarily an institutional compromise, the constraint''s classification shifts from Tangled Rope to a more extractive Snare, as its coordination function is revealed as cover for rent-seeking. This would also imply a higher effective suppression of alternative pedagogical approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_acquisition_mechanism_kernel_reading, conceptual, 'Ambiguity between genuine pedagogical integration and institutional compromise for the ''reading_acquisition_mechanism'' kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative pedagogical approaches structural (e.g., policy mandates, curriculum requirements) or internalized (e.g., teachers'' belief in the ''balanced'' ideal despite evidence)?',
    'Post-policy-change analysis: if teachers continue to resist systematic phonics even after mandates for balanced literacy are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the ''balanced'' ideal persists even without external enforcement. This would make exit options for teachers more ''identity_locked''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t1998, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(read_tr_t2006, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement(read_tr_t2014, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2014, 0.53).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t1998, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(read_be_t2006, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(read_be_t2014, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(read_su_t1998, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(read_su_t2006, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2006, 0.55).
narrative_ontology:measurement(read_su_t2014, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel, which also includes 'phonics_reading' and 'whole_language_reading'. Each represents a distinct pedagogical approach with different structural properties and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
