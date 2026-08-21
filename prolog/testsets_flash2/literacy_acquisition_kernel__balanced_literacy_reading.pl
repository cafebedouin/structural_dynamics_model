% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   instruction, which claims to integrate systematic phonics with meaningful
 *   text engagement. It is one reading of the broader
 *   'literacy_acquisition_kernel' and is often presented as a compromise in
 *   the 'reading wars.' However, critics argue it often dilutes effective
 *   phonics instruction, leading to continued struggles for many learners.
 *   The claimed type is 'tangled_rope' because it purports to coordinate
 *   diverse pedagogical elements but often results in asymmetric extraction,
 *   benefiting educational institutions and publishers while potentially
 *   harming students and burdening teachers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Acquisition").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'eaffb861-f32b-4865-bcad-badd49856d59').
narrative_ontology:cs_kernel_codification('eaffb861-f32b-4865-bcad-badd49856d59', formalized).
narrative_ontology:cs_authority_grounding('eaffb861-f32b-4865-bcad-badd49856d59', lineage).
narrative_ontology:cs_interpretation_layer_present('eaffb861-f32b-4865-bcad-badd49856d59').
narrative_ontology:cs_reading_relation('eaffb861-f32b-4865-bcad-badd49856d59', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('eaffb861-f32b-4865-bcad-badd49856d59', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('eaffb861-f32b-4865-bcad-badd49856d59', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('eaffb861-f32b-4865-bcad-badd49856d59', foundational, reading_is_a_natural_process_with_explicit_components).
narrative_ontology:cs_axiom_status(reading_is_a_natural_process_with_explicit_components, holdable).
narrative_ontology:cs_axiom_grounding('eaffb861-f32b-4865-bcad-badd49856d59', reading_is_a_natural_process_with_explicit_components, conventional).
narrative_ontology:cs_axiom('eaffb861-f32b-4865-bcad-badd49856d59', foundational, instructional_balance_is_optimal).
narrative_ontology:cs_axiom_status(instructional_balance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('eaffb861-f32b-4865-bcad-badd49856d59', instructional_balance_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('eaffb861-f32b-4865-bcad-badd49856d59', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('eaffb861-f32b-4865-bcad-badd49856d59', contemporary_science_of_reading_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('eaffb861-f32b-4865-bcad-badd49856d59', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote balanced literacy as the preferred pedagogical approach, influencing teacher training and certification. They benefit from the continuous demand for professional development and curriculum materials associated with this approach, often resisting critiques that challenge its efficacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, constrained, national).

% Develop and market 'balanced literacy' curriculum packages, often incorporating elements from both phonics and whole language. They profit from the cyclical adoption of new materials and the perceived need for comprehensive, integrated programs, regardless of their scientific backing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Are mandated to implement balanced literacy approaches, often feeling pressure to reconcile conflicting instructional demands (e.g., explicit phonics vs. discovery-based reading). They bear the cost of inadequate training, lack of clear guidance, and the emotional toll of seeing students struggle under ineffective methods.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Are the primary targets of literacy instruction. If balanced literacy fails to provide sufficient explicit phonics, they may not develop foundational decoding skills, leading to persistent reading difficulties and academic setbacks. Their 'exit' is often falling behind or requiring expensive interventions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Conduct research on reading acquisition, often finding strong evidence for systematic phonics. They observe the pedagogical debates and the outcomes of different instructional approaches, providing evidence that frequently challenges the 'balanced' approach's claims of efficacy for all learners.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% Often lack the pedagogical expertise to challenge school curricula directly, but advocate for their children. They would demand more explicit, evidence-based instruction if fully informed of the scientific consensus on reading, but are often excluded from curriculum decision-making processes.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents_of_struggling_readers, excluded,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to synthesize the perceived strengths of phonics (decoding) and whole language (comprehension, motivation) into a single, comprehensive instructional framework, providing a common language and set of practices for educators.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum revenue to education schools and publishers, while transferring instructional burden and potential learning deficits to teachers and struggling readers, respectively.
% ABSENT_VOICES: Advocates for explicit, systematic phonics (e.g., structured literacy proponents, many cognitive scientists, parents of dyslexic children) are often marginalized or dismissed as 'extremists' in the balanced literacy discourse, despite strong empirical evidence supporting their positions.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, there would be an immediate vacuum in teacher training and curriculum. Schools would be forced to adopt either more explicit, systematic approaches (like structured literacy) or revert to less effective whole language methods, leading to significant pedagogical shifts and market disruption for publishers.
% FOUNDING_PROBLEM: The 'reading wars' between phonics and whole language created a polarized and ineffective instructional landscape, leading to calls for a compromise that could integrate both decoding and comprehension.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (education schools, some publishers) argue the problem of polarization is still live and balanced literacy offers the best path forward. Critics (cognitive scientists, structured literacy advocates) argue the 'compromise' often dilutes effective phonics instruction, and the original problem of reading failure persists, corroborated by stagnant literacy rates and scientific consensus on reading acquisition.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the ongoing costs to students who do not acquire strong reading skills and the financial burden on school systems for interventions, while education schools and publishers benefit from curriculum sales and professional development. Suppression (0.70) is high due to institutional inertia, the power of education schools in teacher training, and the marginalization of alternative, evidence-based approaches. The theater ratio (0.40) indicates that while some genuine coordination (e.g., integrating different aspects of literacy) occurs, a significant portion of the activity is performative, maintaining a 'balanced' image without fully addressing the core instructional needs of all students. The metrics show a gradual increase in extractiveness and suppression over time, suggesting a drift towards greater institutional capture and less genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of education schools and publishers, balanced literacy is a necessary and effective synthesis, a 'rope' coordinating complex pedagogical demands. From the perspective of struggling readers and many cognitive scientists, it functions as a 'snare' or 'tangled rope,' extracting learning potential and resources while suppressing more effective, evidence-based alternatives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and curriculum publishers are beneficiaries, gaining revenue and maintaining pedagogical influence. Classroom teachers and struggling readers are payers, bearing the costs of an often-ineffective or confusing instructional mandate. Cognitive scientists act as observers, providing critical, evidence-based analysis that often challenges the prevailing 'balanced' narrative. Parents of struggling readers are excluded, lacking a direct voice in curriculum decisions despite being deeply affected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_for_all_learners,
    'Does balanced literacy provide effective reading instruction for all learners, particularly those with foundational decoding difficulties?',
    'Large-scale, longitudinal randomized controlled trials comparing balanced literacy outcomes against structured literacy outcomes across diverse student populations, with a focus on early decoding and comprehension metrics.',
    'If found ineffective for a significant portion of learners, especially struggling readers, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function as lower, potentially reclassifying it closer to a Snare. If proven effective, its Rope-like qualities would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_for_all_learners, empirical, 'Empirical evidence on the universal efficacy of balanced literacy.').

omega_variable(
    genuine_synthesis_vs_rebranding,
    'Is balanced literacy a genuine pedagogical synthesis of phonics and whole language, or is it primarily a rebranding of whole language with token phonics elements?',
    'Content analysis of balanced literacy curricula and teacher training materials, comparing the depth and systematicity of phonics instruction against the principles of structured literacy, and comparing the emphasis on ''meaning-making'' against whole language principles.',
    'If it''s a rebranding, its claimed coordination function is largely theatrical, increasing its theater_ratio and extractiveness, pushing it closer to a Snare. If it''s a genuine synthesis, its coordination function is more robust, supporting its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebranding, conceptual, 'The conceptual integrity of balanced literacy as a synthesis.').

omega_variable(
    institutional_inertia_vs_pedagogical_consensus,
    'To what extent does the persistence of balanced literacy stem from institutional inertia and the interests of education schools/publishers, versus a genuine pedagogical consensus among practitioners?',
    'Surveys of classroom teachers'' perceived efficacy of balanced literacy, their desire for more explicit phonics training, and the perceived pressure to adhere to specific methodologies, alongside analysis of funding flows to education schools and curriculum publishers.',
    'If institutional inertia is the primary driver, the suppression metric is higher, and the constraint''s persistence is less about coordination and more about maintaining existing power structures, pushing it towards a Snare. If genuine consensus, it supports the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_pedagogical_consensus, empirical, 'Drivers of balanced literacy''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('balanced_literacy_reading') of the 'literacy_acquisition_kernel', which also includes 'phonics_reading', 'whole_language_reading', and 'structured_literacy_reading'. Each reading represents a distinct pedagogical approach with different structural properties and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
