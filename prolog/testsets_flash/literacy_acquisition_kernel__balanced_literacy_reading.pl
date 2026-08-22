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
 *   human_readable: Balanced Literacy Reading Instruction
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   instruction, which posits that reading acquisition requires both
 *   systematic phonics instruction and meaningful text engagement, viewing
 *   them as complementary. It aims to synthesize the 'reading wars' debate
 *   but is often critiqued for diluting systematic phonics. This is one
 *   reading of the broader 'literacy_acquisition_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.6).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '46b50787-42c7-42a0-876e-2392415e1ea1').
narrative_ontology:cs_kernel_codification('46b50787-42c7-42a0-876e-2392415e1ea1', formalized).
narrative_ontology:cs_authority_grounding('46b50787-42c7-42a0-876e-2392415e1ea1', lineage).
narrative_ontology:cs_interpretation_layer_present('46b50787-42c7-42a0-876e-2392415e1ea1').
narrative_ontology:cs_reading_relation('46b50787-42c7-42a0-876e-2392415e1ea1', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('46b50787-42c7-42a0-876e-2392415e1ea1', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('46b50787-42c7-42a0-876e-2392415e1ea1', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('46b50787-42c7-42a0-876e-2392415e1ea1', foundational, reading_is_complex_and_multifaceted).
narrative_ontology:cs_axiom_status(reading_is_complex_and_multifaceted, holdable).
narrative_ontology:cs_axiom_grounding('46b50787-42c7-42a0-876e-2392415e1ea1', reading_is_complex_and_multifaceted, conventional).
narrative_ontology:cs_axiom('46b50787-42c7-42a0-876e-2392415e1ea1', foundational, instructional_balance_is_optimal).
narrative_ontology:cs_axiom_status(instructional_balance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('46b50787-42c7-42a0-876e-2392415e1ea1', instructional_balance_is_optimal, conventional).
narrative_ontology:cs_reference_frame('46b50787-42c7-42a0-876e-2392415e1ea1', synthesis_of_best_practices).
narrative_ontology:cs_drift_state('46b50787-42c7-42a0-876e-2392415e1ea1', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('46b50787-42c7-42a0-876e-2392415e1ea1', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, early_career_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote balanced literacy as the optimal approach, training new teachers in its principles. They benefit from the ongoing demand for professional development and curriculum materials associated with this approach, and from maintaining a perceived middle ground in the 'reading wars'.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell 'balanced literacy' curriculum packages, often incorporating elements of both phonics and whole language. They profit from the continuous cycle of instructional method adoption and the need for new materials.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Are trained in balanced literacy methods and often feel pressured to implement them, even if they observe mixed results. They bear the cognitive load of synthesizing potentially conflicting instructional demands and the professional risk of deviating from prescribed methods.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, early_career_teachers, payer,
    powerless, biographical, identity_locked, local).

% May not receive the explicit, systematic phonics instruction they need, leading to persistent decoding difficulties. They bear the cost of ineffective instruction, which can manifest as reading delays and reduced academic opportunities.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Often observe their children struggling with reading but lack the pedagogical expertise or institutional leverage to advocate for specific instructional changes. They are excluded from the curriculum decision-making process.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents_of_struggling_readers, excluded,
    moderate, biographical, constrained, local).

% Conduct research on reading acquisition and often critique balanced literacy for its insufficient emphasis on systematic phonics, advocating for evidence-based practices. Their influence is primarily through academic publications and expert testimony.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate instructional practices by providing a framework that integrates different aspects of reading (phonics, comprehension, fluency) into a cohesive approach, reducing fragmentation in teaching methods.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum adoption decisions to education institutions and publishers, who then shape teacher training and resource allocation. It also transfers the burden of instructional synthesis to individual teachers.
% ABSENT_VOICES: Advocates for explicit, systematic phonics (e.g., structured literacy proponents) are often marginalized in balanced literacy discourse, as are parents of children who fail to thrive under this approach. They would argue for a stronger, non-negotiable phonics component.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, there would be a significant shift in teacher training, curriculum development, and classroom practice. Schools would likely adopt more explicitly phonics-heavy or structured literacy approaches, leading to a reorganization of the literacy education landscape.
% FOUNDING_PROBLEM: The 'reading wars' created a polarized instructional landscape, with proponents of phonics and whole language in seemingly irreconcilable conflict, leading to inconsistent and often incomplete teacher training.
% FOUNDING_PROBLEM_CORROBORATION: Education schools and many teachers attest that the problem of instructional polarization is still live, and balanced literacy offers a necessary synthesis. However, cognitive scientists and structured literacy advocates argue that the 'balance' often dilutes effective phonics, and the underlying problem of insufficient evidence-based practice persists, corroborated by ongoing research into reading difficulties.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).
:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by teachers (cognitive load, pressure) and struggling readers (suboptimal instruction), while education schools and publishers benefit from its widespread adoption. Suppression (0.6) is significant, as teachers often face institutional pressure to adhere to this model, limiting their autonomy. The theater ratio (0.4) indicates that while there's genuine pedagogical intent, a substantial portion of its maintenance is performative, aimed at appearing 'balanced' rather than strictly evidence-based. The claimed type is 'tangled_rope' because it genuinely attempts coordination (integrating methods) but also involves asymmetric extraction (from teachers/students to institutions/publishers) and requires active enforcement (institutional mandates).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of education schools, balanced literacy is a 'rope' that resolves the 'reading wars' and provides a comprehensive framework. From the perspective of struggling readers and some teachers, it can feel like a 'snare' due to the lack of explicit phonics and the pressure to conform. The engine's classification as 'tangled_rope' reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and curriculum publishers are beneficiaries, as the model sustains their influence and market. Early career teachers and struggling readers are payers, bearing the costs of implementation and potential instructional gaps. Cognitive scientists act as observers, providing external critique. Parents of struggling readers are excluded, lacking a formal voice in the pedagogical debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the 'reading wars' polarization. While it created a widely adopted framework, critics argue it has not fully resolved the underlying instructional effectiveness problem, particularly for struggling readers. The persistence of the 'reading wars' debate, despite balanced literacy's prevalence, suggests a degree of mandatrophy, where the solution itself has become part of the problem, benefiting its administrators more than its intended beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine pedagogical synthesis of phonics and whole language, or a re-branding of whole language with a token phonics component?',
    'Detailed content analysis of balanced literacy curricula and teacher training materials, comparing the proportion and explicitness of phonics instruction to evidence-based benchmarks, and longitudinal studies of student outcomes.',
    'If a re-brand, its extractiveness and theater_ratio would be higher, reclassifying it closer to a snare, as its coordination claim would be largely performative. If a genuine synthesis, its rope-like qualities would be more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, empirical, 'Whether balanced literacy genuinely integrates phonics or merely pays lip service to it.').

omega_variable(
    teacher_autonomy_vs_institutional_pressure,
    'To what extent does the observed suppression on teachers stem from genuine pedagogical consensus versus institutional pressure to conform to a preferred method?',
    'Surveys and qualitative studies of teacher perceptions of autonomy and pressure, alongside analysis of school district and state-level curriculum mandates.',
    'If suppression is primarily institutional, the constraint''s snare-like qualities are amplified, as teachers are coerced into a method that may not align with their professional judgment or student needs. If it''s genuine consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_institutional_pressure, empirical, 'Distinguishing between consensual and coercive aspects of instructional adoption.').

omega_variable(
    victim_identification_ambiguity,
    'Are struggling readers and early career teachers truly ''victims'' of this approach, or are their difficulties attributable to other factors in the complex educational system?',
    'Controlled studies comparing student outcomes and teacher efficacy under balanced literacy versus alternative evidence-based approaches, controlling for other systemic variables.',
    'If their difficulties are primarily due to balanced literacy, the constraint''s extractiveness is confirmed. If other factors dominate, the victim declaration might be overstated, potentially shifting the classification towards a more benign rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, empirical, 'Clarifying the causal link between balanced literacy and negative outcomes for specific groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
