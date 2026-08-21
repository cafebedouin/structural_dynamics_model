% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint mandates that reading instruction prioritize the needs of
 *   the most vulnerable learners, adopting explicit, cumulative, and
 *   diagnostic principles derived from structured literacy. It is one reading
 *   of the broader 'reading_acquisition_legitimacy' kernel. This reading
 *   emphasizes preventative, intervention-grade instruction for all students,
 *   shifting pedagogical norms and curriculum choices. The claimed type is
 *   'tangled_rope' because it genuinely coordinates effective instruction for
 *   vulnerable learners (beneficiary) but imposes significant costs and
 *   suppresses alternative pedagogies (victims).
 *
 * KEY AGENTS:
 *   - students_with_dyslexia_or_reading_difficulties: Primary beneficiary (powerless/trapped)
 *   - structured_literacy_advocates: Agenda-setter (organized/mobile)
 *   - teachers_trained_in_whole_language_or_balanced_literacy: Primary payer (moderate/identity_locked)
 *   - school_districts_with_legacy_curricula: Payer (institutional/constrained)
 *   - publishers_of_non_structured_literacy_materials: Excluded (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.78).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a').
narrative_ontology:cs_kernel_codification('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', formalized).
narrative_ontology:cs_authority_grounding('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', expertise).
narrative_ontology:cs_interpretation_layer_present('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a').
narrative_ontology:cs_reading_relation('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', foundational, instruction_must_be_diagnostic_and_cumulative).
narrative_ontology:cs_axiom_status(instruction_must_be_diagnostic_and_cumulative, holdable).
narrative_ontology:cs_axiom_grounding('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', instruction_must_be_diagnostic_and_cumulative, empirically_contingent).
narrative_ontology:cs_axiom('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', foundational, vulnerable_learners_set_the_instructional_floor).
narrative_ontology:cs_axiom_status(vulnerable_learners_set_the_instructional_floor, holdable).
narrative_ontology:cs_axiom_grounding('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', vulnerable_learners_set_the_instructional_floor, deontological).
narrative_ontology:cs_reference_frame('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', evidence_based_preventative_pedagogy).
narrative_ontology:cs_drift_state('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', contemporary_policy_adoption_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('841bd8b9-3a93-4dc2-b626-c3ffb70a0e5a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_or_reading_difficulties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_departments).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_whole_language_or_balanced_literacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_legacy_curricula).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, publishers_of_non_structured_literacy_materials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students are the primary intended beneficiaries, receiving explicit, diagnostic, and cumulative instruction tailored to their needs, which is often unavailable in other instructional models. Their 'exit' from poor reading outcomes depends on this instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_or_reading_difficulties, beneficiary,
    powerless, biographical, trapped, local).

% Researchers, parent groups, and professional organizations who champion structured literacy based on cognitive science. They actively lobby for policy changes and curriculum adoption, benefiting from the vindication of their scientific and pedagogical claims.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    organized, generational, mobile, national).

% These departments often find their work validated and integrated into general education when structured literacy is adopted, potentially reducing their caseloads for severe reading difficulties. They benefit from a clearer, evidence-based instructional framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_departments, beneficiary,
    institutional, biographical, constrained, regional).

% Teachers whose professional identity and training are rooted in older pedagogical models. They face significant pressure to retrain, adopt new methods, and discard familiar materials, incurring professional and emotional costs. Their identity is locked into prior practices.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_whole_language_or_balanced_literacy, payer,
    moderate, biographical, identity_locked, local).

% Districts that have invested heavily in non-structured literacy curricula and professional development. They bear the financial and logistical costs of curriculum overhaul, new materials, and extensive teacher retraining, often facing resistance from staff.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_legacy_curricula, payer,
    institutional, generational, constrained, regional).

% Companies whose core business relies on selling materials aligned with whole language or balanced literacy. They are excluded from new procurement cycles and face declining market share as policy shifts, forcing them to adapt or exit the market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, publishers_of_non_structured_literacy_materials, excluded,
    powerful, biographical, constrained, national).

% While often advocating for better instruction, they bear the emotional and financial costs of their children's struggles and may pay for private tutoring if schools are slow to adopt effective methods. They are payers of last resort for remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers, payer,
    organized, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional practice across classrooms and schools to ensure all students, especially those most vulnerable, receive consistent, evidence-based reading instruction that prevents reading failure.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources from less effective, less structured methods to highly explicit, diagnostic, and cumulative instructional practices, primarily benefiting students with reading difficulties at the cost of retraining and curriculum change for educators and districts.
% ABSENT_VOICES: Publishers of non-structured literacy materials and educators deeply invested in older methods are often marginalized in policy discussions, arguing for pedagogical freedom or the value of their existing approaches, but are often dismissed as unscientific.
% DISAPPEARANCE_RATIONALE: If this mandate vanished, many schools would revert to less structured, less effective methods, leading to a resurgence of reading difficulties, particularly among vulnerable learners. Special education caseloads would likely increase, and the scientific consensus on reading acquisition would be ignored in practice.
% FOUNDING_PROBLEM: A significant and persistent gap in reading achievement, particularly for students with dyslexia and other learning disabilities, often exacerbated by instructional methods not aligned with cognitive science.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers, medical professionals (pediatricians, neurologists), and parent advocacy groups consistently corroborate the ongoing problem of reading failure and the efficacy of structured literacy, providing evidence from outside the immediate educational system beneficiaries.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because it demands significant investment in retraining and new materials from districts and teachers, effectively extracting compliance and resources. Suppression (0.78) is high due to the active invalidation and marginalization of alternative pedagogical approaches, enforced through policy mandates and professional development requirements. Theater ratio (0.20) is relatively low, as the core function of improving reading outcomes is genuinely pursued, though some 'compliance theater' may occur in implementation. Accessibility collapse (0.45) is moderate, as alternative pedagogies are not entirely eliminated but are significantly constrained. Resistance (0.70) is high, reflecting the strong pushback from educators and publishers whose practices are being displaced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of structured literacy advocates and students with reading difficulties, this is a beneficial coordination mechanism. From the perspective of teachers and districts invested in older methods, it is an extractive mandate that devalues their expertise and imposes significant costs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with reading difficulties and structured literacy advocates are clear beneficiaries (low d). Teachers and districts with legacy practices are targets (high d) due to the costs of retraining and curriculum change. Publishers of non-structured literacy materials are excluded, facing high extraction from market shifts. Special education departments benefit from alignment with evidence-based practices.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling coordination as pure extraction by clearly identifying the genuine coordination function (effective instruction for vulnerable learners) alongside the asymmetric extraction (costs to implement for legacy systems). It is not a piton because there are clear beneficiaries actively driving its enforcement, and it is not a snare because the coordination function is real and directly addresses a critical problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_vs_outcomes,
    'To what extent is the observed improvement in reading outcomes attributable to high-fidelity implementation of structured literacy principles, versus other confounding factors or ''halo effects''?',
    'Longitudinal studies with randomized controlled trials comparing high-fidelity structured literacy implementation to control groups, controlling for teacher quality and socioeconomic factors.',
    'If outcomes are not strongly tied to fidelity, the mandate''s justification weakens, potentially reducing its perceived legitimacy and extractiveness. If fidelity is critical, it reinforces the need for strong enforcement and training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_vs_outcomes, empirical, 'Assessing the causal link between structured literacy implementation and reading outcomes.').

omega_variable(
    teacher_identity_lock_resolution,
    'Is the ''identity_locked'' exit option for teachers a temporary state during a paradigm shift, or a persistent structural barrier to pedagogical change?',
    'Longitudinal surveys of teacher attitudes and professional identity post-retraining, tracking rates of genuine adoption versus performative compliance over time.',
    'If temporary, the constraint''s suppression will decrease as the new paradigm is internalized. If persistent, the suppression remains high, indicating a deeper structural resistance that may require different policy interventions beyond mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_lock_resolution, empirical, 'Understanding the nature and persistence of teacher identity lock-in during pedagogical shifts.').

omega_variable(
    pedagogical_freedom_vs_scientific_consensus,
    'At what point does a scientific consensus on effective pedagogy legitimately override claims of ''pedagogical freedom'' or ''teacher autonomy''?',
    'Conceptual analysis and policy debate, weighing the epistemic authority of cognitive science against professional autonomy and local adaptation. This is a preference-based resolution.',
    'A strong preference for scientific consensus supports the constraint''s legitimacy and its suppressive force. A strong preference for pedagogical freedom would challenge the constraint''s justification, potentially reclassifying it as more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pedagogical_freedom_vs_scientific_consensus, preference, 'The conceptual boundary between evidence-based mandates and teacher autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.12).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.15).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.18).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel. It focuses on structured literacy as a remediation and preventative measure, distinct from other readings that prioritize phonics, whole language, or a balanced approach. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
