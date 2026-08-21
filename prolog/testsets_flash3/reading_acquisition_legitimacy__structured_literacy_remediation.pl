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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Structured Literacy Remediation Mandate for Vulnerable Learners
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the mandate that reading instruction must be
 *   designed for the most vulnerable learners first, following explicit,
 *   cumulative, diagnostic principles from structured literacy. It is a
 *   reading of the broader 'reading_acquisition_legitimacy' kernel,
 *   emphasizing a preventative, intervention-grade approach for all students.
 *   The constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates effective instruction for a critical problem (reading
 *   failure) but does so by extracting significant costs from educators and
 *   institutions, requiring active enforcement to overcome resistance to
 *   change.
 *
 * KEY AGENTS:
 *   - structured_literacy_advocates: Agenda setter (organized/mobile) — drives policy adoption
 *   - students_with_dyslexia_and_reading_difficulties: Primary beneficiary (powerless/trapped) — receives targeted instruction
 *   - teachers_trained_in_other_methods: Payer (moderate/constrained) — bears retraining costs and professional identity shift
 *   - school_districts_with_legacy_curricula: Payer (institutional/constrained) — bears implementation costs and resistance
 *   - parents_of_struggling_readers: Beneficiary (organized/constrained) — advocates for and benefits from effective instruction
 *   - whole_language_proponents: Excluded (organized/identity_locked) — marginalized by the policy shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.7).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate for Vulnerable Learners").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, 'c9a4e57e-4ade-4d3a-8786-31e2b453c024').
narrative_ontology:cs_kernel_codification('c9a4e57e-4ade-4d3a-8786-31e2b453c024', formalized).
narrative_ontology:cs_authority_grounding('c9a4e57e-4ade-4d3a-8786-31e2b453c024', expertise).
narrative_ontology:cs_interpretation_layer_present('c9a4e57e-4ade-4d3a-8786-31e2b453c024').
narrative_ontology:cs_reading_relation('c9a4e57e-4ade-4d3a-8786-31e2b453c024', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('c9a4e57e-4ade-4d3a-8786-31e2b453c024', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('c9a4e57e-4ade-4d3a-8786-31e2b453c024', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('c9a4e57e-4ade-4d3a-8786-31e2b453c024', foundational, reading_is_skill_not_natural_emergence).
narrative_ontology:cs_axiom_status(reading_is_skill_not_natural_emergence, holdable).
narrative_ontology:cs_axiom_grounding('c9a4e57e-4ade-4d3a-8786-31e2b453c024', reading_is_skill_not_natural_emergence, empirically_contingent).
narrative_ontology:cs_axiom('c9a4e57e-4ade-4d3a-8786-31e2b453c024', foundational, vulnerable_learners_set_instructional_floor).
narrative_ontology:cs_axiom_status(vulnerable_learners_set_instructional_floor, holdable).
narrative_ontology:cs_axiom_grounding('c9a4e57e-4ade-4d3a-8786-31e2b453c024', vulnerable_learners_set_instructional_floor, deontological).
narrative_ontology:cs_reference_frame('c9a4e57e-4ade-4d3a-8786-31e2b453c024', universal_intervention_grade_instruction).
narrative_ontology:cs_drift_state('c9a4e57e-4ade-4d3a-8786-31e2b453c024', contemporary_implementation_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c9a4e57e-4ade-4d3a-8786-31e2b453c024', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_difficulties).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_legacy_curricula).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, science_of_reading_evidence).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, early_intervention_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and lobby for the adoption of structured literacy principles, emphasizing early intervention and evidence-based practices for all learners, especially those at risk. They benefit from the increased legitimacy and funding for their methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Are the primary intended beneficiaries, receiving instruction specifically designed to address their learning needs, which was often unavailable or inadequate under previous instructional paradigms. Their 'exit' from reading difficulties is directly tied to the constraint's effective implementation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_difficulties, beneficiary,
    powerless, biographical, trapped, local).

% Face significant professional development costs and pressure to abandon long-held pedagogical beliefs and practices. They must retrain, adapt their teaching, and often feel devalued if their prior methods are dismissed as ineffective. Their professional identity is challenged.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Bear the financial and logistical burden of curriculum overhaul, new materials, and extensive teacher training. They face political pressure from advocates and potential legal challenges if they fail to implement effective reading instruction, but also resistance from entrenched staff and budget limitations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_legacy_curricula, payer,
    institutional, generational, constrained, regional).

% Benefit from the promise of effective instruction for their children, often having advocated for such changes for years. They exert pressure on schools and districts for implementation, but their options are limited by available school choices and resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Are largely excluded from the policy-making conversation, their methods often explicitly rejected by this reading. They continue to advocate for their approach but face declining institutional support and professional marginalization.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_proponents, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reading instruction across diverse student populations and teaching staff, ensuring a consistent, evidence-based approach that prioritizes the needs of the most vulnerable learners, thereby reducing instructional variability and improving outcomes.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources from less structured, eclectic approaches to highly structured, explicit, and diagnostic methods. It also transfers the burden of adaptation and retraining to educators and districts, while transferring the benefit of effective instruction to students.
% ABSENT_VOICES: Proponents of whole language and balanced literacy, who emphasize meaning-making and authentic literature, are largely absent from the policy discussions driving this mandate. They would argue for a broader view of literacy and against a 'one-size-fits-all' approach.
% DISAPPEARANCE_RATIONALE: If this mandate vanished, many schools would likely revert to less structured, more varied instructional methods, potentially leaving vulnerable learners without the targeted support they need. The 'science of reading' movement would lose significant policy traction, and the landscape of literacy education would become fragmented again.
% FOUNDING_PROBLEM: A significant and persistent achievement gap in reading, particularly for students with dyslexia and other learning disabilities, exacerbated by inconsistent and often ineffective instructional practices that failed to apply cognitive science research.
% FOUNDING_PROBLEM_CORROBORATION: Educational psychologists, cognitive scientists, and parent advocacy groups consistently corroborate the existence and severity of the reading achievement gap and the historical failure of many instructional methods to address it. Longitudinal studies on reading outcomes also support the problem's live status.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is high because the mandate imposes substantial costs on existing educational systems, requiring significant investment in new curricula, materials, and extensive professional development for teachers. Suppression (0.70) is also high, reflecting the active policy enforcement, legislative mandates, and public pressure needed to overcome entrenched pedagogical traditions and institutional inertia. The theater ratio (0.20) is relatively low, as the core function of improving reading outcomes for vulnerable learners is genuinely pursued, though some 'compliance theater' may exist in superficial adoption without deep implementation. Accessibility collapse (0.40) is moderate, as alternative instructional methods are not entirely eliminated but are significantly disincentivized or formally restricted. Resistance (0.55) is notable, stemming from teachers' professional identity and districts' resource constraints.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of structured literacy advocates and parents of struggling readers, this is a necessary and beneficial coordination mechanism that corrects historical injustices in reading instruction. From the perspective of teachers trained in other methods and school districts with legacy curricula, it is an extractive mandate that imposes significant costs and disrupts established practices, even if they acknowledge the underlying problem of reading failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured literacy advocates and students with reading difficulties are clear beneficiaries, as the constraint directly supports their interests and needs. Teachers and school districts are payers, bearing the costs of transition and compliance. Parents of struggling readers are also beneficiaries, aligning with the advocates. Whole language proponents are excluded, as their pedagogical approach is actively suppressed by this mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring the significant costs and resistance) or a pure Snare (ignoring the genuine coordination function for vulnerable learners). The mandate addresses a live problem (reading failure) but its implementation involves substantial extraction from existing educational structures, requiring active enforcement to maintain. The rising extractiveness and suppression over time reflect the increasing pressure for adoption and the costs associated with systemic change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_vs_outcome,
    'To what extent is the observed improvement in reading outcomes attributable to high-fidelity implementation of structured literacy principles, versus other confounding factors?',
    'Rigorous randomized controlled trials comparing high-fidelity structured literacy implementation to control groups, controlling for teacher experience, student demographics, and school resources.',
    'If outcomes are not strongly correlated with fidelity, the mandate''s coordination function may be weaker or more theatrical than assumed, potentially shifting its classification towards a Piton or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_vs_outcome, empirical, 'Assesses the causal link between structured literacy implementation and student outcomes.').

omega_variable(
    teacher_professional_identity_cost,
    'Is the resistance from teachers primarily due to a lack of training and resources, or a deeper challenge to their professional identity and pedagogical autonomy?',
    'Qualitative studies and surveys exploring teacher perceptions, professional identity, and the perceived value of different instructional approaches, alongside analysis of resource allocation for professional development.',
    'If identity cost is dominant, the ''extraction'' from teachers is more profound than financial/training burden, potentially increasing their effective directionality and the constraint''s overall extractiveness from that seat. This would also suggest higher suppression is needed to overcome this deeper resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_professional_identity_cost, conceptual, 'Distinguishes between practical and identity-based sources of teacher resistance.').

omega_variable(
    structured_literacy_scope_applicability,
    'Is the ''intervention-grade instruction for all'' principle universally beneficial, or does it impose unnecessary structure on learners who would thrive with less explicit approaches, thereby extracting from them?',
    'Longitudinal studies comparing outcomes for diverse learners under universal structured literacy versus differentiated instruction models, particularly for students without identified reading difficulties.',
    'If universal application proves suboptimal for some learners, the constraint''s beneficiary structure becomes more complex, potentially introducing a ''victim'' class among non-struggling readers, increasing overall extractiveness and shifting the classification towards a Snare for those students.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structured_literacy_scope_applicability, empirical, 'Evaluates the universal applicability and potential over-extraction of structured literacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel. Its focus on remediation and vulnerable learners distinguishes it from other readings that prioritize decoding, meaning-making, or a balance of both. It influences and is influenced by the other readings in the ongoing 'science of reading' debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
