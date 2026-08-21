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
 *   This constraint represents the 'balanced_literacy_reading' of the
 *   'literacy_acquisition_kernel'. It asserts that effective reading
 *   acquisition requires a complementary approach combining systematic
 *   phonics instruction and meaningful text engagement. This reading emerged
 *   as an attempt to synthesize the opposing 'phonics' and 'whole language'
 *   camps in the 'reading wars', but its efficacy and true nature (synthesis
 *   vs. rebrand) remain highly contested.
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
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a').
narrative_ontology:cs_kernel_codification('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', formalized).
narrative_ontology:cs_authority_grounding('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', extraction).
narrative_ontology:cs_interpretation_layer_present('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a').
narrative_ontology:cs_reading_relation('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', foundational, reading_is_a_natural_process_with_explicit_components).
narrative_ontology:cs_axiom_status(reading_is_a_natural_process_with_explicit_components, holdable).
narrative_ontology:cs_axiom_grounding('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', reading_is_a_natural_process_with_explicit_components, empirically_contingent).
narrative_ontology:cs_axiom('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', foundational, instructional_balance_is_optimal).
narrative_ontology:cs_axiom_status(instructional_balance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', instructional_balance_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', synthesis_of_reading_science).
narrative_ontology:cs_drift_state('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', contemporary_reading_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7d5bae7-c8b6-4a2d-b5bd-07e63aaef60a', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, parents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and promote balanced literacy frameworks, train future teachers, and generate revenue from pedagogical method churn and associated professional development. They benefit from the perceived synthesis and ongoing need for training.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, arbitrage, national).

% Profit from developing and selling 'balanced literacy' curriculum materials, textbooks, and assessment tools. They adapt their offerings to align with prevailing pedagogical trends, including the synthesis offered by balanced literacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Are tasked with implementing balanced literacy in classrooms, often navigating conflicting guidance on phonics and whole language integration. They bear the burden of making the 'balance' work, sometimes without adequate training or resources, leading to instructional incoherence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teachers, payer,
    moderate, biographical, constrained, local).

% May not receive the explicit, systematic phonics instruction they need to develop foundational decoding skills, leading to persistent reading difficulties. Their learning outcomes are directly impacted by the effectiveness of the instructional approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and financial costs of their children's reading struggles, often seeking and paying for supplemental tutoring when school instruction is insufficient. They are often excluded from pedagogical debates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents, payer,
    moderate, biographical, constrained, local).

% Argue for a stronger, more explicit emphasis on systematic phonics instruction, believing balanced literacy dilutes its effectiveness. Their preferred approach is often marginalized or reframed within the balanced literacy discourse.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Advocate for a comprehensive, explicit, and cumulative approach to reading instruction, often rooted in the Orton-Gillingham tradition, which they see as distinct from and more effective than balanced literacy, especially for students with dyslexia. Their voice is often seen as too specialized or rigid by balanced literacy proponents.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Conduct research into the cognitive processes of reading acquisition and evaluate the efficacy of different instructional methods. They provide evidence that often challenges the claims of balanced literacy, but their findings may be selectively interpreted or resisted by institutional actors.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To resolve the 'reading wars' by integrating elements of both phonics and whole language, providing a comprehensive framework for teachers to address both decoding and comprehension skills in a supposedly balanced manner.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum development revenue to education schools and publishers. It transfers the burden of synthesizing potentially conflicting instructional philosophies to individual teachers, and potential learning gaps to students, particularly those who require explicit, systematic instruction.
% ABSENT_VOICES: Advocates for pure phonics, structured literacy, and even pure whole language approaches are often marginalized. They would argue that 'balance' is either an insufficient compromise, a rebrand of less effective methods, or a barrier to truly evidence-based instruction.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, the landscape of reading instruction would undergo significant reorganization. Schools would be forced to adopt more explicitly defined pedagogical approaches, likely leading to a resurgence of either pure phonics/structured literacy or a return to whole language, and a renewed intensity in the 'reading wars' debate.
% FOUNDING_PROBLEM: The intense and often unproductive 'reading wars' of the late 20th century, which created deep divisions in educational policy and practice, leading to inconsistent and often ineffective reading instruction across schools.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (education schools, some publishers) argue the problem of balancing instruction is still live and balanced literacy is the best solution. Critics (structured literacy advocates, many cognitive scientists) argue the original problem was misdiagnosed or that balanced literacy has failed to solve it, citing persistent literacy gaps and the emergence of the 'Science of Reading' movement as evidence of its shortcomings. Independent research often highlights the lack of strong empirical support for the 'balanced' approach as implemented.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) is moderate-high because while it claims to offer a balanced approach, it often leads to insufficient explicit phonics instruction for many students, creating a need for supplemental services. Education schools and curriculum publishers benefit from the continuous churn of pedagogical methods and associated materials. Suppression (0.70) is high because alternative, more explicit, or structured approaches are often marginalized or actively resisted within educational institutions that have adopted balanced literacy. The theater ratio (0.40) reflects that the rhetoric of 'balance' often exceeds the reality of its implementation, with actual classroom practice sometimes leaning heavily towards less explicit instruction, despite claims of synthesis. Resistance (0.75) is high due to ongoing advocacy from structured literacy and phonics proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of education schools and publishers, balanced literacy is a necessary and effective synthesis that resolves pedagogical conflicts. From the perspective of many teachers, parents, and structured literacy advocates, it can be an ambiguous, often ineffective, approach that fails to adequately serve all learners, particularly those with specific learning needs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and curriculum publishers are clear beneficiaries, profiting from the development and dissemination of balanced literacy frameworks and materials. Teachers and struggling readers are primary payers/victims, bearing the burden of implementing potentially incoherent instruction and experiencing suboptimal learning outcomes. Parents also pay through supplemental tutoring. Advocates for alternative methods are structurally excluded from the dominant discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the 'reading wars' and improve literacy outcomes. While it provided a truce, its effectiveness in solving the founding problem is contested. The persistence of balanced literacy, despite mounting evidence for more explicit approaches, suggests that its function may have drifted towards maintaining institutional power and generating revenue for beneficiaries, rather than solely serving its original coordination mandate. This makes it a strong candidate for mandatrophy, where the original problem is either 'dead' or 'contested' while the structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_balanced_literacy_a_rebrand,
    'Is balanced literacy a genuine pedagogical synthesis of phonics and whole language, or is it primarily a rebrand of whole language with a token inclusion of phonics?',
    'Detailed analysis of curriculum materials and classroom observations to quantify the actual proportion and explicitness of phonics instruction versus whole language practices, compared to stated intentions.',
    'If primarily a rebrand, the coordination function is largely theatrical, increasing effective extraction and shifting the classification closer to a Snare. If a genuine synthesis, the coordination function is stronger, supporting a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(is_balanced_literacy_a_rebrand, empirical, 'Ambiguity regarding the true nature of balanced literacy''s pedagogical approach.').

omega_variable(
    effectiveness_for_all_learners,
    'Does balanced literacy effectively teach all students to read, particularly those with dyslexia or other learning differences, or does it systematically underserve specific populations?',
    'Longitudinal studies comparing reading outcomes of diverse student populations under balanced literacy versus structured literacy approaches, controlling for socioeconomic factors.',
    'If it systematically underserves specific populations, the victim set is more clearly defined and the extractiveness is higher for those groups, strengthening the Snare-like aspects of the constraint. If universally effective, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_for_all_learners, empirical, 'Efficacy of balanced literacy across the spectrum of student learning needs.').

omega_variable(
    institutional_capture_of_pedagogy,
    'To what extent do education schools and curriculum publishers benefit from the continuous churn of pedagogical methods, including the adoption and adaptation of ''balanced literacy'', regardless of its proven efficacy?',
    'Economic analysis of revenue streams for education schools (enrollment, professional development) and publishers (curriculum sales) correlated with pedagogical shifts, alongside analysis of resistance to evidence-based reforms.',
    'If institutional capture is high, the ''extraction'' grounding of authority is strengthened, and the constraint''s persistence is less about coordination and more about rent-seeking, pushing classification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_pedagogy, empirical, 'Role of institutional self-interest in perpetuating pedagogical trends.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''balanced_literacy_reading'' of the ''literacy_acquisition_kernel''. How do its core premises about reading acquisition differ from sibling readings like ''phonics_reading'', ''whole_language_reading'', and ''structured_literacy_reading''?',
    'Comparative analysis of foundational pedagogical texts and curriculum guidelines for each reading, identifying points of convergence and divergence in instructional sequence, emphasis, and theoretical grounding.',
    'Clarifies the specific structural elements of the kernel that are interpreted differently across readings, informing the engine''s analysis of inter-reading relationships and potential for conflict or synthesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as one specific reading within a contested kernel of literacy acquisition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t6, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(lite_tr_t18, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lite_be_t6, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(lite_be_t18, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lite_su_t6, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(lite_su_t18, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.7).


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
