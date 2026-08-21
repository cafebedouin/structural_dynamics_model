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
 *   human_readable: Balanced Literacy Reading of Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' reading of reading
 *   acquisition, which posits that both systematic phonics instruction and
 *   meaningful text engagement are complementary and essential. It emerged as
 *   a response to the 'reading wars' between phonics and whole language,
 *   aiming for a synthesis. However, its implementation and efficacy are
 *   highly contested, with critics arguing it often serves as a rebrand of
 *   whole language and fails to provide sufficient explicit instruction,
 *   particularly for struggling readers. This story instantiates one reading
 *   of the broader 'literacy_acquisition_kernel'.
 *
 * KEY AGENTS:
 *   - education_schools: Agenda setter, beneficiary (institutional/arbitrage)
 *   - curriculum_publishers: Beneficiary (organized/arbitrage)
 *   - teachers_seeking_clarity: Payer (moderate/constrained)
 *   - students_struggling_with_reading: Payer (powerless/trapped)
 *   - parents: Payer (moderate/constrained)
 *   - phonics_advocates: Excluded (organized/constrained)
 *   - structured_literacy_advocates: Excluded (organized/constrained)
 *   - cognitive_scientists: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading of Reading Acquisition").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'bf508c91-bcd5-4fe4-9c0e-60403a85d2c8').
narrative_ontology:cs_kernel_codification('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', formalized).
narrative_ontology:cs_authority_grounding('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', practice).
narrative_ontology:cs_interpretation_layer_present('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8').
narrative_ontology:cs_reading_relation('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', foundational, reading_is_natural_and_taught).
narrative_ontology:cs_axiom_status(reading_is_natural_and_taught, holdable).
narrative_ontology:cs_axiom_grounding('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', reading_is_natural_and_taught, conventional).
narrative_ontology:cs_axiom('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', foundational, phonics_and_meaning_equally_essential).
narrative_ontology:cs_axiom_status(phonics_and_meaning_equally_essential, holdable).
narrative_ontology:cs_axiom_grounding('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', phonics_and_meaning_equally_essential, conventional).
narrative_ontology:cs_reference_frame('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', synthesis_of_best_practices).
narrative_ontology:cs_drift_state('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', contemporary_reading_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bf508c91-bcd5-4fe4-9c0e-60403a85d2c8', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, students_struggling_with_reading).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, teachers_seeking_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, parents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes and trains teachers in the balanced literacy approach, shaping pedagogical norms and curriculum adoption. Benefits from the continuous demand for professional development and new instructional materials associated with this framework.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, arbitrage, national).

% Develop and market 'balanced literacy' curriculum packages, textbooks, and supplementary materials. Profits from the adoption cycles and the perceived need for comprehensive, integrated resources.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Are tasked with implementing balanced literacy in their classrooms, often navigating conflicting advice and a lack of clear, evidence-based guidance on how to achieve the 'balance.' Bear the burden of mixed student outcomes and professional uncertainty.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teachers_seeking_clarity, payer,
    moderate, biographical, constrained, local).

% Are the primary targets of reading instruction. If balanced literacy fails to provide sufficient explicit phonics or coherent text engagement, these students may not acquire foundational reading skills, leading to long-term academic and social disadvantages.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, students_struggling_with_reading, payer,
    powerless, immediate, trapped, local).

% Invest time and resources in their children's education. If balanced literacy yields poor results, they bear the emotional and financial costs of seeking supplemental instruction or advocating for different pedagogical approaches.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents, payer,
    moderate, biographical, constrained, local).

% Advocate for explicit, systematic phonics instruction as the primary foundation for reading. Their approach is often framed as 'extreme' or 'incomplete' by balanced literacy proponents, limiting its adoption in mainstream education.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Champion a comprehensive, explicit, and cumulative approach to all components of reading. Their methods are often marginalized as specialized interventions rather than universal best practice, despite growing scientific evidence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Conduct research on reading acquisition and cognitive processes. Often find their evidence-based recommendations for explicit, systematic instruction are not fully integrated or are misinterpreted within the balanced literacy framework.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to provide a comprehensive and flexible framework for reading instruction that integrates phonics and meaningful text engagement, seeking to resolve the historical 'reading wars' by offering a middle ground.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum revenue to education institutions and publishers, while transferring the burden of implementation and the risk of suboptimal outcomes to teachers and students.
% ABSENT_VOICES: The voices of students who fail to learn to read effectively under this model are often diffuse and lack institutional power. Researchers whose findings on the science of reading contradict the 'balanced' approach are also often marginalized in pedagogical discourse.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, the 'reading wars' would likely re-intensify, forcing schools and educators to explicitly choose between more phonics-centric or whole-language-centric approaches, or to adopt structured literacy. Curriculum development and teacher training would undergo significant reorganization.
% FOUNDING_PROBLEM: To end the 'reading wars' by synthesizing the perceived strengths of both phonics and whole language, thereby avoiding the perceived extremism and limitations of either approach and providing a more holistic instructional model.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (education schools, some publishers) claim the problem of integrating diverse instructional needs is still live. Critics (structured literacy advocates, some cognitive scientists, parents of struggling readers) argue that the founding problem was never truly solved, and that balanced literacy often perpetuated ineffective practices, citing stagnant literacy rates and a growing body of research on reading science from outside the benefiting parties.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.62) is moderate because while it claims to offer a balanced approach, it often leads to curriculum churn and benefits specific institutions and publishers. Suppression (0.70) is high due to the institutional pressure on teachers to adopt this framework, often marginalizing alternative, more explicit methods. The theater ratio (0.45) reflects the ongoing debate about whether 'balance' is genuinely achieved or if it's a performative label masking a continued emphasis on less explicit instruction. Resistance (0.75) is high from advocates of more explicit methods and from parents of struggling readers. The measurements show a slight increase in extractiveness and theater over time, reflecting the growing critique that the 'balance' has become more rhetorical than functional.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of education schools and curriculum publishers, balanced literacy is a necessary and effective synthesis, providing a comprehensive framework. From the perspective of teachers and parents, especially those with struggling readers, the constraint can feel extractive and suppressive, as it may not deliver effective outcomes and limits access to alternative, more explicit instructional methods. Cognitive scientists often view it as misaligned with research on reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and curriculum publishers are clear beneficiaries, profiting from the framework's adoption and associated materials. Teachers and students, particularly those struggling, bear the costs of implementation and potential instructional inadequacy, making them targets. Phonics and structured literacy advocates are excluded, as their approaches are often sidelined by the dominant 'balanced' narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to resolve the 'reading wars' is contested. If balanced literacy is indeed a rebrand of whole language, as critics suggest, then its coordination function has atrophied, and it persists primarily as a mechanism for institutional and commercial extraction, making it a snare rather than a genuine rope. The high theater ratio and contested founding problem status support this interpretation, preventing mislabeling it as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine synthesis of phonics and whole language, or is it primarily a re-packaging of whole language with superficial phonics elements?',
    'Empirical analysis of curriculum content and classroom implementation, comparing the actual instructional time and depth dedicated to explicit phonics versus implicit strategies, against the stated principles of the balanced literacy framework.',
    'If it''s a rebrand, the constraint''s extractiveness and theater ratio are higher, and its coordination function is largely illusory, pushing its classification closer to a Snare. If it''s a genuine synthesis, its coordination function is stronger, supporting a Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, empirical, 'Whether balanced literacy achieves its stated goal of synthesis or merely rebrands an older approach.').

omega_variable(
    victim_identification_ambiguity,
    'Are the primary victims of balanced literacy students who fail to learn to read, or teachers who are given unclear and ineffective pedagogical guidance?',
    'Longitudinal studies tracking student literacy outcomes correlated with specific instructional practices, alongside surveys and qualitative research on teacher efficacy and professional satisfaction within balanced literacy frameworks.',
    'Clarifying the primary victim group would refine the directionality and impact analysis, potentially highlighting systemic failures in teacher training or curriculum design as the core extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, empirical, 'Identifying the most impacted group by the constraint''s operational failures.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative reading pedagogies structural (e.g., curriculum mandates, funding streams) or internalized (e.g., teacher identity, professional norms)?',
    'Post-mandate-removal analysis: if alternative pedagogies remain suppressed after institutional mandates are lifted, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as teachers carry the suppression with them. If purely structural, removing mandates would quickly diversify pedagogical approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t6, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(lite_tr_t18, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 18, 0.45).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lite_be_t6, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(lite_be_t18, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lite_su_t6, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(lite_su_t18, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', which encompasses multiple contested pedagogical approaches to reading acquisition. Its ε value and structural properties differ significantly from sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
