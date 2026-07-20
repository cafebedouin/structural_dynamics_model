% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition Mandate
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   The balanced literacy reading of the reading acquisition kernel posits
 *   that children learn to read through the integration of explicit phonics
 *   and authentic literature exposure. This constraint story treats the
 *   institutionalization of that readingâparticularly in US teacher
 *   preparation, curriculum adoption, and classroom practiceâas a
 *   pedagogical mandate that coordinates warring factions while
 *   asymmetrically extracting reading outcomes from at-risk learners. The
 *   reading is contested: phonics researchers argue the 'balance' collapses
 *   in practice to whole-language assumptions, while whole-language
 *   proponents view balanced literacy as a betrayal of implicit-learning
 *   theory. This story authors the constraint as a tangled rope: it genuinely
 *   coordinates the professional field and resolves an intractable debate,
 *   but its implementation extracts systematic phonics from the students who
 *   most need it, and its persistence depends on active institutional
 *   enforcement through accreditation, textbook markets, and teacher
 *   preparation.
 *
 * KEY AGENTS:
 *   - teacher_training_programs (agenda_setter/institutional): enforce the framework through accreditation and licensure
 *   - curriculum_publishers (beneficiary/organized): capture revenue from integrated curriculum packages
 *   - classroom_teachers (beneficiary/payer/moderate): gain autonomy but bear implementation ambiguity
 *   - at_risk_readers (payer/powerless): bear the cost of insufficient systematic phonics
 *   - parents_of_struggling_readers (excluded/moderate): excluded from curriculum decisions
 *   - systematic_phonics_researchers (observer/organized): external analytical seat documenting the research-to-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '4634c410-7b0e-4a6c-a638-f909413c30b2').
narrative_ontology:cs_kernel_codification('4634c410-7b0e-4a6c-a638-f909413c30b2', formalized).
narrative_ontology:cs_authority_grounding('4634c410-7b0e-4a6c-a638-f909413c30b2', expertise).
narrative_ontology:cs_interpretation_layer_present('4634c410-7b0e-4a6c-a638-f909413c30b2').
narrative_ontology:cs_reading_relation('4634c410-7b0e-4a6c-a638-f909413c30b2', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('4634c410-7b0e-4a6c-a638-f909413c30b2', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('4634c410-7b0e-4a6c-a638-f909413c30b2', foundational, dual_pathway_integration_required).
narrative_ontology:cs_axiom_status(dual_pathway_integration_required, holdable).
narrative_ontology:cs_axiom_grounding('4634c410-7b0e-4a6c-a638-f909413c30b2', dual_pathway_integration_required, empirically_contingent).
narrative_ontology:cs_axiom('4634c410-7b0e-4a6c-a638-f909413c30b2', secondary, teacher_judgment_determines_balance).
narrative_ontology:cs_axiom_status(teacher_judgment_determines_balance, holdable).
narrative_ontology:cs_axiom_grounding('4634c410-7b0e-4a6c-a638-f909413c30b2', teacher_judgment_determines_balance, conventional).
narrative_ontology:cs_reference_frame('4634c410-7b0e-4a6c-a638-f909413c30b2', integrated_explicit_implicit_pathways).
narrative_ontology:cs_drift_state('4634c410-7b0e-4a6c-a638-f909413c30b2', contemporary_classroom_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4634c410-7b0e-4a6c-a638-f909413c30b2', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, at_risk_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and accredit pre-service and in-service teacher education around balanced literacy frameworks. Their institutional legitimacy, enrollment, and research funding depend on maintaining the integrated model as the research-based standard. They set the licensure requirements that constrain what teachers learn.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs, agenda_setter,
    institutional, generational, constrained, national).

% Develop and market integrated balanced literacy curriculum packages, leveled readers, and assessment systems. Their revenue is tied to district-level adoption of materials that brand themselves as balanced. They benefit financially from the institutional ambiguity around what 'balance' means in practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, generational, constrained, national).

% Implement the balanced literacy framework daily in classrooms. They gain professional autonomy and flexibility compared to highly scripted phonics programs, but bear the cognitive and accountability burden of determining the 'correct balance' on their own. They are blamed when student outcomes falter.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer).

% Children with dyslexia, limited early literacy exposure, or other risk factors who attend schools using balanced literacy. They receive insufficient systematic phonics instruction because the 'balance' in practice tilts toward implicit contextual guessing strategies, and they experience reading failure as a direct result.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, at_risk_readers, payer,
    powerless, immediate, trapped, local).

% Parents who observe their children's reading difficulties and request systematic phonics intervention. They are structurally excluded from curriculum adoption committees and often told the current program is 'research-based,' with their concerns dismissed as parental anxiety or medical overreach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, excluded,
    moderate, immediate, constrained, local).

% Cognitive scientists and reading researchers who produce convergent evidence favoring explicit systematic phonics. They observe and document the gap between the research consensus and actual classroom practice, operating from an analytical seat outside the institutional enforcement chain.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, systematic_phonics_researchers, observer,
    organized, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the 'reading wars' by integrating explicit phonics instruction with meaningful engagement with authentic literature, offering teachers a unified professional framework that does not force a binary choice between decoding skills and comprehension-based instruction.
% TRANSFER_FUNCTION: Moves authority over reading method selection from district-level systematic phonics mandates to individual teacher professional judgment; moves reading failure risk onto at-risk students when classroom implementation collapses toward implicit whole-language strategies.
% ABSENT_VOICES: Parents of children with dyslexia and reading disabilities are structurally excluded from curriculum adoption decisions; systematic phonics researchers are present in policy discourse but systematically excluded from teacher preparation program design and accreditation standards.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, school districts would be forced to choose explicit sides in the reading wars again, teacher training content would restructure around either systematic phonics or whole language, curriculum markets would shift away from integrated packages, and at-risk readers would face clearerâthough still contestedâinstructional conditions.
% FOUNDING_PROBLEM: The polarized 'reading wars' of the 1980s and 1990s between phonics and whole language advocates left teachers without a coherent instructional framework and created hostile, unproductive professional divisions in schools of education and school districts.
% FOUNDING_PROBLEM_CORROBORATION: Literacy historians and policy analysts outside the balanced-literacy beneficiary set document the reading wars as a genuine institutional conflict. However, dyslexia advocacy organizations and cognitive scientists outside the benefiting parties argue that the 'wars' were strategically manufactured by whole-language advocates to resist empirical phonics research, and that the balanced compromise was a tactical retreat rather than a solution to a real problem.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) is moderate-high: the constraint does not extract material rents directly from students, but it extracts developmental time and instructional quality from at-risk readers by subordinating systematic phonics to teacher-determined 'balance.' Suppression (0.52) is moderate: pure systematic phonics curricula are not legally barred, but they are professionally marginalized in mainstream teacher training and labeled reductive. Theater ratio (0.48) reflects the substantial performative componentâclassrooms often claim 'balanced literacy' while spending minimal time on explicit phonics. Accessibility collapse (0.42) is incomplete because alternatives (e.g., structured literacy, systematic phonics programs) remain available in the market even if institutionally disfavored. Resistance (0.62) is elevated due to sustained advocacy from dyslexia parents, cognitive scientists, and legislative inquiry.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (teacher trainers, publishers) experience the constraint as genuine coordination that healed a professional schism; the payer seat (at-risk readers) experiences it as a structurally undersystematized instructional regime that conceals whole-language assumptions beneath a phonics veneer. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher training programs and curriculum publishers are structural beneficiaries: they collect professional authority and revenue from the constraint's persistence (low d). Classroom teachers sit near symmetric: they gain autonomy but pay in professional ambiguity and accountability (d near 0.5). At-risk readers are full targets: the constraint's operation directly raises their probability of reading failure (high d). Parents are excluded secondary targets. Systematic phonics researchers are analytical observers with no direct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe reading wars polarizationâmay be dead or manufactured, yet the arrangement persists because it serves the institutional interests of the benefiting parties. The mismatch between founding_problem_status='contested' and disappearance_verdict='world_rearranges' flags a potential mandatrophy: if the problem is dead or illusory, the constraint persists as a zombie coordination mechanism. However, the genuine factional conflict in the field prevents a clean piton classification; the coordination function remains live for teachers who sincerely seek an integrative framework. The classification as tangled rope captures this ambiguity: neither pure coordination nor pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_or_theory_failure,
    'Does the observed extraction stem from flawed implementation of a sound theory, or from a theoretical framework that structurally prioritizes literature exposure over systematic phonics?',
    'Large-scale randomized controlled trials measuring reading outcomes under controlled implementation fidelity conditions, with classroom observation coding time allocation.',
    'If extraction persists under faithful implementation, the framework is structurally extractive and classification should compute snare-ward; if it vanishes, the constraint is a rope degraded by implementation failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_or_theory_failure, empirical, 'Whether extraction is caused by implementation failure or structural theory flaw').

omega_variable(
    whole_language_rebranding,
    'Is balanced literacy a genuine integration of phonics and whole language, or a rhetorical rebranding that allows whole-language practices to persist under a compromise label?',
    'Classroom observation studies coding the proportion of instructional time spent on explicit phonics vs. implicit contextual guessing across districts self-identifying as balanced literacy.',
    'If observation reveals whole-language dominance, the constraint''s coordination function is largely theatrical and the type should compute toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whole_language_rebranding, empirical, 'Whether balanced literacy is genuine integration or whole-language rebranding').

omega_variable(
    kernel_reading_instability,
    'Does the balanced literacy reading collapse toward whole language in practice because its axioms are weaker than its sibling readings, or because institutional incentives favor less structured instruction?',
    'Comparative institutional analysis of implementation fidelity across jurisdictions with different teacher-autonomy norms and accountability regimes.',
    'If institutional incentives drive collapse regardless of axioms, the reading''s reference frame is structurally unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instability, conceptual, 'Whether implementation collapse is axiom weakness or institutional incentive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reading_acquisition_mechanism__balanced_literacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the reading_acquisition_mechanism constraint family. The kernel 'reading acquisition' decomposes into three structurally distinct readingsâphonics, whole language, and balanced literacyâeach with different Îµ values, beneficiary structures, and victim sets. The family linkage captures their mutual displacement and influence in the pedagogical field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
