% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational psychology / literacy pedagogy / cognitive science
 *
 * SUMMARY:
 *   This constraint instantiates the whole_language_reading reading of the
 *   contested kernel reading_acquisition_mechanism. It asserts that reading
 *   develops naturally through immersion in authentic, meaningful texts, with
 *   decoding skills emerging implicitly without systematic explicit
 *   instruction. Sibling readings include phonics_reading (explicit
 *   systematic phonics as foundational) and balanced_literacy_reading
 *   (integrated practice). The three readings emit structurally distinct
 *   constraints with different epsilon values and victim profiles; they form
 *   a constraint family but each carries a single stable epsilon per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - Literacy faculty (institutional/agenda_setter): Shape teacher preparation and state standards around implicit acquisition theories.
 *   - Classroom teachers (moderate/beneficiary): Receive professional autonomy and lower initial planning burden.
 *   - Educational publishers (powerful/beneficiary): Market leveled readers and thematic curriculum packages.
 *   - Struggling readers (powerless/payer): Bear the cost of undetected decoding failure and delayed intervention.
 *   - Families of struggling readers (moderate/payer): Pay for external tutoring and advocacy.
 *   - Phonics researchers (powerful/excluded): Produce contradictory evidence but are marginalized in curriculum governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.75).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational psychology / literacy pedagogy / cognitive science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '3d82bc02-0ff1-408c-bea7-78e4cff26690').
narrative_ontology:cs_kernel_codification('3d82bc02-0ff1-408c-bea7-78e4cff26690', distributed).
narrative_ontology:cs_authority_grounding('3d82bc02-0ff1-408c-bea7-78e4cff26690', practice).
narrative_ontology:cs_interpretation_layer_present('3d82bc02-0ff1-408c-bea7-78e4cff26690').
narrative_ontology:cs_reading_relation('3d82bc02-0ff1-408c-bea7-78e4cff26690', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d82bc02-0ff1-408c-bea7-78e4cff26690', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3d82bc02-0ff1-408c-bea7-78e4cff26690', foundational, implicit_decoding_acquisition).
narrative_ontology:cs_axiom_status(implicit_decoding_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('3d82bc02-0ff1-408c-bea7-78e4cff26690', implicit_decoding_acquisition, empirically_contingent).
narrative_ontology:cs_axiom('3d82bc02-0ff1-408c-bea7-78e4cff26690', foundational, teacher_autonomy_as_professional_necessity).
narrative_ontology:cs_axiom_status(teacher_autonomy_as_professional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3d82bc02-0ff1-408c-bea7-78e4cff26690', teacher_autonomy_as_professional_necessity, conventional).
narrative_ontology:cs_reference_frame('3d82bc02-0ff1-408c-bea7-78e4cff26690', implicit_emergence_framework).
narrative_ontology:cs_drift_state('3d82bc02-0ff1-408c-bea7-78e4cff26690', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d82bc02-0ff1-408c-bea7-78e4cff26690', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, educational_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, families_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train pre-service teachers in whole-language methods, supervise student teaching, publish research validating implicit acquisition theories, and shape state certification standards; their professional reputations and grant networks are built around the paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_faculty, agenda_setter,
    institutional, generational, mobile, national).

% Exercise broad autonomy to select leveled readers and theme-based units rather than follow a systematic phonics scope-and-sequence; experience lower initial planning burden and less rigid accountability for discrete skill mastery.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, beneficiary,
    moderate, biographical, constrained, national).

% Sell leveled-text collections, whole-language curriculum packages, and teacher guides organized around thematic units and cueing systems rather than decodable texts; revenue scales with district adoption of the framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, educational_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Expected to acquire decoding skills implicitly through repeated exposure to authentic literature; when implicit acquisition fails, they accumulate reading deficits, receive misattributed diagnostic labels, and are referred for costly remediation rather than receiving direct systematic instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Bear financial costs of private tutoring, psychoeducational assessments, and advocacy; often told by schools that their child needs more exposure or is not developmentally ready, delaying intervention while deficits deepen.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, families_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Produce convergent empirical evidence for explicit systematic phonics; are systematically underrepresented on state curriculum committees and in teacher-preparation program design, treated as methodologically reductive within whole-language-dominated institutions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_researchers, excluded,
    powerful, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified pedagogical framework that minimizes rigid scripting, honors teacher professional judgment, and situates reading in meaningful, authentic literary experiences rather than decontextualized skill drills.
% TRANSFER_FUNCTION: Transfers instructional autonomy and reduced planning burden to teachers and ed-school faculty; transfers the cost of implicit decoding failure to struggling readers and their families, who must absorb the literacy deficit or pay for external remediation.
% ABSENT_VOICES: Explicit-systematic-phonics researchers, families of struggling readers in under-resourced districts who cannot afford private tutoring, and older students who could testify that they were never taught to decode are largely absent from curriculum adoption committees and teacher-preparation program design.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, classroom practice would shift toward explicit phonics scope-and-sequence, teacher-preparation curricula would reorient around decoding science, the leveled-reader and whole-language curriculum market would contract, and struggling readers would receive direct instruction rather than being expected to induce grapheme-phoneme correspondences from exposure.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction was perceived as overly rigid, skill-drill-oriented, and disconnected from children's lived language and literature, producing disengaged readers.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and reading researchers outside the whole-language beneficiary network (e.g., National Reading Panel, convergent meta-analyses) attest that the founding problem of disengagement did not require abandoning systematic phonics; they corroborate that the problem is either solved or was misdiagnosed, while whole-language advocates self-assert its continued relevance.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.75) because the constraint systematically externalizes the cost of reading failure onto the least powerful learners while privileging teacher autonomy and institutional convenience. Suppression (0.70) reflects active enforcement through teacher-preparation monopolies, curriculum adoption committees, and the marginalization of systematic phonics materials. Theater ratio rises to 0.50 as the empirical challenge intensifies and practitioners perform 'balanced literacy' or 'responsive' frameworks that preserve core whole-language tenets without acknowledging the drift. Accessibility collapse (0.65) captures the near-total absence of systematic phonics inside whole-language-dominated schools despite its availability outside the institutional frame. Resistance (0.60) is substantial and growing, driven by convergent scientific evidence and parent advocacy, yet insufficient to dislodge the entrenched professional infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the literacy-faculty seat, the constraint appears as a rope â genuine coordination around child-centered, meaningful literacy that respects teacher professionalism. From the struggling-reader and family seats, the same structure computes as a snare: a forced immersion in a method that withholds the explicit instruction they need, while labeling their failure as a deficit in the child or home environment. The engine computes this divergence from the same structural data; the authored claim of tangled_rope captures the hybrid reality without adjudicating the seat-level experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Literacy faculty and educational publishers sit near the beneficiary pole: the constraint subsidizes their professional authority and product lines. Classroom teachers are mixed â they collect coordination benefits (autonomy) but are also structurally constrained to enforce the arrangement; their derived directionality is closer to beneficiary than target. Struggling readers are the primary targets (d near 1.0): they are identity-locked and trapped within compulsory schooling that delivers the constraint directly. Families are constrained targets with slightly more exit (advocacy, tutoring) but no effective voice in institutional design. Phonics researchers are excluded rather than targets; their exclusion is what keeps the beneficiary directionality low for the agenda-setting seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) â the coordination function for some learners and the professional-autonomy benefit are real â while also preventing mislabeling it as pure coordination (rope) â the asymmetric harm to struggling readers and the high long-term remediation cost are structural, not incidental. If the founding problem (rigid drill instruction) were still live and the method were sunset for non-responders, it might be a scaffold; absent those conditions, it is a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_decoding_empirical_validity,
    'Does decoding competence actually emerge implicitly for a significant proportion of learners through meaningful exposure alone, or is this true only for children with high home literacy capital?',
    'Longitudinal neuroimaging and controlled classroom studies comparing implicit-exposure-only versus explicit-systematic phonics conditions, disaggregated by socioeconomic status and pre-literacy skills.',
    'If false for the majority, the constraint''s coordination story collapses and the classification shifts toward snare; if true only for a subset, the constraint is a misaligned scaffold that should carry a sunset clause for non-responders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_decoding_empirical_validity, empirical, 'Empirical validity of implicit decoding acquisition claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative phonics instruction structural (explicit district policy and material bans) or internalized (teachers believe explicit phonics is developmentally harmful even when policy permits it)?',
    'Cross-district comparison where phonics materials are legally permitted but adoption rates vary; post-exit teacher interviews after switching from whole-language to science-of-reading schools.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s persistence is identity-driven; if structural, removal requires policy intervention rather than belief change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    remediation_cost_accrual,
    'Does the high long-term remediation cost accrue to identifiable beneficiaries (tutoring industry, assessment providers) or represent pure deadweight loss borne by families and public systems?',
    'Economic flow analysis tracing special-education and private remediation spending to provider revenue and profit margins.',
    'If captured by a concentrated industry, the constraint exhibits snare-like extraction; if diffuse deadweight loss, the extraction is inertial rather than captured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_cost_accrual, empirical, 'Whether remediation costs are captured or deadweight loss').

omega_variable(
    kernel_reading_contest,
    'Is the whole-language reading constraint a genuine empirical claim about learning mechanisms or a normative commitment to teacher professionalism dressed as empirical science?',
    'Historical sociology of the reading wars examining funding flows, professional society statements, and the evidentiary standards applied to whole-language versus phonics studies.',
    'If the latter, the constraint''s authority_grounding should shift from practice/expertise to extraction or lineage, altering the drift-state evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Empirical versus normative grounding of the whole-language reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_mechanism kernel. The whole_language, phonics, and balanced_literacy readings instantiate structurally distinct constraints with different epsilon values, beneficiary structures, and empirical statuses; they should not be averaged or conflated into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
