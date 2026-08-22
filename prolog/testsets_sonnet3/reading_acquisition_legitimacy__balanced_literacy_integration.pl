% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Reading
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy positions itself as the reasonable synthesis in the
 *   reading wars: reading requires both decoding and meaning-making, so
 *   legitimate instruction blends explicit phonics with authentic literature
 *   exposure and lets teachers toggle between direct instruction and
 *   facilitation depending on each child's need. This reading of the
 *   reading-acquisition-legitimacy kernel is the dominant one in US teacher
 *   preparation and curriculum adoption from roughly the mid-1990s through
 *   the mid-2010s 'science of reading' pushback. Structurally, the
 *   coordination function (serving heterogeneous classrooms without formal
 *   tracking, preserving teacher professional discretion) is real, but it
 *   rides alongside asymmetric extraction: publishers and teacher colleges
 *   profit from the perpetual need for differentiated materials and
 *   retraining, while the framework's diagnostic burden falls on teachers who
 *   were never equipped to determine when to deploy which strand, and its
 *   cost falls hardest on decoding-impaired and dyslexic children whose
 *   specific, systematic-phonics needs are diluted into 'balance.' This is a
 *   distinct constraint from the phonics_decoding_primacy and
 *   whole_language_meaning_primacy readings of the same kernel — those
 *   readings claim reading IS decoding, or IS meaning-making, respectively,
 *   and would produce different ε and different victim sets (a pure phonics
 *   regime would primarily burden students needing rich comprehension
 *   instruction; a pure whole-language regime would primarily burden the same
 *   decoding-impaired population this reading also burdens, but through
 *   neglect rather than dilution).
 *
 * KEY AGENTS:
 *   - district_literacy_coordinators: agenda-setting institutional actor administering curriculum adoption
 *   - balanced_literacy_curriculum_publishers: organized beneficiary profiting from perpetual product differentiation
 *   - teacher_colleges_of_education_faculty: institutional beneficiary with identity-locked commitment to the framework
 *   - struggling_decoders_in_mixed_classrooms and dyslexic_students_underserved_by_incidental_phonics: powerless, trapped payers bearing diluted intervention
 *   - classroom_teachers_lacking_diagnostic_training: moderate-power payer absorbing blame without adequate diagnostic tools
 *   - cognitive_reading_scientists: excluded analytical voice with strong evidence but no adoption authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.42).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.38).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Reading").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'ae4f08bb-0452-44a5-b6ff-c9c564ba06ce').
narrative_ontology:cs_kernel_codification('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', distributed).
narrative_ontology:cs_authority_grounding('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', practice).
narrative_ontology:cs_interpretation_layer_present('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce').
narrative_ontology:cs_reading_relation('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', reading_acquisition_legitimacy__whole_language_meaning_primacy, influences).
narrative_ontology:cs_axiom('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', foundational, reading_is_multicomponential_requiring_both_strands).
narrative_ontology:cs_axiom_status(reading_is_multicomponential_requiring_both_strands, holdable).
narrative_ontology:cs_axiom_grounding('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', reading_is_multicomponential_requiring_both_strands, empirically_contingent).
narrative_ontology:cs_axiom('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', secondary, teacher_professional_discretion_over_method_sequencing_is_legitimate).
narrative_ontology:cs_axiom_status(teacher_professional_discretion_over_method_sequencing_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', teacher_professional_discretion_over_method_sequencing_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', reading_wars_stalemate_synthesis).
narrative_ontology:cs_drift_state('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', post_science_of_reading_legislative_wave, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae4f08bb-0452-44a5-b6ff-c9c564ba06ce', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, district_literacy_coordinators).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_of_education_faculty).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_decoders_in_mixed_classrooms).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students_underserved_by_incidental_phonics).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_lacking_diagnostic_training).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, fluent_readers_in_mixed_classrooms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects and mandates the balanced literacy curriculum and professional development sequence across a district, framing it as the moderate, evidence-synthesizing middle path between 'extremist' phonics-only and whole-language-only camps. Insulated from classroom-level outcomes; career incentives favor stability of the adopted program over responsiveness to reading data.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_literacy_coordinators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Sell leveled-text libraries, guided-reading kits, and 'three-cueing' assessment materials bundled with light phonics components, marketed as the scientifically balanced solution. Revenue depends on the perception that no single method suffices and that ongoing curriculum refreshes and PD contracts are necessary; a decisive resolution toward one pure method would collapse the differentiated product line.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Trains new teachers in balanced literacy as the professionally sophisticated, child-centered synthesis, having built careers, textbooks, and departmental identities around the framework since it superseded pure whole language. Revising the framework toward systematic phonics primacy would require admitting decades of teacher preparation under-equipped graduates to teach decoding explicitly.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_of_education_faculty, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_of_education_faculty, agenda_setter).

% Expected to toggle fluidly between explicit phonics instruction and facilitated meaning-making for each child, but received minimal training in diagnosing which struggling readers need which intervention when. Absorbs the blame when children fail to read, without the assessment tools the framework's design presupposes they have. Leaving the mandated curriculum risks professional censure; leaving the profession forfeits training investment.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_lacking_diagnostic_training, payer,
    moderate, biographical, constrained, local).

% Children whose reading difficulty is specifically decoding-based receive intermixed guided-reading and picture-cueing strategies alongside phonics, diluting the explicit, systematic instruction they most need. Their families often lack the means to purchase private tutoring that supplies pure structured-literacy intervention, so they ride out the balanced approach until failure becomes visible, often years later.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_decoders_in_mixed_classrooms, payer,
    powerless, biographical, trapped, local).

% Require highly systematic, cumulative, diagnostic phonics instruction that the balanced model provides only as one strand among several, contingent on teacher judgment about when to deploy it. Delayed identification is common because meaning-making cueing strategies mask decoding gaps for one to two years, during which the intervention window narrows.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students_underserved_by_incidental_phonics, payer,
    powerless, biographical, trapped, local).

% Children who decode easily benefit from the authentic-literature half of the arrangement, gaining rich exposure to text meaning and motivation to read broadly without needing the systematic phonics component to carry them. For this population the balanced approach functions largely as advertised.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, fluent_readers_in_mixed_classrooms, beneficiary,
    moderate, biographical, mobile, local).

% Publish meta-analyses (National Reading Panel and successors) showing decoding automaticity is the binding constraint for most early reading failure and that three-cueing / incidental-phonics elements of balanced literacy lack support. Cited selectively by curriculum committees but rarely given authority over adoption decisions, which remain with district coordinators and publishers.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_reading_scientists, excluded,
    analytical, civilizational, analytical, global).

% Periodically investigate reading proficiency data and issue guidance or mandates (e.g., 'science of reading' legislation) that can force districts to abandon three-cueing components, but implementation and enforcement vary widely and are frequently absorbed back into balanced framing rather than replaced.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_education_departments, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single instructional framework that lets a district serve a heterogeneous classroom — fluent readers, decoding-strugglers, English learners, dyslexic students — without formally tracking or separately resourcing each group, and gives teachers professional discretion to differentiate moment to moment.
% TRANSFER_FUNCTION: Moves instructional time and curriculum-adoption budgets from systematic, diagnostic phonics intervention toward leveled-text libraries, guided-reading materials, and generalist teacher PD; moves the cost of underserved decoding gaps onto struggling readers and their families in the form of delayed diagnosis and remediation.
% ABSENT_VOICES: Cognitive reading scientists whose meta-analytic evidence on decoding automaticity is cited in policy documents but rarely given adoption authority; parents of dyslexic children who discover the gap only after years of grade-level failure and are not represented in curriculum committee composition.
% DISAPPEARANCE_RATIONALE: Curriculum publishers and teacher colleges would need to restructure product lines and training sequences (world_rearranges for them); many teachers already blend explicit phonics with literature exposure in practice regardless of the framework label, so instructional practice for experienced teachers might change little (world_unchanged); the contest is precisely whether the label does structural work beyond describing what good teachers already do.
% FOUNDING_PROBLEM: The 1980s-90s 'reading wars' pitted phonics-only against whole-language-only camps, each producing casualties (rote decoders who couldn't comprehend; fluent guessers who couldn't decode unfamiliar words); balanced literacy was built to resolve the political and pedagogical stalemate by claiming both were partially right.
% FOUNDING_PROBLEM_CORROBORATION: District coordinators and teacher colleges attest the founding problem (polarized pedagogy harming both extremes) remains live and the balanced synthesis is the solution. Cognitive reading scientists and structured-literacy advocates — outside the beneficiary set — attest the founding problem was actually decoding instruction being insufficiently systematic, and that balanced literacy's incidental-phonics compromise re-created the whole-language failure mode under a different name; state legislative hearings on reading proficiency data (multiple US states, 2019-2023) corroborate persistent decoding-skill gaps under balanced-literacy adoption.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits at moderate-tangled level: this is not a naked extraction mechanism, but the coordination story (serving diverse learners flexibly) provides genuine cover for a structure that also transfers curriculum-adoption resources toward publishers and teacher-college programs while deferring diagnostic costs onto teachers and dyslexic/decoding-impaired children. Suppression (0.38) is moderate — the framework is not coercively imposed against strong alternatives so much as institutionally entrenched through teacher-preparation pipelines and adoption committee composition, making exit costly but not impossible (hence 'contested' on disappearance). Theater ratio (0.47, rising over the interval) reflects the framework's increasing tendency to rebrand as 'the science of reading is compatible with balanced literacy' in response to legislative pressure, without necessarily restructuring the diagnostic and instructional-time allocation that produces the extraction. Accessibility collapse is moderate (0.35) — parents with resources can and do exit to private structured-literacy tutoring, so alternatives have not fully collapsed, but families without resources are effectively trapped. Resistance (0.55) reflects the substantial and growing 'science of reading' advocacy movement, journalism (e.g., Sold a Story), and state legislative action pushing back against the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the district-coordinator and publisher seats, balanced literacy is coordination: it resolves professional conflict, serves diverse learners, and preserves teacher autonomy. From the seat of a dyslexic child's family two years into unrecognized reading failure, the same structure looks like negligent extraction — resources and instructional time were available and were spent on materials and philosophy rather than the systematic intervention that would have worked. The engine computes both seats' types from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   District coordinators and teacher colleges sit near the beneficiary end: they set curriculum policy, retain institutional legitimacy as pedagogical experts, and bear little direct cost when reading outcomes lag. Curriculum publishers sit at the full-beneficiary end with arbitrage-grade exit — they can pivot product lines as policy shifts. Classroom teachers are structurally caught in the middle: nominally implementing agents but functionally payers, since they lack the diagnostic training the framework presupposes and bear reputational and professional risk for outcomes they were not equipped to prevent (moderate power, constrained exit). Struggling decoders and dyslexic students are full targets: powerless, trapped in the classroom assignment their family cannot alter, and the population whose specific instructional need (systematic, cumulative phonics) is most diluted by the balance the framework insists on. Fluent readers are genuine beneficiaries — for this subpopulation the coordination story is essentially accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling balanced literacy as pure extraction (a snare) by preserving its genuine coordination function: heterogeneous classrooms do need differentiated approaches, and framework flexibility has real value for fluent readers and teachers with strong diagnostic skill. It equally prevents mislabeling it as pure rope by requiring the victim declaration — decoding-impaired and dyslexic children are not incidentally affected but structurally underserved by the framework's design logic, which treats phonics as one strand among several rather than as the binding constraint research identifies for most early reading failure. The founding-problem mismatch (status: contested, with corroboration split along beneficiary/non-beneficiary lines) is exactly the mandatrophy signal: the reading-wars stalemate that justified the framework's founding has arguably been resolved by subsequent cognitive science, but the institutional apparatus persists because it now serves administrative and commercial interests independent of its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Is the diagnostic burden balanced literacy places on teachers (toggling between phonics and meaning-making per child) a necessary cost of genuinely differentiated instruction, or is it a design flaw that could be corrected by better initial teacher training without abandoning the pluralistic framework?',
    'Compare reading outcomes and teacher-reported confidence in districts that pair balanced literacy with rigorous diagnostic-assessment training (e.g., structured phonics screeners layered onto balanced literacy) against districts using balanced literacy without such training, controlling for demographics.',
    'If diagnostic training closes most of the outcome gap, the extraction is contingent (a fixable implementation failure) rather than structural, pushing the classification toward rope with an implementation defect. If the gap persists even with strong diagnostic training, the extraction is more likely structural to the framework''s mixed-strand design, supporting the tangled_rope classification as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether balanced literacy''s extraction from struggling decoders is a fixable training gap or a structural feature of the mixed-instruction design.').

omega_variable(
    reading_wars_kernel_disaggregation,
    'Is ''balanced literacy'' a single coherent pedagogical claim, or does the label bundle a genuinely defensible position (reading has multiple components) with a specific, empirically contested implementation choice (three-cueing systems, incidental rather than systematic phonics) that could be separated without abandoning the pluralistic framing?',
    'Track districts and publishers that explicitly rebrand as ''balanced literacy'' while removing three-cueing and adding systematic phonics scope-and-sequence (a growing post-2019 pattern); if outcomes for decoding-impaired students improve substantially while the framework retains its meaning-making/authentic-literature components, this establishes the extraction as located specifically in the historically incidental phonics implementation, not in balance per se.',
    'If separable, a revised balanced-literacy reading (systematic phonics + authentic literature, no three-cueing) could be a genuinely lower-ε constraint, distinct from the reading authored here; if inseparable, the extraction is intrinsic to any framework claiming both decoding and meaning-making are equally weighted without a systematic phonics backbone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_wars_kernel_disaggregation, conceptual, 'Whether the extractive component of this reading is separable from its coordination function via implementation reform, or intrinsic to the balanced-literacy premise itself.').

omega_variable(
    beneficiary_capture_vs_genuine_belief,
    'Do teacher colleges and curriculum publishers maintain balanced literacy primarily because of genuine pedagogical conviction (the identity-lock is sincere and evidence-responsive) or because of institutional and commercial self-interest (the identity-lock is a rationalization protecting sunk costs)?',
    'Examine whether teacher colleges and publishers that have been shown the National Reading Panel and subsequent meta-analytic evidence subsequently revise curricula substantially, or whether they primarily engage in rebranding (retaining substance, changing labels) — the theater_ratio trend in this story''s measurements is one operationalization of this question.',
    'If genuine belief with slow but real evidence-responsiveness, the classification should trend toward rope over time as the framework self-corrects; if primarily rebranding, the classification should trend toward piton or entrenched tangled_rope as the theater ratio continues rising while structural instruction remains unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_belief, conceptual, 'Whether institutional beneficiaries'' continued advocacy reflects sincere conviction or self-interested rationalization, and what that implies for the framework''s trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 6, 0.3).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 12, 0.36).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 18, 0.41).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 24, 0.45).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.47).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(read_be_t6, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(read_be_t18, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(read_su_t6, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 6, 0.26).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(read_su_t18, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 18, 0.34).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of (at least) three sibling readings decomposing the natural-language 'reading wars' / balanced-literacy-vs-phonics debate per the epsilon-invariance principle: phonics_decoding_primacy (reading IS decoding; extraction concentrated on students needing rich comprehension instruction under a narrow decode-first regime), whole_language_meaning_primacy (reading IS meaning-making; extraction concentrated on decoding-impaired students under a no-explicit-phonics regime), and structured_literacy_remediation (legitimacy determined by designing for the most vulnerable learners first; likely rope-leaning from the vantage of the population this reading and whole_language both underserve). Each sibling carries its own epsilon, beneficiary/victim structure, and claimed_type; this reading (balanced_literacy_integration) authors moderate tangled-rope extraction concentrated on decoding-impaired and dyslexic children via dilution rather than neglect or narrow scope. The reading_relations above mark whole_language_meaning_primacy as influenced by (not foreclosed by) this reading, since the historical emergence of balanced literacy was substantially a response to whole language's documented decoding failures — it created downstream legitimacy pressure on pure whole-language claims without making them logically impossible to hold. phonics_decoding_primacy coexists as an ongoing live alternative position, particularly strengthened by post-2019 'science of reading' legislative advocacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
