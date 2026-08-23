% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Mandated Systematic Phonics as Foundational Reading Instruction
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   The 'science of reading' movement has translated cognitive science
 *   consensus on the necessity of systematic phonics into state-level
 *   mandates (38+ states as of 2024). The constraint is the requirement that
 *   all early reading instruction follow an explicit, sequential
 *   scope-and-sequence for grapheme-phoneme correspondence. It is claimed as
 *   a rope (pure coordination for literacy equity) but operates as a tangled
 *   rope: genuine coordination function (preventing reading failure for
 *   at-risk children) coexists with asymmetric extraction (teacher autonomy
 *   narrowed, advanced readers over-instructed, publisher markets
 *   guaranteed). The structural delta is real: high upfront cost (training,
 *   materials, pacing) pays off in lower long-term remediation, but the
 *   distribution of costs and benefits is uneven.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Mandated Systematic Phonics as Foundational Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'cb3209b6-7731-4d51-9625-8d1ddea3844f').
narrative_ontology:cs_kernel_codification('cb3209b6-7731-4d51-9625-8d1ddea3844f', distributed).
narrative_ontology:cs_authority_grounding('cb3209b6-7731-4d51-9625-8d1ddea3844f', expertise).
narrative_ontology:cs_interpretation_layer_present('cb3209b6-7731-4d51-9625-8d1ddea3844f').
narrative_ontology:cs_reading_relation('cb3209b6-7731-4d51-9625-8d1ddea3844f', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('cb3209b6-7731-4d51-9625-8d1ddea3844f', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('cb3209b6-7731-4d51-9625-8d1ddea3844f', foundational, explicit_systematic_phonics_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_phonics_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cb3209b6-7731-4d51-9625-8d1ddea3844f', explicit_systematic_phonics_necessary, empirically_contingent).
narrative_ontology:cs_axiom('cb3209b6-7731-4d51-9625-8d1ddea3844f', foundational, decoding_foundation_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_foundation_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('cb3209b6-7731-4d51-9625-8d1ddea3844f', decoding_foundation_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_reference_frame('cb3209b6-7731-4d51-9625-8d1ddea3844f', alphabetic_principle_primacy).
narrative_ontology:cs_drift_state('cb3209b6-7731-4d51-9625-8d1ddea3844f', post_national_reading_panel, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cb3209b6-7731-4d51-9625-8d1ddea3844f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, literacy_specialists).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, advanced_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, alphabetic_principle_primacy).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, decoding_foundation_precedes_comprehension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children at risk of reading failure (dyslexia, low SES, ELL) who depend on explicit decoding instruction to access text. Without systematic phonics, they fall behind irreversibly. They cannot exit the school system and have no voice in curriculum decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Lose instructional autonomy as mandated scope-and-sequence replaces professional judgment. Gain reduced remediation burden long-term. Compliance monitored through fidelity checks, coaching, and evaluation. Exit means leaving profession or moving to non-mandated settings (private, homeschool).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, classroom_teachers, beneficiary).

% Children who would acquire decoding implicitly receive explicit instruction they don't need, consuming instructional time that could go to comprehension, vocabulary, or enrichment. No ability to opt out of whole-class phonics blocks.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, advanced_readers, payer,
    powerless, biographical, trapped, national).

% Reading coaches, interventionists, and specialists see expanded roles, funding, and professional status under systematic phonics mandates. Their expertise becomes legally required. They can move between districts and states as mandates spread.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_specialists, beneficiary,
    organized, generational, mobile, national).

% Produce and sell mandated scope-and-sequence programs, decodable texts, and assessment systems. State adoption lists create guaranteed markets. They lobby for mandates and fund professional development that locks in their products.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Teachers and scholars committed to meaning-centered, literature-rich approaches. Their methods are delegitimized by policy; professional organizations marginalized. They persist in pockets (private schools, university programs) but cannot influence mainstream mandates.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners, excluded,
    organized, biographical, constrained, national).

% Researchers studying reading acquisition (neuroscience, psychology, linguistics). Provide evidence base for phonics necessity but do not set policy. Their findings are cited by all sides; they observe the policy translation of their work.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, cognitive_scientists, observer,
    analytical, civilizational, analytical, universal).

% State legislators, boards of education, and district leaders who enact phonics mandates (e.g., 'science of reading' laws). Respond to advocacy, test scores, and publisher lobbying. Can shift mandates but face political cost for reversal.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, education_policymakers, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures all children, especially those at risk for reading failure, acquire the alphabetic principle through systematic, explicit instruction rather than leaving decoding to chance or implicit exposure. Solves the coordination problem of guaranteeing a floor of decoding competence across a heterogeneous student population.
% TRANSFER_FUNCTION: Moves instructional time, teacher curricular discretion, and professional autonomy from classroom teachers to mandated scope-and-sequence programs; moves long-term remediation costs (special education, intervention, grade retention) down by preventing reading failure upstream; moves market share and revenue to publishers of aligned materials.
% ABSENT_VOICES: Whole language practitioners and constructivist teacher educators who argue that systematic phonics kills motivation, narrows curriculum, and ignores the social-cultural dimensions of literacy. Parents who want literature-rich, inquiry-based early literacy. These voices are structurally excluded from 'science of reading' policy coalitions.
% DISAPPEARANCE_RATIONALE: If systematic phonics mandates vanished overnight, instruction would immediately revert to mixed methods (balanced literacy, whole language) in most classrooms. Decoding outcomes for at-risk students would decline within 1-2 years; remediation referrals and special education placements would rise. Publisher revenue would collapse. The entire 'science of reading' policy infrastructure (coaches, laws, PD) would lose its mandate.
% FOUNDING_PROBLEM: The persistent 'reading wars' stalemate: whole language dominance (1980s-2000s) left 30-40% of students (disproportionately low-income, minority, ELL, dyslexic) without functional decoding skills. National Reading Panel (2000) identified systematic phonics as essential, but adoption was voluntary and incomplete. The founding problem is a coordination failure: without mandate, evidence-based practice does not reach the classrooms that need it most.
% FOUNDING_PROBLEM_CORROBORATION: National Reading Panel (2000) and subsequent meta-analyses (e.g., Torgerson et al., 2019) corroborate the decoding gap from outside the benefiting parties. However, curriculum publishers and literacy specialists (beneficiaries) also fund advocacy. Whole language proponents contest that the problem was ever 'solved' by phonics alone, citing comprehension and engagement trade-offs.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the transfer of teacher discretion and instructional time to mandated programs, plus publisher rent capture. Not higher because the coordination function (preventing illiteracy) is genuine and large. Suppression (0.62) reflects active enforcement: fidelity monitoring, coaching mandates, licensure requirements, and textbook adoption lists that exclude non-aligned materials. Theater ratio (0.32) is moderate: performative compliance (pacing guides, 'phonics first' signage) grows but core instruction is functional. Accessibility collapse (0.55): whole language alternatives are suppressed in policy but persist in practice pockets. Resistance (0.68): sustained pushback from teacher unions, constructivist faculty, and parent groups.
 *
 * PERSPECTIVAL GAP:
 *   From the struggling reader seat, the constraint is a rope (pure coordination: 'this is what I needed'). From the classroom teacher seat, it is a tangled rope (coordination + extraction: 'it works but I lost my craft'). From the whole language practitioner seat, it is a snare (pure extraction: 'my expertise was erased for publisher profit'). The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers are full beneficiaries (d ~ 0.1): constraint subsidizes their access to decoding. Classroom teachers are payers (d ~ 0.75): bear autonomy loss and compliance burden, though gain long-term remediation relief. Advanced readers are payers (d ~ 0.85): receive instruction they don't need with no exit. Literacy specialists and publishers are beneficiaries (d ~ 0.15): gain professional status and guaranteed markets. Whole language practitioners are excluded (d ~ 0.9): their methods are actively suppressed. Policymakers are agenda setters (d ~ 0.2): they extract political capital from 'literacy crisis' narrative. Cognitive scientists are observers (d = 0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate has not atrophied — it is expanding (more states, stricter laws). However, the founding problem (whole language leaving children behind) is contested as 'dead' by phonics advocates and 'live' by balanced literacy advocates who argue comprehension and engagement are now neglected. If the founding problem is dead but the mandate expands, mandatrophy looms. Currently 'contested' status prevents resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_publisher_capture,
    'Is the mandated scope-and-sequence structure genuinely necessary for coordination (ensuring all teachers deliver effective phonics), or does it primarily serve publisher rent-seeking and specialist professionalization?',
    'Natural experiment: compare outcomes in mandate states using highly scripted programs vs. teacher-developed systematic phonics. If outcomes are equivalent, the mandate''s prescriptiveness is extraction, not coordination.',
    'If prescriptiveness is unnecessary, the constraint shifts toward snare (extraction via mandated materials). If necessary, tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_publisher_capture, empirical, 'Whether the coordination function requires the specific mandated form or just the systematic principle.').

omega_variable(
    struggling_reader_heterogeneity,
    'Does systematic phonics benefit all struggling reader subtypes equally, or does it over-serve some (e.g., dyslexic) while under-serving others (e.g., language comprehension deficits)?',
    'Subgroup analysis of RCT data by reader profile (decoding-only vs. mixed vs. comprehension-only deficits). Longitudinal tracking of mandate states'' NAEP disaggregations.',
    'If benefits are narrow, the beneficiary claim ''struggling_readers'' is overbroad — extraction from non-benefiting subgroups increases. Could shift classification toward snare for those subgroups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(struggling_reader_heterogeneity, empirical, 'Whether the beneficiary group is homogeneous or the constraint extracts from subgroups it doesn''t help.').

omega_variable(
    teacher_discretion_extraction_vs_quality_control,
    'Is the reduction of teacher discretion an extractive cost borne by teachers, or a necessary quality-control mechanism that prevents instructional drift?',
    'Measure instructional quality variance in mandate vs. non-mandate settings. If variance drops without mean improvement, it''s extraction. If mean rises, it''s coordination.',
    'If quality control, teacher seat moves toward beneficiary (d decreases). If extraction, teacher seat stays payer and tangled_rope is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_discretion_extraction_vs_quality_control, conceptual, 'Whether teacher autonomy loss is a bug or a feature of the coordination mechanism.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the phonics reading''s core premise (explicit systematic instruction is NECESSARY) logically foreclose the whole language reading''s core premise (decoding emerges implicitly), or do they coexist as competing frameworks?',
    'Analyze whether any classroom or system can simultaneously hold ''phonics is necessary'' and ''phonics emerges implicitly'' as operational premises. If mutually exclusive in practice, forecloses.',
    'If forecloses, the kernel has a genuine structural split — the readings cannot be reconciled within one framework. If coexists_with, the conflict is political, not logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between this reading and the whole_language_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t1997, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2015, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2019, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2024, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t1997, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1997, 0.25).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2015, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2019, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2019, 0.53).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2024, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t1997, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2015, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2019, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2019, 0.59).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2024, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the phonics_reading of the reading_acquisition_mechanism kernel. The whole_language_reading and balanced_literacy_reading are sibling constraints with different ε, beneficiaries, and claimed types. The ε-invariance principle requires separate stories: whole language has near-zero extractiveness for its adherents but high suppression for phonics advocates; balanced literacy claims rope but may be tangled_rope in practice. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, organized, 0.2).
constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
