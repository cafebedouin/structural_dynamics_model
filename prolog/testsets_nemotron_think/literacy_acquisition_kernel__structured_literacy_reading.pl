% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Instructional Framework (Orton-Gillingham Tradition)
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   Structured literacy — the Orton-Gillingham tradition codified into a
 *   universal instructional framework — mandates explicit, systematic,
 *   cumulative instruction across five pillars (phonological awareness,
 *   phonics, fluency, vocabulary, comprehension). Originally a clinical
 *   intervention for dyslexia, it has been scaled through state legislation
 *   (40+ states with dyslexia laws, 30+ with 'science of reading' mandates)
 *   into a universal Tier 1 requirement. The constraint extracts heavily from
 *   general education teachers (specialized certification, 60-120+ hours
 *   training, fidelity monitoring) while delivering the lowest extraction to
 *   its intended beneficiaries (dyslexic students, for whom the intervention
 *   reduces failure). The claimed type is tangled_rope: genuine coordination
 *   (solving the 'instructional lottery' problem) with asymmetric extraction
 *   (teacher burden) requiring active enforcement (certification, fidelity,
 *   legislative mandate). The claim/metric gap is deliberate: proponents
 *   frame it as pure coordination (rope); the metrics reveal substantial
 *   extraction on teachers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.62).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Instructional Framework (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '9622b2ee-34a9-426c-9b55-053787786193').
narrative_ontology:cs_kernel_codification('9622b2ee-34a9-426c-9b55-053787786193', formalized).
narrative_ontology:cs_authority_grounding('9622b2ee-34a9-426c-9b55-053787786193', expertise).
narrative_ontology:cs_interpretation_layer_present('9622b2ee-34a9-426c-9b55-053787786193').
narrative_ontology:cs_reading_relation('9622b2ee-34a9-426c-9b55-053787786193', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('9622b2ee-34a9-426c-9b55-053787786193', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('9622b2ee-34a9-426c-9b55-053787786193', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('9622b2ee-34a9-426c-9b55-053787786193', foundational, explicit_systematic_cumulative_instruction_required).
narrative_ontology:cs_axiom_status(explicit_systematic_cumulative_instruction_required, holdable).
narrative_ontology:cs_axiom_grounding('9622b2ee-34a9-426c-9b55-053787786193', explicit_systematic_cumulative_instruction_required, empirically_contingent).
narrative_ontology:cs_axiom('9622b2ee-34a9-426c-9b55-053787786193', secondary, dyslexia_intervention_universally_applicable).
narrative_ontology:cs_axiom_status(dyslexia_intervention_universally_applicable, holdable).
narrative_ontology:cs_axiom_grounding('9622b2ee-34a9-426c-9b55-053787786193', dyslexia_intervention_universally_applicable, empirically_contingent).
narrative_ontology:cs_reference_frame('9622b2ee-34a9-426c-9b55-053787786193', orton_gillingham_structured_literacy_framework).
narrative_ontology:cs_drift_state('9622b2ee-34a9-426c-9b55-053787786193', contemporary_science_of_reading_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9622b2ee-34a9-426c-9b55-053787786193', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_general).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, teacher_preparation_programs).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, explicit_systematic_instruction_necessity).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, cumulative_skill_building_principle).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, phonological_awareness_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive explicit, systematic, cumulative instruction in all five pillars (PA, phonics, fluency, vocabulary, comprehension) through specialized intervention. Without this instruction, they face persistent reading failure. They cannot exit the public education system and have no alternative instructional pathway if their school does not provide structured literacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Students without dyslexia diagnosis but with reading difficulties who benefit when structured literacy is implemented school-wide. They gain access to the same systematic instruction but may not receive the intensive dosage that diagnosed students get. Their families may advocate for but cannot guarantee access.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_general, beneficiary,
    powerless, biographical, constrained, national).

% Bear the primary training burden: required to complete extensive coursework (60-120+ hours), practicum supervision, and certification exams to teach structured literacy. Must restructure daily instructional routines, create explicit lesson plans, and maintain fidelity logs. Exit options: leave teaching, move to non-mandate states, or comply. Certification costs (time, money, opportunity) are substantial and not fully compensated.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Already trained in specialized methods; structured literacy mandates validate their expertise and create career advancement (literacy specialist roles, higher pay). But they also bear recertification costs and must align existing practice with new fidelity standards. Their specialized certification becomes a market advantage.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers, payer).

% Design and enforce implementation: write district literacy plans, conduct fidelity observations, gatekeep certification. They benefit professionally (expanded roles, authority) and ideologically (see their framework adopted). They can move between districts, states, and private consulting. Their professional identity is fused with the framework.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, literacy_coaches_specialists, agenda_setter,
    institutional, generational, arbitrage, national).

% Must redesign entire reading methods curricula, hire faculty with structured literacy credentials, create practicum placements with certified mentors, and seek program accreditation from IDA/state. Costs are high (faculty retraining, new partnerships, lost enrollment during transition). They cannot exit the mandate without losing state approval.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, teacher_preparation_programs, payer,
    organized, generational, constrained, national).

% Mandate structured literacy through legislation (dyslexia laws, science of reading acts), licensure rules, and curriculum adoption lists. They bear political costs of implementation but gain legitimacy from alignment with cognitive science consensus. They set the enforcement timeline and define 'fidelity.'
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_education_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Drove the legislative mandates but are excluded from implementation decisions (curriculum selection, fidelity criteria, teacher assignment). They would object to watered-down implementation, inadequate dosage, and lack of progress monitoring. Their children remain in classrooms where the constraint is nominally adopted but not faithfully enacted.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_dyslexia_advocates, excluded,
    organized, biographical, trapped, national).

% Teachers and faculty committed to balanced literacy (workshop model, guided reading, three-cueing). They are structurally displaced by mandates that deem their methods insufficient. They would argue for professional autonomy and complementary approaches. Some adapt; others resist or leave. Their exclusion is the enforcement mechanism.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_practitioners, excluded,
    organized, biographical, constrained, national).

% Provide the empirical warrant (converging evidence on PA, phonics, fluency, vocabulary, comprehension). They do not control implementation but their consensus legitimizes the mandate. They debate specifics (dosage, sequence, population) but agree on the five pillars. Their authority is epistemic, not institutional.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, cognitive_scientists_reading_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of ensuring all children receive evidence-based reading instruction: without a mandated framework, instructional quality varies wildly by teacher preparation, curriculum adoption, and school leadership, leaving the most vulnerable students to chance.
% TRANSFER_FUNCTION: Moves instructional time, teacher knowledge, and certification costs from general education teachers and preparation programs toward structured literacy implementation (materials, coaching, specialist roles). Transfers reading outcomes from chance to systematic acquisition for dyslexic and struggling readers.
% ABSENT_VOICES: Parents of dyslexic children who drove the mandates are excluded from implementation fidelity decisions. Balanced literacy practitioners are displaced without a structured transition pathway. Students in schools that adopt the label but not the practice are invisible in the data.
% DISAPPEARANCE_RATIONALE: If structured literacy mandates vanished overnight, teacher preparation programs would revert to balanced literacy curricula within 2-3 years, certification requirements would lapse, coaching positions would be cut, and dyslexic students would lose their statutory right to evidence-based intervention. The instructional infrastructure would collapse back to local control.
% FOUNDING_PROBLEM: Mid-20th century reading instruction (look-say, whole word) failed dyslexic children systematically. Orton-Gillingham developed a multisensory, structured alternative for clinical settings. The founding problem: how to scale a clinical intervention for dyslexia into a universal Tier 1 instructional framework without losing fidelity.
% FOUNDING_PROBLEM_CORROBORATION: The International Dyslexia Association (benefiting party) attests the problem is live — most teachers still lack adequate training. The National Reading Panel (2000, external scientific body) corroborated the five pillars but did not mandate a single framework. Cognitive scientists (external) confirm the instructional principles but debate whether 'structured literacy' as branded is the only valid instantiation. State legislators (mixed) cite both the ongoing problem and the solution's enactment.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) reflects the training/certification burden on teachers — high but not maximal because the knowledge transfers to practice. Suppression (0.62) is higher: three-cueing materials are removed, balanced literacy PD is defunded, licensure is tied to structured literacy competencies. Theater ratio (0.32) captures performative compliance: schools adopt 'science of reading' curricula but lack certified coaches, dosage is insufficient, progress monitoring is checkbox. Accessibility collapse (0.42) is moderate: balanced literacy persists in non-mandate states, private schools, and as 'supplement' in mandate states. Resistance (0.58) comes from teacher prep programs, unions (workload), and balanced literacy advocates. All metrics on shared time grid (0,5,10,15,20 years post-NRP 2000).
 *
 * PERSPECTIVAL GAP:
 *   From the coach/state seat, this is a rope: a coordination solution finally enacted at scale. From the general education teacher seat, it is a snare: a mandate that extracts labor without commensurate support. From the dyslexic student seat, it is a mountain (if implemented): the instruction is cognitively necessary. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   General education teachers are structural targets (d ~0.85): they pay the certification cost, cannot easily exit, and the constraint's persistence depends on their compliance. Special education teachers are partial beneficiaries (d ~0.35): they gain professional distinction but bear recertification. Literacy coaches and state agencies are agenda-setters/beneficiaries (d ~0.15): they gain authority and resources. Students with dyslexia are beneficiaries with trapped exit (d ~0.10): they receive the intervention but cannot choose it. Parents/advocates are excluded (d undefined): they drove the mandate but control no implementation levers. Balanced literacy practitioners are excluded/constrained (d ~0.75): their methods are actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scaling dyslexia intervention to universal Tier 1) remains contested: proponents say most teachers still lack training (problem live); critics say the mandate has become a certification industry disconnected from student outcomes (problem dead, arrangement persists). The corridor between live and dead is where the extraction accumulates — each year of 'implementation' adds training requirements without proportional student gains. Mandatrophy is unresolved: the constraint's coordination function is real but its enforcement machinery has grown beyond what the founding problem justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_framework,
    'Is the five-pillar structured literacy framework a cognitive necessity (mountain) — the only way the human brain learns to read — or a pedagogical choice among viable alternatives?',
    'Cross-linguistic and cross-cultural studies: if all orthographies require the same five pillars taught explicitly/cumulatively, it trends mountain; if transparent orthographies succeed with less explicit instruction, it is constructed.',
    'If mountain, the constraint''s extraction on teachers is the price of cognitive reality (like gravity). If constructed, the extraction is a policy choice that could be redistributed (e.g., better curricula reducing teacher burden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framework, empirical, 'Whether structured literacy reflects invariant cognitive architecture or contingent instructional design.').

omega_variable(
    phonics_reading_variant_distinction,
    'Is structured_literacy_reading substantively distinct from phonics_reading, or is it a branded variant that adds marginal requirements (fluency, vocabulary, comprehension, cumulative) to the same core?',
    'Meta-analysis of effect sizes: if adding the four non-phonics pillars to systematic phonics yields statistically and practically significant gains for general populations, the distinction is real. If gains are negligible, it is a variant.',
    'If variant, the extraction on teachers (training in all five pillars) includes substantial waste. If distinct, the full framework is necessary and the training burden is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonics_reading_variant_distinction, empirical, 'Whether the structured literacy brand adds value beyond systematic phonics.').

omega_variable(
    universal_applicability_claim,
    'Does the dyslexia intervention (intensive, multisensory, diagnostic) actually benefit typically developing readers equally, or does universal application impose unnecessary intensity on children who need less?',
    'RCTs comparing structured literacy Tier 1 vs. enhanced balanced literacy for general populations, measuring both outcomes and instructional efficiency (gains per hour).',
    'If universal benefit is false, the constraint extracts from general education teachers and typical students (instructional time opportunity cost) without proportional gain — shifting classification toward snare for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_applicability_claim, empirical, 'Whether the ''designed for dyslexia, applicable universally'' claim holds for typical readers.').

omega_variable(
    fidelity_measurement_validity,
    'Do current fidelity instruments (observation rubrics, lesson plan reviews, student outcome thresholds) validly distinguish faithful implementation from performative compliance?',
    'Predictive validity studies: correlate fidelity scores with student growth on standardized reading measures, controlling for demographics and dosage.',
    'If fidelity measures are invalid, the enforcement machinery (suppression) is decoupled from the coordination function — theater ratio is underestimated and the constraint trends piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_measurement_validity, empirical, 'Whether enforcement targets real implementation or ceremonial adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(literacy_acquisition_kernel__structured_literacy_reading_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_preparation_reform_mandates).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_curriculum_adoption_cycles).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_screening_universal_mandates).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, reading_coach_funding_allocations).

% DUAL FORMULATION NOTE:
% Part of the literacy_acquisition_kernel family. This reading (structured_literacy) is the most comprehensive and enforcement-heavy. It forecloses whole_language_reading, influences phonics_reading (expansion pressure), and influences balanced_literacy_reading (systematicity pressure). All four readings share the kernel 'how children learn to read' but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, organized, 0.35).
constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, powerless, 0.1).
constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
