% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured-Literacy Instructional Legitimacy Standard (Vulnerability-First Reading)
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Since the National Reading Panel synthesis (2000) and accelerating
 *   through the 2019-2025 wave of state literacy statutes, structured
 *   literacy has become the operative legitimacy standard for reading
 *   instruction across a growing majority of US jurisdictions: instruction
 *   must be explicit, cumulative, and diagnostic, sequenced for the most
 *   vulnerable learners first, with universal screening and
 *   intervention-grade instruction delivered preventatively to all students.
 *   The constraint operates through statutes, approved-curriculum lists,
 *   teacher-preparation accreditation, funded training pipelines, and
 *   fidelity monitoring. This file instantiates ONE reading of the contested
 *   kernel reading_acquisition_legitimacy — the
 *   structured_literacy_remediation reading — as a clean epsilon-invariant
 *   constraint; the three sibling readings (phonics_decoding_primacy,
 *   whole_language_meaning_primacy, balanced_literacy_integration) are
 *   separate constraints with their own epsilon values, victim sets, and
 *   classifications, linked through network.affects_constraints. Family
 *   decomposition per the epsilon-invariance principle: the colloquial label
 *   'the science of reading' spans structurally distinct claims — a narrow
 *   decoding-primacy claim (lower epsilon, thinner expenditure surface), a
 *   meaning-primacy claim (different victim set: code-trained teachers under
 *   immersion regimes), a balance claim, and this vulnerability-first design
 *   claim (broadest mandate surface, hence the widest extraction channel).
 *   Claim/metric independence is preserved: claimed_type records this
 *   reading's structural assessment (tangled_rope — genuine coordination
 *   function with asymmetric extraction); the metrics describe the operating
 *   regime as descriptively as this seat can render it.
 *
 * KEY AGENTS:
 *   - - dyslexic_students: primary intended beneficiary (powerless/trapped) — receives the standard's protections but cannot enforce them
 *   - - structured_literacy_vendors: commercial beneficiary (organized/arbitrage) — converts mandates into procurement
 *   - - private_dyslexia_tutoring_industry: secondary beneficiary (organized/arbitrage)
 *   - - parents_of_struggling_readers: mobilized beneficiary-advocates (organized/constrained)
 *   - - state_education_agencies: agenda setter (institutional/constrained) — drafts, enforces, monitors
 *   - - school_district_administrators: payer-implementer (organized/constrained)
 *   - - early_career_teachers: payer with embedded benefit (moderate/constrained)
 *   - - veteran_constructivist_teachers: primary payer, identity-locked exit (moderate/identity_locked)
 *   - - teacher_preparation_faculties: institutional payer (institutional/constrained)
 *   - - independent_literacy_researchers: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.5).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured-Literacy Instructional Legitimacy Standard (Vulnerability-First Reading)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '64a3180c-271c-4922-8159-fc18c371b4e7').
narrative_ontology:cs_kernel_codification('64a3180c-271c-4922-8159-fc18c371b4e7', formalized).
narrative_ontology:cs_authority_grounding('64a3180c-271c-4922-8159-fc18c371b4e7', expertise).
narrative_ontology:cs_interpretation_layer_present('64a3180c-271c-4922-8159-fc18c371b4e7').
narrative_ontology:cs_reading_relation('64a3180c-271c-4922-8159-fc18c371b4e7', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('64a3180c-271c-4922-8159-fc18c371b4e7', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('64a3180c-271c-4922-8159-fc18c371b4e7', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_axiom('64a3180c-271c-4922-8159-fc18c371b4e7', foundational, most_vulnerable_learners_design_reference).
narrative_ontology:cs_axiom_status(most_vulnerable_learners_design_reference, holdable).
narrative_ontology:cs_axiom_grounding('64a3180c-271c-4922-8159-fc18c371b4e7', most_vulnerable_learners_design_reference, deontological).
narrative_ontology:cs_axiom('64a3180c-271c-4922-8159-fc18c371b4e7', foundational, continuous_diagnostic_instruction_necessity).
narrative_ontology:cs_axiom_status(continuous_diagnostic_instruction_necessity, holdable).
narrative_ontology:cs_axiom_grounding('64a3180c-271c-4922-8159-fc18c371b4e7', continuous_diagnostic_instruction_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('64a3180c-271c-4922-8159-fc18c371b4e7', vulnerability_first_structured_standard).
narrative_ontology:cs_drift_state('64a3180c-271c-4922-8159-fc18c371b4e7', contemporary_post_mandate_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64a3180c-271c-4922-8159-fc18c371b4e7', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, general_student_population).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, private_dyslexia_tutoring_industry).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, early_career_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, veteran_constructivist_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_district_administrators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_faculties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, early_career_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, simple_view_of_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, scarborough_reading_rope).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, ehri_phases_of_word_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, national_reading_panel_phonics_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Students whose acquisition of written language diverges from typical trajectories. Under this standard they are screened early, taught with explicit sequential code instruction, and regrouped by diagnostic data instead of waiting for failure. They cannot choose their school's method; their outcome depends almost entirely on which legitimacy standard their jurisdiction adopts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_students, beneficiary,
    powerless, biographical, trapped, national).

% All students in mandated grades receive the same explicit, cumulative instruction preventatively. Most reach proficiency under it; a minority of already-fluent or fast-moving readers experience slower, more uniform pacing than literature-rich alternatives would offer — a trade this reading accepts as the price of guaranteeing the floor.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_student_population, beneficiary,
    powerless, biographical, trapped, national).

% Organized parent advocates who testified for the statutes, sit on adoption committees, and monitor implementation. They gain a guaranteed instructional floor for their children but still frequently purchase private tutoring when school delivery lags the standard.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% Publishers and platforms selling curricula, screeners, and training aligned to the standard. Mandates convert their products from optional purchases into compliance requirements; several have rebranded existing materials under the new banner. Exit is easy — product lines pivot to whatever the next standard names.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Certified tutors and clinic networks charging rates most families cannot sustain. The standard's universal tier trims their remedial caseload at the margin but raises demand for certified specialists as schools staff intervention blocks.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, private_dyslexia_tutoring_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Draft and enforce the literacy statutes, approve curricula, fund training, publish approved lists, and monitor fidelity. Their remit and staffing expand with the standard; they also absorb the political risk when implementation disappoints.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Purchase approved programs, schedule screening windows, staff intervention blocks, and report compliance data upward. Budget share shifts toward procurement and away from other uses; deviation from the approved list invites state findings.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_district_administrators, payer,
    organized, biographical, constrained, regional).

% Prepared under accreditation rules that now require the reading science; the standard's methods are their native training. They bear the workload of screening and regrouping but experience little identity conflict, and their skills transfer across districts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, early_career_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, early_career_teachers, beneficiary).

% Trained in meaning-first methods that the standard now names illegitimate. They must retrain, abandon familiar routines, and teach from sequences they did not choose; leaving the profession or the grade band is costly, and their professional self-concept is bound to the displaced methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, veteran_constructivist_teachers, payer,
    moderate, biographical, identity_locked, national).

% Accreditation and state approval now condition program continuity on syllabus overhaul. Faculty formed in the prior paradigm must re-teach themselves; some programs gain enrollment by marketing alignment, others lose instructors who resign rather than retrain.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_faculties, payer,
    institutional, generational, constrained, national).

% University-based researchers who produced much of the underlying evidence and now audit implementation: which programs match the trials, where mandates outrun findings, what universal rollout does to advanced readers. They hold no procurement stake and publish regardless of adoption politics.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, independent_literacy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns thousands of classrooms, preparation programs, procurement offices, and screening schedules on one instructional standard so that a vulnerable learner's outcome stops depending on which teacher or method they happen to get; solves the collective-action problem in which individual schools under-invest in code knowledge that pays off most visibly for the students least able to advocate for themselves.
% TRANSFER_FUNCTION: Moves district budget share from discretionary purchases to approved-program procurement and training contracts (toward vendors and providers); moves instructional authority from teacher judgment to state scope-and-sequence documents and fidelity instruments; moves classroom time from extended literature exploration toward code instruction and intervention blocks; and moves effective instruction toward students previously served by luck.
% ABSENT_VOICES: Whole-language and balanced-literacy adherents are progressively absent from adoption committees and journal gatekeeping; parents of already-fluent readers asking for literature-rich acceleration have no seat in screening-window design; and students — the people instructed — appear only as data points. Their objections currently enter through litigation, press coverage, and the sibling readings' separate files, not through the standard's own procedures.
% DISAPPEARANCE_RATIONALE: Procurement contracts, accreditation standards, screening calendars, intervention staffing, and retraining investments all reference the standard; overnight removal would return instruction to jurisdictional and building-level variation within a year, reopen the method dispute the statutes closed, and strand recently retrained teachers between paradigms — while vulnerable students' protection would again depend on local luck.
% FOUNDING_PROBLEM: Under whole-language dominance (roughly 1985-2000), about a third of students failed to reach proficiency, teacher preparation rarely taught the alphabetic principle, dyslexic students were identified late or never, and remediation was rationed by placement luck rather than by need.
% FOUNDING_PROBLEM_CORROBORATION: NAEP proficiency gaps, late-identification and special-education referral data, and university replications of the phonics-effect meta-analyses attest the problem's persistence from outside the benefiting parties; parent-advocacy testimony corroborates the lived form. No vendor or agency attestation is needed to establish it — though vendors predictably amplify it.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.5, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon (0.50) is authored from this reading's own seat over the standing arrangement — the operating structured-literacy regime of statutes, adoptions, screening schedules, and procurement — not over the principle in the abstract. The reading itself distinguishes the standard from 'SOR-washing': repackaged materials, checkbox training hours, fidelity paperwork. Because the referent is the operating regime, the reading's honest assessment carries real extraction (procurement capture, compliance burden, autonomy transfer) alongside the function the regime performs; it is not zero, and it is not tuned to any predicted output. Suppression (0.58) is a raw structural property, unscaled by power or scope: cueing-practice bans, approved-list gates, accreditation conditions, and fidelity monitoring are coercive machinery the regime actively maintains. Theater ratio (0.33) reflects a functional core — real screening, real instructional change, measurable early-cohort gains — wrapped in growing performative compliance. Accessibility collapse (0.42): alternatives do not vanish — three sibling readings remain live and defended — but within the standard's own terms they are pre-classified as illegitimate, which collapses deliberation without eliminating the practices. Resistance (0.57): organized teacher pushback, scholarly dissent over universal extrapolation, and holdout jurisdictions. Seat divergence: the vendor seat computes near-pure coordination-plus-subsidy; the identity-locked veteran-teacher seat computes near-full extraction; the dyslexic-student seat computes deep benefit it cannot itself enforce. Same-level lateral differentiation: early-career and veteran teachers hold identical power atoms but different exit options (constrained vs identity_locked), so the engine derives different directionalities from identical formal standing. Cyclical note: the monotonic series captures one ascending half-cycle of the longer reading-wars pendulum; within-interval oscillation is absent, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The same statute reads as liberation from the dyslexic family's seat, as overdue correction from the researcher's seat, as market creation from the vendor's seat, and as professional delegitimation from the veteran constructivist teacher's seat. Identity-lock dynamics: the veteran teacher's fusion is professional-identity type — career-long craft judgment constituted under meaning-first training; mandates arrive as a verdict on that biography rather than as new information. If the identity frame broke (successful retraining producing new mastery), that seat would recompute toward the early-career profile: constrained exit, moderate extraction, partial benefit. Inter-institutional dynamics: state agencies and preparation faculties share the institutional power atom but face opposite incentive gradients — agencies expand remit with enforcement while faculties absorb retraining costs — so identical formal standing yields opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward 0: vendors and tutors (arbitrage exit amplifies the subsidy side), dyslexic students and parents (deep benefit, no enforcement lever), the general student population (diffuse benefit). Victim declarations drive d toward 1: veteran teachers (identity_locked pushes toward the full-target end), early-career teachers (constrained), districts and preparation faculties (constrained institutional payers). The agenda_setter seat (state agencies) collects administrative remit — a mild beneficiary tilt — while bearing political risk; the derivation handles it from the power atom without an override. No directionality_overrides are authored: every seat's derived d matches its structural position, and the default discipline is to trust the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whole-language-era reading failure concentrated among the most vulnerable, with teacher preparation blind to the alphabetic principle — remains live by external attestation (NAEP proficiency gaps, late-identification data), so no mandatrophy is declared. The classification prevents two symmetrical mislabels: reading the regime as a snare erases the documented gains of explicitly served dyslexic cohorts; reading it as a pure rope erases the procurement capture and autonomy transfer the receipts record. Tangled rope holds both halves. Drift watch: theater_ratio's steady climb (0.12 to 0.33) is the leading indicator of piton transition — if the diagnostic core atrophies into compliance paperwork while branding persists, the regime becomes maintained performance. The mismatch consumer should find status=live plus world_rearranges consistent today, with the theater trajectory as the early warning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel reading_acquisition_legitimacy — the structured_literacy_remediation reading. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Corpus-level comparison across the four sibling files: victim sets, epsilon values, and mandate surfaces should differ systematically by design-reference point (weakest-first vs average-first vs meaning-first) and by the explicitness requirement; the disagreement lives in those two parameters, not in the underlying evidence base itself.',
    'If the kernel were instead treated as settled under a single legitimate reading, this constraint''s extraction profile would be dismissed as implementation noise; keeping the readings as distinct constraints preserves the legitimacy contest itself as measurable structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of four readings of the reading-acquisition-legitimacy kernel.').

omega_variable(
    universal_preventative_extrapolation,
    'Does the trial evidence for explicit, cumulative instruction with struggling and dyslexic learners extend to universal intervention-grade instruction for all students, including those who would reach proficiency under lighter scaffolding?',
    'Randomized or well-matched comparisons of universal-tier versus tiered-delivery designs tracking advanced-reader outcomes, instructional-time allocation, and full proficiency distributions.',
    'If the extrapolation fails, the universal-mandate component extracts instructional time beyond what the evidence supports, raising effective extraction on the general student population and pushing that segment toward snare-flavored operation; if it holds, the universal tier is coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_preventative_extrapolation, empirical, 'Whether remedial-population evidence licenses universal preventative mandates.').

omega_variable(
    vendor_capture_contingency,
    'How much of the measured extraction is intrinsic to a vulnerability-first legitimacy standard, and how much is contingent on proprietary procurement markets capturing the mandate?',
    'Compare jurisdictions mandating open-source or state-built curricula against proprietary-procurement jurisdictions on cost, fidelity, and outcomes; intrinsic extraction should appear in both, capture-driven extraction only in the latter.',
    'If most extraction is capture-contingent, the tangled-rope reading softens toward rope-with-pathology and remedies target procurement rules; if intrinsic, the standard itself carries the extraction and the classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_contingency, empirical, 'Intrinsic versus capture-contingent share of the regime''s extraction.').

omega_variable(
    teacher_resistance_internalization,
    'Is the measured teacher-side suppression structural (mandates removing discretion, retraining burdens, fidelity surveillance) or internalized (professional shame and identity threat persisting after retraining)?',
    'Post-retraining trajectory studies: teachers who complete high-quality retraining and report restored efficacy indicate a large internalized component that decays with new identity formation; persistent dissatisfaction after demonstrated mastery indicates structural suppression.',
    'If internalized, effective suppression exceeds the structural measure and outlasts any statutory relaxation — the veteran seat stays identity_locked even under lenient regimes; if structural, policy fixes such as waivers and co-design would release it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_resistance_internalization, empirical, 'Structural versus internalized share of teacher-side suppression.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the commitment-system kernel the codified research canon (fixed syntheses and statutes treated as authoritative text) or the living interpretive tradition of ''the science of reading'' as practiced by trainers, coaches, and adoption committees?',
    'Trace adjudication: when practice conflicts with the canon, which wins — citation to the syntheses, or the trainer community''s rendering? A tradition that overrides citations reveals the practice-tradition framing; canonical supremacy reveals the fixed-text framing.',
    'Under the canon-as-kernel framing the system resembles a formalized, expertise-grounded standard; under the tradition-as-kernel framing authority is practice-grounded and drift absorbs silently through training materials — changing the interpretation-layer verdict and the drift accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent framings of the kernel yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slr_legitimacy_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t0, observed).
narrative_ontology:measurement(slr_legitimacy_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t5, observed).
narrative_ontology:measurement(slr_legitimacy_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t10, observed).
narrative_ontology:measurement(slr_legitimacy_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t15, observed).
narrative_ontology:measurement(slr_legitimacy_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t20, observed).
narrative_ontology:measurement(slr_legitimacy_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(slr_legitimacy_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(slr_legitimacy_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(slr_legitimacy_be_t0, observed).
narrative_ontology:measurement(slr_legitimacy_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.33).
narrative_ontology:measurement_basis(slr_legitimacy_be_t5, observed).
narrative_ontology:measurement(slr_legitimacy_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(slr_legitimacy_be_t10, observed).
narrative_ontology:measurement(slr_legitimacy_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(slr_legitimacy_be_t15, observed).
narrative_ontology:measurement(slr_legitimacy_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(slr_legitimacy_be_t20, observed).
narrative_ontology:measurement(slr_legitimacy_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.5).
narrative_ontology:measurement_basis(slr_legitimacy_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(slr_legitimacy_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(slr_legitimacy_su_t0, observed).
narrative_ontology:measurement(slr_legitimacy_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(slr_legitimacy_su_t5, observed).
narrative_ontology:measurement(slr_legitimacy_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(slr_legitimacy_su_t10, observed).
narrative_ontology:measurement(slr_legitimacy_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(slr_legitimacy_su_t15, observed).
narrative_ontology:measurement(slr_legitimacy_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(slr_legitimacy_su_t20, observed).
narrative_ontology:measurement(slr_legitimacy_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(slr_legitimacy_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the science of reading' decomposes, per the epsilon-invariance principle, into four structurally distinct legitimacy claims. This file instantiates structured_literacy_remediation (vulnerability-first design; broadest mandate surface; epsilon ~0.50 with vendor-capture extraction). phonics_decoding_primacy is the upstream sibling — older, narrower, higher empirical confidence — whose claim this reading subsumes and cites as warrant (upstream influences downstream). whole_language_meaning_primacy and balanced_literacy_integration carry different victim sets (code-trained teachers under immersion regimes; struggling readers under unremediated classrooms) and different epsilon profiles. Each sibling file should carry a reciprocal note linking back to this constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
