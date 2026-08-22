% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics Foundational-Instruction Mandate
 *   domain: educational/cognitive-science/political
 *
 * SUMMARY:
 *   This story instantiates the phonics reading of the reading-acquisition
 *   kernel as a standing institutional arrangement: since roughly 2013, a
 *   legislative and administrative movement has converted the claim that
 *   reading acquisition requires explicit, systematic instruction in
 *   grapheme-phoneme correspondence into binding policy — statutory
 *   definitions of evidence-based reading instruction, bans on cueing-based
 *   methods, funded or compelled teacher retraining, and state-approved
 *   curriculum lists concentrated in early grades. The constraint classified
 *   here is that arrangement: the requirement, backed by enforcement
 *   machinery, that systematic code instruction be the foundation of reading
 *   acquisition. Per the epsilon-invariance discipline, the sibling readings
 *   (whole_language_reading, balanced_literacy_reading) are separate
 *   constraint stories with their own epsilon values, victim sets, and
 *   enforcement profiles, linked through network.affects_constraints; nothing
 *   in this file averages across them. The claimed type and the metrics are
 *   authored independently: the type records what this reading's arrangement
 *   is structurally; the metrics record how it operationally behaves.
 *
 * KEY AGENTS:
 *   - - struggling_readers: Primary beneficiary (powerless/trapped) — children who do not induce decoding implicitly; the arrangement's intended recipients
 *   - - dyslexic_students: Primary beneficiary (powerless/trapped) — highest-stakes recipients; the sequence is the difference between literacy and cumulative failure
 *   - - parents_of_struggling_readers: Organized beneficiary (organized/constrained) — advocacy network that drafted and drove the legislative wave
 *   - - curriculum_publishers: Secondary beneficiary (institutional/arbitrage) — collects mandated program, assessment, and training revenue without running instruction
 *   - - classroom_teachers: Primary payer (organized/constrained) — bears retraining labor, fidelity documentation, and narrowed discretion; collects structure and student outcomes secondarily
 *   - - proficient_early_readers: Marginal payer (powerless/trapped) — bear a contested opportunity cost of instructional time
 *   - - state_education_agencies: Agenda setter (institutional/constrained) — defines, funds, and administers the mandate; collects political credit
 *   - - whole_language_academics: Excluded voice (moderate/identity_locked) — constructivist researchers legislated out of the approval process
 *   - - reading_science_community: Analytical observer — produces the evidentiary basis all seats contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.36).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.64).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics Foundational-Instruction Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/cognitive-science/political").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'ea951fa2-edf9-48ef-bbca-eada8469d889').
narrative_ontology:cs_kernel_codification('ea951fa2-edf9-48ef-bbca-eada8469d889', formalized).
narrative_ontology:cs_authority_grounding('ea951fa2-edf9-48ef-bbca-eada8469d889', expertise).
narrative_ontology:cs_interpretation_layer_present('ea951fa2-edf9-48ef-bbca-eada8469d889').
narrative_ontology:cs_reading_relation('ea951fa2-edf9-48ef-bbca-eada8469d889', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('ea951fa2-edf9-48ef-bbca-eada8469d889', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('ea951fa2-edf9-48ef-bbca-eada8469d889', foundational, decoding_requires_explicit_systematic_instruction).
narrative_ontology:cs_axiom_status(decoding_requires_explicit_systematic_instruction, holdable).
narrative_ontology:cs_axiom_grounding('ea951fa2-edf9-48ef-bbca-eada8469d889', decoding_requires_explicit_systematic_instruction, empirically_contingent).
narrative_ontology:cs_axiom('ea951fa2-edf9-48ef-bbca-eada8469d889', foundational, sequenced_code_instruction_precedes_text_independence).
narrative_ontology:cs_axiom_status(sequenced_code_instruction_precedes_text_independence, holdable).
narrative_ontology:cs_axiom_grounding('ea951fa2-edf9-48ef-bbca-eada8469d889', sequenced_code_instruction_precedes_text_independence, empirically_contingent).
narrative_ontology:cs_reference_frame('ea951fa2-edf9-48ef-bbca-eada8469d889', systematic_gpc_first_instructional_order).
narrative_ontology:cs_drift_state('ea951fa2-edf9-48ef-bbca-eada8469d889', contemporary_policy_implementation_gap, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ea951fa2-edf9-48ef-bbca-eada8469d889', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, state_education_agencies).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, proficient_early_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, alphabetic_principle).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, simple_view_of_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, national_reading_panel_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children in early grades who do not work out how letter spellings map to speech sounds on their own. Under the arrangement they receive daily sequenced lessons in letter-sound correspondences and blending practice, with progress checked by screening assessments. They cannot opt out of school or choose their instructional method; what reaches them depends on state rules and district adoption decisions made far above their heads.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Students with persistent difficulty mapping spellings to sounds despite ordinary teaching. For them the mandated sequence is the difference between acquiring usable decoding and falling further behind each year; families frequently relocate or pay for private schooling to reach schools that deliver it. Their only exit from the instruction they receive runs through their parents' resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, dyslexic_students, beneficiary,
    powerless, biographical, trapped, national).

% Organized parent networks, many formed around a child's diagnosed reading difficulty, that testified for and in some cases drafted the statute language now in force. They buy private tutoring where they can afford it and monitor district compliance where they cannot. Their leverage concentrates in legislative testimony rather than classroom control.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% Commercial providers of structured literacy programs, decodable text series, screeners, and teacher-training courses. State approval lists and retraining requirements convert their market from competitive sales into guaranteed institutional demand; several have rebranded existing product lines to meet new approval criteria. They can pivot products and jurisdictions freely.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Elementary teachers who must deliver the prescribed scope and sequence, complete often uncompensated retraining hours, and document lesson fidelity to program scripts. Many report the sequence removes planning burden and gives them concrete moves for stuck readers; others describe loss of professional judgment and demoralization at scripted delivery. Transferring out of early grades or leaving the profession carries real career cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, classroom_teachers, beneficiary).

% Children who arrive in kindergarten already decoding or pick it up quickly with minimal code instruction. They sit through the same sequenced lessons as everyone else; supporters of rival approaches argue this time displaces richer literacy work, while the arrangement's supporters argue the lessons cost them little and shore up spelling. They have no say in how their instructional time is allocated.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, proficient_early_readers, payer,
    powerless, biographical, trapped, national).

% Departments of education and state boards that define evidence-based instruction in statute or rule, maintain approved curriculum lists, fund or compel teacher retraining, and report screening data upward. They absorb political pressure from both dyslexia advocates and teacher organizations, and their standing rises with measurable score improvements attributed to the reforms.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, state_education_agencies, beneficiary).

% Teacher educators and researchers in the constructivist tradition whose methods texts, graduate courses, and consulting practices have lost approved status in mandate states. They publish critiques and testify against bills but hold no seat on curriculum approval bodies; their professional identity is bound to the framework now being dismantled, and retraining into the prevailing paradigm would mean abandoning careers built on it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_academics, excluded,
    moderate, generational, identity_locked, continental).

% Cognitive psychologists, education researchers, and meta-analysts who produce the experimental and longitudinal evidence the mandate cites. They evaluate instructional claims across all three traditions, referee the continuing disputes, and supply the findings that legislative staff translate into statute.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes early-grades reading instruction around a common, sequenced body of letter-sound content so that every child receives explicit decoding instruction regardless of teacher improvisation, materials, or school; solves the collective problem that a large minority of children do not acquire decoding from exposure and that uncoordinated instructional choices reliably miss them.
% TRANSFER_FUNCTION: Moves instructional authority and curricular discretion from individual teachers and education-school faculties to state frameworks and approved commercial programs; moves district and state funds to program, assessment, and training vendors through mandated procurement; moves early-grades instructional time toward sequenced code instruction and away from meaning-first activities.
% ABSENT_VOICES: Constructivist teacher educators and whole-language researchers are legislated out of approval bodies and appear in legislative hearings mainly as opposition witnesses; children, both those who need the sequence and those who do not, hold no seat anywhere in the process; veteran teachers with long improvised-practice expertise were consulted late and largely ceremonially in most mandate states.
% DISAPPEARANCE_RATIONALE: If the mandate layer vanished overnight, early-grades instruction would revert to whatever local tradition and commercially marketed programs supplied, heterogeneously as before the reform wave; struggling-reader outcomes would diverge again along the old fault line; vendor revenue streams tied to approval lists and compelled training would collapse or reroute; and teacher curricular discretion would expand immediately. Every named seat's arrangements depend on the constraint's continued operation.
% FOUNDING_PROBLEM: Mid-twentieth-century reading failure: large fractions of children left school unable to read competently under meaning-based methods, most predictably those who could not infer the letter-sound code from exposure — the problem popularized as 'Why Johnny Can't Read' and formalized in successive national reviews of reading research.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: federal and international assessment series (NAEP, PIRLS) independently document persistent below-basic reading rates; clinical and developmental research literatures on dyslexia prevalence and treatment response are maintained by medical and scientific bodies with no stake in curriculum sales; employer and workforce literacy surveys attest the downstream cost. Vendor-commissioned studies exist on the other side and are not counted here; the sources named are independent of both the publishers and the advocacy organizations.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.36: the arrangement's costs are real but bounded — mandated procurement and retraining spending flowing to program vendors, compliance labor, and narrowed teacher judgment — sitting alongside a large, empirically documented coordination payoff in reduced reading failure. Suppression 0.64 is authored as a raw structural property, unscaled by power or scope: the regime's persistence currently depends on statutory exclusion of rival methods, approval-list control, and certification requirements, not on voluntary uptake alone. Theater_ratio 0.22: instruction and screening are functional and outcome-monitored, but a growing minority of activity is performative — legacy products relabeled to meet approval criteria, fidelity documentation detached from practice. Accessibility_collapse 0.45: alternatives are legally collapsed in mandate states but remain live in others and in private and charter settings. Resistance 0.5: organized teacher-association pushback and continuing academic dissent against a broad pro-mandate coalition. Coordination type enforcement_mechanism: the primary coordination function is governing instructional practice across autonomous classrooms through regulatory machinery; the type-default floor applies. The temporal series run on one shared grid (T0..T30, corresponding to roughly 1995..2025) with all three tracked metrics authored at every point; the suppression_requirement series is authored because this story specifically traces enforcement-capacity change — the machinery rose from weak professional norms to statutory enforcement over the interval. Final series values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the struggling-reader and dyslexia-family seats the arrangement is the thing that stands between them and lifelong reading difficulty — near-pure subsidy. From the classroom-teacher seat the same structure arrives as retraining hours, fidelity documentation, and surrendered curricular judgment, partially offset by reduced planning load and better outcomes for stuck readers. From the publisher seat it is guaranteed institutional demand. From the state-agency seat it is a delivery vehicle for measurable score gains and political credit. From the excluded constructivist-academic seat it is the erasure of a professional tradition. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality seats: struggling readers, dyslexic students, and their organized parents sit near the full-beneficiary end, amplified toward subsidy by trapped exit and powerless-to-organized power. Curriculum publishers also derive near the beneficiary end — they collect without running instruction and hold arbitrage-grade exit. Classroom teachers, declared victims with a secondary beneficiary position, derive high but not maximal directionality: they bear the compliance and autonomy costs through the same structure that delivers their students' outcomes. State education agencies are not declared in either array; as administering agenda-setters with a secondary beneficiary position they sit mildly beneficiary-side. Scope amplification applies modestly at national scale for the verification-dependent procurement layer. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass reading failure among children who do not induce decoding implicitly — remains live by the corroborating record (national assessment data showing roughly a third of fourth-graders below basic, persistent dyslexia prevalence), so no dead-mandate mismatch arises: status live crossed with verdict world_rearranges flags nothing. The tangled-rope classification prevents both characteristic mislabels: reading the arrangement as a snare would erase the demonstrated coordination function (the instructional payoff is the best-evidenced element of the entire dispute), while reading it as a rope would erase the enforcement layer, the suppressed alternatives, and the procurement rents that give identifiable seats reasons to defend the arrangement beyond its instructional function. If the founding problem were ever solved — decoding failure engineered down to noise — the mandate layer would face genuine mandatrophy: the enforcement and procurement apparatus would outlive its justification and drift toward piton or snare depending on who still collected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_capture_status,
    'Is the institutionalized phonics-first mandate the settled empirical content of the reading-acquisition kernel, or one contested reading that has captured the policy apparatus?',
    'Preregistered cross-jurisdiction longitudinal comparison of reading outcomes under each sibling reading, conducted by research bodies independent of curriculum vendors and advocacy organizations.',
    'If sibling readings achieve comparable outcomes at lower coercive overhead, the mandate layer loses its coordination justification and the arrangement shifts toward pure extraction; if phonics outcomes dominate across designs, the mandate is coordination carrying bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_capture_status, conceptual, 'Whether this reading''s policy capture reflects settled science or contested-kernel politics.').

omega_variable(
    gpc_necessity_scope,
    'Does decoding acquisition require explicit systematic grapheme-phoneme instruction for all readers, or only for the minority who fail to induce the code implicitly?',
    'Longitudinal instructional-response heterogeneity studies: distributions of decoding outcomes under delayed or absent explicit instruction, stratified by pre-instruction learner profile.',
    'If necessity is minority-scoped, universal mandates over-apply the constraint and the compliance-cost component borne by all classrooms grows relative to the coordination benefit; if necessity is universal, the current scoping is efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpc_necessity_scope, empirical, 'Scope of the necessity claim underlying the mandate.').

omega_variable(
    vendor_rent_share,
    'What share of mandated curriculum, assessment, and training spending reflects marginal instructional value versus brand capture and relabeled legacy products?',
    'Procurement audits comparing outcome-per-dollar across approved programs, including head-to-head trials of high-cost versus low-cost systematic programs.',
    'A large rent share would locate the extraction primarily in the procurement layer rather than the instructional layer, supporting remedies that preserve the mandate while opening competition; a small share would support the current approved-list structure as efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_rent_share, empirical, 'Rent share inside the mandated-spending stream.').

omega_variable(
    fluent_reader_opportunity_cost,
    'Does instructional time allocated to systematic code instruction measurably slow students who would decode fluently without it?',
    'Randomized studies varying explicit-code minutes for students already above decoding thresholds, measuring comprehension and vocabulary growth.',
    'If the opportunity cost is material, the rival readings identify a real payer seat the current victim declaration omits, raising measured extraction; if negligible, the opportunity-cost critique is rhetorical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fluent_reader_opportunity_cost, empirical, 'Opportunity cost of code instruction for fluent decoders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__phonics_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement_basis(read_tr_t25, observed).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__phonics_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(read_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 25, 0.33).
narrative_ontology:measurement_basis(read_be_t25, observed).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement_basis(read_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(read_su_t25, observed).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(read_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the reading wars' conflates three structurally distinct constraints sharing one kernel. This file decomposes the phonics reading; whole_language_reading and balanced_literacy_reading decompose the siblings. Epsilon differs sharply across the family: each reading's standing arrangement carries its own victim sets, enforcement economics, and instructional-cost structure, so each is authored as a separate story with a single stable epsilon. Historical influence ran from whole language through balanced literacy; the contemporary phonics legislative wave now exerts downstream pressure on both siblings — forcing integrated programs to embed systematic code instruction and displacing constructivist methods from approved status. Edges here record this file's structural influence on both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
