% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Universal Structured Literacy Instruction Mandate (Orton-Gillingham Tradition)
 *   domain: educational/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates one reading of the literacy_acquisition_kernel:
 *   the claim, descending from Samuel Orton's 1920s-30s clinical neurology
 *   and Anna Gillingham's 1936 manual, that reading acquisition requires
 *   explicit, systematic, cumulative instruction across phonological
 *   awareness, phonics, fluency, vocabulary, and comprehension. Devised
 *   originally for students with dyslexia, the approach is now asserted
 *   applicable to all learners and is institutionalized through accredited
 *   teacher certification, licensed curricula, and state statutes. The
 *   arrangement solves a genuine collective problem - alphabetic reading is
 *   not acquired incidentally by every child - while simultaneously
 *   transferring money, time, and instructional authority from teachers and
 *   district budgets to an accreditation-and-publishing apparatus. The claim
 *   and the metrics are independent authored facts: the claimed type states
 *   what I believe is structurally true of the standing arrangement, and the
 *   metrics state what I believe is descriptively true of its operation; the
 *   engine computes each seat's classification from the structural data.
 *
 * KEY AGENTS:
 *   - students_with_learning_disabilities: Primary beneficiary (powerless/trapped) - gain validated decoding instruction unavailable to them elsewhere; no exit from compulsory schooling or from the assigned method
 *   - nondisabled_elementary_students: Dual-position participants (powerless/constrained) - declared beneficiaries under this reading's universal-applicability claim, carrying diffuse indirect costs of hours and budget diversion
 *   - general_education_teachers: Primary target (organized/constrained) - bear the training burden, the licensure attachment, and the loss of instructional authority
 *   - school_districts: Conduit payer (institutional/constrained) - fund the apparatus but recoup compliance value in outcomes, litigation avoidance, and tied funding
 *   - dyslexia_training_institutes: Agenda setter and fee collector (institutional/arbitrage) - write the standards defining qualified instruction and collect tuition and renewal fees
 *   - structured_literacy_curriculum_publishers: Program vendor (powerful/arbitrage) - convert mandated classrooms into adoption-cycle revenue
 *   - parents_of_struggling_readers: Organized advocates turned beneficiaries (organized/constrained) - converted private tutoring expenditure into statutory entitlement
 *   - reading_science_researchers: Analytical observer (analytical/analytical) - produce the evidence base the mandate cites; collect no fees from the apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.48).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Universal Structured Literacy Instruction Mandate (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '3f4938a1-8ea5-419d-81dd-d19da965d827').
narrative_ontology:cs_kernel_codification('3f4938a1-8ea5-419d-81dd-d19da965d827', distributed).
narrative_ontology:cs_authority_grounding('3f4938a1-8ea5-419d-81dd-d19da965d827', expertise).
narrative_ontology:cs_interpretation_layer_present('3f4938a1-8ea5-419d-81dd-d19da965d827').
narrative_ontology:cs_reading_relation('3f4938a1-8ea5-419d-81dd-d19da965d827', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f4938a1-8ea5-419d-81dd-d19da965d827', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('3f4938a1-8ea5-419d-81dd-d19da965d827', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3f4938a1-8ea5-419d-81dd-d19da965d827', foundational, explicit_systematic_cumulative_instruction_necessity).
narrative_ontology:cs_axiom_status(explicit_systematic_cumulative_instruction_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3f4938a1-8ea5-419d-81dd-d19da965d827', explicit_systematic_cumulative_instruction_necessity, empirically_contingent).
narrative_ontology:cs_axiom('3f4938a1-8ea5-419d-81dd-d19da965d827', foundational, structured_literacy_benefit_extends_to_all_learners).
narrative_ontology:cs_axiom_status(structured_literacy_benefit_extends_to_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('3f4938a1-8ea5-419d-81dd-d19da965d827', structured_literacy_benefit_extends_to_all_learners, empirically_contingent).
narrative_ontology:cs_axiom('3f4938a1-8ea5-419d-81dd-d19da965d827', secondary, multisensory_prescriptive_diagnosis_improves_retention).
narrative_ontology:cs_axiom_status(multisensory_prescriptive_diagnosis_improves_retention, holdable).
narrative_ontology:cs_axiom_grounding('3f4938a1-8ea5-419d-81dd-d19da965d827', multisensory_prescriptive_diagnosis_improves_retention, instrumental).
narrative_ontology:cs_axiom('3f4938a1-8ea5-419d-81dd-d19da965d827', secondary, multisensory_training_repairs_neurological_defect).
narrative_ontology:cs_axiom_status(multisensory_training_repairs_neurological_defect, overridden).
narrative_ontology:cs_axiom_grounding('3f4938a1-8ea5-419d-81dd-d19da965d827', multisensory_training_repairs_neurological_defect, empirically_contingent).
narrative_ontology:cs_reference_frame('3f4938a1-8ea5-419d-81dd-d19da965d827', orton_gillingham_prescriptive_canon).
narrative_ontology:cs_drift_state('3f4938a1-8ea5-419d-81dd-d19da965d827', post_legislative_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f4938a1-8ea5-419d-81dd-d19da965d827', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, nondisabled_elementary_students).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_training_institutes).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, school_districts).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, nondisabled_elementary_students).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, phonological_deficit_hypothesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, simple_view_of_reading).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, national_reading_panel_meta_analytic_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elementary and secondary students whose brains process written language atypically. Receive daily explicit, sequenced lessons in sound-letter mapping, decoding, and connected-text work, often in pull-out or intensive-tier settings. They cannot choose their school's method, cannot opt out of instruction, and depend on adults and law for access to appropriate teaching. When the instruction arrives they gain independent reading; when it is delayed they lose ground every year.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, trapped, national).

% Most of every classroom covered by the universal mandate. They receive systematic phonics and structured lessons from kindergarten onward, which builds reliable decoding for most beginners. They carry the indirect costs: instructional hours devoted to routines some master quickly, and district money spent on programs and training instead of other resources. Their households can leave for private or home schooling only at significant cost.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, nondisabled_elementary_students, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, nondisabled_elementary_students, payer).

% Must deliver the mandated lesson structures and, in many states, complete approved training: coursework plus supervised practicum hours that can run past a hundred, sometimes on their own time and at their own expense. Union representation and seniority give them channels to contest specifics, but the requirement attaches to licensure and employment. Leaving the profession is the main way out and carries heavy personal cost; many describe mandated scripts as a loss of the professional judgment their identity rests on.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    organized, biographical, constrained, national).

% Purchase licensed curricula, sign trainer contracts, fund substitute coverage for release-day training, and file implementation reports for the state. They recoup value through measurable outcome gains, litigation avoidance, and eligibility for state funding streams tied to compliance, but cannot legally decline the mandate while operating public schools.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, school_districts, beneficiary).

% Accredit preparation programs, certify practitioners, and maintain the lesson-progression canon descended from the Orton-Gillingham manuals. They write the standards that define qualified instruction and can revise them. Their income is tuition and renewal fees from teachers seeking the credential, so their finances depend on the credential remaining scarce and required.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_training_institutes, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_training_institutes, beneficiary).

% Package the method as scripted teacher guides, decodable text sets, assessment platforms, and companion professional development, sold to districts under adoption cycles. Revenue scales with the number of mandated classrooms, and multi-year contracts with high switching costs keep buyers renewing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Organized into advocacy networks that drafted and pressed the state statutes. Before the mandates many paid several thousand dollars a year for private multisensory tutoring; now validated instruction reaches their children through public schooling. Their leverage is legislative testimony and school-board elections rather than day-to-day control of classrooms.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% Run the longitudinal and experimental studies the mandate cites, publish replications and boundary-condition findings, and advise panels. They collect no fees from the apparatus and their professional standing depends on the causal claims being right rather than popular.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in literacy: alphabetic reading is not acquired incidentally by every child, and classroom-by-classroom improvisation predictably fails roughly a fifth of students. A shared explicit progression lets any prepared teacher deliver validated instruction, lets specialists escalate consistently across tiers, and gives students with dyslexia access to what previously only wealthy families could buy privately.
% TRANSFER_FUNCTION: Moves time and money from classroom teachers (uncompensated or personally funded training hours), from district budgets (licensed programs, trainer contracts, substitute coverage), and from instructional autonomy (lesson decisions shift from teachers to codified sequences and program scripts) toward accredited institutes, curriculum vendors, and the credentialing apparatus.
% ABSENT_VOICES: Children have no seat anywhere in the arrangement: the people whose school hours are being arranged hold no vote in statute, adoption committee, or accreditation. Veteran teachers who already obtain strong results with meaning-rich classrooms enter the process only as implementers, not authors. Researchers documenting boundary conditions where explicit instruction shows flat returns testify occasionally and hold no vote. All three speak from outside the rooms where standards, curricula, and statutes are actually written.
% DISAPPEARANCE_RATIONALE: State statutes go inert, accreditation demand evaporates within a certification cycle, and district adoptions lapse at contract renewal. Specialist multisensory tutoring persists as a private market for families able to pay, restoring the pre-mandate pattern in which outcomes for students with dyslexia tracked household income. Classroom instruction reverts to whatever each district and teacher adopts next. That is a genuine reorganization, not stasis.
% FOUNDING_PROBLEM: In the 1920s-1930s, children with dyslexic profiles failed catastrophically under whole-word and incidental text-exposure methods. Samuel Orton's clinical neurology identified the failure mode, and Anna Gillingham codified an explicit, sequential, multisensory remedial sequence so these students could be taught decoding directly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the peer-reviewed replication literature (National Reading Panel meta-analyses, NICHD-funded longitudinal cohorts), clinical neuropsychological diagnostic practice, and sworn parental testimony before state education committees all attest that students with dyslexic profiles fail under incidental methods. None of these sources collects certification or licensing revenue. What is disputed, chiefly by educators and researchers outside the apparatus, is the later extension of the founding solution from the target population to all learners - not the founding problem itself.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Metrics authored independently of the claim. Extractiveness 0.62 describes the standing arrangement at interval end: genuine instructional delivery coexists with rents layered on top - certification tuition and renewal fees priced above the marginal cost of supervised practicum, licensed programs sold under adoption cycles with high switching costs, and mandated training hours frequently uncompensated. Suppression 0.48 is the raw structural figure, deliberately unscaled (scaling by directionality and spatial scope is the engine's arithmetic): statutes and accreditation gates close much of the alternative space, yet meaning-centered practice survives in private schools and non-adopting classrooms, so closure is partial rather than total. Theater_ratio 0.30 reflects the growing compliance stratum - training-completion logs, fidelity walkthroughs, certificate display - that defends the apparatus alongside real teaching. Accessibility_collapse 0.50: once the experimental evidence is granted, purely incidental methods are untenable, but hybrid practices remain live, so alternatives collapse only halfway. Resistance 0.55: unions contest unfunded mandates, scholars contest universality, and implementation fights recur. The scalars integrate an oscillating history - the reading wars cycled through whole-language ascendancy in the 1980s-1990s before the post-2018 legislative wave - and the terminal values were measured during the enforcement-ascendant phase of that cycle, so they sit above each series' historical mean. The suppression_requirement series is authored because the story specifically traces enforcement-capacity buildup (voluntary clinical practice, then accreditation gates, then statutory mandates); it is not a restatement of the static suppression scalar. All three series share one time grid (0/20/40/60/80/100) so every metric is authored at every examined point. Coordination type enforcement_mechanism fits the arrangement's primary function - a regulatory framework governing classroom practice - with the type-default floor, no override.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the collector seats compute opposite experiences from identical structural facts. From a classroom teacher's position the mandate arrives as lost professional judgment plus billable hours - high effective extraction, amplified by constrained exit. From an accredited institute's position the same statute is demand for the credential only it can grant; from a publisher's position it is an adoption pipeline. Households of students with dyslexia experience the arrangement as rescue: instruction they previously purchased privately at several thousand dollars per year now arrives through public schooling. The engine derives each seat's classification from role, power, exit, and scope data; nothing in the authored claim adjudicates between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place students_with_learning_disabilities and their households near the beneficiary pole: the constraint subsidizes them (formerly private-purchase instruction becomes public provision). nondisabled_elementary_students are declared beneficiaries under this reading's universal claim but carry a secondary payer position (instructional hours and district dollars diverted), locating them short of the beneficiary pole. general_education_teachers sit near the target pole: they bear the transfer directly, and constrained exit - leaving the profession is the realistic route out - keeps them from arbitrage relief. school_districts are conduits: they disburse to institutes and publishers but recoup compliance value, moderating their directional position below the teacher seat's. dyslexia_training_institutes and structured_literacy_curriculum_publishers sit near the beneficiary pole with arbitrage-grade exit - both can repackage method or market if the mandate collapses. No directionality_overrides are authored: the beneficiary/victim declarations together with dual roles and exit grades already locate every seat, and atom-keyed overrides would collide across the two powerless seats and the two institutional seats, distorting agents the derivation already places correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - students with dyslexic profiles failing under incidental methods - is still live wherever structured instruction is absent, so the mandate has not outlived its function and mandatrophy_resolved stays unset. The classification guards both error directions: calling the whole arrangement a snare would erase the documented intervention benefit that anchors its legitimacy for the founding population; calling it a rope would erase the certification-layer rents visible in the temporal series, which rise monotonically across an interval in which the founding population's core needs were met decades ago. The live structural risk is scope creep rather than obsolescence: the universal layer extends beyond the founding warrant, and if tiered evidence ever showed flat returns for low-risk students, the universal layer - not the targeted core - becomes the mandatrophy candidate. Drift detection serves exactly that separation: rising base_extractiveness against a stable founding problem indicates rent accumulation on a functioning core, not a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_reading_or_phonics_variant,
    'Is structured_literacy_reading a genuinely distinct fourth reading of the literacy_acquisition_kernel, or a scoped variant of phonics_reading whose added pillars (phonological awareness, fluency, vocabulary, comprehension, multisensory prescription) ride on phonics'' evidentiary core?',
    'Compare foundational axiom sets against phonics_reading: if phonics_reading''s axioms entail the five-pillar extension whenever resources allow, collapse this reading to a variant; if the universal-applicability and prescriptive-diagnostic commitments rest on premises phonics_reading does not hold, count this as a fourth reading.',
    'Variant status merges network nodes and pools epsilon estimates across the family; fourth-reading status preserves this reading''s separate epsilon and isolates the certification-layer extraction unique to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_reading_or_phonics_variant, conceptual, 'Whether this instantiation is a fourth kernel reading or a phonics_reading variant (manifest-flagged contest).').

omega_variable(
    universal_applicability_boundary,
    'Does the intervention''s demonstrated benefit extend to all students, or is net benefit concentrated in struggling readers such that the universal mandate redistributes cost onto classrooms that would do as well under less demanding methods?',
    'Tiered-response outcome comparisons of universal structured instruction versus targeted-only delivery, stratified by baseline reader risk; paired with archival analysis of when universal-scope language entered accredited standards and marketing (whether the expansion was native to Orton''s own claims or a later commercial move).',
    'If benefit is concentrated in the target population, the universal layer is extraction riding on a targeted intervention''s evidence: the constraint decomposes into a coordination core (targeted intervention) beneath an extractive shell (universal certification mandate), shifting the computed type at the mandate layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_boundary, empirical, 'Whether universal application is warranted or the mandate outruns the founding population''s evidence.').

omega_variable(
    certification_cost_composition,
    'What fraction of certification cost purchases genuine quality assurance (supervised practicum that measurably improves instruction) versus gatekeeping rent (scarcity maintenance of the credential)?',
    'Outcome-matched comparison of fully certified versus brief-trained teachers on student decoding growth, plus fee benchmarking against the marginal delivery cost of practicum supervision.',
    'A high rent fraction tilts the certification layer toward snare and strengthens capture findings at the institute seat; a low fraction supports treating the credential as coordination cost within the tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_cost_composition, empirical, 'Rent-versus-quality-assurance composition of the certification fee structure.').

omega_variable(
    teacher_resistance_mechanism,
    'Is organized teacher resistance driven by the structural training burden (hours, out-of-pocket cost, lost instructional time) or by professional-identity threat to instructional autonomy?',
    'Post-mandate survey factor analysis separating workload grievances from autonomy grievances; attrition timing among experienced teachers relative to mandate phases.',
    'If identity-driven, suppression travels with the teacher after burden relief and exit stays locked even when training is subsidized; if structural, funded release-time training dissolves most resistance and the payer seat''s effective extraction falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_resistance_mechanism, empirical, 'Structural versus identity-based composition of payer-seat resistance.').

omega_variable(
    canon_drift_acknowledgment,
    'Does the accreditation structure acknowledge that mass-market scaled delivery (scripted boxed programs, asynchronous online certification, shortened practica) departs from the founding prescriptive-diagnostic canon, or does it market scaled programs as continuous with it?',
    'Document analysis of accredited-standard revisions, accreditation-body deliberations, and marketing claims against the Gillingham manual''s diagnostic-prescriptive requirements.',
    'Acknowledged drift invites recalibration of the reference frame and lowers measured theater at the compliance layer; unacknowledged drift means the credential certifies membership in a canon the products no longer instantiate, raising the apparatus''s inertial component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canon_drift_acknowledgment, conceptual, 'Whether practice drift from the OG canon is recognized by the authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(lite_tr_t20, observed).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(lite_tr_t40, observed).
narrative_ontology:measurement(lite_tr_t60, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(lite_tr_t60, observed).
narrative_ontology:measurement(lite_tr_t80, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement_basis(lite_tr_t80, observed).
narrative_ontology:measurement(lite_tr_t100, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(lite_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(lite_be_t20, observed).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(lite_be_t40, observed).
narrative_ontology:measurement(lite_be_t60, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(lite_be_t60, observed).
narrative_ontology:measurement(lite_be_t80, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(lite_be_t80, observed).
narrative_ontology:measurement(lite_be_t100, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(lite_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement_basis(lite_su_t20, observed).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 40, 0.14).
narrative_ontology:measurement_basis(lite_su_t40, observed).
narrative_ontology:measurement(lite_su_t60, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(lite_su_t60, observed).
narrative_ontology:measurement(lite_su_t80, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement_basis(lite_su_t80, observed).
narrative_ontology:measurement(lite_su_t100, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(lite_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the literacy_acquisition_kernel per the epsilon-invariance principle: the colloquial label 'the science of reading' covers four structurally distinct claims about reading acquisition, authored as four stories. phonics_reading is upstream of this reading - its decoding-precedes-comprehension claim supplies the evidential core cited to justify the five-pillar extension, and its lower training demands give it a different epsilon. This reading exerts downstream structural pressure on balanced_literacy_reading (statutory exclusion of three-cueing and approved-list conditionality reshape its operating environment without eliminating the position) and logically forecloses whole_language_reading (direct contradiction on the necessity-of-explicit-decoding premise). Each sibling carries its own epsilon, beneficiaries, victims, and claimed type; no averaging across readings occurs in this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
