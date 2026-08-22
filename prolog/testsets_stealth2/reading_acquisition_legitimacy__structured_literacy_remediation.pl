% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate (Vulnerable-First Instructional Standard)
 *   domain: education_policy/cognitive_science
 *
 * SUMMARY:
 *   Since roughly 2013, a legislative wave across U.S. states (preceded by
 *   England's phonics screening check) has codified structured literacy as
 *   the legitimate standard for beginning reading instruction:
 *   approved-curriculum lists for K-3, statutory bans on three-cueing and
 *   related strategies, universal dyslexia screening, and required
 *   professional development delivered largely through a small set of
 *   training providers. This story instantiates the
 *   structured_literacy_remediation reading of the
 *   reading_acquisition_legitimacy kernel: instruction designed for the most
 *   vulnerable learners first, explicit, cumulative, and continuously
 *   diagnostic, delivered at intervention-grade intensity to all students
 *   preventatively. The arrangement holds a genuine coordination function —
 *   explicit, diagnostic instruction reliably serves the students who cannot
 *   otherwise crack the code — and simultaneously channels compulsory demand
 *   toward specific vendors and certification providers while narrowing
 *   teacher discretion. The claim and the metrics are authored independently:
 *   the claimed type is what I judge structurally true of the arrangement;
 *   the metrics describe its observed operation, and the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - state_education_agencies: agenda setter (institutional/mobile) — writes and administers the approval lists, bans, and screening mandates
 *   - dyslexic_and_struggling_readers: primary intended beneficiary (powerless/trapped) — receive the diagnostic, explicit instruction the standard guarantees
 *   - structured_literacy_curriculum_vendors: commercial beneficiary (organized/arbitrage) — convert each mandate into compulsory demand for materials, training, and certification
 *   - dyslexia_advocacy_organizations: mission beneficiary (organized/identity_locked) — authored the legislative wave; their institutional purpose fuses with the standard's adoption
 *   - classroom_teachers: primary target (organized/constrained) — bear retraining hours, fidelity documentation, and loss of instructional discretion
 *   - public_school_districts: institutional target (institutional/constrained) — absorb mandated procurement, screening, and reporting costs
 *   - teacher_preparation_faculty: identity-locked target (moderate/identity_locked) — programs audited against rubrics that delegitimize their trained traditions
 *   - general_education_students: dual-positioned (powerless/trapped) — receive preventative intensity they may not need, bearing opportunity cost
 *   - independent_reading_researchers: analytical observer — assess which mandate components the evidence supports
 *   - early_childhood_play_advocates: excluded voice — object to displacement of play-based foundations but held no seat in authorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate (Vulnerable-First Instructional Standard)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '9d67aca8-499a-4a3a-ab6e-611075edc503').
narrative_ontology:cs_kernel_codification('9d67aca8-499a-4a3a-ab6e-611075edc503', formalized).
narrative_ontology:cs_authority_grounding('9d67aca8-499a-4a3a-ab6e-611075edc503', expertise).
narrative_ontology:cs_interpretation_layer_present('9d67aca8-499a-4a3a-ab6e-611075edc503').
narrative_ontology:cs_reading_relation('9d67aca8-499a-4a3a-ab6e-611075edc503', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9d67aca8-499a-4a3a-ab6e-611075edc503', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('9d67aca8-499a-4a3a-ab6e-611075edc503', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_axiom('9d67aca8-499a-4a3a-ab6e-611075edc503', foundational, vulnerable_learners_design_priority).
narrative_ontology:cs_axiom_status(vulnerable_learners_design_priority, holdable).
narrative_ontology:cs_axiom_grounding('9d67aca8-499a-4a3a-ab6e-611075edc503', vulnerable_learners_design_priority, deontological).
narrative_ontology:cs_axiom('9d67aca8-499a-4a3a-ab6e-611075edc503', foundational, explicit_cumulative_diagnostic_instruction_required).
narrative_ontology:cs_axiom_status(explicit_cumulative_diagnostic_instruction_required, holdable).
narrative_ontology:cs_axiom_grounding('9d67aca8-499a-4a3a-ab6e-611075edc503', explicit_cumulative_diagnostic_instruction_required, empirically_contingent).
narrative_ontology:cs_axiom('9d67aca8-499a-4a3a-ab6e-611075edc503', secondary, universal_intervention_grade_core).
narrative_ontology:cs_axiom_status(universal_intervention_grade_core, holdable).
narrative_ontology:cs_axiom_grounding('9d67aca8-499a-4a3a-ab6e-611075edc503', universal_intervention_grade_core, instrumental).
narrative_ontology:cs_reference_frame('9d67aca8-499a-4a3a-ab6e-611075edc503', vulnerable_first_diagnostic_instruction_norm).
narrative_ontology:cs_drift_state('9d67aca8-499a-4a3a-ab6e-611075edc503', post_2013_mandate_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9d67aca8-499a-4a3a-ab6e-611075edc503', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_and_struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_advocacy_organizations).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, public_school_districts).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_faculty).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and administer the approved-curriculum lists, screening schedules, practice bans, and professional-development requirements that operationalize the standard. Amend guidance and approval criteria as politics and evidence shift; their administrative reach grows with each mandated component.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_education_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Children who cannot decode fluently without explicit, sequenced instruction. They receive the diagnostic assessment and cumulative teaching the standard prescribes, often years earlier than under prior practice. They depend entirely on adults to select their instruction and cannot leave the classroom that delivers it.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_and_struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Sell approved curricula, training licenses, certification pathways, and tutoring programs. Each new mandate converts a discretionary purchase into a compulsory one; product lines are relabeled and repriced to meet approval criteria. They can pivot offerings to whichever state specification changes next.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Parent-founded organizations that drafted and lobbied the legislative wave. Their membership, funding, and moral authority expand when their framework becomes law; their institutional purpose is fused with the standard's adoption, so retrenchment threatens the organization itself.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Complete dozens of mandated professional-development hours, deliver approved lesson sequences with documented fidelity, and retire familiar practices that are now barred in early grades. Instructional discretion narrows; compliance workload grows. Licensure and employment tie them to the system, and union representation channels grievance rather than exemption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers, payer,
    organized, biographical, constrained, national).

% Purchase approved materials for every K-3 classroom, fund screening and intervention staffing, and absorb audit and reporting obligations. Budget lines shift toward mandated purchases and away from locally chosen programs; refusal is not available while state aid and accreditation ride on compliance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, public_school_districts, payer,
    institutional, generational, constrained, regional).

% University faculty whose programs are audited against structured-literacy rubrics. Syllabi are rewritten, favored texts dropped from approved lists, and graduates' credentials gated on the new content. Many built their scholarly identities inside the traditions now being displaced, so professional survival and intellectual allegiance pull in opposite directions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_preparation_faculty, payer,
    moderate, generational, identity_locked, national).

% Receive intervention-grade decoding instruction preventatively, whether or not they were headed for difficulty. Those who benefit are spared later struggle; all of them spend instructional minutes on routines some did not need, with correspondingly less time for content study, writing volume, and wide reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_students, beneficiary).

% Publish component-level efficacy studies, dose-response analyses, and critiques of which mandate elements the evidence actually supports. They take no material position in the procurement flows and can assess the arrangement from outside the coalition that enacted it.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, independent_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% Developmentalists and early-childhood educators who argue that formal decoding drills displace play-based foundations in kindergarten. They publish and testify but held no seat in the legislative rooms where screening ages, minutes allocations, and practice bans were fixed.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, early_childhood_play_advocates, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes one explicit, verifiable standard for how beginning reading is taught: sequenced phoneme-grapheme instruction, universal screening, progress monitoring, and defined escalation to intensity. Teacher preparation, materials, assessment, and intervention stop being a lottery of classroom-by-classroom preference and become a common pipeline in which struggling readers are found early and taught systematically.
% TRANSFER_FUNCTION: Moves instructional authority and discretion from classroom teachers and local districts to state-approved frameworks and their vendors; moves per-teacher and per-pupil funds toward approved curricula, training licenses, and certification; moves instructional minutes in every K-3 classroom toward explicit decoding routines for all students regardless of need.
% ABSENT_VOICES: Early-childhood play advocates, veteran teachers with documented success under meaning-centered approaches, and developmental researchers emphasizing readiness variation were largely outside the drafting rooms; the children subject to the minute-by-minute reallocations appear nowhere in the authorizing record except as aggregate test scores.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand thousands of district procurement contracts, void recently completed certification requirements, orphan screening and intervention schedules mid-year, collapse a vendor and training market built on compulsory demand, and trigger immediate litigation from advocacy organizations — classrooms would whipsaw between regimes while the coalition that built the standard mobilized to restore it.
% FOUNDING_PROBLEM: Widespread reading failure under approaches that delayed or diluted explicit instruction: struggling readers were identified late, remediated expensively or never, and disproportionately routed into special education; dyslexic students in particular were told to guess from pictures and context rather than taught the code.
% FOUNDING_PROBLEM_CORROBORATION: NAEP long-term trend data, special-education identification rates, and grade-retention statistics — all produced outside the benefiting parties — corroborate that severe reading failure persists and that late identification was real. No source outside the beneficiary set attests that the universal-intervention-grade extension (as opposed to targeted intensity) is necessary to solve it; that specific inference is carried as an open question in the story's omega variables.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.58 at interval end: mandate-driven procurement converts discretionary purchases into compulsory ones, training and certification prices sit well above comparable unmandated markets, and the universal-intensity extension widens the paying population beyond students with diagnosed need — but the arrangement also delivers real instructional services, which bounds epsilon well below snare territory. Suppression is 0.65 and is authored as a raw structural property, unscaled by power or scope: statutory practice bans, approval gates, fidelity monitoring, and employment-consequences machinery actively close alternatives inside mandated grades, while alternatives persist outside them (upper grades, private and home settings, unmandated subjects). Only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio is 0.28: most activity is functional instruction that changed in fact, but a growing share is performative compliance — fidelity binders, relabeled legacy products chasing approval lists, box-ticking PD completion. Accessibility collapse is 0.55: within the mandated K-3 tier, understanding the standard collapses alternatives sharply; outside it they survive. Resistance is 0.5: union grievances over workload, researcher dissent on component-level evidence, several states declining the full package, parental objection to retention triggers. All three temporal series run on one shared seven-point grid (2013–2025, biennial) so every metric is authored at every examined time point; trajectories are monotonic rises matching the enforcement build-out, with endpoint values equal to the scalar base_properties.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently, and two same-level contrasts sharpen why. First, two institutional actors sit on opposite sides: state education agencies (agenda setters whose administrative reach grows with each mandate) versus public school districts (payers bound by the same statutes) — identical nominal power, opposite structural relationships, because the constraint-specific factor is who writes the rules versus who must buy compliance. Second, two powerless agents diverge completely: dyslexic students receive the arrangement's core promised good and cannot be exited anywhere else anyway, while general-education students receive intensity they may not need — equal lack of power, opposite experiences, differentiated by diagnostic need rather than status. Teacher-preparation faculty add an identity-lock case: their exit is blocked less by contract than by scholarly identity fused with the displaced traditions, so the same mandate that a career-switcher would shrug off lands on them as professional delegitimation. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations plus exit options drive the derivation, and no directionality overrides are used. Structured-literacy curriculum vendors (beneficiary, arbitrage-grade exit) sit nearest the full-beneficiary end — they can reprice and pivot around any revision. Dyslexia advocacy organizations (beneficiary, identity_locked) are subsidized but cannot abandon the arrangement without dissolving their own purpose. Dyslexic and struggling readers (beneficiary, trapped) enjoy the largest genuine subsidy, though it is contingent on adult gatekeepers. Classroom teachers and districts (payers, constrained) sit near the full-target end: they bear the transfer and cannot leave the system that imposes it. Teacher-preparation faculty (payers, identity_locked) sit nearest the full-target end of all — trapped targets amplify effective extraction. General-education students carry both declarations, and their computed position should land mid-range, reflecting the genuine trade between preventative benefit and opportunity cost. No overrides were authored because the derivation chain captures every seat correctly, and the override mechanism keys on power atoms too coarsely here: an override on 'institutional' would corrupt either the agencies or the districts, which share that atom but sit on opposite sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — late identification and failed instruction for struggling readers — is corroborated as still live by assessment and special-education data from outside the benefiting parties, so the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag; mandatrophy_resolved is not declared. The classification prevents mislabeling in both directions: a pure-coordination reading would conceal the vendor capture and compliance burdens riding on a real function, while a pure-extraction reading would erase the remediation guarantee that dyslexic students demonstrably depend on and that motivated the original coalition. The forward risk is a scaffold-decay path rather than current obsolescence: if universal preventative instruction succeeds, intervention-grade intensity for all students will outlive its justification, and the theater_ratio series is the instrument that would show the transition — watch for it crossing 0.5 while extractiveness keeps climbing on a flat founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading of the reading_acquisition_legitimacy kernel; how would the sibling readings restructure the constraint''s victim set, beneficiary set, and enforcement architecture?',
    'Comparative authoring of the three sibling stories as separate files; engine-computed foreclosure from axiom contradiction and grounding types; cross-reading seat analysis of who counts as harmed under each legitimacy criterion.',
    'Under whole_language_meaning_primacy the harm relocates to comprehension, motivation, and volume-of-reading losses with decoding failure reframed as developmental variation; under balanced_literacy_integration the mandate architecture dissolves into local choice and the vendor-capture channel closes; under phonics_decoding_primacy the scope narrows to the code and the diagnostic/universal apparatus drops away. This file''s epsilon is valid only for the structured_literacy_remediation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    universality_inference_validity,
    'Does designing instruction for the most vulnerable learners first actually require universal intervention-grade instruction for all students, or does it require targeted intensity layered on a differentiated core?',
    'Randomized and quasi-experimental comparisons of universal-intensity versus targeted-intensity implementations, tracking struggling-reader outcomes alongside general-education opportunity costs (content coverage, writing volume, wide reading).',
    'If targeted intensity achieves equivalent outcomes for vulnerable readers, the universal extension over-extracts from general-education students and teachers — the extraction component shrinks toward a targeted-remediation arrangement with far smaller compelled populations; if universal intensity is genuinely preventive, part of the measured extraction is the price of the guarantee itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_inference_validity, empirical, 'Whether the load-bearing inference from vulnerable-first design to universal intervention-grade instruction holds.').

omega_variable(
    vendor_capture_share,
    'What fraction of the measured extraction is irreducible coordination cost versus monopoly rent captured by approved curriculum vendors and training providers?',
    'Benchmark pricing of comparable professional development and materials in unmandated markets; procurement-record analysis of sole-source approvals and post-mandate price movements.',
    'A high rent share marks the commercial layer as the extraction engine and raises snare-drift risk for the arrangement overall; a low share supports reading the arrangement as coordination with incidental commercial benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_share, empirical, 'Decomposing measured extraction into coordination cost versus captured rent.').

omega_variable(
    settled_science_boundary,
    'Which mandated components rest on robust evidence (systematic phonics for at-risk beginners) and which outrun it (multisensory additives, cueing-ban effects on comprehension, dosage thresholds, universal screening intervals)?',
    'Component-level meta-analytic review separating well-supported elements from thinly supported ones, with pre-registered replication of the contested components.',
    'Where bans and requirements outrun the evidence, suppression functions as doctrine maintenance rather than instructional protection — raising effective suppression and accelerating drift toward extraction-dominated operation; where the evidence is solid, the same suppression reads as defending a genuine standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settled_science_boundary, empirical, 'Locating the boundary between the evidence the mandate rests on and the practices it prohibits.').

omega_variable(
    faculty_resistance_identity_lock,
    'Is teacher-preparation faculty resistance to the standard evidence-based dissent or identity-protective cognition from scholars whose careers are fused with the displaced traditions?',
    'Track publication positions of faculty whose empirical work predates their affiliation with balanced-literacy and whole-language frameworks, compared against career-stage-matched peers without that affiliation.',
    'If resistance tracks affiliation rather than evidence quality, the identity-lock amplifies effective extraction on that seat and the resistance metric partly reflects identity defense; if it tracks evidence, the resistance signals genuine weaknesses the mandate suppresses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(faculty_resistance_identity_lock, conceptual, 'Separating identity-fused opposition from evidential dissent on the identity-locked payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 2013, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slr_remediation_tr_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2013, 0.12).
narrative_ontology:measurement(slr_remediation_tr_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(slr_remediation_tr_t2017, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(slr_remediation_tr_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2019, 0.21).
narrative_ontology:measurement(slr_remediation_tr_t2021, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2021, 0.24).
narrative_ontology:measurement(slr_remediation_tr_t2023, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2023, 0.26).
narrative_ontology:measurement(slr_remediation_tr_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(slr_remediation_be_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2013, 0.3).
narrative_ontology:measurement(slr_remediation_be_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(slr_remediation_be_t2017, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(slr_remediation_be_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(slr_remediation_be_t2021, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(slr_remediation_be_t2023, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2023, 0.56).
narrative_ontology:measurement(slr_remediation_be_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(slr_remediation_su_t2013, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(slr_remediation_su_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(slr_remediation_su_t2017, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(slr_remediation_su_t2019, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2019, 0.52).
narrative_ontology:measurement(slr_remediation_su_t2021, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(slr_remediation_su_t2023, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2023, 0.62).
narrative_ontology:measurement(slr_remediation_su_t2025, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the reading wars' conflates four structurally distinct constraints that are readings of one kernel (reading_acquisition_legitimacy). Each reading carries its own epsilon, victim set, and enforcement architecture, so each is authored as a separate story linked through network edges. Upstream/downstream structure: the phonics-evidence base (NRP synthesis, Ehri's phase model) is the upstream claim this reading cites as warrant for its mandates; this reading in turn exerts structural pressure on balanced_literacy_integration (statutory practice bans dismantle its signature strategies) and absorbs phonics_decoding_primacy's distinctiveness into comprehensive packaged programs. The whole_language reading is the displaced predecessor whose institutional position this reading's revival_pressure directly replaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
