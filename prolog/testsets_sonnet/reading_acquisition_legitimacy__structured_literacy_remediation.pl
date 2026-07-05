% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate (Vulnerable-Learner-First Reading Instruction)
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This story instantiates the structured-literacy-remediation reading of
 *   the contested reading-acquisition-legitimacy kernel: the claim that
 *   instruction must be designed FIRST for the most vulnerable learner
 *   (typically operationalized as students at risk of dyslexia or word-level
 *   reading disability), with explicit, cumulative, diagnostic, multisensory
 *   structured-literacy methods extended preventatively to all students
 *   rather than reserved for identified remediation cases. This is
 *   structurally distinct from the phonics_decoding_primacy reading (which
 *   asserts decoding-first for pedagogical reasons, not vulnerability-first
 *   for equity/diagnostic reasons), from whole_language_meaning_primacy
 *   (which rejects explicit systematic instruction as primary), and from
 *   balanced_literacy_integration (which treats explicit phonics as one
 *   component alongside authentic literature rather than as the organizing
 *   principle). The vulnerability-first premise generates a distinctive
 *   structural signature: universal preventative intervention-grade
 *   instruction, continuous diagnostic screening cycles, and
 *   certification/procurement mandates built around identifying and serving
 *   the at-risk minority — which is the source of this reading's specific
 *   beneficiary/victim structure (curriculum vendors and specialists gain;
 *   incumbent-trained teachers and non-at-risk students who prefer lighter
 *   structure pay a cost).
 *
 * KEY AGENTS:
 *   - state_literacy_policy_boards: agenda_setter, institutional power, sets mandates and certification requirements
 *   - students_with_dyslexia_and_reading_disabilities: primary intended beneficiary, powerless, trapped exit
 *   - classroom_teachers_trained_in_other_methods: primary payer, moderate power, constrained exit
 *   - structured_literacy_curriculum_publishers: secondary beneficiary, organized power, arbitrage exit
 *   - independent_reading_researchers: analytical observer with contested findings on universality of benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.28).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.45).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate (Vulnerable-Learner-First Reading Instruction)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '921c0b72-1ca0-4228-8b8f-560c017922ee').
narrative_ontology:cs_kernel_codification('921c0b72-1ca0-4228-8b8f-560c017922ee', distributed).
narrative_ontology:cs_authority_grounding('921c0b72-1ca0-4228-8b8f-560c017922ee', expertise).
narrative_ontology:cs_interpretation_layer_present('921c0b72-1ca0-4228-8b8f-560c017922ee').
narrative_ontology:cs_reading_relation('921c0b72-1ca0-4228-8b8f-560c017922ee', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('921c0b72-1ca0-4228-8b8f-560c017922ee', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('921c0b72-1ca0-4228-8b8f-560c017922ee', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_axiom('921c0b72-1ca0-4228-8b8f-560c017922ee', foundational, vulnerable_learner_priority_design_principle).
narrative_ontology:cs_axiom_status(vulnerable_learner_priority_design_principle, holdable).
narrative_ontology:cs_axiom_grounding('921c0b72-1ca0-4228-8b8f-560c017922ee', vulnerable_learner_priority_design_principle, empirically_contingent).
narrative_ontology:cs_axiom('921c0b72-1ca0-4228-8b8f-560c017922ee', foundational, universal_preventative_intervention_grade_instruction).
narrative_ontology:cs_axiom_status(universal_preventative_intervention_grade_instruction, holdable).
narrative_ontology:cs_axiom_grounding('921c0b72-1ca0-4228-8b8f-560c017922ee', universal_preventative_intervention_grade_instruction, instrumental).
narrative_ontology:cs_axiom('921c0b72-1ca0-4228-8b8f-560c017922ee', secondary, diagnostic_screening_as_continuous_obligation).
narrative_ontology:cs_axiom_status(diagnostic_screening_as_continuous_obligation, holdable).
narrative_ontology:cs_axiom_grounding('921c0b72-1ca0-4228-8b8f-560c017922ee', diagnostic_screening_as_continuous_obligation, conventional).
narrative_ontology:cs_reference_frame('921c0b72-1ca0-4228-8b8f-560c017922ee', ida_knowledge_and_practice_standards_2010s).
narrative_ontology:cs_drift_state('921c0b72-1ca0-4228-8b8f-560c017922ee', post_science_of_reading_media_wave_2020s, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('921c0b72-1ca0-4228-8b8f-560c017922ee', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_disabilities).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_certified_specialists).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_advocacy_organizations).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, schools_with_constrained_intervention_budgets).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, students_who_would_thrive_under_lighter_structure).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, science_of_reading_evidence_base).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, universal_screening_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate structured literacy curricula and diagnostic screening protocols in statute or regulation, citing dyslexia-prevalence research and multi-tiered systems of support. Sets certification requirements for teacher preparation programs and can withhold funding from districts that do not comply. Administers the enforcement machinery but does not itself bear implementation cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_literacy_policy_boards, agenda_setter,
    institutional, generational, analytical, national).

% Historically underserved by meaning-first or balanced approaches that did not diagnose or remediate decoding deficits early. Under this reading, receive explicit, cumulative, multisensory instruction and continuous diagnostic monitoring from the outset. Cannot self-select an instructional approach; entirely dependent on what the classroom or intervention program delivers.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_disabilities, beneficiary,
    powerless, biographical, trapped, local).

% Many were credentialed under whole-language or balanced-literacy teacher preparation and must now retrain, often unpaid or under-compensated, to deliver scripted structured-literacy curricula and administer diagnostic assessments they did not choose. Face professional discipline or non-renewal for noncompliance. Exit means leaving the profession or the jurisdiction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, regional).

% Sell scripted, sequenced curricula, decodable texts, and diagnostic assessment suites that districts are now mandated to purchase. Lobby policy boards to broaden the definition of 'evidence-based' in ways that favor their proprietary materials. Face essentially no exit risk since demand is created by statute.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Hold credentials that are now required or strongly preferred under the mandate, giving them elevated job security and consulting income. Can move between districts or into private tutoring/consulting markets as demand for their specific certification rises.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_certified_specialists, beneficiary,
    moderate, biographical, mobile, national).

% Must fund universal diagnostic screening, tiered intervention staffing, and curriculum replacement without new revenue, often diverting funds from other subjects or support services. Cannot opt out without risking accreditation or funding penalties; cannot easily generate new revenue to comply.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, schools_with_constrained_intervention_budgets, payer,
    moderate, biographical, trapped, local).

% Some children acquire decoding readily and find heavily scripted, repetitive, diagnostic-saturated instruction tedious or disengaging compared to richer literature-based approaches. Under a preventative universal-intervention design, these students receive the same intensive structure as at-risk peers regardless of need, with no individual opt-out.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_who_would_thrive_under_lighter_structure, payer,
    powerless, biographical, trapped, local).

% Advocated for decades for legal recognition of dyslexia and mandated early screening; now shape statutory language and monitor district compliance. Gain policy legitimacy and continued relevance from the mandate's persistence, and can shift advocacy targets across states as needed.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_advocacy_organizations, agenda_setter).

% Study comparative outcomes across structured-literacy, phonics-only, balanced, and whole-language cohorts. Some findings support strong effects for at-risk readers specifically, with more contested effects for typically-developing readers pushed through the same intensive protocol; they do not administer or fund any reading program.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, independent_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates early identification of reading difficulty and delivers a single, sequenced, diagnostically-monitored instructional pathway so that at-risk readers are caught and remediated before failure compounds, rather than waiting for a 'wait to fail' discrepancy model.
% TRANSFER_FUNCTION: Moves curriculum-adoption budgets and teacher retraining time from districts and incumbent-method teachers toward structured-literacy publishers and Orton-Gillingham-credentialed specialists; moves instructional attention and diagnostic burden universally onto all students regardless of individual need, in the name of protecting the most vulnerable minority.
% ABSENT_VOICES: Teachers who found success with balanced or whole-language methods for the majority of their students are largely outside the policy-drafting rooms, which are dominated by dyslexia advocacy groups and structured-literacy researchers/vendors. Students who dislike the intensive drilling have no forum at all.
% DISAPPEARANCE_RATIONALE: Advocates argue that without the mandate, districts would revert to discrepancy-model wait-to-fail practices and dyslexic students would again go undiagnosed for years — a real rearrangement. Critics argue the underlying skill (explicit phonics instruction for those who need it) predates and would survive the mandate's disappearance via ordinary special-education law (IDEA), and that only the vendor-certification-testing apparatus built on top of it would vanish — a smaller, contested rearrangement.
% FOUNDING_PROBLEM: A meaningful minority of children (historically estimated 5-20% depending on definition) do not acquire decoding skill through implicit exposure or balanced approaches and were being systematically misdiagnosed, under-identified, or blamed for 'not being ready to read' under prior instructional regimes, with disproportionate impact on low-income and minority students who lacked private tutoring resources.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers outside the curriculum-publishing industry (e.g. independent university reading labs, some replicated in meta-analyses) corroborate that structured, explicit, diagnostic instruction produces measurably better outcomes for students with word-level reading disabilities specifically. However, independent researchers and some state auditors dispute that the same intensity is warranted or beneficial for students without such disabilities, and note that curriculum vendors and testing companies now co-author much of the 'evidence-based' policy language — a corroboration gap the mandate's own advocacy organizations do not acknowledge.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).
:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28) and rising slowly: the coordination function (catching at-risk readers early) is real and well-corroborated for the target population, so most of the resource transfer is genuine coordination cost, not pure rent. The rising trajectory reflects an accumulating vendor/certification layer (curriculum licensing, proprietary assessment suites, specialist certification renewal fees) that increasingly captures value beyond the diagnostic function itself. Suppression is moderate (0.45) and hardens over the interval as states move from encouragement to statutory mandate with accreditation consequences for noncompliant districts and non-renewal risk for non-retrained teachers — this is a genuine enforcement ratchet, not merely rhetorical. Theater ratio stays comparatively low (0.22) because the diagnostic screening and explicit-instruction components are functionally real and outcome-linked for the target population, though a growing share of compliance activity (vendor-approved fidelity checklists, certification renewal theater) is more performative than functionally necessary.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a student with a genuine word-level reading disability, or their advocate, the arrangement reads as a coordination success — finally, a systematic method that catches what previous approaches missed. From the seat of a veteran teacher whose non-at-risk students thrived under a different method, and who now must retrain and administer diagnostic protocols to students who do not need them, the same arrangement reads as extractive overreach: a genuine minority-need intervention generalized into a universal, budget-consuming, autonomy-reducing mandate. The engine computes these as different seat-level classifications from the same structural data; neither seat is wrong about its own position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — students with reading disabilities, structured-literacy publishers, certified specialists, and advocacy organizations — sit near the low end of directionality: the mandate subsidizes their outcomes, revenue, or institutional relevance. Payers — incumbent-trained teachers, budget-constrained schools, and students who did not need intensive intervention but receive it anyway under universal preventative design — sit near the high end: they bear retraining cost, budget reallocation, or instructional tedium without a corresponding need. The state policy board is the agenda_setter but does not itself absorb the cost of compliance, which is why the enforcement is durable: the entity that can change the mandate is not the entity paying its ongoing cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — undiagnosed, under-served readers with genuine decoding disabilities — remains partially live: the underlying disability rate has not disappeared. What is contested is whether the SCOPE of the remedy (universal preventative intensive instruction for all students, rather than targeted remediation for identified at-risk students) still matches the founding problem, or whether the vendor/certification apparatus has generalized a targeted remediation insight into an industry-scale procurement mandate whose growth outpaces the population it was designed to serve. This is precisely the ambiguity the founding_problem_status of 'contested' and the corroboration gap are meant to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerability_first_vs_universal_mandate_scope,
    'Is the vulnerability-first premise (design for the most at-risk learner) best implemented as targeted remediation for diagnosed students, or as a universal preventative mandate applied to all students regardless of individual need?',
    'Compare reading outcomes and engagement/satisfaction metrics in districts that implement tiered targeted structured-literacy intervention (RTI/MTSS triggered by screening) versus districts that mandate universal Tier-1 structured-literacy instruction for all students from kindergarten regardless of screening result.',
    'If targeted remediation captures nearly all the benefit with far less cost and disruption to non-at-risk students and incumbent teachers, the universal-mandate version of this reading looks more like tangled_rope (real coordination for a subset, extraction from the rest) than a clean rope; if universal preventative structure benefits nearly all students similarly, the rope framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_first_vs_universal_mandate_scope, empirical, 'Whether vulnerability-first legitimacy requires universal mandate or only targeted remediation.').

omega_variable(
    vendor_capture_of_evidence_based_definition,
    'To what extent has the statutory/regulatory definition of ''evidence-based structured literacy'' been shaped by curriculum and assessment vendors who profit from that definition, versus independent cognitive science consensus?',
    'Trace the drafting history and public comment record of state literacy statutes and cross-reference lobbying disclosures and expert-testimony affiliations of those who shaped the definitional language, alongside independent replication of the underlying reading-science claims.',
    'Heavy vendor involvement in defining ''legitimate'' instruction would indicate a tangled_rope or emergent snare dynamic riding on genuine reading-science findings; minimal vendor involvement would support a cleaner rope/coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_of_evidence_based_definition, empirical, 'Whether the legitimacy-defining apparatus is captured by commercial curriculum interests.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading''s disagreement with phonics_decoding_primacy and balanced_literacy_integration sit — is it a genuine disagreement about instructional method, or only about which population the method should be mandatory for?',
    'Compare the specific instructional practices prescribed under each reading for a diagnosed at-risk reader versus a typically-developing reader; if the prescribed practice converges for at-risk readers across all three readings and diverges only for typically-developing readers, the disagreement is located in scope-of-mandate, not in method.',
    'If the disagreement is scope-only, this reading and phonics_decoding_primacy may be far closer structurally than the labels suggest, and the sharper contest is with whole_language_meaning_primacy and the universal-application clause of balanced_literacy_integration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating whether inter-reading disagreement is about method or about mandate scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 4, 0.11).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 8, 0.14).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 12, 0.17).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 16, 0.2).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the reading_acquisition_legitimacy kernel. structured_literacy_remediation shares the explicit-phonics premise with phonics_decoding_primacy but adds a vulnerability-first universal-mandate structure that phonics_decoding_primacy does not require; it directly contradicts whole_language_meaning_primacy's implicit-emergence premise; it shares partial structure with balanced_literacy_integration but rejects that reading's parity between explicit phonics and authentic-literature immersion as insufficient for at-risk readers. ε differs across all four: this reading is authored with the lowest extractiveness of the four (moderate coordination benefit for a real at-risk population, moderate-rising vendor capture) while whole_language_meaning_primacy is expected to show a different beneficiary/victim structure entirely (publishers of trade literature, versus dyslexic students underserved).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
