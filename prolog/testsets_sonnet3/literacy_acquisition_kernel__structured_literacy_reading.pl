% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy / Orton-Gillingham Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The structured literacy / Orton-Gillingham reading is one of four
 *   contested readings of the literacy acquisition kernel: how reading is
 *   best acquired, and what instruction should legally require. This reading
 *   holds that phonological awareness, systematic phonics, fluency,
 *   vocabulary, and comprehension must be taught explicitly, systematically,
 *   and cumulatively — a tradition developed specifically for students with
 *   dyslexia but increasingly mandated as universal practice through state
 *   dyslexia legislation. Its ε is authored for the standing arrangement as
 *   this reading's own advocates would describe it: real coordination benefit
 *   concentrated on students with reading disabilities, with genuine but
 *   growing extraction imposed on the general teaching workforce and
 *   underfunded districts through certification requirements, curriculum
 *   licensing, and compliance mandates that have expanded beyond the
 *   population the tradition was originally built to serve. This is NOT the
 *   same constraint as phonics_reading (decoding-before-comprehension
 *   sequencing without the disability-specific origin or certification
 *   architecture), nor whole_language_reading (which rejects explicit
 *   decoding instruction as unnecessary), nor balanced_literacy_reading
 *   (which treats systematic phonics and rich text engagement as
 *   complementary rather than treating one lineage as universally
 *   authoritative). Each sibling reading is a separate constraint story with
 *   its own ε.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: Primary beneficiary (powerless/trapped) — the population the coordination function was built for
 *   - general_education_teachers: Primary payer (moderate/constrained) — bear certification and training burden
 *   - underfunded_school_districts: Secondary payer (moderate/constrained) — bear compliance funding burden
 *   - structured_literacy_credentialing_bodies: Agenda-setter and beneficiary (organized/arbitrage) — sets fidelity standards, collects fees
 *   - og_curriculum_publishers: Beneficiary (organized/arbitrage) — sells mandated materials
 *   - whole_language_and_balanced_literacy_advocates: Excluded voice — argue against universal mandate scope
 *   - reading_researchers: Analytical observer (analytical/analytical) — assesses comparative efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.48).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy / Orton-Gillingham Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '836740e1-7d52-4ca2-b6f9-4339d63d983e').
narrative_ontology:cs_kernel_codification('836740e1-7d52-4ca2-b6f9-4339d63d983e', distributed).
narrative_ontology:cs_authority_grounding('836740e1-7d52-4ca2-b6f9-4339d63d983e', practice).
narrative_ontology:cs_interpretation_layer_present('836740e1-7d52-4ca2-b6f9-4339d63d983e').
narrative_ontology:cs_reading_relation('836740e1-7d52-4ca2-b6f9-4339d63d983e', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('836740e1-7d52-4ca2-b6f9-4339d63d983e', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('836740e1-7d52-4ca2-b6f9-4339d63d983e', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('836740e1-7d52-4ca2-b6f9-4339d63d983e', foundational, disability_specific_origin_licenses_universal_mandate).
narrative_ontology:cs_axiom_status(disability_specific_origin_licenses_universal_mandate, holdable).
narrative_ontology:cs_axiom_grounding('836740e1-7d52-4ca2-b6f9-4339d63d983e', disability_specific_origin_licenses_universal_mandate, empirically_contingent).
narrative_ontology:cs_axiom('836740e1-7d52-4ca2-b6f9-4339d63d983e', foundational, fidelity_to_named_tradition_requires_formal_certification).
narrative_ontology:cs_axiom_status(fidelity_to_named_tradition_requires_formal_certification, holdable).
narrative_ontology:cs_axiom_grounding('836740e1-7d52-4ca2-b6f9-4339d63d983e', fidelity_to_named_tradition_requires_formal_certification, conventional).
narrative_ontology:cs_reference_frame('836740e1-7d52-4ca2-b6f9-4339d63d983e', og_multisensory_dyslexia_remediation_tradition).
narrative_ontology:cs_drift_state('836740e1-7d52-4ca2-b6f9-4339d63d983e', post_state_dyslexia_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('836740e1-7d52-4ca2-b6f9-4339d63d983e', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, og_curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, underfunded_school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, general_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on explicit, cumulative, multisensory phonics-based instruction to acquire decoding skills that do not develop through incidental exposure. Where structured literacy is implemented with fidelity, failure-to-read rates drop sharply; where it is absent, these students are structurally locked out of literacy with no self-directed remediation path. They have no capacity to demand the instruction themselves — dependent entirely on adult decision-makers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Also benefit from systematic phonics and cumulative skill-building even without a diagnosed disability, though the marginal benefit is smaller than for dyslexic students; some lose engagement time that alternative approaches might have spent on rich text exposure and motivation-building.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_students, beneficiary,
    moderate, biographical, constrained, national).

% Bear the direct cost of the reading: must complete lengthy, often self-funded specialized certification (60-150+ hours for baseline Orton-Gillingham credentials) on top of existing licensure and continuing-education requirements, often without salary adjustment or paid release time. Cannot simply decline — professional evaluation, state literacy mandates, and parent advocacy pressure make competence in this method a de facto job requirement even for teachers whose classrooms are not dyslexia-specific.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Must fund certification programs, curriculum materials, and specialist staff positions to comply with state dyslexia laws requiring structured literacy, often without commensurate state funding. Districts with weak tax bases face a choice between diverting funds from other programs or remaining out of compliance and facing liability.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, underfunded_school_districts, payer,
    moderate, generational, constrained, regional).

% Certify practitioners, define fidelity standards, and lobby for state adoption mandates. Collect certification and training fees directly. Have institutional incentive to expand the scope of who must be certified and to resist streamlined or lower-cost pathways to competence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies, beneficiary).

% Sell proprietary curricula, decodable text sets, and training materials aligned to structured literacy mandates. Revenue scales directly with the number of districts and teachers required to adopt the approach; benefit from mandate expansion regardless of instructional outcomes.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, og_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Argue that mandating one instructional tradition for all students, including those without diagnosed reading disabilities, overstates the universality claim and crowds out balanced approaches that also produce competent readers with less certification overhead. Largely excluded from state legislative mandate processes that have already codified structured literacy requirements.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_and_balanced_literacy_advocates, excluded,
    organized, biographical, constrained, national).

% Study comparative efficacy across instructional approaches, including meta-analyses of Orton-Gillingham fidelity implementations versus alternatives. Their findings are cited selectively by advocates on all sides of the kernel contest.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_credentialing_bodies).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine and previously badly-neglected problem: without explicit, systematic, cumulative phonological and phonics instruction, a substantial minority of students (dyslexic and otherwise) never reliably acquire decoding skill through incidental or context-driven exposure alone. Structured literacy coordinates curriculum sequencing, teacher practice, and assessment around a single evidence-aligned instructional logic.
% TRANSFER_FUNCTION: Moves training time, certification cost, and compliance burden from state education agencies and credentialing bodies onto individual teachers and district budgets, in exchange for reduced downstream failure-to-read outcomes among students, particularly those with dyslexia. Certification fees and curriculum licensing revenue flow to credentialing bodies and publishers.
% ABSENT_VOICES: Whole-language and balanced-literacy practitioners and researchers are largely outside the legislative rooms where state dyslexia laws have codified structured literacy as the mandated approach; general education teachers as a class have limited collective bargaining leverage specifically over pedagogical certification burdens, which are usually treated as professional obligations rather than negotiable working conditions.
% DISAPPEARANCE_RATIONALE: Advocates for dyslexic students argue the world would rearrange sharply and badly — schools would revert to approaches under which a known-effective subgroup intervention silently disappears and reading failure rates among at-risk students climb. Critics of the mandate's universal scope argue that outside the diagnosed-disability population, removing the certification and curriculum mandate would leave general literacy outcomes largely unchanged, since competent teaching under several traditions produces comparable results for typically-developing readers.
% FOUNDING_PROBLEM: Students with dyslexia and related reading disabilities were being taught with methods (whole language, incidental phonics) that did not work for their neurocognitive profile, producing high rates of reading failure, misdiagnosis as low-ability or unmotivated, and long-term educational and economic harm.
% FOUNDING_PROBLEM_CORROBORATION: Special education researchers and dyslexia advocacy organizations (largely aligned with the beneficiary side) attest the founding problem remains live and undersolved in many districts. Independent large-scale reading-outcomes researchers and some general-education teacher unions attest that while the dyslexia-specific problem remains real, the mandate's extension to universal application and its certification-heavy compliance architecture increasingly serves credentialing-body and publisher revenue interests beyond what the founding problem requires.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) is authored mid-range and rising: the coordination function for dyslexic students is real and substantial (this reading's strongest empirical claim), but the extension of certification and curriculum mandates to the general teaching workforce, tracked over the interval as state dyslexia laws proliferated, has grown the extraction on teachers and districts faster than the population actually needing the specialized intervention. Suppression (0.48) reflects that alternatives (balanced or phonics-only approaches) are not fully foreclosed — teachers and districts retain some discretion in many jurisdictions — but state mandates and liability exposure increasingly narrow that discretion. Theater ratio (0.22) is kept modest: fidelity-monitoring and certification renewal do carry some performative compliance overhead, but the underlying instructional function is not primarily theatrical. Accessibility collapse (0.4) and resistance (0.55) reflect a still-contested space: teachers unions, balanced-literacy advocates, and some researchers actively resist the universalized mandate, so alternatives have not collapsed as they would in a settled natural-law-like constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia sit near the full-beneficiary end: they receive the intervention's core benefit and bear essentially no compliance cost themselves (their exit option, 'trapped,' reflects dependency rather than extraction — they cannot self-provide the instruction, but the constraint subsidizes rather than extracts from them). General education teachers and underfunded districts sit toward the target end: they bear certification time, cost, and compliance liability that scales with mandate breadth rather than with their own classroom's population of dyslexic students. Credentialing bodies and curriculum publishers sit at the clear beneficiary end with arbitrage-grade exit — they can expand into new jurisdictions as mandates spread and face little downside if fidelity outcomes are mixed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) preserves the fact that the coordination function for dyslexic students is genuine and well-evidenced — this is not extraction wearing a coordination costume for that population. But the same structure imposes real, asymmetric costs on general education teachers and districts once the mandate is generalized beyond its founding population, satisfying the tangled_rope gate (coordination + enforcement + identifiable victims) rather than either a clean rope (no victims) or a pure snare (no genuine coordination function). Labeling this a snare would erase the real gains to dyslexic students; labeling it a rope would erase the certification-and-curriculum rent structure that has grown around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_reading_or_phonics_variant,
    'Is structured_literacy_reading a genuinely distinct fourth reading of the literacy acquisition kernel, or is it structurally a variant of phonics_reading distinguished mainly by its certification and credentialing apparatus rather than by a different account of how reading is acquired?',
    'Compare the two readings'' core instructional claims independent of institutional apparatus: if the acquisition theory (what causes reading skill to develop) is identical to phonics_reading and only the credentialing/mandate layer differs, they may be the same underlying constraint with different enforcement architectures rather than genuinely different kernel readings.',
    'If structured_literacy_reading collapses into phonics_reading, its distinctively high certification-driven extractiveness should be understood as an enforcement-layer add-on to a shared acquisition theory, not evidence of a fundamentally different reading of the kernel — this would consolidate two constraint stories into one with a noted enforcement-intensity variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_reading_or_phonics_variant, conceptual, 'Whether this is a fourth kernel reading or a certification-heavy variant of phonics_reading.').

omega_variable(
    universal_applicability_evidence_gap,
    'Does the evidence base supporting Orton-Gillingham/structured literacy for dyslexic students actually generalize to justify mandating it as the required approach for the general student population, or does the ''applicable universally'' claim outrun what the founding evidence supports?',
    'Comparative effect-size meta-analysis of structured literacy versus balanced/phonics-only approaches specifically among students without diagnosed reading disabilities.',
    'If the universal-applicability claim is not well-supported for typically-developing readers, the extraction imposed on general education teachers and districts (certification, curriculum cost) for the non-dyslexic majority of students lacks the coordination justification that anchors this reading''s claim to be more than a rent-extracting credentialing regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_evidence_gap, empirical, 'Whether universal-population mandate scope is evidence-supported or outruns the founding population''s evidence base.').

omega_variable(
    credentialing_capture_trajectory,
    'Are credentialing bodies and curriculum publishers actively lobbying to expand mandate scope and certification requirements beyond what outcome data justifies, in a manner consistent with regulatory/professional capture?',
    'Track legislative lobbying records, certification requirement changes over time, and curriculum adoption mandates against contemporaneous outcome data publication timing.',
    'If capture dynamics are confirmed, the rising extractiveness trajectory in the temporal measurements should be read as rent-seeking accumulation rather than as evidence-driven mandate refinement, strengthening the tangled_rope classification''s victim-side asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_capture_trajectory, empirical, 'Whether mandate and certification expansion reflects capture rather than evidence-driven refinement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Four sibling constraint stories decompose the literacy_acquisition_kernel per the ε-invariance principle: phonics_reading (decoding-first sequencing, moderate ε, no certification lineage), whole_language_reading (context-driven acquisition, contested low-to-moderate ε depending on whose failure outcomes are counted), balanced_literacy_reading (complementary-approaches framing, lowest ε among the four given its explicit anti-mandate stance), and this story, structured_literacy_reading (highest ε among the four due to its certification/credentialing architecture layered atop a phonics-aligned acquisition theory). This reading most directly influences phonics_reading (shared decoding-first acquisition theory; the fourth-reading-or-variant omega documents the open question of whether they are the same underlying constraint) and structurally competes with whole_language_reading and balanced_literacy_reading for legislative mandate space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
