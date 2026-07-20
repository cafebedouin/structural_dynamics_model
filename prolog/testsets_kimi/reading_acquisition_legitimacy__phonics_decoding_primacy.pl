% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-Decoding Primacy in Reading Instruction Policy
 *   domain: educational_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the phonics_decoding_primacy reading of the
 *   contested kernel reading_acquisition_legitimacy. It treats reading as
 *   fundamentally a decoding process and legitimate instruction as the
 *   explicit, systematic teaching of the alphabetic principle through
 *   scripted phonics curricula. When embedded in state policy, this reading
 *   creates a mandate that enforces a single instructional ontology,
 *   marginalizes competing approaches, and redirects public education markets
 *   toward decodable-text publishers and universal screening vendors. The
 *   constraint coordinates large-scale reading instruction around a uniform
 *   evidence claim while asymmetrically extracting teacher autonomy,
 *   epistemic pluralism, and instructional resources from students who do not
 *   fit the standardized sequence.
 *
 * KEY AGENTS:
 *   - state_legislators_and_agencies (institutional/constrained): Enact and enforce phonics mandates, audits, and vendor specifications.
 *   - phonics_curriculum_publishers (powerful/mobile): Capture mandated district purchasing as direct beneficiaries.
 *   - early_screening_assessment_vendors (powerful/mobile): Capture recurring assessment revenue from universal screening requirements.
 *   - science_of_reading_researchers (organized/identity_locked): Set the evidentiary agenda and benefit from policy influence and research funding.
 *   - classroom_teachers (moderate/constrained): Lose pedagogical autonomy to scripted curricula and fidelity metrics.
 *   - diverse_learners (powerless/trapped): Bear standardized instruction mismatched to linguistic and cognitive diversity.
 *   - balanced_literacy_educators (organized/constrained): Structurally excluded from policy and teacher preparation accreditation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.72).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-Decoding Primacy in Reading Instruction Policy").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "educational_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, 'cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f').
narrative_ontology:cs_kernel_codification('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', formalized).
narrative_ontology:cs_authority_grounding('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', expertise).
narrative_ontology:cs_interpretation_layer_present('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f').
narrative_ontology:cs_reading_relation('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', foundational, alphabetic_principle_is_cognitive_foundation).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_cognitive_foundation, holdable).
narrative_ontology:cs_axiom_grounding('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', alphabetic_principle_is_cognitive_foundation, empirically_contingent).
narrative_ontology:cs_axiom('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', foundational, systematic_explicit_phonics_is_instructionally_mandatory).
narrative_ontology:cs_axiom_status(systematic_explicit_phonics_is_instructionally_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', systematic_explicit_phonics_is_instructionally_mandatory, instrumental).
narrative_ontology:cs_reference_frame('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', explicit_phonics_first_classroom).
narrative_ontology:cs_drift_state('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', contemporary_policy_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf88cb3d-6d24-485a-90bf-47ab9e6e1e0f', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, early_screening_assessment_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, diverse_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact laws and administrative rules mandating evidence-based reading instruction, universally defined as explicit systematic phonics. They allocate funding for new curricula, require vendor-approved screening tools, and audit district compliance. Reversing these mandates risks political backlash from parent advocacy groups and science-of-reading media campaigns.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_legislators_and_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Produce and sell decodable text series, scope-and-sequence guides, and teacher scripts aligned with state mandates. Their revenue grows directly with policy mandates that require districts to replace existing materials. They lobby for legislation that specifies criteria matching their product lines.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Sell universal screening tools that operationalize decoding-focused progress monitoring. Districts are required by state policy to purchase and administer these assessments to all students at regular intervals, creating a recurring revenue stream tied to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_screening_assessment_vendors, beneficiary,
    powerful, biographical, mobile, national).

% Produce and disseminate research framing systematic phonics as the only evidence-based approach. They advise state departments of education, testify before legislatures, and set the evidentiary boundaries for what counts as legitimate reading science. Their professional standing and funding are tied to the policy adoption of their recommendations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers, beneficiary).

% Must abandon established instructional practices and adopt scripted phonics curricula, often with mandated pacing guides and fidelity checks. They lose autonomy to adjust instruction for individual students and are evaluated on compliance with district-mandated scope and sequence rather than on student growth or relationship.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% English learners, speakers of non-dominant dialects, and students with profiles that do not match the standardized phonics sequence are subjected to the same mandated instruction and screening. They may be over-identified for intervention or relegated to repetitive decoding drills that do not address their linguistic assets or comprehension needs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, diverse_learners, payer,
    powerless, biographical, trapped, local).

% Advocate for integrating phonics within authentic literature and meaning-making contexts. Their professional organizations have been defunded or decertified in states with phonics mandates, their publications removed from approved reading lists, and their teacher preparation programs denied accreditation for including cueing systems or leveled texts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_educators, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production of basic literacy at scale by specifying a uniform, evidence-based sequence of letter-sound instruction, ensuring that all students receive explicit decoding training regardless of local teacher preference or institutional tradition.
% TRANSFER_FUNCTION: Moves public education funding and teacher labor toward standardized phonics curricula and screening assessments; moves professional autonomy from classroom teachers to curriculum publishers and state agencies; moves students with diverse learning needs into standardized intervention tracks.
% ABSENT_VOICES: Whole language educators, balanced literacy practitioners, bilingual education researchers, and culturally responsive literacy scholars are structurally excluded from policy advisory roles and teacher preparation accreditation; they would argue that meaning-making and linguistic diversity require integrated approaches, but are labeled pseudoscientific or relegated to minor markets such as private tutoring and homeschool networks.
% DISAPPEARANCE_RATIONALE: If the phonics-decoding-primacy mandate vanished overnight, school districts would revert to heterogeneous instructional models, curriculum purchasing would shift away from decodable-text publishers, teacher preparation programs would rebalance toward meaning-centered and culturally responsive methods, and the early assessment industry would lose its policy-mandated market â the literacy instruction landscape would reorganize around pluralism rather than curricular monoculture.
% FOUNDING_PROBLEM: Persistently low reading achievement and high illiteracy rates, particularly attributed to whole-language approaches that failed to teach explicit letter-sound correspondence, leaving many students unable to decode text efficiently.
% FOUNDING_PROBLEM_CORROBORATION: Science of reading researchers and state education agencies attest the problem is live, citing stagnant NAEP scores. Balanced literacy advocates and critical literacy scholars attest the founding problem is misdiagnosed â attributing low achievement to poverty, underfunding, and curricular narrowing rather than instructional method â and that the phonics mandate is a solution in search of a problem. Independent reading researchers outside the phonics advocacy network offer mixed corroboration, with meta-analyses showing phonics helps on average but effect sizes vary dramatically by context and learner profile.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the mandate redirects public funds to commercial phonics products and extracts teacher professional judgment. Suppression (0.72) is high due to state bans on three-cueing and balanced literacy, decertification of non-compliant teacher preparation programs, and social-media stigmatization of alternative methods. Theater_ratio (0.45) reflects significant performative compliance: districts purchase new materials and run training sessions to satisfy audit criteria, while actual classroom practice often adapts the scripts. Accessibility_collapse (0.58) captures the narrowing of legitimate alternatives in public schools, though private and homeschool options persist. Resistance (0.62) reflects ongoing organized opposition from balanced literacy networks, teacher unions, and critical literacy scholars. The temporal series trace a cyclical arc: modest extraction during the 1990s whole-language era, a spike under Reading First (t=6), a backlash decline (t=12), and a steep resurgence driven by the science-of-reading social media movement and state legislation (t=18â30).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state agencies, phonics researchers) experience the constraint as evidence-based reform that solves a genuine coordination failure (how to teach reading consistently). The payer seats (teachers, diverse learners) experience the same structure as a coercive standardization that overrides local knowledge and mismatches individual needs. The excluded seats (balanced literacy educators) experience it as epistemic closure â their exclusion is not a side effect but a structural feature necessary for the constraint's coherence. The engine computes these divergent per-seat types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislators and phonics researchers sit low-d: they subsidize and enforce the constraint but do not bear its costs. Curriculum publishers and assessment vendors sit very low-d: they are pure beneficiaries collecting rents from the mandate. Classroom teachers sit high-d: they are direct targets of the fidelity enforcement, paying with autonomy. Diverse learners sit highest-d: they are powerless, trapped, and subjected to standardized intervention regardless of fit. Balanced literacy educators sit high-d as excluded targets of professional suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   If classified as a Rope, the commercial capture and suppression of teacher judgment would be invisible. If classified as a Snare, the genuine coordination value of explicit phonics for many students would be erased. Tangled Rope is the structurally honest classification: it registers both the real collective-action solution (uniform decoding instruction at scale) and the asymmetric extraction (autonomy loss, commercialization, epistemic monoculture) that requires active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_universality_vs_specificity,
    'Does the scientific evidence support phonics as universally primary for all learners, or is its efficacy concentrated in specific populations (monolingual English speakers, non-dyslexic students) while alternative approaches serve others better?',
    'Large-scale randomized trials with subgroup analysis by learner profile (ELL status, dialect, disability) compared to structured literacy and balanced literacy arms.',
    'If efficacy is population-specific, the mandate extracts from mismatched learners and is a snare; if universal, extraction is lower and it is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_universality_vs_specificity, empirical, 'Whether phonics primacy is a universal cognitive necessity or a population-specific intervention.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the suppression of alternative literacy methods structural (state bans on balanced literacy, decertification of teacher prep programs) or internalized (teachers adopting phonics identity, stigmatizing prior practice)?',
    'Post-policy reversal trajectory in states that repeal mandates; if internalized suppression persists, the constraint''s effective suppression outlives the structural mechanism.',
    'If internalized, the constraint''s persistence is more deeply anchored and resistance measures may understate actual control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized suppression mechanism in literacy policy.').

omega_variable(
    kernel_reading_ontology,
    'Is reading acquisition fundamentally a decoding process with comprehension as emergent, or is decoding one of several parallel processes (meaning-making, visual, syntactic) that legitimate instruction must integrate?',
    'Cognitive neuroscience consensus on the necessity and sufficiency of phonological processing for reading acquisition across orthographies.',
    'Resolving this determines whether phonics primacy is a mountain-like cognitive law or a tangled rope policy construct benefiting specific curricula and assessment vendors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ontology, conceptual, 'Whether the alphabetic principle is a discovered cognitive universal or a constructed pedagogical frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 6, 0.2).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.25).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 18, 0.32).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 24, 0.4).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(read_be_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(read_be_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(read_su_t6, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(read_su_t18, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel reading_acquisition_legitimacy. The kernel conflates ontological claims about what reading IS (decoding vs meaning-making) with normative claims about legitimate instruction. Decomposing the kernel into separate constraints per reading enables Îµ-invariant classification of each instantiated policy regime. This reading treats decoding as primary; its siblings treat meaning-making or integration as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
