% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Structured Literacy / Orton-Gillingham Instructional Mandate
 *   domain: educational psychology / literacy pedagogy / cognitive science
 *
 * SUMMARY:
 *   This story instantiates the structured_literacy_reading claim within the
 *   literacy_acquisition_kernel: reading acquisition requires explicit,
 *   systematic, cumulative, multisensory instruction across phonological
 *   awareness, phonics, fluency, vocabulary, and comprehension, in the
 *   Orton-Gillingham tradition, originally designed for dyslexic learners and
 *   increasingly mandated for all teachers and students. The kernel's other
 *   readings (phonics_reading, whole_language_reading,
 *   balanced_literacy_reading) are separate constraints with their own ε and
 *   are NOT part of this story's classification. Structured literacy differs
 *   from plain phonics_reading in scope (it bundles fluency, vocabulary, and
 *   comprehension into one certified pedagogical protocol) and in its
 *   institutional apparatus (a certification and training industry
 *   gatekeeping who may deliver it), which is the structural basis for
 *   treating it as a distinct reading rather than a phonics variant, though
 *   that distinction is itself contested and is routed to an omega below.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: primary beneficiary, powerless, trapped in whatever instructional regime their school provides
 *   - certified_structured_literacy_specialists: agenda-setters who both administer and professionally benefit from the certification requirement
 *   - og_training_and_certification_organizations: institutional beneficiary capturing training revenue as mandates spread
 *   - general_education_teachers: primary payer, bearing uncompensated training burden under legislative mandate
 *   - school_districts_with_limited_training_budgets: institutional payer, absorbing compliance costs without matching funding
 *   - curriculum_publishers_aligned_to_science_of_reading: secondary beneficiary capturing adoption-driven revenue
 *   - balanced_and_whole_language_practitioners: excluded party, professionally devalued by the mandate without a seat in this reading's enforcement structure
 *   - independent_reading_researchers: analytical observers, split on the universal-application evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.44).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy / Orton-Gillingham Instructional Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational psychology / literacy pedagogy / cognitive science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '210efc18-cea6-46ea-b1f2-353a0bf52217').
narrative_ontology:cs_kernel_codification('210efc18-cea6-46ea-b1f2-353a0bf52217', distributed).
narrative_ontology:cs_authority_grounding('210efc18-cea6-46ea-b1f2-353a0bf52217', practice).
narrative_ontology:cs_interpretation_layer_present('210efc18-cea6-46ea-b1f2-353a0bf52217').
narrative_ontology:cs_reading_relation('210efc18-cea6-46ea-b1f2-353a0bf52217', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('210efc18-cea6-46ea-b1f2-353a0bf52217', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('210efc18-cea6-46ea-b1f2-353a0bf52217', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('210efc18-cea6-46ea-b1f2-353a0bf52217', foundational, dyslexia_requires_specialized_multisensory_protocol).
narrative_ontology:cs_axiom_status(dyslexia_requires_specialized_multisensory_protocol, holdable).
narrative_ontology:cs_axiom_grounding('210efc18-cea6-46ea-b1f2-353a0bf52217', dyslexia_requires_specialized_multisensory_protocol, empirically_contingent).
narrative_ontology:cs_axiom('210efc18-cea6-46ea-b1f2-353a0bf52217', secondary, universal_teacher_certification_justified_by_subpopulation_evidence).
narrative_ontology:cs_axiom_status(universal_teacher_certification_justified_by_subpopulation_evidence, holdable).
narrative_ontology:cs_axiom_grounding('210efc18-cea6-46ea-b1f2-353a0bf52217', universal_teacher_certification_justified_by_subpopulation_evidence, instrumental).
narrative_ontology:cs_reference_frame('210efc18-cea6-46ea-b1f2-353a0bf52217', dyslexia_specific_remediation_protocol).
narrative_ontology:cs_drift_state('210efc18-cea6-46ea-b1f2-353a0bf52217', post_science_of_reading_legislative_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('210efc18-cea6-46ea-b1f2-353a0bf52217', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, certified_structured_literacy_specialists).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, og_training_and_certification_organizations).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_training_budgets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, curriculum_publishers_aligned_to_science_of_reading).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, explicit_systematic_phonics_instruction_reduces_reading_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on explicit, cumulative, multisensory instruction to acquire decoding skills that do not develop through incidental exposure. Without structured literacy intervention, many experience persistent reading failure with lasting academic and psychological cost; the intervention converts a high-failure trajectory into a viable one. They have no ability to choose their instructional method and are entirely dependent on whether their school has trained staff.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Hold Orton-Gillingham or equivalent certification, administer intervention programs, and often advocate for legislative mandates requiring structured literacy training. Their professional standing, salary premium, and referral pipeline depend on the certification credential remaining a gatekept, actively-taught requirement rather than a folded-in general competency.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, certified_structured_literacy_specialists, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, certified_structured_literacy_specialists, beneficiary).

% Sell multi-level certification coursework, practicum supervision, and continuing-education credits to teachers and districts. Revenue scales directly with the number of educators required to obtain and maintain certification; legislative mandates for structured literacy training are a direct revenue driver.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, og_training_and_certification_organizations, beneficiary,
    organized, generational, arbitrage, national).

% Increasingly required by state literacy laws to complete structured literacy / OG-aligned coursework and demonstrate competency, often unpaid or under-compensated, on top of existing certification and classroom duties. Cannot decline without risking licensure consequences in mandate states; leaving the profession is the only real exit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Must fund coach training, substitute coverage for training days, and curriculum replacement to comply with structured literacy mandates, frequently without matching state appropriations. Rural and low-tax-base districts absorb this as a disproportionate share of already-strained budgets.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_training_budgets, payer,
    moderate, generational, constrained, regional).

% Have rebranded or newly developed curricula marketed as 'structured literacy aligned' and 'science of reading' compliant, capturing state adoption dollars as mandates spread; benefit whether or not the underlying instructional shift improves outcomes.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, curriculum_publishers_aligned_to_science_of_reading, beneficiary,
    powerful, generational, arbitrage, national).

% Teachers and reading specialists trained in balanced literacy or whole language approaches now find their methods legislatively disfavored or banned in mandate states. Their professional judgment and prior training investment are devalued by statute without direct negotiation; they are the losing party in the kernel contest but are not named as parties to this reading's enforcement structure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_and_whole_language_practitioners, excluded,
    moderate, biographical, trapped, national).

% Study meta-analytic evidence on decoding instruction, dyslexia intervention efficacy, and the generalizability of dyslexia-specific protocols to general education populations. Some corroborate the dyslexia-intervention efficacy claim; many are skeptical of universal application and of the certification-industry incentive structure riding on top of it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, independent_reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of an explicit, sequenced, cumulative decoding-instruction protocol validated for students whose reading acquisition does not proceed through incidental exposure — solving a genuine, well-evidenced failure mode (dyslexia and related decoding difficulty) that generic classroom instruction does not reliably address.
% TRANSFER_FUNCTION: Moves training time, compliance labor, and district budget from general education teachers and their employing districts to certification bodies, curriculum publishers, and structured-literacy specialists, in exchange for (contested) improved reading outcomes for students, especially those with diagnosed or suspected dyslexia.
% ABSENT_VOICES: Balanced literacy and whole language practitioners whose training and professional identity are devalued by mandate are structurally outside the enforcement conversation — they are litigants and lobbyists against the mandate in some states, but this constraint's own operation does not seat them. Non-dyslexic struggling readers who might respond equally well to less resource-intensive instruction are also not directly represented; their outcomes are used as evidence by both sides without their own voice in the debate.
% DISAPPEARANCE_RATIONALE: Structured-literacy advocates and dyslexia-intervention families would say the world rearranges catastrophically for students with decoding disabilities, who would lose access to evidence-supported intervention. Districts and general-education teachers under mandate burden would say the world stays largely the same for the majority of readers who acquire decoding skills under multiple instructional approaches, while the compliance and certification burden simply disappears. Whether disappearance rearranges the world depends on which population's outcomes are weighted, which is exactly the contested empirical and political question the kernel dispute turns on.
% FOUNDING_PROBLEM: A meaningful minority of students, historically including many with dyslexia, failed to acquire functional reading skills under generic classroom-exposure or meaning-emphasis instruction, with severe and often permanent downstream academic and life consequences.
% FOUNDING_PROBLEM_CORROBORATION: Dyslexia researchers and intervention specialists outside the certification-selling organizations (e.g., independent cognitive science labs studying phonological processing deficits) corroborate that the founding problem — decoding failure in a specific subpopulation — remains live and that explicit, systematic intervention measurably improves outcomes for that subpopulation. Independent literacy researchers are split on whether the problem, as originally scoped to dyslexic students, justifies universal mandate application to all students and all teachers; some state education audits describe the universal-mandate framing as having outrun the original dyslexia-specific evidence base.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.52) is authored moderate-high and rising: the underlying decoding-intervention function for dyslexic students is well-evidenced and low-extraction on its own, but the expected structural delta specifically calls out extraction concentrating on general education teachers via training mandates — that is the dominant driver of the extractiveness trajectory over the interval as more states adopt mandate legislation. Suppression (0.44) reflects licensure and statutory consequences for teachers who do not complete certification in mandate states, rising over time as more states harden the requirement, but it stops well short of a snare-level suppression because alternative-method practitioners can still exit the profession or relocate to non-mandate states. Theater ratio (0.28) is moderate: much of the training activity is functionally real (evidence-based decoding instruction is genuinely transmitted), but a growing share is rebranding and compliance-credentialing activity that trails the actual instructional shift, which is why theater_ratio rises across the interval alongside extractiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the certification specialist and training-organization seats, this is legitimate professional standard-setting protecting a vulnerable population. From the general education teacher and district seats under unfunded mandate, the identical structure is an imposed compliance burden that shifts cost downward without proportional support. The engine computes these divergent seat classifications from the declared power/exit/scope data; the claimed_type (tangled_rope) is authored to reflect that both readings are structurally accurate simultaneously, not that one seat is simply mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia are the clearest structural beneficiaries — the intervention converts a documented high-failure trajectory into a viable one, and they have no agency over whether it is delivered, so their directionality sits near the beneficiary end despite their powerlessness. General education teachers and their districts are the structural payers: they bear the training burden and compliance risk without a proportional say in whether the mandate structure is calibrated to actual classroom need versus certification-industry expansion. The certification and publishing organizations occupy the agenda-setter/beneficiary role even though they do not directly administer classrooms, because they draft standards, lobby for mandate legislation, and capture the resulting revenue.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decoding failure among dyslexic and dyslexia-adjacent students under prior instructional regimes — remains genuinely live, corroborated by independent cognitive science research outside the certification-selling organizations. This blocks a clean mandatrophy verdict: the coordination function has not simply evaporated while the apparatus persists. What is contested is scope-creep: whether the mandate, sunset-free and expanding to universal teacher certification, has outrun the population (dyslexic learners) for whom the evidence is strongest, converting a targeted intervention into a general-population compliance regime that captures rents beyond its original justification. The tangled_rope classification reflects that both the coordination function AND the asymmetric extraction are real and co-occurring, not that the coordination function is fake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_vs_phonics_reading_distinctness,
    'Is structured_literacy_reading a structurally distinct kernel reading from phonics_reading, or is it a certification-industry variant riding on the same underlying instructional claim?',
    'Compare outcome data and cost structures for districts that adopt bare systematic phonics curricula without OG-style certification mandates versus districts that adopt full structured-literacy certification regimes; if outcomes are statistically indistinguishable but costs diverge sharply, the distinctness is administrative/institutional rather than pedagogical.',
    'If not structurally distinct, the certification and training apparatus captured by og_training_and_certification_organizations should be classified as extraction layered onto phonics_reading rather than a genuine fourth reading, materially raising phonics_reading''s own extractiveness and potentially reclassifying part of this story''s beneficiary set as illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_vs_phonics_reading_distinctness, conceptual, 'Whether structured literacy is a genuinely distinct reading or a phonics variant with added certification extraction.').

omega_variable(
    dyslexia_specific_vs_universal_application_evidence_gap,
    'Does the evidence base supporting structured literacy for dyslexic students generalize to justify universal application to all students and mandatory certification for all general education teachers?',
    'Meta-analytic comparison of effect sizes for structured literacy protocols in diagnosed-dyslexic populations versus general education populations; audit state mandate legislation for whether it cites dyslexia-specific studies to justify universal-teacher-training requirements.',
    'If the evidence does not generalize, the extractiveness measured against general_education_teachers and school_districts is largely unjustified by the founding problem and the tangled_rope classification tips toward snare on the teacher-facing side; if it generalizes well, the tangled_rope''s coordination function extends more broadly than the dyslexia-specific framing suggests, tempering the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dyslexia_specific_vs_universal_application_evidence_gap, empirical, 'Whether dyslexia-specific efficacy evidence justifies universal mandate scope.').

omega_variable(
    certification_industry_capture_of_legislative_mandate,
    'To what extent have og_training_and_certification_organizations and aligned publishers shaped state ''science of reading'' legislation to expand certification requirements beyond what outcome evidence supports?',
    'Trace lobbying records, model-legislation authorship, and financial ties between certification/publishing organizations and state literacy mandate bill sponsors across adopting states.',
    'Strong evidence of legislative capture would shift the classification of the certification and publishing beneficiary seats from incidental beneficiaries of a genuine coordination function toward active extractors using the dyslexia-intervention evidence as cover, pushing the overall story toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(certification_industry_capture_of_legislative_mandate, empirical, 'Whether legislative mandate expansion reflects evidence-driven policy or industry capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% literacy_acquisition_kernel decomposes into (at least) four sibling readings: phonics_reading, whole_language_reading, balanced_literacy_reading, and this story, structured_literacy_reading. Each has its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because the natural-language label 'how children learn to read' conflates structurally distinct claims about instructional sequencing, evidentiary scope, and institutional apparatus. This story's distinguishing structural feature — a dedicated certification-and-training industry layered on top of the underlying instructional claim — is itself contested as either a genuine fourth reading or an extractive variant of phonics_reading (see omega structured_literacy_vs_phonics_reading_distinctness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
