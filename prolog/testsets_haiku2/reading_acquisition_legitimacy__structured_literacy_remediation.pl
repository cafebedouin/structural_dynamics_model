% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Structured Literacy Remediation as Universal Prevention Principle
 *   domain: education/cognitive_science
 *
 * SUMMARY:
 *   The 'reading-acquisition-legitimacy' kernel is the shared commitment that
 *   reading instruction must be 'legitimate' — grounded in sound pedagogy and
 *   evidence. This story instantiates ONE reading of that kernel: the
 *   structured-literacy-remediation reading, which asserts that legitimate
 *   instruction is explicitly sequenced, diagnostically responsive, and
 *   designed first for students with dyslexia and reading disabilities. The
 *   constraint extracts from general educators and meaning-centered
 *   practitioners by mandating adoption of structured frameworks, suppressing
 *   alternative pedagogies, and transferring professional authority to
 *   specialists and certified practitioners. The constraint coordinates real
 *   services — explicit intervention for dyslexic readers — but
 *   simultaneously extracts through market creation (intervention products
 *   and training), pedagogical authority consolidation, and suppression of
 *   competing legitimate approaches. Sibling readings contest what
 *   'legitimate' means (phonics primacy, meaning-making priority, or balanced
 *   integration); this reading does not resolve the contest, it instantiates
 *   one position within it.
 *
 * KEY AGENTS:
 *   - structured_literacy_practitioners: agenda-setters who define legitimate instruction through frameworks and certification (organized, high exit)
 *   - reading_intervention_industry: commercial beneficiaries of expansion of intervention markets (institutional, arbitrage exit)
 *   - general_education_teachers: payers of autonomy loss and retraining burden (moderate power, constrained exit)
 *   - whole_language_advocates: identity-locked payers whose expertise is delegitimized (moderate power, identity-locked exit)
 *   - under_resourced_schools: trapped payers facing mandates without funding (powerless, trapped exit)
 *   - students_with_dyslexia: beneficiaries of explicit intervention but also surveilled and segregated (powerless, trapped exit)
 *   - early_literacy_researchers: analytical observers producing evidence that shapes which readings dominate institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.72).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation as Universal Prevention Principle").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '962ca43c-e8f0-4d74-a1e3-bb857342f90f').
narrative_ontology:cs_kernel_codification('962ca43c-e8f0-4d74-a1e3-bb857342f90f', distributed).
narrative_ontology:cs_authority_grounding('962ca43c-e8f0-4d74-a1e3-bb857342f90f', expertise).
narrative_ontology:cs_interpretation_layer_present('962ca43c-e8f0-4d74-a1e3-bb857342f90f').
narrative_ontology:cs_reading_relation('962ca43c-e8f0-4d74-a1e3-bb857342f90f', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('962ca43c-e8f0-4d74-a1e3-bb857342f90f', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('962ca43c-e8f0-4d74-a1e3-bb857342f90f', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('962ca43c-e8f0-4d74-a1e3-bb857342f90f', foundational, vulnerability_first_design_principle).
narrative_ontology:cs_axiom_status(vulnerability_first_design_principle, holdable).
narrative_ontology:cs_axiom_grounding('962ca43c-e8f0-4d74-a1e3-bb857342f90f', vulnerability_first_design_principle, deontological).
narrative_ontology:cs_axiom('962ca43c-e8f0-4d74-a1e3-bb857342f90f', foundational, explicit_cumulative_diagnostic_necessity).
narrative_ontology:cs_axiom_status(explicit_cumulative_diagnostic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('962ca43c-e8f0-4d74-a1e3-bb857342f90f', explicit_cumulative_diagnostic_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('962ca43c-e8f0-4d74-a1e3-bb857342f90f', evidence_responsive_pedagogy_explicit_structure).
narrative_ontology:cs_drift_state('962ca43c-e8f0-4d74-a1e3-bb857342f90f', contemporary_policy_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('962ca43c-e8f0-4d74-a1e3-bb857342f90f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, reading_intervention_industry).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_specialists).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, meaning_centered_instructors).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, under_resourced_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_advocacy_organizations).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_specialists).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional organizations, training providers, and researchers who authoritatively define 'legitimate' reading instruction through structured literacy frameworks. Set curriculum standards, train teachers, conduct research that privileges intervention-model assessment. Benefit from certification markets, training contracts, and institutionalized authority over pedagogy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners, agenda_setter,
    organized, generational, arbitrage, national).

% Commercial publishers, software vendors, and intervention programs (Orton-Gillingham, Wilson Reading System, Structured Literacy Online) that produce materials and training under the structured literacy model. Revenue depends on schools and districts adopting remedial intervention tiers; universal prevention framing expands addressable market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, reading_intervention_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Speech-language pathologists, reading specialists, and special educators who deliver intensive intervention in the structured literacy model. They gain professional status and job security as dyslexia and reading disability categories expand under diagnostic precision, but must also adapt practice if frameworks change.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_specialists, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_specialists, payer).

% Classroom teachers tasked with implementing structured literacy principles in core instruction. Face mandated adoption of explicit, cumulative, scripted curricula; reduced autonomy in pedagogical choice; continuous assessment burden to identify 'at-risk' students for intervention. Retraining costs and fidelity compliance pressure are substantial.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Educators and researchers whose professional identity and published work were built on constructivist, meaning-centered reading theory. Structured literacy's rise constitutes delegitimation of their expertise and decades of practice; cannot exit without career and identity reconstruction. Retain influence in some jurisdictions but face institutional marginalization.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_advocates, payer,
    moderate, biographical, identity_locked, national).

% Teachers who practiced literature-centered, student-choice-driven reading instruction and believe in emergent literacy. Forced to adopt explicit phonics and diagnostic assessment regimes they experience as counter to reading enjoyment and meaning-making. Professional identity fusion makes exit costly; excluded from policy decisions that mandate their practice change.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, meaning_centered_instructors, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, meaning_centered_instructors, excluded).

% Schools serving high-poverty, linguistically diverse, or disproportionately disabled student populations. Structured literacy's premise is to serve these students first, but implementation requires funding for structured curricula, specialist training, and intervention tiers that under-resourced districts cannot afford. Policy mandates intervention-grade instruction for all without resource provision.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, under_resourced_schools, payer,
    powerless, generational, trapped, local).

% Children with specific reading disability who benefit from explicit, multisensory, diagnostic-responsive instruction. Structured literacy's principles can prevent early failure and provide appropriate intervention. However, they bear the cost of continuous diagnostic testing and segregation into remedial tiers; normal reading development is also disrupted by constant monitoring and external pacing.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia, payer).

% Cognitive scientists and educational researchers who conduct longitudinal studies on reading acquisition. Measure effectiveness of instructional approaches across diverse populations. Produce evidence that structures policy decisions; no direct stake in outcome but methodological choices (comparison groups, assessment tools, follow-up duration) shape what counts as 'legitimate' evidence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, early_literacy_researchers, observer,
    analytical, generational, analytical, national).

% State education departments and policy boards that mandate structured literacy adoption, set reading standards, allocate professional development funding. Can enforce adoption through certification, accountability measures, and funding conditions. Increasingly pressured by advocacy organizations and research summaries to adopt structured literacy as the sole legitimate framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, policy_makers_state_education, agenda_setter,
    institutional, generational, analytical, national).

% Nonprofit and parent-led organizations (International Dyslexia Association, Decoding Dyslexia state chapters) that advocate structured literacy as the evidence-based, morally imperative standard. Leverage parent activism, legislative testimony, and research citation to institutionalize structured literacy. Benefit from visibility and policy influence; structure their mission around this reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes reading instruction around explicit, cumulative, diagnostic principles so that children with dyslexia and other reading disabilities receive appropriate intervention early; prevents failure through universal prevention-first design that teaches all students using intervention-grade methods.
% TRANSFER_FUNCTION: Transfers pedagogical authority from classroom teachers (meaning-centered, student-responsive) to structured literacy practitioners (explicit-sequence, fidelity-monitored). Moves professional development funding from generalist teacher training to specialist certification. Shifts instructional time from authentic literature and student choice toward scripted cumulative sequences and continuous diagnostic assessment.
% ABSENT_VOICES: Students who develop reading fluency through literature exposure without explicit phonics instruction; teachers and researchers whose entire professional practice rests on constructivist, meaning-centered theory; under-resourced schools unable to afford specialist training and intervention tiers; general classroom teachers whose autonomy and professional judgment are suspended in favor of compliance with structured scripts.
% DISAPPEARANCE_RATIONALE: If the structured literacy mandate vanished, general education would revert to meaning-centered and whole-language approaches in many districts; specialist intervention markets would shrink; teacher training would diversify again; assessment would shift from diagnostic precision toward classroom observation and running records. The reading-acquisition landscape would reorganize around local teacher judgment and diverse valid approaches.
% FOUNDING_PROBLEM: Children with dyslexia and significant phonological deficits were being instructed in meaning-centered frameworks where decoding was presumed to 'emerge' naturally, resulting in persistent reading failure and misidentification as low-ability. Structured literacy's explicit, diagnostic approach was developed to prevent this failure through early intervention targeting specific deficits.
% FOUNDING_PROBLEM_CORROBORATION: Dyslexia researchers and specialist educators attest the founding problem is live and structured literacy addresses it effectively. General education researchers and meaning-centered educators attest the problem is overstated — that reading difficulty is often situational, not inherent disability, and that prevention through strong classroom instruction and literature access is more effective and equitable than early identification and remediation. Evidence comes from both camps; the contest is genuine.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 across the interval as structured literacy gains institutional adoption and policy mandate. The constraint begins as professional framework (specialist adoption, genuine coordination around intervention need) and evolves into enforced pedagogy (state mandates, teacher retraining pressure, diagnostic precision expansion). Theater ratio rises from 0.18 to 0.41, indicating that as extraction increases, performative assessment and compliance activity grows relative to actual instruction — continuous diagnostic monitoring becomes theater when used to justify intervention tiers rather than adaptive classroom practice. Suppression requirement rises from 0.48 to 0.72 as the constraint faces resistance from meaning-centered educators and must actively exclude alternative legitimate framings from policy consideration. Accessibility collapse is moderate (0.71) because alternative literacy approaches remain available in some districts and countries, but collapse within U.S. policy circles is nearly complete. Resistance is moderate (0.58) because many teachers comply from genuine belief in structured literacy's effectiveness and because dyslexia advocacy has real legitimacy claims; resistance comes from advocates of equity and meaning-making rather than from uniform teacher opposition.
 *
 * PERSPECTIVAL GAP:
 *   The constraint computes very differently across seats. From the structured-literacy-practitioners and dyslexia-advocacy seats, this is a rope — genuine coordination to serve students with dyslexia, justified by evidence and morally imperative. From the general-education-teacher seat and the under-resourced-school seat, this is a snare — mandated adoption without resources, loss of autonomy, and suppression of approaches that work for students who are not dyslexic. From the meaning-centered-instructor seat, this is a piton — a performative apparatus that persists through authority pressure and internalized suppression despite diminished function (meaning-making happens regardless of method, but the explicit-sequence mandate treats it as a secondary luxury). The engine computes these divergent types from the structural data (power, exit, beneficiary/victim declaration) — the authored claim of tangled_rope asserts that the constraint genuinely coordinates (serves dyslexic readers) AND asymmetrically extracts (from teachers, under-resourced schools, and competing pedagogies). The per-seat computed types will reveal whether this asymmetry resolves as rope (coordination dominates) or snare (extraction dominates) from each vantage point.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured literacy practitioners: d ≈ 0.1 (powerful, arbitrage exit, beneficiaries — low directionality, strong beneficiary position). Intervention industry: d ≈ 0.15 (institutional, arbitrage exit, beneficiaries — direct gain from market expansion). General education teachers: d ≈ 0.8 (moderate power, constrained exit, victims — high directionality, coerced compliance). Whole language advocates: d ≈ 0.85 (moderate power, identity_locked exit, victims — expertise delegitimized, cannot exit). Under-resourced schools: d ≈ 0.95 (powerless, trapped exit, victims — mandates without resources). Special education specialists: d ≈ 0.5 (moderate power, constrained exit, mixed beneficiary/payer — professional elevation through expansion but also retraining burden and diagnostic precision pressure). Students with dyslexia: d ≈ 0.65 (powerless, trapped exit, mixed beneficiary/payer — benefit from structured approach but bear cost of constant surveillance and segregation). The overweight of high-d victims in the stakeholder set drives the constraint toward snare-flavored extraction despite genuine coordination at the beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows classic mandatrophy dynamics. Founding problem (dyslexic students failing in meaning-centered classrooms) was real and urgent. Structured literacy's solution was professionally sound and empirically defensible for that population. However, the policy mandate has extended the framework to ALL students (founding_problem_status: contested — some educators and researchers argue the problem was always smaller than claimed or that alternatives work equally well). The constraint now persists partly through institutional inertia, partly through genuine continued function for dyslexic students, and increasingly through supplier capture (intervention industry) and credential protection (practitioners). Theater ratio rising to 0.41 suggests that diagnostic monitoring and compliance documentation now occupy substantial space relative to actual instruction. The founding_problem_status × disappearance_verdict mismatch (status=contested, verdict=world_rearranges) indicates a zombie constraint — the original founding problem's contestedness means the constraint's persistence is no longer justified by its original rationale, yet institutional structures and resource flows keep it in place. Mandatrophy is not resolved; the constraint persists because it benefits organized constituencies (practitioners, vendors, specialists) despite diffuse costs to teachers and schools.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_vs_prevention_scope,
    'Does structured literacy''s ''design for the most vulnerable first'' mandate apply to the entire student population (universal prevention framework) or primarily to identified at-risk students (remedial intervention framework)?',
    'Policy document analysis and implementation audit in districts adopting structured literacy: examine whether all students receive explicit, cumulative instruction or only flagged students; measure classroom time allocation and curriculum fidelity data.',
    'If universal prevention (all students, intervention-grade instruction), the constraint''s scope widens and extraction increases (all teachers must comply, no alternatives). If remedial-identified (only at-risk), the constraint''s extraction concentrates on special ed and intervention tiers but general ed retains more autonomy. This reading asserts the universal interpretation; the sibling readings may not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_vs_prevention_scope, conceptual, 'Whether structured literacy is a universal prevention model or targeted intervention model.').

omega_variable(
    kernel_reading_alternative_legitimacy,
    'Which reading of the reading-acquisition-legitimacy kernel is correct? Does legitimate instruction privilege: (a) explicit decoding (phonics_decoding_primacy), (b) explicit structure with cumulative sequence (this reading, structured_literacy_remediation), (c) meaning-making with balance (balanced_literacy_integration), or (d) meaning-making first, decoding emergent (whole_language_meaning_primacy)?',
    'Meta-analysis of longitudinal reading outcomes across diverse student populations (including dyslexic, typical, and linguistically diverse learners) over 5+ years, comparing instructional approaches on reading fluency, comprehension, and reader identity/engagement. Requires specification of outcome weighting (decoding accuracy vs. comprehension vs. reading motivation) and student subgroup analysis.',
    'This reading asserts that structured literacy''s explicit, cumulative, diagnostic approach is the legitimate framework. Competing readings assert alternative legitimacy claims. The measurement drift (extractiveness rising from 0.42 to 0.68 over the interval) may reflect institutional adoption of this reading as dominant, compression of alternative framings, or genuine evidence accumulation — the omega documents that the kernel itself is contested and outcomes data may not resolve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_legitimacy, empirical, 'Which pedagogical reading of reading acquisition is empirically valid and which parties'' claims about effectiveness are corroborated.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural — external mandates, accountability pressure, funding tied to adoption — or internalized — teachers accepting structured literacy''s framing of meaning-centered instruction as suboptimal or harmful?',
    'Post-mandate qualitative study of teachers in jurisdictions adopting structured literacy: measure attitude shift independently of compliance. If suppression persists after mandates ease (teachers continue avoiding meaning-centered methods even absent external pressure), the suppression is partially internalized.',
    'If structural, removing mandates would restore teacher autonomy and alternative approaches. If internalized, the constraint''s effective suppression is higher than the institutional measure suggests — teachers carry the suppression with them even absent external enforcement. The suppression omega indicates the constraint has both elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized teacher suppression under structured literacy adoption.').

omega_variable(
    reading_disability_natural_vs_constructed,
    'Is dyslexia and reading disability a natural neurocognitive category that structured literacy correctly targets, or is it partly a construction enabled by diagnostic precision and deficit framing?',
    'Cross-cultural and historical comparison of dyslexia prevalence and literacy intervention needs; examine whether structured literacy''s rise has expanded dyslexia identification rates; qualitative study of students labeled dyslexic in structured-literacy vs. whole-language dominant jurisdictions.',
    'If natural, structured literacy legitimately serves a real population need. If constructed, the constraint''s claim to serve ''the most vulnerable'' may actually constitute them as vulnerable through diagnostic practices. This reading''s vindication proposition (orthographic_mapping_explicit) rests on the natural-category framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_disability_natural_vs_constructed, conceptual, 'Whether reading disability is a natural neurocognitive category or partly a diagnostic construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.24).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.31).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.4).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_identification_and_intervention).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_professional_autonomy_curriculum).

% DUAL FORMULATION NOTE:
% The reading-acquisition-legitimacy kernel has four distinct readings. Each is a separate constraint story with its own ε, stakeholder structure, and computed type. This story (structured_literacy_remediation) stands in structural relationship to its siblings: it forecloses phonics_primacy (both claim explicit-structure-first, but this adds 'design-for-vulnerability-first' which is incompatible with phonics_primacy's universalist claim); coexists with whole_language (they partition legitimate pedagogy and compete across districts); influences balanced_literacy (this reading's rise has pressured balanced approaches to adopt more structure). All four stories share the kernel 'reading_acquisition_legitimacy' and the network edges map the reading_relations declared in cs_structure. The upstream constraint (fundamental neurocognitive facts about dyslexia and orthographic mapping) influences this story; this story influences downstream constraints (teacher retraining, curriculum mandates, intervention markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
