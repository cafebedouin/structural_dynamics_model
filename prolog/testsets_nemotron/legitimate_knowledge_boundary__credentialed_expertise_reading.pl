% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Boundary for Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the credentialed_expertise_reading of the
 *   legitimate_knowledge_boundary kernel. It asserts that legitimate
 *   knowledge is produced through methodologically rigorous inquiry validated
 *   by credentialed peer review — a reading that structures academic
 *   disciplines, research funding, publication systems, hiring and promotion,
 *   and policy advisory roles. The constraint coordinates quality control
 *   across distributed knowledge production (a genuine coordination function)
 *   while simultaneously extracting epistemic authority, material resources,
 *   and career capital from knowledge traditions that do not conform to its
 *   methodological and credentialing standards (an asymmetric extraction
 *   function). The coordination story — preventing error, ensuring
 *   reproducibility, enabling cumulative progress — is real and structurally
 *   necessary. The extraction story — excluding community knowledge, lived
 *   experience, indigenous epistemologies, and independent scholarship — is
 *   equally real and structurally necessary for the constraint's persistence.
 *   The constraint requires active enforcement: journals reject
 *   non-conforming manuscripts, funding agencies deny grants to
 *   non-credentialed applicants, hiring committees filter for institutional
 *   pedigree, and policy bodies cite 'lack of peer-reviewed evidence' to
 *   exclude experiential testimony.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.72).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Boundary for Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '0c3a33a4-c6da-4754-bc6b-cecc831bcef5').
narrative_ontology:cs_kernel_codification('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', distributed).
narrative_ontology:cs_authority_grounding('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', practice).
narrative_ontology:cs_interpretation_layer_present('0c3a33a4-c6da-4754-bc6b-cecc831bcef5').
narrative_ontology:cs_reading_relation('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', foundational, methodological_rigor_necessary_for_legitimate_knowledge).
narrative_ontology:cs_axiom_status(methodological_rigor_necessary_for_legitimate_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', methodological_rigor_necessary_for_legitimate_knowledge, deontological).
narrative_ontology:cs_axiom('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', foundational, credentialed_peer_review_sufficient_for_epistemic_legitimacy).
narrative_ontology:cs_axiom_status(credentialed_peer_review_sufficient_for_epistemic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', credentialed_peer_review_sufficient_for_epistemic_legitimacy, conventional).
narrative_ontology:cs_axiom('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', secondary, expert_consensus_as_truth_proxy).
narrative_ontology:cs_axiom_status(expert_consensus_as_truth_proxy, holdable).
narrative_ontology:cs_axiom_grounding('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', expert_consensus_as_truth_proxy, instrumental).
narrative_ontology:cs_reference_frame('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', postwar_scientific_institutionalization).
narrative_ontology:cs_drift_state('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', contemporary_open_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c3a33a4-c6da-4754-bc6b-cecc831bcef5', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, accreditation_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_journals).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_scholars).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_epistemic_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_practitioners).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_as_truth_indicator).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, expert_consensus_as_epistemic_proxy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, institutional_credentialing_as_quality_filter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold advanced degrees and institutional appointments that grant them standing in peer review, grant panels, editorial boards, and policy advisory roles. They define what counts as rigorous method, legitimate evidence, and valid inference. Their careers depend on the constraint's persistence; they can move between institutions but cannot exit the credentialing system without losing epistemic authority. They benefit from the constraint's coordination function (quality communities) and its extraction function (monopoly on legitimacy).
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter).

% Universities, professional associations, and certification bodies that confer credentials, define curricula, and accredit programs. They collect tuition, membership fees, and institutional prestige from maintaining the boundary. Their business model depends on credential scarcity and gatekeeping authority. They face competitive pressure from alternative credentialing (micro-credentials, bootcamps, portfolio-based hiring) but retain monopoly on 'legitimate' credentials in most domains.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, accreditation_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, accreditation_institutions, beneficiary).

% Government and private funders (NIH, NSF, ERC, Gates Foundation, etc.) that allocate research resources through peer review panels composed of credentialed experts. They benefit from a legitimate, defensible allocation mechanism that reduces political vulnerability. They face increasing pressure to fund community-engaged research, indigenous knowledge integration, and open science — but their core allocation machinery remains credential-gated.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies, beneficiary).

% Commercial and society publishers that operate the journal system. They extract substantial rents (APCs, subscriptions) from the credentialing constraint while providing the infrastructure of gatekeeping. Their profit model depends on the constraint's persistence: if legitimacy decouples from journal publication, their value proposition collapses. They resist open access mandates, preprint recognition, and alternative evaluation metrics.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_journals, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_journals, agenda_setter).

% Indigenous communities, local residents, patient groups, and practitioners of traditional/embodied knowledge whose epistemic traditions are excluded by credentialing standards. Their knowledge is often extracted (data, tissues, cultural practices) and re-published by credentialed researchers without co-authorship, benefit-sharing, or epistemic credit. They cannot exit the constraint because their knowledge is about their own lives, lands, and bodies — leaving means abandoning their epistemic sovereignty. The constraint suppresses their alternatives by defining them as 'anecdote,' 'tradition,' or 'bias' rather than knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders, payer,
    powerless, generational, identity_locked, local).

% Researchers without institutional affiliation who produce rigorous work but cannot access funding, publication venues, or policy influence without credentialed partners. They pay the constraint through exclusion from material resources and legitimacy economies. Some exit by securing affiliations; others persist at margins. Their situation reveals that the constraint filters for institutional position, not just methodological rigor.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_scholars, payer,
    moderate, biographical, constrained, global).

% Communities systematically excluded from credentialing pipelines by historical and ongoing structural barriers (race, gender, class, geography, disability). They bear the constraint's extraction (their questions unfunded, their methods dismissed, their data mined) and its suppression (their knowledge traditions delegitimized). They are trapped because the constraint shapes the very institutions that could remediate exclusion — universities, funders, journals — and those institutions are run by the constraint's beneficiaries.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_epistemic_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_epistemic_communities, excluded).

% Practitioners in fields where knowledge is validated through practice and outcome rather than publication: clinicians, engineers, artisans, organizers, caregivers. Their knowledge is treated as 'application' not 'production' — legitimate only when translated into credentialed terms. They pay by having their expertise mediated, delayed, or captured by credentialed intermediaries. Exit means abandoning practice for academia, which few can or will do.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Philosophers of science, STS scholars, metascientists, and historians of knowledge who analyze the constraint from outside its operation. They see the full structure: the genuine coordination function, the asymmetric extraction, the identity-lock dynamics, the network of downstream constraints. They do not collect from the constraint nor pay into it — their situation is analytical clarity.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, epistemology_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of coordinating reliable knowledge claims across strangers in large-scale, distributed knowledge production systems: prevents error propagation, enables cumulative progress, provides defensible basis for policy and practice decisions.
% TRANSFER_FUNCTION: Moves epistemic authority, research funding, publication access, career capital, and policy influence FROM community knowledge holders, independent scholars, marginalized epistemic communities, and experiential practitioners TO credentialed experts, accreditation institutions, funding agencies, and peer review journals — mediated by methodological and credentialing standards.
% ABSENT_VOICES: Indigenous elders, community health workers, patient advocates, independent researchers without PhDs, traditional medicine practitioners, citizen scientists, and Global South knowledge producers who are structurally excluded from credentialing pipelines. They are absent from editorial boards, grant review panels, hiring committees, and policy advisory bodies — the very rooms where the constraint's boundaries are maintained.
% DISAPPEARANCE_RATIONALE: If the credentialed expertise boundary vanished overnight, research funding would need new allocation mechanisms, journals would lose their gatekeeping monopoly, hiring would shift to portfolio/competency evaluation, policy would need new evidence standards, and community knowledge traditions would enter legitimacy economies on equal footing. The coordination problem (reliable knowledge at scale) would not disappear — but the specific institutional solution would, and the extraction flows would redirect.
% FOUNDING_PROBLEM: Post-WWII expansion of science required a scalable, defensible mechanism for quality control across rapidly growing, geographically distributed, discipline-fragmented research enterprises. Credentialed peer review provided a standardized, portable legitimacy signal that enabled cumulative knowledge building and resource allocation without requiring every funder or user to evaluate every claim directly.
% FOUNDING_PROBLEM_CORROBORATION: The credentialed establishment attests the founding problem remains live (replication crisis, misinformation, AI-generated fraud). Critics from STS, decolonial epistemology, open science, and community-based research attest the founding problem has been substantially solved by digital infrastructure (preprints, open data, post-publication review, registered reports) and that the constraint now persists as rent extraction. No neutral arbiter exists — the corroboration split mirrors the constraint's beneficiary/victim structure.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the constraint's dual character: it coordinates genuine quality assurance (which has inherent costs) while extracting authority from alternative epistemic traditions. The trajectory shows rising extraction as credential inflation, journal impact factors, and metric-driven evaluation have intensified gatekeeping. Suppression (0.72) captures both structural barriers (funding, publication, hiring) and the internalized devaluation of non-credentialed knowledge — the latter amplified by identity_locked exit dynamics for many excluded practitioners. Theater ratio (0.41) has risen as performative rigor (p-hacking, citation gaming, methodological theater) has replaced substantive quality control in portions of the system. Accessibility collapse (0.63) is moderate: alternatives exist (preprint servers, community research, open science) but remain marginal in legitimacy economies. Resistance (0.54) reflects active contestation from open science movements, decolonial epistemology, community-based participatory research, and patient-led research — but resistance is fragmented across domains and lacks unified institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed expert seat, the constraint appears as a rope: it solves the genuine coordination problem of distributed quality assurance in knowledge production. From the community knowledge holder seat, it appears as a snare: the coordination story is cover for extracting epistemic authority and resources. The engine computes this divergence from the structural data. The analytical observer sees the tangled rope structure: both functions are real, neither reducible to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts (institutional power, arbitrage exit) are primary beneficiaries: they control gatekeeping, capture career capital, and define methodological norms — directionality near 0.1. Accreditation institutions and peer review journals (institutional power, constrained exit) benefit structurally: they monetize gatekeeping and derive authority from it — directionality near 0.2. Funding agencies (institutional power, mobile exit) benefit from legitimate allocation mechanisms but face pressure to diversify — directionality near 0.25. Community knowledge holders and experiential practitioners (powerless, identity_locked exit) are primary targets: their knowledge is systematically excluded, devalued, or extracted without credit — directionality near 0.9. Independent scholars (moderate, constrained exit) and marginalized epistemic communities (powerless, trapped to constrained exit) bear high extraction with limited recourse — directionality 0.7–0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating reliable knowledge across strangers — remains live (contested status). However, the constraint's current form extracts substantially beyond what coordination requires. The mandatrophy analysis: the constraint began as a scaffold for post-war science organization but has become a tangled rope where extraction is locked in by career structures, funding metrics, and institutional inertia. The mandate has not been resolved; it has been captured. The constraint persists not because the coordination problem is solved, but because the beneficiaries of extraction control the mechanisms that would reform it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimate_knowledge_boundary,
    'Is the credentialed expertise reading a genuine coordination mechanism for reliable knowledge, or a structured exclusion that extracts epistemic authority from alternative knowledge traditions?',
    'Comparative analysis of knowledge production outcomes in domains where credentialed gatekeeping is relaxed (e.g., open science movements, community-based participatory research) vs. domains where it remains entrenched; longitudinal tracking of error rates, innovation velocity, and epistemic justice metrics.',
    'If credentialing primarily coordinates quality, the constraint is a genuine tangled rope. If it primarily extracts authority while suppressing valid alternatives, it trends toward snare. The reading''s classification hinges on this structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimate_knowledge_boundary, conceptual, 'Whether the credentialed expertise reading of the legitimate_knowledge_boundary kernel is a coordination function with necessary exclusion, or an extraction mechanism using coordination as cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding gatekeeping, publication barriers, hiring criteria) or internalized (epistemic self-doubt, imposter phenomenon, devaluation of own knowledge traditions)?',
    'Post-exclusion trajectory studies: track epistemic confidence and knowledge production capacity of practitioners excluded from credentialed pathways. If suppression persists after structural barriers are removed (e.g., via open access mandates, alternative funding), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest — excluded agents carry the constraint with them. This would increase the constraint''s effective extraction on payer seats beyond what base metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in epistemic exclusion.').

omega_variable(
    methodological_rigor_asymmetry,
    'Is methodological rigor enforced symmetrically across all knowledge claims, or asymmetrically — applied strictly to challengers while incumbents'' assumptions go unexamined?',
    'Audit of peer review and funding decisions: compare methodological scrutiny applied to novel/alternative paradigms vs. established frameworks. Measure rejection rates, revision demands, and evidentiary thresholds by epistemic tradition.',
    'Asymmetric enforcement would confirm the extraction function: the constraint coordinates around incumbent paradigms while extracting legitimacy from challengers. Symmetric enforcement would support the coordination-only reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_rigor_asymmetry, empirical, 'Whether methodological standards function as neutral quality filters or as asymmetric barriers protecting incumbent epistemic authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(legi_tr_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1990, 0.29).
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(legi_tr_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2020, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(legi_be_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(legi_be_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(legi_su_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(legi_su_t2020, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_allocation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_hiring_and_promotion).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_evidence_standards).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, clinical_practice_guidelines).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, regulatory_science_standards).

% DUAL FORMULATION NOTE:
% This constraint is one member of the legitimate_knowledge_boundary constraint family. The credentialed_expertise_reading instantiates high barriers and centralized gatekeeping; the experiential_pluralism_reading instantiates distributed validation and methodological pluralism; the hybrid_coproduction_reading instantiates integrative processes. All three share the kernel but diverge on ε (0.68 vs ~0.25 vs ~0.45), beneficiary/victim structures, and coordination_type. Linked via affects_constraints to downstream institutional constraints that implement the boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, institutional, 0.15).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, powerless, 0.88).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
