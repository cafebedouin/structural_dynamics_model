% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy as Pathologization of Neurological Variation (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The DSM taxonomy, read through the neurodiversity lens, is a formally
 *   specified diagnostic system that coordinates clinical, administrative,
 *   and research communication while simultaneously pathologizing natural
 *   human neurological variation that conflicts with institutional behavioral
 *   norms. The coordination function (shared diagnostic language for
 *   insurance, law, research, clinical care) is genuine — but the extraction
 *   is asymmetric: neurodivergent individuals bear the costs of
 *   pathologization (coercive normalization, denied accommodation, denied
 *   self-determination) while institutional systems (schools, employers,
 *   carceral systems, insurance) collect the benefits of a taxonomy that
 *   converts difference into deviance requiring management. The constraint
 *   persists through active enforcement: diagnostic gatekeeping,
 *   institutional mandates, pharmaceutical marketing, and the fusion of
 *   diagnostic identity with self-concept that makes exit structurally
 *   difficult.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.75).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.8).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Pathologization of Neurological Variation (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e').
narrative_ontology:cs_kernel_codification('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', formalized).
narrative_ontology:cs_authority_grounding('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', expertise).
narrative_ontology:cs_interpretation_layer_present('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e').
narrative_ontology:cs_reading_relation('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', foundational, neurological_variation_is_natural_not_pathological).
narrative_ontology:cs_axiom_status(neurological_variation_is_natural_not_pathological, holdable).
narrative_ontology:cs_axiom_grounding('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', neurological_variation_is_natural_not_pathological, deontological).
narrative_ontology:cs_axiom('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', foundational, institutional_conformity_drives_pathologization).
narrative_ontology:cs_axiom_status(institutional_conformity_drives_pathologization, holdable).
narrative_ontology:cs_axiom_grounding('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', institutional_conformity_drives_pathologization, empirically_contingent).
narrative_ontology:cs_reference_frame('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', scientific_psychiatry_framework).
narrative_ontology:cs_drift_state('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', contemporary_neurodiversity_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce2e899b-6ee2-4d7b-9dbb-3f50c56c410e', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_requiring_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, insurance_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, diagnostic_standardization_enables_coordination).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, clinical_communication_requires_shared_taxonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to diagnostic categories that frame their neurological variation as pathology. Experience coercive normalization (behavioral interventions, social skills training, masking demands), denial of accommodation (school, workplace, carceral settings), and denial of self-determination (guardianship, involuntary treatment, diagnostic gatekeeping for transition-related care). The diagnostic identity fuses with self-concept — exit means rejecting a framework that structures their self-understanding and access to resources.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    moderate, biographical, identity_locked, global).

% Schools, employers, carceral systems, and child welfare agencies use DSM categories to legitimize conformity demands: special education placement, workplace 'fit' standards, competency determinations, parenting fitness evaluations. They benefit from a taxonomy that converts neurological difference into deviance requiring management. They can shift between diagnostic frameworks (ICD, educational codes, functional assessments) with minimal cost.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_requiring_conformity, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_requiring_conformity, beneficiary).

% Controls diagnostic criteria development through APA committees, field trials, and clinical guidelines. Holds epistemic authority over what counts as disorder. Collects professional status, insurance reimbursement, and legal gatekeeping power from diagnostic monopoly. Constrained exit: individual clinicians can dissent but the profession's institutional position depends on DSM's authority.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter,
    institutional, generational, constrained, global).

% DSM categories structure drug development pipelines, FDA approval pathways, and marketing indications. Expanding diagnostic boundaries (e.g., ADHD adult diagnosis, bipolar spectrum, pediatric bipolar) create new markets. Can pivot to ICD or dimensional frameworks if DSM authority wanes — arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Require DSM codes for reimbursement — the taxonomy operationalizes medical necessity determinations. Benefits from categorical gatekeeping that limits utilization. Can adopt alternative coding systems (ICD, proprietary) if DSM becomes unstable.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, insurance_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Organize against pathologization (autistic self-advocacy, mad pride, psychiatric survivor movements). Demand diagnostic reform, accommodation over normalization, and self-determination. Structurally excluded from DSM revision processes — APA consultation periods do not grant voting power. Their frameworks (neurodiversity paradigm, social model of disability) are treated as advocacy positions rather than epistemic peers.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, excluded,
    organized, biographical, constrained, global).

% Sees the full structure: a formally specified taxonomy that coordinates clinical communication and resource allocation while extracting compliance from neurodivergent people to serve institutional conformity demands. Recognizes both the genuine coordination function and the asymmetric extraction as structurally inseparable in the current formation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides shared diagnostic language enabling clinical communication across providers and institutions; structures insurance reimbursement, disability benefits, special education eligibility, and legal competency determinations; coordinates pharmaceutical development and regulatory approval; enables epidemiological research and cross-site clinical trials.
% TRANSFER_FUNCTION: Moves diagnostic authority and resource allocation power from neurodivergent individuals to institutional systems. Pathologization transfers legitimacy to conformity demands: schools extract compliance through special education frameworks, employers extract 'fit' through fitness-for-duty evaluations, carceral systems extract manageability through competency and risk assessments. Neurodivergent individuals pay with denied self-determination, coercive normalization, and internalized stigma.
% ABSENT_VOICES: Neurodivergent individuals historically excluded from DSM criterion development (no autistic people on DSM-IV or DSM-5 neurodevelopmental disorder workgroups until late-stage consultation). Mad pride and psychiatric survivor movements whose frameworks (iatrogenic harm, social model, neurodiversity paradigm) are treated as political advocacy rather than epistemic contributions. Indigenous and non-Western frameworks of neurological difference (e.g., Māori 'takiwātanga', Two-Spirit understandings) that do not map to DSM categories.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished overnight, clinical communication would lose its shared vocabulary; insurance reimbursement would lack coding infrastructure; disability law (ADA, IDEA) would lose its diagnostic gatekeeping mechanism; pharmaceutical regulation would lose its indication framework; special education would lose its eligibility categories. The world would rearrange — new coordination mechanisms would need to be built, but the extraction would not automatically cease (institutional conformity demands would adopt whatever replacement taxonomy emerges).
% FOUNDING_PROBLEM: Late 19th/early 20th century psychiatry lacked reliable diagnostic communication: different institutions used incompatible classifications; insurance and legal systems had no standard for 'mental disease'; research could not aggregate across sites. The DSM project (from DSM-I 1952 onward) aimed to create a shared nosology for clinical, administrative, and research coordination.
% FOUNDING_PROBLEM_CORROBORATION: Historical records confirm the coordination problem: APA's 1952 DSM-I preface cites 'need for a common language'; Spitzer's DSM-III (1980) explicitly targeted reliability for research and clinical use. Cross-cultural psychiatry critiques (Kleinman, Good, Fabrega) corroborate that the coordination function serves Western institutional interests. Neurodiversity movement testimony (Sinclair 1993, ASAN policy statements) and mad pride archives corroborate that the founding problem persists but the solution pathologizes variation. No corroboration from outside the benefiting parties that the current categorical structure is the only or best solution.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because pathologization itself constitutes harm — the diagnostic label enables coercive interventions, denies self-determination, and structures resource allocation against neurodivergent people. Suppression is very high (0.8) because the taxonomy's persistence depends on active enforcement: institutional mandates requiring DSM codes, pharmaceutical marketing expanding diagnostic boundaries, professional gatekeeping of diagnostic authority, and the internalized fusion of diagnostic identity with selfhood. Theater ratio is moderate (0.4) — the coordination function is real (clinicians do communicate via DSM, insurance does require codes) but a growing share of the taxonomy's expansion (e.g., pediatric bipolar, attenuated psychosis syndrome, disruptive mood dysregulation disorder) serves market and institutional interests rather than clinical validity. Accessibility collapse is high (0.7) because once diagnosed, neurodivergent individuals face structural barriers to accessing neurodiversity-affirming frameworks — the diagnostic label becomes a master status that reorganizes their social world. Resistance is substantial (0.65) from the neurodiversity movement, mad pride, psychiatric survivor movements, and critical psychiatry — but this resistance has not yet fractured the taxonomy's institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (neurodivergent individuals) experiences this as a snare: pathologization with no exit. The agenda_setter seats (psychiatric profession, institutional systems) experience it as a rope: genuine coordination they built and maintain. The beneficiary seats (pharma, insurance) experience it as a mountain: a natural feature of the landscape they exploit. The engine computes this divergence from the structural data — the claimed type (tangled_rope) captures the structural reality that both coordination and extraction are real and inseparable in the current formation.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are full targets (d near 1.0): they bear the extraction, have identity-locked exit (diagnostic identity fuses with self-concept; leaving the framework means losing access to accommodations, community, self-understanding), and face structural barriers to alternative frameworks. Institutional systems requiring conformity are full beneficiaries (d near 0.0): they collect the gains of a taxonomy that legitimizes conformity demands, have arbitrage-grade exit (can switch to ICD, functional assessments, proprietary systems), and administer the constraint. The psychiatric profession sits at agenda_setter with constrained exit — they control the taxonomy but their institutional position depends on its authority. Pharmaceutical industry and insurance systems are beneficiaries with arbitrage exit. Neurodiversity advocates are excluded — they would object but are structurally kept out of revision processes. The analytical observer sees the full structure from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diagnostic coordination) remains live — clinical communication, insurance, law, and research still need shared language. But the current categorical structure has accumulated extraction: diagnostic boundaries expanded to serve pharmaceutical markets (critical psychiatry reading) and institutional conformity demands (this reading). The mandate has not been resolved — the coordination need persists — but the solution has become extractive. The taxonomy is not a piton (it is actively maintained and expanded, not theatrically preserved) but a tangled rope where the coordination function is real and the extraction is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Is the DSM''s genuine coordination function (shared diagnostic language for clinical, insurance, legal, research communication) structurally separable from its pathologization function, or does the coordination require the categorical pathologization of variation?',
    'Natural experiment from dimensional/transdiagnostic frameworks (HiTOP, RDoC, ICD-11 dimensional approaches): if clinical coordination, insurance reimbursement, and legal gatekeeping can function without categorical pathologization, the functions are separable and the extraction is contingent. If dimensional frameworks fail to achieve coordination, the categorical structure may be necessary for the coordination function.',
    'If separable, the current taxonomy is a tangled rope where extraction is layered onto coordination and could be removed via reform. If inseparable, the pathology framework is the price of coordination — moving toward a rope (if coordination dominates) or a snare (if extraction dominates) depending on power dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components of DSM taxonomy are structurally separable.').

omega_variable(
    biomedical_validity_of_categories,
    'Do any DSM categories correspond to discrete neurobiological disease entities with natural boundaries, or are all categories pragmatic constructs that carve continuous variation at institutional convenience?',
    'Convergent evidence from genetics, neuroscience, and longitudinal outcome studies: if distinct biological signatures map onto DSM categories (high specificity/sensitivity), the biomedical reading gains ground. If categories show high heterogeneity, comorbidity, and dimensional structure, the neurodiversity/critical psychiatry readings gain ground.',
    'If categories have biomedical validity, the neurodiversity reading''s claim of ''pathologizing natural variation'' is partially falsified for those categories — the constraint becomes more rope-like (coordination around real entities). If categories lack validity, the extraction is foundational to the taxonomy itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomedical_validity_of_categories, empirical, 'Whether DSM categories track discrete neurobiological entities or are pragmatic constructs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by neurodivergent individuals primarily structural (institutional gatekeeping, legal mandates, professional authority) or internalized (diagnostic identity fusion, self-stigma, epistemic dependency on the framework), or both in what proportion?',
    'Post-diagnosis trajectory studies: if suppression metrics (denied accommodation, coerced normalization, internalized stigma) persist after institutional barriers are removed (e.g., in neurodiversity-affirming communities, post-guardianship), the internalized component is significant. Comparative analysis of diagnosed vs. self-identified neurodivergent populations.',
    'If primarily structural, reform of institutional gatekeeping reduces suppression. If significantly internalized, the constraint''s effective suppression exceeds its structural measure — the taxonomy installs a self-reinforcing suppression that persists after structural removal, requiring epistemic/identity work for liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in diagnostic pathologization.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the neurodiversity reading''s core premise (neurological variation is natural, not pathological) logically foreclose the biomedical reading''s core premise (categories map to objective disease entities) within any single commitment framework, or can a framework hold both for different categories (e.g., autism as variation, schizophrenia as disease)?',
    'Analyze whether any DSM categories are claimed by neurodiversity advocates as potentially valid disease entities (e.g., some advocates distinguish autism from schizophrenia). If the neurodiversity paradigm makes a universal claim about ALL neurological variation, it forecloses the biomedical reading universally. If it makes category-specific claims, they may coexist with different categories assigned to different readings.',
    'If forecloses universally, the kernel has a structural fault line — no single framework can hold both readings. If category-specific coexistence is possible, the kernel''s contest is fragmented and the engine should model category-level readings rather than kernel-level readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether neurodiversity and biomedical readings foreclose each other universally or category-specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t1952, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1952, 0.25).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t1968, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t2000, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.39).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t2024, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t1952, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1952, 0.45).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t1968, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.65).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t2000, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t2024, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t1952, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1952, 0.55).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t1968, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.72).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t2000, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.78).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t2024, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.05).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_drug_market).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, special_education_system).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, disability_benefits_system).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, involuntary_commitment_law).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, guardianship_law).

% DUAL FORMULATION NOTE:
% DSM taxonomy kernel decomposes into three readings: biomedical_reading (Mountain claim: categories map to natural disease kinds), critical_psychiatry_reading (Snare claim: categories constructed for pharmaceutical markets), neurodiversity_reading (this file, Tangled Rope claim: categories coordinate institutions while pathologizing variation for conformity). The biomedical reading claims Mountain status (emerges_naturally=true, low extraction) but declares beneficiaries (psychiatric profession, pharma) — FSM candidate. The critical psychiatry reading claims Snare (victims: patients/consumers; beneficiaries: pharma, managed care). This reading claims Tangled Rope (coordination + extraction). All three share the same kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, moderate, 0.9).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
