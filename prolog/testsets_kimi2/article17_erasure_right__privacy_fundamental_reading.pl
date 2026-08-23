% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Right to Erasure â Privacy Fundamental Reading
 *   domain: technology_governance/legal
 *
 * SUMMARY:
 *   Article 17 of the GDPR, read as an instantiation of individual data
 *   sovereignty as a fundamental right, creates a legal entitlement for
 *   individuals to obtain erasure of personal data from platform operators.
 *   This reading treats the constraint as a legitimate limitation on
 *   corporate data retention, grounded in the EU Charter of Fundamental
 *   Rights, with individuals as primary beneficiaries and platforms as
 *   structurally constrained parties bearing compliance costs. The constraint
 *   is actively enforced by data protection authorities and courts, and its
 *   interpretation has broadened over time to lower epistemic friction for
 *   data subjects.
 *
 * KEY AGENTS:
 *   - data_subjects: Primary beneficiary (moderate/mobile) â exercise erasure rights with low friction
 *   - dominant_platforms: Primary payer (institutional/constrained) â bear high-volume compliance and infrastructure costs globally
 *   - small_platforms: Secondary payer (moderate/constrained) â bear disproportionate compliance costs relative to scale
 *   - data_protection_authorities: Agenda setter (institutional/analytical) â interpret and enforce the right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.63).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.72).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure â Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/legal").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '4c2ff296-06bd-4404-a93b-6135685292b9').
narrative_ontology:cs_kernel_codification('4c2ff296-06bd-4404-a93b-6135685292b9', formalized).
narrative_ontology:cs_authority_grounding('4c2ff296-06bd-4404-a93b-6135685292b9', lineage).
narrative_ontology:cs_interpretation_layer_present('4c2ff296-06bd-4404-a93b-6135685292b9').
narrative_ontology:cs_reading_relation('4c2ff296-06bd-4404-a93b-6135685292b9', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c2ff296-06bd-4404-a93b-6135685292b9', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('4c2ff296-06bd-4404-a93b-6135685292b9', foundational, data_sovereignty_fundamental_right).
narrative_ontology:cs_axiom_status(data_sovereignty_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('4c2ff296-06bd-4404-a93b-6135685292b9', data_sovereignty_fundamental_right, deontological).
narrative_ontology:cs_axiom('4c2ff296-06bd-4404-a93b-6135685292b9', secondary, proportionality_favors_data_subject).
narrative_ontology:cs_axiom_status(proportionality_favors_data_subject, holdable).
narrative_ontology:cs_axiom_grounding('4c2ff296-06bd-4404-a93b-6135685292b9', proportionality_favors_data_subject, conventional).
narrative_ontology:cs_reference_frame('4c2ff296-06bd-4404-a93b-6135685292b9', individual_data_sovereignty).
narrative_ontology:cs_drift_state('4c2ff296-06bd-4404-a93b-6135685292b9', contemporary_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c2ff296-06bd-4404-a93b-6135685292b9', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, dominant_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, small_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals exercise a statutory right to demand erasure of personal data from platforms, with low filing cost and broad interpretive support. They gain control over their digital footprint without needing individual litigation or technical expertise.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    moderate, biographical, mobile, continental).

% Large platforms operate global data-processing infrastructure subject to broad EU erasure obligations. They must maintain technical systems to identify, propagate, and verify deletion across complex data stores, backup regimes, and third-party processors. Exiting the EU market is structurally available but commercially prohibitive.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, dominant_platforms, payer,
    institutional, generational, constrained, global).

% Smaller platforms and startups lack dedicated legal and engineering resources to handle broad, low-friction erasure requests at scale. Compliance costs consume disproportionate share of revenue, and technical deletion requirements exceed their infrastructure capacity.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, small_platforms, payer,
    moderate, biographical, constrained, continental).

% National supervisory authorities interpret Article 17, adjudicate complaints, and levy fines for non-compliance. They sit within the EU legal framework, shaping how broadly the right is read and how much epistemic friction requesters face.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes individual control over personal data across all EU data controllers, solving the collective action problem in which individuals cannot negotiate deletion terms individually against platforms with superior bargaining power and information asymmetry.
% TRANSFER_FUNCTION: Transfers compliance burden, technical cost, and legal risk from individuals to platform operators, and shifts decisive control over data persistence from platforms to data subjects.
% ABSENT_VOICES: Content creators and other users who benefit from persistent shared data are less audible in the fundamental-rights framing; non-EU data controllers who would prefer lower compliance standards are structurally excluded from the EU legislative conversation.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, platforms would revert to indefinite data retention as default, individual control would collapse to standard-form contract terms, and the EU data protection framework would lose its most direct individual remedy.
% FOUNDING_PROBLEM: Individuals lacked effective control over personal data processed by powerful digital platforms; data retention was effectively perpetual and governed by platform-dictated terms, creating severe informational power asymmetry.
% FOUNDING_PROBLEM_CORROBORATION: Civil society privacy advocates and academic data protection scholars attest to ongoing informational asymmetry from outside the platform beneficiary set; platform operators contest the severity, arguing that market self-regulation and user control tools already suffice.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.63 reflects moderate-high extraction from platforms in the form of compliance infrastructure, legal review, and technical deletion workflows. Suppression at 0.72 reflects the active legal coercion that prevents platforms from opting out of erasure obligations or setting their own retention defaults. Theater ratio at 0.35 acknowledges that some platform compliance is performative (surface deletion without backend propagation) but the core right produces real outcomes. Accessibility collapse at 0.65: once platforms understand the legal framework, alternatives to compliance collapse into legal penalty. Resistance at 0.55: platforms lobby for narrower interpretation and build technical friction, but do not openly defy the framework.
 *
 * PERSPECTIVAL GAP:
 *   The data_subject seat experiences the constraint as empowerment and restored agency, computing toward coordination or low-extraction subsidy. The platform_operator seats experience the same legal text as asymmetric cost imposition and operational disruption, computing toward tangled_rope or snare. The engine derives this divergence from identical structural data via opposed directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Data_subjects are declared beneficiaries with low directionality (near 0.0), receiving structural subsidy from the constraint's operation. Platform_operators are declared victims/payers with high directionality (near 1.0), legally trapped and identity-locked into their role as data controllers subject to EU jurisdiction. The divergence is structural: the same statute produces negative effective extraction for individuals and positive effective extraction for platforms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â informational asymmetry and perpetual platform-controlled retention â remains live, as attested by external privacy scholars and civil society. The constraint has not atrophied; it is actively invoked and enforced. Classifying it as piton would be incorrect because the coordination function (individual control) is not theatrical â it produces genuine erasure outcomes. Classifying it as pure snare would miss the genuine coordination problem solved: without Article 17, individuals cannot bargain for deletion at scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_ambiguity,
    'Does Article 17 function primarily as individual data sovereignty, competitive moat, or censorship mechanism?',
    'Cross-reading empirical analysis comparing erasure request patterns, platform compliance cost distribution, and speech impact studies across jurisdictions with varying interpretive frameworks.',
    'Determines whether the constraint''s primary structural effect is coordination (privacy), extraction (incumbent protection), or suppression (speech restriction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_ambiguity, conceptual, 'Which sibling reading captures the dominant structural effect').

omega_variable(
    friction_cost_asymmetry,
    'Does low epistemic friction for erasure requests create asymmetric extraction by imposing disproportionate verification and deletion costs on platforms relative to the individual''s request cost?',
    'Comparative cost accounting of request processing versus request filing across platform size classes and DPA complaint records.',
    'If costs are highly asymmetric, the constraint operates as tangled_rope with extraction amplified for smaller platforms; if roughly symmetric, it operates closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_cost_asymmetry, empirical, 'Whether request friction asymmetry drives platform extraction').

omega_variable(
    naturalness_of_data_sovereignty,
    'Is individual data sovereignty a pre-political moral fact that law recognizes, or a constructed regulatory allocation?',
    'Philosophical and comparative legal analysis of whether data sovereignty claims hold across legal traditions absent specific statutory instantiation.',
    'If pre-political, the constraint''s legitimacy is closer to mountain; if constructed, it remains a coordination-dependent legal rope or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_data_sovereignty, conceptual, 'Whether data sovereignty is natural law or legal construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_priv_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(art17_priv_tr_t1, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(art17_priv_tr_t2, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(art17_priv_tr_t3, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(art17_priv_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(art17_priv_tr_t5, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(art17_priv_tr_t6, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(art17_priv_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(art17_priv_be_t1, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(art17_priv_be_t2, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(art17_priv_be_t3, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(art17_priv_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(art17_priv_be_t5, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(art17_priv_be_t6, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 6, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(art17_priv_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(art17_priv_su_t1, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 1, 0.52).
narrative_ontology:measurement(art17_priv_su_t2, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(art17_priv_su_t3, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 3, 0.63).
narrative_ontology:measurement(art17_priv_su_t4, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement(art17_priv_su_t5, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(art17_priv_su_t6, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% The article17_erasure_right kernel decomposes into three structurally distinct constraints: privacy_fundamental_reading (this file), competitive_moat_reading, and censorship_mechanism_reading. Each reading assigns different epsilon, beneficiary structure, and classification to the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
