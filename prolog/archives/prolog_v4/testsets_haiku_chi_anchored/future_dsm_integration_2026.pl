% ============================================================================
% CONSTRAINT STORY: future_dsm_integration_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_future_dsm_integration_2026, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: future_dsm_integration_2026
 *   human_readable: Future DSM Strategic Vision (SCE-DoH & Intersectionality)
 *   domain: technological/political
 *
 * SUMMARY:
 *   The APA's empaneling of a subcommittee to integrate socioeconomic,
 *   cultural, and environmental determinants (SCE-DoH) and intersectionality
 *   into the DSM-5-TR and future DSM revisions represents a contested
 *   institutional moment. Ostensibly, the project responds to decades of
 *   critique that DSM diagnoses naturalize socially produced harms as
 *   individual pathology, disproportionately affecting marginalized
 *   populations. However, the constraint exhibits a fundamental tension: the
 *   proposal to integrate social context into a categorical diagnostic
 *   framework that was architecturally designed to standardize around
 *   decontextualized symptom clusters. This creates a tangled hybrid of
 *   coordination (genuinely solving the problem of diagnostic communication
 *   across settings) and extraction (maintaining the medical model's monopoly
 *   on legitimate knowledge while appearing to incorporate structural
 *   analysis). The constraint operates across eight distinct perspectives,
 *   revealing how the same institutional proposal appears as pure extraction
 *   to those excluded from diagnostic epistemology, as temporary
 *   institutional scaffolding to alternative medicine advocates, as degraded
 *   ritual (piton) to those dependent on insurance billing codes, and at risk
 *   of appearing as natural law to observers who naturalize the medical
 *   diagnostic monopoly.
 *
 * KEY AGENTS:
 *   - APA Standardization Authority: Institutional beneficiary (institutional/arbitrage) — controls diagnostic standard-setting; defines what counts as valid clinical knowledge
 *   - Pharmaceutical Industry: Powerful beneficiary (powerful/arbitrage) — extracts market definition and indication targeting from DSM categories; resists heterogeneity
 *   - Marginalized Patient Populations: Primary victim (powerless/trapped) — cannot exit diagnostic frame; face surveillance through documentation of social determinants without resource to address them
 *   - Clinical Practice Establishment: Institutional beneficiary (institutional/constrained) — DSM standardization enables billing, EMR compatibility, treatment coordination; threatened by diagnostic heterogeneity
 *   - Community-Based Clinicians: Secondary victim (moderate/constrained) — experience dual burden: documentation of social context without resources; constrained by resource barriers
 *   - Structural Critique Frameworks (Critical Psychiatry, Mad Studies): Victim (moderate/constrained) — epistemologically excluded from DSM standard-setting; cannot compete in medical publishing venues; framings excluded from diagnostic legitimacy
 *   - Insurance/Billing System: Institutional actor (institutional/constrained) — maintains piton through locked-in infrastructure; requires categorical codes
 *   - Intersectionality Implementation Coalition: Organized agents (organized/constrained) — academics, advocates, lived-experience networks pushing for genuine structural reform; building alternative diagnostic pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(future_dsm_integration_2026, 0.52).
domain_priors:suppression_score(future_dsm_integration_2026, 0.65).
domain_priors:theater_ratio(future_dsm_integration_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(future_dsm_integration_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(future_dsm_integration_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(future_dsm_integration_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(future_dsm_integration_2026, tangled_rope).
narrative_ontology:human_readable(future_dsm_integration_2026, "Future DSM Strategic Vision (SCE-DoH & Intersectionality)").
narrative_ontology:topic_domain(future_dsm_integration_2026, "technological/political").

domain_priors:requires_active_enforcement(future_dsm_integration_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, diagnostic_standardization_institutions).
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, clinical_practice_establishment).
narrative_ontology:constraint_victim(future_dsm_integration_2026, marginalized_patient_populations).
narrative_ontology:constraint_victim(future_dsm_integration_2026, structural_critique_frameworks).
narrative_ontology:constraint_victim(future_dsm_integration_2026, alternative_diagnostic_epistemologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY INVISIBLE PATIENT (SNARE) — Marginalized populations (low-income, racialized, gender-nonconforming) remain trapped within DSM diagnostic frames designed around normative assumptions. The constraint appears to offer integration (SCE-DoH, intersectionality) but maintains gate-keeping: diagnosis still requires alignment with medical model categories. No alternative epistemic pathway exists outside DSM classification for insurance coverage, treatment access, or institutional recognition. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(future_dsm_integration_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY-BASED CLINICIAN (TANGLED ROPE) — Coordination function: DSM provides common diagnostic language enabling treatment coordination across settings. Extraction: clinicians in under-resourced settings face disproportionate burden documenting social determinants while lacking resources to address them; documentation becomes surveillance rather than care. d≈0.68, f(d)≈1.04, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(future_dsm_integration_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: APA STANDARDIZATION AUTHORITY (ROPE) — Experiences constraint as pure coordination: DSM-5 establishment enabled global mental health standardization, insurance billing, research comparability. SCE-DoH integration is perceived as solving a coordination problem (aligning diagnosis with contextual factors) rather than addressing extraction. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through institutional control.
constraint_indexing:constraint_classification(future_dsm_integration_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Coordination function: DSM classification enables disease identification and drug indication targeting. Extraction: SCE-DoH integration threatens to create diagnostic heterogeneity (same presentation, different social context = different diagnosis), complicating pharmacological marketing and RCT standardization. Industry advocates for DSM-standardized criteria while opposing outcome measurement that attributes pharmaceutical failure to untreated social determinants. d≈0.15, f(d)≈0.02, σ=1.2 → χ≈0.01. Minimal effective extraction because pharmaceutical framing already controls the outcome definition.
constraint_indexing:constraint_classification(future_dsm_integration_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STRUCTURAL CRITIQUE FRAMEWORK (SNARE) — Critical psychiatry and Mad Studies frameworks argue that DSM medicalizes structural oppression, naturalizing as individual pathology what are socially produced harms. SCE-DoH integration is presented as addressing this critique but maintains the medical diagnostic monopoly: adding 'context' to diagnosis without challenging whether diagnosis is the appropriate epistemic frame. Critique communities are excluded from standard-setting; their epistemologies cannot compete in medical journal publishing venues. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(future_dsm_integration_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERSECTIONALITY IMPLEMENTATION COALITION (SCAFFOLD) — Organized advocates, academic researchers, and lived-experience networks are pushing for genuine DSM reform: not just adding checkboxes for social determinants, but fundamentally restructuring diagnostic frames to center structural analysis. The constraint is temporary — alternative diagnostic frameworks (trauma-informed care, social medicine, participatory epistemology) are emerging as viable institutional pathways. The sunset: as these alternatives mature institutionally, DSM's monopoly on diagnosis weakens. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22.
constraint_indexing:constraint_classification(future_dsm_integration_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INSURANCE/BILLING SYSTEM DEPENDENCE (PITON) — DSM classification persists as the global standard for mental health diagnosis not because it has proven superior diagnostic accuracy, but because insurance, medical records systems, and billing infrastructure have locked into its codes and categories. theater_ratio=0.58 reflects that much contemporary DSM use is administrative theater: diagnosis drives billing category, not clinical formulation. Alternative diagnostic systems (ICD-11 QD-84, formulation-based approaches) exist but cannot displace the installed institutional base. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.44.
constraint_indexing:constraint_classification(future_dsm_integration_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW RISK (MOUNTAIN) — At risk of framing DSM-as-medical-monopoly as immutable natural law: 'any diagnostic system must standardize categories; some epistemic frame always dominates.' This naturalizes what is a contingent institutional arrangement. However, the base properties (ε=0.52, suppression=0.65, theater=0.58, requires_active_enforcement=true) contradict mountain classification. The engine will flag this as a false summit: DSM dominance is maintained by enforcement, not by inherent logical necessity.
constraint_indexing:constraint_classification(future_dsm_integration_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(future_dsm_integration_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(future_dsm_integration_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(future_dsm_integration_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(future_dsm_integration_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(future_dsm_integration_2026, TR),
    TR >= 0.70.

:- end_tests(future_dsm_integration_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The DSM's institutional monopoly on legitimate mental health diagnosis extracts from marginalized populations through diagnostic framing that medicalizes structural oppression, and from alternative epistemologies through exclusion from standard-setting. However, the extraction is not maximally severe because genuine coordination function exists: DSM enables treatment communication and coordination. The rising trajectory (0.35→0.52 over 10 years) reflects increasing extraction as social determinant coding becomes mandatory documentation burden without corresponding resource allocation. Suppression (0.65): Moderate-high. Significant barriers to exiting DSM dominance include insurance billing system lock-in, medical training gatekeeping, research publication venues requiring DSM categories, and therapeutic licensing tied to medical model credentials. Alternative diagnostic frameworks exist but face institutional suppression through publication bias and funding concentration in medical model research. Theater ratio (0.58): Moderate-high. Diagnostic assessment rituals (symptom checklists, severity rating scales, clinical interviews) are partially performative — they produce billable categories and research data, but the clinical formulation process (understanding the patient's lived experience within social context) often diverges from diagnostic coding. The rising trajectory (0.42→0.58) reflects that DSM usage increasingly serves administrative (billing, metrics) rather than clinical (actual understanding) functions as EHRs require categorical coding before clinical notes are written.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival disagreement across the observation site. The APA institutional perspective sees pure coordination (Rope) — solving the legitimate problem of diagnostic communication. The pharmaceutical industry sees low extraction (Tangled Rope with χ≈0.01) — existing medical framing already controls outcomes definition; SCE-DoH integration is a minor adaptation. The marginalized patient population sees pure extraction (Snare, χ≈0.72) — diagnostic framing medicalizes structural oppression while excluding alternative epistemologies. The structural critique framework sees extraction masked as inclusion (Snare, χ≈0.67) — adding 'context' checkboxes while maintaining medical monopoly. The insurance/billing system sees a degraded ritual (Piton, χ≈0.44) — diagnosis as administrative theater preserved through lock-in. The intersectionality coalition sees temporary institutional scaffolding with a sunset (Scaffold, χ≈0.22) — alternative diagnostic frameworks are emerging. The analytical observer risks seeing natural law (Mountain, false summit) — framing medical monopoly as inevitable. The community-based clinician experiences the genuine hybrid (Tangled Rope, χ≈0.49) — coordination benefits paired with asymmetric burden.
 *
 * DIRECTIONALITY LOGIC:
 *   APA Standardization Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Institutional control of standard-setting produces net benefit; effective extraction is negative because the institution IS the constraint. Pharmaceutical Industry: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Despite beneficiary status, effective extraction is minimal (χ≈0.01) because pharmaceutical framing already controls outcome definitions; SCE-DoH integration adds complexity but doesn't threaten market structure. Community-Based Clinician: Victim + constrained → d≈0.68, f(d)≈1.04. Constrained exit (cannot refuse DSM framework and maintain credentials/insurance coverage) combined with victim status (bears documentation burden) produces high directionality toward target. Marginalized Patient Population: Victim + trapped → d≈0.92, f(d)≈1.38. No alternative epistemic pathway outside DSM for institutional recognition, insurance coverage, treatment access; maximum extraction. Structural Critique Framework: Victim + constrained → d≈0.88, f(d)≈1.28. Epistemologically excluded; constrained by publishing and funding gatekeeping; high extraction. Insurance/Billing System: Mixed (piton classification) → d≈0.55, f(d)≈0.75. Neither fully beneficiary nor victim; persists through institutional inertia. Intersectionality Coalition: Organized + constrained → d≈0.42, f(d)≈0.42. Coalition agency and emerging alternative pathways reduce effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.52 requires resolution for ε>0.46, though <0.70 threshold): The classification as Tangled Rope prevents false conflation of coordination and extraction. The constraint simultaneously solves a genuine coordination problem (enabling diagnostic communication across settings) AND extracts from marginalized populations through diagnostic hegemony and epistemological gatekeeping. The mandatrophy would arise if this were classified as pure Rope (coordination only) — this would miss the extraction of diagnostic monopoly, marginalization of alternative epistemologies, and selective burden on community-based clinicians. Alternatively, misclassifying as pure Snare (extraction only) would miss that DSM genuinely enables clinical coordination. The Tangled Rope classification captures both functions: χ=ε×f(d)×σ(S) varies across perspectives from χ≈-0.06 (APA institutional, net beneficiary) to χ≈0.72 (marginalized patient, target), revealing that the same institutional arrangement distributes coordination benefits and extraction costs unevenly. This uneven distribution IS the extraction mechanism: the coordination function is preserved, but its benefits accrue to standardizing institutions and pharmaceutical industry while costs fall on marginalized populations and alternative epistemologies. SCE-DoH integration appears to solve this (adding structural context) but risks becoming false synthesis if it maintains medical diagnostic monopoly while appearing to incorporate structural critique. The resolve: implementing genuine participatory epistemology in DSM revision (lived-experience networks, structural critique frameworks, alternative medicine practitioners in standard-setting) rather than adding context variables within unchanged categorical logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_epistemology_equivalence,
    'Are DSM diagnostic categories and structural/social medicine epistemologies describing the same phenomena through different frameworks, or are they fundamentally incommensurable?',
    'Comparative outcome analysis: tracking patient trajectories under medical diagnosis vs structural/participatory approaches; epistemological analysis of what each framework counts as valid knowledge',
    'If equivalent: SCE-DoH integration is meaningful reform (scaffold perspective confirmed). If incommensurable: integration is false synthesis that preserves medical hegemony while appearing to incorporate critique (snare perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diagnostic_epistemology_equivalence, conceptual, 'Whether diagnostic frameworks are epistemically equivalent or incommensurable').

omega_variable(
    social_determinant_operationalization,
    'Can social determinants of health be meaningfully operationalized within a nosological framework designed around individual symptom clusters?',
    'Implementation analysis of SCE-DoH pilot projects: whether social determinant assessment drives clinical decision-making or becomes administrative compliance; whether diagnostic codes for social context improve treatment outcomes',
    'If operationalizable: tangled_rope perspective (genuine coordination + extraction) is correct. If not: integration attempt fails, and the constraint remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_determinant_operationalization, empirical, 'Whether SDH can be operationalized within nosological frameworks').

omega_variable(
    intersectionality_compatibility,
    'Is intersectionality (a framework for analyzing overlapping structural oppressions) compatible with DSM categorical logic (diagnostic categories based on symptom clusters)?',
    'Structural analysis of pilot DSM revisions incorporating intersectionality: whether revisions produce genuine diagnostic heterogeneity or whether intersectionality becomes a context variable applied within unchanged categorical structure',
    'If compatible: structural critique is being genuinely incorporated (scaffold). If incompatible: ''intersectionality DSM'' is a marketing frame preserving medical monopoly (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersectionality_compatibility, conceptual, 'Compatibility of intersectionality with categorical diagnostic logic').

omega_variable(
    insurance_system_reform_coupling,
    'Can DSM reform (toward SCE-DoH integration) proceed without parallel reform of insurance billing systems that require categorical diagnosis codes?',
    'Policy analysis of proposed insurance billing changes paired with DSM revisions; historical study of previous DSM revisions and insurance system adaptation lag',
    'If decoupled: DSM reform will be constrained by insurance system inertia (piton perspective). If coupled: genuine institutional change pathway exists (scaffold perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_system_reform_coupling, empirical, 'Whether insurance reform is coupled with DSM reform').

omega_variable(
    alternative_diagnostic_institutional_viability,
    'Can alternative diagnostic frameworks (trauma-informed, social medicine, participatory epistemology) achieve institutional parity with DSM, or will they remain marginalized despite superior outcomes in specific populations?',
    'Longitudinal institutional analysis: tracking adoption rates, research publication volume, insurance coverage, clinical training integration for alternative frameworks; comparison with DSM institutional infrastructure',
    'If viable alternative pathways emerge: scaffold sunset is real (alternative frameworks displace DSM monopoly). If DSM remains hegemonic: constraint persists as tangled_rope/snare permanently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_diagnostic_institutional_viability, empirical, 'Institutional viability of alternative diagnostic frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(future_dsm_integration_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_tr_t0, future_dsm_integration_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dsm_tr_t5, future_dsm_integration_2026, theater_ratio, 5, 0.5).
narrative_ontology:measurement(dsm_tr_t10, future_dsm_integration_2026, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm_be_t0, future_dsm_integration_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsm_be_t5, future_dsm_integration_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dsm_be_t10, future_dsm_integration_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(future_dsm_integration_2026, information_standard).
narrative_ontology:affects_constraint(future_dsm_integration_2026, mental_health_insurance_billing_lock_in).
narrative_ontology:affects_constraint(future_dsm_integration_2026, clinical_training_medical_model_dominance).
narrative_ontology:affects_constraint(future_dsm_integration_2026, pharmaceutical_indication_targeting).

% DUAL FORMULATION NOTE:
% DSM-as-coordination-standard and DSM-as-diagnostic-hegemony are structurally distinct constraints. The coordination function (enabling diagnostic communication) has ε≈0.08-0.15 (Mountain or Rope). The hegemonic function (excluding alternative epistemologies, medicalizing structural oppression) has ε≈0.52 (Tangled Rope). The integration proposal (SCE-DoH, intersectionality) operates on both constraints simultaneously, and its efficacy depends on whether it addresses hegemony or merely adds context variables within hegemonic structure. This story models the hegemonic constraint; a separate story should model coordination function independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(future_dsm_integration_2026, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
