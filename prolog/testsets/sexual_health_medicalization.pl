% ============================================================================
% CONSTRAINT STORY: sexual_health_medicalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sexual_health_medicalization, []).

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
 *   constraint_id: sexual_health_medicalization
 *   human_readable: Sexual Health Medicalization
 *   domain: healthcare/sexuality/pharmaceutical
 *
 * SUMMARY:
 *   Sexual health medicalization—the process by which sexual variation and
 *   sexual function have been increasingly integrated into medical diagnostic
 *   categories, pharmaceutical treatment protocols, and specialist medical
 *   practice—exhibits the signature of a tangled rope constraint: genuine
 *   coordination benefits (medical treatment for people experiencing distress
 *   or physiological dysfunction) layered with significant asymmetric
 *   extraction (pathologization of diversity, pharmaceutical gatekeeping,
 *   narrowing of legitimacy to medical frames). The constraint has
 *   intensified over the past 60 years as the pharmaceutical industry
 *   developed treatments for erectile dysfunction, female sexual
 *   interest/arousal disorder, and other categories, and as medical
 *   specialization created urology and sexual medicine as professional
 *   domains. The theater_ratio (0.68) reflects that diagnostic rituals
 *   (standardized questionnaires, sexual history assessments, imaging
 *   studies) are substantially performative—they legitimize the pathology
 *   frame but do not resolve underlying questions about whether sexual
 *   variation is dysfunction. The constraint manifests differently across six
 *   perspectives: as snare for those trapped in a diagnostic frame once
 *   medicalization occurs; as tangled rope for those seeking treatment
 *   benefits while constrained by the narrowed frame; as rope for
 *   pharmaceutical manufacturers for whom medicalization is pure market
 *   coordination; as scaffold for advocacy movements building alternative
 *   legitimacy pathways with generational sunset potential; as piton for the
 *   DSM/ICD diagnostic infrastructure persisting through institutional
 *   inertia; and as false mountain for the analytical observer who might
 *   naturalize medicalization as inherent to biology.
 *
 * KEY AGENTS:
 *   - Medicalized Individuals: Primary victims (powerless/trapped) — those whose sexual function or variation has been labeled pathological; bear extraction without exit pathways
 *   - Patients Seeking Treatment: Secondary victims/participants (moderate/constrained) — experience genuine coordination benefit (medical access) alongside extraction (narrowed frame, pharmaceutical dependency)
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — central to medicalization extraction; each diagnosis creates profitable market
 *   - Sexual Medicine Practitioners: Secondary beneficiaries (institutional/arbitrage) — professional domain created by medicalization; direct career benefit from pathology frame
 *   - LGBTQ+ and Sex-Positive Advocates: Organized resistance (organized/constrained) — building alternative legitimacy pathways; sunset logic based on depathologization precedent (homosexuality removal from DSM)
 *   - Medical Diagnostic Bureaucracy: Institutional maintenance (institutional/arbitrage) — DSM/ICD categories persist through credentialing, billing codes, research funding
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as biological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sexual_health_medicalization, 0.58).
domain_priors:suppression_score(sexual_health_medicalization, 0.52).
domain_priors:theater_ratio(sexual_health_medicalization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sexual_health_medicalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(sexual_health_medicalization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sexual_health_medicalization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sexual_health_medicalization, tangled_rope).
narrative_ontology:human_readable(sexual_health_medicalization, "Sexual Health Medicalization").
narrative_ontology:topic_domain(sexual_health_medicalization, "healthcare/sexuality/pharmaceutical").

domain_priors:requires_active_enforcement(sexual_health_medicalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sexual_health_medicalization, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(sexual_health_medicalization, urology_specialization).
narrative_ontology:constraint_beneficiary(sexual_health_medicalization, sexual_medicine_practitioners).
narrative_ontology:constraint_victim(sexual_health_medicalization, sexual_autonomy_and_diversity).
narrative_ontology:constraint_victim(sexual_health_medicalization, non_pathologized_sexuality).
narrative_ontology:constraint_victim(sexual_health_medicalization, populations_excluded_from_medical_categories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MEDICALIZED INDIVIDUAL (SNARE) — Patient trapped in the diagnostic frame once sexual function is labeled pathological. Medical authority naturalizes one pathway (pharmaceutical intervention) as the only legitimate solution. Exit requires rejecting medical legitimacy itself, which carries social/relational costs. Maximum extraction from perspective of the person whose sexuality is now a manageable disease.
constraint_indexing:constraint_classification(sexual_health_medicalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PATIENT SEEKING TREATMENT (TANGLED ROPE) — Genuine coordination benefit: medical access provides real options for those with physiological dysfunction or distress. But medicalization also extracts by narrowing the frame (redefining normal variation as disorder), creating pharmaceutical dependency, and naturalizing one solution pathway. Constrained by cost, access, and social legitimacy — cannot easily exit the medical frame once entered but has some agency in how treatment proceeds.
constraint_indexing:constraint_classification(sexual_health_medicalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURERS (ROPE) — Benefits substantially from medicalization frame: each diagnosis creates a market. Sexual medicine is pure coordination from manufacturer perspective — defining sexual variation as pathology solves their fundamental problem: identifying profitable markets. Experiences the constraint as coordination infrastructure, not extraction.
constraint_indexing:constraint_classification(sexual_health_medicalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LGBTQ+ AND SEX-POSITIVE ADVOCACY (SCAFFOLD) — Organized agents resisting medicalization frame (depathologization movements, sex-positive feminism, comprehensive sexuality education). See medicalization as temporary regulatory failure with sunset potential: destigmatization norms, expanded sexual autonomy frameworks, and holistic models can replace pathology logic. Constrained by medical institution power but building alternative legitimacy pathways. Sunset mechanism: generational norm shift and institutional policy change (DSM depathologization precedent).
constraint_indexing:constraint_classification(sexual_health_medicalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL DIAGNOSTIC BUREAUCRACY (PITON) — DSM and ICD categories for sexual dysfunction persist through institutional inertia despite ongoing challenge. The categories maintain themselves through professional credentialing, research funding, and billing codes, not because the pathology model is epistemically superior. Theater_ratio high: diagnostic rituals (sexual history questionnaires, standardized scales) are performative — they legitimize the pathology frame but don't resolve underlying questions about whether variation is dysfunction. Degraded function: the diagnostic categories actively misclassify normal variation.
constraint_indexing:constraint_classification(sexual_health_medicalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a philosophical view, medicalization of sexual function appears immutable: sexual variation is empirical fact, dysfunction is conceptually distinct from variation (disease = harm to organism), therefore some pathologization is inevitable. The engine will identify this as a false summit — the 'naturalness' of the medical frame conceals that the boundary between variation and dysfunction is drawn by institutional choice, not inherent to biology.
constraint_indexing:constraint_classification(sexual_health_medicalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sexual_health_medicalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sexual_health_medicalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sexual_health_medicalization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sexual_health_medicalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sexual_health_medicalization, TR),
    TR >= 0.70.

:- end_tests(sexual_health_medicalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Medicalization creates significant extraction through pharmaceutical gatekeeping, pathologization of diversity, and institutional narrowing of legitimacy. But this is not pure extraction because medical access genuinely helps some people. The 0.58 reflects the mixture: pharmaceutical companies extract substantially (price markup, market creation, off-label expansion), practitioners extract moderately (credential gatekeeping, specialization rents), and patients in the snare perspective experience high extraction (framed as disease requiring medical intervention). Suppression (0.52): Moderate-high. Multiple layers: institutional gatekeeping (medical licensing, specialist access), pharmaceutical marketing normalizing pathology frame, internalized shame and medicalized identity, social stigma against non-medicalized sexuality, and economic barriers to non-pharmaceutical approaches. Suppression has increased over time as pharmaceutical marketing has normalized medicalization and as DSM expansion has pathologized more sexual variation. Theater ratio (0.68): High and increasing. Diagnostic rituals are substantially performative. Standardized questionnaires (FSFI, IIEF) legitimize pathology but don't resolve fundamental questions. Sexual history assessments have theatrical elements (legitimacy creation without resolution). Physical exams for sexual dysfunction are often low-information. The increase over 60 years reflects intensified use of these rituals as medicalization has expanded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The pharmaceutical manufacturer sees pure coordination (Rope)—medicalization is the solution to market identification. The sexual medicine specialist sees moderate coordination with some extraction (Rope or light Tangled Rope)—specialization provides real benefits alongside professional gatekeeping. The patient seeking treatment sees mixed benefits and costs (Tangled Rope)—genuine medical access alongside narrowed framing and pharmaceutical dependency. The individual trapped in a diagnostic label sees pure extraction (Snare)—the pathology frame constrains their sexual autonomy with no exit. Advocacy movements see a temporary problem with generational sunset (Scaffold)—depathologization norms and holistic models are building alternative legitimacy. The diagnostic bureaucracy sees its own degraded ritual (Piton)—DSM categories persist through inertia, not epistemological superiority. The civilizational analytical observer risks seeing a natural law (Mountain)—sexual variation must have some dysfunction boundary—but the structural data reveals this as naturalization: the boundary is institutional choice, not biology.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across perspectives. Pharmaceutical manufacturers experience low d (full beneficiaries with arbitrage): medicalization solves their market identification problem, producing negative effective extraction f(d) (they benefit). Sexual medicine practitioners experience moderate-low d (beneficiaries with constrained exit): they benefit from professional domain creation but face constraints (licensing requirements, malpractice concerns, market saturation). Patients seeking treatment experience moderate d (mixed beneficiary/victim status): they benefit from medical access but are constrained by the narrowed frame and pharmaceutical dependency, producing moderate χ. Medicalized individuals trapped in diagnosis experience high d (full victims with trapped exit): no legitimate exit pathway, maximum f(d), producing high χ. The analytical observer at civilizational scope experiences d ≈ 0.72 (seeing false mountain), which the engine will flag as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how institutional extraction masks itself as coordination. The manufacturer's 'coordination' (medicalization solves market identification) is simultaneous extraction (pathologizes diversity to create markets). The specialist's 'coordination' (provides medical access) is simultaneous extraction (creates professional gatekeeping). The patient's 'tangled rope' experience is real—both benefits and costs are genuine—but the asymmetry runs toward manufacturers and specialists, not patients. The snare experienced by trapped individuals reveals the extraction mechanism: once a diagnosis is applied, exit requires rejecting medical legitimacy itself, which carries relational and social costs. The scaffold perspective's sunset logic depends on depathologization precedent (homosexuality removal from DSM in 1973)—suggesting that institutional frames can shift, but only through organized resistance. The piton classification of the diagnostic bureaucracy reveals that the categories persist not because they identify real dysfunction but because they are institutionally embedded. The false mountain of the analytical observer naturalizes all of this as inevitable. Mandatrophy is resolved by recognizing that the constraint is genuinely a tangled rope—both coordination and extraction are real—but the extraction is concentrated in institutional and pharmaceutical beneficiaries while the coordination benefit is dispersed unevenly among patients, with trapped individuals experiencing primarily extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    variation_versus_dysfunction_boundary,
    'Where is the boundary between sexual variation and sexual dysfunction, and who has authority to draw it?',
    'Cross-cultural comparison of sexual norms; longitudinal tracking of DSM/ICD reclassifications; analysis of exclusion/inclusion criteria for diagnostic categories over time',
    'If boundary is biological/universal: medicalization is coordination (identifying real dysfunction). If boundary is socially constructed: medicalization is extraction (pathologizing diversity). Current evidence suggests boundary is drawn by institutional power, not biology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(variation_versus_dysfunction_boundary, conceptual, 'Whether variation/dysfunction boundary is biological or institutionally constructed').

omega_variable(
    pharmaceutical_efficacy_versus_normalization,
    'Does pharmaceutical treatment improve sexual function/satisfaction for diagnosed individuals, or does it primarily normalize pharmaceutical dependency?',
    'Longitudinal patient outcome studies with extended follow-up; comparison of medication outcomes vs. psychosocial interventions; tracking of dose escalation and medication switching patterns',
    'If efficacy > normalization: tangled rope classification confirmed (genuine coordination benefit alongside extraction). If normalization > efficacy: snare classification becomes dominant (pharmaceutical dependency masks extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_efficacy_versus_normalization, empirical, 'Whether pharmaceutical treatment addresses dysfunction or normalizes dependency').

omega_variable(
    depathologization_feasibility,
    'Can depathologization pathways (sex-positive, LGBTQ+ affirmative, comprehensive sexuality models) replace medicalization frame without simply transferring authority to different gatekeepers?',
    'Analysis of existing depathologization movements (homosexuality removal from DSM, intersex depathologization efforts); tracking of power distribution in alternative frameworks; identification of new gatekeeping mechanisms in non-medical approaches',
    'If feasible without gate-transfer: scaffold sunset is real. If depathologization requires new gatekeepers: the constraint simply shifts form, not type (piton may persist as different institution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depathologization_feasibility, conceptual, 'Whether depathologization can succeed without institutional gate-shifting').

omega_variable(
    identity_locked_mechanism_strength,
    'How fused are individual identities with medicalized sexual self-concepts, and what proportion of suppression is internalized vs. structural?',
    'Qualitative analysis of patient narratives; longitudinal tracking of internalized stigma post-depathologization; comparison of exit trajectories across cohorts with different identity-fusion profiles',
    'If fusion is high and internalized: identity_locked exit may apply to portions of the trapped perspective. If fusion is low: exit barriers are primarily structural (cost, access, relational), not identity-constituted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_strength, empirical, 'Degree of identity fusion with medicalized sexual self-concepts').

omega_variable(
    suppression_mechanism_composition,
    'Is suppression of non-medicalized sexuality primarily structural (institutional gatekeeping, pharmaceutical marketing, medical licensing) or internalized (shame, medicalized identity, cognitive capture)?',
    'Analysis of suppression trajectories post-exposure to depathologization frames; comparison of suppression levels across cohorts with different access to counter-narratives; post-exit persistence of internalized suppression',
    'If structural dominates: suppression metric is accurate as-is. If internalized dominates: actual suppression may be higher post-exit, as internalized frames persist. This affects piton vs. snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression (structural vs. internalized)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sexual_health_medicalization, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sexmed_tr_t0, sexual_health_medicalization, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sexmed_tr_t20, sexual_health_medicalization, theater_ratio, 20, 0.58).
narrative_ontology:measurement(sexmed_tr_t40, sexual_health_medicalization, theater_ratio, 40, 0.68).
narrative_ontology:measurement(sexmed_tr_t60, sexual_health_medicalization, theater_ratio, 60, 0.72).

% Extraction over time
narrative_ontology:measurement(sexmed_be_t0, sexual_health_medicalization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sexmed_be_t20, sexual_health_medicalization, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sexmed_be_t40, sexual_health_medicalization, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(sexmed_be_t60, sexual_health_medicalization, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sexual_health_medicalization, resource_allocation).
narrative_ontology:affects_constraint(sexual_health_medicalization, pharmaceutical_marketing_regulation).
narrative_ontology:affects_constraint(sexual_health_medicalization, sexual_autonomy_and_consent_frameworks).
narrative_ontology:affects_constraint(sexual_health_medicalization, lgbtq_medical_affirmation).

% DUAL FORMULATION NOTE:
% Sexual health medicalization decomposes into multiple constraints: pharmaceutical market creation (high ε, snare), medical access and treatment coordination (moderate ε, tangled rope), diagnostic boundary maintenance (high theater, piton), and depathologization resistance (moderate-high extraction). This story tracks the aggregate constraint; downstream stories track specific mechanisms. The network links to related constraints in pharmaceutical regulation, sexuality frameworks, and LGBTQ+ medical affirmation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sexual_health_medicalization, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
