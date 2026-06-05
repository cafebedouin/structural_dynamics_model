% ============================================================================
% CONSTRAINT STORY: genetic_predisposition_mania
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_predisposition_mania, []).

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
 *   constraint_id: genetic_predisposition_mania
 *   human_readable: Genetic Blueprint for Manic Episodes in Bipolar Disorder
 *   domain: biomedical/psychiatric_genetics
 *
 * SUMMARY:
 *   The identification of genetic blueprints predicting manic episodes in
 *   bipolar disorder creates a structural constraint operating across
 *   research, clinical care, insurance classification, and patient autonomy.
 *   The constraint exhibits properties of tangled rope: it enables genuine
 *   coordination (precision medicine, early identification) while
 *   simultaneously extracting value from patients through stigmatization,
 *   reproductive counseling pressure, and gatekeeping of treatment access.
 *   The core extraction mechanism is the suppression of alternative
 *   etiological frameworks (psychosocial, environmental, trauma-informed)
 *   that would distribute clinical responsibility across multiple domains
 *   rather than concentrating it in psychiatric-pharmaceutical institutions.
 *   Patients classified as genetically predisposed experience permanent label
 *   attachment with no exit option; they become ensnared in genetic
 *   determinism narratives that override their own agency in managing
 *   modifiable risk factors (sleep, stress, substance use, circadian
 *   disruption). Meanwhile, research institutions, pharmaceutical
 *   manufacturers, and diagnostic systems benefit from the genetic framing
 *   through market segmentation, patent-protected biomarkers, and predictable
 *   funding streams. The theater ratio (0.58) reflects that genetic testing
 *   is often ordered performatively—to justify treatment decisions already
 *   clinically indicated—rather than to drive novel therapeutic choices.
 *   Open-science and functional medicine movements represent organized
 *   challenges with sunset logic: alternative frameworks de-emphasizing
 *   genetic determinism in favor of systems-level biomarkers and lifestyle
 *   interventions are gradually displacing genetic determinism as the primary
 *   organizing paradigm.
 *
 * KEY AGENTS:
 *   - Bipolar Patients: Primary victim (powerless/trapped) — bears costs of permanent genetic labeling, social stigma, insurance discrimination, and reproductive counseling pressure
 *   - Patient Advocacy and Access Cohorts: Secondary victim (moderate/constrained) — need genetic evidence to challenge misdiagnosis but constrained by research gatekeeping
 *   - Psychiatric Research Institutions: Primary beneficiary (institutional/arbitrage) — enable publication pipelines, grant funding, cohort stratification through genetic research
 *   - Pharmaceutical Manufacturers: Organized extractor (organized/constrained) — segment markets, patent biomarker-based therapeutics, suppress alternative etiological frameworks
 *   - Open Science and Functional Medicine Movements: Organized challengers (organized/mobile) — building alternative frameworks that de-emphasize genetic determinism
 *   - DSM and Insurance Classification: Institutional gatekeeper (institutional/constrained) — maintains genetic reductionism through classification inertia despite contradictory evidence
 *   - Psychiatric Nosology: Abstract victim — risks permanent reification of genetic categories despite growing evidence of spectrum heterogeneity and environmental modulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition_mania, 0.38).
domain_priors:suppression_score(genetic_predisposition_mania, 0.62).
domain_priors:theater_ratio(genetic_predisposition_mania, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition_mania, extractiveness, 0.38).
narrative_ontology:constraint_metric(genetic_predisposition_mania, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(genetic_predisposition_mania, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_predisposition_mania, tangled_rope).
narrative_ontology:human_readable(genetic_predisposition_mania, "Genetic Blueprint for Manic Episodes in Bipolar Disorder").
narrative_ontology:topic_domain(genetic_predisposition_mania, "biomedical/psychiatric_genetics").

domain_priors:requires_active_enforcement(genetic_predisposition_mania).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_predisposition_mania, psychiatric_researchers).
narrative_ontology:constraint_beneficiary(genetic_predisposition_mania, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(genetic_predisposition_mania, diagnostic_institutions).
narrative_ontology:constraint_victim(genetic_predisposition_mania, bipolar_patients).
narrative_ontology:constraint_victim(genetic_predisposition_mania, psychiatric_nosology).
narrative_ontology:constraint_victim(genetic_predisposition_mania, treatment_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BIPOLAR PATIENT (SNARE) — Trapped in genetic determinism narrative. Cannot exit the identification as genetically predisposed; faces social stigma, insurance discrimination, and reproductive counseling pressure. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(genetic_predisposition_mania, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT ADVOCACY AND ACCESS COHORTS (TANGLED ROPE) — Constrained by need for clinical evidence to challenge misdiagnosis and denial of care, but also benefit from genetic research that enables early intervention and targeted therapeutics. Coordination: genetic blueprints enable precision medicine. Extraction: genetic labels enable gatekeeping and rationing. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(genetic_predisposition_mania, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PSYCHIATRIC RESEARCH INSTITUTIONS (ROPE) — Primary beneficiaries. See genetic blueprint as coordination mechanism: enables stratification of patient cohorts, justifies grant funding, drives publication pipelines, supports differential diagnosis. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary position.
constraint_indexing:constraint_classification(genetic_predisposition_mania, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURERS (SNARE) — Organized institutional actor. Benefits from genetic blueprints enabling market segmentation and patent-protected biomarker-based therapeutics. Constrains competitors via proprietary diagnostic panels. Suppresses alternative etiological frameworks (psychosocial, environmental, trauma-informed) that would distribute treatment responsibility. d≈0.22, f(d)≈0.18, σ=1.2 → χ≈0.09. Classified as snare despite low χ because suppression (0.62) and asymmetric market power are high.
constraint_indexing:constraint_classification(genetic_predisposition_mania, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE AND FUNCTIONAL MEDICINE MOVEMENTS (SCAFFOLD) — Organized agents building alternative frameworks that de-emphasize genetic determinism in favor of modifiable risk factors (sleep, stress, substance use, circadian disruption). See genetic blueprint as temporary gate that will be sunset by systems-level biomarkers and lifestyle interventions. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.29.
constraint_indexing:constraint_classification(genetic_predisposition_mania, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DSM DIAGNOSTIC CRITERIA AND INSURANCE CLASSIFICATION (PITON) — Maintains genetic reductionism through institutional inertia. The DSM-5 codification of bipolar disorder as a biological category drives insurance coverage, legal disability determination, and stigma. Theater_ratio=0.58 reflects that genetic testing is often performative — ordered to justify treatment already clinically indicated, not to drive new therapeutic decisions. The classification system persists despite growing evidence of polygenicity, environmental triggers, and spectrum heterogeneity. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(genetic_predisposition_mania, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risks naturalizing statistical association as causal biology. From a civilizational universal scope, genetic variation constrains neural function universally — but this does not establish that a particular SNP set determines mania risk. The 'constraint' from the analytical view is the fact of heritability itself (genetic variation exists). However, structural data (ε=0.38, suppression=0.62, theater=0.58) contradicts mountain classification — this is a false summit, naturalizing contingent psychiatric institutional arrangements.
constraint_indexing:constraint_classification(genetic_predisposition_mania, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_mania_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(genetic_predisposition_mania, TR),
    TR >= 0.70.

:- end_tests(genetic_predisposition_mania_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The genetic blueprint enables genuine coordination benefits (precision medicine, early intervention targeting) but systematically suppresses alternative etiological frameworks that would distribute clinical control. The extraction is not maximal because informed patients can still access psychosocial and environmental interventions; the suppression is institutional rather than absolute. The trajectory shows increase from 0.18 to 0.38 as genetic testing has become standard and market-based (Goodhart drift). Suppression (0.62): Moderate-high. Significant barriers to non-genetic explanations include: pharmaceutical industry funding favoring pharmacological research, psychiatric institution dependency on biomedical framing for legitimacy, DSM codification creating path dependency, and patient difficulty accessing trauma-informed or environmental treatment frameworks. However, suppression is not total—functional medicine and open-science communities are actively building alternatives. Theater ratio (0.58): Moderate-high. Genetic testing is frequently ordered to justify treatment already clinically indicated rather than to drive novel decisions. The theater has increased from 0.32 to 0.58 as genetic testing has become routine despite limited clinical utility at individual patient level (per omega_predictive_utility). The constraint shows the signature of institutional inertia combined with market incentive: theaters expand as the original coordination function (identifying true genetic risk) proves more limited than initially claimed.
 *
 * PERSPECTIVAL GAP:
 *   The bipolar patient sees pure extraction (snare) — genetic identification offers no escape route, only permanent labeling and reproductive pressure. Research institutions see pure coordination (rope) — genetic blueprints enable legitimate scientific progress. Pharmaceutical manufacturers see controlled extraction (snare with organized power) — they can segment markets and suppress competitors through proprietary diagnostics. Patient advocacy organizations see mixed coordination-extraction (tangled rope) — they need genetic evidence to fight misdiagnosis but are constrained by the research institutions that gatekeep that evidence. Open-science movements see a temporary problem with sunset logic (scaffold) — alternative frameworks are gradually displacing genetic determinism. The DSM/insurance system sees degraded ritual (piton) — genetic testing is performed for institutional legitimacy rather than clinical utility. The analytical observer risks seeing an immutable natural law (mountain) — genetic variation inherently influences neural function — but the structural data reveals this as a false summit naturalizing contingent psychiatric institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Bipolar Patients: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit from genetic label. Patient Advocacy Cohorts: Victim + constrained + beneficiary from research → d≈0.68, f(d)≈1.02. High extraction but mitigated by some access to coordination benefits. Research Institutions: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Pharmaceutical Manufacturers: Organized beneficiary + constrained → d≈0.22, f(d)≈0.18. Low effective extraction from this power position but high structural suppression justifies snare classification. Open Science/Functional Medicine: Organized challenger + mobile → d≈0.45, f(d)≈0.52. Can exit genetic determinism frame entirely; see genuine alternative pathways. DSM/Insurance: Institutional gatekeeper + constrained → d≈0.35, f(d)≈0.32. Piton classification from theater gate (0.58) and inertia, not from high χ. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. False summit detector identifies naturalization fallacy.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL RESOLUTION: The constraint resolves mandatrophy by distinguishing between two structurally separate claims that are often conflated in psychiatric discourse: (1) Genetic variation influences bipolar phenotypes (TRUE, low ε, mountain-like) and (2) Identified genetic blueprints determine manic episode onset at clinical decision-making thresholds (CONTESTED, high ε, snare-like). The first claim is a near-immutable fact of biology; the second is a contingent institutional claim driven by research incentives and pharmaceutical market structure. By separating these, the framework reveals that 'genetic predisposition' operates as a snare for patients and tangled rope for institutions precisely because it conflates biological constraint (low ε) with institutional extraction mechanism (high ε). The tangled rope classification holds because the constraint simultaneously enables genuine precision medicine coordination (beneficiaries: research institutions, some patients) AND suppresses modifiable risk factor research (victims: undiagnosed environmental depression, treatment access equity). The theater ratio trajectory (0.32→0.58) confirms mandatrophy: as genetic testing became routine, its actual clinical utility did not improve proportionally—the theater expanded while the function stagnated. This is the diagnostic signature of a constraint degrading from scaffold (promise of precision medicine with sunset as environment dominates) toward piton (genetic testing persists through institutional inertia despite limited clinical utility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polygenicity_vs_monogenic_model,
    'Does the identified genetic blueprint explain variance via a small set of high-penetrance variants or through polygenicity distributed across hundreds of common variants with minimal individual effect?',
    'Genome-wide association study replication across independent cohorts; effect size measurement and variance explained by identified loci; investigation of rare vs common variant contribution',
    'If monogenic: genetic determinism is structurally justified; snare classification stable. If highly polygenic: genetic ''blueprint'' is actually a distributional pattern; causality becomes probabilistic rather than deterministic; constraint shifts toward rope/scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(polygenicity_vs_monogenic_model, empirical, 'Whether genetic effects are concentrated in few variants or distributed across many').

omega_variable(
    environmental_penetrance_ratio,
    'What fraction of individuals carrying the genetic blueprint experience manic episodes? Conversely, what fraction of manic episodes occur in individuals lacking the identified genetic markers?',
    'Longitudinal prospective cohort studies tracking genetic carriers with and without environmental triggers; retrospective analysis of manic episode cases for genetic marker prevalence',
    'If penetrance <40%: genetic blueprint is risk factor not determinant; snare classification weakens toward tangled rope. If environmental non-carriers >30%: extraction mechanism (suppression of environmental explanations) is revealed as falsifying.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_penetrance_ratio, empirical, 'Penetrance of genetic blueprint for manic episodes').

omega_variable(
    institutional_capture_of_etiology,
    'To what extent does pharmaceutical industry funding and psychiatric institution gatekeeping suppress or undermine research into modifiable risk factors (sleep disruption, psychosocial stress, substance use, circadian rhythm dysregulation) in favor of genetic/pharmacological framings?',
    'Comparative analysis of research funding by etiology type; publication bias analysis (effect sizes for genetic vs environmental studies); tracking of guideline development committee member financial relationships',
    'If high capture: constraint is snare (suppression of alternatives). If low capture: constraint is tangled rope (genuine mixed coordination-extraction). If capture is demonstrated, scaffold perspective becomes primary exit pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_etiology, empirical, 'Whether institutional interests suppress non-genetic etiological research').

omega_variable(
    predictive_utility_vs_statistical_association,
    'Can the identified genetic blueprint predict manic episode onset in individual patients at clinical decision-making thresholds (>80% sensitivity/specificity), or does it only stratify population-level risk?',
    'Prospective clinical utility studies; analysis of predictive value at individual patient level; comparison to clinical staging models and environmental risk assessment',
    'If clinical utility <60%: genetic testing is theater (piton classification confirmed). If >80%: snare classification strengthens (genuine extraction of treatment control). If 60-80%: tangled rope confirmed (mixed coordination for research + extraction for gatekeeping).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_utility_vs_statistical_association, empirical, 'Whether genetic blueprint has clinical predictive utility for individual patients').

omega_variable(
    reproductive_counseling_autonomy,
    'Are bipolar individuals receiving genetic risk information being informed of the probabilistic and environmentally-modifiable nature of risk, or are they being counseled toward reproductive restriction?',
    'Content analysis of genetic counseling materials; patient survey on counseling received; review of genetic testing recommendations to reproductive endocrinologists and gynecologists',
    'If autonomy is preserved: extraction mechanism is limited. If reproductive counseling emphasizes determinism: snare classification is strengthened (coercive extraction of reproductive choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reproductive_counseling_autonomy, empirical, 'Whether genetic risk information is used to restrict reproductive autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_predisposition_mania, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpm_tr_t0, genetic_predisposition_mania, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gpm_tr_t5, genetic_predisposition_mania, theater_ratio, 5, 0.45).
narrative_ontology:measurement(gpm_tr_t10, genetic_predisposition_mania, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gpm_be_t0, genetic_predisposition_mania, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpm_be_t5, genetic_predisposition_mania, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gpm_be_t10, genetic_predisposition_mania, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_predisposition_mania, information_standard).
narrative_ontology:affects_constraint(genetic_predisposition_mania, psychiatric_nosology_reductionism).
narrative_ontology:affects_constraint(genetic_predisposition_mania, psychiatric_medication_access_inequality).
narrative_ontology:affects_constraint(genetic_predisposition_mania, polygenic_risk_score_adoption).

% DUAL FORMULATION NOTE:
% This constraint decomposes from a natural-language concept 'genetic basis of bipolar disorder' into three structurally distinct claims: (1) genetic_predisposition_mania (ε=0.38, tangled rope) — institutional extraction via suppression of environmental frameworks; (2) psychiatric_nosology_reductionism (ε=0.52, snare) — DSM codification constraining diagnosis beyond genetics; (3) polygenic_risk_score_adoption (ε=0.22, rope) — technology coordination benefit with minimal extraction. The three stories are linked by research gatekeeping and institutional incentive alignment but have different ε values reflecting different measurement methodologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genetic_predisposition_mania, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
