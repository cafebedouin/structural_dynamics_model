% ============================================================================
% CONSTRAINT STORY: metabolic_phenotype_individualization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metabolic_phenotype_individualization, []).

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
 *   constraint_id: metabolic_phenotype_individualization
 *   human_readable: Metabolic Phenotype Individualization in Personalized Medicine
 *   domain: biomedical/medical_systems
 *
 * SUMMARY:
 *   Metabolic phenotype individualization represents a shift in medicine from
 *   population-level standardized metrics to individual-level metabolic
 *   profiling. This constraint arises from the tension between legitimate
 *   clinical coordination (better treatment matching through personalized
 *   metabolic understanding) and extractive stratification (tiered access
 *   creating health inequity, data lock-in, algorithmic opacity). The
 *   constraint exhibits both genuine coordination benefits and asymmetric
 *   extraction mechanisms, making it a canonical tangled rope. Theater ratio
 *   (0.55) reflects moderate performative content: while some phenotyping
 *   genuinely improves outcomes, significant portions involve
 *   marketing-driven phenotyping in contexts with weak evidence, algorithm
 *   opaqueness obscuring whether improvements are from better personalization
 *   or from behavioral selection effects (patients with access to phenotyping
 *   are often those with resources and adherence capacity). The
 *   extractiveness trajectory (0.28 → 0.52 over 6 years) shows rapid
 *   accumulation as phenotyping integration into standard of care creates
 *   path dependency — patients lacking phenotypic data face increasing
 *   disadvantage. Suppression (0.48) reflects moderate barriers: cost
 *   barriers are real but not absolute for many populations; information
 *   asymmetry is structured (algorithm opaqueness, data ownership);
 *   alternative pathways exist (population-level medicine, equipoise-based
 *   trial design) but face institutional headwinds.
 *
 * KEY AGENTS:
 *   - Precision Medicine Companies: Primary beneficiary (institutional/arbitrage) — generate revenue from phenotyping services, biomarker licensing, algorithmic platforms. Have full exit capacity (can pivot to different personalization metrics). Net beneficiary without constraints.
 *   - Patients Without Genetic Data or Resources: Primary victim (powerless/trapped) — cannot afford phenotyping; as individualization becomes standard, excluded patients face deteriorating health outcomes relative to phenotyped populations. No exit pathway; trapped by cost barriers and information asymmetry.
 *   - Moderately-Resourced Patients: Secondary victim (moderate/constrained) — partial access through insurance or cost-sharing; benefit from some phenotyping but face ongoing expense, data ownership loss, algorithmic opacity. Can exit by refusing phenotyping but at cost of suboptimal treatment matching.
 *   - Clinical Practice Guidelines Bodies: Institutional actor (institutional/constrained) — must decide when to require vs recommend phenotyping; constrained by evidence gaps, industry pressure, and conflicting outcomes research. Active enforcement required to maintain dual guidance.
 *   - Population-Level Public Health: Degraded institutional actor (institutional/constrained) — epidemiology systems built on standardized measures face degradation as phenotyping creates two-tiered data collection. Constrained by the piton mechanism: maintain old standards despite being suboptimal, because individualized replacements haven't fully scaled.
 *   - Analytical Observer: Sees the constraint as coordinating with extractive overlay — genuine personalization benefit coexisting with asymmetric access and data control structures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metabolic_phenotype_individualization, 0.52).
domain_priors:suppression_score(metabolic_phenotype_individualization, 0.48).
domain_priors:theater_ratio(metabolic_phenotype_individualization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metabolic_phenotype_individualization, extractiveness, 0.52).
narrative_ontology:constraint_metric(metabolic_phenotype_individualization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(metabolic_phenotype_individualization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metabolic_phenotype_individualization, tangled_rope).
narrative_ontology:human_readable(metabolic_phenotype_individualization, "Metabolic Phenotype Individualization in Personalized Medicine").
narrative_ontology:topic_domain(metabolic_phenotype_individualization, "biomedical/medical_systems").

domain_priors:requires_active_enforcement(metabolic_phenotype_individualization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metabolic_phenotype_individualization, precision_medicine_companies).
narrative_ontology:constraint_beneficiary(metabolic_phenotype_individualization, genomic_testing_industry).
narrative_ontology:constraint_beneficiary(metabolic_phenotype_individualization, metabolic_research_institutions).
narrative_ontology:constraint_victim(metabolic_phenotype_individualization, patients_without_genetic_data).
narrative_ontology:constraint_victim(metabolic_phenotype_individualization, socioeconomically_disadvantaged_populations).
narrative_ontology:constraint_victim(metabolic_phenotype_individualization, medical_standardization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PATIENT (SNARE) — Cannot afford metabolic phenotyping; health outcomes increasingly depend on individualized metabolic profiling that is inaccessible. Trapped by cost barriers and information asymmetry. As personalized medicine becomes standard, exclusion from phenotyping becomes a material health disadvantage with no alternative pathway.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MODERATELY-RESOURCED PATIENT (TANGLED ROPE) — Has partial access to metabolic phenotyping through insurance or out-of-pocket cost-sharing. Experiences genuine coordination benefit (better treatment matching) alongside extraction (ongoing profiling costs, data ownership asymmetry, algorithmic opaqueness). Mixed experience with real constraints but also real agency.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRECISION MEDICINE COMPANY (ROPE) — Experiences metabolic phenotyping as pure coordination infrastructure. The constraint solves the market problem: how to match patients to treatments efficiently. The company has full arbitrage options (pivot to different personalization metrics, switch biomarker suites, license technology). Net beneficiary but genuinely coordinating — the system works for them.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL PRACTICE GUIDELINES AUTHORITY (TANGLED ROPE) — Faces pressure to incorporate metabolic phenotyping into standard of care (coordination function: improving treatment matching). Simultaneously constrained by evidence gaps, cost-benefit ambiguity, and institutional capture by precision medicine industry. Active enforcement required to maintain dual guidance (when to individualize, when to standardize). Genuine coordination conflict with asymmetric interests.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POPULATION-LEVEL EPIDEMIOLOGY (PITON) — The shift toward metabolic phenotyping degrades population-level public health data collection. Public health surveillance systems are built on standardized metabolic measures (BMI, fasting glucose, lipid panels). Individualized phenotyping creates a two-tiered system: detailed phenotypes for those with access, degraded population statistics for public health planning. The constraint is maintained through institutional inertia — standard measures persist despite being analytically inferior to individual phenotypes — because alternatives haven't fully replaced them for population-level purposes.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, metabolic phenotyping coordination is real (better treatment matching, reduced trial-and-error) but asymmetrically distributed (concentrated among high-income populations and research-intensive regions). The constraint exhibits genuine coordination function alongside asymmetric extraction and information control. The engine's classification matches the structural data: requires active enforcement, has clear beneficiaries and victims, demonstrates both coordination and extraction at meaningful scale.
constraint_indexing:constraint_classification(metabolic_phenotype_individualization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_phenotype_individualization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metabolic_phenotype_individualization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_phenotype_individualization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metabolic_phenotype_individualization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metabolic_phenotype_individualization, TR),
    TR >= 0.70.

:- end_tests(metabolic_phenotype_individualization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Above the 0.46 threshold that triggers tangled rope territory. The metric reflects that metabolic phenotyping creates significant asymmetric returns: precision medicine companies capture value through data ownership and algorithmic licensing; excluded populations bear health disadvantage; treatment matching benefits are distributed unevenly. The upward trajectory (0.28 → 0.52) indicates path dependency — as phenotyping becomes incorporated into standard care, non-participation creates cumulative disadvantage. Suppression (0.48): Moderate but structured. Cost barriers are primary (phenotyping often costs $500-$5000 per individual, often out-of-pocket). Information asymmetry is significant (algorithms are proprietary, phenotype selection is often opaque, benefit claims are often marketing-driven). Institutional barriers are real (guidelines still accommodate both phenotyped and non-phenotyped care, but with implicit assumptions favoring phenotyped patients). Theater ratio (0.55): Mid-range. A substantial portion of phenotyping — perhaps 40-50% — involves genuine personalization that improves outcomes (some genetic variations do affect drug metabolism, some biomarkers do predict response). The remaining 45-60% exhibits theatrical properties: marketing claims exceed evidence, algorithm opaqueness prevents verification of improvement attribution, phenotyping becomes a status good and insurance optimality signal independent of actual clinical value. Claimed type: Tangled rope because (1) genuine coordination function exists (matching treatments to metabolic capacity), (2) asymmetric extraction is structurally embedded (access concentrated by income), (3) active enforcement required (guidelines bodies must decide when to individualize vs standardize).
 *
 * PERSPECTIVAL GAP:
 *   The gap between excluded patients (snare) and precision medicine companies (rope) is stark: excluded patients see pure extraction with no escape; companies see pure coordination with full arbitrage. The moderately-resourced patient sees the true structure (tangled rope) — real benefit alongside real extraction. The clinical guidelines authority sees the enforcement problem (tangled rope at institutional scale) — how to individualize where it helps while protecting access equity. The epidemiology perspective sees degradation (piton) — population-level data systems losing function as individualization fragments standardization. The analytical observer confirms tangled rope: the structural data matches the classification — beneficiaries are concentrated, victims are dispersed, enforcement is contested, coordination function coexists with extraction. If perspectives converged on rope, the constraint would be non-extractive; if all converged on snare, the constraint would be pure extraction with no coordination claim. The perspectival diversity itself is the diagnostic signal of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from agent power and structural relationship to the phenotyping flow. Precision medicine companies (institutional/arbitrage) experience low d (~0.15): they are beneficiaries with exit capacity, so the sigmoid f(d) produces near-zero or negative chi. Excluded patients (powerless/trapped) experience high d (~0.95): they are victims with no exit, so f(d) produces maximum chi. Moderately-resourced patients (moderate/constrained) experience d ~0.55: they are both somewhat victimized (extraction happens) and somewhat benefited (treatment matching), with constrained exit (can exit phenotyping but at health cost). Clinical guidelines authority (institutional/constrained) experiences d ~0.45: they have institutional power but are constrained by the tangled rope structure itself — they must enforce dual guidance (when to individualize, when to standardize) even though the structural incentives pull toward individualization. The variability in d across perspectives is not noise — it is the measured heterogeneity of the extraction mechanism. The constraint does not extract uniformly; it extracts from those with low exit capacity and concentrated benefit in those with high exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE ATROPHY RISK: The constraint contains the classical setup for mandate collapse — a genuine coordination problem (treatment matching) masked by extraction asymmetry (access stratification). If guidelines bodies mandate phenotyping without addressing access barriers, the mandate becomes a coercive asymmetry: those who can afford phenotyping are required to phenotype; those who cannot are implicitly excluded from standard-of-care treatment pathways. If guidelines bodies refuse to mandate phenotyping despite accumulating evidence, the coordination function remains suboptimal. The resolution is not to collapse the mandate but to restructure the constraint: either (1) make phenotyping universally accessible (converting snare → rope), (2) develop parallel personalization pathways that don't require expensive phenotyping (reducing extraction), or (3) establish equity gates that prevent phenotyping from becoming a stratification mechanism. The analytical observer's tangled rope classification is correct and stable across the interval if and only if the enforcement body continues to acknowledge both the coordination function AND the extraction asymmetry. If enforcement becomes purely celebratory (phenotyping is always better), the constraint degrades into a false rope and the extraction hardens into a hidden snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phenotype_universality_threshold,
    'At what percentage population coverage does individualized phenotyping cease to be extractive and become coordinated population medicine?',
    'Coverage data analysis; comparison of treatment outcomes in high-coverage vs low-coverage populations; identification of equity threshold where phenotyping becomes universal rather than stratified access good',
    'If threshold < 60%: current state remains extractive (snare/tangled rope). If threshold > 85%: phenotyping could transition to rope classification if access barriers fall. Impacts policy implications for mandating coverage expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phenotype_universality_threshold, empirical, 'Coverage threshold for phenotyping to shift from extraction to coordination').

omega_variable(
    phenotype_evidence_sufficiency,
    'Does the empirical evidence justify phenotyping-based treatment personalization at current scale, or does the evidence base primarily support narrow clinical contexts (rare genetic conditions, specific cancer subtypes) extrapolated to general populations?',
    'Systematic review of randomized controlled trials with phenotype stratification; meta-analysis of treatment outcome improvements attributable to metabolic phenotyping vs unmeasured confounders; comparison of predicted vs actual benefit in diverse populations',
    'If evidence is narrow: phenotyping expansion is justified by market incentives more than clinical necessity — snare/extraction frame correct. If evidence is broad: genuine coordination function is larger than extraction — tangled rope frame correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phenotype_evidence_sufficiency, empirical, 'Evidence base for phenotyping-guided treatment across populations').

omega_variable(
    data_ownership_asymmetry,
    'Can phenotypic data be held in patient-controlled repositories with real algorithmic portability, or is data lock-in an inherent feature of precision medicine business models?',
    'Analysis of data transfer agreements; identification of technical barriers vs business-model barriers to portability; case studies of patient data portability attempts across precision medicine platforms',
    'If data is genuinely portable: extraction mechanism is weakened — tangled rope can evolve toward rope. If lock-in is systematic: data ownership creates permanent victim status — snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_ownership_asymmetry, empirical, 'Feasibility of patient-controlled metabolic data portability').

omega_variable(
    standardization_feedback_loop,
    'Does successful metabolic phenotyping in high-resource contexts create pressure to standardize phenotypes, or does individualization create irreversible path dependency toward ever-finer stratification?',
    'Historical analysis of other medical standardization cycles (from individualized bloodletting to standardized transfusion protocols); predictive modeling of phenotype standardization adoption in lower-resource regions',
    'If standardization pressure dominates: phenotyping constraint is temporary — scaffold classification becomes more appropriate. If stratification path dependency dominates: the constraint becomes entrenched — snare/tangled rope classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standardization_feedback_loop, conceptual, 'Whether phenotyping creates momentum toward standardization or irreversible stratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metabolic_phenotype_individualization, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metab_tr_t0, metabolic_phenotype_individualization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(metab_tr_t3, metabolic_phenotype_individualization, theater_ratio, 3, 0.45).
narrative_ontology:measurement(metab_tr_t6, metabolic_phenotype_individualization, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(metab_be_t0, metabolic_phenotype_individualization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(metab_be_t3, metabolic_phenotype_individualization, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(metab_be_t6, metabolic_phenotype_individualization, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metabolic_phenotype_individualization, resource_allocation).
narrative_ontology:affects_constraint(metabolic_phenotype_individualization, medical_data_ownership_asymmetry).
narrative_ontology:affects_constraint(metabolic_phenotype_individualization, algorithmic_opacity_in_clinical_decision_making).
narrative_ontology:affects_constraint(metabolic_phenotype_individualization, health_equity_stratification_by_income).

% DUAL FORMULATION NOTE:
% Metabolic phenotyping is downstream of both genomic sequencing capability (which determines what can be phenotyped) and precision medicine business model (which determines access and pricing). The phenotyping constraint is distinct from these upstream constraints: it represents the coordination-extraction hybrid at the clinical decision layer, whereas upstream constraints have different epsilon values reflecting the empirical/technical and economic layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metabolic_phenotype_individualization, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
