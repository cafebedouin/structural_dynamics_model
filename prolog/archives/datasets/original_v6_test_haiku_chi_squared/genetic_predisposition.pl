% ============================================================================
% CONSTRAINT STORY: genetic_predisposition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_predisposition, []).

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
 *   constraint_id: genetic_predisposition
 *   human_readable: Socio-Economic Response to Genetic Predisposition
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The socio-economic response to genetic predisposition creates a
 *   structural tension between the legitimate scientific ambition to
 *   understand biological variation and institutional incentives to extract
 *   value from genetic information asymmetry. This constraint is NOT the
 *   biological fact that genetic variants correlate with phenotypes — that is
 *   a natural phenomenon (potentially a Mountain at high confidence). Rather,
 *   it is the institutional system built on genetic data: testing regimes,
 *   insurance risk stratification, employment screening, reproductive
 *   counseling, and public health policy. This system exhibits all six DR
 *   types from different perspectives. The same genetic information appears
 *   as pure extraction (Snare) to the powerless individual bearing stigma, as
 *   coordination-plus-extraction (Tangled Rope) to institutions using genetic
 *   risk for resource allocation, as performative theater (Piton) in the
 *   genetic determinism narrative, and as a natural epistemic limit
 *   (Mountain) to the analytical observer who risks naturalizing contingent
 *   institutional choices. The extractiveness has increased from 0.32 to 0.58
 *   over the measurement interval as institutional gatekeeping (insurance,
 *   employment, reproductive pressure) has expanded faster than
 *   evidence-based thresholds. The theater ratio has risen from 0.42 to 0.64
 *   as genetic predisposition discourse has become more deterministic in
 *   popular culture than empirical evidence supports. The constraint
 *   exemplifies how socio-economic systems can crystallize around scientific
 *   facts while implementing institutional extraction that exceeds the facts'
 *   empirical scope.
 *
 * KEY AGENTS:
 *   - Genetically Predisposed Individuals: Primary victim (powerless/trapped) — bear stigma, discrimination, and reproductive pressure; cannot exit genetic identity
 *   - Low-Income Populations: Secondary victim (moderate/constrained) — face information asymmetry with institutions; cannot afford comprehensive genetic interpretation
 *   - Genetic Testing Industry: Primary beneficiary (institutional/arbitrage) — extracts rent through monopoly patents, proprietary algorithms, and expanded testing mandates
 *   - Insurance and Employment Gatekeepers: Secondary beneficiary (institutional/arbitrage) — use genetic information to stratify risk pools and exclude costly individuals
 *   - Behavioral Genetics Field: Victim and partial beneficiary (powerful/constrained) — trapped by publication bias and funding incentives; constrained by inability to establish causation
 *   - Genetic Determinism Narrative: Institutional performance (institutional/arbitrage) — maintains social theater despite weak empirical foundation; benefits institutions seeking simple risk models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition, 0.58).
domain_priors:suppression_score(genetic_predisposition, 0.68).
domain_priors:theater_ratio(genetic_predisposition, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition, extractiveness, 0.58).
narrative_ontology:constraint_metric(genetic_predisposition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(genetic_predisposition, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_predisposition, snare).
narrative_ontology:human_readable(genetic_predisposition, "Socio-Economic Response to Genetic Predisposition").
narrative_ontology:topic_domain(genetic_predisposition, "technological/social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_predisposition, genetic_testing_industry).
narrative_ontology:constraint_beneficiary(genetic_predisposition, insurance_companies).
narrative_ontology:constraint_beneficiary(genetic_predisposition, high_income_interpreters).
narrative_ontology:constraint_victim(genetic_predisposition, genetically_predisposed_individuals).
narrative_ontology:constraint_victim(genetic_predisposition, low_income_populations).
narrative_ontology:constraint_victim(genetic_predisposition, field_of_behavioral_genetics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENETICALLY PREDISPOSED INDIVIDUAL (SNARE) — Cannot exit genetic identity; bears costs of probabilistic stigma, insurance discrimination, employment screening, and reproductive pressure. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME POPULATION (SNARE) — Constrained by cost of genetic screening and interpretation; trapped by risk of genetic information asymmetry with institutions that can afford testing. d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENETIC TESTING INDUSTRY (TANGLED ROPE) — Solves coordination problem (making genetic information accessible) but extracts rent through monopoly patents, high test costs, and proprietary interpretation algorithms. Benefits from expanded testing mandates; exploits information asymmetry with low-income populations. d≈0.12, f(d)≈0.05, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE AND EMPLOYMENT GATEKEEPERS (TANGLED ROPE) — Use genetic information to coordinate risk pools and resource allocation, but extract value by denying coverage, raising premiums, or excluding applicants based on probabilistic predisposition. d≈0.15, f(d)≈0.08, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BEHAVIORAL GENETICS FIELD (SNARE) — Constrained by incentive to find genetic signals (publication bias, funding dependency on positive results); trapped by inability to establish causation from correlations. Victim of social demand for genetic explanations that exceed empirical support. d≈0.72, f(d)≈1.13, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GENETIC DETERMINISM NARRATIVE (PITON) — Persists as social theater despite weak empirical foundation; phenotype correlations with genetic variants are consistently overinterpreted as causal determinants. Theater ratio=0.64 reflects that much public discourse on 'genetic predisposition' is performative essentialism rather than functional science. Institutional inertia maintains the narrative through educational curricula, media coverage, and policy frameworks that predate robust evidence.
constraint_indexing:constraint_classification(genetic_predisposition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMITS VIEW (MOUNTAIN) — From civilizational scope, some limits on prediction from genetic data are intrinsic: incomplete penetrance, gene-environment interactions, polygenic variance, and nonlinear phenotypic mapping create irreducible epistemic barriers. However, the structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts mountain classification — social extraction and institutional suppression are contingent, not laws of nature.
constraint_indexing:constraint_classification(genetic_predisposition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_predisposition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_predisposition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genetic_predisposition, TR),
    TR >= 0.70.

:- end_tests(genetic_predisposition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Institutions extract value by controlling access to genetic information and using it to deny insurance coverage, exclude job applicants, and pressure reproductive choices. The extraction is not as severe as a pure financial snare (e.g., debt traps, predatory lending) because the genetic information has some legitimate utility for risk assessment. However, extraction occurs because institutions use genetic signals beyond evidence-based thresholds — they employ simplified models (genetic determinism) that exceed the empirical support. The 10-point rise (0.32 → 0.58) reflects accelerating institutional gatekeeping. Suppression (0.68): High. Multiple barriers prevent individuals from exiting or resisting the constraint: (1) genetic identity is non-negotiable, (2) institutional gatekeeping is asymmetric (institutions know more than individuals), (3) regulatory frameworks (genetic non-discrimination laws) are weak and unenforced, (4) social pressure for genetic 'explanation' of behavior constrains reproductive autonomy, (5) low-income populations cannot afford independent genetic interpretation. Theater ratio (0.64): High-moderate. Public discourse on 'genetic predisposition' is substantially performative: headlines claim genetic 'determinism' despite twin study heritabilities of 30-60%; genetic risk scores explain 2-5% of phenotypic variance yet are marketed as predictive; insurance companies invoke genetic risk while ignoring larger environmental factors; media coverage naturalizes genetic explanations that lack mechanistic evidence.
 *
 * PERSPECTIVAL GAP:
 *   The genetically predisposed individual sees pure extraction (Snare) — they bear costs (stigma, discrimination, reproductive pressure) with no offsetting benefit. The low-income population sees extraction magnified by information asymmetry (Snare) — they cannot afford to contest institutional interpretation. The genetic testing industry and insurance gatekeepers see coordination (Tangled Rope) — they solve a real problem (risk assessment) but extract rent through monopoly and asymmetry; they frame themselves as both beneficiary and provider of social good. The behavioral genetics field sees a Snare from within — pressured to find genetic signals, trapped by publication bias, constrained by inability to establish causation. The genetic determinism narrative is Piton — institutional theater maintained by inertia and convenience despite empirical rot. The analytical observer risks seeing a Mountain (genetic limits are real) but the structural data reveals this as a false summit: institutional extraction and suppression are contingent, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Genetically Predisposed Individual: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — cannot exit genetic identity. Low-Income Population: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction — cannot afford interpretation or contestation. Genetic Testing Industry: Beneficiary + arbitrage → d≈0.12, f(d)≈0.05. Net beneficiary — controls proprietary technology and gatekeeps access. Insurance/Employment Gatekeepers: Beneficiary + arbitrage → d≈0.15, f(d)≈0.08. Net beneficiary — use genetic information to lower costs and exclude expensive individuals. Behavioral Genetics Field: Victim + constrained → d≈0.72, f(d)≈1.13. Moderate-high extraction — constrained by publication bias, trapped by funding incentives, victimized by pressure to overstate findings. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk — observer might naturalize institutional choices as epistemic inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in genetic predisposition arises from the conflation of two distinct constraints: (1) THE BIOLOGICAL CONSTRAINT: Genetic variants causally influence phenotypes within bounded heritabilities. This is a Mountain (emerges naturally, ε≈0.08, low suppression, high accessibility). (2) THE INSTITUTIONAL CONSTRAINT: Socio-economic systems extract value by using genetic information to stratify, exclude, and control populations. This is a Snare (ε=0.58, suppression=0.68, requires active enforcement via institutional gatekeeping). The false claim 'genetic predisposition is a natural law therefore institutional gatekeeping is natural' naturalizes the second constraint by hiding it inside the first. The framework resolves this by separating them: the biological fact is a Mountain (or possibly a Rope if we model it as the coordination function 'understanding shared genetic basis for population variation'); the institutional response is a Snare because it enforces asymmetric extraction by controlling information access. The Piton perspective (genetic determinism narrative) is the theater that sustains the false conflation — it maintains institutional extraction by claiming it merely reflects natural differences. The framework catches this through theater ratio (0.64 > 0.50, Goodhart drift) and field victimization (behavioral genetics trapped by incentives to find genetic signals). Resolution: distinguish biological constraint from institutional response; model each separately; acknowledge that extractiveness (0.58) and suppression (0.68) are institutional facts, not natural laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_vs_correlational,
    'At what effect size threshold does genetic correlation warrant causal claims in social prediction domains?',
    'Mendelian randomization studies, natural experiments, and intervention trials; longitudinal tracking of prediction error rates in polygenic risk scores across diverse populations',
    'If threshold is low: genetic predisposition becomes functionally predictive (justifies gatekeeping). If threshold is high: genetic signals remain correlational noise (snare classification confirmed). If threshold is culture-dependent: extraction is revealed as adaptive to institutional convenience rather than empirical robustness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_vs_correlational, empirical, 'Threshold for causal claims from genetic correlation data').

omega_variable(
    population_specificity,
    'How much do polygenic risk scores computed in one population lose predictive validity in genetically distinct populations?',
    'Cross-population comparison of polygenic score performance; validation studies in African, Asian, Indigenous, and admixed populations; measurement of bias propagation',
    'If scores generalize well: genetic predisposition is universal (field legitimacy increases). If scores fail in diverse populations: institutional gatekeeping becomes racially biased extraction (snare classification deepens; suppression gate fires harder).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(population_specificity, empirical, 'Generalizability of polygenic risk scores across populations').

omega_variable(
    intervention_cascade,
    'Do genetic risk stratification interventions (screening, counseling, preventive treatment) reduce measured adverse outcomes, or primarily reduce diagnostic uncertainty?',
    'RCTs comparing genetic screening + intervention vs standard care on clinical outcomes; measurement of unnecessary medication or treatment in low-risk individuals; health equity analysis by socioeconomic status',
    'If interventions improve outcomes: coordination function is real (tangled_rope classification). If interventions mainly reduce uncertainty without improving outcomes: institutions benefit while populations bear costs (snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_cascade, empirical, 'Whether genetic risk interventions improve health outcomes').

omega_variable(
    institutional_capture,
    'Do insurance/employment policies based on genetic information reflect evidence-based risk assessment or institutional convenience in excluding costly groups?',
    'Audit studies of genetic-based decisions; analysis of decision thresholds vs actuarial necessity; comparison of genetic exclusions to environmental risk factors of equivalent magnitude',
    'If institutional thresholds match empirical evidence: gatekeeping is rational coordination (rope). If thresholds are lower than evidence supports: gatekeeping is extractive discrimination (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture, empirical, 'Whether genetic gatekeeping matches empirical risk thresholds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_predisposition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genpred_tr_t0, genetic_predisposition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(genpred_tr_t5, genetic_predisposition, theater_ratio, 5, 0.53).
narrative_ontology:measurement(genpred_tr_t10, genetic_predisposition, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(genpred_be_t0, genetic_predisposition, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(genpred_be_t5, genetic_predisposition, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(genpred_be_t10, genetic_predisposition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_predisposition, resource_allocation).
narrative_ontology:affects_constraint(genetic_predisposition, insurance_risk_stratification).
narrative_ontology:affects_constraint(genetic_predisposition, employment_screening_systems).
narrative_ontology:affects_constraint(genetic_predisposition, reproductive_autonomy_constraint).

% DUAL FORMULATION NOTE:
% The genetic predisposition constraint decomposes into upstream and downstream components: UPSTREAM: the biological fact that genetic variants correlate with phenotypes (ε≈0.08, Mountain). DOWNSTREAM: the institutional response that uses genetic information for gatekeeping and extraction (ε=0.58, Snare). The biological fact is necessary but not sufficient for institutional extraction — extraction emerges only when institutions control access and use information asymmetrically. The two constraints are linked via network.affects_constraints: if the biological constraint (correlation) is contested or weakened by new evidence, the institutional constraint's justification erodes but institutional practices persist (Piton degradation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genetic_predisposition, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
