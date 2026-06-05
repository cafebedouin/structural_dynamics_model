% ============================================================================
% CONSTRAINT STORY: insurance_risk_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insurance_risk_stratification, []).

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
 *   constraint_id: insurance_risk_stratification
 *   human_readable: Insurance Risk Stratification and Premium Extraction
 *   domain: economic/insurance/actuarial
 *
 * SUMMARY:
 *   Insurance risk stratification creates a structural tension between
 *   legitimate actuarial coordination (separating high-risk and low-risk
 *   pools to enable accurate pricing and sustainable markets) and extractive
 *   discrimination (using stratification to exclude or charge excessive
 *   premiums to unfavorable risk categories). The constraint exhibits the
 *   full diagnostic range of Deferential Realism classifications depending on
 *   the observer's structural position. For low-risk individuals and
 *   institutional underwriters, stratification solves the adverse selection
 *   problem and enables efficient markets (Rope). For high-risk individuals
 *   trapped in constrained labor markets with pre-existing conditions,
 *   stratification becomes a mechanism of perpetual extraction with no exit
 *   (Snare). For organized health equity coalitions, stratification mixes
 *   genuine coordination with extractive outcomes, requiring regulatory
 *   rebalancing (Tangled Rope). The regulatory framework designed to prevent
 *   discrimination is itself substantially performative, permitting
 *   proxy-based stratification that replicates the discrimination it
 *   ostensibly prevents (Piton). The analytical observer risks naturalizing
 *   stratification as an immutable information-asymmetry problem (Mountain),
 *   when alternative coordination mechanisms exist and have been successfully
 *   deployed in other jurisdictions. The extractiveness trajectory (0.35 →
 *   0.58 over 20 years) reflects increasing use of algorithmic risk scores
 *   and granular data harvesting, enabling more precise segmentation and
 *   higher extraction targeting. The theater ratio trajectory (0.32 → 0.48)
 *   reflects the rise of algorithmic opaqueness: early stratification was
 *   transparent actuarial logic; modern stratification uses machine-learning
 *   black boxes that obscure the classification basis and prevent meaningful
 *   regulatory oversight.
 *
 * KEY AGENTS:
 *   - Insurance Underwriters: Primary beneficiary (institutional/arbitrage) — capture efficiency gains and cost reduction through risk separation; can arbitrage to alternative models
 *   - Low-Risk Individuals: Secondary beneficiary (powerful/arbitrage) — benefit from lower premiums; strong bargaining position with multiple insurers
 *   - High-Risk Individuals: Primary victim (powerless/trapped) — face escalating premiums, limited market access, no realistic exit options
 *   - Adverse Selection Targets: Secondary victim (moderate/constrained) — selected out of standard risk pools, forced into substandard products at high cost
 *   - Health Equity Coalition: Organized agent (organized/constrained) — advocates for regulatory constraints on proxy discrimination; has some agency but cannot eliminate stratification
 *   - Insurance Regulators: Institutional actor (institutional/constrained) — ostensibly enforce non-discrimination rules but often captured by underwriter interests; maintain performative framework
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating information asymmetry as natural law rather than contingent market condition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insurance_risk_stratification, 0.58).
domain_priors:suppression_score(insurance_risk_stratification, 0.62).
domain_priors:theater_ratio(insurance_risk_stratification, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insurance_risk_stratification, extractiveness, 0.58).
narrative_ontology:constraint_metric(insurance_risk_stratification, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(insurance_risk_stratification, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insurance_risk_stratification, tangled_rope).
narrative_ontology:human_readable(insurance_risk_stratification, "Insurance Risk Stratification and Premium Extraction").
narrative_ontology:topic_domain(insurance_risk_stratification, "economic/insurance/actuarial").

domain_priors:requires_active_enforcement(insurance_risk_stratification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insurance_risk_stratification, insurance_underwriters).
narrative_ontology:constraint_beneficiary(insurance_risk_stratification, low_risk_pools).
narrative_ontology:constraint_victim(insurance_risk_stratification, high_risk_individuals).
narrative_ontology:constraint_victim(insurance_risk_stratification, adverse_selection_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-RISK INDIVIDUAL (SNARE) — Trapped by pre-existing conditions, demographic factors, or health history. Cannot exit insurance markets (legally mandated in many jurisdictions) or move to lower-risk categories. Bears maximum extraction as premiums escalate with every new medical event. No alternative markets available; suppression is structural.
constraint_indexing:constraint_classification(insurance_risk_stratification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELF-EMPLOYED HIGH-RISK WORKER (TANGLED ROPE) — Faces high premiums and limited access to group plans, but retains some agency through employer status or geographic arbitrage. Can reduce premiums through lifestyle changes (coordination function) but bears significant extraction cost. Exit is possible at high cost (relocate to lower-cost region, change occupation).
constraint_indexing:constraint_classification(insurance_risk_stratification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURANCE UNDERWRITER (ROPE) — Experiences risk stratification as a coordination mechanism: precise categorization enables risk pooling and prevents cross-subsidization. Arbitrage exit available (alternative underwriting models, peer-to-peer insurance). Net beneficiary — extraction runs toward this institution. Classification reflects genuine coordination function: stratification solves the adverse selection problem.
constraint_indexing:constraint_classification(insurance_risk_stratification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOW-RISK POOL (ROPE) — Benefits from stratification through lower premiums. Experiences the constraint as pure coordination: accurate risk assessment keeps their premiums low and prevents subsidization of high-risk pools. Arbitrage exit available (self-insurance, direct arrangements). Strong net beneficiary.
constraint_indexing:constraint_classification(insurance_risk_stratification, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HEALTH EQUITY COALITION (TANGLED ROPE) — Organized agents (patient advocacy, public health authorities, regulators) see stratification as mixing coordination (actuarial accuracy) with extraction (discriminatory pricing that reproduces health inequities). Can constrain underwriting practices through regulation but cannot eliminate stratification entirely. Seeks to rebalance the coordination-extraction mix.
constraint_indexing:constraint_classification(insurance_risk_stratification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Insurance regulation ostensibly prevents discrimination, but the framework is largely performative: regulators prohibit explicit use of protected characteristics (race, gender) while permitting proxies (ZIP code, occupation, education) that replicate the same stratification. Theater ratio reflects regulatory capture — rules exist but enforcement creates exceptions. The constraint persists through institutional inertia.
constraint_indexing:constraint_classification(insurance_risk_stratification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, risk stratification is an inherent response to information asymmetry: insurers cannot know individual risk profiles perfectly, so they use proxies. This perspective sees the stratification as a natural law of insurance markets. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that information asymmetry is manageable through alternative mechanisms (community rating, subsidized pools, transparency mandates) rather than an immutable constraint.
constraint_indexing:constraint_classification(insurance_risk_stratification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insurance_risk_stratification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insurance_risk_stratification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insurance_risk_stratification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insurance_risk_stratification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insurance_risk_stratification, TR),
    TR >= 0.70.

:- end_tests(insurance_risk_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Risk stratification generates significant extraction for high-risk agents (premiums escalate far beyond actuarially justified levels due to cream-skimming and exclusion mechanisms), but extraction is not total (low-risk agents benefit, coordination function is real). The value reflects that stratification successfully solves the adverse selection coordination problem while simultaneously enabling substantial extraction from constrained agents. Suppression (0.62): High. Significant barriers to exit include: (1) legal mandates requiring insurance coverage in many jurisdictions, eliminating market exit; (2) pre-existing conditions and health status are largely immutable, preventing risk category changes; (3) geographic and occupational constraints limit arbitrage; (4) information asymmetry prevents high-risk individuals from challenging actuarial classifications. Theater ratio (0.48): Moderate. Early insurance stratification was relatively transparent actuarial logic; modern stratification increasingly uses algorithmic black boxes (credit scores, behavioral data, algorithmic risk models) that obscure the classification basis. Some of the performative element reflects regulatory compliance theater (non-discrimination rules are followed in letter but circumvented through proxies); some reflects genuine technical opacity.
 *
 * PERSPECTIVAL GAP:
 *   High-risk individuals experience this constraint as a Snare (high extraction, no exit, maximum suppression) while underwriters experience it as Rope (coordination that solves adverse selection, arbitrage available). This gap is not a measurement error — it reflects genuine structural asymmetry. The constraint simultaneously coordinates (separates pools to prevent cross-subsidization) and extracts (charges constrained agents far above actuarially justified premiums). The analytical observer risks conflating coordination function with legitimate outcome, naturalizing the extraction as necessary information-processing cost.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk individuals are trapped victims (d→0.94, f(d)→1.38). Underwriters are arbitrage beneficiaries (d→0.08, f(d)→-0.10). Low-risk individuals are mobile beneficiaries (d→0.22, f(d)→0.08). Constrained agents (self-employed, occupationally locked) are constrained victims (d→0.68, f(d)→1.08). Organized coalitions are organized agents with constrained exit (d→0.48, f(d)→0.65). The regulatory framework has institutional status but constrained exit from insurance market logic (d→0.45, f(d)→0.58). These structural derivations explain why the same constraint classifies as Snare, Rope, and Tangled Rope from different positions — the d values differ by a factor of ten, producing radically different effective extraction experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint is genuinely hybrid — it solves the adverse selection coordination problem (Rope function) while simultaneously enabling asymmetric extraction (Snare outcome). The mandatrophy resolves by acknowledging that risk stratification is not correctly classified as pure extraction (Snare alone) nor as pure coordination (Rope alone). It is a Tangled Rope where the coordination and extraction functions are deeply entangled. The analytical observer's Mountain classification (information asymmetry is inherent) is a false summit — information asymmetry is real, but alternative coordination mechanisms exist (community rating, subsidized pools, transparency mandates) that achieve similar risk pooling without the same extraction mechanisms. The regulatory framework's Piton classification reveals that the rules designed to prevent extraction are largely performative — explicit protections (no discrimination by race, gender) are circumvented through proxies (ZIP code, occupation, credit score) that replicate the same effects. The health equity coalition's Tangled Rope perspective indicates that the coordination-extraction split can be rebalanced through regulatory intervention without eliminating risk stratification entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_vs_causation_ambiguity,
    'Does risk stratification based on proxies (ZIP code, occupation, family medical history) identify genuine causal risk factors or merely replicate historical discrimination patterns?',
    'Actuarial validation studies separating correlation from causation; cross-jurisdictional analysis comparing outcomes under stratification vs community rating models',
    'If proxies identify genuine risk: stratification is justifiable coordination (Rope dominates). If proxies replicate discrimination: stratification is extractive proxy enforcement (Snare dominates, regulatory framework fails).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_vs_causation_ambiguity, empirical, 'Whether risk proxies measure genuine causation or replicate discrimination').

omega_variable(
    adverse_selection_quantification,
    'What proportion of insurance market instability is attributable to adverse selection vs other factors (claims inflation, underpricing, operational costs)?',
    'Historical claims data analysis under different stratification regimes; comparison of market stability in community-rating vs risk-stratified systems',
    'If adverse selection is dominant (>60%): stratification is essential coordination. If minimal (<20%): stratification is unnecessary extraction justified by exaggerated actuarial concerns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adverse_selection_quantification, empirical, 'Quantification of adverse selection as driver of stratification necessity').

omega_variable(
    lifestyle_modifiability_assumption,
    'Are the behavioral changes implied by risk stratification (lifestyle modifications) actually achievable for constrained agents, or does the stratification assume agency that doesn''t exist?',
    'Longitudinal tracking of high-risk individuals'' ability to reduce premiums through behavioral change; comparison of success rates across socioeconomic strata',
    'If achievable across strata: stratification provides legitimate coordination incentive. If impossible for constrained agents: stratification extracts from those with no realistic exit pathway (Snare, not Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lifestyle_modifiability_assumption, empirical, 'Whether behavioral modification is achievable across risk strata').

omega_variable(
    regulatory_proxy_enforcement,
    'Are insurance regulators actually enforcing non-discrimination rules, or does regulatory capture permit proxy enforcement that circumvents explicit protections?',
    'Analysis of regulatory enforcement actions; comparison of explicit vs proxy-based stratification patterns before/after regulatory intervention',
    'If enforced: some Piton theater is eliminated, constraint lowers toward genuine Rope. If captured: regulatory framework is purely performative (Piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_proxy_enforcement, empirical, 'Whether regulatory enforcement constrains or permits proxy discrimination').

omega_variable(
    alternative_coordination_feasibility,
    'Can alternative insurance models (mutual aid, community rating, subsidized pools, public option) coordinate the genuine coordination function of risk pooling without the extraction mechanism?',
    'Comparative analysis of alternative models'' sustainability, coverage rates, and adverse selection outcomes',
    'If feasible: stratification is unnecessary (reclassify as Snare, not Tangled Rope). If infeasible: stratification is genuinely hybrid coordination-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, conceptual, 'Whether alternative models can replicate coordination without extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insurance_risk_stratification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ins_risk_tr_t0, insurance_risk_stratification, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ins_risk_tr_t10, insurance_risk_stratification, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ins_risk_tr_t20, insurance_risk_stratification, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ins_risk_tr_t5, insurance_risk_stratification, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(ins_risk_be_t0, insurance_risk_stratification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ins_risk_be_t10, insurance_risk_stratification, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ins_risk_be_t20, insurance_risk_stratification, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ins_risk_be_t5, insurance_risk_stratification, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insurance_risk_stratification, resource_allocation).
narrative_ontology:affects_constraint(insurance_risk_stratification, health_care_access_inequality).
narrative_ontology:affects_constraint(insurance_risk_stratification, algorithmic_discrimination).
narrative_ontology:affects_constraint(insurance_risk_stratification, adverse_selection_market_spiral).

% DUAL FORMULATION NOTE:
% Insurance risk stratification decomposes into multiple structurally distinct constraints: (1) actuarial coordination (risk pooling), (2) proxy-based discrimination (replication of protected class exclusion), (3) information asymmetry (market condition enabling cream-skimming), and (4) regulatory capture (performative non-discrimination). Each has different extractiveness and should be tracked separately as a constraint family if detailed decomposition is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(insurance_risk_stratification, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
