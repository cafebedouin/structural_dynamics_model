% ============================================================================
% CONSTRAINT STORY: algorithmic_bias_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_bias_externality, []).

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
 *   constraint_id: algorithmic_bias_externality
 *   human_readable: Algorithmic Bias Externality in Automated Decision Systems
 *   domain: technology/AI_ethics/social_equity
 *
 * SUMMARY:
 *   Algorithmic bias externality describes the structural constraint in which
 *   automated decision systems trained on historical data systematize and
 *   amplify existing discrimination while distributing costs to marginalized
 *   groups and concentrating benefits in deploying organizations. The
 *   constraint exhibits both genuine coordination functions (automating
 *   decisions at scale requires standardization; fairness requirements do
 *   improve outcomes for some) and extraction mechanisms (organizations
 *   externalize discrimination costs; regulatory compliance remains largely
 *   performative; affected populations lack exit options). The constraint's
 *   evolution over 2015-2025 shows increasing extractiveness (algorithms
 *   deployed across more high-stakes domains) and rising theater ratio
 *   (fairness disclosures, bias audits, and diversity statements proliferate
 *   while actual outcome disparities persist). This is a diagnostic exemplar
 *   of regulatory capture applied to emerging technology: initial fairness
 *   mechanisms (audits, transparency) function as coordination at low
 *   theater. As deployment scales and organizational interest in concealing
 *   bias rises, theater increases faster than actual fairness improvements,
 *   shifting the constraint toward Piton. Simultaneously, regulatory
 *   frameworks (EU AI Act, algorithmic impact assessments) represent Scaffold
 *   sunset mechanisms — if effective, they would constrain extraction over
 *   5-10 year horizons. The empirical question is whether regulation adapts
 *   fast enough to prevent Snare classification.
 *
 * KEY AGENTS:
 *   - Marginalized demographic groups (powerless/trapped) — subjected to discriminatory algorithmic decisions in hiring, lending, criminal justice; cannot exit or contest; bear full cost of discrimination externality
 *   - Technology companies deploying algorithms (institutional/arbitrage) — benefit from automation cost reductions; experience compliance requirements as modest coordination overhead; high exit options through jurisdictional arbitrage
 *   - Civil rights organizations (moderate/constrained) — limited resources for algorithm auditing and discrimination detection; benefit from increased transparency but constrained by institutional barriers
 *   - Regulatory bodies (organized/constrained) — building algorithmic governance frameworks with sunset logic; constrained by industry pressure and technical complexity
 *   - Fairness researchers and auditors (moderate/constrained) — produce fairness metrics and bias detection tools; constrained by access to proprietary algorithms and deployment data
 *   - Equal opportunity principle (powerless/trapped) — abstract collective good; primary victim; cannot organize or exit; bears externalized discrimination costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_bias_externality, 0.58).
domain_priors:suppression_score(algorithmic_bias_externality, 0.62).
domain_priors:theater_ratio(algorithmic_bias_externality, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_bias_externality, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_bias_externality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_bias_externality, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_bias_externality, tangled_rope).
narrative_ontology:human_readable(algorithmic_bias_externality, "Algorithmic Bias Externality in Automated Decision Systems").
narrative_ontology:topic_domain(algorithmic_bias_externality, "technology/AI_ethics/social_equity").

domain_priors:requires_active_enforcement(algorithmic_bias_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_bias_externality, algorithm_deployers).
narrative_ontology:constraint_beneficiary(algorithmic_bias_externality, cost_minimizing_organizations).
narrative_ontology:constraint_victim(algorithmic_bias_externality, marginalized_demographic_groups).
narrative_ontology:constraint_victim(algorithmic_bias_externality, equal_opportunity_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRIMINATED ALGORITHM SUBJECT (SNARE) — Lacks exit options from algorithmic decisions in hiring, lending, criminal justice, housing. Cannot opt out of automated systems that govern access to employment, credit, or liberty. Trapped by system opacity and inability to contest or escape. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(algorithmic_bias_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL RIGHTS ADVOCACY ORGANIZATION (TANGLED ROPE) — Benefits from algorithmic efficiency improvements and transparency mechanisms while bearing costs of ongoing discrimination detection and remediation work. Constrained by resource limitations and institutional barriers to algorithm auditing. Mixed coordination and extraction.
constraint_indexing:constraint_classification(algorithmic_bias_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANY DEPLOYING ALGORITHM (ROPE) — Experiences the constraint as coordination: bias mitigation protocols, fairness metrics, and transparency reports coordinate with regulatory requirements and stakeholder expectations. Benefits from efficiency gains while paying modest compliance costs. High exit optionality through arbitrage across jurisdictions.
constraint_indexing:constraint_classification(algorithmic_bias_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGULATORY AND STANDARDS BODIES (SCAFFOLD) — Organized actors (EU AI Act, NIST AI Risk Management, algorithmic impact assessments) are building sunset provisions into algorithmic governance: mandatory audits, fairness certifications, and transparency requirements are designed to phase out or constrain high-bias systems over 5-10 year horizons. Extraction decreases as regulatory standards mature.
constraint_indexing:constraint_classification(algorithmic_bias_externality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALGORITHMIC FAIRNESS THEATER (PITON) — Fairness metrics, bias audits, and diversity statements persist through institutional inertia despite limited actual impact on discriminatory outcomes. Organizations deploy fairness theater (debiasing reports, diversity dashboards) that create appearance of addressing bias while preserving core extraction mechanisms. Theater ratio high because compliance activities diverge from actual system behavior.
constraint_indexing:constraint_classification(algorithmic_bias_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, algorithmic bias is both a genuine coordination problem (automating decisions at scale requires standards) and an extraction mechanism (automation amplifies historical bias and distributes costs to marginalized groups while concentrating benefits in deploying institutions). The constraint persists because it solves coordination problems for deployers while externalizing discrimination costs onto victims.
constraint_indexing:constraint_classification(algorithmic_bias_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_bias_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_bias_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_bias_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_bias_externality, TR),
    TR >= 0.70.

:- end_tests(algorithmic_bias_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. Organizations deploying algorithms capture efficiency gains (cost reduction, faster decisions, operational scale) that are worth 0.30-0.40 in pure extraction value. However, fairness mechanisms and regulatory pressure impose compliance costs that reduce net extraction to ~0.58. The value is high because extraction is concentrated and persistent, but not in the maximum range because some fairness constraints are working. Suppression (0.62): High. Marginalized groups face structural barriers to exit: algorithmic decisions in employment, lending, and criminal justice are mandatory or effectively unavoidable. Opacity of algorithmic logic prevents understanding of or contesting discrimination. However, suppression is not total (0.95+) because some individuals can appeal decisions, litigation exists, and media attention creates modest constraints on flagrant bias. Theater ratio (0.68): High and rising. Organizations deploy fairness audits, bias mitigation reports, diversity statements, and fairness metrics that create appearance of addressing discrimination. These activities are partially functional (they do detect some bias) but substantially performative (metric selection biases toward favorable results; audits often do not change deployment decisions). Theater has increased over the measurement interval because organizational incentives to demonstrate fairness compliance have risen faster than genuine fairness improvements.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as fundamentally different types depending on the observer's structural position. The deployed algorithm's organization experiences Rope — fairness compliance is a coordination mechanism for managing stakeholder expectations and regulatory risk. The marginalized group subjected to discrimination experiences Snare — no coordination benefit, only extraction, no exit. The analytical observer sees Tangled Rope — genuine coordination problems (automating high-stakes decisions requires standards; some fairness mechanisms work) coexist with extraction mechanisms (bias amplification; cost externalization; regulatory capture). The regulatory body sees Scaffold — algorithmic governance frameworks are being built with sunset logic. The fairness theater perspective (Piton) reveals institutional degradation: bias audits and fairness metrics persist through compliance inertia even when they fail to prevent discrimination. The perspectival gaps reveal that the constraint is not a simple extraction system but a hybrid coordination-extraction mechanism where the coordination functions primarily serve the deployers while the extraction costs are borne by marginalized groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim relationships and exit options. Technology companies (institutional/arbitrage) are beneficiaries with high exit optionality — they can relocate operations, change algorithms, or lobby for favorable regulations. The pipeline computes d ≈ 0.15, producing low effective extractiveness chi from their perspective (they experience the constraint as coordination, not extraction). Marginalized demographic groups (powerless/trapped) are victims with zero exit optionality — they cannot opt out of algorithmic decisions in employment, lending, or criminal justice. The pipeline computes d ≈ 0.95, producing high effective extractiveness chi. Civil rights organizations (moderate/constrained) are secondary victims with high but not zero exit costs — they could redirect resources away from algorithmic fairness work, but this would abandon affected populations. The pipeline computes d ≈ 0.70. Regulatory bodies (organized/constrained) are neither pure beneficiaries nor victims — they coordinate standard-setting while bearing enforcement costs. The pipeline computes d ≈ 0.50. These derived directionality values produce the perspectival gap: same constraint, six different experienced extraction intensities based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by explicitly declaring both coordination and extraction functions. Genuine coordination exists: automating decisions at scale requires standardization; fairness metrics improve outcomes for some groups; regulatory frameworks are building better governance. Genuine extraction exists: discriminatory algorithms amplify historical bias; costs are externalized to marginalized groups; deploying organizations benefit from efficiency gains and bear modest compliance costs. The constraint is Tangled Rope because both are true and persistent. The mandate to classify it would fail if forced to choose between 'pure coordination' (Rope) and 'pure extraction' (Snare) — the constraint is structurally hybrid. The analytical observer's Tangled Rope classification reflects this hybridity. The Piton perspective (fairness theater) reveals how the hybrid nature creates degradation risk: if compliance activities become entirely performative, the coordination function atrophies and the constraint slides toward Snare. The Scaffold perspective reveals a path forward: if regulatory sunset mechanisms work, the constraint could transition to genuine coordination at lower extraction. The empirical question is which trajectory — degradation toward Snare or maturation toward Scaffold — will dominate over the next 5-10 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fairness_metric_capture,
    'Do algorithmic fairness metrics genuinely constrain bias or enable organizations to claim fairness while perpetuating discrimination through metric selection?',
    'Longitudinal comparison of fairness metric scores vs. actual outcome disparities post-deployment; analysis of metric selection patterns by industry and demographic group',
    'If metrics genuinely constrain: classification shifts toward Rope (fairness mechanisms work). If metrics enable gaming: classification confirms Snare (theater disguises extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_metric_capture, empirical, 'Whether fairness metrics constrain bias or enable metric gaming').

omega_variable(
    bias_source_causality,
    'Is observed outcome disparity caused by algorithmic bias amplification of historical data patterns, by human decision-making that the algorithm amplifies, or by structural inequality that the algorithm merely reflects?',
    'Counterfactual analysis comparing algorithmic decisions to human-only decisions on identical cases; causal decomposition of outcome variance',
    'If algorithm amplifies: suppression is primarily technical and remediable (shift to Scaffold). If algorithm reflects structural inequality: suppression is socially embedded and remediation requires systemic change (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_source_causality, empirical, 'Causal source of observed outcome disparities').

omega_variable(
    regulatory_effectiveness_lag,
    'Can regulatory frameworks (audit requirements, transparency mandates, fairness certifications) adapt at sufficient speed to constrain bias in rapidly evolving algorithmic systems?',
    'Comparative analysis of regulatory implementation timelines vs. algorithmic deployment cycles in high-bias domains (criminal justice, hiring, lending); tracking of regulatory capture by technology industry',
    'If regulations keep pace: Scaffold sunset mechanism is real. If regulations lag: Piton theater persists and Snare extraction continues unconstrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_lag, empirical, 'Whether regulatory frameworks can keep pace with algorithmic evolution').

omega_variable(
    victim_coalition_viability,
    'Can marginalized demographic groups organize collective resistance to discriminatory algorithms, or are individual victims too dispersed to coordinate?',
    'Historical analysis of successful algorithmic discrimination lawsuits, class action formations, and advocacy coalition effectiveness; network analysis of victim coordination capacity',
    'If coalition formation viable: powerless agent power level could upgrade to organized, changing classification. If dispersed: powerless agents remain trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_viability, empirical, 'Whether victims can form effective coalitions against algorithmic discrimination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_bias_externality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algbias_tr_t0, algorithmic_bias_externality, theater_ratio, 0, 0.45).
narrative_ontology:measurement(algbias_tr_t3, algorithmic_bias_externality, theater_ratio, 3, 0.62).
narrative_ontology:measurement(algbias_tr_t6, algorithmic_bias_externality, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(algbias_be_t0, algorithmic_bias_externality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algbias_be_t3, algorithmic_bias_externality, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(algbias_be_t6, algorithmic_bias_externality, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_bias_externality, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_bias_externality, criminal_risk_assessment_automation).
narrative_ontology:affects_constraint(algorithmic_bias_externality, predictive_policing_deployment).
narrative_ontology:affects_constraint(algorithmic_bias_externality, hiring_algorithm_gatekeeping).
narrative_ontology:affects_constraint(algorithmic_bias_externality, lending_discrimination_amplification).

% DUAL FORMULATION NOTE:
% Algorithmic bias externality is upstream of domain-specific deployments (criminal justice, hiring, lending). Each domain has its own constraint story reflecting domain-specific bias sources and regulatory contexts. This story captures the general structural constraint; domain stories capture manifestations with different epsilon values reflecting differing levels of transparency and regulatory maturity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_bias_externality, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
