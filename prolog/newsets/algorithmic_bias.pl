% ============================================================================
% CONSTRAINT STORY: algorithmic_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_bias, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_bias
 *   human_readable: Algorithmic Bias
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic bias occurs when automated systems create systematically
 *   prejudiced outcomes, typically by encoding and scaling historical
 *   inequities found in training data. This constraint is a diagnostic
 *   exemplar, demonstrating how a single technological phenomenon can be
 *   classified as all six DR types depending on the observer's structural
 *   position. The core conflict is between the system deployers, who see an
 *   efficient coordination tool, and the marginalized populations, who
 *   experience an opaque mechanism of extraction that denies them
 *   opportunities in critical areas like employment, finance, and justice.
 *
 * KEY AGENTS:
 *   - Historically Marginalized Demographics: Primary victims (powerless/trapped) — Systematically disadvantaged by biased outputs.
 *   - System Deployers (Corporations/Institutions): Primary beneficiaries (institutional/arbitrage) — Gain efficiency and cost savings by automating decisions.
 *   - Data Scientists: Implementers (moderate/mobile) — Caught between technical goals and ethical implications.
 *   - AI Ethics Regulators: Reformers (organized/constrained) — Attempt to build a scaffold of rules and standards to mitigate harm.
 *   - Corporate PR Departments: Performers (institutional/arbitrage) — Manage public perception through 'ethics-washing'.
 *   - Techno-Determinists: Observers (analytical/analytical) — Naturalize the bias as an unavoidable reflection of reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_bias, 0.68).
domain_priors:suppression_score(algorithmic_bias, 0.75).
domain_priors:theater_ratio(algorithmic_bias, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_bias, extractiveness, 0.68).
narrative_ontology:constraint_metric(algorithmic_bias, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(algorithmic_bias, theater_ratio, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_bias, tangled_rope).
narrative_ontology:human_readable(algorithmic_bias, "Algorithmic Bias").
narrative_ontology:topic_domain(algorithmic_bias, "technological/social").

domain_priors:requires_active_enforcement(algorithmic_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_bias, system_deployers).
narrative_ontology:constraint_beneficiary(algorithmic_bias, historically_privileged_demographics).
narrative_ontology:constraint_victim(algorithmic_bias, historically_marginalized_demographics).
narrative_ontology:constraint_victim(algorithmic_bias, societal_fairness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MARGINALIZED APPLICANT (SNARE) — Cannot opt-out of biased automated systems for loans, housing, or employment. Experiences the system as an opaque, inescapable mechanism of pure extraction that denies opportunity based on group identity. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(algorithmic_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SYSTEM DEPLOYER (ROPE) — Experiences the algorithm as a pure coordination tool for efficiency, risk management, and profit maximization. The costs (bias) are externalized, while the benefits (cost savings, speed) are internalized. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(algorithmic_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE DATA SCIENTIST (TANGLED ROPE) — Directly engages with both the coordination function (building a predictive model) and the extractive reality (witnessing bias in the training data and outcomes). They are caught between professional incentives for 'accuracy' and ethical concerns about fairness. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE AI ETHICS REGULATOR (SCAFFOLD) — Views algorithmic bias as a temporary market failure to be corrected by new standards, auditing requirements, and legislation (e.g., EU AI Act). These interventions act as a scaffold, intended to support the transition to a fairer technological ecosystem, with an implicit sunset clause as norms and technologies mature.
constraint_indexing:constraint_classification(algorithmic_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CORPORATE PR DEPARTMENT (PITON) — Engages in 'ethics-washing' by publishing vague AI principles and commissioning favorable audits. The function is almost entirely performative, designed to maintain public trust and preempt regulation, while the underlying extractive systems remain unchanged. The high theater_ratio (0.72) satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(algorithmic_bias, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE TECHNO-DETERMINIST (MOUNTAIN) — Frames bias as an unfortunate but unavoidable reflection of 'ground truth' in the data. From this viewpoint, the algorithm is merely a mirror to society's existing inequalities, making the resulting bias a natural, immutable law of data-driven systems. The engine will flag this as a false summit, as the high ε and suppression are products of contingent design choices, not natural law.
constraint_indexing:constraint_classification(algorithmic_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_bias, TR),
    TR >= 0.70.

:- end_tests(algorithmic_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high because the constraint systematically diverts life-altering opportunities (jobs, loans, parole) away from certain groups. Suppression (0.75) is high due to the opacity of 'black box' models and the lack of alternatives when systems are deployed at scale. Theater Ratio (0.72) is high, reflecting the widespread corporate practice of 'ethics-washing'—publishing vague principles and performing superficial audits that serve a PR function without altering the underlying extractive dynamics. Both extractiveness and theater have increased significantly since 2010 as systems became more powerful and public scrutiny grew.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. For a marginalized applicant (powerless/trapped), the system is a Snare. For the corporation deploying it (institutional/arbitrage), it's a Rope. For the regulator trying to fix it (organized/constrained), it's a Scaffold. For the PR team spinning it (institutional/arbitrage), it's a Piton. For the data scientist building it (moderate/mobile), it's a Tangled Rope. For the determinist who naturalizes it (analytical/analytical), it's a Mountain. This diversity reveals that 'algorithmic bias' is not a monolithic object but a structural relationship, and its classification is an indexical fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (system deployers, privileged demographics) have low directionality (d), resulting in low or negative effective extraction (χ), hence they perceive a Rope. Victims (marginalized demographics) are trapped, giving them maximum directionality (d≈0.95) and thus maximum effective extraction (χ), making the system a Snare from their perspective. Other agents fall in between, their derived `d` value shaping their view of the constraint as a Tangled Rope, Scaffold, etc.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that no single classification is sufficient. To label algorithmic bias as only a 'Snare' would be to ignore the genuine (if problematic) coordination function it serves for its deployers. To label it only a 'Rope' would be to erase the severe extraction experienced by its victims. The Deferential Realism framework shows that all perspectives are structurally valid readings from different positions. The complete description is the full set of indexed classifications, not a single 'correct' one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_remediability,
    'Can algorithmic bias be technically ''de-biased'' through fairness-aware ML techniques, or is it an inherent feature of statistical optimization on historical data?',
    'Longitudinal studies of deployed ''fair'' models to measure if bias re-emerges or if fairness metrics successfully prevent harm over time.',
    'If technically remediable, the constraint is closer to a Scaffold (a temporary problem). If inherent, it is a more permanent Snare/Tangled Rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_remediability, empirical, 'Whether bias is technically correctable or inherent to the paradigm').

omega_variable(
    fairness_definition_conflict,
    'Which definition of fairness (e.g., demographic parity, equal opportunity, equalized odds) is appropriate, given that they are often mutually exclusive?',
    'Legal and philosophical consensus-building, likely codified in sector-specific regulation.',
    'The choice of fairness metric fundamentally alters the beneficiary/victim structure, protecting one group often at the expense of another''s statistical outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fairness_definition_conflict, conceptual, 'Conflict between mutually exclusive mathematical definitions of fairness').

omega_variable(
    opacity_vs_accountability,
    'Is the ''black box'' nature of complex models a necessary trade-off for accuracy, or is it a deliberate mechanism to evade accountability?',
    'Breakthroughs in explainable AI (XAI) or regulatory mandates for model transparency and auditability.',
    'If opacity is a necessary evil, suppression remains high. If it is a choice, suppression could be lowered via regulation, shifting the classification away from Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_vs_accountability, empirical, 'Whether model opacity is a necessary feature or a choice to evade accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_bias, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t2010, algorithmic_bias, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(algo_tr_t2017, algorithmic_bias, theater_ratio, 2017, 0.5).
narrative_ontology:measurement(algo_tr_t2024, algorithmic_bias, theater_ratio, 2024, 0.72).

% Extraction over time
narrative_ontology:measurement(algo_be_t2010, algorithmic_bias, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(algo_be_t2017, algorithmic_bias, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(algo_be_t2024, algorithmic_bias, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_bias, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_bias, credit_scoring_systems).
narrative_ontology:affects_constraint(algorithmic_bias, predictive_policing).
narrative_ontology:affects_constraint(algorithmic_bias, automated_hiring_filters).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
