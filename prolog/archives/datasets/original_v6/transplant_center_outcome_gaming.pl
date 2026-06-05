% ============================================================================
% CONSTRAINT STORY: transplant_center_outcome_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transplant_center_outcome_gaming, []).

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
 *   constraint_id: transplant_center_outcome_gaming
 *   human_readable: Transplant Center Outcome Gaming Under Risk-Adjusted Mortality Metrics
 *   domain: healthcare/organ_transplantation
 *
 * SUMMARY:
 *   Transplant centers in the United States operate under risk-adjusted
 *   mortality metrics (UNOS/CMS reporting systems) designed to enable fair
 *   comparison of surgical quality across centers with different patient
 *   case-mixes. This constraint creates a structural problem: centers have
 *   strong incentives to game outcomes by selecting lower-risk patients,
 *   delaying reports of poor outcomes, and avoiding marginal cases. The
 *   genuine coordination function — enabling the system to identify
 *   underperforming centers and distribute organs efficiently — coexists with
 *   a perverse extraction function: high-performing centers and elite
 *   surgeons capture reputational benefits and patient volume by gaming
 *   metrics, while high-risk patients and the system's equity objective bear
 *   the costs. This is a prototypical tangled rope: the constraint solves a
 *   real coordination problem (how to compare centers fairly) while
 *   simultaneously enabling asymmetric extraction (risk-averse centers
 *   exclude marginalized patients). The theater_ratio (0.68) reflects that
 *   outcome reporting has evolved into a performative ritual — centers invest
 *   heavily in statistical gaming and patient selection rather than genuine
 *   outcome improvement, and the clinical literature increasingly documents
 *   risk stratification rather than actual quality variation.
 *
 * KEY AGENTS:
 *   - High-Risk Patients (Powerless/Trapped): Structurally excluded from transplant lists by gaming — bear full cost of adverse selection
 *   - Elite Transplant Centers (Institutional/Arbitrage): Primary beneficiaries — capture reputation, patient selectivity, and funding through favorable metrics; can avoid high-risk cases
 *   - Modest-Performing Centers (Moderate/Constrained): Secondary victims — face competitive pressure and regulatory liability from gaming; constrained by need to participate in metrics
 *   - UNOS/CMS Regulatory Bodies (Institutional/Constrained): Enforcer of the outcome metric system; constrained by political pressure from both high-performing centers (lobbying against transparency) and patient advocates (demanding access)
 *   - Organ Allocation Equity Objective (Powerless/Trapped): Abstract system value that cannot organize or exit; bears cost of distorted allocation patterns
 *   - Transplant System Integrity (Powerless/Trapped): Epistemic commons damaged by selective reporting and outcome inflation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transplant_center_outcome_gaming, 0.58).
domain_priors:suppression_score(transplant_center_outcome_gaming, 0.65).
domain_priors:theater_ratio(transplant_center_outcome_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transplant_center_outcome_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(transplant_center_outcome_gaming, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(transplant_center_outcome_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transplant_center_outcome_gaming, tangled_rope).
narrative_ontology:human_readable(transplant_center_outcome_gaming, "Transplant Center Outcome Gaming Under Risk-Adjusted Mortality Metrics").
narrative_ontology:topic_domain(transplant_center_outcome_gaming, "healthcare/organ_transplantation").

domain_priors:requires_active_enforcement(transplant_center_outcome_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transplant_center_outcome_gaming, high_performing_centers).
narrative_ontology:constraint_beneficiary(transplant_center_outcome_gaming, elite_surgeons).
narrative_ontology:constraint_victim(transplant_center_outcome_gaming, high_risk_patients).
narrative_ontology:constraint_victim(transplant_center_outcome_gaming, organ_allocation_equity).
narrative_ontology:constraint_victim(transplant_center_outcome_gaming, transplant_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-RISK PATIENT (SNARE) — Structurally trapped. Cannot refuse a risky transplant (waiting list offers limited organs). Cannot exit the allocation system. Center outcome gaming directly harms this agent: risk-averse centers reject their cases, creating artificial scarcity. Maximum experienced extraction with no alternatives.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODEST-PERFORMING CENTER (TANGLED ROPE) — Constrained by competitive pressure and regulatory oversight. Faces both coordination (genuine need to report outcomes for system-wide learning) and extraction (reporting metrics creates liability for risk-taking). Benefits from participating in the transplant network; bears cost through transparency burden and risk-aversion incentives.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ELITE TRANSPLANT CENTER (ROPE) — Institutional actor with arbitrage options (reputation, selectivity). Experiences the constraint as pure coordination: risk-adjusted reporting enables centers to share methods and patient selection protocols. Can avoid high-risk cases and maintain favorable outcomes. Net beneficiary — the outcome metric system subsidizes their reputation.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY BODY (TANGLED ROPE) — Coordinating the entire transplant system while managing outcome transparency. Faces extraction from elite centers (who resist public reporting) and compression from powerless patients (who need access). Enforces the metric system actively but cannot fully resolve the coordination-extraction tension. Constrained by political pressure and industry lobbying.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CLINICAL OUTCOMES LITERATURE (PITON) — Risk-adjusted mortality reporting (UNOS Kaplan-Meier risk models) was originally designed to enable fair center comparison. But the literature has become theater: centers invest in gaming the metrics (patient selection, selective non-reporting) rather than improving actual outcomes. The original coordination function (benchmarking) has atrophied; the metric persists through institutional inertia and regulatory mandate despite degraded informational value.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, outcome gaming is an immutable feature of any measurement-based allocation system: when a metric becomes a target, it ceases to be a good metric (Goodhart's Law). The constraint appears as a law of organizational behavior, unchangeable through policy refinement. However, structural data reveals this as false naturalization — the gaming mechanism is contingent on specific incentive design (individual center accountability, public reporting, scarce organ allocation) and could be restructured.
constraint_indexing:constraint_classification(transplant_center_outcome_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transplant_center_outcome_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transplant_center_outcome_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transplant_center_outcome_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transplant_center_outcome_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transplant_center_outcome_gaming, TR),
    TR >= 0.70.

:- end_tests(transplant_center_outcome_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The metric system creates real incentives for patient selection and outcome gaming. Elite centers gain reputational and financial benefits during the measurement period (typically 3-year rolling windows). However, the extraction is not maximal (not 0.75+) because some genuine quality variation is real, some gaming is constrained by medical ethics, and the system does enable some centers to improve. The trajectory shows increasing gaming over time as centers refine their strategies. Suppression (0.65): High. Multiple barriers lock actors into the gaming dynamic: (1) high-risk patients have no alternative organ sources, (2) modest centers cannot exit the competitive metric system without losing reputation, (3) regulators cannot discontinue reporting without political backlash, (4) the clinical literature is locked into risk-stratification framing. Suppression has persisted even as awareness of gaming has increased. Theater ratio (0.68): High and increasing. Early UNOS reporting (1990s) focused on genuine outcome comparison. By 2010s-2020s, centers have become highly sophisticated at gaming: careful patient selection based on risk models, delays in reporting complications, selective non-transplantation of marginal organs, and aggressive statistical management of outcomes. The performative content has increased because centers invest more in gaming than in actual outcome improvement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a sharp perspectival gap between beneficiaries and victims. Elite centers experience the metric system as enabling (Rope): they use risk-adjustment to justify their selectivity and benchmark their practice. High-risk patients experience it as extractive (Snare): they are systematically excluded from consideration. Regulatory bodies experience mixed pressure (Tangled Rope): enforcing transparency while facing lobbying from elite centers. The modest-performing centers are squeezed between these poles. The theater is created by the gap itself: elite centers invest in reputation management (gaming) precisely because they can afford to; high-risk patients bear the cost because they cannot. The analytical observer risks naturalizing this as immutable organizational behavior, but the structure is contingent on metric visibility and allocation scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from each agent's structural relationship to the extraction flow. High-risk patients are pure targets (d=0.95): they bear costs and have no exit. Elite centers are beneficiaries (d=0.10): they gain from gaming and have arbitrage options (reputation, patient selectivity). Modest centers are mixed (d=0.55): constrained exit options (cannot opt out of competition), both benefits (network participation) and costs (liability for risk-taking). The regulatory body is constrained between extraction from above (lobbying by elite centers) and extraction from below (political pressure from patient advocates) — effectively a secondary victim despite institutional power. The analytical perspective risks seeing gaming as a law of organizational behavior (Goodhart's Law as mountain), but the structural data reveals it as a contingent feature of specific incentive design.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES VIA MECHANISM DECOMPOSITION: This constraint resolves mandatrophy by distinguishing three separable gaming mechanisms: (1) patient selection (legitimate quality assessment tool hijacked for exclusion), (2) outcome reporting delay (pure extraction with no coordination function), and (3) selective non-transplantation (rational response to perverse incentives). The coordination function (fair center comparison) is genuine and necessary. The extraction function (risk-averse selection amplifies disparity) is equally real. The tangled rope classification is correct because removing the metric system would harm the genuine coordination while removing the extraction — this is the defining tension of a hybrid constraint. The piton classification of the clinical literature reflects that the original purpose (identifying underperformers) has atrophied — literature now documents risk stratification as unavoidable fact rather than as problematic gaming.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_adjustment_validity,
    'Are current risk-adjustment models (UNOS Kaplan-Meier) sufficiently accurate to distinguish center quality from patient case-mix?',
    'External validation: independent re-analysis of center outcomes using alternative risk models and prospective prediction studies; assessment of model calibration across risk strata',
    'If models invalid: outcome metrics reflect patient selection, not center quality — gaming is rational response to bad measurement. If valid: gaming indicates centers are consciously distorting allocation for reputation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_adjustment_validity, empirical, 'Validity of risk-adjustment models for distinguishing center quality').

omega_variable(
    counterfactual_organ_allocation,
    'If outcome metrics were not reported or not visible to centers, would organ allocation patterns change and would actual patient outcomes improve?',
    'Randomized policy experiment or natural experiment (compare allocation patterns pre/post metric publication); track both gaming indicators (patient selection strictness) and actual outcomes (graft survival, patient mortality)',
    'If allocation improves: metrics are the extraction mechanism — removing them is structural solution. If allocation worsens: metrics enable necessary accountability despite gaming.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_organ_allocation, empirical, 'Counterfactual impact of outcome metrics on allocation and outcomes').

omega_variable(
    gaming_detection_completeness,
    'How much transplant center outcome gaming is currently undetected because it operates through patient exclusion (declining high-risk cases) rather than overt outcome manipulation?',
    'Audit of case-decline patterns: track rates at which centers refuse cases by patient risk profile; compare against clinical contraindication rates; identify systematic differences between centers in accepting marginal cases',
    'If high undetected gaming: suppression metric understates true constraint strength. If low: current detection mechanisms are adequate and the constraint is more transparent than perceived.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gaming_detection_completeness, empirical, 'Extent of undetected patient exclusion gaming').

omega_variable(
    interdependence_of_extraction_mechanisms,
    'Do the three gaming mechanisms (patient selection, case reporting delay, selective transplantation) operate independently or are they coupled such that preventing one escalates others?',
    'Analysis of center behavior under targeted interventions: (1) strict reporting requirements, (2) case review audits, (3) outcome transparency policies; measure whether centers shift gaming to non-targeted mechanisms',
    'If coupled (likely): suppressing one mechanism cascades to others — extraction is structurally persistent. If independent: targeted policy can reduce total gaming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interdependence_of_extraction_mechanisms, empirical, 'Coupling of outcome gaming mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transplant_center_outcome_gaming, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transplant_tr_t0, transplant_center_outcome_gaming, theater_ratio, 0, 0.48).
narrative_ontology:measurement(transplant_tr_t5, transplant_center_outcome_gaming, theater_ratio, 5, 0.62).
narrative_ontology:measurement(transplant_tr_t10, transplant_center_outcome_gaming, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(transplant_be_t0, transplant_center_outcome_gaming, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(transplant_be_t5, transplant_center_outcome_gaming, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(transplant_be_t10, transplant_center_outcome_gaming, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transplant_center_outcome_gaming, resource_allocation).
narrative_ontology:affects_constraint(transplant_center_outcome_gaming, organ_allocation_equity).
narrative_ontology:affects_constraint(transplant_center_outcome_gaming, transplant_center_selective_reporting).

% DUAL FORMULATION NOTE:
% Outcome gaming is upstream of two related constraints: equity in organ allocation (which experiences gaming as a distorting force) and selective outcome reporting (which represents one gaming mechanism in isolation). The network captures how the same base metric system cascades into multiple structural problems depending on the observational angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transplant_center_outcome_gaming, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
