% ============================================================================
% CONSTRAINT STORY: aging_well_assessment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aging_well_assessment, []).

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
 *   constraint_id: aging_well_assessment
 *   human_readable: The Commercialized Functional Aging Assessment System
 *   domain: health/economic
 *
 * SUMMARY:
 *   The Commercialized Functional Aging Assessment System transforms
 *   research-derived physical proxy tests (Sitting-Rising Test, 4-Meter
 *   Walking Speed, Grip Strength) into a standardized clinical tool for risk
 *   stratification, intervention prescription, and billing justification.
 *   Originally developed as predictive biomarkers in prospective aging
 *   cohorts, these tests have been packaged by assessment vendors into
 *   proprietary platforms, marketed to insurance companies as
 *   cost-containment mechanisms, and embedded into clinical care through
 *   reimbursement mandates. The constraint exhibits a complex tangled
 *   structure: genuine coordination value (standardized risk assessment,
 *   early intervention opportunity) mixed with extraction mechanisms (cost
 *   concentration on low-income elderly, vendor control over metrics,
 *   clinical theater replacing genuine decision-making). The system requires
 *   active enforcement (insurance mandates, physician referrals) and
 *   suppression (knowledge asymmetry, limited alternatives, cultural barriers
 *   to refusing assessment). The theater ratio has risen as the system
 *   matured — the clinical value of the assessment appears to have plateaued
 *   while the administrative and billing overhead has increased, suggesting
 *   institutional degradation toward piton status.
 *
 * KEY AGENTS:
 *   - Assessment Vendors (Institutional/Arbitrage): Primary beneficiary — owns protocol, licenses tests, sells bundled platforms; benefits from switching costs and market consolidation
 *   - Low-Income Elderly (Powerless/Trapped): Primary victim — bears assessment costs and copays; cannot refuse without losing insurance coverage; no agency in system design
 *   - Insurance Risk Managers (Organized/Constrained): Secondary beneficiary — uses assessment for risk stratification and premium differentiation; constrained by regulation and liability
 *   - Research Aging Scientists (Moderate/Constrained): Beneficiary and victim — validates metrics (funding benefit) but loses commons (proprietary data, restricted access)
 *   - Gerontology Profession (Institutional/Arbitrage): Theater-maintainer — uses assessments as billing justification and clinical ritual; sees own processes as degraded
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing commercial extraction as biological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aging_well_assessment, 0.52).
domain_priors:suppression_score(aging_well_assessment, 0.58).
domain_priors:theater_ratio(aging_well_assessment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aging_well_assessment, extractiveness, 0.52).
narrative_ontology:constraint_metric(aging_well_assessment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(aging_well_assessment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aging_well_assessment, tangled_rope).
narrative_ontology:human_readable(aging_well_assessment, "The Commercialized Functional Aging Assessment System").
narrative_ontology:topic_domain(aging_well_assessment, "health/economic").

domain_priors:requires_active_enforcement(aging_well_assessment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aging_well_assessment, assessment_vendors).
narrative_ontology:constraint_beneficiary(aging_well_assessment, intervention_providers).
narrative_ontology:constraint_beneficiary(aging_well_assessment, insurance_risk_managers).
narrative_ontology:constraint_victim(aging_well_assessment, elderly_low_income_populations).
narrative_ontology:constraint_victim(aging_well_assessment, aging_research_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME ELDERLY (SNARE) — Trapped in the assessment ecosystem by insurance mandates and physician referrals. Cannot refuse testing without loss of coverage or care access. Costs (repeated assessments, intervention copays, time burden) fall entirely on the vulnerable population. No exit option; maximum experienced extraction relative to coordination benefit received.
constraint_indexing:constraint_classification(aging_well_assessment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESEARCH AGING SCIENTIST (TANGLED ROPE) — Benefits from the commercialized validation of their metrics (increased adoption, funding) but constrained by loss of scientific commons (proprietary data, restricted access, publication bias toward intervention efficacy). Mixed coordination (shared standards) and extraction (standardized metrics now controlled by vendors). Moderate experienced extraction — has some agency through academic credentials but constrained by institutional dependencies.
constraint_indexing:constraint_classification(aging_well_assessment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ASSESSMENT VENDOR (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: owning the assessment protocol creates switching costs and standardization value. Can arbitrage the system (licensing tests, selling platforms, bundling with interventions). Net extraction flows toward this agent. No cost-bearing from the vendor's perspective — the system solves their coordination problem of capturing market share.
constraint_indexing:constraint_classification(aging_well_assessment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE RISK MANAGER (TANGLED_ROPE) — Organized institutional actor. Benefits from risk stratification (identifies high-cost elderly before intervention) but constrained by regulatory requirements, data privacy law, and actuarial liability. The assessment system provides coordination (standardized risk profiles) and extraction (premium differentiation, intervention mandates). Significant enforcement overhead required to sustain the system. Moderate extraction — organized enough to adapt, but institutionally constrained.
constraint_indexing:constraint_classification(aging_well_assessment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GERONTOLOGY PROFESSION (PITON) — Maintains ritualistic assessment protocols that were developed for research prediction but now serve primarily as billing justification and clinical theater. Physicians use the tests because insurance requires them and vendors market them, not because they substantially change treatment. The profession sees its own processes as partially degraded — vestiges of evidence-based protocols maintained through institutional inertia and reimbursement incentives rather than functional necessity. Theater ratio high; actual clinical decision-making impact low.
constraint_indexing:constraint_classification(aging_well_assessment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a biological universalist view, functional decline with aging is an immutable property of human biology. The constraint appears as a natural fact: assessments measure real biological processes (muscle loss, gait degradation, grip decline), and testing these measures is inherent to gerontological science. However, the structural data — that the assessment is commercialized, requires active enforcement, extracts from vulnerable populations, and maintains theater — reveals this perspective as a naturalization of contingent institutional arrangements, not a mountain.
constraint_indexing:constraint_classification(aging_well_assessment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aging_well_assessment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aging_well_assessment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aging_well_assessment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aging_well_assessment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(aging_well_assessment, TR),
    TR >= 0.70.

:- end_tests(aging_well_assessment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The vendors capture significant economic value through licensing, platform fees, and bundled interventions. Low-income populations bear assessment costs as copays and time burden without proportional benefit. The extraction is substantial but not maximal because some populations do benefit from early intervention, and the assessment provides genuine information value. The rise from 0.28 to 0.52 over the interval reflects increasing vendor control and commercialization deepening. Suppression (0.58): Moderate-high. Strong barriers include insurance reimbursement mandates (active enforcement), knowledge asymmetry (elderly populations cannot assess test validity), limited alternative pathways (proprietary metrics block open-source competition), and cultural/medical authority factors. But suppression is not total — some elderly populations can refuse, some providers challenge the system, and resistance is emerging. Theater ratio (0.64): High and rising. Initially (t=0), the tests had research validity and clinicians used them for genuine prediction. Over time, the clinical decision-making value plateaued while administrative burden increased. Physicians now order tests primarily because insurance requires them and because billing depends on test documentation, not because the results substantially change care decisions. The rise from 0.35 to 0.64 indicates Goodhart drift — the metric has become decoupled from the outcome it was supposed to predict.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism is experienced as coordination, extraction, theater, or natural law depending on position. The vendor sees Rope — the system solves their standardization and licensing problem. The low-income elderly see Snare — mandatory, costly, inescapable. The research scientist sees Tangled Rope — benefits (validation, adoption) mixed with costs (loss of commons, data control). The insurance manager sees Tangled Rope — risk stratification benefit mixed with regulatory constraint. The gerontologist sees Piton — the ritual persists through inertia and billing, not functional necessity. The analytical observer risks seeing Mountain — aging is biological fact — but the structural data reveals this as naturalization: the extraction is institutional, not biological. The perspectival gap is the entire story.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Assessment vendors (institutional, arbitrage exit) experience low d (~0.10) — they are primary beneficiaries with escape options; the system solves their market problem. Low-income elderly (powerless, trapped exit) experience high d (~0.90) — they are trapped targets with no exit; maximum experienced extraction. Research scientists (moderate, constrained exit) experience moderate d (~0.55) — they benefit from validation but constrained by intellectual property control; mixed experience. Insurance managers (organized, constrained exit) experience low-moderate d (~0.35) — institutional enough to adapt but constrained by regulation; moderate experienced extraction. The pipeline computes f(d) from these d values and applies scope modifiers: low-income elderly in a national scope experience χ ≈ 0.90 × 1.0 ≈ 0.90; vendors in global scope with low d experience χ ≈ 0.52 × (-0.12) × 1.2 ≈ negligible (beneficiary discount).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy (false coordinate/extraction distinction) by clearly separating genuine coordination function (standardized risk assessment, early detection) from extraction mechanism (vendor licensing, cost concentration, knowledge asymmetry). The tangled rope classification affirms both: the system does coordinate prediction (genuine coordination value) AND extracts through reimbursement mandates, proprietary metrics, and suppression of alternatives (genuine extraction). The theater ratio rising above 0.5 indicates that the coordination function is degrading while the extraction mechanism persists — the system is drifting toward Snare (extraction) and Piton (theater-without-function). The mandatrophy is resolved by recognizing that commercialization was not inevitable: the original research tests were pure Rope (coordination through shared standards); the commercialization layer is a Snare added on top. Decomposing into separate stories would split these, but they are causally linked (vendor control of coordination enables extraction), so a single Tangled Rope story captures the hybrid structure correctly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_validity_threshold,
    'What level of predictive validity justifies mandatory assessment frequency and intervention mandates for low-income populations?',
    'Prospective cohort analysis comparing assessment-guided interventions vs non-assessed controls; cost-effectiveness analysis relative to invasiveness and population burden',
    'If validity low (r² < 0.30): assessment is extraction without coordination benefit. If validity moderate (0.30 < r² < 0.50): tangled rope classification confirmed. If validity high (r² > 0.70): classification shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_validity_threshold, empirical, 'Predictive validity of commercialized functional tests for actual health outcomes').

omega_variable(
    intervention_efficacy_independent,
    'Are intervention outcomes driven by the assessment itself (early detection, adherence motivation) or by vendor marketing and selective targeting of populations likely to improve anyway?',
    'Randomized controlled trial comparing assessment-identified vs clinician-identified vs unselected controls for intervention effectiveness; analysis of outcome drift post-commercialization',
    'If assessment is causal: tangled rope with genuine coordination function confirmed. If outcomes driven by vendor selection bias or publication bias: assessment is theater, classification shifts toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_efficacy_independent, empirical, 'Whether assessment drives intervention efficacy or reflects selection bias').

omega_variable(
    equity_enforcement_mechanism,
    'Is suppression maintained by billing/insurance mandate (active enforcement) or by knowledge barriers and cultural factors (passive suppression)?',
    'Policy analysis of reimbursement requirements; qualitative interviews with low-income elderly populations; comparison of assessment adoption in systems with vs without insurance mandates',
    'If active enforcement dominates: suppression is designed, extraction is intentional, classification remains tangled rope/snare. If passive suppression dominates: constraint may degrade toward piton as enforcement fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_enforcement_mechanism, empirical, 'Whether suppression is actively enforced or passively sustained').

omega_variable(
    open_data_commons_viability,
    'Could a non-proprietary, open-access aging assessment framework provide equivalent predictive validity without vendor lock-in and cost extraction?',
    'Comparative analysis of open-source gerontology metrics (public datasets, published protocols) vs proprietary vendor platforms; international comparisons (countries with public assessment systems)',
    'If viable: the commercialization is contingent rent-seeking, not inevitable. Constraint could decompose into a Mountain (biological invariance of aging) + Snare (vendor extraction) + Rope (research coordination). If not viable: vendor control reflects coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_data_commons_viability, empirical, 'Feasibility of open-source aging assessment alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aging_well_assessment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aging_tr_t0, aging_well_assessment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aging_tr_t5, aging_well_assessment, theater_ratio, 5, 0.5).
narrative_ontology:measurement(aging_tr_t10, aging_well_assessment, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(aging_be_t0, aging_well_assessment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(aging_be_t5, aging_well_assessment, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(aging_be_t10, aging_well_assessment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aging_well_assessment, information_standard).
narrative_ontology:affects_constraint(aging_well_assessment, biomarker_discovery_capture).
narrative_ontology:affects_constraint(aging_well_assessment, geriatric_clinical_guideline_ossification).

% DUAL FORMULATION NOTE:
% The commercialized assessment system is downstream of legitimate research advances in aging biomarkers but represents a distinct structural constraint. The upstream constraint (biomarker discovery capture by pharma) has different ε reflecting the research/development boundary; this constraint has ε=0.52 reflecting the clinical/commercial implementation layer and its extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aging_well_assessment, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
