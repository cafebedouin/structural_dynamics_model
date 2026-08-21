% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Platform Precarity and Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'precarity_extraction_reading' of
 *   the 'flexible_employment_legitimacy' kernel. It describes how the concept
 *   and practice of 'flexible employment' in the platform economy function as
 *   a structural mechanism to enable the extraction of surplus value from gig
 *   workers, primarily through externalizing risks and costs. The narrative
 *   of 'flexibility' serves as a cover for a system of algorithmic control
 *   and economic precarity, leading to high extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.85).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Platform Precarity and Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '2eb0143e-c930-445a-91b1-2c69141f74fd').
narrative_ontology:cs_kernel_codification('2eb0143e-c930-445a-91b1-2c69141f74fd', formalized).
narrative_ontology:cs_authority_grounding('2eb0143e-c930-445a-91b1-2c69141f74fd', extraction).
narrative_ontology:cs_interpretation_layer_present('2eb0143e-c930-445a-91b1-2c69141f74fd').
narrative_ontology:cs_reading_relation('2eb0143e-c930-445a-91b1-2c69141f74fd', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eb0143e-c930-445a-91b1-2c69141f74fd', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('2eb0143e-c930-445a-91b1-2c69141f74fd', foundational, labor_is_a_cost_to_be_minimized).
narrative_ontology:cs_axiom_status(labor_is_a_cost_to_be_minimized, holdable).
narrative_ontology:cs_axiom_grounding('2eb0143e-c930-445a-91b1-2c69141f74fd', labor_is_a_cost_to_be_minimized, empirically_contingent).
narrative_ontology:cs_axiom('2eb0143e-c930-445a-91b1-2c69141f74fd', foundational, risk_externalization_is_profit).
narrative_ontology:cs_axiom_status(risk_externalization_is_profit, holdable).
narrative_ontology:cs_axiom_grounding('2eb0143e-c930-445a-91b1-2c69141f74fd', risk_externalization_is_profit, empirically_contingent).
narrative_ontology:cs_reference_frame('2eb0143e-c930-445a-91b1-2c69141f74fd', neoliberal_labor_market).
narrative_ontology:cs_drift_state('2eb0143e-c930-445a-91b1-2c69141f74fd', contemporary_platform_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2eb0143e-c930-445a-91b1-2c69141f74fd', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, traditional_labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the terms of 'flexible' employment, including payment structures, algorithmic management, and dispute resolution. They benefit from low labor costs, externalized risks, and minimal social security contributions. They actively lobby against reclassification of workers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Engage in 'flexible' work out of economic necessity, often facing unpredictable income, lack of benefits, and no collective bargaining power. They bear the costs of precarity, including health risks, retirement insecurity, and the burden of self-employment taxes. Exit is constrained by limited alternative employment options.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, trapped, local).

% Benefit from convenient, on-demand services at competitive prices, enabled by the low labor costs and externalized risks borne by gig workers. They are largely unaware of or detached from the underlying labor conditions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_consumers, beneficiary,
    moderate, immediate, mobile, local).

% Are largely excluded from organizing gig workers due to their classification as independent contractors, which undermines traditional labor protections and collective bargaining. They advocate for reclassification and stronger worker rights but face significant legal and political barriers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_labor_unions, excluded,
    organized, generational, constrained, national).

% Investigate and litigate the classification of gig workers, seeking to enforce existing labor laws or propose new ones. They face strong lobbying from platform operators and legal challenges, leading to slow and often fragmented policy responses.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, biographical, analytical, national).

% Research and highlight the social costs of flexible employment, advocating for stronger social safety nets, universal benefits, and policies that address worker precarity. They work to shift public and political discourse.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_policy_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches a flexible labor supply to fluctuating consumer demand for on-demand services, facilitating efficient resource allocation for platforms.
% TRANSFER_FUNCTION: Transfers surplus value (uncompensated labor, externalized risks, lack of benefits) from gig workers to platform operators (as profit) and to consumers (as lower prices/convenience).
% ABSENT_VOICES: Gig workers, if empowered to collectively bargain, would demand fair wages, benefits, and protections. Traditional labor unions, if allowed to organize, would challenge the independent contractor classification. Both are actively suppressed by legal frameworks and platform policies.
% DISAPPEARANCE_RATIONALE: If the current 'flexible employment' model (as a mechanism for precarity and extraction) vanished overnight, the platform economy would undergo a fundamental restructuring. Platforms would face significantly higher labor costs, potentially leading to higher consumer prices or reduced service availability. Workers would gain protections and benefits, fundamentally altering labor market dynamics and social welfare systems.
% FOUNDING_PROBLEM: The need for highly flexible, on-demand labor to meet unpredictable consumer demand, particularly in sectors like transportation, delivery, and personal services, while minimizing overhead costs for businesses.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and some economists argue the problem is live, emphasizing the efficiency and innovation enabled by flexibility. Labor economists, social policy researchers, and worker advocacy groups contend that the 'problem' has been reframed to justify extraction, and the original need for flexibility could be met with fair labor practices. Independent studies on worker conditions and platform profitability corroborate the latter view.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant transfer of value from workers to platforms and consumers, manifested in low wages, lack of benefits, and externalized operational costs. Suppression (0.78) is high due to the legal classification of workers as independent contractors, algorithmic management that limits worker agency, and active lobbying against labor reforms. The theater ratio (0.45) indicates that while some genuine flexibility exists, a substantial portion of the 'flexibility' narrative is performative, masking the underlying control and extraction. Accessibility collapse (0.70) is high because many gig workers have limited alternative employment options, making exit difficult. Resistance (0.60) is moderate, reflecting ongoing but often fragmented efforts by workers and unions to challenge the model.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators perceive this as a legitimate, efficient market mechanism (a Rope or even a Mountain of economic law). Gig workers and labor advocates experience it as a Snare, designed to extract value and suppress collective action. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are clear beneficiaries (d=0.0-0.1) as they design and profit from the system. Gig economy consumers are indirect beneficiaries (d=0.2-0.3) through lower prices and convenience. Gig workers are the primary targets (d=0.9-1.0), bearing the brunt of precarity and extraction. Traditional labor unions are excluded (d=1.0) as their very existence challenges the constraint's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing flexible work and efficient matching) is contested. While some matching function remains, the 'precarity_extraction_reading' argues that the primary function has drifted to rent-seeking. The high extractiveness and suppression, coupled with the contested founding problem status, prevent mislabeling this as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_vs_power_asymmetry,
    'Is ''flexible employment'' primarily a response to genuine market demand for flexibility, or is it a consequence of platform operators'' asymmetric market power and control over labor supply?',
    'Empirical studies comparing labor outcomes in genuinely competitive flexible markets versus platform-dominated markets, and analysis of platform business models'' reliance on externalized costs.',
    'If primarily market-driven, the constraint might lean towards a Tangled Rope (coordination with some extraction). If driven by power asymmetry, it strongly confirms the Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_efficiency_vs_power_asymmetry, empirical, 'Distinguishing market efficiency from power-driven extraction in flexible employment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gig workers structural (legal classification, algorithmic control) or internalized (economic desperation, lack of awareness of rights)?',
    'Post-policy intervention analysis: if suppression persists after legal reclassification or algorithmic transparency, reclassify as partially internalized. Worker surveys on perceived agency and alternatives.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as workers carry the suppression with them even if external barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gig workers.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''flexible_employment_legitimacy'' kernel best understood as an economic mechanism, a social policy challenge, or a power struggle over labor relations?',
    'Analysis of legislative debates, court rulings, and academic discourse across different disciplines. The choice of framing significantly alters the perceived legitimacy and policy solutions.',
    'If framed primarily as an economic mechanism (market_efficiency_reading), the constraint might appear more benign. If framed as a power struggle (precarity_extraction_reading), its extractive nature is foregrounded, leading to a Snare classification. If framed as a social policy challenge (developmental_state_reading), it might be seen as a Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Conceptual framing of flexible employment as economic, social, or power-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(flex_tr_t30, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.85).
narrative_ontology:measurement(flex_be_t30, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(flex_su_t30, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
