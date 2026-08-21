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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Flexible Employment as Precarity-Driven Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story analyzes 'flexible employment' within the platform
 *   economy from the 'precarity extraction' reading. It argues that the
 *   structural flexibility offered to workers primarily serves to enable
 *   platforms to extract surplus value by externalizing labor costs and risks
 *   onto individual workers and public social safety nets. The constraint is
 *   framed as a Snare, where the coordination narrative (matching supply and
 *   demand) serves as a cover for systematic extraction maintained by
 *   algorithmic control and legal classifications that suppress worker power.
 *
 * KEY AGENTS:
 *   - platform_operators: Primary beneficiary and agenda-setter (institutional/arbitrage)
 *   - gig_workers: Primary target and victim (powerless/identity_locked)
 *   - social_security_systems: Secondary target/victim (institutional/constrained)
 *   - gig_economy_investors: Secondary beneficiary (powerful/arbitrage)
 *   - labor_unions: Excluded voice (organized/constrained)
 *   - regulators_and_policymakers: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.85).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.75).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Precarity-Driven Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '8d4a464a-7758-4fe0-98ec-ec777ee9185d').
narrative_ontology:cs_kernel_codification('8d4a464a-7758-4fe0-98ec-ec777ee9185d', distributed).
narrative_ontology:cs_authority_grounding('8d4a464a-7758-4fe0-98ec-ec777ee9185d', extraction).
narrative_ontology:cs_interpretation_layer_present('8d4a464a-7758-4fe0-98ec-ec777ee9185d').
narrative_ontology:cs_reading_relation('8d4a464a-7758-4fe0-98ec-ec777ee9185d', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d4a464a-7758-4fe0-98ec-ec777ee9185d', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('8d4a464a-7758-4fe0-98ec-ec777ee9185d', foundational, labor_is_not_a_commodity).
narrative_ontology:cs_axiom_status(labor_is_not_a_commodity, holdable).
narrative_ontology:cs_axiom_grounding('8d4a464a-7758-4fe0-98ec-ec777ee9185d', labor_is_not_a_commodity, deontological).
narrative_ontology:cs_axiom('8d4a464a-7758-4fe0-98ec-ec777ee9185d', foundational, platform_design_shapes_labor_outcomes).
narrative_ontology:cs_axiom_status(platform_design_shapes_labor_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('8d4a464a-7758-4fe0-98ec-ec777ee9185d', platform_design_shapes_labor_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('8d4a464a-7758-4fe0-98ec-ec777ee9185d', labor_as_social_relation).
narrative_ontology:cs_drift_state('8d4a464a-7758-4fe0-98ec-ec777ee9185d', contemporary_platform_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8d4a464a-7758-4fe0-98ec-ec777ee9185d', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_investors).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and control the algorithms that allocate tasks, set pay rates, and manage gig workers. They benefit from low labor costs, minimal overhead for benefits, and the ability to scale labor supply on demand. They actively lobby against reclassification of workers as employees.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of precarity: unstable income, lack of benefits (health insurance, paid leave, retirement), and no collective bargaining power. They are often dependent on platform income due to limited alternative employment options or the need for flexible hours, creating an identity-lock to the platform's terms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, identity_locked, local).

% Experience reduced tax contributions from gig work, leading to funding shortfalls. They also face increased demand for social safety nets as precarious workers struggle with income volatility and lack of employer-provided benefits. Their ability to adapt is constrained by legislative processes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems).

% Profit from the high margins enabled by the flexible employment model, which minimizes labor costs and regulatory burdens. They exert significant influence on platform operators to maintain and expand this model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_economy_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Are largely excluded from organizing gig workers due to their classification as independent contractors, which limits their ability to negotiate for better wages and conditions. They advocate for reclassification and stronger labor protections.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Observe the evolving landscape of gig work, balancing innovation with worker protection. They are under pressure from both platform lobbies and labor advocates to define the legal status of gig workers and the responsibilities of platforms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, regulators_and_policymakers, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connects individuals seeking flexible work with consumers needing on-demand services, efficiently matching supply and demand through digital platforms.
% TRANSFER_FUNCTION: Transfers a significant portion of the value generated by gig workers to platform operators and investors, while externalizing risks and costs (e.g., benefits, social security contributions) onto workers and public systems.
% ABSENT_VOICES: Gig workers, often isolated and lacking collective representation, are largely absent from the policy discussions that define their employment terms. Labor unions, representing traditional workers, are actively excluded from organizing gig workers by current legal classifications.
% DISAPPEARANCE_RATIONALE: If the 'flexible employment' classification (as a mechanism for precarity-driven extraction) vanished overnight, platforms would be forced to reclassify workers as employees, incurring significant labor costs and regulatory burdens. This would fundamentally alter their business model, likely leading to higher service prices, reduced investor returns, and a restructuring of the gig economy, with many platforms unable to sustain current operations.
% FOUNDING_PROBLEM: The founding problem was to create a highly flexible, scalable labor force for on-demand services, bypassing traditional employment rigidities and costs.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and investors attest that the need for flexible labor and low operating costs remains central to their business model. Labor advocates and social policy experts corroborate that the problem of minimizing labor costs is indeed 'live' for platforms, but argue that this 'problem' is itself a driver of social precarity, not a solution to a genuine market failure.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) due to the significant portion of value transferred from workers to platforms, coupled with the externalization of costs. Suppression (0.75) is high because algorithmic management, lack of collective bargaining, and legal classifications effectively prevent workers from challenging terms or seeking alternatives. Theater ratio (0.20) is relatively low, as the 'flexibility' narrative is a functional part of the extraction mechanism, not merely a performance; the coordination function is genuine but heavily skewed. Accessibility collapse (0.60) reflects that while some alternatives exist, the immediate need for income often traps workers in the gig economy. Resistance (0.70) is substantial, evidenced by ongoing legal challenges and worker organizing efforts, but often fragmented.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators and investors perceive 'flexible employment' as an efficient market mechanism (a Rope or even a Mountain of economic law), solving a genuine coordination problem. Gig workers and social policy advocates, however, experience it as a Snare, where the 'flexibility' is a euphemism for precarity and a mechanism for extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators and investors are clear beneficiaries (d near 0.0) as they capture the economic gains and control the terms. Gig workers are direct targets (d near 1.0) as they bear the costs of precarity and lack of benefits. Social security systems are also targets, as they face funding shortfalls and increased burdens. Labor unions are excluded, their efforts to represent workers suppressed by the current legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a structurally extractive arrangement as mere coordination. The 'flexibility' mandate, while initially addressing a genuine need for on-demand services, has atrophied into a justification for systematic cost externalization and worker precarity. The high extractiveness and suppression, coupled with the identified victims, clearly point to a Snare, not a Rope, despite the claimed coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_flexibility,
    'What is the full social cost of ''flexible employment'' when accounting for externalized risks (healthcare, retirement, unemployment) and reduced social security contributions?',
    'Comprehensive national-level economic modeling that integrates platform revenue, worker income, and public expenditure on social safety nets, comparing against traditional employment models.',
    'If the full social cost significantly outweighs the economic benefits claimed by platforms, it would strengthen the argument for reclassification of workers and/or mandatory platform contributions to social security, potentially reclassifying the constraint as a more explicit Snare or even a Tangled Rope if a coordination function is acknowledged but extraction is asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_flexibility, empirical, 'Quantifying the externalized costs of gig work.').

omega_variable(
    algorithmic_control_vs_autonomy,
    'To what extent does algorithmic management genuinely offer worker autonomy versus imposing a new form of control that mimics independent contractor status?',
    'Empirical studies on worker experience, task allocation patterns, and platform penalties for non-compliance, compared against self-employment benchmarks. Legal analysis of algorithmic terms of service.',
    'If algorithmic control is found to be highly prescriptive and punitive, it would undermine the ''independent contractor'' justification, increasing the measured suppression and extractiveness, and reinforcing the Snare classification. If genuine autonomy is demonstrated, it might shift towards a more complex Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_vs_autonomy, empirical, 'Assessing the reality of worker autonomy under algorithmic management.').

omega_variable(
    naturalness_of_precarity,
    'Is the precarity inherent in ''flexible employment'' an unavoidable feature of modern labor markets, or a constructed outcome of specific policy and platform design choices?',
    'Comparative analysis with countries that have implemented stronger protections for gig workers or alternative models of platform governance (e.g., worker cooperatives, ''platform-as-utility'' regulations).',
    'If precarity is shown to be a constructed outcome, it would further delegitimize the ''natural market efficiency'' argument, solidifying the Snare classification and highlighting the role of active enforcement in maintaining the extractive structure. If it''s genuinely unavoidable, it might suggest a Mountain-like aspect to the underlying economic conditions, though the extractive mechanism would still be a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_precarity, conceptual, 'Distinguishing inherent market forces from constructed precarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, labor_law_classification_framework).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
