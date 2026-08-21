% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'market efficiency' reading of the
 *   flexible employment legitimacy kernel. From this perspective, flexible
 *   employment is a legitimate and efficient mechanism for matching labor
 *   supply and demand, offering benefits to workers, consumers, and platform
 *   operators. The low extractiveness and suppression metrics reflect this
 *   reading's view of the arrangement as primarily coordinative and
 *   voluntary, with minimal coercive overhead. The claimed type 'rope' aligns
 *   with this interpretation, emphasizing mutual benefit and efficient
 *   resource allocation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.25).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.15).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '2c8d623d-fcea-4fe2-b677-f2f99dc4e29a').
narrative_ontology:cs_kernel_codification('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', implicit).
narrative_ontology:cs_authority_grounding('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', expertise).
narrative_ontology:cs_reading_relation('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', flexible_employment_legitimacy__precarity_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', flexible_employment_legitimacy__developmental_state_reading, forecloses).
narrative_ontology:cs_axiom('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', foundational, labor_market_clears_efficiently).
narrative_ontology:cs_axiom_status(labor_market_clears_efficiently, holdable).
narrative_ontology:cs_axiom_grounding('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', labor_market_clears_efficiently, empirically_contingent).
narrative_ontology:cs_axiom('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', foundational, worker_autonomy_is_maximized).
narrative_ontology:cs_axiom_status(worker_autonomy_is_maximized, holdable).
narrative_ontology:cs_axiom_grounding('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', worker_autonomy_is_maximized, deontological).
narrative_ontology:cs_reference_frame('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', neoclassical_labor_market_equilibrium).
narrative_ontology:cs_drift_state('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', contemporary_platform_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c8d623d-fcea-4fe2-b677-f2f99dc4e29a', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, operate, and profit from the digital platforms that facilitate flexible employment. They frame flexible work as an efficient solution to market friction and a source of worker autonomy.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Seek flexible work arrangements for supplemental income, autonomy, or to fit around other commitments. They value the low barriers to entry and the ability to set their own hours, seeing it as a net benefit.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, beneficiary,
    moderate, biographical, mobile, local).

% Benefit from the convenience, speed, and often lower cost of services provided by flexible workers. They perceive the system as efficient and responsive to their needs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers, beneficiary,
    organized, immediate, mobile, local).

% Face competition for labor from flexible work platforms, which can drive up wages for certain skills or reduce the available pool for traditional roles. They must adapt their hiring practices or risk losing talent.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_employers, payer,
    powerful, biographical, constrained, national).

% Advocate for traditional employment protections, collective bargaining, and benefits, which are often absent in flexible work models. From this reading's perspective, their concerns are seen as hindering market efficiency and worker choice.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Raise concerns about the lack of social safety nets, health benefits, and retirement savings for flexible workers. This reading views these concerns as external to the efficient functioning of the labor market.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, social_welfare_advocates, excluded,
    moderate, generational, constrained, national).

% Study the dynamics of labor markets, including the impact of flexible employment on wages, employment rates, and economic efficiency. From this seat, the constraint is analyzed through models of supply and demand.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Efficiently matches a diverse and flexible labor supply with fluctuating consumer and business demand, reducing search costs and underemployment.
% TRANSFER_FUNCTION: Facilitates the exchange of labor services for wages, moving income to workers and providing convenient services to consumers, while generating profits for platform operators.
% ABSENT_VOICES: Labor unions and social welfare advocates are largely excluded from the design and legitimization of flexible employment models, as their focus on traditional protections is seen as incompatible with market efficiency and worker autonomy.
% DISAPPEARANCE_RATIONALE: If flexible employment models vanished overnight, a significant portion of the labor market would lose its primary clearing mechanism, leading to increased unemployment for those seeking flexible work, higher costs and reduced convenience for consumers, and a substantial disruption to the platform economy.
% FOUNDING_PROBLEM: The traditional labor market suffered from rigidities, high transaction costs for short-term tasks, and an inability to efficiently match diverse worker availability with fluctuating demand, leading to underemployment and unmet consumer needs.
% FOUNDING_PROBLEM_CORROBORATION: Economic models, platform company reports, and surveys of flexible workers consistently highlight the ongoing need for efficient labor matching and the benefits of flexible work arrangements. Independent labor economists often corroborate the market-clearing function, even while acknowledging other concerns.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects the belief that flexible employment primarily creates value through efficient matching, with any 'cost' being a fair market price for coordination. Suppression (0.15) is low because participation is framed as voluntary, driven by worker autonomy and choice. The theater ratio (0.10) is minimal, as the market-clearing function is seen as genuinely effective and not performative. Accessibility collapse is moderate (0.40) because it opens new avenues for work, even if it shifts the landscape for traditional employment. Resistance (0.10) is low from this perspective, as the system is viewed as largely beneficial.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the benefits of flexibility and efficiency, leading to a 'rope' classification. Other readings (e.g., 'precarity_extraction_reading') would highlight the asymmetric power dynamics and lack of worker protections, leading to a much higher extractiveness and a 'snare' classification. The engine's computation of per-seat types would reflect these structural differences, even if the claimed type here is 'rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are agenda-setters and beneficiaries, profiting from the efficient matching. Flexible workers are beneficiaries due to increased autonomy and income opportunities. Consumers benefit from convenient services. Traditional employers are payers, facing competitive pressures. Labor unions and social welfare advocates are excluded, as their perspectives on worker protections are seen as external to this reading's focus on market efficiency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_vs_power_asymmetry,
    'Does the observed ''efficiency'' of flexible employment genuinely reflect optimal market clearing, or is it a consequence of power asymmetries that allow platforms to dictate terms?',
    'Empirical studies on wage setting power, elasticity of labor supply in platform contexts, and the impact of collective bargaining on platform worker earnings.',
    'If power asymmetry is the primary driver, the true extractiveness is higher, and the constraint leans towards a ''tangled_rope'' or ''snare'' classification, challenging the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_vs_power_asymmetry, empirical, 'Distinguishing market efficiency from power-driven outcomes in flexible employment.').

omega_variable(
    worker_autonomy_vs_precarity,
    'Is the ''autonomy'' offered by flexible employment a genuine benefit, or does it mask underlying precarity and lack of control over essential work conditions?',
    'Qualitative research on worker experiences, analysis of income stability, access to benefits, and the ability of workers to genuinely refuse tasks without penalty.',
    'If precarity is dominant, the suppression metric is higher, and the ''beneficiary'' role for flexible workers is challenged, shifting their directionality towards ''target''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_autonomy_vs_precarity, conceptual, 'The true nature of worker ''autonomy'' in flexible employment.').

omega_variable(
    legitimacy_of_non_standard_employment,
    'Is flexible employment, by its nature, a legitimate and sustainable form of work, or does its deviation from traditional employment standards inherently create social costs that should be addressed?',
    'Policy debates, legal rulings on worker classification, and societal consensus on the responsibilities of employers and platforms in the future of work.',
    'If deemed unsustainable without intervention, the ''rope'' classification is challenged, potentially shifting towards a ''scaffold'' (requiring transitional support) or ''tangled_rope'' (if extraction is identified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_non_standard_employment, preference, 'Societal acceptance of flexible employment as a long-term work model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2008, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(flex_tr_t2013, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2013, 0.09).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(flex_tr_t2023, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(flex_be_t2008, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(flex_be_t2013, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2013, 0.22).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2018, 0.24).
narrative_ontology:measurement(flex_be_t2023, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2008, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(flex_su_t2013, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2013, 0.12).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2018, 0.14).
narrative_ontology:measurement(flex_su_t2023, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'flexible_employment_legitimacy' kernel. This 'market_efficiency_reading' emphasizes the coordinative and beneficial aspects, while sibling readings focus on precarity and the need for state management.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
