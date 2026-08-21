% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_floor_reading' of the
 *   'income_support_conditionality' kernel. It describes unconditional income
 *   support as a mechanism that decommodifies labor power, thereby creating
 *   positive freedom for individuals to refuse coercive or exploitative work.
 *   From this perspective, the constraint (the policy of unconditional income
 *   support) functions as a 'rope' by coordinating a societal floor of
 *   economic security, which in turn reduces the extractive and suppressive
 *   forces of a precarious labor market. The metrics reflect this reduction
 *   in extraction and suppression over time as the policy takes effect.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'fd8cbc47-63b9-416b-8183-a8371587b3aa').
narrative_ontology:cs_kernel_codification('fd8cbc47-63b9-416b-8183-a8371587b3aa', formalized).
narrative_ontology:cs_authority_grounding('fd8cbc47-63b9-416b-8183-a8371587b3aa', practice).
narrative_ontology:cs_interpretation_layer_present('fd8cbc47-63b9-416b-8183-a8371587b3aa').
narrative_ontology:cs_reading_relation('fd8cbc47-63b9-416b-8183-a8371587b3aa', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd8cbc47-63b9-416b-8183-a8371587b3aa', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('fd8cbc47-63b9-416b-8183-a8371587b3aa', foundational, decommodification_enhances_freedom).
narrative_ontology:cs_axiom_status(decommodification_enhances_freedom, holdable).
narrative_ontology:cs_axiom_grounding('fd8cbc47-63b9-416b-8183-a8371587b3aa', decommodification_enhances_freedom, deontological).
narrative_ontology:cs_axiom('fd8cbc47-63b9-416b-8183-a8371587b3aa', secondary, economic_security_is_a_human_right).
narrative_ontology:cs_axiom_status(economic_security_is_a_human_right, holdable).
narrative_ontology:cs_axiom_grounding('fd8cbc47-63b9-416b-8183-a8371587b3aa', economic_security_is_a_human_right, deontological).
narrative_ontology:cs_reference_frame('fd8cbc47-63b9-416b-8183-a8371587b3aa', post_scarcity_labor_autonomy).
narrative_ontology:cs_drift_state('fd8cbc47-63b9-416b-8183-a8371587b3aa', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fd8cbc47-63b9-416b-8183-a8371587b3aa', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unemployed_individuals).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, advocacy_groups_for_basic_income).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_reliant_on_cheap_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain the economic security to refuse exploitative or undesirable work, increasing their bargaining power and positive freedom in the labor market. Their ability to exit precarious employment is significantly enhanced.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, mobile, national).

% Receive a basic income that covers essential needs, removing the immediate pressure to accept any available job and allowing them to pursue education, training, or more suitable employment. This reduces the coercive aspect of unemployment.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unemployed_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Lose their structural power to coerce workers into low-wage or poor-condition jobs, as workers now have a viable alternative. They face pressure to improve wages and working conditions to attract and retain staff, effectively 'paying' for the increased worker freedom.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_reliant_on_cheap_labor, payer,
    powerful, biographical, constrained, national).

% Administer and fund the unconditional income support program, managing its implementation and facing political costs or benefits depending on public perception and economic outcomes. They are responsible for the policy's design and enforcement.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% See their long-standing policy goals realized, gaining legitimacy and influence. They benefit from the empirical demonstration of the policy's effects on worker freedom and well-being.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, advocacy_groups_for_basic_income, beneficiary,
    organized, generational, analytical, national).

% Contribute to the funding of the unconditional income support through taxes. While some may benefit indirectly from a more stable society, they bear the direct financial cost of the program.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    organized, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal floor for economic security, allowing individuals to refuse exploitative labor and improving overall labor market matching by reducing desperation-driven employment. It ensures a baseline of human dignity and reduces the social costs of poverty.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to individuals, ensuring a basic standard of living independent of labor market participation. This transfer shifts power from employers to workers.
% ABSENT_VOICES: Those who believe in the inherent moral value of work for its own sake, or those who fear a 'lazy' populace, are often excluded from the policy design, as their objections are framed as moralizing rather than practical. Employers who benefit from cheap labor also actively resist this policy.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, low-wage workers and unemployed individuals would immediately lose their freedom to refuse coercive work, reverting to a state of economic precarity. Employers would regain significant leverage, and the labor market would become more coercive, leading to a rapid reorganization of power dynamics and increased social inequality.
% FOUNDING_PROBLEM: The problem of a coercive labor market where individuals are forced to accept exploitative or undesirable work due to the threat of destitution, leading to widespread precarity, suppressed wages, and limited opportunities for self-actualization.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists, social justice organizations, and human rights advocates consistently corroborate the existence and persistence of coercive labor market dynamics, citing evidence of poverty wages, precarious employment, and the psychological toll of economic insecurity. International human rights frameworks also support the need for basic economic security.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.15) is low because the policy actively reduces the extraction of surplus value from workers by increasing their bargaining power. Suppression (0.10) is also low, as the primary function of the policy is to remove the coercive pressure to accept any job, thus expanding individual agency. Theater ratio is negligible (0.05) because the policy's function is direct and transparent: a financial transfer. Accessibility collapse is low (0.10) as it actively creates alternatives for workers. Resistance (0.70) is high, reflecting the significant opposition from employers and political factions who benefit from the existing coercive labor market structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of workers, this constraint is a liberating force, a 'rope' that offers genuine coordination for collective well-being. From the perspective of employers reliant on cheap labor, it is an extractive 'snare' that takes away their power and forces them to pay more for labor. The engine's computation will highlight this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers and unemployed individuals are clear beneficiaries, experiencing reduced extraction and increased freedom (low directionality). Employers reliant on cheap labor are victims, as their ability to extract labor at low cost is diminished (high directionality). Government agencies act as agenda-setters, implementing the policy. Taxpayers are payers, bearing the financial cost, though some may also be indirect beneficiaries of a more stable society.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_dependency_ambiguity,
    'Does unconditional income support primarily create positive freedom to refuse coercive work, or does it foster new forms of dependency on state provision?',
    'Longitudinal studies tracking labor market participation, entrepreneurial activity, and subjective well-being of recipients, compared to control groups, to assess the net effect on autonomy and agency.',
    'If dependency is found to be dominant, the constraint''s effective suppression might be higher than currently assessed, and its classification could shift towards a ''tangled_rope'' or even ''snare'' from certain perspectives. If freedom is confirmed, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_vs_dependency_ambiguity, empirical, 'Whether the policy''s primary outcome is increased freedom or new dependency.').

omega_variable(
    labor_market_impact_ambiguity,
    'What is the actual, empirically observed impact of unconditional income support on labor supply, wage levels, and employer investment in automation or job quality?',
    'Large-scale randomized control trials (RCTs) or natural experiments in jurisdictions implementing unconditional income support, with detailed economic and sociological data collection.',
    'If evidence shows significant reduction in labor supply or unintended wage suppression, the ''freedom_floor_reading'' might be challenged by the ''dependency_trap_reading'' or ''wage_subsidy_reading''. If it leads to higher wages and improved job quality, this reading is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_ambiguity, empirical, 'Empirical effects on labor market dynamics and employer behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__freedom_floor_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__freedom_floor_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__freedom_floor_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__freedom_floor_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, minimum_wage_laws).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, unemployment_benefits_conditionality).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_union_bargaining_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'income_support_conditionality' kernel, each representing a distinct structural claim about the policy's function and effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
