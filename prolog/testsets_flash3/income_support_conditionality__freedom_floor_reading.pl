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
 *   This constraint story represents the 'freedom floor' reading of
 *   unconditional income support conditionality. In this reading, the
 *   provision of unconditional income support is seen as a mechanism to
 *   decommodify labor power, granting individuals the positive freedom to
 *   refuse coercive or exploitative work. It shifts the balance of power in
 *   the labor market, moving from a system where workers are compelled by
 *   necessity to one where they have genuine choice. The constraint itself is
 *   the *absence* of coercive conditions, enabled by the income support.
 *
 * KEY AGENTS:
 *   - low_wage_workers: Primary beneficiary (moderate/mobile) — gains freedom to refuse coercive work
 *   - unemployed_individuals: Primary beneficiary (moderate/mobile) — gains freedom to pursue meaningful work/training
 *   - caregivers: Primary beneficiary (moderate/mobile) — gains recognition and financial stability for unpaid labor
 *   - employers_reliant_on_coercive_labor: Primary victim (powerful/constrained) — loses coercive power over labor
 *   - advocacy_groups_for_labor_rights: Agenda setter (organized/analytical) — champions the policy
 *   - taxpayers: Payer (moderate/constrained) — bears the financial cost of the program
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
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '39393673-cfb2-419e-b5c5-a64c3221b825').
narrative_ontology:cs_kernel_codification('39393673-cfb2-419e-b5c5-a64c3221b825', formalized).
narrative_ontology:cs_authority_grounding('39393673-cfb2-419e-b5c5-a64c3221b825', lineage).
narrative_ontology:cs_interpretation_layer_present('39393673-cfb2-419e-b5c5-a64c3221b825').
narrative_ontology:cs_reading_relation('39393673-cfb2-419e-b5c5-a64c3221b825', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('39393673-cfb2-419e-b5c5-a64c3221b825', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('39393673-cfb2-419e-b5c5-a64c3221b825', foundational, labor_power_decommodification_is_freedom).
narrative_ontology:cs_axiom_status(labor_power_decommodification_is_freedom, holdable).
narrative_ontology:cs_axiom_grounding('39393673-cfb2-419e-b5c5-a64c3221b825', labor_power_decommodification_is_freedom, deontological).
narrative_ontology:cs_axiom('39393673-cfb2-419e-b5c5-a64c3221b825', secondary, basic_income_enhances_human_agency).
narrative_ontology:cs_axiom_status(basic_income_enhances_human_agency, holdable).
narrative_ontology:cs_axiom_grounding('39393673-cfb2-419e-b5c5-a64c3221b825', basic_income_enhances_human_agency, instrumental).
narrative_ontology:cs_reference_frame('39393673-cfb2-419e-b5c5-a64c3221b825', post_industrial_social_contract).
narrative_ontology:cs_drift_state('39393673-cfb2-419e-b5c5-a64c3221b825', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('39393673-cfb2-419e-b5c5-a64c3221b825', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unemployed_individuals).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, caregivers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercive_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a baseline income, allowing them to refuse exploitative or unsafe work conditions without immediate destitution. This increases their bargaining power and ability to seek better employment.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, mobile, national).

% Are provided with a safety net that supports their basic needs, reducing the pressure to accept any available job and enabling them to pursue education, training, or entrepreneurial ventures.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unemployed_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Receive income support that recognizes the value of unpaid care work, providing financial stability and reducing the need to enter the formal labor market out of necessity.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, mobile, national).

% Lose their ability to rely on workers' desperation to fill low-wage, undesirable jobs. They face pressure to improve working conditions and wages to attract and retain employees.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_reliant_on_coercive_labor, payer,
    powerful, immediate, constrained, national).

% Champion the implementation and expansion of unconditional income support as a means to empower workers and advance social justice. They actively shape policy debates and public opinion.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, advocacy_groups_for_labor_rights, agenda_setter,
    organized, generational, analytical, national).

% Bear the financial cost of the income support program through taxes. Their willingness to support the program depends on perceived benefits to society and economic stability.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers, payer,
    moderate, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal agreement on a basic standard of living, ensuring that all individuals have a floor below which they cannot fall, thereby enabling greater individual agency in labor market participation.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to individuals, particularly those with low or no income, to provide a basic living standard.
% ABSENT_VOICES: The voices of those who would benefit from a more coercive labor market (e.g., certain business lobbies) are often present in policy debates, but their arguments for 'work incentives' are reframed by this reading as attempts to maintain exploitative conditions.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, low-wage workers and unemployed individuals would immediately lose their freedom to refuse coercive work, reverting to a state of heightened economic precarity and reduced bargaining power. The labor market dynamics would shift back towards employer dominance.
% FOUNDING_PROBLEM: The problem of widespread poverty, economic insecurity, and the coercive nature of labor markets that force individuals into undesirable work due to lack of alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists, social policy researchers, and international human rights organizations corroborate the ongoing existence of these problems, citing data on poverty rates, income inequality, and worker exploitation, independent of the direct beneficiaries of the policy.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.15) because the system is designed to provide a net benefit to the majority of its participants (workers) by reducing the extraction they face in the labor market. Suppression is also low (0.1) as the constraint's purpose is to *reduce* coercive pressure, not impose it. The theater ratio is minimal (0.05) as the policy's function directly aligns with its stated goal of providing a freedom floor. The metrics reflect a system that genuinely coordinates a social good.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage workers and unemployed individuals, this constraint is a clear Rope, providing a vital safety net and increasing their agency. For employers reliant on coercive labor, it is a Snare, as it removes their ability to exploit desperation. The analytical observer sees the overall shift in power dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers, unemployed individuals, and caregivers are beneficiaries (d near 0.0) as the income support directly empowers them. Employers reliant on coercive labor are victims (d near 1.0) as they lose a key mechanism of control. Taxpayers are payers (d near 0.5) as they contribute to the system but also benefit from a more stable and equitable society. Advocacy groups are agenda-setters, pushing for the policy's implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a genuine coordination mechanism (providing a freedom floor) as a Snare or Tangled Rope. The low extractiveness and suppression, coupled with clear beneficiaries and a coordination function, correctly classify it as a Rope. The 'mandate' here is to provide a baseline of security, which remains live and functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_on_labor_supply,
    'Does unconditional income support significantly reduce overall labor supply, leading to economic contraction, or does it reallocate labor towards more productive and meaningful sectors?',
    'Longitudinal studies and comparative analyses of regions/countries implementing unconditional income support programs, tracking labor force participation rates, sectorial shifts, and economic growth metrics.',
    'If labor supply significantly contracts without reallocation, the ''freedom floor'' reading''s positive economic impacts would be challenged, potentially shifting the constraint towards a more ''dependency trap'' interpretation. If reallocation occurs, it strengthens the ''freedom floor'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_labor_supply, empirical, 'Uncertainty regarding the aggregate effect of unconditional income support on labor market dynamics and economic productivity.').

omega_variable(
    coercion_definition_ambiguity,
    'Is ''coercive work'' an objective category, or is its definition subjective and open to interpretation, potentially leading to moral hazard?',
    'Development of objective criteria for ''coercive work'' (e.g., below living wage, unsafe conditions, lack of autonomy) through deliberative processes and legal frameworks, or empirical studies on worker perceptions of coercion.',
    'If ''coercive work'' remains ill-defined, the ''freedom floor'' claim could be undermined by accusations of enabling ''laziness'' or ''moral hazard'', aligning more with the ''dependency trap'' reading. Clearer definitions strengthen the claim of positive freedom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''coercive work'' and its implications for the justification of unconditional income support.').

omega_variable(
    sibling_reading_reconciliation,
    'How does this ''freedom floor'' reading reconcile with the ''dependency trap'' and ''wage subsidy'' readings of income support conditionality?',
    'Empirical evidence on labor market outcomes (e.g., worker bargaining power, wage levels, skill development) and policy analysis comparing the actual effects of unconditional income support against the claims of each reading.',
    'If evidence strongly supports the ''dependency trap'' or ''wage subsidy'' readings, this ''freedom floor'' reading''s validity would be challenged, potentially leading to a reclassification of the underlying policy as a Snare or Tangled Rope from a different perspective. If this reading''s claims are robust, it would highlight the ideological nature of the competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_reconciliation, conceptual, 'This constraint is one reading of the ''income_support_conditionality'' kernel. The ''dependency_trap_reading'' argues that unconditional support undermines work incentives, while the ''wage_subsidy_reading'' claims it primarily benefits employers by enabling wage suppression. This omega documents the contestation between these interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.03).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.13).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
