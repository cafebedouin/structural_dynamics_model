% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom floor' reading of
 *   unconditional income support, where it is seen as a mechanism to enhance
 *   individual autonomy by removing the coercive pressure of economic
 *   necessity from labor markets, eliminating welfare stigma, and providing a
 *   buffer against market shocks. It is framed as a Pareto improvement, with
 *   no direct victims, and a moderate level of extraction representing the
 *   necessary taxation for its operation. The claimed type is Rope,
 *   reflecting its function as a coordination mechanism for social well-being
 *   and individual freedom.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde').
narrative_ontology:cs_kernel_codification('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', formalized).
narrative_ontology:cs_authority_grounding('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', practice).
narrative_ontology:cs_interpretation_layer_present('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde').
narrative_ontology:cs_reading_relation('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', unconditional_income_support__dependency_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', foundational, labor_market_coercion_is_real).
narrative_ontology:cs_axiom_status(labor_market_coercion_is_real, holdable).
narrative_ontology:cs_axiom_grounding('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', labor_market_coercion_is_real, empirically_contingent).
narrative_ontology:cs_axiom('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', foundational, basic_needs_security_enables_autonomy).
narrative_ontology:cs_axiom_status(basic_needs_security_enables_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', basic_needs_security_enables_autonomy, deontological).
narrative_ontology:cs_reference_frame('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', autonomy_maximization_framework).
narrative_ontology:cs_drift_state('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ff4f04ea-ef59-4c0e-9beb-ba9e5097ffde', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, broader_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, autonomy_as_social_good).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, dignity_of_labor_redefined).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proposes, designs, and implements the unconditional income support program. Bears the political cost of initial implementation and ongoing fiscal responsibility. Aims to achieve social stability and economic justice.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, government_legislators, agenda_setter,
    institutional, generational, constrained, national).

% Receives a basic income, enabling them to refuse exploitative labor, pursue education, or transition to more fulfilling work. Reduces their vulnerability to market shocks and employer coercion.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, local).

% Receives income that recognizes the value of unpaid care work, providing financial stability and reducing the pressure to enter the formal labor market out of necessity. Enhances their autonomy in family decisions.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, local).

% Receives income that allows them to pursue creative work without immediate commercial pressure, fostering cultural production and innovation that might not be market-viable in the short term.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, local).

% Receives financial independence, providing a critical pathway to exit abusive relationships where economic dependency is a primary trap. This directly addresses a form of interpersonal coercion.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, identity_locked, local).

% Contributes to the funding of the unconditional income support through taxes. While bearing a direct financial cost, they indirectly benefit from reduced social costs (healthcare, crime, poverty-related services) and a more stable society.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers, payer,
    organized, generational, mobile, national).

% Benefits from reduced poverty, improved public health, increased social cohesion, and a more resilient economy less prone to demand shocks. Experiences a shift towards a more just and equitable distribution of resources.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, broader_society, beneficiary,
    institutional, civilizational, analytical, national).

% Would object to the principle of unconditional income, arguing it distorts market incentives and creates moral hazard. Their voices are often influential in policy debates but are structurally excluded from the direct benefits or administration of such a program.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, market_fundamentalists, excluded,
    powerful, generational, analytical, national).

% Analyze the effects of unconditional income support on labor markets, poverty rates, health outcomes, and social well-being. Provide empirical data and theoretical frameworks to inform policy debates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, social_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal economic floor, coordinating individual economic security and enabling more voluntary participation in labor, creative, and care markets by removing the coercion of absolute necessity.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens/residents, unconditionally, to ensure basic needs are met and autonomy is enhanced.
% ABSENT_VOICES: Those who believe in a purely market-driven allocation of resources or those who fear the moral hazard of 'unearned' income are often excluded from the design and implementation of such programs, though their arguments are present in public discourse.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, the social safety net would collapse, leading to a sharp increase in poverty, precarity, and social instability, particularly for vulnerable populations. Labor markets would revert to higher levels of coercion, and many individuals would lose their autonomy.
% FOUNDING_PROBLEM: Economic precarity, labor market coercion, welfare stigma, and vulnerability to economic shocks, which limit individual autonomy and create social instability.
% FOUNDING_PROBLEM_CORROBORATION: Social policy experts, labor economists, poverty advocates, and human rights organizations consistently attest to the ongoing nature of these problems, citing empirical data on poverty, income inequality, and the psychological impacts of precarity.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects the view that the financial transfer is a necessary cost for a significant social good, not a rent-seeking mechanism. Suppression (0.15) is low because the constraint's purpose is to *reduce* coercion, not impose it. Theater ratio (0.10) is low as the program's function is direct and transparent. Accessibility collapse (0.10) is low because it aims to *expand* alternatives for individuals. Resistance (0.40) is moderate, reflecting the political contestation around UBI, but not from those directly benefiting.
 *
 * PERSPECTIVAL GAP:
 *   While this reading posits a net benefit for all, other readings (e.g., 'dependency trap') would frame the same transfers as extractive from taxpayers and suppressive of individual initiative. The engine's per-seat classification would highlight this divergence if those alternative framings were instantiated as separate constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse victims are clear beneficiaries, experiencing a significant increase in autonomy and security. The broader society also benefits from reduced social costs and increased stability. Taxpayers bear the financial cost but are considered net beneficiaries due to the overall societal improvements. There are no direct victims in this reading, as the system is designed for universal benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effects_ambiguity,
    'What are the actual long-term effects of unconditional income support on labor supply and the nature of work?',
    'Longitudinal studies of large-scale UBI pilots and national implementations, tracking labor force participation, hours worked, and types of employment.',
    'If labor supply significantly decreases in essential sectors, the ''freedom floor'' claim of enabling voluntary participation might be challenged, potentially increasing the perceived extractiveness from taxpayers or shifting the classification towards a ''dependency trap'' reading. If labor shifts to more creative/care work, it reinforces the autonomy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effects_ambiguity, empirical, 'Uncertainty regarding the empirical impact on labor market dynamics.').

omega_variable(
    inflationary_pressure_ambiguity,
    'Does unconditional income support lead to significant inflationary pressures, effectively eroding its purchasing power and transferring wealth to asset holders?',
    'Macroeconomic modeling and empirical observation of price changes in goods and services, particularly housing and basic necessities, in regions with UBI programs.',
    'If significant inflation occurs, the ''freedom floor'' benefit would be diluted, and the program could be re-evaluated as having unintended extractive effects on those with fixed incomes or limited assets, potentially introducing a ''victim'' class not currently claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflationary_pressure_ambiguity, empirical, 'Uncertainty regarding the macroeconomic impact on inflation and wealth distribution.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint a genuine ''Rope'' enabling autonomy, or is its coordination function a cover for a ''Snare'' that creates dependency, as argued by the ''dependency_trap_reading''?',
    'Resolution depends on the adopted normative framework: whether individual freedom is prioritized over traditional labor market participation, and how ''dependency'' is defined (e.g., dependency on a state vs. dependency on an employer). Empirical data on well-being and labor choices would inform, but not fully resolve, this conceptual difference.',
    'If the ''dependency_trap_reading'' is adopted, the constraint would be reclassified as a Snare, with higher extractiveness and suppression, and a clear victim class (taxpayers, or those ''trapped'' in idleness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Conceptual ambiguity between autonomy-enabling and dependency-creating framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(unco_tr_t6, unconditional_income_support__freedom_floor_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__freedom_floor_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(unco_tr_t18, unconditional_income_support__freedom_floor_reading, theater_ratio, 18, 0.09).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__freedom_floor_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__freedom_floor_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(unco_be_t6, unconditional_income_support__freedom_floor_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__freedom_floor_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(unco_be_t18, unconditional_income_support__freedom_floor_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__freedom_floor_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__freedom_floor_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(unco_su_t6, unconditional_income_support__freedom_floor_reading, suppression_requirement, 6, 0.17).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__freedom_floor_reading, suppression_requirement, 12, 0.16).
narrative_ontology:measurement(unco_su_t18, unconditional_income_support__freedom_floor_reading, suppression_requirement, 18, 0.15).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__freedom_floor_reading, suppression_requirement, 24, 0.14).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__freedom_floor_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
