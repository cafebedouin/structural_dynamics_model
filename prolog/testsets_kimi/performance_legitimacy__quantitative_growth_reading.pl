% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy: Quantitative Growth Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates the quantitative_growth_reading of the
 *   performance_legitimacy kernel: regime legitimacy is grounded in
 *   maintaining headline GDP growth rates, enforced through Five-Year Plans,
 *   cadre evaluation KPIs, and fiscal allocation mechanisms. It is claimed as
 *   developmental coordination but operates as a tangled ropeâgenuinely
 *   coordinating capital and labor toward industrialization while
 *   asymmetrically extracting from households, workers, and the environment
 *   to benefit the industrial-export complex and officials whose careers
 *   depend on hitting targets. The kernel has sibling readings (qualitative
 *   development, techno-nationalism, livelihood security) that assign
 *   legitimacy to different metrics; this reading makes aggregate growth rate
 *   the supreme constraint.
 *
 * KEY AGENTS:
 *   - Central planning authority: agenda_setter (institutional/generational) â sets GDP targets and cadre evaluation criteria
 *   - Local government officials: agenda_setter/beneficiary (organized/biographical) â enforce targets locally and capture promotion and rents
 *   - Industrial-export complex: beneficiary (powerful/biographical) â receives preferential credit and subsidies
 *   - General public: payer (powerless/biographical) â bears externalized costs of pollution, debt, and underfunded services
 *   - Workers in overcapacity sectors: payer (powerless/immediate) â face instability and layoffs with limited exit
 *   - Welfare and services advocates: excluded (moderate/biographical) â sidelined because social spending is GDP-weak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.72).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy: Quantitative Growth Reading").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '6673fa12-f141-4eae-91bd-cdb11da7cb80').
narrative_ontology:cs_kernel_codification('6673fa12-f141-4eae-91bd-cdb11da7cb80', formalized).
narrative_ontology:cs_authority_grounding('6673fa12-f141-4eae-91bd-cdb11da7cb80', expertise).
narrative_ontology:cs_interpretation_layer_present('6673fa12-f141-4eae-91bd-cdb11da7cb80').
narrative_ontology:cs_reading_relation('6673fa12-f141-4eae-91bd-cdb11da7cb80', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('6673fa12-f141-4eae-91bd-cdb11da7cb80', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6673fa12-f141-4eae-91bd-cdb11da7cb80', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('6673fa12-f141-4eae-91bd-cdb11da7cb80', foundational, aggregate_growth_supreme_metric).
narrative_ontology:cs_axiom_status(aggregate_growth_supreme_metric, holdable).
narrative_ontology:cs_axiom_grounding('6673fa12-f141-4eae-91bd-cdb11da7cb80', aggregate_growth_supreme_metric, empirically_contingent).
narrative_ontology:cs_axiom('6673fa12-f141-4eae-91bd-cdb11da7cb80', foundational, externalized_growth_costs_necessary).
narrative_ontology:cs_axiom_status(externalized_growth_costs_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6673fa12-f141-4eae-91bd-cdb11da7cb80', externalized_growth_costs_necessary, instrumental).
narrative_ontology:cs_reference_frame('6673fa12-f141-4eae-91bd-cdb11da7cb80', rapid_industrialization_growth_state).
narrative_ontology:cs_drift_state('6673fa12-f141-4eae-91bd-cdb11da7cb80', post_industrial_overcapacity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6673fa12-f141-4eae-91bd-cdb11da7cb80', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, general_public).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, workers_in_overcapacity_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national GDP growth targets and embeds them in Five-Year Plans, government work reports, and cadre evaluation criteria. Its political survival is tied to delivering headline growth, making the target a structural imperative that constrains even top leadership's policy autonomy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authority, agenda_setter,
    institutional, generational, constrained, national).

% Enforce growth targets locally through investment promotion, land sales, debt-financed infrastructure, and project approval. Their promotions and political survival depend on hitting GDP numbers, fusing personal career incentives with the constraint's maintenance. They collect political legitimacy, tax revenue, and sometimes direct rents from the investment pipeline.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary).

% Receives preferential credit, subsidies, regulatory forbearance, and market protection in exchange for delivering output, employment, and export figures that feed aggregate GDP metrics. Their business models depend on continued state-directed investment and tolerance of overcapacity.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, constrained, global).

% Bears the externalized costs of growth-target enforcementâenvironmental degradation, underfunded healthcare and education systems, misallocated capital, and local government debtâwithout a seat at the target-setting table. Their consumption is suppressed relative to investment, and their grievances are managed rather than addressed structurally.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, general_public, payer,
    powerless, biographical, constrained, national).

% Employed in industries kept alive by investment-target pressure rather than market demand; face wage arrears, unsafe conditions, and eventual layoffs when overcapacity becomes unsustainable. Mobility is limited by household registration, skill barriers, and regional economic concentration.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, workers_in_overcapacity_sectors, payer,
    powerless, immediate, trapped, national).

% Argue for redirecting fiscal resources toward healthcare, education, pensions, and environmental remediation. They are present in public discourse but systematically sidelined in budget allocation because social spending does not register as strongly in GDP growth metrics as infrastructure and industrial investment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, welfare_and_services_advocates, excluded,
    moderate, biographical, constrained, national).

narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national-scale capital allocation and labor mobilization toward measurable output targets, solving the collective-action problem of rapid industrialization and employment generation in a capital-scarce developing economy.
% TRANSFER_FUNCTION: Moves capital, land, and labor from household consumption and social services toward heavy industry, infrastructure, and export manufacturing; moves political legitimacy and career advancement from the center to local officials who hit targets; moves environmental degradation, debt burdens, and opportunity costs from the industrial-export complex to the general public.
% ABSENT_VOICES: Welfare advocates, environmental regulators seeking to enforce standards over growth targets, and household consumption advocates are present in discourse but excluded from budget priority; future generations who will service the debt and restore degraded environments have no representation.
% DISAPPEARANCE_RATIONALE: If GDP growth ceased to be the primary legitimacy constraint, capital allocation would shift away from investment-heavy industry, local officials would reorient promotion criteria, the industrial-export complex would lose preferential credit, and household consumption and services would rise in political priorityâthe developmental model would reorganize around a different metric of success.
% FOUNDING_PROBLEM: Post-colonial and post-revolutionary underdevelopment characterized by low industrial capacity, rural surplus labor, foreign technological dependence, and insufficient state revenue to fund military and economic modernization.
% FOUNDING_PROBLEM_CORROBORATION: Development economists outside the state-industrial complex acknowledge the initial industrialization gap was real but argue it has been substantially closed; international financial institutions and independent macroeconomic analysts cite overcapacity, debt sustainability risks, and demographic headwinds as evidence that the founding problem's solution has generated new pathologies that the quantitative growth model cannot resolve.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically channels surplus from household consumption and social spending toward investment-heavy industry and infrastructure, decoupling political survival from welfare delivery. Suppression (0.68) reflects the active exclusion of alternative development modelsâenvironmental enforcement, consumption-led growth, and welfare expansion are subordinated to the target. Theater ratio (0.45) captures the growing share of performative investment (ghost infrastructure, statistical smoothing, excess capacity) that registers as GDP but yields diminishing marginal utility. Accessibility collapse (0.60) indicates that while alternative models exist in theory, they collapse in practice because the cadre evaluation system makes GDP the dominant institutional language. Resistance (0.55) comes from environmental regulators, indebted localities, and popular grievance, but is systematically overridden by target pressure.
 *
 * PERSPECTIVAL GAP:
 *   The central planning authority and local officials experience the constraint as necessary coordinationâwithout growth targets, they argue, capital disperses and modernization stalls. The general public and overcapacity workers experience it as extractionâ their health, savings, and job security are sacrificed to investment figures they do not control. The engine computes this divergence from the structural data: same constraint, different directionalities depending on beneficiary versus payer position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Local government officials and the industrial-export complex are structural beneficiaries (low d): the constraint subsidizes their political survival, revenue, and preferential credit. The general public and overcapacity workers are structural targets (high d): they pay through externalized costs, suppressed consumption, and labor precarity. Welfare advocates are excluded, sitting outside the directionality calculation entirely. The central planning authority is the agenda setter whose directionality is intermediateâit imposes the constraint but is also trapped by its own legitimacy logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve genuine underdevelopmentâlow industrial capacity and surplus labor. That founding problem is now contested: industrial capacity is vast, overcapacity is chronic, and the constraint persists because the industrial-export complex and official promotion system depend on it, not because the original development gap remains. The framework detects this via the R5 genealogy interview (founding_problem_status contested) and the rising theater_ratio measurement series, which signals that proxy goals (headline GDP) have partially replaced the original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the quantitative growth reading exhaust the performance legitimacy kernel, or do the sibling readings (qualitative development, livelihood security, techno-nationalism) represent structurally distinct constraints with non-overlapping victim sets?',
    'Comparative analysis of the sibling constraint stories and their directionality profiles; identification of whether a single regime can hold multiple readings simultaneously without logical contradiction.',
    'If the readings are mutually coexisting rather than foreclosing, the kernel is distributed; if they are mutually exclusive in practice, each reading is a competing constraint for the same institutional space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural relationship of this reading to sibling kernel readings').

omega_variable(
    founding_problem_liveness,
    'Has the original underdevelopment problem that justified quantitative growth targeting been solved, leaving the constraint as rent-preserving extraction, or does persistent regional inequality and technological dependency keep the problem live?',
    'Independent macroeconomic assessment of industrial capacity utilization rates, regional development gaps, and technological import dependency versus export competitiveness.',
    'If the founding problem is dead, the constraint is mandatrophic and the measured extraction is largely surplus preservation; if live, a significant portion of the extraction is the necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the original development gap persists or has been superseded').

omega_variable(
    cost_bearing_ambiguity,
    'Are the costs of the growth target borne diffusely by the general public through foregone services and environmental degradation, or concentrated on specific demographic and geographic populations?',
    'Spatial and sectoral analysis of debt burdens, pollution exposure, and social-spending shortfalls relative to GDP contribution by region and industry.',
    'If costs are concentrated on powerless populations, effective extraction is higher than the aggregate metric suggests; if diffuse, the constraint operates more like a broad coordination tax.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_bearing_ambiguity, empirical, 'Distribution of extraction costs across populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perfleg_quantgrowth_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perfleg_quantgrowth_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(perfleg_quantgrowth_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(perfleg_quantgrowth_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(perfleg_quantgrowth_tr_t32, performance_legitimacy__quantitative_growth_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(perfleg_quantgrowth_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(perfleg_quantgrowth_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perfleg_quantgrowth_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(perfleg_quantgrowth_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(perfleg_quantgrowth_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(perfleg_quantgrowth_be_t32, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(perfleg_quantgrowth_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perfleg_quantgrowth_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perfleg_quantgrowth_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(perfleg_quantgrowth_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(perfleg_quantgrowth_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(perfleg_quantgrowth_su_t32, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(perfleg_quantgrowth_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is the quantitative_growth_reading of the performance_legitimacy kernel. Sibling readings instantiate different constraints from the same kernel, each with distinct beneficiary/victim structures and epsilon values. The quantitative reading influences resource availability for the qualitative and livelihood readings while coexisting with the techno-nationalist reading in industrial policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
