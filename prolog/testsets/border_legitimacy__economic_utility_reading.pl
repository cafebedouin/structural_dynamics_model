% ============================================================================
% CONSTRAINT STORY: border_legitimacy__economic_utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__economic_utility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__economic_utility_reading
 *   human_readable: Border Legitimacy: Economic Utility Reading
 *   domain: political_philosophy/migration_policy/constitutional_law
 *
 * SUMMARY:
 *   The economic utility reading of border legitimacy frames border policy as
 *   justified when the aggregate economic benefit of permitting migration
 *   exceeds the costs. This constraint is one reading of a contested
 *   kernel—the foundational question of what legitimates state border
 *   control. The economic reading competes with sovereignty-primary readings
 *   (borders are justified because states have the right to
 *   self-determination) and freedom-of-movement readings (borders are
 *   justified only if they protect individual liberty, not aggregate
 *   welfare). This story instantiates the economic utility reading as a
 *   clean, ε-invariant constraint, assuming throughout that 'net benefit >
 *   net cost' is the operative decision rule. The critical structural
 *   feature: this reading produces a victim set that varies depending on
 *   whether the net benefit calculation yields positive or negative results.
 *   If net benefit is positive, the primary victims are inefficiently
 *   excluded migrants (those who would contribute more than they cost but are
 *   excluded anyway). If net benefit is negative, the primary victims are
 *   displaced citizen workers (those who bear the labor market adjustment
 *   costs). Extractiveness (0.58) reflects the measurement uncertainty and
 *   the distributional asymmetry: the constraint enforces a utilitarian
 *   aggregation that can be used to justify extraction from any particular
 *   group in service of claimed aggregate gain. Suppression (0.62) reflects
 *   the enforcement barriers required to maintain border control against the
 *   economic logic that would favor higher migration in many sectors. Theater
 *   (0.68) reflects that immigration administration routinizes economic
 *   justification (visa category for 'skills,' labor certification
 *   requirements) without actually performing the granular cost-benefit
 *   analysis the reading theoretically requires.
 *
 * KEY AGENTS:
 *   - Economically Displaced Citizen Worker (powerless/trapped): Primary victim in positive-benefit scenario. Bears concentrated, localized labor market costs. No exit option; no voice in aggregate welfare calculation.
 *   - Excluded Migrant (powerless/trapped): Primary victim in negative-benefit scenario. Trapped outside border; excluded by economic logic that claims to optimize their non-admission. Bears opportunity cost of exclusion.
 *   - Domestic Labor Coalition (moderate/constrained): Mixed beneficiary-victim. Organized labor experiences both coordination (wage protection) and extraction (concentrated adjustment costs on some workers). Can mobilize but cannot exit the national labor market.
 *   - Capital and High-Skill Sector Beneficiaries (institutional/arbitrage): Primary beneficiaries. Gain from labor supply expansion; have exit options (offshore investment). Economic utility reading aligns with their structural interests.
 *   - Border Enforcement Bureaucracy (institutional/arbitrage): Maintains the constraint through routinized immigration administration. Performs substantially performative economic assessment (piton perspective). Has arbitrage options but benefits from institutional continuity.
 *   - Organized Migration Advocates and Rights Communities (organized/constrained): Experience both coordination (rights protection can be framed as welfare-maximizing) and extraction (individual rights claims subordinated to aggregate calculation). Constrained by jurisdictional and international law frameworks.
 *   - Skilled Migrant Beneficiaries (institutional/arbitrage): Experience the constraint as enabling coordination. Have arbitrage options across multiple destination countries. Economic utility reading legitimizes their admission.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the economic utility reading as logical necessity rather than contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__economic_utility_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__economic_utility_reading, 0.62).
domain_priors:theater_ratio(border_legitimacy__economic_utility_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__economic_utility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__economic_utility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__economic_utility_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__economic_utility_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__economic_utility_reading, "Border Legitimacy: Economic Utility Reading").
narrative_ontology:topic_domain(border_legitimacy__economic_utility_reading, "political_philosophy/migration_policy/constitutional_law").

domain_priors:requires_active_enforcement(border_legitimacy__economic_utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__economic_utility_reading, '9523c478-0896-494c-b621-daaa4da7a60b').
narrative_ontology:cs_kernel_codification('9523c478-0896-494c-b621-daaa4da7a60b', formalized).
narrative_ontology:cs_authority_grounding('9523c478-0896-494c-b621-daaa4da7a60b', lineage).
narrative_ontology:cs_interpretation_layer_present('9523c478-0896-494c-b621-daaa4da7a60b').
narrative_ontology:cs_reading_relation('9523c478-0896-494c-b621-daaa4da7a60b', border_legitimacy__sovereignty_primary_reading, coexists_with).
narrative_ontology:cs_reading_relation('9523c478-0896-494c-b621-daaa4da7a60b', border_legitimacy__freedom_of_movement_primary_reading, forecloses).
narrative_ontology:cs_axiom('9523c478-0896-494c-b621-daaa4da7a60b', foundational, welfare_aggregation_supremacy).
narrative_ontology:cs_axiom_status(welfare_aggregation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9523c478-0896-494c-b621-daaa4da7a60b', welfare_aggregation_supremacy, instrumental).
narrative_ontology:cs_axiom('9523c478-0896-494c-b621-daaa4da7a60b', secondary, measurable_aggregation_feasibility).
narrative_ontology:cs_axiom_status(measurable_aggregation_feasibility, overridden).
narrative_ontology:cs_axiom_grounding('9523c478-0896-494c-b621-daaa4da7a60b', measurable_aggregation_feasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('9523c478-0896-494c-b621-daaa4da7a60b', utilitarian_border_optimization).
narrative_ontology:cs_drift_state('9523c478-0896-494c-b621-daaa4da7a60b', contemporary_measurement_crisis, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9523c478-0896-494c-b621-daaa4da7a60b', '2025-06-15T14:32:18Z').
narrative_ontology:cs_kernel_id(border_legitimacy__economic_utility_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__economic_utility_reading, net_economic_gainers_aggregate).
narrative_ontology:constraint_beneficiary(border_legitimacy__economic_utility_reading, capital_and_high_skill_sectors).
narrative_ontology:constraint_victim(border_legitimacy__economic_utility_reading, economically_displaced_citizens).
narrative_ontology:constraint_victim(border_legitimacy__economic_utility_reading, inefficiently_excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__economic_utility_reading, labor_market_adjustment_burden_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISPLACED CITIZEN (SNARE) — Subject to border policy justified by aggregate welfare gain but bears concentrated, localized labor market displacement costs. Cannot exit national labor market; experiences extraction as pure welfare loss without countervailing benefit or voice in cost-benefit calculation. Maximum suppression: no collective exit option, minimal compensation mechanisms.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED MIGRANT—NEGATIVE SCENARIO (SNARE) — When aggregate benefit calculus excludes migration, the excluded migrant is trapped outside the border and derives no benefit from the constraint logic that justifies their exclusion. Bears the full opportunity cost of non-migration. No exit from their exclusion; no voice in benefit accounting. This perspective is the dual victim of the displacement scenario — trapped on the other side of the gate.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMESTIC LABOR COALITION (TANGLED ROPE) — Partially organized labor unions and worker advocates experience both coordination and extraction. The constraint coordinates labor market stability (prevents destabilizing wage shocks in low-skill sectors) but also extracts by concentrating adjustment costs on displaced workers while benefits diffuse across consumers and employers. Exit options are constrained: workers can organize for retraining programs and transition support but cannot exit the national labor market itself. Some sectors and skill levels benefit (protected high-skill domestic workers); others lose. Moderate experienced extraction reflects this asymmetry within the coalition.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL AND HIGH-SKILL BENEFICIARIES (ROPE) — Experience the constraint as coordination: migration policy that admits skilled labor solves labor shortage coordination problems; capital gains from labor supply expansion; consumers benefit from lower service prices; firms in export sectors gain from labor availability. These agents have arbitrage options (relocate investment, offshore production) and experience the constraint as enabling coordination rather than extractive. The economic utility reading aligns with their structural interests.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BORDER ENFORCEMENT BUREAUCRACY (PITON) — The administrative apparatus for implementing economic utility calculations (immigration courts, labor market surveillance, border agencies) performs substantially performative functions: cost-benefit analyses are routinized but not empirically validated; individual cases are adjudicated by proxies (visa category, skills assessment) rather than actual economic impact; the system persists through institutional inertia despite low functional economic optimization. Theater ratio high because the constraint's theoretical justification (net benefit calculation) exceeds the actual decision-making precision. The bureaucracy has become the constraint's own maintenance mechanism.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MIGRATION ADVOCATES AND RIGHTS COMMUNITIES (TANGLED ROPE) — Organized actors (migrant rights groups, international labor organizations, humanitarian advocates) experience both coordination and extraction. They have constrained exit options (they can mobilize support and challenge policy but operate within national jurisdictions and international law frameworks). They experience genuine coordination benefit: the constraint's economic utility logic can be reframed toward human-rights-protective end (maximizing aggregate welfare includes protecting migrants from exploitation, enforcing labor standards). But they also experience extraction: their framing is subordinated to aggregate welfare calculus; individual rights claims are overridden by cost-benefit accounting.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SKILLED MIGRANT BENEFICIARIES (ROPE) — Migrants who clear the positive net benefit threshold experience the constraint as enabling coordination: the policy permits beneficial migration and solves the coordination problem of labor market matching. These agents have arbitrage options (multiple countries competing for talent; can relocate). Experience the constraint as pure coordination mechanism that benefits them. The economic utility reading legitimizes their admission by serving their interests alongside capital's interests.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER—NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some version of 'optimize aggregate welfare' may appear as an inevitable normative law: rational policy must maximize net benefit, just as physics must obey conservation laws. This perspective reads the constraint as emerging naturally from economic logic itself. However, the structural data contradicts the mountain classification: identifiable beneficiaries (capital, high-skill sectors) and victims (displaced workers, excluded migrants) exist; the constraint requires active enforcement; suppression is substantial. The engine will detect this as a false summit—naturalization of a contingent distributional choice as logical necessity.
constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__economic_utility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__economic_utility_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__economic_utility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_legitimacy__economic_utility_reading, TR),
    TR >= 0.70.

:- end_tests(border_legitimacy__economic_utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The economic utility reading justifies extraction by claiming to maximize aggregate welfare, but produces extractive outcomes by concentrating costs on identifiable groups (displaced workers or excluded migrants) while distributing benefits diffusely across consumers, employers, and skilled migrants. The extractiveness is not maximal because genuine coordination benefits exist (labor market matching, wage stabilization in some sectors, skill supply for growth sectors). The measurement reflects uncertainty: if the net benefit calculation is genuinely negative (short-term focus, narrow welfare metrics), extractiveness approaches Snare (>0.66); if genuinely positive (long-term focus, broad metrics), extractiveness approaches Rope (<0.35). At 0.58, we assume moderate uncertainty about which calculation is correct. Suppression (0.62): Moderate-high. Border control requires substantial enforcement apparatus (immigration courts, labor market surveillance, deportation mechanisms) to suppress the natural economic incentive for higher migration. This enforcement is active and resource-intensive. Suppression increases over the interval as administrative burden of managing the constraint grows. Theater ratio (0.68): Moderate-high. Immigration administration performs economic justification through visa categories and labor certification but does not actually conduct granular cost-benefit analysis for individual cases. The constraint's theoretical logic (optimize net benefit) exceeds the actual implementation precision (categorical assignment). Theater increases over the interval as routinized procedures replace individualized assessment.
 *
 * PERSPECTIVAL GAP:
 *   The economic utility reading produces a radical perspectival gap: the same structural constraint (permission/exclusion of migration justified by aggregate welfare) appears as Snare to displaced workers, Rope to capital, Tangled Rope to organized labor advocates, Piton to the bureaucracy, and Mountain (false summit) to analytical observers who naturalize the logic. The gap arises because the reading's aggregation logic (sum costs and benefits across all affected agents, permit migration if sum > 0) does not distribute the costs and benefits equally. The beneficiaries (capital, consumers, skilled migrants) are dispersed and diffuse; the victims (displaced workers or excluded migrants) are concentrated and identifiable. From the victim's perspective, they bear concentrated cost justified by diffuse aggregate gain in which they do not participate—this is the experience of extraction. From the beneficiary's perspective, they solve a coordination problem (labor market matching)—this is the experience of rope. The analytical observer risks seeing the aggregate welfare logic as a natural law (mountain) rather than as a contingent institutional choice that benefits some agents more than others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the cost-benefit flow. Displaced workers have high d (d ≈ 0.90): they bear costs without corresponding benefit; their power is low; their exit is trapped. Excluded migrants have high d (d ≈ 0.92) in the negative-benefit scenario: they are excluded by the very calculation that claims to optimize their exclusion; they have no voice in the calculation; they are trapped. Capital beneficiaries have low d (d ≈ 0.15): they gain from the constraint; they have arbitrage options; the flow of benefits runs toward them. Skilled migrant beneficiaries have very low d (d ≈ 0.08): they are admitted; they benefit; they can arbitrage across jurisdictions. Organized labor and advocates have moderate d (d ≈ 0.55): they experience mixed cost-benefit; they have constrained exit (can organize but not escape national labor market); they are neither pure beneficiaries nor pure victims. The bureaucracy has low d (d ≈ 0.20): it benefits from institutional continuity and has arbitrage options (can relocate resources). The analytical observer has d ≈ 0.72 (standard canonical for analytical power): they see the full structure but risk naturalizing it.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The economic utility reading resolves mandatrophy by accepting that the constraint is genuinely a Tangled Rope that produces extraction from some agents while coordinating for others. The temptation to classify as pure Rope (coordination of labor markets) or pure Snare (extraction from workers) is resisted by recognizing that both elements are structurally present. The constraint does solve a coordination problem (matching labor supply and demand across borders) AND it does extract from workers who bear adjustment costs. This is mandatrophy resolved—accepting the hybrid classification rather than trying to force the constraint into one pure type. The false summit is the analytical observer's reading that this is a natural law (Mountain)—the reading itself must be rejected as a form of framing bias that naturalizes distributional choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_benefit_calculation_uncertainty,
    'What is the true net economic benefit of permitting migration, and whose discount rates, time horizons, and valuation methods determine the calculation?',
    'Meta-analysis of migration economic impact studies; identification of methodological disagreement (fiscal vs. dynamic effects, labor-market equilibration periods, consumption benefits, long-term fiscal impacts); sensitivity analysis across discount rates (3%, 5%, 7% produce different long-term signs)',
    'Different benefit calculations produce different victim sets: low discount rate = long-term benefits outweigh short-term displacement (victims are excluded migrants); high discount rate = near-term costs dominate (victims are displaced citizens). Classification hinges on which calculation is deployed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(net_benefit_calculation_uncertainty, empirical, 'Uncertainty in net economic benefit calculation methodology and discount rates').

omega_variable(
    aggregate_welfare_versus_distributional_justice,
    'Does the economic utility reading foreclose or coexist with distributional justice readings of border legitimacy?',
    'Logical analysis of frameworks: can a single framework hold both ''maximize aggregate welfare'' and ''distribute burdens fairly regardless of aggregate outcome''? If yes, reading coexists; if no, economic utility forecloses rights-based readings.',
    'If economic utility forecloses justice: siblings can only coexist through institutional separation (different policy spheres). If coexist: hybrid frameworks possible (welfare-maximizing plus minimum protections). If influences: economic reading constrains but does not eliminate justice considerations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_versus_distributional_justice, conceptual, 'Logical relationship between aggregate welfare and distributional justice frameworks').

omega_variable(
    measurement_externalities_and_scope,
    'Which costs and benefits are counted in ''net economic benefit''? Are global welfare gains counted equally to national gains? Are environmental, health, and inequality effects included or excluded?',
    'Specification of the welfare function: which variables enter the calculation? Comparison of studies using inclusive vs. narrow welfare metrics; analysis of how national-vs-global framing changes sign of net benefit',
    'Narrow (national, fiscal only) calculations: migration often shows negative net benefit in short term (victim = displaced citizens). Broad (global, distributional, long-term) calculations: migration shows positive benefit (victim = excluded migrants). Epsilon is stable but victim set varies by scope definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_externalities_and_scope, conceptual, 'Definitional scope of ''net economic benefit'' and what costs/benefits are counted').

omega_variable(
    committer_reading_kernel_ambiguity,
    'Is border legitimacy ultimately grounded in state sovereignty, individual freedom of movement, or aggregate welfare maximization—and does this reading''s premise logically foreclose the other two sibling readings?',
    'Analysis of whether a single authority structure can hold multiple grounding premises (sovereignty + freedom + welfare as simultaneous commitments) or whether they represent genuinely alternative kernels. If coexist in practice, determine whether they are held by different parties (institutional separation) or within the same framework (theoretical synthesis).',
    'If economic utility forecloses sovereignty: legitimacy cannot rest on state power alone. If forecloses freedom: legitimacy cannot rest on individual rights. If neither: all three are live readings coexisting across different institutional actors and interpretive traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_kernel_ambiguity, conceptual, 'Logical and institutional relationship among border legitimacy sibling readings').

omega_variable(
    theater_in_economic_assessment,
    'How much of the border enforcement and immigration administration apparatus performs substantive economic optimization versus performative compliance with the economic utility logic?',
    'Audit of immigration court decisions: what proportion are decided by explicit economic impact assessment vs. visa category matching vs. bureaucratic procedure? Analysis of whether individual case outcomes correlate with ex-post economic impact or with administrative category assignment.',
    'High theater (>0.70): the constraint is substantially performative (Piton tendency). Low theater (<0.40): the constraint is functionally optimizing. Theater is a diagnostic of how much the constraint''s enforcement actually instantiates its theoretical justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_in_economic_assessment, empirical, 'Proportion of immigration administration that performs economic optimization vs. bureaucratic procedure').

omega_variable(
    excluded_migrant_counterfactual_benefit,
    'When migration is excluded by the economic utility reading, do excluded migrants derive any benefit from the constraint (e.g., wage protection in their origin country due to reduced labor supply), or does the constraint impose pure cost without countervailing benefit?',
    'Analysis of origin-country labor market effects: does exclusionary migration policy improve wage outcomes for non-migrants in origin country? Comparison of excluded-migrant welfare under exclusionary vs. open policy regimes.',
    'If excluded migrants derive benefit: their experience softens from pure Snare toward constrained Tangled Rope (they experience coordination benefit even though excluded). If no benefit: Snare classification is correct—pure extraction with no offsetting gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_migrant_counterfactual_benefit, empirical, 'Whether excluded migrants derive countervailing benefit from exclusionary policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__economic_utility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_econ_theater_t0, border_legitimacy__economic_utility_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(border_econ_theater_t5, border_legitimacy__economic_utility_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(border_econ_theater_t10, border_legitimacy__economic_utility_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(border_econ_extractiveness_t0, border_legitimacy__economic_utility_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(border_econ_extractiveness_t5, border_legitimacy__economic_utility_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(border_econ_extractiveness_t10, border_legitimacy__economic_utility_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_econ_suppression_t0, border_legitimacy__economic_utility_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(border_econ_suppression_t5, border_legitimacy__economic_utility_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(border_econ_suppression_t10, border_legitimacy__economic_utility_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__economic_utility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_legitimacy__economic_utility_reading, 0.18).
narrative_ontology:affects_constraint(border_legitimacy__economic_utility_reading, border_legitimacy__sovereignty_primary_reading).
narrative_ontology:affects_constraint(border_legitimacy__economic_utility_reading, border_legitimacy__freedom_of_movement_primary_reading).
narrative_ontology:affects_constraint(border_legitimacy__economic_utility_reading, labor_market_adjustment_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__economic_utility_reading, skilled_migration_selection_system).

% DUAL FORMULATION NOTE:
% The border legitimacy kernel decomposes into three reading-specific constraints: economic_utility_reading, sovereignty_primary_reading, and freedom_of_movement_primary_reading. Each reading has its own ε (empirical uncertainty about net benefit, legitimacy grounding, freedom constraints), its own beneficiary/victim structure (varies by reading's metrics), and its own classification. They are linked via the kernel network: all three affect each other because changes in one reading's structural support (e.g., empirical demonstration that migration produces net negative benefit) alter the legitimacy landscape for the other readings. The kernel itself (border legitimacy authority structure) is the upstream node; the three readings are downstream. This story instantiates the economic utility reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__economic_utility_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
