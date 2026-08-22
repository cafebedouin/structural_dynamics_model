% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: GDP Growth Rate as Legitimacy Anchor
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint story captures the quantitative growth reading of the
 *   performance_legitimacy kernel: the state's legitimacy rests on
 *   maintaining GDP growth rates that demonstrate continued economic
 *   expansion and job creation. The reading emerged from the 1990s reform
 *   consensus, was stress-tested in the 2008 stimulus, and now operates as
 *   the primary constraint on credit allocation, land use, and official
 *   promotion. The arrangement coordinates massive resource mobilization
 *   (infrastructure, manufacturing capacity, export platforms) but extracts
 *   heavily from household consumption share, migrant labor protections,
 *   small private firms' credit access, and environmental sinks. The
 *   beneficiary structure is clear: the industrial-export complex (SOEs in
 *   steel, cement, machinery, electronics assembly) and local government
 *   officials whose career advancement is tied to hitting growth targets. The
 *   constraint requires active enforcement — credit guidance, land quotas,
 *   hukou restrictions, and information control — to sustain the investment
 *   share of GDP above 40% and suppress rebalancing toward consumption. The
 *   ε-invariance principle applies: this reading produces a different ε than
 *   the qualitative_development_reading because the referent (the standing
 *   investment-driven arrangement) is assessed by different lights — growth
 *   rate vs. structural quality.
 *
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
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "GDP Growth Rate as Legitimacy Anchor").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'd53e58c4-79c5-4c1f-8fc6-a93830eb8fba').
narrative_ontology:cs_kernel_codification('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', formalized).
narrative_ontology:cs_authority_grounding('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', extraction).
narrative_ontology:cs_interpretation_layer_present('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba').
narrative_ontology:cs_reading_relation('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_reading_relation('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', foundational, gdp_growth_rate_is_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(gdp_growth_rate_is_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', gdp_growth_rate_is_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', foundational, investment_driven_model_is_necessary_for_growth).
narrative_ontology:cs_axiom_status(investment_driven_model_is_necessary_for_growth, holdable).
narrative_ontology:cs_axiom_grounding('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', investment_driven_model_is_necessary_for_growth, empirically_contingent).
narrative_ontology:cs_axiom('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', secondary, export_dependency_is_acceptable_cost).
narrative_ontology:cs_axiom_status(export_dependency_is_acceptable_cost, holdable).
narrative_ontology:cs_axiom_grounding('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', export_dependency_is_acceptable_cost, instrumental).
narrative_ontology:cs_reference_frame('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', post_1989_performance_legitimacy_consensus).
narrative_ontology:cs_drift_state('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', post_2015_new_normal_pivot, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d53e58c4-79c5-4c1f-8fc6-a93830eb8fba', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_sector).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, migrant_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_private_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_absorptive_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large state-owned enterprises in steel, cement, machinery, chemicals, and electronics assembly. They receive directed credit at below-market rates, preferential land allocation, export tax rebates, and implicit guarantees. Their profits and investment scale are sustained by the growth-target system. They can arbitrage globally — shifting production, accessing foreign markets, and lobbying for policy continuity — but their domestic privilege depends on the constraint.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Prefectural and provincial officials whose promotion tournaments are explicitly keyed to GDP growth, fiscal revenue, and investment attraction. They administer the constraint: they allocate land, direct credit via local state-owned financing vehicles, suppress labor unrest, and enforce environmental selectivity. Their professional identity is fused to the growth metric — leaving the system means losing the only metric that validates their career. They benefit from the rents of land finance and patronage networks but are also trapped by the tournament.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary).

% Urban and rural households bearing the transfer: suppressed interest rates on savings (financial repression), high housing costs driven by land finance, limited social safety net (healthcare, pensions, unemployment) because fiscal resources flow to investment. They can exit marginally — reduce consumption, move to lower-tier cities, invest in property — but cannot escape the macroeconomic structure. Organized resistance appears as savings strikes, property protests, and demographic withdrawal (low fertility).
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_sector, payer,
    organized, biographical, constrained, national).

% Rural-to-urban migrants in export manufacturing and construction. They are coordinated into the growth model as low-cost labor but extracted from via hukou exclusion from urban public services, wage suppression, precarious contracts, and workplace injury without recourse. Their exit options are structurally blocked: returning to rural areas offers no viable livelihood; staying in cities offers no path to full citizenship. They are the most extracted-from group per capita.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, migrant_workers, payer,
    powerless, immediate, trapped, national).

% Private firms in services, light manufacturing, and tech. They face credit rationing (banks prioritize SOEs and LGFVs), higher borrowing costs, regulatory discretion, and land access barriers. They are coordinated by the market access the growth model creates but extracted from via the financial and regulatory subsidy to the industrial complex. Some exit by moving offshore, shifting to asset-light models, or capturing niche policy windows — but the structural bias remains.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_private_enterprises, payer,
    moderate, biographical, constrained, national).

% Air, water, and soil systems that absorb the pollution and waste intensity of the investment-export model. The constraint's operation treats environmental capacity as a free input — extraction without compensation or consent. No voice, no exit. Listed as a victim stakeholder (agent: false) to mark the structural extraction; the engine excludes non-agents from directionality derivation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_absorptive_capacity, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, environmental_absorptive_capacity).

% The top leadership (Politburo Standing Committee, State Council) that sets the national growth target, authorizes the enforcement apparatus, and manages the tension between this reading and sibling readings. They could change the constraint (and have signaled 'high-quality development' pivot) but face the mandatrophy trap: the beneficiary coalition they depend on for implementation is the same coalition that resists rebalancing. Their seat is analytical in the sense that they see the full structure, but they are also the ultimate agenda_setter.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_policy_leadership, agenda_setter,
    institutional, generational, analytical, national).

% Academic and policy economists (domestic and international) who analyze the constraint's operation, measure the growth-employment elasticity decay, document the overcapacity and debt dynamics, and propose rebalancing. They have analytical exit (can publish abroad, shift research focus) but their domestic influence is constrained by information control. They provide the corroboration for the founding_problem_status = contested.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, independent_economists, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes savings, land, labor, and credit at massive scale to build industrial capacity, infrastructure, and export platforms in a compressed timeframe — solving the late-developer's coordination problem of simultaneous heavy industrialization and urbanization without market-price signals.
% TRANSFER_FUNCTION: Moves real resources (capital, land, labor, environmental capacity) from household sector, migrant workers, small private firms, and ecological systems to the industrial-export complex and local government financing vehicles, via suppressed interest rates, undervalued land conversion, hukou-excluded labor, and directed credit.
% ABSENT_VOICES: Rural elderly left behind by migration (no healthcare/pension portability), children in pollution hotspots (no voice in zoning), future generations bearing ecological debt, private entrepreneurs in sectors the model starves. They are structurally excluded — the constraint's enforcement apparatus (hukou, credit guidance, information control) is designed to keep them out of the agenda.
% DISAPPEARANCE_RATIONALE: If the growth-target constraint vanished overnight, credit would reallocate toward consumption and services, land markets would liberalize, hukou would face immediate reform pressure, the industrial-export complex would face hard budget constraints, and local official promotion criteria would lose their primary metric. The political economy would reorganize around a different legitimacy anchor — likely contested between qualitative_development, techno_nationalist, and livelihood_security readings.
% FOUNDING_PROBLEM: Post-1989 legitimacy crisis: the party-state needed a performance-based mandate to replace revolutionary legitimacy. Rapid GDP growth delivering visible job creation and rising living standards was the available, measurable, and internationally legible metric. The investment-export model was the known technology to deliver it.
% FOUNDING_PROBLEM_CORROBORATION: The party-state (central leadership, state media) attests the problem is live — growth remains the foundation of stability. Independent economists (World Bank 'China 2030', LSE Growth Commission, domestic scholars like Yu Yongding, Huang Yiping) attest the problem is dead — growth-employment elasticity has collapsed, the model generates debt and overcapacity, and legitimacy now requires delivery on pollution, inequality, and elderly care. The qualitative_development_reading and livelihood_security_reading provide alternative framings from outside the beneficiary coalition.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.72) reflects the sustained gap between investment share of GDP (~43%) and household consumption share (~38%), a structural transfer from households to the industrial-export complex mediated by suppressed interest rates, undervalued currency, and directed credit. Suppression (0.68) captures the active enforcement apparatus: credit allocation discipline, land use control, migration restrictions, and narrative control that prevent exit from the growth model. Theater ratio (0.45) has risen as 'high-quality development' rhetoric overlays the unchanged investment-driven core — the coordination function (mobilization) is real but the stated justification (job creation per unit of growth) is weakening (employment elasticity declining). Accessibility collapse (0.62) is moderate: alternative development models exist (consumption-led, green, high-tech) but are structurally blocked by the constraint's enforcement. Resistance (0.58) is significant but fragmented: local debt crises, labor unrest, private sector credit starvation, and environmental pushback all contest the arrangement without coalescing into a regime-level alternative.
 *
 * PERSPECTIVAL GAP:
 *   From the local official's seat, the constraint is a rope — it coordinates resources they control and delivers the promotion metric they need. From the migrant worker's seat, it is a snare — they are coordinated into export zones but extracted from via hukou exclusion and wage suppression. From the industrial SOE seat, it is a scaffold — they know the model is transitional but extract maximum rent before transition. The engine computes this divergence from power/exit/role data; the claim (tangled_rope) reflects the aggregate structural reality: genuine coordination function (massive infrastructure/industrial capacity) + asymmetric extraction (household sector subsidizes the complex) + active enforcement required.
 *
 * DIRECTIONALITY LOGIC:
 *   Industrial-export complex and local officials are structural beneficiaries (d ≈ 0.15-0.25): they collect rents from directed credit, land discounts, and promotion systems tied to growth. Household sector, migrant workers, and small private enterprises are structural targets (d ≈ 0.75-0.85): they bear the costs of suppressed consumption, excluded social protections, and credit rationing. Environmental absorptive capacity is a non-agent victim — extraction without voice. The analytical observer (this reading's seat) sees the full structure (d = 0.5). Directionality derivation from beneficiary/victim + exit options: local officials have identity_locked exit (career fused to growth targets), migrant workers are trapped (hukou), private firms are constrained (credit access), SOEs have arbitrage (state backing).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1989 legitimacy through delivery of growth and jobs) was live in the 1990s and 2000s. By the 2010s, the growth-employment coupling had weakened (elasticity falling), the investment model generated overcapacity and debt, and the 'high-quality development' pivot acknowledged the founding problem's shift. Yet the constraint persists because the beneficiary coalition (industrial complex + local officials) captures the enforcement apparatus. This is classic mandatrophy: the arrangement's mandate (growth = legitimacy) has outlived its function (growth no longer reliably delivers broad-based employment gains), but the constraint remains because the beneficiaries are the agenda_setters. The six_questions.founding_problem_status = 'contested' captures this: the party-state claims the problem is live; independent economists and the qualitative_development_reading claim it is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the quantitative growth reading a distinct constraint from the qualitative development reading, or the same constraint measured differently?',
    'Test ε-invariance: if evaluating the same policy arrangement (e.g., credit allocation to steel vs. semiconductors) under ''growth rate'' vs. ''high-quality development'' observables yields materially different ε, they are different constraints. The kernel decomposition already treats them as separate stories.',
    'If ε varies by observable, the kernel splits into multiple constraint stories linked by network.affects_constraints. If ε is invariant, the kernel is a single constraint with perspectival disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the performance_legitimacy kernel decomposes into multiple ε-invariant constraints or one constraint with contested readings').

omega_variable(
    growth_employment_coupling,
    'Does GDP growth at current margins still produce meaningful employment gains, or has the coupling decoupled?',
    'Empirical elasticity estimates: employment growth per 1% GDP growth over rolling 5-year windows. If elasticity has fallen toward zero, the ''job creation'' justification is structurally hollow.',
    'If decoupled, the constraint''s coordination function (job creation) is fictive; extraction becomes the primary operation, shifting classification toward snare. If coupled, the coordination function remains real, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_employment_coupling, empirical, 'Whether the stated coordination function (job creation via growth) still operates at current margins').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (censorship, hukou, credit control) or internalized (career incentives, professional identity fused to growth targets)?',
    'Post-exit trajectory study: do officials who leave the system still enforce growth logic in new roles? If suppression persists after structural barriers are removed, internalized component is significant.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them. This affects χ for identity_locked agents and the classification boundary between tangled_rope and snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for local officials and industrial managers').

omega_variable(
    environmental_absorptive_capacity_as_victim,
    'Should environmental absorptive capacity be treated as a victim stakeholder when it lacks agency and voice?',
    'Analytical consistency check: the constraint''s operation extracts from ecological systems without consent or compensation. If the framework admits non-agent victims, it belongs in victims[]. If not, it is a vindicated proposition (the constraint vindicates ''growth outweighs ecology'').',
    'Classification may shift if a non-agent victim changes the structural extraction profile. The schema excludes non-agents from directionality derivation, so the formal χ computation is unaffected; the signal is analytical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(environmental_absorptive_capacity_as_victim, conceptual, 'Whether non-agent ecological systems belong in victims[] or vindicated_propositions[]').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1990, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(perf_tr_t2008, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2008, 0.33).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t1990, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(perf_be_t2008, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1990, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(perf_su_t2008, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, local_government_debt_overhang).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, soe_credit_allocation_priority).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, hukou_migration_restriction).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, interest_rate_liberalization_delay).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four constraint stories because each reading evaluates the standing arrangement (investment-driven, export-dependent growth model) by a different observable and produces a different ε. This reading (quantitative_growth) yields ε ≈ 0.72; qualitative_development yields lower ε on the same arrangement (it sees the arrangement as failing its own metric); techno_nationalist yields variable ε depending on strategic sector; livelihood_security yields highest ε (the arrangement extracts most from daily life). All four stories link to each other via affects_constraints. The upstream story (this one) influences downstream stories because the growth-rate constraint's resource absorption (credit, land, official attention) structurally limits what the other readings can achieve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, institutional, 0.2).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, organized, 0.35).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
