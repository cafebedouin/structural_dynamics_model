% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Livelihood-Security Reading of Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   Within the performance-legitimacy settlement of a state-capitalist
 *   developmental regime, this story instantiates the livelihood-security
 *   reading: the ruling center's claim to govern rests on delivering
 *   tangible, directly experienced improvements in daily life — employment,
 *   healthcare, education, elderly care. As a constraint, the reading binds
 *   fiscal allocation and cadre incentives: service delivery and the social
 *   safety net become the primary claims on public resources, consumption
 *   support takes precedence over investment, and redistribution machinery
 *   strengthens. The arrangement coordinates genuine welfare provision while
 *   imposing concentrated costs on capital-intensive industrial expansion and
 *   on local governments whose fiscal models were built around infrastructure
 *   investment. This file is ONE READING of the contested
 *   performance_legitimacy kernel; the quantitative-growth,
 *   qualitative-development, and techno-nationalist readings are separate
 *   constraints with their own epsilon values, beneficiary structures, and
 *   classifications, linked through network.affects_constraints. Per the
 *   epsilon-referent rule, extractiveness here is authored for THIS
 *   arrangement as this reading holds it — not for the growth-first
 *   arrangement rivals would restore. KEY AGENTS (by structural
 *   relationship): - central_planning_authority: Agenda setter
 *   (institutional/arbitrage) — owns plan priorities and cadre evaluation
 *   weights - urban_households: Primary beneficiary (moderate/constrained) —
 *   experiences delivered services directly - rural_pensioners: Beneficiary
 *   (powerless/trapped) — transfer-dependent, minimal effective voice -
 *   service_sector_providers: Beneficiary (organized/constrained) — budgets
 *   expand with the mandate - capital_intensive_industrial_firms: Primary
 *   payer (powerful/constrained) — loses credit and procurement priority -
 *   land_finance_local_governments: Payer with residual beneficiary position
 *   (institutional/trapped) — funds mandates, receives transfers -
 *   strategic_industry_ministries: Excluded (institutional/constrained) —
 *   investment-first faction outside the operative coalition -
 *   fiscal_policy_analysts: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.48).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.55).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Livelihood-Security Reading of Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '7482f64b-78c5-42a2-ab9a-6fe521caf56f').
narrative_ontology:cs_kernel_codification('7482f64b-78c5-42a2-ab9a-6fe521caf56f', formalized).
narrative_ontology:cs_authority_grounding('7482f64b-78c5-42a2-ab9a-6fe521caf56f', lineage).
narrative_ontology:cs_interpretation_layer_present('7482f64b-78c5-42a2-ab9a-6fe521caf56f').
narrative_ontology:cs_reading_relation('7482f64b-78c5-42a2-ab9a-6fe521caf56f', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('7482f64b-78c5-42a2-ab9a-6fe521caf56f', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('7482f64b-78c5-42a2-ab9a-6fe521caf56f', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('7482f64b-78c5-42a2-ab9a-6fe521caf56f', foundational, legitimacy_requires_experienced_welfare_gains).
narrative_ontology:cs_axiom_status(legitimacy_requires_experienced_welfare_gains, holdable).
narrative_ontology:cs_axiom_grounding('7482f64b-78c5-42a2-ab9a-6fe521caf56f', legitimacy_requires_experienced_welfare_gains, empirically_contingent).
narrative_ontology:cs_axiom('7482f64b-78c5-42a2-ab9a-6fe521caf56f', secondary, consumption_priority_over_capital_accumulation).
narrative_ontology:cs_axiom_status(consumption_priority_over_capital_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('7482f64b-78c5-42a2-ab9a-6fe521caf56f', consumption_priority_over_capital_accumulation, instrumental).
narrative_ontology:cs_reference_frame('7482f64b-78c5-42a2-ab9a-6fe521caf56f', lived_welfare_delivery_compact).
narrative_ontology:cs_drift_state('7482f64b-78c5-42a2-ab9a-6fe521caf56f', contemporary_slow_growth_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7482f64b-78c5-42a2-ab9a-6fe521caf56f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, rural_pensioners).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_providers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, land_finance_local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, land_finance_local_governments).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, experienced_welfare_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets five-year plan priorities, cadre evaluation weights, and budget envelopes. Decides how much fiscal space goes to services and consumption support versus industrial investment, and can reweight the balance — but reweighting away from livelihood delivery carries a legitimacy cost it cannot ignore.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_planning_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Experience the arrangement directly: employment programs, hospital access, school quality, housing-adjacent services. Their collective satisfaction is the currency the arrangement runs on, but their individual voice channels are limited and household registration and housing costs limit geographic mobility.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    moderate, biographical, constrained, national).

% Depend on transfer payments, village clinics, and expanded rural insurance pools for daily survival. They receive the arrangement's flows most acutely in cash terms but have the least capacity to complain effectively or move elsewhere; their welfare is a headline metric they cannot verify or contest.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, rural_pensioners, beneficiary,
    powerless, biographical, trapped, national).

% Public hospital systems, school networks, and elder-care operators whose budgets, staffing quotas, and capital construction expand under the mandate. They gain revenue and scale but remain dependent on public procurement and fee schedules they do not set; partial private-market exit exists for the top tier only.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_providers, beneficiary,
    organized, biographical, constrained, national).

% Heavy-industry conglomerates and capital-intensive manufacturers that previously enjoyed subsidized credit, cheap land, and procurement priority. Under the livelihood-first allocation they compete for scarcer capital and policy attention. Their assets are location- and sector-specific, so they cannot relocate or repurpose quickly; they resist through lobbying channels and by reframing projects as strategically essential.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms, payer,
    powerful, generational, constrained, continental).

% Provincial and municipal governments whose fiscal models were built on land sales and infrastructure investment. They must fund expanding service mandates from shrinking land-finance revenue while absorbing debt-service obligations on past projects. They simultaneously receive equalization transfers and earmarked welfare funds, making them net payers in allocation terms but residual recipients of the redistribution they carry out. They cannot exit their obligations or their territory.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, land_finance_local_governments, payer,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, land_finance_local_governments, beneficiary).

% Bureaucracies aligned with investment-first and strategic-industry priorities. They argue that crowding out capital formation undermines long-run capacity and security, and they retain formal standing in planning processes, but under the current evaluation hierarchy their claims rank below livelihood metrics. They are inside the state but outside the operative coalition.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, strategic_industry_ministries, excluded,
    institutional, generational, constrained, national).

% Independent economists, multilateral observers, and academic analysts who track whether service mandates are funded, whether delivery statistics track lived experience, and whether the reallocation is sustainable given local-government debt. They publish assessments none of the seated parties controls.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, fiscal_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns state fiscal capacity with mass welfare provision: pooling central and local resources to deliver employment, healthcare, education, and elder care at population scale, and giving the cadre system a common, legible evaluation target tied to conditions citizens directly experience.
% TRANSFER_FUNCTION: Moves fiscal resources from capital-intensive industrial expansion and local infrastructure investment toward household consumption support, service delivery budgets, and intergovernmental transfers earmarked for welfare provision.
% ABSENT_VOICES: Strategic-industry ministries and investment-first planners would object that diverting capital from industrial expansion erodes long-run productive capacity and security; they are marginalized within the current evaluation hierarchy rather than persuaded. Rural pensioners, though nominal beneficiaries, lack any seat that reports whether delivered services match their actual experience.
% DISAPPEARANCE_RATIONALE: If the livelihood-first mandate vanished overnight, budget envelopes and cadre evaluations would revert toward investment and industrial channels, service expansion would stall mid-buildout, and household expectations formed under the welfare promise would collide with retrenchment — forcing the regime to re-anchor legitimacy in one of the rival readings (headline growth, high-quality development, or techno-nationalist achievement) rather than simply continuing as before.
% FOUNDING_PROBLEM: The arrangement was built to solve the legitimacy gap that opens when headline growth slows: as sustained rapid GDP expansion became harder to deliver, the regime required a credibility basis anchored in welfare gains citizens directly experience rather than in aggregate statistics they cannot feel.
% FOUNDING_PROBLEM_CORROBORATION: Party organs attest the problem self-interestedly, so external corroboration is required and exists: independent economists and multilateral observers document the growth slowdown that motivated the pivot, and published analyses of cadre-evaluation reforms and budget composition shifts — from sources outside the benefiting parties — corroborate both that the problem arose and that the arrangement was constructed in response. No outside body attests that the arrangement SOLVES the problem; corroboration covers the genealogy, not the cure.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48: the constraint forces substantial reallocation with concentrated losses on identifiable payers, but its core function genuinely delivers welfare, which bounds how much of its operation reads as extraction rather than coordination. Suppression is 0.55 as a raw structural property — persistence depends on cadre evaluation weights, budget discipline, and credit guidance that foreclose alternative allocations; per the engine's rule this scalar is NOT scaled by power or scope, unlike extractiveness. Theater ratio is 0.31: hospitals are built and pensions are paid (the function is real), but delivery statistics feeding cadre promotions invite systematic embellishment, so roughly a third of measured activity is performative compliance. Accessibility collapse is 0.42 — alternative allocations do not fully collapse: industrial firms retain lobbying channels, local governments stretch mandates through hidden debt, and rival readings remain live within the same planning apparatus. Resistance is 0.55: ministry pushback, disguised infrastructure borrowing, and reframing campaigns are persistent but non-blocking. The measurement series run on ONE shared time grid (points 0,2,4,6,8,10,12) with all three tracked metrics authored at every point; trajectories are monotone rising, modeling consolidation of the reading — no cyclical dynamics are claimed, so no intermittent-reinforcement mechanism is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute divergent types from identical structural data. From capital_intensive_industrial_firms, the arrangement operates as enforced diversion of capital they once commanded, backed by an evaluation hierarchy they cannot vote on — extraction-dominant. From rural_pensioners and urban_households, the same structure operates as subsidy and provision — coordination-dominant with incidental cost. land_finance_local_governments straddle the divide: they administer the transfers (agenda-adjacent), fund them painfully (payer), and receive earmarked relief (residual beneficiary), so their computed position should sit mid-range. The agenda-setter seat experiences the arrangement as a deliberate, reversible trade-off it controls. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: rural_pensioners (trapped, powerless) sit nearest the full-beneficiary end because the constraint subsidizes them and they cannot arbitrage it; urban_households are slightly less subsidized-feeling because they also absorb indirect costs (fee schedules, local co-financing); service_sector_providers collect revenue but under administered prices. Victims derive high directionality: capital_intensive_industrial_firms combine full-target position with constrained exit (asset specificity amplifies effective extraction), and land_finance_local_governments combine payer costs with trapped exit, partially offset by their secondary beneficiary position. On the receipt surface: gain_flow is authored as 'diffuse' as an affirmative checked claim — the redistribution is deliberately designed to disperse gains across household and service seats, and re-reading every stakeholder situation confirms no single seat captures the flows (urban households, rural pensioners, and service providers each receive a distinct slice; none aggregates them). fixing_cost is authored as 'prohibitive': the agenda-setter could administratively reweight tomorrow, but removal would forfeit the legitimacy basis the arrangement exists to provide, and that cost exceeds the fiscal benefit of freeing investment channels — the cost-asymmetry that keeps the constraint held despite payer resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining legitimacy under slowing headline growth — is live and intensifying, so no mandatrophy declaration is made and no zombie flag should fire: founding_problem_status=live combined with disappearance_verdict=world_rearranges is the coherent cell. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure snare would erase the genuine welfare coordination that traps no one into dependency gratuitously and that beneficiaries would defend; reading it as pure rope would erase the concentrated, resisted losses borne by industrial capital and infrastructure-dependent local governments through the very same budget lines that fund the safety net. Both functions run through one enforcement apparatus — cadre evaluation plus fiscal discipline — which is the tangled-rope signature, not a mixture of two separate constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the performance_legitimacy kernel; if the quantitative_growth_reading governed instead, which structural features of this story invert?',
    'Observe which evaluation criteria dominate cadre promotion and budget-cycle outcomes over successive planning periods: livelihood-metric dominance sustains this reading; GDP-target dominance restores the growth reading''s beneficiary/victim structure (industry regains priority, household support thins).',
    'Under the growth reading, beneficiaries and victims swap positions for the industrial and household seats, and this story''s epsilon no longer describes the operative arrangement — the corpus would need the sibling file to govern classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which sibling reading of the performance-legitimacy kernel is operative is an open structural question, not settled by this file.').

omega_variable(
    local_fiscal_sustainability,
    'Can expanding service mandates actually be funded as land-finance revenue contracts, or does delivery quietly devolve unfunded costs onto households and future budgets?',
    'Track local-government consolidated debt, land-sale revenue, and earmarked-transfer adequacy against mandated service spending; audit whether service co-payments and informal charges rise as transfers lag.',
    'If mandates are systematically unfunded, the arrangement''s coordination function decays toward theatrical compliance (theater_ratio rises, piton pressure builds) and the payer burden silently shifts onto the nominal beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_fiscal_sustainability, empirical, 'Whether the welfare mandate is fiscally backed or nominally funded.').

omega_variable(
    reported_vs_experienced_delivery,
    'Do official delivery statistics (employment placements, clinic buildouts, insurance enrollment) track citizens'' lived experience of services, or does promotional reporting inflate the measured function?',
    'Independent household surveys and administrative cross-checks comparing reported delivery indicators against utilization and satisfaction data gathered outside the cadre-evaluation incentive structure.',
    'A widening report-experience gap raises the true theater_ratio above the authored 0.31 and pushes the arrangement toward piton dynamics (performance maintained, function atrophying); convergence supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reported_vs_experienced_delivery, empirical, 'Whether the constraint''s functional activity is as real as its reported activity.').

omega_variable(
    welfare_allegiance_link_strength,
    'The reading''s foundational axiom holds that citizen allegiance tracks directly experienced welfare gains; does the empirical link actually hold as growth slows and expectations ratchet?',
    'Longitudinal analysis correlating service-delivery improvements with expressed satisfaction, compliance, and unrest incidence at provincial granularity, controlling for expectation effects and comparison groups.',
    'If allegiance decouples from delivered welfare (unrest despite gains, indifference to improvements), the axiom is empirically overridden, the reading loses its grounding, and the kernel contest resolves toward a rival reading regardless of fiscal allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_allegiance_link_strength, empirical, 'Whether the causal premise binding welfare delivery to legitimacy survives contact with data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_legit_livelihood_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_legit_livelihood_tr_t2, performance_legitimacy__livelihood_security_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(perf_legit_livelihood_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(perf_legit_livelihood_tr_t6, performance_legitimacy__livelihood_security_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(perf_legit_livelihood_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(perf_legit_livelihood_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(perf_legit_livelihood_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.31).

% Extraction over time
narrative_ontology:measurement(perf_legit_livelihood_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(perf_legit_livelihood_be_t2, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2, 0.36).
narrative_ontology:measurement(perf_legit_livelihood_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(perf_legit_livelihood_be_t6, performance_legitimacy__livelihood_security_reading, base_extractiveness, 6, 0.41).
narrative_ontology:measurement(perf_legit_livelihood_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(perf_legit_livelihood_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(perf_legit_livelihood_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(perf_legit_livelihood_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_legit_livelihood_su_t2, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2, 0.44).
narrative_ontology:measurement(perf_legit_livelihood_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(perf_legit_livelihood_su_t6, performance_legitimacy__livelihood_security_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(perf_legit_livelihood_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(perf_legit_livelihood_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(perf_legit_livelihood_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'performance legitimacy' decomposes, per the epsilon-invariance principle, into four structurally distinct resource-allocation constraints — one per reading of the kernel. Each has its own epsilon, beneficiary/victim structure, and classification; measuring 'performance' by livelihood delivery versus GDP rate versus transformation quality versus strategic-industry milestone yields different constraints, not one constraint under different observables. This file is the livelihood-security member. The upstream members (quantitative_growth_reading historically dominant, higher empirical confidence in its operation) influence the downstream members because growth-era fiscal architecture is the baseline this reading reallocates; edges here record this reading's resource-pressure on its siblings, and each sibling file should carry the reciprocal edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
