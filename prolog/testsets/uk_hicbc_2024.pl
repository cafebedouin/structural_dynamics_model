% ============================================================================
% CONSTRAINT STORY: uk_hicbc_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_hicbc_2024, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_hicbc_2024
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic/tax_policy/welfare_redistribution
 *
 * SUMMARY:
 *   The UK High Income Child Benefit Charge (HICBC), introduced in January
 *   2013, represents a hybrid extraction-coordination mechanism that reclaims
 *   Child Benefit payments from families where at least one partner earns
 *   above £60,000 annually. The constraint operates through a progressive
 *   clawback: 1% of Child Benefit for every £100 of income above the
 *   threshold, reaching full clawback at £50,000 above threshold (roughly
 *   £80,000+ household income). This creates an effective marginal tax rate
 *   of 60%+ when combined with income tax and National Insurance
 *   contributions. The policy sits at a critical fault line between three
 *   narratives: (1) redistributive fairness — ensuring high-income families
 *   do not benefit from universal payments; (2) work incentive destruction —
 *   creating perverse marginal rates that suppress labor supply; (3)
 *   administrative theater — means-testing through self-assessment with
 *   incomplete enforcement and widespread accounting optimization. The
 *   constraint's classification depends entirely on which agent's perspective
 *   is privileged: the Treasury sees coordination (revenue for
 *   redistribution), the high-earner sees extraction (marginal rate trap),
 *   the low-to-middle income family sees mixed coordination and externality
 *   cost, the administrative system sees degraded verification (Piton), and
 *   the analytical observer risks naturalizing a 2013 political choice as
 *   inevitable fiscal architecture.
 *
 * KEY AGENTS:
 *   - HM Treasury / Exchequer (institutional/arbitrage): Primary beneficiary — claws back approximately £2 billion annually; has full exit optionality to adjust or repeal
 *   - High-income earning parents (moderate/constrained): Primary victims — face 60%+ effective marginal rates; constrained exit options (relocation costly, income reduction sacrifices opportunities)
 *   - Low-to-middle income families (moderate/mobile): Secondary beneficiaries/victims — preserve Child Benefit universality but bear efficiency costs of reduced high-earner labor supply and competitive wage effects
 *   - Self-employed and professional workers (powerful/arbitrage): Secondary beneficiaries of avoidance mechanisms — pension contributions, investment schemes, income timing create legal exits from clawback
 *   - HMRC and Payroll Administration (institutional/arbitrage): Institutional enforcer — means-testing mechanism is substantially performative; enforcement incomplete relative to nominal rates
 *   - Policy Reform Coalition (organized/constrained): Cross-party acknowledgment of perverse incentives; Universal Credit integration phases in relief; sunset trajectory embedded in welfare consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_hicbc_2024, 0.52).
domain_priors:suppression_score(uk_hicbc_2024, 0.68).
domain_priors:theater_ratio(uk_hicbc_2024, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_hicbc_2024, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_hicbc_2024, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_hicbc_2024, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_hicbc_2024, tangled_rope).
narrative_ontology:human_readable(uk_hicbc_2024, "UK High Income Child Benefit Charge (HICBC)").
narrative_ontology:topic_domain(uk_hicbc_2024, "economic/tax_policy/welfare_redistribution").

domain_priors:requires_active_enforcement(uk_hicbc_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_hicbc_2024, exchequer_revenue).
narrative_ontology:constraint_beneficiary(uk_hicbc_2024, low_to_middle_income_families).
narrative_ontology:constraint_victim(uk_hicbc_2024, high_income_families).
narrative_ontology:constraint_victim(uk_hicbc_2024, work_incentive_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-EARNING PARENT (SNARE) — Constrained by UK residency and employment. Faces effective marginal tax rates exceeding 60% (combination of income tax, NI, and HICBC clawback). Cannot arbitrage away from clawback without relocation or income reduction. The constraint extracts through suppression of labor supply incentive and creates perverse incentives to avoid crossing the £60k threshold.
constraint_indexing:constraint_classification(uk_hicbc_2024, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: HM TREASURY (ROPE) — Primary beneficiary. Clawback mechanism achieves redistribution goals through tax collection infrastructure. Net beneficiary with high exit optionality (can modify thresholds, adjust rates, or repeal). Experiences the constraint as coordination of revenue targets with welfare objectives. Theater is low from this perspective — administrative mechanism is straightforward.
constraint_indexing:constraint_classification(uk_hicbc_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LOW-TO-MIDDLE INCOME FAMILIES (TANGLED ROPE) — Benefit from preserved Child Benefit universality and exchequer revenue that funds public services. Also bear costs: labor supply distortion reduces overall tax base, and high-earner exit/suppression may reduce wages in competitive markets. Mobile exit exists (career timing, relocation) but at moderate cost. Mixed coordination (preserves benefit) and extraction (bears efficiency cost).
constraint_indexing:constraint_classification(uk_hicbc_2024, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYMENT AND PAYROLL ADMINISTRATION (PITON) — The means-testing mechanism is substantially performative. Self-assessment clawback requires declaration of partner's income; enforcement is incomplete. Many high-earning families avoid or minimize clawback through accounting optimization (pension contributions, Enterprise Investment Scheme) without formal violation. Theater ratio is high — administrative ritual of means-testing obscures that actual extraction is lower than nominal rates. Mechanism persists through institutional inertia despite degraded verification function.
constraint_indexing:constraint_classification(uk_hicbc_2024, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-ORIENTED POLICY COALITION (SCAFFOLD) — Cross-party agreement exists that HICBC creates perverse work incentives. Universal credit integration and threshold adjustments represent temporary coordination with a sunset trajectory. High-earner exemption from means-testing phases in as welfare consolidation progresses. Low effective extraction because coalition has agency and sees exit path through welfare reform; this constraint's life is limited by design.
constraint_indexing:constraint_classification(uk_hicbc_2024, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational perspective, progressive taxation and means-testing represent immutable features of any welfare state seeking to balance revenue with incentives. Some form of clawback is inherent to fiscal architecture. However, the base properties contradict mountain classification — this is a contingent 2013-onwards policy choice, not a law of nature. The mountain perspective risks naturalizing a political choice as inevitable.
constraint_indexing:constraint_classification(uk_hicbc_2024, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_hicbc_2024_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_hicbc_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_hicbc_2024, TR),
    TR >= 0.70.

:- end_tests(uk_hicbc_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The clawback achieves £2 billion annual revenue, representing genuine extraction from high-income families. However, the effective extraction is suppressed by accounting optimization (estimated 25-40% avoidance through tax-efficient arrangements), and the nominal rate exceeds behavioral extraction. The value reflects that the mechanism is neither pure coordination nor pure predation — it combines legitimate redistributive intent with poorly-calibrated marginal rates. Suppression (0.68): High. Multiple barriers prevent exit: (1) UK residency requirement for Child Benefit eligibility; (2) Relocation costs exceed tax savings for most families; (3) Income reduction sacrifices genuine earning opportunities; (4) Accounting optimization requires specialist knowledge and cost. Suppression is not total because arbitrage exists, but it is substantially higher than for a typical tax mechanism. Theater ratio (0.55): Moderate. The mechanism exhibits theater in two forms: (a) means-testing ritual — self-assessment clawback with incomplete verification and widespread legal avoidance; (b) universality claim — Child Benefit is nominally 'universal' but is means-tested for a substantial cohort. The theater has increased since 2013 as accounting optimization mechanisms have matured. Theater ratio is lower than a pure Piton (which would show ≥0.70) because the administrative function, while degraded, is not primarily performative — the clawback genuinely collects revenue.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival divide separates institutional actors (Treasury, HMRC, Reform Coalition) from individual agents (high-earner, family). Institutional actors experience the constraint as modifiable coordination: they can adjust thresholds, rates, or integration pathways. Individual agents experience it as fixed extraction: their marginal rate is determined by the policy, and their exit options (relocation, income reduction) are costly. Within individual agents, a secondary gap separates direct victims (high-earners) from indirect beneficiaries (low-to-middle income families). The high-earner sees a Snare because they experience only costs; the low-income family sees Tangled Rope because they experience both benefits (preserved universal benefit) and costs (reduced wage competition, efficiency loss). The analytical observer's Mountain perspective is a false summit: it naturalizes 2013 policy design as a law of fiscal gravity, obscuring that different threshold choices (£50k, £70k, no clawback) are all feasible and have been chosen in other jurisdictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's power level, exit options, and beneficiary/victim status. Treasury (institutional/arbitrage) experiences low d (~0.10) despite collecting revenue, because arbitrage exit and beneficiary status produce negative f(d) — they experience the constraint as easy to modify, shifting from extraction target to coordinator. High-earners (moderate/constrained) experience high d (~0.75) — victim status plus constrained exit produce high f(d), experiencing maximum extraction. Low-to-middle income families (moderate/mobile) experience moderate d (~0.50) — mixed beneficiary/victim status plus mobile exit options produce neutral f(d) around 0.65. Payroll administrators (institutional/arbitrage) experience low d despite enforcement role, because they are beneficiaries of the routing infrastructure. The directionality derivation encodes why the same policy is experienced as coordination by one agent and extraction by another: it is the combination of power level, exit capacity, and structural benefit/cost flow that determines the experienced extractiveness, not the policy mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: NO. The constraint exhibits genuine ambiguity that cannot be resolved by structural data alone — it is a matter of empirical and normative choice. (1) EMPIRICAL UNCERTAINTY: The effective extractiveness depends on labor supply elasticity (omega_labor_supply_elasticity) and avoidance prevalence (omega_accounting_avoidance_prevalence). If behavioral response is large (elasticity > 0.3), the constraint is primarily Snare (extraction-driven). If avoidance is widespread (>40%), the constraint is primarily Piton (performative). Current data is insufficient to distinguish. (2) NORMATIVE UNCERTAINTY: Whether the constraint is 'fair' depends on distributional principle (omega_threshold_distributional_cliff). If the £60k threshold reflects a principled definition of 'high income' for welfare distribution, the Tangled Rope classification dominates: mixed coordination (targeting) and extraction (clawback). If it is arbitrary political choice, the Snare classification dominates: pure extraction with universality theater. (3) DESIGN INTENT: The policy's architects (2013 Coalition government) intended Tangled Rope: coordination of redistribution with work incentives. The outcome (high marginal rates, widespread avoidance) has shifted the experienced constraint toward Snare and Piton. No single classification captures both intent and outcome. The presheaf over observation site (multi-perspective view) is more informative than forcing a single type. Until labor supply response and avoidance prevalence are quantified, mandatrophy remains unresolved. Recommend: empirical measurement of omega_labor_supply_elasticity and omega_accounting_avoidance_prevalence; if both are resolved, re-classify to dominant type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity,
    'What is the actual behavioral response to the 60.25% effective marginal rate created by HICBC? Do high-earners reduce hours, exit the workforce, or relocate?',
    'Longitudinal labor supply data; tax microdata linking income jumps to benefit clawback; international comparison with similar thresholds (France, Germany effective rates)',
    'If elasticity > 0.3: constraint is primarily extraction-driven (Snare classification strengthens). If elasticity < 0.1: constraint is primarily performative (Piton classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity, empirical, 'Behavioral response to effective marginal tax rate from HICBC').

omega_variable(
    accounting_avoidance_prevalence,
    'What fraction of affected families successfully avoid or minimize clawback through pension contributions, investment schemes, or other tax-efficient arrangements?',
    'Tax compliance data; audit rates on self-assessment returns; cross-reference benefit records with declared income; comparative analysis pre/post introduction',
    'If > 40% avoidance: theater ratio should be higher (0.65+), suggesting Piton as more accurate primary classification. If < 15% avoidance: enforcement is tighter than apparent, shifting toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accounting_avoidance_prevalence, empirical, 'Prevalence of tax-efficient avoidance of HICBC').

omega_variable(
    threshold_distributional_cliff,
    'Is the £60k threshold arbitrary political choice or justified by distributional principle? Does the cliff create identifiable cohorts of near-threshold avoiders?',
    'Income distribution analysis around threshold; regional clustering of self-employed income below £60k; survey data on work decisions relative to threshold',
    'If cliff is arbitrary: delegitimizes the constraint (Snare interpretation dominates). If cliff aligns with distributional principle: legitimacy increases (Tangled Rope interpretation strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_distributional_cliff, conceptual, 'Whether £60k threshold is arbitrary or principled').

omega_variable(
    universal_benefit_rationale,
    'Given the clawback, is Child Benefit meaningfully ''universal'' or is the universality claim performative?',
    'Eligibility and clawback data by income distribution; comparative analysis with explicitly targeted benefits; legislative intent analysis from 2013 reform documentation',
    'If universality is mostly intact: rope classification (coordination function) strengthens. If universality is mostly nominal: snare classification (extraction with universality theater) strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_benefit_rationale, conceptual, 'Whether universal benefit claim is substantive or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_hicbc_2024, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hicbc_tr_t0, uk_hicbc_2024, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hicbc_tr_t6, uk_hicbc_2024, theater_ratio, 6, 0.48).
narrative_ontology:measurement(hicbc_tr_t11, uk_hicbc_2024, theater_ratio, 11, 0.55).

% Extraction over time
narrative_ontology:measurement(hicbc_be_t0, uk_hicbc_2024, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hicbc_be_t6, uk_hicbc_2024, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(hicbc_be_t11, uk_hicbc_2024, base_extractiveness, 11, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_hicbc_2024, resource_allocation).
narrative_ontology:affects_constraint(uk_hicbc_2024, universal_child_benefit_universality).
narrative_ontology:affects_constraint(uk_hicbc_2024, progressive_taxation_fairness).
narrative_ontology:affects_constraint(uk_hicbc_2024, uk_welfare_state_efficiency).

% DUAL FORMULATION NOTE:
% HICBC is downstream of two distinct policy constraints: (1) universal_child_benefit_universality (ε ≈ 0.15, Mountain-like): the principle that Child Benefit should be paid to all families regardless of income, from which HICBC represents a degradation. (2) progressive_taxation_fairness (ε ≈ 0.35, Tangled Rope): the principle that high-income families should not benefit from universal welfare payments. HICBC was designed to reconcile these two upstream constraints, but created a novel constraint (high marginal rates) in the process. HICBC affects both parent constraints: it partially negates universality and partially enforces progressivity, creating tension rather than resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_hicbc_2024, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
