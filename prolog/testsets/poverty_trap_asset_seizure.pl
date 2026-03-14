% ============================================================================
% CONSTRAINT STORY: poverty_trap_asset_seizure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_poverty_trap_asset_seizure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: poverty_trap_asset_seizure
 *   human_readable: Poverty Trap Asset Seizure via Debt and Fines
 *   domain: economic/legal/social
 *
 * SUMMARY:
 *   Asset seizure for debt and fines operates as a pure extraction mechanism
 *   targeting economically trapped populations. The constraint works by
 *   converting the last resources that could enable exit — housing,
 *   transportation, income — into debt collection payments. This creates a
 *   self-reinforcing trap: seizing a vehicle eliminates transportation to
 *   employment; seizing wages reduces the income available for living
 *   expenses, forcing additional borrowing; legal fines for failure to pay
 *   compound the debt spiral. The mechanism persists through formal
 *   institutional structures (courts, collection agencies) that treat it as
 *   coordination (debt recovery) when the structural outcome is pure
 *   extraction (poverty deepening). Theater ratio is relatively low (0.35)
 *   because the enforcement is genuine and consequential — unlike vestigial
 *   Piton constraints, asset seizure actually accomplishes its nominal
 *   purpose of transferring wealth. The constraint is therefore Snare, not
 *   Piton. But the analytical observer risks naturalizing this as an
 *   immutable feature of credit systems, when cross-national comparison
 *   reveals it as a contingent institutional choice with alternatives.
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victims (powerless/trapped) — face seizure of vehicles, homes, and wages with no legal exit options; suppression is structural (forced seizure via court order)
 *   - Children in Seized-Asset Households: Secondary victims (powerless/trapped) — inherit poverty trap through parental asset loss; educational and mobility opportunities directly reduced
 *   - Enforcement Agencies & Creditors: Primary beneficiaries (institutional/arbitrage) — capture wealth transfer with legal authority; can modify terms, forgive, or restructure at will; minimal suppression
 *   - Small-Scale Creditors: Mixed actor (moderate/constrained) — benefit from enforcement coordination but also harmed by borrower insolvency cycles that reduce repayment capacity
 *   - Asset Exemption Advocates: Organized challengers (organized/constrained) — building alternative pathways (homestead exemptions, bankruptcy discharge) that would sunset the current seizure regime
 *   - Analytical Observer: Civilizational view (analytical/analytical) — at risk of naturalizing asset seizure as inherent to credit, missing that it is a design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(poverty_trap_asset_seizure, 0.68).
domain_priors:suppression_score(poverty_trap_asset_seizure, 0.78).
domain_priors:theater_ratio(poverty_trap_asset_seizure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, extractiveness, 0.68).
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(poverty_trap_asset_seizure, snare).
narrative_ontology:human_readable(poverty_trap_asset_seizure, "Poverty Trap Asset Seizure via Debt and Fines").
narrative_ontology:topic_domain(poverty_trap_asset_seizure, "economic/legal/social").

domain_priors:requires_active_enforcement(poverty_trap_asset_seizure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, enforcement_agencies).
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, debt_collectors).
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, creditors).
narrative_ontology:constraint_victim(poverty_trap_asset_seizure, low_income_households).
narrative_ontology:constraint_victim(poverty_trap_asset_seizure, unbanked_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED HOUSEHOLD (SNARE) — Trapped by structural barriers: seizing vehicles eliminates transportation to work; seizing homes eliminates shelter; seizing wages creates debt spiral. No exit options; maximum suppression via legal enforcement. Experienced extraction is maximal — every asset that could enable escape is targeted.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHILDREN IN SEIZED HOUSEHOLDS (SNARE) — Inherited poverty trap. Parent's seized assets (housing, transportation, educational goods) directly impair child's opportunity set. Generational extraction — the constraint passes down through asset deprivation and diminished mobility.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT AGENCIES & CREDITORS (ROPE) — Perceive the mechanism as debt recovery coordination: legal framework ensures debtors cannot arbitrage (hide assets, flee jurisdiction). Net beneficiary with high structural power and exit arbitrage (can modify terms, forgive debt, or restructure obligations). Experiences the constraint as coordination that protects their interests.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SMALL CREDITORS (TANGLED ROPE) — Face genuine coordination problem (ensuring debt repayment requires mechanism to prevent debtor exit), but also contribute to extraction through debt-trap interest rates and wage garnishment fees. Constrained by larger debt-collection infrastructure; benefits from enforcement but also harmed by borrower insolvency cycles that reduce repayment capacity.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEBTOR PROTECTION MOVEMENTS (SCAFFOLD) — Organized agents (legal aid societies, bankruptcy reform advocates, homestead exemption proponents) see asset seizure as a temporary enforcement regime with a sunset: alternative mechanisms (income-based repayment, asset exemptions, bankruptcy discharge) create pathways to exit that reduce suppression over time. Constraint is enforceable but transitional.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEBTOR PRISON LEGACY (PITON) — Asset seizure persists as a formal mechanism through institutional inertia despite widespread recognition that it deepens poverty and reduces repayment capacity. The constraint maintains itself theatrically through legal ritual (court orders, foreclosure procedures, wage garnishment documents) while functional repayment actually declines as borrowers lose income-earning assets. Theater ratio reflects the gap between enforcement activity and actual debt recovery.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURALIZATION (MOUNTAIN) — From a global civilizational view, asset seizure appears as an immutable requirement of credit systems: 'without enforcement, credit cannot exist; without seizure threat, debtors cannot be deterred.' This perspective naturalizes what is actually a contingent institutional choice (seizure vs. alternative mechanisms like income-based repayment, bankruptcy discharge, or precautionary lending). The analytical view risks treating institutional arrangement as law of nature.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poverty_trap_asset_seizure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(poverty_trap_asset_seizure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poverty_trap_asset_seizure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(poverty_trap_asset_seizure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(poverty_trap_asset_seizure, TR),
    TR >= 0.70.

:- end_tests(poverty_trap_asset_seizure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism directly extracts wealth from trapped populations through forced asset transfer. The trajectory over time shows extractiveness rising (0.52 → 0.68) as enforcement intensifies and debt spirals deepen borrowers' vulnerability. Suppression (0.78): Very high. Structural barriers to exit are severe: court orders are legally enforceable; wage garnishment leaves minimal income for subsistence; asset seizure removes the tools for escaping poverty. Alternatives (informal economy, migration) are themselves illegal or prohibitively costly. Theater ratio (0.35): Low. This is not theatrical constraint — it is functional extraction. Enforcement proceedings are real; asset transfers are real; wealth redistribution occurs. The low theater distinguishes Snare from Piton. Claimed type: Snare. The constraint has a victim (low-income borrowers), high extraction (0.68), high suppression (0.78), and low alternative provision. It is pure extraction with minimal coordination function beyond 'enabling debt collection.' Beneficiaries (enforcement agencies, creditors) experience it as coordination/rope. The gap is maximal.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between the beneficiary's experience (Rope: 'we need enforcement to recover debts') and the victim's experience (Snare: 'seizure traps me in poverty'). The small-scale creditor occupies the middle position (Tangled Rope: genuine coordination problem combined with extraction through fees and interest). The organized debtor-protection movement sees a Scaffold — alternative mechanisms (exemptions, bankruptcy discharge, income-based repayment) are being built, and the current seizure regime has a sunset. The institutional view from enforcement agencies sees Rope (coordination mechanism). The piton perspective emerges if you track the constraint over centuries — historical debtor prisons are gone, but asset seizure persists with reduced functional effectiveness (high theater, low recovery rates), maintained through institutional inertia. The analytical observer risks Mountain (false naturalization), treating credit enforcement as immutable law rather than institutional design. The constraint resolves mandatrophy by showing that all types are legitimate perspectives: Snare for victims, Rope for beneficiaries, Tangled Rope for mixed-position actors, Scaffold for organized alternatives, Piton for the historical trajectory, and Mountain only if you mistake institutional choice for natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position. Powerless trapped victims have d ≈ 0.95 (full target of extraction): no exit options, no power to modify terms, no escape from seizure. Institutional beneficiaries with arbitrage options have d ≈ 0.05 (full beneficiary): they can restructure debt, forgive, or decline to seize at will. Moderate constrained creditors have d ≈ 0.55 (symmetric): they face genuine coordination problem (need enforcement to recover debt) but also contribute to extraction (fees, interest rates that deepened borrowers' insolvency). The engine derives these d values from the beneficiary/victim declarations and exit options, producing χ = ε × f(d) × σ(S). For powerless trapped agents at national scope, f(d) ≈ 1.42 (maximum power modifier), producing χ ≈ 0.68 × 1.42 × 1.0 = 0.96 (extreme extraction). For institutional arbitrage beneficiaries, f(d) ≈ -0.12 (negative modifier), producing χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (they perceive the constraint as beneficial, not extractive).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: Asset seizure for poverty-trap debt resolves mandatrophy by showing that Snare and alternative types are structurally valid from different positions. The false move is to ask 'which type is correct?' when the answer is 'all are correct from their respective positions.' The Snare classification is correct for victims: high extraction, high suppression, no coordination benefit to the target. The Rope classification is correct for beneficiaries: they experience the constraint as coordination that protects their interests. The Tangled Rope is correct for mixed-position agents: genuine coordination problem (debt repayment) combined with asymmetric extraction (fees, interest, bankruptcy outcomes). The Scaffold perspective is valid because alternative mechanisms (bankruptcy discharge, homestead exemptions, income-based repayment) are structurally real alternatives with sunset potential. The Piton perspective is valid historically: asset seizure evolved from debtor prisons through institutional transformation toward alternative mechanisms. The Mountain perspective is a false summit: treating institutional enforcement design as natural law. The mandatrophy is resolved by indexical classification — different observers see different types because they occupy different structural positions, and all perspectives are valid from their positions. The policy challenge is not 'what type is it really?' but 'whose position do we privilege in institutional design?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seizure_proportionality_threshold,
    'At what ratio of seized assets to outstanding debt does seizure cease to maximize repayment and instead trigger default cycles?',
    'Longitudinal tracking of post-seizure repayment rates by asset type; comparison of recovery outcomes under seizure vs. alternative enforcement (income garnishment, debt restructuring)',
    'If threshold is low (20-30% of debt value): current seizure practices are counterproductive to stated recovery goal. If threshold is high (80%+): seizure is functional enforcement mechanism, not pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seizure_proportionality_threshold, empirical, 'Threshold at which asset seizure becomes counterproductive to debt recovery').

omega_variable(
    asset_exemption_sufficiency,
    'Do homestead exemptions and vehicle exemptions in bankruptcy actually prevent the poverty trap, or do they merely delay it while allowing interest and fees to accumulate?',
    'Post-bankruptcy asset recovery tracking; measurement of exemption usage rates vs. actual poverty exit within 5 years',
    'If effective: exemptions represent functional sunset mechanism (Scaffold validates). If ineffective: exemptions are theatrical compliance without structural change (Piton validates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_exemption_sufficiency, empirical, 'Whether asset exemptions functionally prevent poverty traps or merely delay them').

omega_variable(
    wage_garnishment_debt_spiral_causality,
    'Does wage garnishment cause job loss (through employer response, commute failure, or child care disruption) or does job loss cause garnishment defaults?',
    'Propensity score matching on garnishment timing relative to employment status; survey of employers on garnishment-driven termination practices',
    'If causality is garnishment → job loss: extraction is active mechanism that deepens poverty trap. If causality is job loss → garnishment: seizure is reactive, not constitutive of trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_garnishment_debt_spiral_causality, empirical, 'Causal direction between wage garnishment and job loss').

omega_variable(
    alternative_credit_access_substitutability,
    'If asset seizure were eliminated, would alternative credit mechanisms (unsecured lending, income-based repayment, payday loans) emerge that replicate the poverty trap through different enforcement means?',
    'Cross-national comparison of credit markets in jurisdictions with and without seizure enforcement; analysis of substitution effects in payday lending and informal credit markets',
    'If substitution is high: constraint is structurally persistent despite regulatory form changes (true Snare). If substitution is low: constraint-specific design choices matter and Scaffold sunset is possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_credit_access_substitutability, conceptual, 'Whether eliminating asset seizure would be substituted by alternative poverty-trap mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(poverty_trap_asset_seizure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ptas_tr_t0, poverty_trap_asset_seizure, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ptas_tr_t10, poverty_trap_asset_seizure, theater_ratio, 10, 0.32).
narrative_ontology:measurement(ptas_tr_t20, poverty_trap_asset_seizure, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(ptas_be_t0, poverty_trap_asset_seizure, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ptas_be_t10, poverty_trap_asset_seizure, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(ptas_be_t20, poverty_trap_asset_seizure, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(poverty_trap_asset_seizure, enforcement_mechanism).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, payday_lending_cycle).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, wage_garnishment_employment_loss).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, child_poverty_inheritance).

% DUAL FORMULATION NOTE:
% Asset seizure via debt and fines decomposes into structurally distinct mechanisms: (1) wage garnishment (ongoing income extraction), (2) asset seizure (one-time capital extraction), (3) fines and court costs (enforcement overhead extraction). Each has different ε values and trajectories. This story focuses on the unified constraint as experienced by trapped households; decomposed stories track each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
