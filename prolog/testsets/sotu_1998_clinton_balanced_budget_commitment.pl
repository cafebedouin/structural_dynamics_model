% ============================================================================
% CONSTRAINT STORY: sotu_1998_clinton_balanced_budget_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1998_clinton_balanced_budget_commitment, []).

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
 *   constraint_id: sotu_1998_clinton_balanced_budget_commitment
 *   human_readable: Balanced Budget Requirement: Fiscal Discipline Lock (1998)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The balanced budget requirement announced in Clinton's 1998 State of the
 *   Union creates a structural constraint on federal spending through
 *   procedural gatekeeping: all new spending proposals must be
 *   deficit-neutral. The mechanism institutionalizes fiscal austerity by
 *   requiring that constituencies seeking new spending identify offsetting
 *   cuts or revenue, while defenders of existing spending face no symmetric
 *   offsetting burden. This creates an asymmetric ratchet where spending can
 *   decline but rarely increases without offsetting action. The constraint
 *   functions as both genuine coordination (solving bond market credibility
 *   problems) and extraction (suppressing new spending capacity). The theater
 *   ratio rises over the interval from 0.48 (when surpluses are genuine) to
 *   0.72 (when the rule persists despite deficits and becomes performative).
 *   The extractiveness rises initially as the constraint binds most heavily,
 *   then stabilizes as political resistance and macroeconomic disruptions
 *   force periodic relaxation.
 *
 * KEY AGENTS:
 *   - Creditors and Deficit Hawks: Primary beneficiaries (institutional/arbitrage) — benefit from institutionalized fiscal discipline and bond market credibility premium
 *   - New Spending Constituencies: Primary victims (powerless/trapped) — must find offsetting cuts or revenue for any new initiative; zero flexibility
 *   - Moderate Congressional Coalitions: Secondary actors (moderate/constrained) — constrained by credibility concerns but can exit through procedural override at political cost
 *   - Progressive Reform Movements: Organized opposition (organized/constrained) — developing sunset strategies and procedural workarounds
 *   - Federal Reserve and Macro Stabilizers: Institutional observer (institutional/arbitrage) — forced to carry stabilization through monetary policy alone
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible tension between coordination (credibility) and extraction (fiscal suppression)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1998_clinton_balanced_budget_commitment, 0.52).
domain_priors:suppression_score(sotu_1998_clinton_balanced_budget_commitment, 0.58).
domain_priors:theater_ratio(sotu_1998_clinton_balanced_budget_commitment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1998_clinton_balanced_budget_commitment, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1998_clinton_balanced_budget_commitment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1998_clinton_balanced_budget_commitment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1998_clinton_balanced_budget_commitment, tangled_rope).
narrative_ontology:human_readable(sotu_1998_clinton_balanced_budget_commitment, "Balanced Budget Requirement: Fiscal Discipline Lock (1998)").
narrative_ontology:topic_domain(sotu_1998_clinton_balanced_budget_commitment, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1998_clinton_balanced_budget_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_balanced_budget_commitment, creditors).
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_balanced_budget_commitment, deficit_hawks).
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_balanced_budget_commitment, bond_market_interests).
narrative_ontology:constraint_victim(sotu_1998_clinton_balanced_budget_commitment, new_spending_constituencies).
narrative_ontology:constraint_victim(sotu_1998_clinton_balanced_budget_commitment, social_programs_advocates).
narrative_ontology:constraint_victim(sotu_1998_clinton_balanced_budget_commitment, countercyclical_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW SPENDING CONSTITUENCIES (SNARE) — Trapped by the structural requirement that all new initiatives must be deficit-neutral. This creates an asymmetric bargaining position: constituencies seeking new spending must identify offsetting cuts or revenue increases, while those defending existing spending bear no offsetting burden. The constraint creates a ratchet: spending can be cut, but new spending requires double negotiation. Zero exit options — must work within the procedural gate.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE CONGRESSIONAL COALITIONS (TANGLED ROPE) — Can exit through legislative override or procedural waiver, but at significant political cost (credibility damage, bond market response). Constrained by fiscal credibility concerns and market discipline. Also benefit from the constraint's capacity to discipline their own party members — members seeking spending use the neutral-cost rule to resist internal demands. Mixed coordination-extraction experience.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITORS AND DEFICIT HAWKS (ROPE) — Primary beneficiaries. The balanced budget commitment coordinates a collective action solution to the credibility problem: markets will lend at lower rates if fiscal discipline is institutionalized. This is genuine coordination — the rule solves an information asymmetry between government and bond markets. Arbitrage exit: can always shift capital allocation if fiscal policy becomes undisciplined. Negative effective extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE REFORM MOVEMENT (SCAFFOLD) — Organized opposition sees the constraint as temporary and containable: Build Back Better, reconciliation-bill procedures, and sunset-based budget frameworks are developing exit strategies. The constraint has a structural sunset — once surpluses materialize and debt-to-GDP stabilizes, the rationale for the 100%-reserve rule weakens. Political pressure to loosen the constraint increases when fiscal space expands. Scaffold classification derives from the sunset logic and organized agency.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESERVE AND MACRO STABILIZERS (PITON) — The balanced budget rule is substantially performative from the stabilization perspective. Fiscal policy's countercyclical capacity — its primary macroeconomic function — is degraded by the constraint. The Federal Reserve must carry stabilization entirely through monetary policy, which has different transmission mechanisms and political constraints. The rule persists through institutional inertia (credibility commitments, electoral messaging) even as its macroeconomic cost accumulates. Theater ratio high: the commitment is about credibility theater, not optimal stabilization policy.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/analytical scope, the constraint exhibits both genuine coordination (bond market credibility) and real extraction (suppression of countercyclical policy). The tension between these functions is irreducible. Markets benefit from the credibility purchase, but the broader fiscal system loses stabilization capacity. The constraint is neither purely extractive nor purely coordinating — it is a hybrid that concentrates benefits among creditors while dispersing costs across constituencies and economic cycles.
constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1998_clinton_balanced_budget_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1998_clinton_balanced_budget_commitment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1998_clinton_balanced_budget_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1998_clinton_balanced_budget_commitment, TR),
    TR >= 0.70.

:- end_tests(sotu_1998_clinton_balanced_budget_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes real costs on spending constituencies but is neither absolute nor unopposed. Political workarounds (reconciliation bills, emergency spending, reclassification) provide partial exit. The extractiveness peaks at 0.52 during the 2000–2008 interval when surpluses exist but are locked to Social Security, then moderates as the constraint becomes less binding during deficits. Suppression (0.58): Moderate-high. The barrier to new spending is significant (offsetting requirement) but not insurmountable (legislative override, scoring changes, emergency exemptions). Suppression reflects political rather than material barriers — constituencies can organize and overcome the constraint at electoral cost. Theater ratio (0.64 at endpoint): Rising over time from 0.48 to 0.72. Initially (1998–2000) the constraint is substantive — actual surpluses exist and are reserved. By 2006–2010, post-9/11 deficits and Iraq War spending force repeated procedural exemptions, rendering the balanced budget rule increasingly performative. The rule persists as messaging and credibility theater even as it ceases to bind actual fiscal outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Creditors see pure coordination: the rule solves an information problem and benefits them through lower borrowing costs (Rope). Deficit hawks see a temporary victory that requires constant defense against political pressure (Tangled Rope at the institutional level, but experiencing it as Rope). New spending constituencies see a snare: the offsetting requirement creates an asymmetric burden with no exit. Moderate congressional coalitions see constrained but navigable barriers (Tangled Rope). The Federal Reserve sees a degraded stabilization mechanism (Piton). The analytical observer sees an irreducible hybrid: genuine credibility coordination + real spending suppression coexisting in the same mechanism. No single perspective resolves the tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (creditors, deficit hawks) have arbitrage exit options and benefit from the constraint's credibility purchase — their d value is low (~0.15–0.20), producing negative or minimal effective extraction f(d). The constraint subsidizes their bond purchases and market position. Victims (new spending constituencies) are powerless and trapped — their d value is high (~0.85–0.95), producing maximum experienced extractiveness f(d). They must bear the offsetting burden with no reciprocal requirement on existing spending. Moderate coalitions (congressional actors) are constrained but have legislative override capacity — their d value is mid-range (~0.55–0.65), producing moderate extracted value. The analytical observer recognizes both the genuine coordination function (credibility problem solved) and the asymmetric extraction mechanism (spending ratchet imposed). This mixed structure — coordination + extraction coexisting — defines tangled rope from the analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification splits cleanly along structural position: beneficiaries experience genuine coordination (Rope from their perspective) because the constraint solves a real problem (credibility) that benefits them. Victims experience extraction (Snare from their perspective) because the offsetting requirement asymmetrically constrains new spending. The analytical observer (Tangled Rope) sees both functions simultaneously — the constraint genuinely coordinates on credibility AND genuinely suppresses spending, these are not mutually exclusive. The mandatrophy is resolved by recognizing that a single constraint can have coordination and extraction functions that serve different constituencies. The beneficiary's rope and the victim's snare are not contradictory — they are the dual faces of the same asymmetric structure. The analytical classification (Tangled Rope) reflects this irreducible hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deficit_neutral_measurement_ambiguity,
    'How are counterfactual baseline spending and revenue dynamics measured for deficit-neutral scoring? Does the scoring capture dynamic revenue effects or use static scoring?',
    'Comparative analysis of proposed spending scoring under different baseline assumptions and macroeconomic scenarios. Longitudinal comparison of ''deficit-neutral'' proposals'' actual vs. predicted fiscal impact.',
    'If static scoring: constraint is more severe and extractive (high suppression of spending). If dynamic scoring with optimistic assumptions: constraint becomes theater (proposals pass despite real deficit impact). Extraction value could shift by ±0.15.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_neutral_measurement_ambiguity, empirical, 'Measurement methodology for deficit-neutral baseline and revenue effects').

omega_variable(
    countercyclical_cost_quantification,
    'What is the actual macroeconomic cost (foregone stabilization) of the balanced budget constraint during recession periods when countercyclical spending would be optimal?',
    'Counterfactual econometric modeling: compare actual recession-period fiscal responses with and without the constraint. DSGE model sensitivity analysis with and without balanced-budget rule.',
    'If cost is high (> 0.5% GDP loss per recession cycle): constraint''s extraction signature extends to the macroeconomic commons, shifting tangled rope toward snare. If cost is low: constraint is primarily a distributional (not allocational) mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countercyclical_cost_quantification, empirical, 'Macroeconomic stabilization cost of fiscal constraint').

omega_variable(
    credibility_alternative_mechanisms,
    'Could the same bond market credibility be purchased through alternative mechanisms (explicit inflation targeting, currency board, fiscal commission) without suppressing countercyclical capacity?',
    'International comparison of credibility-purchasing mechanisms and their fiscal flexibility tradeoffs. Cross-national econometric analysis of borrowing cost vs. fiscal rule stringency.',
    'If yes: constraint appears as an unnecessarily extractive choice rather than a necessary coordination mechanism. Reclassifies toward snare. If no: constraint is revealed as the minimal-cost way to solve a genuine credibility problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_alternative_mechanisms, conceptual, 'Whether alternative credibility mechanisms exist').

omega_variable(
    revenue_side_asymmetry,
    'Is the deficit-neutral requirement applied symmetrically to revenue increases and tax cuts, or does political capture make revenue-side proposals easier to exempt?',
    'Longitudinal audit of deficit-neutral scoring for revenue vs. spending proposals. Analysis of legislative exemptions and scoring convention changes applied to each category.',
    'If asymmetric (tax cuts easier to pass): constraint functions primarily as a spending suppression device, increasing snare characteristics. If symmetric: constraint is genuinely neutral and operates as rope for both sides.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_side_asymmetry, empirical, 'Symmetry of deficit-neutral requirement across revenue and spending').

omega_variable(
    surplus_reserve_fungibility,
    'Does the 100% Social Security reserve actually constrain spending, or is it politically fungible through reclassification and accounting changes?',
    'Historical analysis of reserve exemptions, reclassifications, and accounting adjustments. Comparison of enacted policies vs. announced reserve intent.',
    'If fungible: constraint is primarily theater, shifting classification toward piton. If binding: constraint genuinely suppresses competing spending categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surplus_reserve_fungibility, empirical, 'Binding nature of Social Security surplus reserve').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1998_clinton_balanced_budget_commitment, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bud_theater_1998, sotu_1998_clinton_balanced_budget_commitment, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bud_theater_2002, sotu_1998_clinton_balanced_budget_commitment, theater_ratio, 4, 0.58).
narrative_ontology:measurement(bud_theater_2006, sotu_1998_clinton_balanced_budget_commitment, theater_ratio, 8, 0.64).
narrative_ontology:measurement(bud_theater_2010, sotu_1998_clinton_balanced_budget_commitment, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(bud_extract_1998, sotu_1998_clinton_balanced_budget_commitment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bud_extract_2002, sotu_1998_clinton_balanced_budget_commitment, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(bud_extract_2006, sotu_1998_clinton_balanced_budget_commitment, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(bud_extract_2010, sotu_1998_clinton_balanced_budget_commitment, base_extractiveness, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1998_clinton_balanced_budget_commitment, resource_allocation).
narrative_ontology:affects_constraint(sotu_1998_clinton_balanced_budget_commitment, welfare_reform_1996).
narrative_ontology:affects_constraint(sotu_1998_clinton_balanced_budget_commitment, healthcare_cost_escalation).
narrative_ontology:affects_constraint(sotu_1998_clinton_balanced_budget_commitment, tax_policy_distributional_asymmetry).

% DUAL FORMULATION NOTE:
% The balanced budget constraint is a procedural/institutional story distinct from the underlying fiscal policy dynamics. Downstream constraints (welfare reform, healthcare escalation, tax asymmetry) exist within the fiscal space created by this constraint. The constraint shapes the feasibility space for all downstream fiscal initiatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1998_clinton_balanced_budget_commitment, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
