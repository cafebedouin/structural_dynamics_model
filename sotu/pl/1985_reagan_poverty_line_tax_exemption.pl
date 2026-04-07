% ============================================================================
% CONSTRAINT STORY: 1985_reagan_poverty_line_tax_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1985_reagan_poverty_line_tax_exemption, []).

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
 *   constraint_id: 1985_reagan_poverty_line_tax_exemption
 *   human_readable: 1985 Reagan Poverty Line Tax Exemption Policy
 *   domain: social_policy/fiscal_transfer
 *
 * SUMMARY:
 *   The 1985 Reagan poverty line tax exemption represents a structural shift
 *   in how federal policy addresses low-income workers. Rather than direct
 *   welfare transfers, the mechanism uses the tax code to exempt poverty-line
 *   earners from federal income tax liability while simultaneously increasing
 *   personal exemptions across the income distribution. The policy is framed
 *   as tax relief and fairness rather than redistribution, yet it functions
 *   as both coordination mechanism (for low-income workers) and revenue
 *   transfer mechanism (from higher earners). The constraint exhibits tangled
 *   rope structure: genuine coordination function (low-wage workers retain
 *   earned income without tax burden) coexists with asymmetric extraction
 *   (revenue burden transfers upward, and near-poverty workers face cliff
 *   effects). The theater ratio (0.58) reflects that the exemption reframes
 *   poverty assistance as tax fairness, creating performative distance from
 *   explicit welfare programs. Base extractiveness (0.35) is moderate because
 *   the revenue transfer is substantial but not maximal — the exemption
 *   coordinates real redistribution rather than pure extraction. Suppression
 *   (0.48) is moderate: low-income workers experience low suppression (they
 *   perceive genuine benefit), but workers just above the threshold
 *   experience high cliff-effect suppression.
 *
 * KEY AGENTS:
 *   - Low-income workers at/below poverty line: Primary beneficiary (powerless/trapped) — experience coordinate benefit of income retention without tax burden
 *   - Near-poverty workers (100-120% of poverty line): Secondary victim (moderate/constrained) — face cliff-effect extraction; marginal cost of crossing threshold is severe
 *   - Federal revenue base: Primary victim (powerful/mobile) — bears upward revenue transfer; extraction mechanism targets revenue system, not individual earners
 *   - Higher-income taxpayers: Secondary victim (powerful/mobile) — bear portion of transferred burden through tax-rate adjustment or base-broadening pressure
 *   - Anti-poverty coalition: Organized advocate (organized/mobile) — perceive policy as temporary redistribution mechanism with sunset dynamics as wage growth moves workers out of poverty
 *   - Welfare/HHS bureaucracy: Institutional actor (institutional/constrained) — experience policy as reframing welfare as tax relief, creating piton dynamics (performative function replaces direct assistance)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1985_reagan_poverty_line_tax_exemption, 0.35).
domain_priors:suppression_score(1985_reagan_poverty_line_tax_exemption, 0.48).
domain_priors:theater_ratio(1985_reagan_poverty_line_tax_exemption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1985_reagan_poverty_line_tax_exemption, extractiveness, 0.35).
narrative_ontology:constraint_metric(1985_reagan_poverty_line_tax_exemption, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1985_reagan_poverty_line_tax_exemption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1985_reagan_poverty_line_tax_exemption, tangled_rope).
narrative_ontology:human_readable(1985_reagan_poverty_line_tax_exemption, "1985 Reagan Poverty Line Tax Exemption Policy").
narrative_ontology:topic_domain(1985_reagan_poverty_line_tax_exemption, "social_policy/fiscal_transfer").

domain_priors:requires_active_enforcement(1985_reagan_poverty_line_tax_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1985_reagan_poverty_line_tax_exemption, low_income_workers_below_poverty_line).
narrative_ontology:constraint_beneficiary(1985_reagan_poverty_line_tax_exemption, earned_income_credit_expansion).
narrative_ontology:constraint_victim(1985_reagan_poverty_line_tax_exemption, federal_revenue_base).
narrative_ontology:constraint_victim(1985_reagan_poverty_line_tax_exemption, higher_income_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER (ROPE) — The worker below the poverty threshold experiences the exemption as pure coordination benefit: they solve the income retention problem through the exemption mechanism. They remain trapped in low-wage labor markets (cannot easily exit) but the exemption mechanism coordinates their material interest (keep earned income) with policy design. No suppression experienced from the mechanism itself — suppression comes from labor market, not from the tax code. This perspective perceives the constraint as Rope because the primary function is coordination of their income security, not extraction.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEAR-POVERTY WORKER (TANGLED ROPE) — Workers just above the exemption threshold (101-115% of poverty line) face an extraction mechanism: they pay federal income tax while near-equivalent workers below the line do not, creating a cliff effect. The constraint coordinates redistribution upward (toward higher earners) while extracting from the marginal worker. They are constrained (can theoretically reduce work hours to drop below the line, but cost is significant) and experience both benefit (policy attention to low-wage workers) and asymmetric extraction (cliff effect). The beneficiary for them is the higher-income population capturing the transferred burden.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-POVERTY COALITION (SCAFFOLD) — Organized anti-poverty advocates see the exemption as a temporary fix with a sunset clause built in: wage growth and economic expansion will move workers out of poverty, reducing the exemption's scope over time. The policy coordinates redistribution without explicitly reframing it as welfare — it appears as tax relief rather than transfers. From the coalition perspective, the constraint has low suppression (they can mobilize, propose alternatives) and the theater is functional (tax code changes are technically verifiable). However, the constraint depends on the poverty line remaining stable and inflation dynamics playing out as expected — external economic conditions will degrade the policy over time.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL REVENUE SYSTEM (SNARE) — The revenue base experiences the exemption as pure extraction: the mechanism transfers revenue burden away from low-income earners toward higher earners and corporations. The federal government's capacity to fund programs depends on the revenue base remaining stable or growing. As the exemption expands (due to inflation adjustment and poverty line changes), the extraction mechanism intensifies. The revenue system cannot exit (it must fund federal operations) and suppression is high (the deficit mechanisms — spending cuts or tax increases elsewhere — constrain alternatives). From the budgetary perspective, the exemption is extractive without substantial coordination benefit.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE BUREAUCRACY (PITON) — The welfare administration system (HHS, state social services) experiences the exemption as a degraded institutional function: the policy reframes welfare as tax relief rather than direct transfers, creating performative distance from explicit 'welfare' programs. The bureaucracy persists (poverty measurement, exemption administration, IRS coordination) but the primary function shifts from direct poverty relief to tax code management. Theater is high (the exemption appears as tax fairness rather than poverty program) and institutional inertia is strong (poverty measurement infrastructure persists even as explicit welfare programs contract). The constraint maintains itself through the tax code rather than through direct appropriations, reducing visibility of redistribution.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, the exemption appears as a natural correction for market economies: market wage distribution inevitably produces subsistence workers whose income tax capacity is minimal. The exemption reflects an economic law — below-poverty earners cannot sustain tax obligations without creating dependency — rather than a contingent policy choice. However, the structural data contradicts this mountain framing. Identifiable beneficiaries (low-income workers, those captured in earned-income credit expansion) exist. The exemption is not an immutable feature of economies; other countries use alternative mechanisms (earned income supplements, direct transfers, wage subsidies). The mountain classification signals a false summit: the naturalizing of a policy choice as economic necessity.
constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1985_reagan_poverty_line_tax_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1985_reagan_poverty_line_tax_exemption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1985_reagan_poverty_line_tax_exemption, TR),
    TR >= 0.70.

:- end_tests(1985_reagan_poverty_line_tax_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The exemption transfers genuine revenue burden upward in the income distribution, but the extraction is not severe because the mechanism has a primary coordination function (enabling low-wage workers to retain earned income). The trajectory shows rising extractiveness (0.20 → 0.35 over 6 years) as eligibility expands and the revenue burden accumulates. Suppression (0.48): Moderate. Low-income workers experience low suppression from the tax code itself (they have genuine option not to pay federal income tax), but suppression rises for near-poverty workers facing cliff effects. The overall suppression captures the average across the affected population. Theater ratio (0.58): Moderate-high. The exemption reframes poverty assistance as tax fairness rather than welfare, creating performative distance from direct transfers. The tax code mechanism obscures the redistribution — it appears as tax relief rather than transfers. As the policy matures, theater increases (0.45 → 0.58) because the framing-as-fairness becomes more institutionalized and the underlying redistribution becomes more obscured.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a classic split between beneficiary and victim perspectives. Low-income workers perceive Rope (coordination benefit dominates). The anti-poverty coalition perceives Scaffold (temporary mechanism with sunset as wage growth occurs). The welfare bureaucracy perceives Piton (institutional function degrades as direct programs contract). Near-poverty workers perceive Tangled Rope (mixed coordination for those below line, extraction for those above). The revenue system perceives Snare (pure extraction without compensation). The analytical observer risks perceiving Mountain (naturalizing the exemption as a law of market economies) — this is a false summit candidate because identifiable beneficiaries exist and alternative mechanisms are used in comparable economies. The perspectival gap is wide: from rope (low-income worker) to snare (revenue system), the same mechanism is experienced as both coordination and pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies dramatically by agent position. Low-income workers at the exemption threshold (beneficiaries with trapped exit) experience low d (high benefit, cannot exit despite wanting to) → negative χ (the constraint subsidizes them). Near-poverty workers (victims with constrained exit, above the threshold) experience high d (extraction, some cost to exit) → high χ (effective extraction). The federal revenue system (victim with mobile options, constrained by budget rules) experiences high d (pure extraction, significant cost to 'exit' through deficit reduction) → high χ. Higher-income taxpayers (victims with mobile options, arbitrage available) experience moderate d (can adjust tax strategy, but burden increases) → moderate χ. The anti-poverty coalition (organized beneficiary advocate, mobile) experiences low d (they benefit from policy visibility and can mobilize) → low/negative χ. The welfare bureaucracy (institutional victim/beneficiary hybrid, constrained) experiences moderate d (they maintain function but in degraded piton form, constrained by institutional budget). Each perspective's directionality reflects their structural position within the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that the classification depends entirely on the agent's structural position relative to the exemption mechanism. Low-income workers perceive genuine coordination (Rope) because the exemption mechanism solves their problem (retaining earned income). The revenue system perceives extraction (Snare) because the exemption mechanism transfers burden away from low earners toward higher earners or deficit funding. The near-poverty worker perceives hybrid mechanics (Tangled Rope) because they sit at the cliff boundary — some workers just below the threshold benefit from coordination, while workers just above the threshold bear asymmetric extraction. The anti-poverty coalition perceives sunset dynamics (Scaffold) because they see external economic conditions (wage growth, inflation) as external to the constraint but structurally important to its lifecycle. The analytical observer must resist the temptation to perceive Mountain (natural law of market economies) — the structural data reveals beneficiaries exist and alternative mechanisms are available, triggering false summit detection. The mandate does not collapse: all six classifications are correct perspectival readings of the same base metrics. The policy IS coordination for its intended beneficiaries, AND it IS extraction from the revenue base, AND it IS a temporary mechanism awaiting wage-growth exit, AND it IS a degraded welfare function relocated to the tax code. The perspectival multiplicity is the policy's actual structure, not a sign of measurement failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    poverty_line_measurement_stability,
    'Does the poverty line measure remain stable in real terms as inflation adjusts nominal thresholds, or does it lose definition across time?',
    'Historical analysis of poverty line threshold adequacy: comparison of actual living costs for poverty-line households across decades vs official threshold adjustments',
    'If stable: exemption scope contracts in real terms as wage growth and inflation occur (supporting scaffold framing). If unstable: exemption scope expands or contracts unpredictably, undermining policy predictability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(poverty_line_measurement_stability, empirical, 'Whether poverty line measurement provides stable target over time').

omega_variable(
    tax_cliff_behavioral_response,
    'Do workers just above the exemption threshold reduce work hours to fall below the line (claiming the exemption), or do labor market constraints prevent this response?',
    'Labor supply analysis: comparison of work-hour distributions and labor force participation for workers at 100-120% of poverty threshold vs broader low-wage population; identification of discontinuities at exemption boundary',
    'If substantial response: exemption creates perverse incentive structure (snare from broader economic perspective). If minimal response: labor market constraints dominate and workers cannot exercise exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_cliff_behavioral_response, empirical, 'Whether workers respond to exemption cliff by reducing work hours').

omega_variable(
    welfare_program_substitution_effect,
    'Does the tax exemption substitute for or complement direct welfare programs? Do exemptions reduce other poverty assistance, or do they stack?',
    'Program administration analysis: tracking of AFDC/TANF enrollment, food assistance, housing subsidies across pre- and post-exemption periods; identification of means-test interactions',
    'If substitution: exemption merely relocates poverty assistance to tax code, with minimal net benefit (piton framing confirmed). If complementary: exemption provides genuine addition to total support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_program_substitution_effect, empirical, 'Whether exemption substitutes for or complements other welfare programs').

omega_variable(
    revenue_transfer_incidence,
    'Who actually bears the revenue burden transferred by the exemption? Does it fall on higher-income taxpayers, corporate tax base, or lower-income workers through inflation/reduced services?',
    'Tax incidence analysis: dynamic scoring of budget impacts; comparison of tax rate changes and spending cuts across income distribution; identification of who loses service access',
    'If progressive incidence: exemption achieves redistribution toward low-income workers (rope/scaffold framing). If regressive incidence: burden falls back on low-income population through reduced services (snare framing confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_transfer_incidence, empirical, 'Who bears the revenue burden of the exemption').

omega_variable(
    dependency_rhetoric_vs_structure,
    'Does the exemption actually reduce ''welfare dependency'' (measured as behavioral change toward self-sufficiency), or does the framing as independence mask a form of dependency transfer?',
    'Longitudinal analysis of exemption recipients: employment trajectories, earnings growth, program usage over 5-10 years; comparison to control groups in alternative policy regimes',
    'If reduces dependency: policy achieves stated goal (rope/scaffold framing). If transfers dependency: policy merely relocates welfare to tax code while claiming independence (piton/snare framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dependency_rhetoric_vs_structure, conceptual, 'Whether exemption reduces welfare dependency or transfers it').

omega_variable(
    false_summit_naturalizing_policy_choice,
    'Is the exemption mechanism a natural law consequence of market economies, or a contingent policy choice presented as natural necessity?',
    'Comparative institutional analysis: identification of alternative mechanisms used in other market economies (earned income supplements, direct transfers, wage subsidies); documentation of deliberate policy choice in U.S. context',
    'If natural law: mountain classification is appropriate. If contingent choice: false summit detector should reclassify to tangled_rope (identifiable beneficiaries + extraction mechanism present).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalizing_policy_choice, conceptual, 'Whether exemption is natural law or naturalized policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1985_reagan_poverty_line_tax_exemption, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pov_tax_tr_t0, 1985_reagan_poverty_line_tax_exemption, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pov_tax_tr_t3, 1985_reagan_poverty_line_tax_exemption, theater_ratio, 3, 0.52).
narrative_ontology:measurement(pov_tax_tr_t6, 1985_reagan_poverty_line_tax_exemption, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(pov_tax_be_t0, 1985_reagan_poverty_line_tax_exemption, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(pov_tax_be_t3, 1985_reagan_poverty_line_tax_exemption, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(pov_tax_be_t6, 1985_reagan_poverty_line_tax_exemption, base_extractiveness, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1985_reagan_poverty_line_tax_exemption, resource_allocation).
narrative_ontology:affects_constraint(1985_reagan_poverty_line_tax_exemption, earned_income_credit_expansion).
narrative_ontology:affects_constraint(1985_reagan_poverty_line_tax_exemption, tax_bracket_compression_1980s).
narrative_ontology:affects_constraint(1985_reagan_poverty_line_tax_exemption, federal_deficit_accumulation_reagan_era).

% DUAL FORMULATION NOTE:
% The poverty line exemption is structurally linked to broader Reagan-era tax and spending dynamics. The exemption itself represents a redistribution mechanism that transfers revenue burden upward; it is downstream of the broader supply-side tax philosophy and upstream of earned-income credit expansion (which later codifies the exemption logic into explicit transfer payments). The constraint family includes: (1) the tax exemption (this story, ε=0.35, Tangled Rope from analytical perspective), (2) the EITC expansion (ε=0.20, Rope from low-income worker perspective), and (3) the deficit accumulation mechanism (ε=0.55, Snare from fiscal sustainability perspective). Each story has different base extractiveness and different classification from different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1985_reagan_poverty_line_tax_exemption, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
