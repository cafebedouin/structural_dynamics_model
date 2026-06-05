% ============================================================================
% CONSTRAINT STORY: freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_floor_reading, []).

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
 *   constraint_id: freedom_floor_reading
 *   human_readable: Income Support as Autonomy-Enabling Floor (Freedom Floor Reading)
 *   domain: political_economy/social_policy/labor_markets
 *
 * SUMMARY:
 *   Income support mechanisms (minimum income, unemployment insurance,
 *   guaranteed income programs) can be read as either autonomy-enabling tools
 *   that expand worker choice sets and reduce employer monopsony power, or as
 *   dependency-creating subsidies that enable wage suppression and perpetuate
 *   low-wage labor market equilibria. This constraint story instantiates the
 *   FREEDOM FLOOR READING — the reading in which income support functions
 *   primarily as an expansion of exit options and a reduction in monopsony
 *   extraction. From this perspective, workers gain the ability to refuse
 *   unacceptable wages because they have an alternative to destitution;
 *   employers lose extractive power because workers are no longer forced to
 *   accept any available wage; the state gains labor market stability. The
 *   mechanism is rope-like: it coordinates income smoothing across the tax
 *   base and coordinates labor supply with demand at higher wage equilibria.
 *   This reading is one of three possible readings of the contested kernel
 *   (income_support_commitment). The sibling readings —
 *   dependency_trap_reading and subsidy_capture_reading — would use the same
 *   structural data but emphasize different causal channels and long-term
 *   outcomes. The distinction between readings is not resolvable by adding
 *   more data points; it depends on theoretical framing of how income floors
 *   affect labor supply, bargaining power, and wage equilibrium.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary beneficiary (powerless→mobile transition via income floor) — gain expanded exit options, ability to refuse exploitative wages, reduced desperation-driven labor supply
 *   - Working-Class Households: Primary beneficiary (moderate/constrained) — gain income smoothing, reduced vulnerability to job loss, improved household financial security
 *   - Monopsony Employers: Mixed beneficiary/victim (powerful/constrained) — lose extractive power through reduced wage suppression, gain coordination benefits through stable labor supply and reduced turnover
 *   - Labor Movement: Organized agent (organized/constrained) — sees income floor as scaffolding enabling higher bargaining power over time; expects sunset as labor organization strengthens
 *   - State Apparatus: Institutional beneficiary (institutional/arbitrage) — gains labor market equilibration, reduced social crisis costs, improved public health; pays via tax commitment
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent policy choice as immutable economic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_floor_reading, 0.32).
domain_priors:suppression_score(freedom_floor_reading, 0.22).
domain_priors:theater_ratio(freedom_floor_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_floor_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(freedom_floor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(freedom_floor_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_floor_reading, rope).
narrative_ontology:human_readable(freedom_floor_reading, "Income Support as Autonomy-Enabling Floor (Freedom Floor Reading)").
narrative_ontology:topic_domain(freedom_floor_reading, "political_economy/social_policy/labor_markets").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(freedom_floor_reading, wage_floor_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — Income floor expands exit options from trapped (no alternative to exploitative wage) to mobile (can refuse unacceptable terms). Worker experiences the constraint as genuine coordination mechanism: income support enables labor market participation on terms closer to voluntary choice. Low experienced extraction because the mechanism directly enables this agent's mobility.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS HOUSEHOLD (ROPE) — Experiences income floor as risk-pooling coordination: reduces catastrophic income loss during job transitions, illness, or sectoral decline. Coordination function is genuine — the mechanism solves the collective action problem of income volatility. Suppression is low because the mechanism operates with consent and transparent terms. Worker pays taxes but retains agency.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONOPSONY EMPLOYER (TANGLED ROPE) — Income floor reduces monopsony extractiveness by raising the reservation wage. Employer faces both loss (cannot extract as much labor value) and coordination benefit (more stable labor supply, reduced turnover costs, access to workers with reduced desperation). Mixed experience: extraction cost mixed with genuine operational benefit. Extractiveness is moderate because the mechanism constrains predatory wage-setting while enabling coordination.
constraint_indexing:constraint_classification(freedom_floor_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT (SCAFFOLD) — Income floor as temporary wage support during labor market transition. Organized labor sees the constraint as a scaffolding mechanism enabling worker mobility that should, ideally, enable workers to organize and demand higher wages, reducing long-term reliance on income support. Sunset clause rationale: as labor organization strengthens and sectoral investment increases, reliance on income floor should decline. Theater is moderate because the mechanism is transparent and results are measurable (employment rates, wage growth).
constraint_indexing:constraint_classification(freedom_floor_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE STATE (ROPE) — Income floor enables state to coordinate labor market equilibration without direct wage-setting intervention. State receives benefit: reduced social crisis, improved public health, lower criminal justice costs, higher productivity. State pays cost: tax revenue commitment. Net experience is coordination (rope) because the mechanism solves a genuine collective action problem — maintaining livable income floor requires coordinated action across tax base; market alone does not produce this equilibration.
constraint_indexing:constraint_classification(freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, income floor appears as an immutable requirement for preventing destitution and maintaining labor market participation. Below certain income thresholds, workers cannot participate in markets (malnutrition, homelessness, health collapse prevent work). The floor is not a policy choice but a structural necessity. However, this reading naturalizes what is actually a policy decision — the false summit detector will evaluate whether identifiable beneficiaries (workers gaining mobility, employers gaining stable labor) justify reclassification as a constructed constraint.
constraint_indexing:constraint_classification(freedom_floor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_floor_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The income floor generates moderate extraction costs (taxation, bureaucratic overhead, labor supply effects) but its primary function is coordination rather than extraction. Workers gain mobility; employers gain labor market stability. The mechanism does not require coercion — income support operates via transparent tax and transfer mechanisms that beneficiaries understand. Suppression (0.22): Low. While some suppression exists (taxation is compulsory, income floor recipients must meet eligibility criteria), it is far below the levels required for snare or high tangled_rope. Workers retain agency; the mechanism operates openly; exit from participation is theoretically possible (though costly). Theater ratio (0.35): Low-moderate. The income floor mechanism is relatively transparent: benefits are visible, costs are visible (though distributed across tax base), eligibility criteria are explicit. Theater increases modestly over time as administrative complexity grows and political contestation around the floor intensifies, but the mechanism does not degrade to piton levels (where function becomes purely symbolic).
 *
 * PERSPECTIVAL GAP:
 *   The freedom floor reading produces a perspectival gap between beneficiaries who experience autonomy expansion (rope) and the analytical observer who might naturalize the floor as immutable necessity (mountain). The gap reveals whether the floor is a genuine coordination mechanism or a false summit. Monopsony employers occupy a distinctive middle position: they experience both extraction loss (reduced wage suppression) and coordination gain (stable labor supply). This dual experience (tangled_rope) is specific to the freedom floor reading — in the subsidy_capture_reading, employers would experience net benefit (reduced wages offset by lower recruitment costs), shifting toward rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The freedom floor reading emphasizes the beneficiary relationship: workers gain autonomy (exit options improve from trapped to mobile), employers lose extractive power (monopsony suppression declines). This beneficiary structure — with workers as primary beneficiaries gaining expanded choice sets — is the defining feature of this reading. The sibling dependency_trap_reading would emphasize the cost structure (perpetual reliance on state transfers, reduced wage bargaining if workers accept lower wages in exchange for income security, lock-in effects). The subsidy_capture_reading would emphasize the employer benefit (workers accept lower wages because income floor covers the gap, socializing reproduction costs). From the freedom floor perspective, these cost structures are secondary to the autonomy gain. The directionality derivation reflects this: beneficiaries with expanded mobile options experience low effective extraction; victims (employers) lose power but gain coordination benefits, producing mixed experience (tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved through the reading determination omega: the same structural constraint (income floor mechanism) can be classified differently depending on which reading of the kernel dominates. The freedom floor reading resolves mandatrophy by emphasizing the beneficiary structure (workers gain autonomy) and the coordination function (labor market equilibration). If the empirical data shows that income floors primarily enable worker autonomy and reduce monopsony power, the rope classification is robust. If the empirical data shows that income floors primarily create dependency and enable wage suppression, the dependency_trap_reading would be correct instead, producing higher extractiveness (tangled_rope or snare). The mandatrophy is not 'which type is correct?' but 'which reading of the kernel is empirically supported?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_floor_vs_dependency_trap,
    'Does income support structurally enable worker autonomy and expanded exit options, or does it create structural dependency that reduces bargaining power and long-term labor supply?',
    'Longitudinal tracking of beneficiary employment outcomes; wage trajectory analysis comparing income support recipients to non-recipients; measurement of reservation wage changes; labor supply elasticity at different income floor levels',
    'If autonomy-enabling (this reading): classification as rope, low extractiveness, beneficiaries include workers. If dependency-trap (sibling reading): classification shifts toward snare/tangled_rope, higher extractiveness, beneficiaries shift to state/employers. This is the reading-determining omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_floor_vs_dependency_trap, empirical, 'Whether income support enables or constrains worker autonomy').

omega_variable(
    monopsony_power_reduction_magnitude,
    'How much does the income floor reduce monopsony extraction, and does the reduction align with the coordination benefits employers gain from stable labor supply?',
    'Comparative analysis of wage suppression (wage-to-productivity gap) before and after income floor implementation; measurement of labor turnover and training costs; employer bargaining outcomes (hiring discrimination, conditional work requirements)',
    'If reduction is large and benefits exceed costs: employers experience net coordination gain, reinforcing rope classification. If reduction is minimal while suppression remains high: extraction continues, shifting toward tangled_rope with higher asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monopsony_power_reduction_magnitude, empirical, 'Magnitude of monopsony power reduction from income floor').

omega_variable(
    tax_base_distributional_incidence,
    'Who bears the extraction cost of the income floor, and does the incidence align with beneficiary gains?',
    'Tax incidence analysis by income quintile; effective tax rate comparison across wage and capital income; measurement of tax-funded income support as percentage of total state budget',
    'If incidence is progressive (high earners bear most cost) and benefits concentrate on low-wage workers: rope classification is robust. If incidence is regressive or shifted to middle-class wage earners: extractiveness increases, tangled_rope becomes more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_base_distributional_incidence, empirical, 'Distributional incidence of income floor financing').

omega_variable(
    reading_determination_omega,
    'Is income support primarily a mechanism for expanding worker autonomy and choice (freedom floor reading), or primarily a mechanism for socializing the cost of labor reproduction to benefit employers and capital accumulation (dependency trap or subsidy capture readings)?',
    'This omega routes the contested kernel itself. The kernel is income_support_commitment, which can be read as: (a) freedom floor: autonomy-enabling, expands worker exit options, reduces monopsony power (this reading); (b) dependency trap: creates structural reliance on state, reduces wage bargaining power, perpetuates low-wage labor market (sibling reading); (c) subsidy capture: socializes reproduction costs, enables wage suppression, transfers rent to employers (sibling reading). Resolution requires structural analysis of counterfactual wage and employment outcomes.',
    'This omega determines which reading is correct. No single empirical test resolves it — the difference between readings is framing of the SAME structural data. The reading-determining factors are: (1) the magnitude of autonomy gains vs. dependency risks; (2) the distribution of extraction costs; (3) the counterfactual wage structure absent the floor; (4) the long-term effect on labor supply and bargaining power. Different frameworks weight these factors differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_determination_omega, conceptual, 'Kernel reading determination: freedom floor vs. dependency trap vs. subsidy capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_floor_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffr_tr_t0, freedom_floor_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ffr_tr_t3, freedom_floor_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(ffr_tr_t6, freedom_floor_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(ffr_be_t0, freedom_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ffr_be_t3, freedom_floor_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(ffr_be_t6, freedom_floor_reading, base_extractiveness, 6, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(freedom_floor_reading, monopsony_wage_suppression).
narrative_ontology:affects_constraint(freedom_floor_reading, labor_market_exit_capacity).
narrative_ontology:affects_constraint(freedom_floor_reading, reproduction_cost_distribution).

% DUAL FORMULATION NOTE:
% The income support commitment decomposes into three constraint readings with different ε values and beneficiary structures. The freedom floor reading emphasizes worker autonomy gains (ε=0.32, rope). Sibling readings (dependency_trap and subsidy_capture) would show higher extractiveness and different victim/beneficiary distributions. All three readings share the same observable mechanism (income transfers funded by taxation) but interpret its function and incidence differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(freedom_floor_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
