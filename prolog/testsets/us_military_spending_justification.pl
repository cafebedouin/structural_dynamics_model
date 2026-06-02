% ============================================================================
% CONSTRAINT STORY: us_military_spending_justification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_military_spending_justification, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_military_spending_justification
 *   human_readable: US Military Spending Justification Framework
 *   domain: geopolitical/economic/security
 *
 * SUMMARY:
 *   The US military spending justification framework represents a constraint
 *   that genuinely coordinates legitimate security functions while
 *   simultaneously enabling substantial extraction through budget lock-in,
 *   information asymmetry, and institutional momentum. The constraint emerged
 *   from Cold War deterrence logic and persists through path dependency,
 *   geographic dispersion of military spending across congressional
 *   districts, and technological commitments that create irreversible
 *   infrastructure. The framework exhibits the core tangled_rope signature:
 *   real coordination benefits (deterrence, alliance stability, force
 *   projection capacity) coexist with real asymmetric extraction (contractors
 *   capture rents, military institutional hierarchy resists oversight,
 *   civilians lose fiscal capacity for social investment). The rising theater
 *   ratio (0.42 → 0.68) over the 1991-2021 interval reflects increasing
 *   performativity of threat narratives to justify spending levels that
 *   exceed Cold War-era deterrence minimums. The extractiveness growth (0.35
 *   → 0.58) reflects the constraint's shift from genuine necessity (post-Cold
 *   War force realignment) toward institutional momentum and supplier
 *   pressure (sustained 3-4% of GDP military spending despite absence of peer
 *   military threat). Suppression increased (0.48 → 0.62) as information
 *   asymmetry deepened—classified budgets, contractor lobbying opacity, and
 *   Congressional expertise deficit created conditions for extraction hidden
 *   behind security classification.
 *
 * KEY AGENTS:
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) — Lockheed Martin, Boeing, Raytheon, General Dynamics. Capture rents through cost-plus contracts, technological lock-in, and geographic distribution across congressional districts. Structural coupling through campaign contributions, revolving-door employment.
 *   - Military Institutional Hierarchy: Secondary beneficiary (institutional/arbitrage) — Joint Chiefs, combatant commands, service branches. Benefits from budget lock-in, personnel growth, organizational prestige, and budget primacy. Resists civilian oversight and competitive alternatives.
 *   - Strategic Alliance Partners: Tertiary beneficiary (institutional/mobile) — NATO allies, Japan, South Korea, Taiwan. Benefit from extended deterrence and force projection capacity. Receive implicit security guarantee funded by US taxpayers.
 *   - Congress and Executive Authority: Organized victim (organized/constrained) — Trapped between legitimate security needs, lobbying pressure, and electoral incentives (military spending concentrated in key districts). Subject to information asymmetry from DoD.
 *   - Domestic Social Spending Capacity: Primary victim (powerless/trapped) — Abstract victim. Healthcare, education, infrastructure, climate adaptation suffer from fiscal crowding out. No institutional advocate; cannot organize or exit.
 *   - Fiscal Deficit and Economic Stability: Systemic victim (analytical/trapped) — Structural constraint on long-term fiscal sustainability. Military spending contributes to deficit accumulation and constrains future policy flexibility.
 *   - Geopolitical Rival States: Prisoner's Dilemma victim (powerless/trapped) — China, Russia face security dilemma logic: US military spending triggers escalation response, forcing costly counter-spending. Trapped in arms race spiral despite mutual long-term welfare loss.
 *   - Taxpaying Public: Diffuse victim (powerless/constrained) — Citizens bear fiscal cost through taxation or inflation; lack detailed knowledge of budget allocations; face information asymmetry and suppression of alternatives discussion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_spending_justification, 0.58).
domain_priors:suppression_score(us_military_spending_justification, 0.62).
domain_priors:theater_ratio(us_military_spending_justification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_spending_justification, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_military_spending_justification, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_military_spending_justification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_military_spending_justification, tangled_rope).
narrative_ontology:human_readable(us_military_spending_justification, "US Military Spending Justification Framework").
narrative_ontology:topic_domain(us_military_spending_justification, "geopolitical/economic/security").

domain_priors:requires_active_enforcement(us_military_spending_justification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_military_spending_justification, defense_contractors).
narrative_ontology:constraint_beneficiary(us_military_spending_justification, military_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(us_military_spending_justification, strategic_alliance_partners).
narrative_ontology:constraint_victim(us_military_spending_justification, domestic_social_spending_capacity).
narrative_ontology:constraint_victim(us_military_spending_justification, fiscal_deficit_constraint).
narrative_ontology:constraint_victim(us_military_spending_justification, geopolitical_rival_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIVAL STATE / SECURITY DILEMMA (SNARE) — Cannot exit the arms escalation spiral without unilateral vulnerability. The structure of mutual deterrence creates a prisoner's dilemma where extraction occurs through forced military expenditure and capability matching. Each state is a victim of the system's logic, unable to exit without accepting strategic disadvantage.
constraint_indexing:constraint_classification(us_military_spending_justification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC SOCIAL SPENDING / FISCAL CROWDING OUT (SNARE) — Trapped by budget scarcity. Military justification framework locks spending levels through path dependency (existing contracts, base locations, workforce), preventing reallocation to healthcare, education, infrastructure. Generational time horizon reveals that children born today inherit constrained social investment as structural fact of US fiscal architecture.
constraint_indexing:constraint_classification(us_military_spending_justification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS / EXECUTIVE (TANGLED ROPE) — Organized institutional actors. Genuine coordination function: military readiness is a legitimate public good. But constrained by information asymmetry (DoD budget complexity exceeds legislative oversight capacity), lobbying pressure, and electoral incentives (military spending concentrated in key districts). Experiences both coordination benefit (deterrence function) and constraint (cannot reduce without appearing weak).
constraint_indexing:constraint_classification(us_military_spending_justification, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Institutional beneficiaries with arbitrage options. Experience the constraint as pure coordination: military demand creates stable, profitable markets with high barriers to entry. Contract concentration, geographic distribution across congressional districts, and technological lock-in create structural coupling that makes exit costly for contractors and politically costly for legislators. Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(us_military_spending_justification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC ALLIANCE PARTNERS (ROPE) — NATO allies, Japan, South Korea, and other partners benefit from US force projection and security umbrella. Experience the constraint as beneficial coordination: US military capacity enables extended deterrence and alliance stability. Exit options are mobile but costly (would require independent military buildup). Net beneficiary — the justification framework sustains alliance commitments that benefit these actors.
constraint_indexing:constraint_classification(us_military_spending_justification, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONAL RESIDUE (PITON) — The post-Cold War security apparatus maintains many structural features designed for a threat that no longer exists at Cold War intensity. Theater ratio (0.68) reflects that substantial portion of justification discourse (NATO expansion prevention, containment logic, regional great-power competition) persists through institutional inertia rather than direct functional necessity. The apparatus continues because alternatives haven't fully replaced it and because beneficiaries resist change, not because the original threat persists at Cold War scale.
constraint_indexing:constraint_classification(us_military_spending_justification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRATEGIC NECESSITY VIEW (TANGLED ROPE) — From a cross-position analytical frame, the constraint is genuinely hybrid. Legitimate deterrence and alliance coordination functions coexist with extractive lock-in, institutional momentum, and budget inflation driven by supplier lobbying rather than threat-based strategic logic. The 0.58 extractiveness reflects this hybridity: real coordination benefits (genuine, not performative) combined with real asymmetric extraction (contractors, military hierarchy benefit disproportionately from justification opacity).
constraint_indexing:constraint_classification(us_military_spending_justification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_spending_justification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_military_spending_justification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_spending_justification, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_military_spending_justification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_military_spending_justification, TR),
    TR >= 0.70.

:- end_tests(us_military_spending_justification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination benefits—deterrence is real, alliance commitments are substantive, force projection capacity delivers strategic value. But extractiveness has grown from 0.35 (1991, genuine post-Cold War restructuring) to 0.58 (2021, institutional momentum dominates) because: (1) threat environment did not escalate proportionally to spending growth (Cold War competitor USSR is gone; China and Russia present different threat profiles not requiring 1990s force structure), (2) contractor influence expanded through campaign finance and revolving-door employment, (3) information asymmetry deepened as budgets became more opaque and complex, (4) geographic dispersion locked spending into congressional incentives independent of strategic necessity. Suppression (0.62): Moderate-high. Real suppressive mechanisms include: (1) information asymmetry—classified budgets prevent public assessment of necessity and efficiency, (2) organizational complexity—DoD budget exceeds Congressional expertise capacity, creating dependency on executive briefings, (3) path dependency—existing contracts and bases create sunk costs that make reductions politically costly, (4) geographic lock-in—military spending distributed across all 50 states and most congressional districts creates constituency support independent of strategic logic, (5) ideology capture—post-Cold War security narrative ('forward defense,' 'global great power competition') frames spending as natural necessity rather than contingent choice. Theater ratio (0.68): High and rising. Performative elements include: (1) threat inflation in justification documents (China strategic competition narrative emerged gradually despite slow military modernization in 1990s-2000s), (2) rhetorical coupling of military spending to diverse goods (terrorism, climate, pandemic) disconnected from actual doctrine, (3) technological demonstration as necessity (expensive platforms justified by vendor capability rather than strategic requirement), (4) Cold War institutional language persisting post-Cold War (NATO expansion prevention framing despite absence of Soviet threat). The rising trajectory reflects increasing performativity—1991 spending cuts reflected genuine force restructuring; 2021 spending stability reflects institutional inertia and justification theater rather than strategic logic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a fundamental perspectival gap between beneficiary and victim perspectives. The defense contractor (institutional/arbitrage) experiences pure coordination—their survival depends on military demand, so the constraint appears as a legitimate market signal. Congress (organized/constrained) experiences tangled rope—they perceive genuine deterrence needs but face pressure and information asymmetry that prevent rational assessment. Rival states (powerless/trapped) experience snare—they are locked into matching military capacity regardless of cost or preference. Domestic social spending (powerless/trapped) experiences snare—fiscal scarcity is presented as natural constraint rather than policy choice. The strategic alliance partner (institutional/mobile) experiences rope—they benefit from deterrence coordination without bearing spending cost. The analytical observer sees tangled rope—genuine coordination plus extraction are both real, not competing narratives. The key perspectival disagreement is over whether current spending reflects genuine deterrence needs (would support lower extractiveness, ~0.35-0.45) or institutional momentum (would support higher extractiveness, ~0.65-0.75). The measured value (0.58) represents the analytical assessment that both drivers operate simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from beneficiary/victim structure: Defense contractors are declared beneficiaries with institutional power and arbitrage exit options. Automatic derivation assigns d ≈ 0.05-0.15 (full beneficiary with exit → low directionality, they pull extraction toward themselves). Military hierarchy is institutional beneficiary, less mobile → d ≈ 0.20. Congress is organized victim and organized authority (constrained exit) → d ≈ 0.48. Domestic social spending is powerless victim with trapped exit → d ≈ 0.95. These heterogeneous d values reflect the constraint's asymmetry: extraction flows strongly toward contractors and military hierarchy, moderately toward Congress, catastrophically away from civilian social investment. The f(d) sigmoid transforms these d values into experienced extractiveness multipliers. At d=0.05 (beneficiary), f(d) ≈ -0.12 (extraction subsidizes the agent). At d=0.95 (powerless victim), f(d) ≈ 1.42 (extraction is multiplied by 1.42 relative to baseline ε). The scope modifier σ(S) for national scope is 1.0 (no dampening or amplification), so χ = ε × f(d) × 1.0 produces: contractors experience χ ≈ 0.58 × (-0.12) ≈ -0.07 (negative, they are subsidized); powerless victims experience χ ≈ 0.58 × 1.42 ≈ 0.82 (high extraction). This directionality structure explains the perspectival gap: beneficiaries see negative extraction (they are funded); victims see high extraction (they are depleted). No directionality overrides are required—the structural declaration (beneficiaries + arbitrage, victims + trapped) produces the correct d derivation automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by documenting that tangled_rope is the correct classification at extractiveness 0.58. The mandatrophy asks: is this coordination (rope) or extraction (snare)? Answer: it is both. The coordination function is genuine—deterrence, alliance commitment, and force projection deliver real security benefits. The extraction component is also genuine—contractors, military hierarchy, and institutional momentum enable rents, budget lock-in, and suppression of alternatives that would benefit domestic social investment. The omegas document the irreducible empirical uncertainties that prevent collapsing this to either pure type: (1) threat inflation mechanism determines whether spending reflects genuine necessity or institutional pressure, (2) deterrence function sufficiency determines the baseline ε value below which all spending is extractive excess, (3) information asymmetry scope determines how much suppression reflects legitimate classification versus enabled extraction. If threat inflation is high and deterrence sufficiency is low, extractiveness should increase toward 0.70+ (snare territory). If threat inflation is low and deterrence sufficiency is high, extractiveness should decrease toward 0.40 (rope territory). The current measurement (0.58) reflects moderate threat inflation (Cold War is gone; China threat is real but slower-growing than spending growth) and moderate deterrence sufficiency (current spending exceeds minimum necessary by estimated 15-25% margin based on alliance peer comparisons). The tangled_rope classification is stable under current uncertainty bounds. The theater ratio (0.68) and rising extractiveness trajectory (0.35 → 0.58) confirm the constraint is drifting toward snare; the coordination component is real but eroding as institutional momentum dominates. The mandatrophy is resolved by acknowledging both functions are real, measuring their relative magnitudes empirically, and accepting that the classification depends on those empirical facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_inflation_mechanism,
    'How much of military spending growth is driven by genuine threat escalation versus institutional and contractor pressure to justify existing capacity?',
    'Comparative analysis of threat indicators (rival military capability growth, geographic proximity, demonstrated hostile intent) versus budget growth trajectories and budget justification language over 20-year periods',
    'If threat-driven: extractiveness should be lower (0.35-0.45) because spending reflects genuine security needs. If pressure-driven: extractiveness should be higher (0.65-0.75) because institutional actors are manufacturing threat narratives to justify spending.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_inflation_mechanism, empirical, 'Degree to which spending growth reflects genuine threat escalation vs institutional pressure').

omega_variable(
    deficit_crowding_out_magnitude,
    'What is the true fiscal opportunity cost of military spending in terms of foregone domestic investment?',
    'Counterfactual analysis using historical periods of lower military spending; comparative international analysis of military spending ratios and domestic investment outcomes; econometric modeling of substitution elasticity',
    'If crowding-out is severe (e.g., 1:1 substitution): suppression metric should increase to 0.75+ because military spending directly prevents social investment. If partial (e.g., 0.3:1): suppression should decrease to 0.50 because budget choices are partly constrained by other factors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_crowding_out_magnitude, empirical, 'Fiscal substitution rate between military and domestic spending').

omega_variable(
    technological_lock_in_reversibility,
    'How reversible are the technological and geographic commitments that create path dependency in military spending?',
    'Historical case analysis of attempted major reductions (Base Realignment and Closure, strategic force structure changes); cost and timeline estimates for capability transition; identification of irreversible infrastructure and training investments',
    'If largely irreversible: lock-in is a genuine structural feature and suppression remains high. If reversible over 5-10 years: suppression should decrease to 0.45 because the constraint is institutional inertia rather than sunk-cost trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in_reversibility, empirical, 'Reversibility of technological and geographic commitments in military spending').

omega_variable(
    deterrence_function_sufficiency,
    'What minimum level of military spending maintains effective deterrence versus sufficient-but-not-necessary levels?',
    'Game-theoretic modeling of deterrence thresholds; comparative analysis with peer NATO allies; strategic stability analysis under different spending scenarios; historical comparison with Cold War deterrence stability at lower spending ratios',
    'If current spending is near minimum necessary: claimed tangled_rope is correct and extractiveness reflects genuine coordination plus modest extraction overhead. If current spending exceeds minimum by 20%+ margin: extractiveness should increase to 0.68+ because extraction component dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_function_sufficiency, empirical, 'Minimum military spending required for effective deterrence').

omega_variable(
    information_asymmetry_scope,
    'How complete is congressional oversight of military spending justifications, and how much is based on executive assessment versus independent verification?',
    'Analysis of Congressional Budget Office and Government Accountability Office reports; documentation of classified vs publicly available budget justifications; audit findings on contractor compliance and cost overruns',
    'If asymmetry is high (>60% classified or unaudited): suppression should increase to 0.70+ because information barriers enable extraction. If asymmetry is low (<20%): suppression should decrease to 0.50 because transparency enables accountability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_scope, empirical, 'Degree of information asymmetry in military spending oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_military_spending_justification, 1991, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmil_tr_t0, us_military_spending_justification, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usmil_tr_t15, us_military_spending_justification, theater_ratio, 15, 0.55).
narrative_ontology:measurement(usmil_tr_t30, us_military_spending_justification, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(usmil_be_t0, us_military_spending_justification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usmil_be_t15, us_military_spending_justification, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(usmil_be_t30, us_military_spending_justification, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usmil_su_t0, us_military_spending_justification, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(usmil_su_t15, us_military_spending_justification, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(usmil_su_t30, us_military_spending_justification, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_military_spending_justification, resource_allocation).
narrative_ontology:boltzmann_floor_override(us_military_spending_justification, 0.18).
narrative_ontology:affects_constraint(us_military_spending_justification, us_deficit_fiscal_constraint).
narrative_ontology:affects_constraint(us_military_spending_justification, nato_alliance_stability).
narrative_ontology:affects_constraint(us_military_spending_justification, arms_race_escalation_spiral).
narrative_ontology:affects_constraint(us_military_spending_justification, defense_contractor_rent_seeking).

% DUAL FORMULATION NOTE:
% US military spending justification framework has upstream dependency on geopolitical threat assessment (constraint family: us_china_strategic_competition, russia_deterrence_posture) and downstream effects on fiscal policy (us_deficit_fiscal_constraint), alliance management (nato_alliance_stability), and competitor military spending (arms_race_escalation_spiral). Decomposition note: the genuine deterrence coordination function and the extractive lock-in are separate structurally distinct constraints that could be modeled separately (deterrence_capability_coordination ε≈0.25 vs military_budget_lock_in ε≈0.68), but they are deeply coupled in implementation—military personnel, platforms, and bases serve both functions simultaneously—making decomposition impractical. The tangled_rope classification reflects this structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
