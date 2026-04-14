% ============================================================================
% CONSTRAINT STORY: sotu_1974_nixon_voluntary_inflation_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1974_nixon_voluntary_inflation_control, []).

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
 *   constraint_id: sotu_1974_nixon_voluntary_inflation_control
 *   human_readable: Voluntary Wage-Price Stabilization Through Congressional Cooperation (1974)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   In early 1974, the Nixon administration proposed a voluntary wage-price
 *   stabilization framework presented as cooperative fiscal restraint:
 *   Congress would agree to hold down government spending growth, enabling
 *   the administration to control inflation without inducing recession
 *   through monetary contraction. The mechanism distributes anti-inflation
 *   burden across federal budgets rather than imposing it through
 *   unemployment. Workers and consumers were positioned as beneficiaries of
 *   inflation control achieved without job losses; federal agencies and
 *   contractors bore costs through constrained appropriations and
 *   procurement. The constraint exemplifies the tension between genuine
 *   coordination (inflation control serves all parties' long-term interests)
 *   and extraction (fiscal constraint asymmetrically burdens government
 *   sectors). The rising theater ratio reflects that the 'voluntary
 *   cooperation' framing became increasingly hollow as explicit budget caps
 *   replaced rhetorical appeals; by year 18 of the interval, the performative
 *   coordination of the early period had degraded into mandatory fiscal
 *   constraint. The extractiveness trajectory shows accumulating burden on
 *   agencies/contractors, as year-over-year spending caps compound and
 *   deferred maintenance/capability gaps accumulate.
 *
 * KEY AGENTS:
 *   - Federal Agencies and Contractors: Primary victims (powerless/trapped, organized/constrained) — face hard spending caps with no negotiable exit; extraction flows toward fiscal constraint regardless of mission requirements
 *   - Workers and Consumers: Primary beneficiaries (moderate/constrained) — gain employment stability and inflation control; face constrained purchasing power but avoid recession-driven job losses
 *   - Congress: Institutional actor (institutional/constrained) — nominally cooperating to control inflation; simultaneously surrendering fiscal autonomy and facing pressure from constituent agencies bearing extraction costs
 *   - The Administration: Primary architect (institutional/arbitrage) — gains macroeconomic control via fiscal constraint; designed the mechanism to serve executive anti-inflation objectives without monetary contraction's political cost
 *   - Organized Labor: Secondary beneficiary (powerful/mobile) — gains negotiating leverage from recession avoidance; supports inflation control that protects sectoral wage stability
 *   - The Voluntary Compliance Framework: Institutional theater (institutional/arbitrage) — performative coordination mechanism that degrades over time as explicit controls replace voluntary appeals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1974_nixon_voluntary_inflation_control, 0.52).
domain_priors:suppression_score(sotu_1974_nixon_voluntary_inflation_control, 0.48).
domain_priors:theater_ratio(sotu_1974_nixon_voluntary_inflation_control, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1974_nixon_voluntary_inflation_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1974_nixon_voluntary_inflation_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1974_nixon_voluntary_inflation_control, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1974_nixon_voluntary_inflation_control, tangled_rope).
narrative_ontology:human_readable(sotu_1974_nixon_voluntary_inflation_control, "Voluntary Wage-Price Stabilization Through Congressional Cooperation (1974)").
narrative_ontology:topic_domain(sotu_1974_nixon_voluntary_inflation_control, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1974_nixon_voluntary_inflation_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_voluntary_inflation_control, workers_and_consumers).
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_voluntary_inflation_control, administration_macroeconomic_control).
narrative_ontology:constraint_victim(sotu_1974_nixon_voluntary_inflation_control, federal_agencies).
narrative_ontology:constraint_victim(sotu_1974_nixon_voluntary_inflation_control, government_contractors).
narrative_ontology:constraint_victim(sotu_1974_nixon_voluntary_inflation_control, fiscal_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL AGENCY BUDGET MANAGER (SNARE) — Faces hard spending caps with no negotiable exit. Agency mission requirements (hiring, operations, service delivery) are structurally fixed; budget constraints are imposed externally without coordination. Cannot exit the constraint without abandoning the agency's core function. Maximum extraction: forced to prioritize spending, defer maintenance, reduce staffing. No genuine coordination benefit — the constraint is pure coercion justified by macro-level goals the agency cannot influence.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT CONTRACTORS ASSOCIATION (TANGLED ROPE) — Bears constrained spending in government procurement; reduced contract opportunities extract value. But also benefits from inflation control that protects their supply chains and labor costs. Organized agents with some lobbying power can negotiate exceptions and maintain essential contracts. Mixed experience: genuine coordination (inflation control benefits contractor economics) alongside extraction (demand destruction from budget caps). Constrained exit — can lobby for exemptions but cannot simply exit government market dependence.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED LABOR IN INFLATION-PROTECTED SECTORS (ROPE) — Benefits from voluntary wage stabilization that avoids recession-driven job losses. Unions in manufacturing, construction, and government sectors gain negotiating leverage when recession is avoided. Mobile exit options: can shift wage demands across sectors, can focus on non-wage benefits. Experiences the constraint as coordination: cooperation on price stability serves labor's core interest (employment stability). Low extraction — this agent sees genuine mutual benefit.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKERS AND CONSUMERS GENERALLY (ROPE) — Primary intended beneficiaries of inflation control achieved without recession. Avoiding job losses through demand maintenance protects wage stability and employment. Some constraint on their purchasing power during the stabilization interval, but this is presented as temporary. Exit options: constrained by employment dependence, but mobile across sectors and geographic regions over biographical time. Genuine coordination benefit dominates extraction cost.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ADMINISTRATION / EXECUTIVE BRANCH (ROPE) — Primary architect and beneficiary of the voluntary stabilization framework. Gains macroeconomic control via fiscal constraint without imposing the political cost of explicit monetary contraction or unemployment. Arbitrage exit: can unilaterally modify the constraint or relax enforcement. Experiences the mechanism as purely coordinating — the constraint IS the administration's preferred anti-inflation tool. Zero experienced extraction — this agent designed the mechanism to serve its interests.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESS AS INSTITUTIONAL ACTOR (TANGLED ROPE) — Congress faces a mixed coordination-extraction dynamic. Nominally cooperating to control inflation, Congress is simultaneously surrendering fiscal autonomy to executive coordination. Members face pressure from constituent agencies and contractors bearing extraction costs, yet institutional interest in inflation control aligns with executive preference. Constrained exit: politically difficult to abandon the cooperative framework without appearing to sabotage inflation control. Benefits from coordination (macro stability, avoiding blame for recession); bears extraction costs (loss of appropriations autonomy, vulnerability to executive coordination capture).
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: VOLUNTARY COMPLIANCE FRAMEWORK AS INSTITUTIONAL THEATER (PITON) — The 'voluntary' framing is substantially performative. Congressional cooperation is nominally voluntary but politically coerced by macro-economic conditions and administration pressure. The theater ratio is elevated because the mechanism is repeatedly invoked as cooperation even when constraints are effectively mandatory. Over time, the framework degrades: agency budgets must be cut harder to maintain inflation control, and the 'voluntary' framing becomes increasingly hollow. Piton classification reflects that the primary function (signaling macroeconomic seriousness through voluntary action) decays over the stabilization interval — eventually explicit controls replace the cooperative fiction.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, the constraint might appear as an immutable property of macroeconomic governance: stagflation forces trade-offs between inflation and employment; avoiding recession requires coordination; voluntary fiscal constraint is an inevitable feature of inflation control during periods of supply shock. However, the structural data contradicts the mountain classification — identifiable beneficiaries and extraction mechanisms indicate a contingent political arrangement, not a natural law. The engine's false summit detector will identify this as naturalization of what is actually a specific policy choice.
constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1974_nixon_voluntary_inflation_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1974_nixon_voluntary_inflation_control, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1974_nixon_voluntary_inflation_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1974_nixon_voluntary_inflation_control, TR),
    TR >= 0.70.

:- end_tests(sotu_1974_nixon_voluntary_inflation_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, rising to 0.65): The constraint begins as moderate mixed coordination-extraction but trends toward high extraction as budget caps become harder. Initial extractiveness is moderate because genuine inflation control benefits exist — avoiding recession does protect employment and worker purchasing power. However, extractiveness rises over the interval because agencies face cumulative spending constraints, deferred maintenance accumulates, and the burden becomes visibly asymmetric. By month 18, the extractiveness approaches snare-level severity for federal agencies. Suppression (0.48): Moderate-high. Agency managers have limited alternatives: they must maintain operations within constrained budgets or resign (low-power agents have minimal exit). Congress faces political suppression: abandoning the cooperative framework appears to sabotage inflation control. However, suppression is not total — agencies can lobby for exemptions, Congress can legislatively reverse constraints, and contractors can reduce dependence on government work. Theater ratio (0.65, rising to 0.78): The 'voluntary cooperation' framing is substantially performative, especially in later periods. Early in the interval, rhetoric emphasizes genuine cooperation and shared interest. Over time, the frame becomes increasingly hollow — budget caps become explicit and mandatory, the voluntary language persists ceremonially, and the constraint functions as unilateral executive-congressional fiscal policy rather than true coordination. The rising theater ratio reflects metric substitution: the constraint's function shifts from signaling macroeconomic seriousness to mechanically enforcing budget reduction, yet the 'voluntary cooperation' theater persists.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal. The administration frames the constraint as pure coordination (Rope) — all parties benefit from inflation control achieved without recession. Agencies and contractors frame it as pure extraction (Snare) — they bear costs with no offsetting benefit specific to their operations. Congress occupies the middle (Tangled Rope) — gains macro-level coordination benefit but loses fiscal discretion. Workers/consumers see coordination (Rope) — inflation control serves their employment and purchasing-power interests. The perspectival disagreement is not about facts but about which effects matter: does employment stability (coordination benefit) outweigh spending constraint extraction? Does inflation control serve the beneficiary narrative? The answer depends on which agent's time horizon, power level, and exit options you privilege. The engine's classification captures this: each perspective produces a different type from the same underlying constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality d reflects their structural position in the extraction-coordination flow. Federal agencies face extraction (d ≈ 0.95): they are victims of spending caps, have no exit option (abandoning the agency is not an acceptable exit), and receive no benefit specific to their operations — the benefit (macro inflation control) accrues to consumers and workers, not to agency budgets. Congress faces mixed directionality (d ≈ 0.50): institutional power gives them agency over the constraint, but constrained exit (political cost of abandoning inflation control) limits actual autonomy. The administration faces low directionality (d ≈ 0.05-0.15): this agent designed and benefits from the constraint, has arbitrage exit (can modify unilaterally), and receives concentrated benefit (macroeconomic control). Workers/consumers face moderate directionality (d ≈ 0.40-0.50): they benefit from inflation control and employment stability, but face constrained purchasing power during the stabilization interval. The sigmoid f(d) transforms these d values into experienced extractiveness: agency managers (high d) experience maximum extraction; the administration (low d) experiences near-zero extraction or negative extraction (subsidy effect); Congress (moderate d) experiences moderate extraction; workers (moderate-low d) experience low-to-moderate extraction with offsetting benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the tangled_rope classification correctly captures the hybrid coordination-extraction dynamic. The constraint is genuinely a coordination mechanism (inflation control serves all parties' long-term interests) AND a mechanism of asymmetric extraction (burden concentrates on federal agencies/contractors while beneficiaries escape). Both descriptions are true simultaneously; the mandatrophy is resolved by recognizing that tangled_rope is the precise classification for hybrid mechanisms. The rising theater ratio indicates that the constraint is drifting toward piton classification: the performative 'voluntary cooperation' framing increasingly dominates the actual mechanism (mandatory fiscal caps). The analytical observer's natural-law perspective is a false summit: presenting the constraint as an inevitable feature of inflation-control policy naturalizes a specific political choice. Alternative distributional mechanisms (progressive taxation, demand management, supply-side investment) could achieve inflation control without asymmetric fiscal extraction. The false summit reveals that calling the constraint 'natural' serves the beneficiaries' interests by precluding discussion of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_framing,
    'Is the congressional cooperation genuinely voluntary, or is it politically coerced by stagflation conditions and executive pressure?',
    'Comparison of congressional behavior under equivalent inflation conditions when no executive coordination framework is proposed; analysis of legislative voting patterns and committee resistance; examination of private congressional correspondence during the period',
    'If genuinely voluntary: coordination mechanism (Rope from congressional perspective). If coerced: extraction mechanism (Tangled Rope or Snare). Changes the classification of the constraint from pure coordination to hybrid or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_framing, empirical, 'Whether congressional cooperation is voluntary or politically coerced').

omega_variable(
    recession_avoidance_efficacy,
    'Did the voluntary stabilization framework actually prevent recession, or would the economy have avoided recession regardless given other factors?',
    'Counterfactual analysis using econometric models comparing actual inflation/unemployment outcomes to modeled outcomes under alternative policy scenarios; comparison to peer economies without similar coordination frameworks',
    'If stabilization was efficacious: genuine coordination benefit justifies extraction costs for agencies/contractors. If ineffective: extraction persists without corresponding benefit — constraint degrades toward Snare. Changes the beneficiary/victim ratio and cost justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recession_avoidance_efficacy, empirical, 'Whether voluntary stabilization prevented recession').

omega_variable(
    fiscal_flexibility_cost_accumulation,
    'Does the constraint''s extraction of fiscal flexibility accumulate over time, creating structural vulnerability when macro conditions shift?',
    'Tracking of government spending constraint severity; analysis of deferred maintenance, hiring freezes, and capability gaps; comparison of government operational efficiency before/after prolonged stabilization; assessment of fiscal response capacity during subsequent crises',
    'If flexibility costs accumulate: piton classification is confirmed — performative cooperation becomes structurally degraded. If flexibility is recoverable: Rope/Scaffold classification sustained. Changes long-term classification and sustainability assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_flexibility_cost_accumulation, empirical, 'Whether fiscal constraint extraction accumulates structural cost').

omega_variable(
    distributional_equity_of_stabilization_burden,
    'Is the inflation control burden distributed equitably across sectors, or does extraction concentrate on federal agencies and contractors while beneficiaries (workers, consumers) escape cost?',
    'Sectoral analysis of inflation/deflation exposure; comparison of government sector vs private sector wage/employment outcomes during stabilization period; tracking of contractor profit margins and government spending as share of contractor revenue',
    'If burden concentrates on federal sector: validates Snare/Tangled Rope classification for agencies/contractors. If burden distributes widely: Rope classification gains validity. Affects assessment of whether cooperation masks redistribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_equity_of_stabilization_burden, empirical, 'Distributional equity of stabilization burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1974_nixon_voluntary_inflation_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu74_tr_t0, sotu_1974_nixon_voluntary_inflation_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sotu74_tr_t6, sotu_1974_nixon_voluntary_inflation_control, theater_ratio, 6, 0.58).
narrative_ontology:measurement(sotu74_tr_t12, sotu_1974_nixon_voluntary_inflation_control, theater_ratio, 12, 0.68).
narrative_ontology:measurement(sotu74_tr_t18, sotu_1974_nixon_voluntary_inflation_control, theater_ratio, 18, 0.78).

% Extraction over time
narrative_ontology:measurement(sotu74_be_t0, sotu_1974_nixon_voluntary_inflation_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu74_be_t6, sotu_1974_nixon_voluntary_inflation_control, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(sotu74_be_t12, sotu_1974_nixon_voluntary_inflation_control, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(sotu74_be_t18, sotu_1974_nixon_voluntary_inflation_control, base_extractiveness, 18, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1974_nixon_voluntary_inflation_control, resource_allocation).
narrative_ontology:affects_constraint(sotu_1974_nixon_voluntary_inflation_control, federal_budget_authority_capture).
narrative_ontology:affects_constraint(sotu_1974_nixon_voluntary_inflation_control, agency_discretionary_authority_erosion).

% DUAL FORMULATION NOTE:
% The voluntary stabilization framework is downstream of stagflation macro conditions but represents a distinct structural constraint on fiscal governance. Federal budget authority capture and discretionary erosion are downstream consequences: as stabilization constraints persist, agency operational autonomy and Congress's budgetary authority are subordinated to executive macroeconomic objectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1974_nixon_voluntary_inflation_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
