% ============================================================================
% CONSTRAINT STORY: demand_destruction_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demand_destruction_policy, []).

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
 *   constraint_id: demand_destruction_policy
 *   human_readable: Demand Destruction Policy as Coordination-Extraction Hybrid
 *   domain: economic_policy/inflation_control
 *
 * SUMMARY:
 *   Demand destruction policy represents a macroeconomic constraint wherein
 *   central banks intentionally reduce aggregate demand (through rate
 *   increases, quantitative tightening, and contractionary fiscal
 *   coordination) to control inflation. The policy creates a structural
 *   tension: inflation control is a genuine coordination problem requiring
 *   reduced nominal spending, but the specific institutional mechanism
 *   selected — targeting unemployment and suppressing wages through labor
 *   market slack — distributes costs asymmetrically onto wage earners while
 *   distributing benefits toward creditors and asset holders. The constraint
 *   exhibits tangled_rope characteristics: it solves a real coordination
 *   problem (excess aggregate demand) while simultaneously extracting from
 *   powerless agents (wage earners, small business owners) through
 *   suppression mechanisms (unemployment risk, credit restriction). The
 *   theater ratio reflects that demand destruction justification relies on
 *   contested theoretical frameworks (Phillips curve, NAIRU) and naturalizes
 *   contingent institutional choices as economic necessity.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — bear extraction through unemployment risk, wage suppression, and reduced bargaining power
 *   - Small Business Owners: Secondary victims (powerless/trapped) — bear extraction through falling revenues, rising borrowing costs, collateral value loss
 *   - Central Bank: Primary institutional beneficiary (institutional/arbitrage) — coordinates inflation control; benefits from mandate fulfillment and credibility restoration
 *   - Creditor Class: Secondary institutional beneficiary (institutional/arbitrage) — benefits from deflation of debtor incomes and restoration of real asset values
 *   - Labor Coalition: Organized secondary agent (organized/constrained) — benefits from inflation control but bears asymmetric extraction through unemployment targeting
 *   - Unemployed Workers: Tertiary victims (moderate/constrained) — bear full extraction during unemployment phase but benefit from eventual price stability
 *   - Inflationist Reform Coalition: Alternative agent (organized/mobile) — advocates for sunset through alternative mechanisms (wage-price coordination, supply policy)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choice as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demand_destruction_policy, 0.58).
domain_priors:suppression_score(demand_destruction_policy, 0.65).
domain_priors:theater_ratio(demand_destruction_policy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demand_destruction_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(demand_destruction_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(demand_destruction_policy, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demand_destruction_policy, tangled_rope).
narrative_ontology:human_readable(demand_destruction_policy, "Demand Destruction Policy as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(demand_destruction_policy, "economic_policy/inflation_control").

domain_priors:requires_active_enforcement(demand_destruction_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demand_destruction_policy, creditor_class).
narrative_ontology:constraint_beneficiary(demand_destruction_policy, asset_holders).
narrative_ontology:constraint_beneficiary(demand_destruction_policy, inflation_hawks).
narrative_ontology:constraint_victim(demand_destruction_policy, wage_earners).
narrative_ontology:constraint_victim(demand_destruction_policy, small_business_owners).
narrative_ontology:constraint_victim(demand_destruction_policy, unemployed_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped by income dependence and inability to exit labor markets. Bears full extraction through unemployment risk, wage suppression, and reduced bargaining power. No exit option; maximum experienced extraction. Suppression mechanisms include reserve army of the unemployed and restricted credit access.
constraint_indexing:constraint_classification(demand_destruction_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (SNARE) — Trapped by debt obligations and inability to exit credit markets during contraction. Bears extraction through falling revenues, inability to service debt, and loss of collateral value. Suppression includes rising borrowing costs and lack of alternative financing sources.
constraint_indexing:constraint_classification(demand_destruction_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (ROPE) — Coordinates inflation control across the macroeconomy through rate adjustment. Views demand destruction as legitimate coordination mechanism: painful but necessary to restore price stability. Benefits from institutional mandate fulfillment and credibility restoration. Has exit option (stop tightening) and uses it strategically.
constraint_indexing:constraint_classification(demand_destruction_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDITOR CLASS (ROPE) — Experiences demand destruction as coordination mechanism that restores their real asset values and purchasing power. Benefits from deflation of debtor incomes and restoration of interest margin. Has exit option (lending terms) and exercises it with selectivity. Nets positive from constraint — extraction flows toward them.
constraint_indexing:constraint_classification(demand_destruction_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR COALITION (TANGLED ROPE) — Organized but constrained by capital mobility and political fragmentation. Benefits from potential inflation control (real wage protection) but bears asymmetric extraction through unemployment targeting. Has some agency (unionization, strikes) but faces structural coordination problems. Mixed extraction-coordination hybrid.
constraint_indexing:constraint_classification(demand_destruction_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: UNEMPLOYED WORKER (TANGLED ROPE) — Constrained by job search costs and skill degradation. Experiences demand destruction as primary mechanism for inflation control (surplus labor dampens wage inflation). Moderate power through labor market participation when reemployed. Mixed: benefits from eventual price stability but bears full extraction cost during unemployment phase.
constraint_indexing:constraint_classification(demand_destruction_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INFLATIONIST REFORM COALITION (SCAFFOLD) — Advocates for alternative inflation-control mechanisms (wage-price coordination, supply-side policy, international coordination). Sees demand destruction as temporary tool to be replaced by better coordination. Views the policy as having natural sunset once inflation normalizes and alternative mechanisms mature. Low effective extraction because coalition has exit path and mobility.
constraint_indexing:constraint_classification(demand_destruction_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: NEOLIBERAL CONSENSUS (PITON) — Demand destruction has become the default institutional response to inflation despite contested theoretical justification and degraded empirical performance. Theater ratio reflects performative consensus among policymakers, central bankers, and economists despite growing dissent. The mechanism persists through institutional inertia and ideological capture rather than proven effectiveness. Constraint maintained by narrative rather than function.
constraint_indexing:constraint_classification(demand_destruction_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some demand destruction appears inherent to inflation control: reducing aggregate demand is a thermodynamic necessity when nominal spending exceeds real production capacity. However, the structural data reveals this as a false summit — demand destruction is one institutional choice among multiple policy pathways (wage-price coordination, supply expansion, money velocity adjustment). The 'natural law' framing masks distributional choices.
constraint_indexing:constraint_classification(demand_destruction_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demand_destruction_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demand_destruction_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demand_destruction_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demand_destruction_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(demand_destruction_policy, TR),
    TR >= 0.70.

:- end_tests(demand_destruction_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy does solve a real coordination problem (excess aggregate demand during inflation) but uses a specific mechanism that asymmetrically extracts from wage earners and small business owners. The extraction is not maximal (0.75+) because some agents benefit from inflation control and the mechanism has genuine coordination function. The measurement shows increase over 4 years (0.42→0.58) as policy duration extends and unemployment accumulates, indicating extraction accumulation over time. Suppression (0.65): High. Multiple suppression mechanisms operate: unemployment threat reduces labor bargaining power; credit restriction limits business investment; reduced purchasing power limits consumption choices; political economy structures prevent alternative inflation-control mechanisms from being tried. Theater ratio (0.48): Moderate. Demand destruction justification relies on theoretical frameworks (Phillips curve, NAIRU, output gap) that are increasingly contested empirically. The constraint is partially functional (excess demand does need to be reduced) but partially performative (the specific unemployment-targeting mechanism is justified by ideology as much as economics). Theater increases slightly over 4 years (0.35→0.48) as performance costs accumulate without proportional inflation reduction.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective's classification follows from its structural position. Powerless trapped agents see snare (pure extraction) because they experience only costs. Institutional arbitrage agents see rope (pure coordination) because they experience net benefit. Organized constrained agents see tangled rope (mixed) because they have some agency but face extraction. The perspectival gap reveals that no single classification is correct — the presheaf of classifications IS the analysis. The gap between institutional perspectives (central bank rope vs. creditor rope) shows that institutional agents with different beneficiary status still converge on rope classification, but directionality values differ. The mountain perspective exposes false naturalization: 'inflation control requires demand destruction' is a misstatement of thermodynamic law — it conflates 'reducing aggregate demand' (necessary) with 'targeting unemployment' (contingent institutional choice).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each agent relative to this specific constraint. Wage earners (trapped, victim) have d ≈ 0.95 (full target), producing maximum f(d) ≈ 1.42. This translates to experienced extractiveness χ = 0.58 × 1.42 × 1.0 ≈ 0.82 at national scope. Central banks (arbitrage, beneficiary) have d ≈ 0.10 (near-beneficiary), producing f(d) ≈ -0.01, translating to χ ≈ -0.01 (or clamped to ~0.0). Creditors (arbitrage, beneficiary) have d ≈ 0.15, producing f(d) ≈ 0.02, translating to χ ≈ 0.01. Organized labor (constrained, mixed) has d ≈ 0.60 (symmetric), producing f(d) ≈ 0.65, translating to χ ≈ 0.38. The directionality logic shows why the same base extractiveness (0.58) appears as snare to the trapped, rope to the beneficiary, and tangled rope to organized agents — each experiences different effective extraction based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Demand destruction policy resolves mandatrophy by explicit declaration of coordination function (inflation control) paired with asymmetric extraction (unemployment targeting). The constraint is tangled rope, not pure snare or pure rope. The coordination function (reducing nominal spending to match productive capacity) is genuine and necessary. The extraction (concentrating adjustment costs onto wage earners and small businesses while benefiting creditors) is not necessary but is a distributional choice embedded in the institutional design. Alternative inflation-control mechanisms exist (wage-price coordination, supply expansion, international coordination, velocity management) that would solve the same coordination problem with different or lower extraction. The mandatrophy is resolved by showing that the classification as tangled rope is stable under all reasonable observables: whether measured by unemployment incidence, real wage changes, or wealth distribution, the constraint shows both coordination function and asymmetric extraction. No reformulation of the measurement basis would collapse this to pure rope or pure snare — the hybrid classification is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_destruction_necessity,
    'Is demand destruction an inherent requirement for inflation control, or a contingent institutional choice among alternatives?',
    'Comparative institutional analysis: countries using wage-price coordination, supply-side policy, or international coordination achieve inflation control without proportional demand destruction. Evidence of alternative causal pathways to price stability.',
    'If inherent: classification approaches mountain (natural constraint). If contingent: classification confirms tangled_rope (institutional hybrid with extractive overlay). Affects whether policy can be reformed vs. must be accepted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demand_destruction_necessity, conceptual, 'Whether demand destruction is inherent or contingent to inflation control').

omega_variable(
    extraction_incidence_measurement,
    'What proportion of demand destruction''s costs fall on wage earners vs. distributed across all income classes?',
    'Income distribution analysis: wages vs. asset values during contraction; unemployment rate targeting relative to income distribution; real purchasing power changes by decile.',
    'If concentrated on lower incomes: confirms snare classification for wage earners (high d). If distributed: reclassifies as more symmetric tangled_rope. Determines whether suppression is structural inequality or coordinated sacrifice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_incidence_measurement, empirical, 'Incidence of demand destruction costs across income distribution').

omega_variable(
    alternative_mechanism_availability,
    'Do realistic institutional alternatives to demand destruction (wage-price accords, import competition management, productivity acceleration) have materially lower transaction costs?',
    'Institutional economics: transaction costs of coordinating wage-price agreements vs. unemployment costs; comparative analysis of countries using alternative inflation-control regimes; simulation of multi-mechanism coordination.',
    'If alternatives have lower costs: demand destruction is rent-extraction (snare classification confirmed). If alternatives have higher costs: coordination necessity is real (tangled_rope confirmed). Affects whether the constraint can be reformed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_availability, empirical, 'Transaction costs of alternative inflation-control mechanisms').

omega_variable(
    creditor_power_asymmetry,
    'Does demand destruction systematically advantage creditors (fixed income, asset holders) over debtors (wage earners, small businesses)?',
    'Financial incidence analysis: real return on bonds vs. equities during contraction; creditor concentration in financial sector vs. small business debt distribution; distributional outcome measurements.',
    'If yes: confirms extraction mechanism (snare/tangled_rope from victims'' perspectives). If symmetric: reclassifies constraint as more balanced coordination. Determines whether beneficiary list is complete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creditor_power_asymmetry, empirical, 'Asymmetric advantage to creditors from demand destruction').

omega_variable(
    unemployment_targeting_justification,
    'Is unemployment targeting (using joblessness to suppress wage inflation) justified by economic necessity or by distributional preference for creditor interests?',
    'Counterfactual analysis: inflation outcomes under alternative suppression mechanisms (wage controls, price controls, import opening); NAIRU validity testing; evidence of explicit unemployment targeting vs. incidental effect.',
    'If justified: validates tangled_rope classification (mixed coordination-extraction). If preference-driven: confirms snare classification (pure extraction disguised as necessity). Affects policy reform viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unemployment_targeting_justification, preference, 'Justification for unemployment targeting as suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demand_destruction_policy, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demand_destroy_tr_t0, demand_destruction_policy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(demand_destroy_tr_t2, demand_destruction_policy, theater_ratio, 2, 0.42).
narrative_ontology:measurement(demand_destroy_tr_t4, demand_destruction_policy, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(demand_destroy_be_t0, demand_destruction_policy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(demand_destroy_be_t2, demand_destruction_policy, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(demand_destroy_be_t4, demand_destruction_policy, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demand_destruction_policy, resource_allocation).
narrative_ontology:affects_constraint(demand_destruction_policy, unemployment_targeting_mechanism).
narrative_ontology:affects_constraint(demand_destruction_policy, wage_suppression_via_slack).
narrative_ontology:affects_constraint(demand_destruction_policy, creditor_advantage_in_deflation).

% DUAL FORMULATION NOTE:
% Demand destruction policy can be decomposed into multiple structurally distinct constraints: (1) inflation_control_mechanism — the genuine coordination problem of excess aggregate demand (ε≈0.15, Rope) — and (2) unemployment_targeting_extraction — the specific institutional choice to suppress wages through joblessness (ε≈0.68, Snare). This story focuses on the hybrid constraint that includes both. The decomposition shows that the coordination problem (excess demand) has ε≈0.15, but the extraction overlay (unemployment targeting) raises effective ε to 0.58.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(demand_destruction_policy, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
