% ============================================================================
% CONSTRAINT STORY: prosecutorial_discretion_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prosecutorial_discretion_asymmetry, []).

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
 *   constraint_id: prosecutorial_discretion_asymmetry
 *   human_readable: Prosecutorial Discretion Asymmetry in Criminal Justice Systems
 *   domain: legal/political/criminal_justice
 *
 * SUMMARY:
 *   Prosecutorial discretion in charging decisions creates an asymmetric
 *   extraction mechanism embedded within ostensibly neutral criminal justice
 *   processes. Prosecutors possess effective monopoly authority over charging
 *   decisions with minimal appellate review, yet this discretion is exercised
 *   unequally across defendants stratified by wealth, race, and social
 *   position. The constraint manifests as a tangled rope — genuine
 *   coordination function (resource allocation in criminal prosecution)
 *   coupled with systematic asymmetric extraction from powerless populations.
 *   Suppression (0.72) reflects multiple mechanisms: wealth barriers to
 *   adequate legal defense, procedural complexity, plea pressure tactics,
 *   implicit bias in discretion deployment, and appellate deference to
 *   prosecutorial judgment. Theater ratio (0.48) indicates that formal
 *   oversight mechanisms (judicial review, appellate process) provide
 *   moderate performative legitimacy while rarely constraining actual
 *   discretion. The constraint exhibits all eight perspectives, with critical
 *   perspectival gaps between the beneficiary (prosecutors experiencing
 *   coordination) and the trapped (defendants facing snare), and between the
 *   natural law view (discretion is inevitable) and the analytical observer's
 *   insight that the constraint is contingent institutional choice.
 *
 * KEY AGENTS:
 *   - Economically Disadvantaged Defendants: Primary victims (powerless/trapped) — face asymmetric charging, plea pressure, inadequate counsel, longer sentences; no exit option from criminal system
 *   - Racial Minority Populations: Primary victims (powerless/trapped) — experience disparate discretion deployment by race, producing differential extraction within trapped population
 *   - Prosecutors and District Attorneys: Primary beneficiaries (institutional/arbitrage) — gain career advancement, conviction metrics, political capital, and resource control through discretionary charging; experience constraint as legitimate coordination
 *   - Defense Bar and Civil Society: Secondary actors (organized/mobile) — benefit from system problems through caseload and advocacy work; bear costs through resource constraints and limited countervailing power; provide partial coordination function
 *   - Affluent Defendants with Private Counsel: Secondary victims (moderate/constrained) — experience reduced asymmetry through wealth-purchased legal capacity but still face discretionary charging; constrained but not trapped
 *   - Criminal Justice Reform Advocates: Organized agents (organized/constrained) — see constraint as addressable through reform; building alternative pathways with generational sunset logic
 *   - Judicial Oversight System: Institutional actor (institutional/arbitrage) — maintains performative review with limited functional constraint on discretion; piton classification reflects degraded function and institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable features of criminal justice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prosecutorial_discretion_asymmetry, 0.58).
domain_priors:suppression_score(prosecutorial_discretion_asymmetry, 0.72).
domain_priors:theater_ratio(prosecutorial_discretion_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prosecutorial_discretion_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(prosecutorial_discretion_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(prosecutorial_discretion_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prosecutorial_discretion_asymmetry, tangled_rope).
narrative_ontology:human_readable(prosecutorial_discretion_asymmetry, "Prosecutorial Discretion Asymmetry in Criminal Justice Systems").
narrative_ontology:topic_domain(prosecutorial_discretion_asymmetry, "legal/political/criminal_justice").

domain_priors:requires_active_enforcement(prosecutorial_discretion_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prosecutorial_discretion_asymmetry, prosecutors_institutional).
narrative_ontology:constraint_beneficiary(prosecutorial_discretion_asymmetry, state_law_enforcement).
narrative_ontology:constraint_victim(prosecutorial_discretion_asymmetry, economically_disadvantaged_defendants).
narrative_ontology:constraint_victim(prosecutorial_discretion_asymmetry, racial_minorities).
narrative_ontology:constraint_victim(prosecutorial_discretion_asymmetry, criminal_procedure_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISADVANTAGED DEFENDANT (SNARE) — Faces asymmetric enforcement risk with no exit option. Lacks resources for adequate legal defense, faces plea pressure from prosecutors wielding charging discretion. Bears full extraction cost through reduced agency in plea bargaining, longer sentences, and constrained trial options. No exit: must navigate the criminal system or face heightened charges.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACIAL MINORITIES (SNARE) — Structural disparities in discretionary charging, sentencing recommendation, and plea pressure create differential extraction within the trapped population. Prosecutors exercise discretion asymmetrically by race, producing disparate impact. No exit option — must engage criminal system on unequal terms.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE BAR AND CIVIL SOCIETY (TANGLED ROPE) — Organized agents with mobile exit options (can shift resources, litigation strategies, advocacy focus) benefit from the structural problem they aim to solve: prosecutors' discretion creates case load, legal career opportunities, and advocacy targets. Simultaneously bear costs through case overload, resource constraints, and limited ability to countervail prosecutorial power. Mixed extraction with genuine coordination function (negotiating plea deals, brokering justice outcomes).
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PROSECUTORS AND DISTRICT ATTORNEYS (ROPE) — Institutional beneficiaries with arbitrage options. Prosecutorial discretion enables case prioritization, resource allocation, and political signaling (high-profile cases, conviction rate metrics). Experience the constraint as coordination: deciding which cases to prioritize solves the resource allocation problem in criminal justice. Minimal suppression from their perspective — they have agency in how discretion is deployed. Net beneficiary.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AFFLUENT DEFENDANTS WITH PRIVATE COUNSEL (TANGLED ROPE) — Constrained but not trapped: can afford adequate legal defense, reducing asymmetry in plea pressure. Experience both extraction (still subject to discretionary charging) and coordination (prosecutors and defense counsel negotiate outcomes). Exit is constrained by wealth (losing assets to defense costs) but real — can shift jurisdiction, hire expert counsel, mount effective trial defense. Moderate experienced extraction relative to powerless counterparts.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CRIMINAL JUSTICE REFORM ADVOCATES (SCAFFOLD) — See prosecutorial discretion asymmetry as a temporary institutional failure addressable through reform: prosecutorial accountability mechanisms, charging guidelines, sentencing transparency, algorithmic risk assessment limits. Organized advocates are building alternative pathways (specialized courts, diversion programs, prosecutorial conduct standards) that sunset the current discretionary regime. Constrained by political resistance but see the problem as solvable within generational timescale.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: JUDICIAL OVERSIGHT SYSTEM (PITON) — Formal appeals process and judicial review provide performative oversight of prosecutorial discretion while rarely overturning decisions on discretion grounds. Theater ratio high: judges ostensibly review charging decisions but defer to prosecutorial judgment, appellate courts rarely second-guess probable cause determinations. The oversight system persists as institutional ritual (checking boxes, producing written opinions) with limited functional capacity to constrain discretion. Maintained through inertia — appellate courts have carved out prosecutorial discretion as largely unreviewable.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, prosecutorial discretion appears immutable: scarce prosecutorial resources always require prioritization, perfect charging equality is incompatible with resource constraints, and some discretion is inherent to law enforcement. This perspective sees the asymmetry as a natural law of criminal systems. However, the structural data (high suppression, organized victims, successful reform examples) contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'inevitable scarcity' naturalizes what is actually a contingent institutional arrangement vulnerable to reform.
constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prosecutorial_discretion_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prosecutorial_discretion_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prosecutorial_discretion_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prosecutorial_discretion_asymmetry, TR),
    TR >= 0.70.

:- end_tests(prosecutorial_discretion_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, derived from the magnitude of outcome disparity between similarly-situated defendants and the systematic directionality of disparity toward powerless populations. The extraction is substantial but not maximal because prosecutors' discretion serves a genuine resource-allocation function — not all discretion is extractive overhead. The increase over the interval (0.38→0.58) reflects documented growth in charging severity and plea pressure as prosecutors face resource constraints and political pressure for high conviction rates. Suppression (0.72): High, reflecting multiple barriers to exit and agency: wealth gaps in legal counsel quality, procedural complexity that favors experienced prosecutors, plea pressure tactics that constrain defendant choice, implicit bias that structures discretion deployment, and appellate deference that removes most judicial check on discretion. Racial minorities face additional suppression through bias-based discretion. Theater ratio (0.48): Moderate, reflecting that formal judicial oversight (probable cause hearings, appellate review, prosecutorial conduct standards) exists and produces documented scrutiny while rarely constraining substantive discretion. The theater is lower than pure symbolic systems because some meaningful review occurs; but it is notable because appellate courts have carved out prosecutorial charging discretion as largely unreviewable except for constitutional violations.
 *
 * PERSPECTIVAL GAP:
 *   Prosecutors see rope (coordination); powerless defendants see snare (extraction). Organized reform advocates see scaffold (temporary, solvable). Judicial oversight sees piton (degraded ritual). The gap reflects different structural positions and exit options. A prosecutor with arbitrage can shift cases, deprioritize categories, negotiate with defense counsel. A trapped defendant has no exit at any price other than incarceration. The natural law observer sees discretion as inevitable feature of law enforcement; the analytical observer using structural data sees it as contingent institutional choice vulnerable to reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the structural position of each agent relative to the extraction flow. Prosecutors (institutional/arbitrage) experience low d: they are beneficiaries positioned to extract value through charging decisions. Their exit option (arbitrage) means they can reallocate resources, shift prosecutorial strategy, or move between jurisdictions with minimal cost. Powerless defendants (powerless/trapped) experience high d: they bear costs with no exit. Trapped exit means neither monetary/resource barriers nor structural mobility options exist — must navigate criminal system as-is or face heightened punishment. The derived d feeds the sigmoid f(d) to produce effective extractiveness chi experienced by each agent. High-d agents experience chi amplified by their powerlessness; low-d beneficiaries experience chi dampened or negative (the constraint subsidizes them). The racial disparities add additional directionality loading: prosecutors' discretion is deployed asymmetrically toward high-d populations (minorities, disadvantaged), creating feedback where the most vulnerable experience maximal extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves through perspectival mapping rather than classification collapse. The tangled_rope classification is analytically correct from the system-level view: prosecutorial discretion does coordinate resource allocation (genuine rope function) while simultaneously extracting from vulnerable populations (snare overlay). The mandatrophy dissolves when we recognize that 'tangled rope' precisely captures the structure: coordination AND extraction are both real. The beneficiary perspective (rope) and victim perspective (snare) both see parts of the true structure; the tangled rope perspective integrates both. The false natural law view is detected by the engine's false summit analysis — if the constraint were truly immutable natural law, it would produce mountain from all perspectives, which it does not. The scaffold perspective is not contradictory but rather forward-looking: reformers see the tangled rope structure and argue the extraction component can be untangled through institutional redesign (sentencing guidelines, prosecutorial conduct standards, charging accountability mechanisms). Empirical evidence from reform jurisdictions supports this claim — the constraint can be partially converted from snare back toward pure rope through governance changes. The mandatrophy is resolved by showing that the confusion comes from whether one measures the constraint's intrinsic structure (tangled rope) or its potential under reform (partially-reversible rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_versus_equality_tradeoff,
    'Is prosecutorial discretion asymmetry an unavoidable tradeoff between prosecutorial resource efficiency and charging equality, or a contingent institutional choice that could be restructured?',
    'Comparative analysis of jurisdictions with different prosecutorial discretion constraints (charging guidelines, mandatory charging thresholds, prosecutorial conduct standards); correlation between discretion scope and outcome disparity; longitudinal measurement of disparity reduction under reform regimes',
    'If unavoidable tradeoff: mountain classification more defensible; constraint fundamentally coordinates scarce resources at cost of inequality. If contingent choice: tangled rope or snare more accurate; inequality is extractive overhead avoidable through institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_versus_equality_tradeoff, empirical, 'Whether discretion asymmetry is inevitable or contingent').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.72) primarily structural (wealth barriers to defense, legal system complexity, procedural disadvantage) or internalized (defendants internalize narratives of inevitability, reduced self-efficacy, identity-based fatalism)?',
    'Post-counsel-provision suppression persistence: measure plea pressure and case outcomes for randomly-assigned adequate counsel groups vs inadequate counsel groups; track whether defendants'' perceived agency increases with legal representation quality; survey data on internalized beliefs about fairness of charging decisions',
    'If primarily structural: suppression can be reduced through resource provision and procedural reform. If partially internalized: suppression persists even after barriers removed; constraint requires epistemic/narrative intervention alongside structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanisms').

omega_variable(
    race_neutrality_of_discretion_asymmetry,
    'Does discretionary charging asymmetry produce racial disparities through neutral application to differently-situated populations, or through explicit or implicit racial bias in discretion deployment?',
    'Case-level analysis controlling for offense severity, prior record, and defendant characteristics; comparison of charging patterns across prosecutors for identical cases; experimental studies of prosecutorial charging decisions with race manipulated; statistical tests for disparate impact and disparate treatment',
    'If race-neutral disparate impact: constraint is primarily about class/wealth asymmetry; reform focuses on resource equalization. If race-based discretion: constraint is fundamentally racialized extraction; reform must include prosecutorial conduct standards and implicit bias training.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(race_neutrality_of_discretion_asymmetry, empirical, 'Racial basis of discretionary charging disparities').

omega_variable(
    plea_coercion_threshold,
    'At what magnitude of charge/sentence differential does prosecutorial plea pressure constitute involuntary coercion rather than legitimate negotiation?',
    'Empirical analysis of plea rates and sentence outcomes as functions of charging differential; experimental studies of defendants'' perception of choice when facing charge escalation on rejection of plea; comparison to international jurisdictions with different plea mechanics',
    'If threshold is high: current disparities are within voluntary negotiation range (snare/tangled_rope distinction more nuanced). If threshold is low: most plea pressure crosses into coercion (snare more universal, fewer defendants experience tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plea_coercion_threshold, empirical, 'Threshold for coercive plea pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prosecutorial_discretion_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prosec_tr_t0, prosecutorial_discretion_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prosec_tr_t10, prosecutorial_discretion_asymmetry, theater_ratio, 10, 0.45).
narrative_ontology:measurement(prosec_tr_t20, prosecutorial_discretion_asymmetry, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(prosec_be_t0, prosecutorial_discretion_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prosec_be_t10, prosecutorial_discretion_asymmetry, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(prosec_be_t20, prosecutorial_discretion_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prosecutorial_discretion_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(prosecutorial_discretion_asymmetry, plea_bargaining_information_asymmetry).
narrative_ontology:affects_constraint(prosecutorial_discretion_asymmetry, bail_system_wealth_stratification).
narrative_ontology:affects_constraint(prosecutorial_discretion_asymmetry, criminal_defense_resource_scarcity).

% DUAL FORMULATION NOTE:
% Prosecutorial discretion asymmetry is upstream of multiple downstream criminal justice constraints. Plea bargaining information asymmetry depends on prosecutorial charging discretion creating pressure. Bail system wealth stratification intersects with discretionary charging disparities. Criminal defense resource scarcity is amplified by prosecutorial discretion concentrated on disadvantaged populations. Each downstream constraint has its own epsilon reflecting its specific structural properties, but all are influenced by the discretionary charging regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prosecutorial_discretion_asymmetry, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
