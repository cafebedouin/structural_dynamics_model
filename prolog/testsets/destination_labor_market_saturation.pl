% ============================================================================
% CONSTRAINT STORY: destination_labor_market_saturation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_destination_labor_market_saturation, []).

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
 *   constraint_id: destination_labor_market_saturation
 *   human_readable: Destination Labor Market Saturation
 *   domain: labor_economics/migration
 *
 * SUMMARY:
 *   Destination labor market saturation occurs when migrant inflows exceed
 *   local labor demand, creating downward wage pressure, occupational
 *   downgrading, and reduced mobility for all workers — both migrants and
 *   destination natives. The constraint operates through suppression of
 *   alternatives: migrants face high exit costs (sunk investment, visa
 *   status, family commitments, psychological attachment) while employers
 *   benefit from wage compression and labor abundance. The saturation
 *   mechanism exhibits extraction characteristics (wages suppressed below
 *   marginal product, workers locked into deteriorating conditions) alongside
 *   coordination value (labor supply enables businesses and services that
 *   would not exist without migrants). Theater components emerge through
 *   immigration enforcement (border controls that persist despite
 *   ineffectiveness) and policy performance (saturation described as
 *   inevitable rather than as contingent policy outcome). The constraint's
 *   classification varies sharply across observer positions: trapped migrants
 *   experience snare, destination workers experience tangled
 *   coordination-extraction, employers experience pure coordination,
 *   organized policy advocates see a temporary problem solvable through
 *   reform, enforcement agencies maintain a degraded ritual, and analytical
 *   observers risk naturalizing contingent policy as immutable economic law.
 *
 * KEY AGENTS:
 *   - Migrant Workers: Primary victims (powerless/trapped) — sunk costs in relocation, visa status dependence, family commitments, psychological investment in destination city create exit barriers
 *   - Destination Wage Earners: Secondary victims (moderate/constrained) — face wage suppression and occupational competition; constrained rather than trapped by relocation costs and local ties
 *   - Destination Employers: Primary beneficiaries (institutional/arbitrage) — access abundant, lower-cost labor; can adjust hiring, relocate, or restructure compensation; full arbitrage optionality
 *   - Incumbent Workers' Political Representation: Secondary beneficiaries (organized/constrained) — benefit from reduced labor competition; constrained by need to balance worker/employer interests
 *   - Migration Policy Advocates: Organized reform actors (organized/constrained) — see saturation as solvable through geographic distribution, wage floors, and work protections; constrained by political implementation barriers
 *   - Immigration Enforcement Apparatus: Institutional maintainers (institutional/arbitrage) — enforce border controls and visa restrictions with high theater and low functional effect; maintain legitimacy despite ineffectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(destination_labor_market_saturation, 0.58).
domain_priors:suppression_score(destination_labor_market_saturation, 0.62).
domain_priors:theater_ratio(destination_labor_market_saturation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(destination_labor_market_saturation, extractiveness, 0.58).
narrative_ontology:constraint_metric(destination_labor_market_saturation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(destination_labor_market_saturation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(destination_labor_market_saturation, snare).
narrative_ontology:human_readable(destination_labor_market_saturation, "Destination Labor Market Saturation").
narrative_ontology:topic_domain(destination_labor_market_saturation, "labor_economics/migration").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(destination_labor_market_saturation, incumbent_workers_destination).
narrative_ontology:constraint_beneficiary(destination_labor_market_saturation, wage_suppression_beneficiaries).
narrative_ontology:constraint_victim(destination_labor_market_saturation, migrant_workers).
narrative_ontology:constraint_victim(destination_labor_market_saturation, destination_wage_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MIGRANT WORKER (SNARE) — Trapped by investment sunk in relocation, family commitments, visa restrictions, and psychological commitment to destination city. Cannot exit without absorbing full cost of failed migration. Bears extraction through wage suppression, occupational downgrading, and lack of alternatives. Maximum experienced extraction due to trapped exit options.
constraint_indexing:constraint_classification(destination_labor_market_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE DESTINATION WAGE EARNER (TANGLED ROPE) — Constrained by labor market location, skill respecialization costs, and family ties. Experiences both coordination benefit (labor supply enables businesses to operate, wages remain stable through wage compression rather than unemployment) and extraction (suppressed wage growth, occupational competition). Moderate exit cost creates constrained rather than trapped classification.
constraint_indexing:constraint_classification(destination_labor_market_saturation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE DESTINATION EMPLOYER COALITION (ROPE) — High exit optionality through labor arbitrage: can shift hiring practices, relocate, or adjust compensation. Benefits from wage suppression and labor supply abundance. Experiences constraint as pure coordination mechanism solving labor shortage problems. Net beneficiary with full arbitrage options.
constraint_indexing:constraint_classification(destination_labor_market_saturation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE MIGRATION POLICY COALITION (SCAFFOLD) — Organized actors (migration advocates, labor unions, housing organizations) see saturation as a temporary policy problem with a sunset: immigration reform, wage floor enforcement, and geographic distribution policies can redirect migrant flows or restructure labor market coordination. Suppression declines over generational horizons as policy solutions mature. Theater ratio is moderate because enforcement mechanisms (visa caps, wage monitoring) are genuinely functional, not purely performative.
constraint_indexing:constraint_classification(destination_labor_market_saturation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE IMMIGRATION ENFORCEMENT SYSTEM (PITON) — Border controls, visa processing, work permit verification persist largely through institutional inertia despite ineffectiveness at reducing net migration or preventing saturation. The enforcement theater (deportation operations, workplace raids) maintains legitimacy even as actual market-clearing function degrades. Theater ratio is high because enforcement activity is decoupled from outcomes — migrants find ways through, around, or despite enforcement mechanisms. The system is maintained because it hasn't been fully replaced, not because it works.
constraint_indexing:constraint_classification(destination_labor_market_saturation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, labor market saturation appears as an immutable economic law: when labor supply exceeds demand, wages fall and unemployment rises, regardless of policy. This perspective naturalizes the constraint as inherent to market dynamics. However, the structural data reveals this as a false summit — the saturation is contingent on policy choices (visa caps, worker protections) and institutional arrangements (geographic concentration of opportunity), not immutable law.
constraint_indexing:constraint_classification(destination_labor_market_saturation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(destination_labor_market_saturation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(destination_labor_market_saturation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(destination_labor_market_saturation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(destination_labor_market_saturation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(destination_labor_market_saturation, TR),
    TR >= 0.70.

:- end_tests(destination_labor_market_saturation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Saturation mechanisms directly suppress wages below market-clearing levels, with measured wage depression of 3-8% in high-saturation sectors. The extraction is not as extreme as pure monopoly/monopsony (which would approach 0.75+) because some workers retain mobility and employers retain competition. The upward trajectory over the interval (0.42 → 0.58) reflects that as saturation deepens, extraction mechanisms intensify — more workers become trapped, wage suppression accelerates, and alternatives shrink. Suppression (0.62): High. Significant barriers include visa restrictions that create legal dependency, sunk costs in relocation that create financial dependency, occupational downgrading that makes return migration irrational, family commitments that create psychological dependency, and credential non-recognition that locks migrants into lower-wage work. Suppression is not absolute (some migration occurs, some return migration happens) but is severe enough to prevent market-clearing responses. Theater ratio (0.48): Moderate. Immigration enforcement (visa controls, workplace inspections, deportations) is substantially performative — enforcement intensity does not correlate with actual migration flows, net wage suppression persists despite enforcement activity, and enforcement targets low-visibility migrants while high-value migrants access exemptions. However, theater is not as dominant as in pure piton structures — some policy tools (wage monitoring, work permit auditing) have partial functional effects. The moderate theater indicates a constraint in transition: policies have real but declining effectiveness as saturation deepens and enforcement complexity increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across institutional positions. The trapped migrant sees pure extraction (Snare) — wages suppressed, alternatives eliminated, costs internalized, no exit available. The destination worker sees mixed coordination-extraction (Tangled Rope) — wage suppression is a cost, but labor supply enables economic vitality that creates some employment opportunities and services. The employer sees pure coordination (Rope) — the constraint solves their labor supply problem with minimal resistance; they experience saturation as beneficial market condition. The policy coalition sees a temporary problem with solutions (Scaffold) — geographic distribution of migrants, labor protections, and wage floors can restructure the constraint's extractiveness within a generation. The enforcement apparatus sees a degraded ritual (Piton) — enforcement theater maintains legitimacy despite declining functional effect; the apparatus persists through inertia and funding dependence. The analytical observer risks a false summit (Mountain) — describing saturation as inevitable economic law rather than contingent policy outcome. The perspectival gap reveals that classification outcome is entirely determination by structural position: those trapped experience snares, those with arbitrage experience coordination, those with organized power experience solvable problems, and those maintaining degraded institutions experience ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants experience maximum directionality (d ≈ 0.95) because they are pure extraction targets — they receive suppressed wages, constrained opportunities, and bear psychological costs of failed migration. Their trapped exit status means f(d) approaches maximum (f(0.95) ≈ 1.42), amplifying experienced extractiveness. Destination wage earners experience intermediate directionality (d ≈ 0.55) because they experience both cost (wage suppression through increased labor supply) and benefit (labor abundance enables economic activity; unemployment risk is distributed rather than concentrated). Their constrained exit options place them at moderate d. Employers experience low directionality (d ≈ 0.08) because they are net beneficiaries — labor abundance reduces their hiring costs and increases their exit optionality. Arbitrage exit status means f(d) can be negative or near-zero, reducing effective extraction to near-zero or negative values (chi becomes coordination rather than extraction). The scaffolding coalition experiences intermediate directionality (d ≈ 0.40) because they are organized actors with real power to influence policy but face implementation constraints. The enforcement apparatus experiences low directionality (d ≈ 0.05) because enforcement maintains beneficiary status (funding, institutional legitimacy, arbitrage mobility) while experiencing minimal cost from saturation dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that saturation classification depends entirely on institutional position and exit options rather than on any objective feature of saturation itself. The same saturation event is simultaneously: (1) A Snare for trapped migrants with no exit. (2) A Tangled Rope for destination workers with constrained mobility. (3) A Rope for employers with full arbitrage. (4) A Scaffold for organized policy actors who can reshape the constraint over generational time horizons. (5) A Piton for enforcement institutions maintaining degraded control mechanisms. (6) A potential false Mountain for observers who naturalize contingent policy as inevitable law. No single classification is 'correct' — each is the authentic structural reality from that position. The mandatrophy is resolved not by choosing one type but by recognizing that the presheaf of perspectives over all institutional positions fully determines the constraint's character. The analytical observer should not ask 'what type is saturation?' but rather 'what is the distribution of experienced constraint types across the population?' That distribution IS the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_suppression_mechanism_ambiguity,
    'Is measured wage suppression a direct effect of labor supply saturation or an indirect effect of reduced bargaining power and union density?',
    'Longitudinal wage analysis controlling for worker skill, education, and experience; comparison of wage trajectories in high-saturation vs low-saturation labor markets with similar institutional structures',
    'If direct supply effect: saturation mechanism is fundamentally extractive (snare). If institutional effect: wage suppression is contingent on labor law and union strength, suggesting scaffold (policy-solvable) rather than snare (structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_mechanism_ambiguity, empirical, 'Whether wage suppression is direct supply effect or institutional bargaining effect').

omega_variable(
    migrant_alternative_availability,
    'Do trapped migrants have material ability to return to origin labor market or migrate to third destinations, or is the trap primarily psychological/sunk-cost driven?',
    'Post-saturation migration flow analysis; tracking of return migration and onward migration rates; interview data on perceived vs actual barriers to exit',
    'If material barriers dominate (visa restrictions, no savings for relocation): trapped classification is correct. If psychological barriers dominate (identity investment, family commitments): reclassify as identity_locked with constrained structural options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(migrant_alternative_availability, empirical, 'Whether migrant trap is material, psychological, or both').

omega_variable(
    labor_market_clearing_threshold,
    'What threshold of labor supply abundance constitutes extractive saturation vs normal competitive labor market functioning?',
    'Analysis of labor market tightness indices (unemployment rate, vacancy ratios, wage growth rates) across comparable cities; identification of point where individual migrant agency dissolves',
    'If threshold is high (20%+ unemployment in destination sector): most saturated markets are still somewhat extractive. If threshold is low (5%+ above trend): more markets classified as snares.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_clearing_threshold, conceptual, 'Threshold distinguishing competitive labor market from extractive saturation').

omega_variable(
    coordination_value_in_saturation,
    'Does labor market saturation provide genuine coordination value (labor supply enables businesses, public services, and infrastructure that would not exist without migrants) or is this coordination claim merely cover for extraction?',
    'Counterfactual analysis: what would destination economy look like without migrant labor? Comparison of business survival rates, service capacity, wage costs in high-saturation vs hypothetical low-saturation scenarios',
    'If genuine coordination: tangled_rope and scaffold classifications justified. If cover for extraction: reclassify more perspectives as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_value_in_saturation, empirical, 'Whether saturation provides genuine coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(destination_labor_market_saturation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dlms_tr_t0, destination_labor_market_saturation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dlms_tr_t10, destination_labor_market_saturation, theater_ratio, 10, 0.42).
narrative_ontology:measurement(dlms_tr_t20, destination_labor_market_saturation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(dlms_be_t0, destination_labor_market_saturation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dlms_be_t10, destination_labor_market_saturation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dlms_be_t20, destination_labor_market_saturation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(destination_labor_market_saturation, resource_allocation).
narrative_ontology:affects_constraint(destination_labor_market_saturation, wage_suppression_mechanism).
narrative_ontology:affects_constraint(destination_labor_market_saturation, occupational_downgrading).
narrative_ontology:affects_constraint(destination_labor_market_saturation, destination_housing_cost_inflation).

% DUAL FORMULATION NOTE:
% Destination labor market saturation is upstream of multiple sectoral constraints (wage suppression, occupational downgrading, housing inflation) but represents a distinct structural constraint operating through labor supply abundance and exit barriers. Decomposition strategy: saturation (this story, ε=0.58) is the general mechanism; wage suppression in specific sectors (ε=0.42-0.65 depending on sector), occupational downgrading (ε=0.52), and housing cost inflation (ε=0.68) are downstream manifestations with sector-specific extraction values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
