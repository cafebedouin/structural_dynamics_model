% ============================================================================
% CONSTRAINT STORY: hu_2026_electoral_parity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hu_2026_electoral_parity, []).

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
 *   constraint_id: hu_2026_electoral_parity
 *   human_readable: The 2026 Hungarian Mixed-Member Majoritarian Electoral System Inertia
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   Hungary's mixed-member majoritarian electoral system combines
 *   single-member constituency elections (199 seats) with a compensatory
 *   national list mechanism designed to correct geographic vote
 *   concentration. The system's structural logic rewards geographic
 *   clustering: when a party wins constituency seats, those votes are added
 *   back to the national pool for list compensation, amplifying the advantage
 *   for geographically concentrated parties. The Fidesz-KDNP coalition's
 *   support base is heavily concentrated in rural and small-town
 *   constituencies, while opposition parties' voters are dispersed across
 *   urban centers and regionally fragmented ethnic communities. This
 *   geography-to-outcome mapping creates an apparent paradox: the system is
 *   justified as a 'proportionality corrective' (coordination rationale) yet
 *   systematically amplifies the majority coalition's advantage (extraction
 *   logic). The constraint exhibits six distinct classifications depending on
 *   observer position, revealing a genuine tangled rope structure where
 *   coordination and extraction are inseparable. The theater_ratio (0.58)
 *   reflects that formal commission procedures, constitutional safeguards,
 *   and proportionality language create legitimacy theater while the
 *   mathematical structure (winner compensation + geographic configuration)
 *   pre-determines outcomes.
 *
 * KEY AGENTS:
 *   - Fidesz-KDNP Coalition: Primary beneficiary (institutional/arbitrage) — geographic concentration in support base is amplified by winner-compensation formula; achieves supermajorities despite modest vote-share advantages
 *   - Fragmented Opposition Parties: Primary victim (powerless/trapped) — vote dispersal across constituencies produces minimal list compensation; face structural disadvantage that cannot be remedied without rule change (requiring majority control they cannot achieve)
 *   - Mid-Tier Opposition Parties: Secondary actor (moderate/constrained) — regional concentration (ethnic minorities, regional strongholds) enables some list-compensation benefits, but national-level coordination faces barriers
 *   - European Union / Democratic Oversight: Organized actor (organized/mobile) — sees system as temporary coordination failure resolvable through electoral standards harmonization; exerts pressure but lacks enforcement mechanism short of funding conditionality
 *   - Hungarian Constitutional Court / Electoral Commission: Institutional referee (institutional/arbitrage) — nominally supervises fairness but maintains system through procedural legitimacy theater; observes degradation but lacks institutional incentive to challenge majority coalition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing winner-compensation as inherent to mixed-member design; false summit risk when treating contingent choice as mathematical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hu_2026_electoral_parity, 0.38).
domain_priors:suppression_score(hu_2026_electoral_parity, 0.48).
domain_priors:theater_ratio(hu_2026_electoral_parity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hu_2026_electoral_parity, extractiveness, 0.38).
narrative_ontology:constraint_metric(hu_2026_electoral_parity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hu_2026_electoral_parity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hu_2026_electoral_parity, tangled_rope).
narrative_ontology:human_readable(hu_2026_electoral_parity, "The 2026 Hungarian Mixed-Member Majoritarian Electoral System Inertia").
narrative_ontology:topic_domain(hu_2026_electoral_parity, "political/electoral_systems").

domain_priors:requires_active_enforcement(hu_2026_electoral_parity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hu_2026_electoral_parity, fidesz_orban_coalition).
narrative_ontology:constraint_beneficiary(hu_2026_electoral_parity, constituency_victors).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, opposition_parties).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, proportional_representation_principle).
narrative_ontology:constraint_victim(hu_2026_electoral_parity, coalition_fragmentation_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRAGMENTED OPPOSITION (SNARE) — Multiple opposition parties lack structural exit from the mixed-member system's voter-suppression arithmetic. Scattered votes across constituency lines produce minimal list-compensation because the model rewards geographic concentration. Opposition parties cannot reorganize constituencies or rewrite electoral mathematics without legislative change (which requires majority control they cannot achieve under these rules). Trapped in a system designed to penalize their dispersed voter base.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIDESZ-KDNP COALITION (ROPE) — Experiences the system as pure coordination mechanism. Winner-compensation formula reinforces geographic concentration already present in coalition support patterns. Coalition benefits from both direct constituency mandates AND list-compensation, creating a virtuous cycle. System enables strategic coordination: geographic concentration is rewarded, and the arithmetic produces a durable majority without structural coercion. Arbitrage exit option because coalition can operate across both tiers.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-TIER OPPOSITION PARTIES (TANGLED ROPE) — Experience simultaneous coordination benefits and extraction losses. The system enables representation through list compensation for parties with regional concentration (e.g., ethnic minority parties), providing coordination benefits. However, national-level opposition parties face mixed outcomes: they benefit from list compensation in some cycles but are structurally disadvantaged by vote dispersal. Constrained exit because reform requires supermajority or coalition defection. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EUROPEAN DEMOCRATIC OVERSIGHT (SCAFFOLD) — EU institutions and international observer networks see Hungarian electoral rules as a temporary coordination failure that will be resolved through EU electoral standards harmonization or generational political realignment. This perspective anticipates a sunset: either EU conditionality forces reform within 10-15 years, or demographic/political change undermines the geographic patterns that enable the system's extractive logic. Mobile exit because external pressure can alter rules; sunset is credible because EU democratic standards are rising.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT / ELECTORAL COMMISSION (PITON) — Institutions nominally responsible for electoral system administration observe that the mixed-member system is largely performative in its 'fairness' guarantees. Constitutional safeguards against gerrymandering are underutilized; commission rulings on vote-shifting procedures are routinized theater. The system persists through institutional inertia despite acknowledged structural problems. High theater_ratio because formal procedures create appearance of fairness review while material outcomes are predetermined by the mathematical structure. Institutional actors maintain the system via arbitrage (preserved legitimacy) even though they recognize degradation.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure mathematical perspective, mixed-member systems with winner compensation are structurally inherent features of electoral design space. The compensation formula follows necessarily from the goal of combining geographic representation with proportionality. This view naturalizes the system as a technical necessity rather than a contingent political choice. However, this perspective risks false summitry: the choice to use winner-compensation rather than loser-compensation, the specific geographic constituency boundaries, and the list-threshold rules are contingent political decisions, not mathematical inevitabilities. The engine should flag this as naturalization.
constraint_indexing:constraint_classification(hu_2026_electoral_parity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hu_2026_electoral_parity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hu_2026_electoral_parity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hu_2026_electoral_parity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hu_2026_electoral_parity, TR),
    TR >= 0.70.

:- end_tests(hu_2026_electoral_parity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The system extracts measurable advantage for the Fidesz-KDNP coalition: empirically, parties with 45-50% national vote share regularly achieve 55-65% seat shares due to geographic concentration amplification. However, extraction is not as severe as a pure snare (ε≥0.46) because the mechanism operates through relatively transparent mathematical formulas rather than hidden coercion. Opposition voters understand the mechanism; the extraction is structural but visible. Suppression (0.48): Moderate. Barriers to opposition victory include structural vote-dispersal (a real constraint) but not violent coercion or vote denial. The suppression is mathematical-structural rather than coercive. Theater ratio (0.58): Moderate-high. The system maintains legitimacy through formal procedures (Constitutional Court review, commission transparency, published algorithms) while achieving predetermined outcomes through mathematical structure. Formal safeguards create appearance of neutrality; the theater has increased as gap between procedural fairness and outcome bias has widened (measurement progression 0.42 → 0.58).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is fundamental and structural. The Fidesz-KDNP coalition genuinely experiences the system as coordination (Rope): they are solving the problem of translating geographic support into parliamentary seats. Fragmented opposition genuinely experiences pure extraction (Snare): they have no exit and bear full cost. Mid-tier parties with geographic niches experience the genuine mixed structure (Tangled Rope): benefits from regional concentration, costs from national dispersal. The Constitutional Court experiences performative degradation (Piton): they see the system as fair in form but fixed in outcome, maintained through institutional inertia. EU oversight sees a sunset (Scaffold): generational political realignment or EU pressure will eventually force reform. The analytical observer risks false naturalization (Mountain): treating winner-compensation as a mathematical necessity when it is a contingent institutional choice. The perspectival gap is not a measurement error — it reflects genuine structural asymmetry in how the constraint operates for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position within the system. Fidesz-KDNP coalition members occupy beneficiary + arbitrage position: low d → negative effective extraction (they experience the system as a coordination benefit). Opposition powerless agents occupy victim + trapped position: high d → high effective extraction (they experience maximum constraint). Mid-tier moderate parties occupy mixed positions: some regional concentration enables benefits (lower d), but national-level marginalization creates costs (higher d), producing moderate d values and mixed classification. EU oversight occupies organized + mobile position: external pressure creates exit options that lower perceived extraction. The Mathematical structure determines directionality: geographic concentration is the primary driver. The same rule (winner-compensation) functions as coordination for geographically clustered beneficiaries and extraction for dispersed victims. The directionality pipeline correctly captures this asymmetry through power + exit + beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that 'is it coordination or extraction?' is the wrong question. The system IS simultaneous coordination and extraction — but for different agents. The mixed-member structure genuinely solves a coordination problem (how to combine geographic representation with proportionality) in a way that works for geographically concentrated beneficiaries. The same structure functions as pure extraction for dispersed victims. The tangled rope classification is not a compromise or hedging; it is the accurate structural description. The theater (formal procedural safeguards) is not hiding a true underlying type; the theater itself is part of the constraint's mechanism — it maintains legitimacy for the beneficiary coalition while preventing opposition exit. The mandatrophy is resolved by showing that both aspects (coordination + extraction) are essential to understanding the system's persistence: it persists because it coordinates for the majority AND extracts from the opposition, creating a stable equilibrium that benefits the coalition even if it violates proportional representation norms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vote_efficiency_sensitivity,
    'How sensitive is the observed seat-vote gap to small shifts in geographic voter distribution (±3-5% constituency-level swing)?',
    'Historical counterfactual analysis: recompute seat allocations under 2018, 2022 election parameters with ±3% redistribution of votes across constituencies; measure seat-change sensitivity',
    'If highly sensitive (>10% seat swing from 3% vote shift): system is designed for maximum extraction by geographic configuration. If robust (seat swing <3%): mathematical structure is less extractive than political geography.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vote_efficiency_sensitivity, empirical, 'Sensitivity of seat outcomes to voter distribution changes').

omega_variable(
    opposition_coalition_viability,
    'Could a coalition of currently fragmented opposition parties achieve parliamentary majority through geographic coordination alone, without electoral rule change?',
    'Electoral modeling: simulate 2026 outcome under hypothetical opposition alliance with geographically concentrated candidacies; identify minimum threshold for coalition seat majority',
    'If viable (<25% additional coordination needed): opposition has exit option through strategic alliance, lowering perception of trapment. If not viable (>40% additional coordination needed): opposition truly lacks structural exit short of electoral reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_viability, empirical, 'Whether opposition coalition coordination can overcome system geometry').

omega_variable(
    eu_electoral_standards_timeline,
    'What is the realistic timeline for EU conditionality to force Hungarian electoral rule change toward proportional representation?',
    'Tracking of EU communications, funding conditionality language, and historical precedent for forcing member-state electoral reform (Poland, Romania cases); assess political will and institutional mechanisms',
    'If timeline 5-10 years: scaffold sunset is credible and imminent. If timeline >20 years: scaffold is aspirational rather than structural; system persists through institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_electoral_standards_timeline, empirical, 'Timeline for EU-driven electoral reform in Hungary').

omega_variable(
    list_compensation_actual_function,
    'Does list-compensation actually function as a proportionality corrective mechanism, or does it systematically reinforce geographic concentration?',
    'Historical analysis of list-compensation allocations across 2014-2022 elections; measure whether list seats reduce or amplify geographic clustering of seat distribution',
    'If corrective: system has genuine coordination function (Rope classification justified). If reinforcing: list compensation is extractive theater masking geographic concentration logic (Snare classification amplified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(list_compensation_actual_function, empirical, 'Whether list compensation corrects or amplifies geographic clustering').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hu_2026_electoral_parity, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hu_2026_tr_t0, hu_2026_electoral_parity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hu_2026_tr_t4, hu_2026_electoral_parity, theater_ratio, 4, 0.52).
narrative_ontology:measurement(hu_2026_tr_t8, hu_2026_electoral_parity, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(hu_2026_be_t0, hu_2026_electoral_parity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hu_2026_be_t4, hu_2026_electoral_parity, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(hu_2026_be_t8, hu_2026_electoral_parity, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hu_2026_electoral_parity, resource_allocation).
narrative_ontology:affects_constraint(hu_2026_electoral_parity, hungarian_media_market_concentration).
narrative_ontology:affects_constraint(hu_2026_electoral_parity, eu_democratic_standards_harmonization).
narrative_ontology:affects_constraint(hu_2026_electoral_parity, central_european_electoral_drift).

% DUAL FORMULATION NOTE:
% The 2026 Hungarian electoral system can be decomposed into two distinct constraints: (1) the mathematical winner-compensation formula itself (lower ε, more mountain-like), and (2) the geographic configuration of voter distribution that amplifies the formula's extractive effects (higher ε, more snare-like). This story treats them as integrated because the formula cannot be separated from the geography in the Hungarian case — the constraint's persistence depends on both. The network link to 'central_european_electoral_drift' captures the broader structural tendency across post-communist democracies toward mixed-member systems that advantage dominant coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hu_2026_electoral_parity, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
