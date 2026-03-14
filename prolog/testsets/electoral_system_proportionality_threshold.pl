% ============================================================================
% CONSTRAINT STORY: electoral_system_proportionality_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electoral_system_proportionality_threshold, []).

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
 *   constraint_id: electoral_system_proportionality_threshold
 *   human_readable: Electoral System Proportionality Threshold
 *   domain: political_systems/electoral_mechanics
 *
 * SUMMARY:
 *   Electoral system proportionality thresholds create a structural tension
 *   between the genuine need for government-formation simplicity
 *   (coordination function) and the systematic suppression of political
 *   alternatives (extraction function). A proportionality threshold—a minimum
 *   vote share required to gain parliamentary seats—operates as pure
 *   coordination from the perspective of established parties (it simplifies
 *   coalition arithmetic and ensures governable majorities), but as pure
 *   extraction from the perspective of sub-threshold parties and their voters
 *   (their votes are discounted or wasted regardless of their preference
 *   intensity). The constraint exhibits tangled-rope structure: it
 *   coordinates government formation (genuine function) while extracting from
 *   minorities (asymmetric harm). The theater ratio (0.48) reflects that the
 *   threshold maintains moderate performative legitimacy through the language
 *   of 'governing stability' and 'preventing fragmentation,' but these are
 *   contestable claims about the necessity of the specific threshold
 *   level—lower thresholds in other democracies demonstrate feasibility with
 *   alternative coalition norms. The extractiveness has increased over the
 *   40-year interval as party fragmentation has accelerated, raising the
 *   effective suppression without changing the formal threshold.
 *
 * KEY AGENTS:
 *   - Minor Party Voter: Trapped victim (powerless/trapped) — faces strategic voting pressure or vote waste; no exit option
 *   - Sub-Threshold Movement: Trapped victim (powerless/trapped) — cannot convert support into legislative power; cannot grow without first clearing discontinuity
 *   - Regional Minority Party: Moderate beneficiary-victim (moderate/constrained) — may clear threshold regionally; mixed coordination (local aggregation) and extraction (national discounting)
 *   - Established Party (Above Threshold): Primary beneficiary (institutional/arbitrage) — reliably converts vote share to seat share; experiences threshold as governance-enabling coordination
 *   - Governing Coalition: Primary beneficiary (institutional/arbitrage) — threshold simplifies coalition math and enables stable majorities; experiences constraint as coordination solution
 *   - Electoral Reform Coalition: Organized challenger (organized/constrained) — sees threshold as temporary and changeable through mixed-member systems or lower thresholds; has structural agency
 *   - Electoral Commission: Institutional manager (institutional/arbitrage) — mechanically enforces threshold; maintains performative legitimacy but does not drive persistence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the threshold value as inherent to representative democracy when only the discontinuity (not the level) is inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electoral_system_proportionality_threshold, 0.58).
domain_priors:suppression_score(electoral_system_proportionality_threshold, 0.62).
domain_priors:theater_ratio(electoral_system_proportionality_threshold, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electoral_system_proportionality_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(electoral_system_proportionality_threshold, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(electoral_system_proportionality_threshold, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electoral_system_proportionality_threshold, tangled_rope).
narrative_ontology:human_readable(electoral_system_proportionality_threshold, "Electoral System Proportionality Threshold").
narrative_ontology:topic_domain(electoral_system_proportionality_threshold, "political_systems/electoral_mechanics").

domain_priors:requires_active_enforcement(electoral_system_proportionality_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electoral_system_proportionality_threshold, established_parties).
narrative_ontology:constraint_beneficiary(electoral_system_proportionality_threshold, plurality_winners).
narrative_ontology:constraint_victim(electoral_system_proportionality_threshold, minor_parties).
narrative_ontology:constraint_victim(electoral_system_proportionality_threshold, electoral_representation_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINOR PARTY VOTER (SNARE) — Structurally trapped by the proportionality threshold. A voter supporting a sub-threshold party faces a choice: vote for a preferred party and contribute zero legislative seats (wasted vote), or strategically vote for a threshold-viable party they do not prefer. The voter cannot exit; withdrawal from participation is the only option, and participation withdrawal carries its own political cost.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUB-THRESHOLD MOVEMENT (SNARE) — A political movement below the proportionality threshold cannot convert electoral support into legislative power. The constraint extracts the movement's voter base (siphoning votes to major parties through strategic voting) and provides no exit: growing large enough to breach the threshold requires crossing a discontinuity that the threshold itself prevents. Maximum extraction with no alternatives.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL MINORITY PARTY (TANGLED ROPE) — A party with strong regional concentration (e.g., Welsh Plaid Cymru, Basque ETA successor parties) may clear the proportionality threshold in their region while failing nationally. They benefit from the coordination function (local representation is aggregated into national power) but bear extraction costs (their votes are discounted at the national scale and their coalition power is constrained by their sub-threshold national standing). Mixed coordination and extraction.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED PARTY (ROPE) — A party reliably above the proportionality threshold experiences the constraint as a coordination mechanism: it reliably converts vote share to seat share, enabling coalition formation and government construction. The threshold solves the collective action problem of government formation (too many tiny parties makes governance difficult). Net beneficiary — the constraint subsidizes their seat representation relative to raw vote share.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNING COALITION (ROPE) — The threshold enables stable majorities by suppressing splinter parties. A coalition above the threshold experiences the constraint as governance-enabling coordination: it simplifies legislative arithmetic, reduces veto players, and makes coalition agreements feasible. The threshold is a coordination solution to a genuine problem (too many parties makes government formation intractable).
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized actors (civil society organizations, small parties, some academics) see the proportionality threshold as a temporary coordination problem with a structural sunset. Mixed-member proportional systems, lower thresholds, or regional representation systems create alternative pathways. The reform coalition has agency (they can change the law) and a visible exit: restructured electoral mechanics. Low effective extraction because the constraint is perceived as changeable by organized action.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ELECTORAL COMMISSION (PITON) — The proportionality threshold is largely performative at the administrative level. The electoral commission enforces the threshold mechanically (counting votes, computing seat allocation) but this administrative function is not why the threshold persists. It persists through institutional inertia: changing thresholds requires legislative action, which major parties above the threshold have no incentive to support. The commission's role is degraded ritual — procedural enforcement of a constraint that majors parties maintain for coordination benefits, not because the commission's function requires it.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, some threshold effect is inherent to representative democracy itself: any electoral system with finite legislative seats and continuous voter distributions must map a continuum of votes onto a discrete number of seats, creating a discontinuity. This perspective risks naturalizing the specific threshold value (3%, 5%, 10%) as an inherent property of governance. However, the structural data reveals this as a false summit: the threshold value is politically contingent; the discontinuity is not.
constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electoral_system_proportionality_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electoral_system_proportionality_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electoral_system_proportionality_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electoral_system_proportionality_threshold, TR),
    TR >= 0.70.

:- end_tests(electoral_system_proportionality_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The threshold creates a discontinuous penalty for sub-threshold parties—their votes are entirely discounted below the threshold (100% vote-to-seat conversion failure), while above-threshold parties receive disproportionate seat gains. Over the 40-year measurement interval, extractiveness has increased from 0.35 to 0.58 as party-system fragmentation has accelerated: more parties now fall below the threshold, and the extraction from each trapped party grows. Suppression (0.62): Moderate-high. Sub-threshold voters face genuine barriers to independent party participation (strategic voting pressure, explicit discouragement, media blackout of non-viable parties) and face no exit—withdrawal from participation is costly, and alternative votes are preempted. But suppression is not total; voters can still vote for preferred parties at the cost of vote waste, and periodic movements do cross thresholds through mobilization surges. Theater ratio (0.48): Moderate. The threshold's legitimacy claim rests on 'governing stability' and 'preventing fragmentation'—claims about why thresholds are necessary. But these claims are contestable: lower-threshold systems (e.g., 2%) exist in functioning democracies (Netherlands, Denmark), and their governance is stable under multi-party coalition norms. The performative element is moderate: the threshold has real effects (it does simplify coalition arithmetic) but the specific level is not determined by mechanical governance requirements.
 *
 * PERSPECTIVAL GAP:
 *   The major perspectival gap separates threshold-beneficiaries (Rope perspectives: established parties, coalitions) from threshold-victims (Snare perspectives: minor parties, sub-threshold voters). Both groups perceive the same structural object (the threshold), but their classifications differ by two constraint types—four steps on the severity scale. This gap reveals the constraint's core tension: what established parties experience as coordination (simplifying coalition math) is what sub-threshold parties experience as extraction (suppressing their alternatives). The reform coalition's Scaffold perspective occupies a structural middle ground—they acknowledge the threshold's current function (temporary coordination burden) while asserting its changeability (sunset via electoral reform). The piton and mountain perspectives reveal how institutional inertia and naturalizing analysis enable the extraction to persist: the commission maintains it mechanically, and analytical observers risk explaining it as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural position relative to the threshold. Agents above the threshold (institutional parties, governing coalitions) have low d (they are beneficiaries—the constraint subsidizes their representation). Agents below the threshold (minor party voters, sub-threshold movements) have high d (they are victims—the constraint extracts from them through vote suppression). Regional parties have intermediate d (they are simultaneously beneficiaries regionally and victims nationally). Organized reformers have lower d despite their reformist stance because they possess agency and exit options. The electoral commission has low d (institutional beneficiary status through proximity to enforcement) despite their performative role. The analytical observer has d ≈ 0.72 (observer canonical d) but risks an identity lock if their academic identity becomes fused with the 'governing stability' narrative (explaining why thresholds are necessary rather than questioning the specific threshold level).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy of electoral thresholds is the tension between genuine coordination function and systematic extraction. The constraint would be pure Rope if (1) the threshold level were optimized for governance stability rather than incumbent protection, and (2) lower thresholds produced ungovernably fragmented legislatures. It would be pure Snare if the governance-stability claim were entirely pretextual. The tangled-rope classification reflects ambiguity: some threshold effect IS necessary for coordination (the discontinuity forces choices that prevent infinite fragmentation), but the specific level (3%, 5%, 10%) is politically chosen and biased toward protecting majors. The omega variables operationalize this: if research confirms that lower thresholds remain governable (omega 3), the extraction case strengthens. If research confirms that strategic voting is suppressing sub-threshold votes beyond their true preference (omega 2), the extraction is larger than vote counts reveal. The constraint avoids false naturalization (mountain) by centering the contingency of the threshold value while acknowledging the inevitability of some discontinuity in vote-to-seat mapping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_level_optimality,
    'Is the specific threshold level (3%, 5%, 10% depending on jurisdiction) optimized for governance stability or calibrated to protect incumbent parties?',
    'Comparative analysis across democracies: correlate threshold level with government stability (coalition count, legislative duration) and identify whether thresholds were raised historically in response to party fragmentation or incumbent anxiety',
    'If optimized for stability: lower thresholds remain feasible if compensated with stronger coalition-formation norms. If calibrated to protect incumbents: the threshold is pure extraction disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_level_optimality, empirical, 'Whether threshold is optimized for governance or incumbent protection').

omega_variable(
    strategic_voting_magnitude,
    'How much of the sub-threshold vote is genuine supporter preference vs. forced strategic voting?',
    'Exit polling and ballot-choice analysis: compare stated preferences in confidential surveys to actual ballots cast; correlation between threshold proximity and vote switching',
    'If strategic voting dominates: the true sub-threshold support is higher than observed votes (extraction is larger). If preference is genuinely aligned with threshold parties: the observed distribution is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_voting_magnitude, empirical, 'Strategic voting as hidden vote suppression').

omega_variable(
    coalition_formation_feasibility,
    'Do lower-threshold electoral systems (e.g., 2% vs 5%) actually produce ungovernable legislatures or do alternative coalition-formation norms (multi-party coalitions, confidence-and-supply) enable stable governance?',
    'Longitudinal governance stability analysis: compare government duration, legislative productivity, and executive-legislative conflict across electoral systems with different thresholds; control for external factors (party-system fragmentation, regional polarization)',
    'If alternative norms work: the threshold is not necessary for coordination (extraction is not justified). If lower thresholds destabilize: the threshold is necessary coordination overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_formation_feasibility, empirical, 'Whether lower thresholds remain governable under alternative norms').

omega_variable(
    representation_accuracy_measurement,
    'What is the normative threshold for acceptable electoral distortion? At what vote-to-seat disparity does a threshold cease to be coordination overhead and become undemocratic extraction?',
    'Democratic theory specification: formalize acceptable proportionality ranges and assess whether achieved distortion exceeds them; international comparison against best-practice proportionality metrics',
    'If distortion exceeds norms: threshold is definitively extractive. If within acceptable range: threshold remains deniable as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(representation_accuracy_measurement, conceptual, 'Normative threshold for acceptable electoral distortion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electoral_system_proportionality_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_prop_tr_t0, electoral_system_proportionality_threshold, theater_ratio, 0, 0.32).
narrative_ontology:measurement(elec_prop_tr_t20, electoral_system_proportionality_threshold, theater_ratio, 20, 0.42).
narrative_ontology:measurement(elec_prop_tr_t40, electoral_system_proportionality_threshold, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(elec_prop_be_t0, electoral_system_proportionality_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(elec_prop_be_t20, electoral_system_proportionality_threshold, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(elec_prop_be_t40, electoral_system_proportionality_threshold, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electoral_system_proportionality_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(electoral_system_proportionality_threshold, strategic_voting_behavioral_lock).
narrative_ontology:affects_constraint(electoral_system_proportionality_threshold, political_fragmentation_stabilization).
narrative_ontology:affects_constraint(electoral_system_proportionality_threshold, coalition_formation_bottleneck).

% DUAL FORMULATION NOTE:
% The proportionality threshold decomposes into two structurally distinct constraints: (1) the geometric discontinuity inherent to mapping a vote continuum onto discrete seats (inevitable coordination problem, low ε), and (2) the specific threshold level chosen to resolve that discontinuity (politically contingent, extraction-prone, higher ε). This story addresses the combined constraint. Upstream influences include party-system fragmentation and coalition-formation norms; downstream effects include strategic voting dynamics and voter suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electoral_system_proportionality_threshold, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
