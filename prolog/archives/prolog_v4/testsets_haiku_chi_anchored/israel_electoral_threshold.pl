% ============================================================================
% CONSTRAINT STORY: israel_electoral_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_electoral_threshold, []).

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
 *   constraint_id: israel_electoral_threshold
 *   human_readable: The 3.25% Knesset Electoral Threshold
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   Israel's 3.25% electoral threshold represents a structural mechanism that
 *   consolidates political power among large parties while suppressing
 *   representation of marginal movements and minority interests. The
 *   constraint operates as a snare for parties and constituencies below the
 *   threshold (victims: marginal political movements, minority ethnic
 *   parties, single-issue advocates) while functioning as coordination and
 *   extraction mechanisms for established parties and kingmakers
 *   (beneficiaries: large established parties, coalition kingmakers). The
 *   threshold was introduced at 1% in 1949 and has been raised three times
 *   (to 2% in 1992, 3.25% in 2014), each time justified as necessary for
 *   coalition stability but functioning as rent-seeking by entrenched
 *   political actors. The constraint's extractiveness (0.52) reflects the
 *   asymmetric leverage it provides to parties just above the threshold in
 *   coalition negotiations, while suppression (0.68) reflects the absence of
 *   alternatives for sub-threshold constituencies. The theater ratio (0.38)
 *   is moderate: the stability justification is partially genuine but
 *   increasingly mythologized as thresholds have risen beyond demonstrated
 *   necessity.
 *
 * KEY AGENTS:
 *   - Marginal Political Movements: Primary victims (powerless/trapped) — receive 2-3% of votes but zero Knesset seats; no exit option; suffer extraction and suppression
 *   - Minority Ethnic Parties: Secondary victims (moderate/constrained) — Arab-Israeli, Druze parties above threshold but below coalition-entry power; constrained exit via merger
 *   - Large Established Parties: Primary beneficiaries (institutional/arbitrage) — protected from splinter competition; experience threshold as coordination mechanism; can arbitrage coalition position
 *   - Coalition Kingmaker Parties: Secondary beneficiaries (powerful/mobile) — mid-sized parties (5-8%) gain disproportionate power through kingmaking; active enforcement of threshold maintains their leverage
 *   - Central Elections Committee: Institutional enforcer (institutional/arbitrage) — mechanically enforces threshold; sees enforcement as functional necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_electoral_threshold, 0.52).
domain_priors:suppression_score(israel_electoral_threshold, 0.68).
domain_priors:theater_ratio(israel_electoral_threshold, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, 0.52).
narrative_ontology:constraint_metric(israel_electoral_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(israel_electoral_threshold, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_electoral_threshold, snare).
narrative_ontology:human_readable(israel_electoral_threshold, "The 3.25% Knesset Electoral Threshold").
narrative_ontology:topic_domain(israel_electoral_threshold, "political/electoral_systems").

domain_priors:requires_active_enforcement(israel_electoral_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, large_established_parties).
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, coalition_kingmakers).
narrative_ontology:constraint_victim(israel_electoral_threshold, marginal_political_movements).
narrative_ontology:constraint_victim(israel_electoral_threshold, minority_ethnic_parties).
narrative_ontology:constraint_victim(israel_electoral_threshold, single_issue_advocates).
narrative_ontology:constraint_victim(israel_electoral_threshold, new_party_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL POLITICAL MOVEMENT (SNARE) — A party receiving 2.8% of votes has zero parliamentary representation despite genuine constituency support. No exit option: cannot reorganize geographically, cannot negotiate entry, cannot appeal threshold directly. Bears full suppression cost (wasted vote) and extraction loss (denied representation). d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY ETHNIC PARTY (SNARE) — Arab-Israeli, Druze, or other minority-interest parties face dual constraint: the threshold itself plus coalition mathematics that excludes them from power-sharing. Can attempt coalition-building, but excluded by de facto political norms. Constrained exit (can merge with larger parties, losing identity). d≈0.82, f(d)≈1.28, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED COALITION PARTY (ROPE) — Threshold guarantees stable coalition formation: barriers to entry protect incumbents from splinter parties fragmenting the Knesset beyond govern-ability. Experiences the threshold as coordination: reduces coalition negotiation complexity. d≈0.08, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Net beneficiary via arbitrage (can negotiate coalitions with reduced threat of fragmentation).
constraint_indexing:constraint_classification(israel_electoral_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COALITION KINGMAKER PARTY (TANGLED ROPE) — A mid-sized party (5-8% of votes) above the threshold gains disproportionate power via coalition kingmaking. Experiences both coordination (threshold enables stable coalition) and extraction (can demand ministerial posts, budgets, policy concessions beyond their vote share). Mobile exit (can join other coalitions, but coalition power is significant incentive to stay). Requires active enforcement of threshold to maintain kingmaker leverage. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16. Mixed: benefits from threshold (coordination) and extracts via kingmaking (asymmetric leverage).
constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL ADMINISTRATION (PITON) — The Central Elections Committee mechanically enforces the threshold. The ritual is functional (prevents explosion of micro-parties) but increasingly theatrical: thresholds have risen three times (from 1% in 1949 to 3.25% in 2014), each time performed as 'necessary for stability' while actually serving to concentrate power. theater_ratio≈0.38 is moderate, reflecting that enforcement is genuine but the stability justification is partially mythologized. d≈0.10, f(d)≈0.00, σ=1.0 → χ≈0.00. Piton gate not satisfied; classification is institutional/functional, not degraded.
constraint_indexing:constraint_classification(israel_electoral_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN, FALSE) — Thresholds are sometimes presented as natural laws: 'all democracies require thresholds to prevent fragmentation.' This perspective risks naturalizing a contingent institutional choice. Structural data contradicts mountain classification: ε=0.52 (moderate-high extraction), suppression=0.68 (significant). The engine's false summit detector identifies this as naturalized extraction, not immutable law. Multiple democracies operate without thresholds (Netherlands, Israel 1949-1992) or with lower ones (Germany 5%, Denmark no formal threshold), proving governability without this specific constraint.
constraint_indexing:constraint_classification(israel_electoral_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_electoral_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_electoral_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_electoral_threshold, TR),
    TR >= 0.70.

:- end_tests(israel_electoral_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The threshold's initial function (1949: 1%) was genuine coordination—preventing total fragmentation. But each increase (1992: 2%, 2014: 3.25%) has extracted more power from marginal movements while providing little marginal governance benefit. The increasing extractiveness reflects that stability gains plateau while political concentration deepens. Israeli Knessets from 1949-1992 with lower thresholds were governable; the rises in 1992 and 2014 were justified politically as 'necessary for stability' but functioned as rent-seeking. Suppression (0.68): High and stable. The threshold creates absolute barriers to representation: a party receiving 2.8% of votes gets zero seats despite genuine support. This suppression is structural—there is no alternative for sub-threshold constituencies except merger (identity loss) or abstention. Theater ratio (0.38): Moderate. The stability justification is partially real (some threshold is functional) but increasingly mythologized. International comparison reveals multiple stable democracies without thresholds (Netherlands) or with much lower ones (Germany 5%, Denmark no formal threshold). The ratio is not high (not piton) because enforcement is genuine, but not low (not rope) because the justification exceeds functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The marginal movement sees pure extraction (snare): they receive 2.8% of votes, get zero representation, and have no path to power except merger at the cost of identity. The kingmaker party sees both coordination and extraction (tangled rope): the threshold enables stable coalitions but gives them disproportionate leverage to extract ministerial posts and budgets. The established large party sees pure coordination (rope): the threshold protects them from splinter competition and enables stable coalition formation. The analytical observer risks seeing a mountain (natural law of democracy) when the structural data reveals a snare with mythologized justification. The suppression level (0.68) and extractiveness (0.52) make this snare, not mountain. The perspectival gap reveals that the same constraint is experienced as immutable law by those who benefit (institutional perspective) and as pure extraction by those harmed (powerless perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Marginal movements: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit option; they cannot reorganize geographically or negotiate entry. Minority ethnic parties: Victims + constrained → d≈0.82, f(d)≈1.28. High extraction. Can attempt merger to escape threshold, but this requires identity loss—constrained, not fully trapped. Coalition kingmakers: Beneficiary + mobile → d≈0.35, f(d)≈0.30. Low extraction from threshold perspective, but active enforcement maintains their kingmaker leverage. Mobile exit (can negotiate with alternative coalitions) but coalition power is significant incentive to maintain threshold. Large established parties: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.05. Negative effective extraction (net beneficiary). They can exit the constraint entirely (no coalition risk from threshold) and arbitrage their large vote share into coalition power. Electoral administration: Institutional + arbitrage → d≈0.10, f(d)≈0.00. Neutral/functional enforcement role; threshold appears as legitimate coordination from this perspective. Analytical observer: Analytical + analytical → d≈0.72, f(d)≈1.15. Risks naturalizing extraction as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via decomposition: The threshold exhibits genuine dual structure (tangled rope aspects) but fails the tangled rope gate because the 'coordination' function is partially mythologized while the extraction is real. Specifically: (1) GENUINE COORDINATION FUNCTION: Some threshold does prevent Knesset fragmentation into ungovernable splinters. Historical data from 1949-1992 with 1% threshold shows stable governance. This satisfies the coordination claim. (2) REAL ASYMMETRIC EXTRACTION: The threshold extracts from sub-threshold constituencies (wasted votes, zero representation) and provides kingmaker leverage to mid-sized parties above the threshold. This satisfies the extraction claim. (3) ACTIVE ENFORCEMENT: The Central Elections Committee actively enforces the threshold, and the Israeli political system requires it to function. This satisfies the enforcement requirement. (4) THE MANDATROPHY: A tangled rope must have BOTH genuine coordination AND asymmetric extraction. The threshold does both. However, the snare classification is correct because the theater ratio (0.38) is too low and the extractiveness (0.52) too high for tangled rope chi boundaries (0.40 ≤ χ ≤ 0.90). Computing χ from the primary victim perspective (marginal movement, powerless/trapped): χ = 0.52 × 1.40 × 1.0 = 0.73, which exceeds the tangled rope ceiling. From the kingmaker perspective: χ = 0.52 × 0.30 × 1.0 = 0.16, which is below the floor. This perspectival gap (snare from victim view, rope from beneficiary view) is correctly classified as snare at the system level because the highest chi perspective (victim at 0.73) determines the constraint type when perspectives disagree. The mandatrophy is resolved: the constraint is snare because the extraction mechanism dominates the coordination function from the perspective of those most harmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_floor,
    'What is the actual minimum threshold required to prevent ungovernable Knesset fragmentation, versus the politically claimed level?',
    'Comparative analysis of historical Israeli coalitions (1949-2014) and foreign legislatures with varying thresholds; agent-based modeling of coalition formation under different thresholds; empirical data on coalition stability vs threshold level',
    'If true minimum is 1.5%: current 3.25% is 2.17x extraction above functional necessity. If true minimum is 4.5%: current threshold is appropriately calibrated. Classification shifts from snare toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_floor, empirical, 'Minimum threshold to prevent ungovernable fragmentation').

omega_variable(
    representation_equity_boundary,
    'At what threshold level does democratic representation equity conflict with coalition stability, and where should the balance be drawn?',
    'Democratic theory analysis; survey of political science consensus on representation thresholds; historical comparison of countries with different thresholds and their stability/representation outcomes',
    'If equity requires <2%: threshold is indefensible extraction. If equity allows 3.25%: threshold is legitimate balance. This is a preference/values question, not purely empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(representation_equity_boundary, preference, 'Optimal balance between representation equity and coalition stability').

omega_variable(
    kingmaker_power_source,
    'Is the kingmaker power extracted from the threshold itself, or from Israel''s proportional representation system and coalition necessity, which would exist even without a threshold?',
    'Comparison with threshold-free systems (Netherlands, Denmark) and with single-member-district systems; analysis of whether removing the threshold while keeping proportional representation would eliminate kingmaker leverage',
    'If from threshold alone: threshold is primary extraction mechanism. If from proportional representation: threshold is secondary; removing it alone doesn''t solve extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kingmaker_power_source, empirical, 'Whether kingmaker power source is the threshold or the proportional system').

omega_variable(
    merger_coercion_mechanism,
    'Does the threshold coerce mergers that genuinely consolidate political platforms, or does it force identity-erasing coalitions that fragment again post-election?',
    'Historical tracking of pre-election merger coalitions vs post-election coalition outcomes; analysis of merger durability; survey of threshold-driven merger parties on coalition satisfaction',
    'If genuine consolidation: threshold reduces fragmentation (rope logic). If forced coalitions fragment: threshold just delays fragmentation (snare logic with illusory coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merger_coercion_mechanism, empirical, 'Whether threshold-driven mergers produce genuine consolidation or delayed fragmentation').

omega_variable(
    minority_representation_externality,
    'What is the causal path by which the threshold suppresses minority (Arab-Israeli, Druze) party representation, and is it intrinsic to thresholds or to Israel''s specific coalition politics?',
    'Comparison of minority party representation in Israeli elections across threshold levels (1949-2014); analysis of minority party vote shares vs representation before and after 1992 threshold increase; comparison with other democracies'' minority party thresholds',
    'If threshold is the primary mechanism: removing it would significantly increase minority representation. If coalition exclusion is primary: threshold is secondary; removing it alone doesn''t solve representation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_representation_externality, empirical, 'Causal mechanism linking threshold to minority party suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_electoral_threshold, 1992, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iet_tr_t0, israel_electoral_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(iet_tr_t25, israel_electoral_threshold, theater_ratio, 25, 0.32).
narrative_ontology:measurement(iet_tr_t50, israel_electoral_threshold, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(iet_be_t0, israel_electoral_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(iet_be_t25, israel_electoral_threshold, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(iet_be_t50, israel_electoral_threshold, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_electoral_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_electoral_threshold, israeli_coalition_kingmaking).
narrative_ontology:affects_constraint(israel_electoral_threshold, arab_israeli_political_representation).

% DUAL FORMULATION NOTE:
% The electoral threshold is upstream of coalition dynamics and minority representation constraints. Its ε=0.52 reflects the extraction inherent to the mechanism itself, independent of coalition politics. Downstream constraints (kingmaking, minority representation) inherit the threshold's structural properties: the kingmaking power depends on threshold existence; the minority representation suppression depends on threshold level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
